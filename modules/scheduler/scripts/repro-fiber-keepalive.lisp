;;;; IMPL-382 reproducer: fiber/coroutine keepalive hang under heavy
;;;; allocation.  Self-contained -- no hemidemi data required.
;;;;
;;;; Usage:
;;;;
;;;;   nix develop --command \
;;;;     ./epsilon/epsilon eval --module epsilon.web --module epsilon.http \
;;;;       --load epsilon-contrib/scheduler/scripts/repro-fiber-keepalive.lisp t
;;;;
;;;; Or invoke specific entry points from a REPL after loading:
;;;;
;;;;   (epsilon.scheduler.repro:run-server)        ; start service on port 4099
;;;;   (epsilon.scheduler.repro:drive-keepalive)   ; hammer it
;;;;   (epsilon.scheduler.repro:dump)              ; dump fiber/backtrace state
;;;;
;;;; What this script does:
;;;;
;;;; 1. Defines a heavy-allocation handler: builds a deeply-nested
;;;;    epsilon.map HAMT of `tree-leaves` leaves and JSON-encodes it.
;;;;    The encoder + HAMT walk is the same pattern that wedges the
;;;;    hemidemi resolver path (a tree of nested HAMTs followed by
;;;;    json:encode).
;;;;
;;;; 2. Stands up an epsilon.web service with the scheduler enabled and
;;;;    binds *coroutine-trace* so every fiber entry/exit prints a
;;;;    `#fiber-trace` line to *trace-log-path* (defaults to
;;;;    .scratch/fiber-trace.log under the repo root).
;;;;
;;;; 3. Drives N keepalive requests against the service in-process via
;;;;    epsilon.http.client (which reuses the TCP connection by default).
;;;;
;;;; 4. After each request prints `:control-stack-usage` for the carrier's
;;;;    known coroutines.  If the run wedges, dump-carrier-backtraces is
;;;;    called from a watchdog timer thread to capture each carrier's
;;;;    Lisp backtrace before the runtime locks up entirely.

(defpackage :epsilon.scheduler.repro
  (:use :cl)
  (:import (epsilon.scheduler sched)
           (epsilon.scheduler.coroutine coro)
           (epsilon.web web)
           (epsilon.http.client client)
           (epsilon.http.connection-pool pool)
           (epsilon.http.response response)
           (epsilon.http.request request)
           (epsilon.json json)
           (epsilon.map map)
           (epsilon.sys.thread thread))
  (:export #:run-server #:stop-server #:drive-keepalive #:dump
           #:run-tests
           #:*service* #:*trace-stream* #:*pool*
           #:build-tree #:make-handler))

(in-package :epsilon.scheduler.repro)

(defparameter *port* 4099)
(defparameter *trace-log-path*
  ;; Resolve to <repo-root>/.scratch/fiber-trace.log: this file lives at
  ;; <repo>/epsilon-contrib/scheduler/scripts/repro-fiber-keepalive.lisp,
  ;; so going up three directories from *load-pathname* lands at the
  ;; repo root regardless of the process CWD.
  (let ((here (or *load-pathname* *compile-file-pathname*)))
    (if here
        (namestring
         (merge-pathnames ".scratch/fiber-trace.log"
                          (make-pathname
                           :directory (butlast (pathname-directory here) 3)
                           :defaults here)))
        ".scratch/fiber-trace.log"))
  "Where *coroutine-trace* writes when TRACE-FIBERS is true.  Computed at
load time relative to the script's own location, so it lands in the repo
root's .scratch/ directory regardless of the process CWD.")
(defparameter *tree-fanout* 8
  "HAMT children per node.  Combined with *tree-depth* this controls the
total number of leaves and therefore the response body size.")
(defparameter *tree-depth* 4
  "Depth of the synthetic HAMT.  Default 4 with fanout 8 produces ~4096
leaves and a JSON body around 200 KB -- comparable to the hemidemi
prelude resolver response.")

(defvar *service* nil)
(defvar *trace-stream* nil
  "Open output stream that *coroutine-trace* writes to during the run.")
(defvar *pool* nil
  "Dedicated keepalive connection pool the driver uses, so every request
in DRIVE-KEEPALIVE shares a single TCP connection (the IMPL-382
trigger condition).")

(defun build-tree (depth fanout)
  "Construct a deeply-nested epsilon.map HAMT.  Each non-leaf node has
FANOUT children keyed \"k0\"..\"kN-1\"; leaves are short strings.  This
mirrors the structural shape of a resolved score's display-list tree
without needing any hemidemi data."
  (if (zerop depth)
      (format nil "leaf-~A" (random 1000000))
      (let ((m map:+empty+))
        (dotimes (i fanout)
          (setf m
                (map:assoc m
                           (format nil "k~D" i)
                           (build-tree (1- depth) fanout))))
        m)))

(defun make-handler ()
  "Return an HTTP handler that builds a fresh tree per request and
JSON-encodes it.  The fresh tree per request is deliberate: it forces
allocation pressure (the response can't be cached)."
  (lambda (req)
    (declare (ignore req))
    (let* ((tree (build-tree *tree-depth* *tree-fanout*))
           (body (with-output-to-string (s)
                   (json:encode tree s))))
      (response:make-response
       :status 200
       :body body
       :headers (map:make-map "Content-Type" "application/json")))))

(web:defservice repro-svc
  :routes ("" :children
              (("alloc" :get #'(lambda (r) (funcall (make-handler) r))))))

(defun run-server (&key (port *port*) (trace-fibers nil)
                        (trace-path *trace-log-path*))
  "Start the reproducer service.  When TRACE-FIBERS is true, opens
TRACE-PATH and sets *coroutine-trace* so every fiber entry and exit is
logged; this is verbose but shows whether the connection coroutine's
stack grows across keepalive turns.  When TRACE-FIBERS is NIL (default)
the run uses only the on-demand snapshot facility."
  (when *service*
    (warn "Reproducer service already running; stop first.")
    (return-from run-server *service*))
  (when trace-fibers
    (setf *trace-stream*
          (open trace-path
                :direction :output
                :if-exists :append
                :if-does-not-exist :create))
    (setf coro:*coroutine-trace* *trace-stream*))
  (format *trace-output*
          "~&;; reproducer: starting on port ~D, trace=~A~%"
          port trace-fibers)
  (force-output *trace-output*)
  (setf *service*
        (web:start-service repro-svc
                           :port port
                           :address "127.0.0.1"))
  (setf *pool* (pool:create-connection-pool :max-size 4))
  *service*)

(defun stop-server ()
  (when *service*
    (web:stop-service *service*)
    (setf *service* nil))
  (when *pool*
    (ignore-errors (pool:shutdown-connection-pool *pool*))
    (setf *pool* nil))
  (setf coro:*coroutine-trace* nil)
  (when *trace-stream*
    (ignore-errors (close *trace-stream*))
    (setf *trace-stream* nil))
  (values))

(defun drive-keepalive (&key (n 12) (warn-after-seconds 6))
  "Drive N sequential GET requests against the running service over
HTTP/1.1 keepalive (epsilon.http.client reuses the connection by
default).  After WARN-AFTER-SECONDS without progress on a single
request, schedule a watchdog that calls DUMP from a fresh thread.

Prints one line per request with status, elapsed time, and a
fiber-stack snapshot summary (max control-stack-usage seen across all
known coroutines)."
  (unless *service*
    (error "drive-keepalive: no service running -- call (run-server) first"))
  (let* ((url (format nil "http://127.0.0.1:~D/alloc" *port*))
         (start-overall (get-internal-real-time)))
    (dotimes (i n)
      (let ((start (get-internal-real-time))
            (watchdog (thread:make-thread
                       (let ((deadline (+ (get-internal-real-time)
                                          (* warn-after-seconds
                                             internal-time-units-per-second))))
                         (lambda ()
                           (loop while (< (get-internal-real-time) deadline)
                                 do (sleep 0.5))
                           (format *error-output*
                                   "~&;; WATCHDOG: request ~D exceeded ~Ds, dumping~%"
                                   i warn-after-seconds)
                           (force-output *error-output*)
                           (handler-case (dump)
                             (error (e)
                               (format *error-output*
                                       "~&;; watchdog dump failed: ~A~%" e)))))
                       :name "repro-watchdog")))
        (handler-case
            (let* ((resp (client:get url :pool *pool*))
                   (status (response:response-status resp))
                   (body (response:response-body resp))
                   (elapsed (/ (- (get-internal-real-time) start)
                               (float internal-time-units-per-second))))
              (format *trace-output*
                      "~&;; r~D status=~A bytes=~D elapsed=~,3Fs csu-max=~D~%"
                      i status (if body (length body) 0) elapsed
                      (max-csu-across-carriers))
              (force-output *trace-output*))
          (error (e)
            (format *error-output*
                    "~&;; r~D ERROR after ~,3Fs: ~A~%"
                    i
                    (/ (- (get-internal-real-time) start)
                       (float internal-time-units-per-second))
                    e)
            (force-output *error-output*)))
        ;; Cancel watchdog if the request returned in time.
        (when (thread:thread-alive-p watchdog)
          (ignore-errors (thread:destroy-thread watchdog)))))
    (format *trace-output*
            "~&;; repro complete: ~D requests in ~,3Fs total~%"
            n
            (/ (- (get-internal-real-time) start-overall)
               (float internal-time-units-per-second)))
    (force-output *trace-output*)))

(defun max-csu-across-carriers ()
  "Walk every coroutine known to the service's scheduler and return the
largest control-stack-usage observed.  Useful as a single-number proxy
for fiber stack growth -- if the connection coroutine's stack is
growing across requests, this number trends upward."
  (let ((sched (web:service-scheduler *service*)))
    (if sched
        (let ((max-csu 0))
          (dolist (c (sched:carrier-fiber-stack-snapshot sched))
            (dolist (s (getf c :coroutines))
              (let ((csu (getf s :control-stack-usage)))
                (when (and csu (> csu max-csu))
                  (setf max-csu csu)))))
          max-csu)
        0)))

(defun dump (&key (stream *error-output*))
  "Print the current carrier-fiber-stack snapshot and the per-carrier
backtraces.  Safe to call from any thread."
  (let ((sched (web:service-scheduler *service*)))
    (cond
      ((null sched)
       (format stream "~&;; dump: service has no scheduler (running on threads)~%"))
      (t
       (format stream "~&;; ============================================================~%")
       (format stream ";; Snapshot at ~A~%" (get-universal-time))
       (format stream ";; ============================================================~%")
       (let ((snap (sched:carrier-fiber-stack-snapshot sched)))
         (dolist (c snap)
           (format stream ";; carrier ~D: ~D coroutines~%"
                   (getf c :carrier) (length (getf c :coroutines)))
           (dolist (s (getf c :coroutines))
             (format stream
                     ";;   coro ~D run-count=~D state=~S csu=~D/~D bsu=~D/~D~%"
                     (getf s :id)
                     (getf s :run-count)
                     (getf s :state)
                     (getf s :control-stack-usage)
                     (getf s :control-stack-size)
                     (getf s :binding-stack-usage)
                     (getf s :binding-stack-size)))))
       (format stream ";; -- backtraces --~%")
       (sched:dump-carrier-backtraces sched :stream stream)
       (force-output stream)))))

;; Self-test entry point for `epsilon eval --load`.  Drives the
;; reproducer end-to-end and reports.
(defun run-tests (&key (n 30) (warn-after-seconds 6) (trace-fibers nil)
                       (trace-path *trace-log-path*))
  (run-server :trace-fibers trace-fibers :trace-path trace-path)
  (unwind-protect
       (drive-keepalive :n n :warn-after-seconds warn-after-seconds)
    (stop-server)))
