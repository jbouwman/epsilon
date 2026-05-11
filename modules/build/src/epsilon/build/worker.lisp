;;;; epsilon.build.worker - SBCL build worker
;;;;
;;;; Long-running child process. Reads compile requests on stdin and
;;;; writes replies on stdout, using epsilon.build.protocol
;;;; framing. After MAX-JOBS successful compiles the worker emits a
;;;; (:retiring ...) frame and exits 0; the scheduler spawns a fresh
;;;; replacement. *standard-output* is rebound to *error-output* for
;;;; the duration of the worker so any incidental printing from the
;;;; loader or compiler does not corrupt the frame stream.

(defpackage epsilon.build.worker
  (:use :cl)
  (:import (epsilon.build.protocol proto)
           (epsilon.loader loader)
           (epsilon.project project)
           (epsilon.file fs))
  (:export #:run-worker
           #:run-worker-cli))

(in-package :epsilon.build.worker)

(defun %scan-project-directories (project-dir)
  "Read project.sexp at PROJECT-DIR and run loader:scan-module-directory
   for every entry in its `:scan` list.  Bypasses project:load-project
   on purpose -- that path runs check-runtime-version against env:version,
   which returns \"unknown\" outside the epsilon source tree (no VERSION
   file in CWD) and signals.  The parent has already validated the
   project; the worker only needs the module-registration side-effect.
   Returns the number of directories scanned."
  (let* ((project-file (merge-pathnames "project.sexp" project-dir))
         (plist (with-open-file (s project-file :direction :input)
                  (read s)))
         (scan-dirs (getf plist :scan))
         (scanned 0))
    (dolist (rel-dir scan-dirs)
      (let ((abs-dir (merge-pathnames (concatenate 'string rel-dir "/")
                                      project-dir)))
        (when (probe-file abs-dir)
          (loader:scan-module-directory (namestring abs-dir))
          (incf scanned))))
    scanned))

(defun discover-project ()
  "Walk upward from the worker's CWD looking for a project.sexp and,
   if found, scan its `:scan` directories so the same module set the
   parent sees is registered in this image.  Without this, modules
   outside EPSILON_HOME/modules (avalon/*, kreisler/*, etc.) come
   back as 'Unknown module' when the parent dispatches them for
   compilation."
  (handler-case
      (progn
        ;; Ensure the loader environment exists.  In a saved-core
        ;; worker it should already be bound from the core-build phase,
        ;; but we re-initialise defensively in case the core was built
        ;; without a populated environment.
        (loader:environment)
        (let* ((cwd (handler-case (fs:get-cwd) (error () nil)))
               (project-dir (and cwd
                                 (handler-case (project:find-project-file cwd)
                                   (error () nil)))))
          (cond
            ((null cwd)
             (format *error-output*
                     "~&worker: discover-project: no CWD~%"))
            ((null project-dir)
             (format *error-output*
                     "~&worker: discover-project: no project.sexp from ~A~%" cwd))
            (t
             (let ((n (%scan-project-directories project-dir)))
               (format *error-output*
                       "~&worker: discover-project: scanned ~D dir~:P from ~Aproject.sexp~%"
                       n project-dir))))))
    (error (e)
      ;; Never kill the worker on discovery errors; the parent will
      ;; dispatch jobs and the un-registered ones will fail with the
      ;; existing "Unknown module" path, which is at least diagnosable.
      (format *error-output*
              "~&worker: discover-project failed: ~A~%" e))))

(defvar *control-out* nil
  "Stream the worker uses for response frames. Bound at startup to the
   inherited *standard-output*; the dynamic *standard-output* is then
   redirected to *error-output*.")

(defvar *jobs-done* 0)

(defun emit (sexp)
  (proto:write-frame *control-out* sexp))

(defun condition-message (c)
  (handler-case (princ-to-string c)
    (error () (format nil "<~A>" (type-of c)))))

(defun handle-compile (plist)
  (let* ((module-name (getf plist :module))
         (force (getf plist :force))
         (started (get-internal-real-time)))
    (unless (and (stringp module-name) (plusp (length module-name)))
      (emit (list :error :module module-name :reason "missing :module"))
      (return-from handle-compile))
    (handler-case
        (progn
          (loader:load-module module-name :force force :verbose nil)
          (incf *jobs-done*)
          (let* ((mod (loader:get-module module-name))
                 (hash (and mod (loader:module-content-hash mod)))
                 (elapsed-ms (round (* 1000 (/ (- (get-internal-real-time) started)
                                               internal-time-units-per-second)))))
            (emit (list :ok
                        :module module-name
                        :module-hash hash
                        :elapsed-ms elapsed-ms
                        :jobs-done *jobs-done*))))
      (error (e)
        (emit (list :error
                    :module module-name
                    :reason (condition-message e)
                    :type (string (type-of e))))))))

(defun handle-request (request)
  "Dispatch one request. Returns :continue or :stop. The job-count
   retire decision lives in the parent scheduler so its accounting is
   authoritative; the worker keeps running until told to shut down."
  (let ((op (and (consp request) (first request)))
        (rest (and (consp request) (rest request))))
    (case op
      (:compile
       (handle-compile rest)
       :continue)
      (:ping
       (emit (list :pong :pid (sb-posix:getpid) :jobs-done *jobs-done*))
       :continue)
      (:shutdown
       (emit (list :goodbye :jobs-done *jobs-done*))
       :stop)
      (t
       (emit (list :error
                   :reason (format nil "unknown op: ~S" op)))
       :continue))))

(defun run-worker (&key (max-jobs 20))
  "Worker main loop. Reads framed requests on stdin and writes framed
   responses on the inherited stdout. MAX-JOBS is informational only --
   the scheduler decides when to retire this worker."
  (let ((*control-out* *standard-output*)
        (*standard-output* *error-output*))
    (handler-bind ((style-warning #'muffle-warning)
                   (sb-ext:compiler-note #'muffle-warning))
      ;; Mirror the parent's project discovery so modules registered
      ;; via project.sexp :scan are visible here too.  Must happen
      ;; before the first :compile arrives.
      (discover-project)
      (emit (list :hello
                  :pid (sb-posix:getpid)
                  :max-jobs max-jobs))
      (loop
        (let ((request (handler-case (proto:read-frame *standard-input*)
                         (proto:protocol-error (e)
                           (format *error-output*
                                   "~&worker ~D: ~A~%" (sb-posix:getpid) e)
                           (return-from run-worker)))))
          (unless request (return-from run-worker))
          (case (handle-request request)
            (:stop (return-from run-worker))))))))

(defun run-worker-cli (args passthrough-args)
  "CLI handler for `epsilon build-worker`. Parses --max-jobs and runs
   the worker loop."
  (declare (ignore passthrough-args))
  (let ((max-jobs 20))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((and (string= arg "--max-jobs") (< (1+ i) (length args)))
                (let ((n (handler-case (parse-integer (nth (1+ i) args))
                           (error () nil))))
                  (when (and n (plusp n))
                    (setf max-jobs n)))
                (incf i 2))
               (t (incf i))))
    (run-worker :max-jobs max-jobs)
    (finish-output *standard-output*)
    (finish-output *error-output*)
    (sb-ext:exit :code 0)))
