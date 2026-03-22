;;;; stress-tests.lisp - Stress tests for the POSIX subprocess library
;;;;
;;;; Tests spawn under load: mass spawning, pipe buffer exhaustion,
;;;; rapid kill cycles, subprocess trees, fd leak detection, and
;;;; concurrent I/O.
;;;;
;;;; Part of IMPL-261: POSIX Subprocess Library (Stage 6)

(defpackage epsilon.process.stress-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process.spawn spawn)
            (epsilon.process.posix posix))
  (:local-nicknames (lib epsilon.foreign))
  (:enter t))

;;; ============================================================================
;;; Mass Spawn Tests
;;; ============================================================================

(deftest test-mass-spawn-1000 ()
  "Spawn 1000 /bin/true processes and verify all exit cleanly."
  (let ((handles (loop repeat 1000
                       collect (spawn:spawn (spawn:make-spawn-spec "true")))))
    ;; Wait for all
    (dolist (h handles)
      (spawn:wait-for-handle h))
    ;; Verify all exited with code 0
    (dolist (h handles)
      (assert-true (spawn:handle-exited-p h))
      (assert-equal 0 (spawn:process-handle-exit-code h)))
    ;; Verify no zombies remain by trying waitpid(-1, WNOHANG)
    (handler-case
        (multiple-value-bind (pid status)
            (posix:waitpid -1 posix:+wnohang+)
          (declare (ignore status))
          ;; pid=0 means no children waiting, -1/error means no children at all
          (assert-true (or (zerop pid) (null pid))
              "Expected no zombie children after mass spawn"))
      ;; ECHILD error is fine -- means no children
      (error () (assert-true t)))))

;;; ============================================================================
;;; Pipe Buffer Exhaustion
;;; ============================================================================

(deftest test-pipe-buffer-exhaustion ()
  "Pipe >1MB through dd, verify no hang or data loss."
  (let* ((byte-count (* 1024 1024 2)) ; 2MB
         (handle (spawn:spawn
                  (spawn:make-spawn-spec "dd"
                    :args (list "dd" "if=/dev/zero"
                                (format nil "bs=~D" byte-count)
                                "count=1" "status=none")
                    :stdout :pipe
                    :stderr :null))))
    (unwind-protect
         (let ((output (spawn:read-handle-output handle
                         :stream :stdout
                         :max-bytes (* 3 1024 1024))))
           (spawn:wait-for-handle handle)
           (assert-equal 0 (spawn:process-handle-exit-code handle))
           ;; dd should have produced exactly byte-count bytes
           (assert-equal byte-count (length output)))
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; Rapid Spawn + Kill
;;; ============================================================================

(deftest test-rapid-spawn-kill-100 ()
  "Spawn and immediately kill 100 sleep processes."
  (let ((handles nil))
    (dotimes (i 100)
      (let ((h (spawn:spawn (spawn:make-spawn-spec "sleep"
                              :args '("sleep" "60")))))
        (push h handles)
        (spawn:handle-kill h posix:+sigkill+)))
    ;; Wait for all to finish
    (dolist (h handles)
      (spawn:wait-for-handle h 5))
    ;; All should have exited (by signal or otherwise)
    (dolist (h handles)
      (assert-true (spawn:handle-exited-p h)))))

;;; ============================================================================
;;; Subprocess Tree
;;; ============================================================================

(deftest test-subprocess-tree ()
  "Shell spawning children -- verify clean exit propagation."
  (let ((handle (spawn:spawn
                 (spawn:make-spawn-spec "sh"
                   :args '("sh" "-c" "for i in 1 2 3 4 5; do echo $i; done")
                   :stdout :pipe))))
    (unwind-protect
         (progn
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (spawn:wait-for-handle handle)
             (assert-equal 0 (spawn:process-handle-exit-code handle))
             ;; Should have output from all 5 iterations
             (assert-true (search "1" output))
             (assert-true (search "5" output))))
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; FD Leak Detection Under Stress
;;; ============================================================================

(deftest test-no-fd-leak-under-stress ()
  "200 piped processes under with-fd-tracking -- assert no leaks."
  (spawn:with-fd-tracking
    (dotimes (i 200)
      (let ((handle (spawn:spawn (spawn:make-spawn-spec "echo"
                                   :args '("echo" "leak-test")
                                   :stdout :pipe))))
        (spawn:read-handle-output handle :stream :stdout)
        (spawn:wait-for-handle handle)
        (spawn:handle-close-streams handle)))
    ;; with-fd-tracking will error on exit if any fds are still tracked
    (assert-equal 0 (spawn:tracked-fd-count))))

;;; ============================================================================
;;; Concurrent Spawn with I/O
;;; ============================================================================

(deftest test-concurrent-spawn-with-io ()
  "50 processes with stdout capture, all running concurrently."
  (let ((handles (loop for i from 1 to 50
                       collect (spawn:spawn
                                (spawn:make-spawn-spec "echo"
                                  :args (list "echo" (format nil "proc-~D" i))
                                  :stdout :pipe)))))
    (unwind-protect
         (progn
           ;; Read output from all handles
           (let ((outputs (mapcar (lambda (h)
                                    (spawn:read-handle-output h :stream :stdout))
                                  handles)))
             ;; Wait for all to finish
             (dolist (h handles)
               (spawn:wait-for-handle h))
             ;; Verify each produced its expected output
             (loop for output in outputs
                   for i from 1
                   do (assert-true (search (format nil "proc-~D" i) output)
                          (format nil "Missing output for proc-~D" i)))
             ;; All exited with 0
             (dolist (h handles)
               (assert-equal 0 (spawn:process-handle-exit-code h)))))
      ;; Cleanup
      (dolist (h handles)
        (spawn:handle-close-streams h)))))
