;;;; epsilon.build.core - Pre-warmed worker SBCL core image
;;;;
;;;; The pool optionally caches a pre-built SBCL core file at
;;;; _build/.parallel/worker.core that already contains epsilon and
;;;; epsilon.build. Workers boot from this core via SBCL --core,
;;;; skipping the boot/eval-of-cli-run/load-module phases entirely.
;;;;
;;;; Build flow: the parent shells out one helper subprocess per cold
;;;; build that loads epsilon.build and calls SAVE-AND-EXIT,
;;;; which invokes sb-ext:save-lisp-and-die. The cached core is keyed
;;;; on epsilon.build's module-content-hash; the cache is
;;;; reused across subsequent runs and invalidates whenever any source
;;;; file feeding into the worker changes.

(defpackage epsilon.build.core
  (:use :cl)
  (:import (epsilon.build.worker worker))
  (:export #:save-and-exit
           #:run-from-core))

(in-package :epsilon.build.core)

(defun run-from-core ()
  "Toplevel for a saved-core worker. Workers spawned via
   sbcl --core worker.core start here. Drops directly into the
   standard worker loop and exits when the parent sends :shutdown."
  (handler-case (worker:run-worker)
    (error (e)
      (format *error-output* "~&worker (from core): ~A~%" e)))
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (sb-ext:exit :code 0))

(defun save-and-exit (output-path)
  "Save the current SBCL image as a core file at OUTPUT-PATH and
   terminate. The saved core's toplevel is RUN-FROM-CORE so a fresh
   SBCL booting from this core lands directly in the worker loop.

   Intended to be invoked from a one-shot helper subprocess via:
     epsilon eval --module epsilon.build \\
       \"(epsilon.build.core:save-and-exit \\\"...\\\")\""
  (let ((dir (make-pathname :name nil :type nil :defaults output-path)))
    (ensure-directories-exist dir))
  ;; save-lisp-and-die expects single-threaded; epsilon.log keeps an
  ;; async writer thread when its background drainer was started, so
  ;; ask it to drain and stop before we save. If logging never started
  ;; a thread the call is a no-op.
  (handler-case
      (let ((sym (find-symbol "STOP-BACKGROUND-WRITER" :epsilon.log)))
        (when (and sym (fboundp sym))
          (funcall sym)))
    (error () nil))
  (sb-ext:save-lisp-and-die output-path
                            :toplevel #'run-from-core
                            :executable nil
                            :save-runtime-options nil
                            :purify t))
