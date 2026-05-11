;;;; epsilon.build.cli - `epsilon build` command handler.

(defpackage epsilon.build.cli
  (:use :cl)
  (:import (epsilon.build.pool pool)
           (epsilon.loader loader)
           (epsilon.commands commands)
           (epsilon.sys.env env))
  (:export #:run))

(in-package :epsilon.build.cli)

(defun parse-args (args)
  (let ((modules '())
        (workers nil)
        (max-jobs 20)
        (force nil)
        (fail-fast nil)
        (all nil)
        (verbose nil)
        (load nil)
        (print-plan nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((and (string= arg "--workers") (< (1+ i) (length args)))
                (setf workers
                      (handler-case (parse-integer (nth (1+ i) args))
                        (error ()
                          (format *error-output* "Error: --workers expects an integer~%")
                          (sb-ext:exit :code 1))))
                (incf i 2))
               ((and (string= arg "--max-jobs-per-worker") (< (1+ i) (length args)))
                (setf max-jobs
                      (handler-case (parse-integer (nth (1+ i) args))
                        (error ()
                          (format *error-output*
                                  "Error: --max-jobs-per-worker expects an integer~%")
                          (sb-ext:exit :code 1))))
                (incf i 2))
               ((string= arg "--force")     (setf force t) (incf i))
               ((string= arg "--fail-fast") (setf fail-fast t) (incf i))
               ((string= arg "--all")       (setf all t) (incf i))
               ((string= arg "--verbose")   (setf verbose t) (incf i))
               ((string= arg "--print-plan") (setf print-plan t) (incf i))
               ((string= arg "--load-targets") (setf load t) (incf i))
               ((and (> (length arg) 0) (char/= (char arg 0) #\-))
                (push arg modules) (incf i))
               (t (incf i))))
    (list :modules (nreverse modules)
          :workers workers
          :max-jobs-per-worker max-jobs
          :force force
          :fail-fast fail-fast
          :all all
          :verbose verbose
          :print-plan print-plan
          :load load)))

(defun all-module-names ()
  (let ((platform (string-downcase (symbol-name (env:platform)))))
    (loop for mod in (loader:query-modules)
          for name = (loader:module-name mod)
          for metadata = (loader:module-metadata mod)
          for mod-platform = (getf metadata :platform)
          when (or (not mod-platform) (string-equal mod-platform platform))
          collect name)))

(defun resolve (modules &key all)
  (let ((targets modules))
    (when all
      (setf targets (all-module-names)))
    (when (null targets)
      (format *error-output* "Usage: epsilon build [modules...] [options]~%")
      (sb-ext:exit :code 1))
    (let ((resolved (mapcar (lambda (m)
                              (or (commands:resolve-module-name m)
                                  (progn
                                    (format *error-output*
                                            "Warning: module ~A not found~%" m)
                                    nil)))
                            targets)))
      (remove-duplicates (remove nil resolved) :test #'string=))))

(defun load-targets-in-parent (targets)
  "After a successful parallel build, load TARGETS in the current image.
   Returns (VALUES LOADED-COUNT ELAPSED-MS)."
  (let ((start (get-internal-real-time))
        (loaded 0))
    (dolist (name targets)
      (handler-case
          (progn
            (loader:load-module name)
            (incf loaded))
        (error (e)
          (format *error-output* "  load error for ~A: ~A~%" name e))))
    (values loaded
            (round (* 1000 (/ (- (get-internal-real-time) start)
                              internal-time-units-per-second))))))

(defun run (args passthrough-args)
  (declare (ignore passthrough-args))
  (let* ((opts (parse-args args))
         (resolved (resolve (getf opts :modules) :all (getf opts :all))))
    (when (getf opts :print-plan)
      (pool:print-build-plan resolved)
      (sb-ext:exit :code 0))
    (let ((summary (pool:build-modules-parallel
                    resolved
                    :workers (getf opts :workers)
                    :max-jobs-per-worker (getf opts :max-jobs-per-worker)
                    :force (getf opts :force)
                    :fail-fast (getf opts :fail-fast)
                    :verbose (getf opts :verbose))))
    (format t "~%~D module~:P  ~D ok  ~D failed  ~D skipped  (~,2Fs, ~D worker spawns)~%"
            (pool:pool-summary-total summary)
            (pool:pool-summary-succeeded summary)
            (pool:pool-summary-failed summary)
            (pool:pool-summary-skipped summary)
            (/ (pool:pool-summary-elapsed-ms summary) 1000.0)
            (pool:pool-summary-workers-spawned summary))
    (when (pool:pool-summary-failures summary)
      (format *error-output* "~%Failures:~%")
      (dolist (f (pool:pool-summary-failures summary))
        (format *error-output* "  ~A: ~A~%" (car f) (cdr f))))
    (when (and (getf opts :load)
               (zerop (pool:pool-summary-failed summary)))
      (multiple-value-bind (n ms) (load-targets-in-parent resolved)
        (format t "Loaded ~D target~:P into parent image (~,2Fs).~%"
                n (/ ms 1000.0))))
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code (if (zerop (pool:pool-summary-failed summary)) 0 1)
                 :abort t))))
