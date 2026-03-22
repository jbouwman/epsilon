;;;; Test work directory management
;;;;
;;;; Creates repository-local .epsilon/test-work/<module>/<timestamp-pid>/
;;;; directories for test artifacts, replacing /tmp pollution.

(defpackage epsilon.test.workdir
  (:use :cl)
  (:local-nicknames
   (:fs :epsilon.file)
   (:str :epsilon.string)
   (:project :epsilon.project))
  (:export
   :make-run-directory
   :cleanup-run-directory))

(in-package :epsilon.test.workdir)

(defun timestamp-tag ()
  "Return YYYYMMDD-HHMMSS-<pid> string for directory naming."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~D"
            year month day hour min sec (sb-posix:getpid))))

(defun find-work-root ()
  "Find the project root's .epsilon/test-work/ directory.
   Falls back to $TMPDIR/epsilon-test-work/ when no project root is found."
  (let ((project-root (handler-case
                           (project:find-project-file (namestring (truename ".")))
                         (error () nil))))
    (if project-root
        (fs:join-paths project-root ".epsilon" "test-work")
        (fs:join-paths (or (sb-ext:posix-getenv "TMPDIR") "/tmp")
                       "epsilon-test-work"))))

(defun make-run-directory (module-name &key project-root)
  "Create a test run directory for MODULE-NAME.
   Returns the run directory path string (e.g. .epsilon/test-work/epsilon.ssh/20260310-143022-12345/).
   Also creates a tmp/ subdirectory for temp file allocation."
  (let* ((work-root (or project-root (find-work-root)))
         (run-dir (fs:join-paths work-root module-name (timestamp-tag)))
         (tmp-dir (fs:join-paths run-dir "tmp")))
    (fs:make-dirs tmp-dir)
    run-dir))

(defun cleanup-run-directory (path)
  "Recursively delete the run directory at PATH."
  (when (and path (fs:dir-p path))
    (fs:delete-directory path)))
