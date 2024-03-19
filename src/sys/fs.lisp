(defpackage #:sys.fs
  (:use
   #:cl
   #:lib.string
   #:lib.type)
  (:export
   #:current-dir
   #:temp-dir
   #:with-current-dir
   #:with-temp-file))

(in-package #:sys.fs)

(defun temp-dir ()
  "Return a default directory to use for temporary files"
  (sys.env:getenv "TMPDIR"))

(defmacro with-temp-file ((name) &body body)
  `(let ((,name (lib.string:concat
                 (temp-dir)
                 (random-string 16)
                 ".tmp")))
     (unwind-protect
          (progn
            ,@body)
       (when (sys.filesystem:file-exists-p ,name)
         (sys.filesystem:delete-file* ,name)))))

(defun current-dir ()
  (sb-unix:posix-getcwd/))

(defun (setf current-dir) (dir)
  (sb-posix:chdir dir)
  dir)

(defun call-with-current-dir (function dir)
  (let ((current (current-dir)))
    (if (string= current dir)
        (funcall function)
        (progn
          (setf (current-dir) dir)
          (unwind-protect (funcall function)
            (setf (current-dir) current))))))

(defmacro with-current-dir ((dir) &body body)
  `(call-with-current-dir (lambda () ,@body) ,dir))
