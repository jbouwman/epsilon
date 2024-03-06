(defpackage #:sys.directory
  (:use
   #:cl
   #:lib.type)
  (:export
   #:with-tempfile))

(in-package #:sys.directory)

(defun random-string (n)
  (let ((s (->u8 n)))
    (loop :for i :from 0 :below n
          :do (setf (aref s i)
                    (+ 65 32 (random 26))))
    (lib.char:u8-to-string s)))

(defun temporary-directory ()
  "Return a default directory to use for temporary files"
  (sys.env:getenv "TMPDIR"))

(defmacro with-tempfile ((name) &body body)
  `(let ((,name (lib.string:concat
                 (temporary-directory)
                 (random-string 16)
                 ".tmp")))
     (unwind-protect
          (progn
            ,@body)
       (when (sys.filesystem:file-exists-p ,name)
         (sys.filesystem:delete-file* ,name)))))
