(defpackage epsilon.tool.boot
  (:use cl)
  (:export boot))

(in-package :epsilon.tool.boot)

#-win32
(require :sb-posix)
#-win32
(require :sb-bsd-sockets)
(require :sb-rotate-byte)

(defparameter *module-core* "module/core/src/")

(defparameter *files*
  '("lib/syntax"
    "lib/map"
    "lib/sequence"
    "lib/string"
    "sys/pkg"
    "lib/symbol"
    "lib/type"
    "sys/env"
    "lib/path"
    "sys/fs"
    "tool/common"
    "lib/char"
    "lib/function"
    "lib/list"
    "lib/edn"
    "lib/digest/common"
    "lib/digest/reader"
    "lib/digest/generic"
    "lib/digest/sha-2"
    "lib/digest/public"
    "lib/hex"
    "lib/url"
    "lib/uuid"
    "tool/build"
    "tool/dev"))

(defparameter *boot-fasl*
  #+win32 "target\\boot.fasl"
  #-win32 "target/boot.fasl")

(defun ensure-target-dir ()
  (let ((target-dir #+win32 "target\\" #-win32 "target/"))
    (unless (probe-file target-dir)
      (ensure-directories-exist target-dir))))

(defun file-newer-p (file1 file2)
  "Return true if file1 is newer than file2, or if file2 doesn't exist"
  (or (not (probe-file file2))
      (and (probe-file file1)
           (> (file-write-date file1) (file-write-date file2)))))

(defun bootfile-needs-rebuild-p ()
  "Check if any source files are newer than the boot FASL"
  (or (not (probe-file *boot-fasl*))
      (some (lambda (file)
              (let ((source-path (concatenate 'string *module-core* 
                                              #+win32 (substitute #\\ #\/ file)
                                              #-win32 file
                                              ".lisp")))
                (file-newer-p source-path *boot-fasl*)))
            *files*)))

(defun load-boot-fasl ()
  "Load the concatenated boot FASL if it exists and is current"
  (when (and (probe-file *boot-fasl*) 
             (not (bootfile-needs-rebuild-p)))
    (load *boot-fasl*)
    t))

(defun concat-fasls (fasl-files output-file)
  "Create a single FASL file from individual FASL files"
  (format t "~&;;; Creating boot FASL: ~A~%" output-file)
  (with-open-file (output output-file :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
    (dolist (fasl-file fasl-files)
      (when (probe-file fasl-file)
        (with-open-file (input fasl-file :direction :input
                                         :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte input nil nil)
                while byte
                do (write-byte byte output)))))))

(defun generate-boot-fasl ()
  (let ((fasl-files '()))
    (dolist (file *files*)
      (let ((source-path (concatenate 'string *module-core* 
                                      #+win32 (substitute #\\ #\/ file)
                                      #-win32 file
                                      ".lisp")))
        (force-output)
        (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
          (let ((fasl-path 
                 (compile-file source-path :print nil :verbose nil)))
            (unless fasl-path
              (error "compile-file returned NIL for ~A" source-path))
            (push fasl-path fasl-files)
            (load fasl-path)))))
    (concat-fasls (reverse fasl-files) *boot-fasl*)))

(defun boot ()
  (ensure-target-dir)
  (unless (load-boot-fasl)
    ;; load is a side-effect
    (generate-boot-fasl)))
