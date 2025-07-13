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
    "lib/vector"
    "lib/char"
    "lib/function"
    "lib/list"
    "lib/edn"
    "lib/digest/common"
    "lib/digest/reader"
    "lib/digest/generic"
    "lib/digest/sha-2"
    "lib/digest/public"                 ; rename
    "lib/hex"
    "lib/regex"
    "lib/url"
    "tool/build"))

(defparameter *boot-log* 
  #+win32 "target\\boot.log"
  #-win32 "target/boot.log")

(defparameter *stage1-fasl*
  #+win32 "target\\stage-1.fasl"
  #-win32 "target/stage-1.fasl")

(defun ensure-target-dir ()
  (let ((target-dir #+win32 "target\\" #-win32 "target/"))
    (unless (probe-file target-dir)
      (ensure-directories-exist target-dir))))

(defun file-newer-p (file1 file2)
  "Return true if file1 is newer than file2, or if file2 doesn't exist"
  (or (not (probe-file file2))
      (and (probe-file file1)
           (> (file-write-date file1) (file-write-date file2)))))

(defun stage1-needs-rebuild-p ()
  "Check if any source files are newer than the stage-1 FASL"
  (or (not (probe-file *stage1-fasl*))
      (some (lambda (file)
              (let ((source-path (concatenate 'string *module-core* 
                                              #+win32 (substitute #\\ #\/ file)
                                              #-win32 file
                                              ".lisp")))
                (file-newer-p source-path *stage1-fasl*)))
            *files*)))

(defun load-stage1-fasl ()
  "Load the concatenated stage-1 FASL if it exists and is current"
  (when (and (probe-file *stage1-fasl*) 
             (not (stage1-needs-rebuild-p)))
    (format t "~&;;; Loading stage-1 from concatenated FASL...~%")
    (load *stage1-fasl*)
    t))

(defun create-concatenated-fasl (fasl-files output-file)
  "Create a concatenated FASL file from individual FASL files"
  (format t "~&;;; Creating concatenated FASL: ~A~%" output-file)
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

(defun boot ()
  (ensure-target-dir)
  
  ;; Try to load from concatenated FASL first
  (unless (load-stage1-fasl)
    ;; Need to rebuild - compile individual files
    (let ((total (length *files*))
          (current 0)
          (fasl-files '()))
      (with-open-file (log *boot-log* :direction :output 
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
        (format t "~&;;; Bootstrapping epsilon~%;;;~%")
        (dolist (file *files*)
          (incf current)
          (let ((source-path (concatenate 'string *module-core* 
                                          #+win32 (substitute #\\ #\/ file)
                                          #-win32 file
                                          ".lisp")))
            (format t ";;; [~2D/~2D] ~A~%" current total file)
            (force-output)
            (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
              (let ((fasl-path 
                      (let ((*standard-output* log)
                            (*error-output* log))
                        (compile-file source-path :print nil :verbose nil))))
                (unless fasl-path
                  (error "compile-file returned NIL for ~A" source-path))
                (push fasl-path fasl-files)
                (load fasl-path))))))
      (format t ";;; Bootstrap complete (~D files compiled)~%" total)
      
      ;; Create concatenated FASL for next time
      (when fasl-files
        (create-concatenated-fasl (reverse fasl-files) *stage1-fasl*)))))
