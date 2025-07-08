(defpackage epsilon.tool.boot
  (:use cl)
  (:export boot repo-root))

(in-package :epsilon.tool.boot)

#-win32
(require :sb-posix)
#-win32
(require :sb-bsd-sockets)
(require :sb-rotate-byte)

(defun repo-root ()
  "Get the repository root directory, preferring EPSILON environment variable"
  (let ((epsilon-env #+sbcl (sb-posix:getenv "EPSILON") #-sbcl nil))
    (if epsilon-env
        (substitute #\/ #\\ epsilon-env)  ; normalize path separators
        ;; Fallback: assume we're running from repo root (current directory)
        #+win32 (substitute #\/ #\\ (sb-ext:native-namestring (truename ".")))
        #-win32 (sb-posix:getcwd))))

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
    "lib/uri"
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
    "tool/build"))

(defun boot-log-path ()
  (concatenate 'string (repo-root) "/" 
               #+win32 "target\\boot.log"
               #-win32 "target/boot.log"))

(defun ensure-target-dir ()
  (let ((target-dir (concatenate 'string (repo-root) "/" 
                                 #+win32 "target\\" #-win32 "target/")))
    (unless (probe-file target-dir)
      (ensure-directories-exist target-dir))))

(defun boot ()
  (ensure-target-dir)
  (let ((total (length *files*))
        (current 0))
    (with-open-file (log (boot-log-path) :direction :output 
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
      (format t "~&;;; Bootstrapping epsilon (~D files)...~%" total)
      (dolist (file *files*)
        (incf current)
        (let ((source-path (concatenate 'string 
                                        (repo-root) "/" *module-core* 
                                        #+win32 (substitute #\\ #\/ file)
                                        #-win32 file
                                        ".lisp")))
          (format t "~C[2K~C;;; [~3D/~3D] ~A~C" 
                  #\Escape #\Return current total file #\Return)
          (force-output)
          (handler-case
              (let ((fasl-path 
                     (let ((*standard-output* log)
                           (*error-output* log))
                       (compile-file source-path :print nil :verbose nil))))
                (unless fasl-path
                  (error "compile-file returned NIL for ~A" source-path))
                (load fasl-path))
            (error (e)
              (format t "~%;;; ERROR compiling ~A: ~A~%" file e)
              (error e))))))
    (format t "~C[2K~C;;; Bootstrap complete (~D files compiled)~%" 
            #\Escape #\Return total)))
