(require :sb-posix)
(require :sb-rotate-byte)

(defparameter *core-module* "modules/core/src/")

(defparameter *files*
  '("syntax"
    "map"
    "set"
    "sequence"
    "string"
    "table"
    "log"
    "argparse"
    "sys/pkg"
    "symbol"
    "type"
    "sys/env"
    "path"
    "sys/fs"
    "tool/common"
    "char"
    "function"
    "list"
    "hex"
    "url"
    "protocol"
    "compile-api"
    "compile-location"
    "compile-hooks"
    "compile"
    "compile-integration"
    "loader"
    "main"))

(defparameter *boot-fasl*
  #+win32 "modules\\core\\target\\bootstrap.fasl"
  #-win32 "modules/core/target/bootstrap.fasl")

(defparameter *boot-dirs*
  #+win32
  '("modules\\core\\target\\" 
    "modules\\core\\target\package\\fasl\\")
  #-win32
  '("modules/core/target/"
    "modules/core/target/package/fasl/"))

(defun ensure-target-dir ()
  (dolist (dir *boot-dirs*)
    (unless (probe-file dir)
      (ensure-directories-exist dir))))

(defun file-newer-p (file1 file2)
  "Return true if file1 is newer than file2, or if file2 doesn't exist"
  (or (not (probe-file file2))
      (and (probe-file file1)
           (> (file-write-date file1) (file-write-date file2)))))

(defun epk-fasl-path (file)
  "Generate EPK FASL path for a given source file"
  (concatenate 'string 
               #+win32 "modules\\core\\target\\package\\fasl\\"
               #-win32 "modules/core/target/package/fasl/"
               #+win32 (substitute #\\ #\/ file)
               #-win32 file
               ".fasl"))

(defun bootfile-needs-rebuild-p ()
  "Check if any source files are newer than the boot FASL"
  (or (not (probe-file *boot-fasl*))
      (some (lambda (file)
              (let ((source-path (concatenate 'string *core-module* 
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
      (let* ((source-path (concatenate 'string *core-module* 
                                       #+win32 (substitute #\\ #\/ file)
                                       #-win32 file
                                       ".lisp"))
             (target-fasl-path (epk-fasl-path file)))
        ;; Ensure the directory exists for this FASL
        (ensure-directories-exist target-fasl-path)
        (force-output)
        (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
          (let ((fasl-path 
                 (compile-file source-path 
                               :output-file target-fasl-path
                               :print nil :verbose nil)))
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
