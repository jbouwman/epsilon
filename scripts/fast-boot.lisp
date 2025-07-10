;;;; Fast Boot Script for Epsilon
;;;; 
;;;; This script provides optimized boot when epsilon.core EPK exists locally.
;;;; It checks for a cached combined FASL and uses it for rapid startup.

(defpackage epsilon.tool.fast-boot
  (:use cl)
  (:export fast-boot))

(in-package :epsilon.tool.fast-boot)

;; Required for basic functionality
#-win32
(require :sb-posix)
#-win32
(require :sb-bsd-sockets)
(require :sb-rotate-byte)

(defparameter *boot-cache-dir*
  (merge-pathnames ".epsilon/boot-cache/" (user-homedir-pathname))
  "Directory for boot FASL cache")

(defparameter *epsilon-core-cache*
  (merge-pathnames "epsilon.core.boot.fasl" *boot-cache-dir*)
  "Path to cached epsilon.core boot FASL")

(defparameter *epsilon-core-manifest*
  (merge-pathnames "epsilon.core.manifest" *boot-cache-dir*)
  "Path to cached epsilon.core manifest")

(defparameter *module-core* "module/core/")

(defparameter *bootstrap-files*
  ;; Minimal files needed to check cache validity
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
    "lib/digest/public"
    "lib/hex"
    "tool/build"))

(defun ensure-directories ()
  "Ensure required directories exist"
  (ensure-directories-exist *boot-cache-dir*)
  (ensure-directories-exist "target/"))

(defun calculate-source-hash ()
  "Calculate hash of source files for cache validation"
  ;; Simple implementation - in production would use SHA-256
  (let ((hash 0))
    (dolist (file *bootstrap-files*)
      (let ((path (merge-pathnames (concatenate 'string file ".lisp")
                                   *module-core*)))
        (when (probe-file path)
          (incf hash (file-write-date path)))))
    (format nil "~X" hash)))

(defun load-manifest ()
  "Load boot manifest if it exists"
  (when (probe-file *epsilon-core-manifest*)
    (with-open-file (stream *epsilon-core-manifest*)
      (read stream nil nil))))

(defun save-manifest (hash)
  "Save boot manifest"
  (with-open-file (stream *epsilon-core-manifest*
                         :direction :output
                         :if-exists :supersede)
    (write `(:source-hash ,hash
             :timestamp ,(get-universal-time)
             :sbcl-version ,(lisp-implementation-version))
           :stream stream)))

(defun cache-valid-p ()
  "Check if boot cache is valid"
  (let ((manifest (load-manifest)))
    (and manifest
         (probe-file *epsilon-core-cache*)
         (equal (getf manifest :source-hash)
                (calculate-source-hash))
         (equal (getf manifest :sbcl-version)
                (lisp-implementation-version)))))

(defun create-boot-cache ()
  "Create combined FASL for fast loading"
  (format t "~&;;; Creating boot cache for epsilon.core...~%")
  (let ((temp-dir (merge-pathnames 
                   (format nil "epsilon-boot-~D/" (random 1000000))
                   (merge-pathnames "tmp/" (user-homedir-pathname))))
        (fasl-files '()))
    
    (ensure-directories-exist temp-dir)
    
    (unwind-protect
         (progn
           ;; Compile all files
           (dolist (file *bootstrap-files*)
             (let* ((source-path (merge-pathnames 
                                  (concatenate 'string file ".lisp")
                                  *module-core*))
                    (fasl-path (merge-pathnames
                                (concatenate 'string file ".fasl")
                                temp-dir)))
               (ensure-directories-exist fasl-path)
               (format t "~&;;; Compiling ~A...~%" file)
               (compile-file source-path 
                            :output-file fasl-path
                            :verbose nil 
                            :print nil)
               (push fasl-path fasl-files)))
           
           ;; Create combined FASL
           (format t "~&;;; Creating combined FASL...~%")
           (with-open-file (output *epsilon-core-cache*
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
             ;; Write FASL header
             (write-sequence #.(coerce '(35 33 47 117 115 114 47 98 105 110 47 
                                        101 110 118 32 115 98 99 108 10)
                                      '(vector (unsigned-byte 8)))
                            output)
             
             ;; Concatenate all FASLs
             (dolist (fasl (reverse fasl-files))
               (with-open-file (input fasl :element-type '(unsigned-byte 8))
                 (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
                   (loop for bytes-read = (read-sequence buffer input)
                         while (> bytes-read 0)
                         do (write-sequence buffer output :end bytes-read))))))
           
           ;; Save manifest
           (save-manifest (calculate-source-hash)))
      
      ;; Cleanup
      (when (probe-file temp-dir)
        #+sbcl (sb-ext:delete-directory temp-dir :recursive t)
        #-sbcl (error "Cleanup not implemented for this implementation")))))

(defun bootstrap-minimal ()
  "Bootstrap minimal files needed for boot system"
  (format t "~&;;; Bootstrapping epsilon (minimal)...~%")
  (with-open-file (log "target/boot.log" 
                      :direction :output 
                      :if-exists :supersede)
    (dolist (file *bootstrap-files*)
      (let ((source-path (merge-pathnames 
                          (concatenate 'string file ".lisp")
                          *module-core*)))
        (format t "~&;;; Loading ~A...~%" file)
        (handler-case
            (let ((fasl-path 
                   (let ((*standard-output* log)
                         (*error-output* log))
                     (compile-file source-path :print nil :verbose nil))))
              (load fasl-path))
          (error (e)
            (format t "~&;;; ERROR compiling ~A: ~A~%" file e)
            (error e)))))))

(defun fast-boot ()
  "Fast boot with cache support"
  (ensure-directories)
  
  (cond
    ;; Case 1: Valid cache exists - fastest path
    ((cache-valid-p)
     (format t "~&;;; Fast boot: Loading epsilon.core from cache~%")
     (let ((start-time (get-internal-real-time)))
       (load *epsilon-core-cache*)
       (format t "~&;;; Boot completed in ~,3F seconds~%"
               (/ (- (get-internal-real-time) start-time)
                  internal-time-units-per-second))))
    
    ;; Case 2: No valid cache - bootstrap and create cache
    (t
     (format t "~&;;; No valid cache found, bootstrapping...~%")
     (let ((start-time (get-internal-real-time)))
       (bootstrap-minimal)
       (create-boot-cache)
       (format t "~&;;; Bootstrap completed in ~,3F seconds~%"
               (/ (- (get-internal-real-time) start-time)
                  internal-time-units-per-second))))))

;; For backward compatibility
(defun boot ()
  (fast-boot))