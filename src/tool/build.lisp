(defpackage #:tool.build
  (:use
   #:cl
   #:lib.url
   #:sys.fs)
  (:export
   #:build))

(in-package #:tool.build)

;;; Generates an ordered list of build steps for a module, given the
;;; repository root URL.  Uses `compute-build-order` to determine the
;;; build order and dependencies of the source files.  Checks the hash
;;; of each file's contents against the build products to determine if
;;; compilation is needed.  Returns a list of `:compile` and `:load`
;;; steps in the correct order to build the module.

(defun build-module (repo-root-url)
 (let ((build-order (compute-build-order repo-root-url))
       (build-steps '()))
   (labels ((process-file (file-entry &optional changed-files)
              (destructuring-bind (file-url . dependencies) file-entry
                (let* ((file-path (lib.url:uri-path file-url))
                       (file-hash (sha256-file file-path))
                       (build-product (get-build-product file-hash)))
                  (if (or (null build-product)
                          (some (lambda (dep-url)
                                  (member (lib.url:uri-path dep-url) changed-files :test #'equal))
                                dependencies))
                      (progn
                        (push (list :compile file-url) build-steps)
                        (push file-path changed-files))
                      (push (list :load (slot-value build-product 'fasl-url)) build-steps)))))
            (process-dependencies (file-entries changed-files)
              (dolist (file-entry file-entries)
                (process-file file-entry changed-files))))
     (process-dependencies build-order '())
     (nreverse build-steps))))


(defclass build-file ()
  (source-url :initarg :source
   fasl-url :initarg :fasl
   ))

(defun build (build-file)
  )
