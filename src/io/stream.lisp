(in-package #:epsilon.io)

(defvar *stream-providers*
  (make-hash-table :test #'equalp))

(defstruct stream-provider
  input
  output)

(defun define-stream-provider (name in-f out-f)
  (setf (gethash name *stream-providers*)
        (make-stream-provider :input in-f
                              :output out-f)))

(define-stream-provider "file"
    (lambda (uri)
      (open (uri:path uri)
            :direction :input
            :element-type 'epsilon.lib.type:u8))
  (lambda (uri)
    (open (uri:path uri)
          :direction :output
          :element-type 'epsilon.lib.type:u8)))

(defun stream-provider (url)
  (or (gethash (uri:scheme url) *stream-providers*)
      (error "No stream provider for scheme ~A" (uri:scheme url))))

(defun open-stream (url)
  (funcall (stream-provider-input (stream-provider url)) url))

(defun read-bytes (url)
  (error "fixme"))

(defun open-text-stream (url)
  (epsilon.lib.stream:make-input-stream (open-stream url)))

(defun read-string (url)
  (with-open-stream (stream (open-stream url))
    (let ((input (epsilon.lib.stream:make-input-stream stream)))
      (with-output-to-string (output)
        (epsilon.lib.stream:copy-stream input output)))))
