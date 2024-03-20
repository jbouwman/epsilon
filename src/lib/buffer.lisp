(defpackage #:lib.buffer
  (:use #:cl
        #:lib.stream
        #:lib.type
        #:lib.xsubseq)
  (:export #:*default-memory-limit*
           #:*default-disk-limit*

           #:smart-buffer
           #:make-smart-buffer
           #:write-to-buffer
           #:finalize-buffer
           #:buffer-on-memory-p
           #:delete-stream-file

           #:buffer-limit-exceeded))

(in-package #:lib.buffer)

(defvar *default-memory-limit* (expt 2 20))
(defvar *default-disk-limit* (expt 2 30))

(defstruct (smart-buffer (:conc-name :buffer-)
                         (:constructor %make-smart-buffer))
  (memory-limit *default-memory-limit*)
  (disk-limit *default-disk-limit*)
  (current-len 0)
  (on-memory-p t)
  (memory-buffer (make-concatenated-xsubseqs))
  (disk-buffer nil))

(defun make-smart-buffer (&rest initargs &key memory-limit disk-limit &allow-other-keys)
  (let ((buffer (apply #'%make-smart-buffer initargs)))
    (when (and memory-limit
               disk-limit
               (< disk-limit memory-limit))
      (setf (buffer-memory-limit buffer) disk-limit))
    buffer))

(define-condition buffer-limit-exceeded (error)
  ((limit :initarg :limit
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Buffer exceeded the limit~:[~;~:*: ~A~]"
                     (slot-value condition 'limit)))))

(defun write-to-buffer (buffer seq &optional (start 0) (end (length seq)))
  (check-type seq (array u8 (*)))
  (incf (buffer-current-len buffer) (- end start))
  (if (buffer-on-memory-p buffer)
      (xnconcf (buffer-memory-buffer buffer) (xsubseq seq start end))
      (with-open-file (out (buffer-disk-buffer buffer)
                           :direction :output
                           :element-type 'u8
                           :if-exists :append)
        (write-sequence seq out :start start :end end))))

(defun finalize-buffer (buffer)
  (if (buffer-on-memory-p buffer)
      (make-vector-stream
       (typecase (buffer-memory-buffer buffer)
         (null-concatenated-xsubseqs #())
         (t (coerce-to-sequence (buffer-memory-buffer buffer)))))
      (open (buffer-disk-buffer buffer) :direction :input :element-type 'u8)))

(defun delete-stream-file (stream)
  (when (typep stream 'file-stream)
    (ignore-errors (delete-file (pathname stream))))
  (values))
