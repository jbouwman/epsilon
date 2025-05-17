(defpackage #:epsilon.lib.stream
  (:use
   #:cl
   #:sb-gray
   #:epsilon.lib.type)
  (:export
   #:buffer
   #:copy-stream
   #:with-input-stream
   #:with-output-stream
   #:make-input-stream
   #:make-output-stream))

(in-package #:epsilon.lib.stream)

(defclass binary-stream ()
  ((buffer :accessor buffer
           :initarg :buffer
           :type ->u8)))

(defmethod sb-gray::stream-element-type ((stream binary-stream))
  'u8)

;; input stream

(defclass binary-input-stream (binary-stream fundamental-binary-input-stream)
  ((index :accessor index :initarg :index :type array-index)
   (end :accessor end :initarg :end :type array-index)))

(defmethod stream-read-byte ((stream binary-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (declare (type ->u8 buffer))
    (cond
      ((>= index (end stream)) :eof)
      (t
       (setf (index stream) (1+ index))
       (aref buffer index)))))

(defmethod stream-read-sequence ((stream binary-input-stream) seq &optional (start 0) end)
  (typecase seq
    (->u8
     (let ((end (or end (length seq))))
       (let ((buffer (buffer stream))
             (index (index stream))
             (buffer-end (end stream)))
         (declare (type ->u8 buffer))
         (let* ((remaining (- buffer-end index))
                (length (- end start))
                (amount (min remaining length)))
           (replace seq buffer :start1 start :end1 end :start2 index :end2
                    buffer-end)
           (setf (index stream) (+ index amount))
           (+ start amount)))))
    (t (call-next-method))))

(defun make-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with bytes instead of characters."
  (declare (type ->u8 buffer)
           (type array-index start)
           (type (or array-index null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'binary-input-stream
                   :buffer buffer :index start :end end)))

(defmacro with-input-stream ((var buffer &optional (start 0) end) &body body)
  `(with-open-stream (,var (make-input-stream ,buffer ,start ,end))
     ,@body))

;;; output stream

(defclass binary-output-stream (binary-stream fundamental-binary-output-stream)
  ((index :accessor index :initform 0 :type array-index)))

(defmethod stream-write-byte ((stream binary-output-stream) integer)
  (declare (type u8 integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream)))
    (declare (type ->u8 buffer))
    (when (>= index (length buffer))
      (let ((new-buffer (make-array (* 2 length)
                                    :element-type 'u8)))
        (declare (type ->u8 new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

(defmethod stream-write-sequence ((stream binary-output-stream) seq &optional (start 0) end)
  (typecase seq
    (->u8
     (let ((end (or end (length seq))))
       (let* ((buffer (buffer stream))
              (length (length buffer))
              (index (index stream))
              (amount (- end start)))
         (declare (type ->u8 buffer))
         (when (>= (+ index amount) length)
           (let ((new-buffer
                   (make-array (* 2 (max amount length)) :element-type
                               'u8)))
             (declare (type ->u8 new-buffer))
             (replace new-buffer buffer)
             (setf buffer new-buffer
                   (buffer stream) new-buffer)))
         (replace buffer seq :start1 index :start2 start :end2 end)
         (incf (index stream) amount)
         seq)))
    (t (call-next-method))))

(defmethod sb-gray::stream-clear-output ((stream binary-output-stream))
  (setf (index stream) 0)
  nil)

(defun get-output-stream-bytes (stream)
  "As GET-OUTPUT-STREAM-STRING, only with an octet output-stream instead
of a string output-stream."
  (let ((buffer (buffer stream))
        (index (index stream)))
    (setf (index stream) 0)
    (subseq buffer 0 index)))

(defun make-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with bytes instead of characters."
  (make-instance 'binary-output-stream
                 :buffer (make-array 128 :element-type 'u8)))

(defmacro with-output-stream ((var) &body body)
  `(with-open-stream (,var (make-output-stream))
     ,@body
     (get-output-stream-bytes ,var)))

(defun copy-stream (input output &key (element-type (stream-element-type input))
                                   (buffer-size 4096)
                                   (buffer (make-array buffer-size :element-type element-type))
                                   (start 0) end
                                   finish-output)
  "Reads data from INPUT and writes it to OUTPUT. Both INPUT and OUTPUT must
be streams, they will be passed to READ-SEQUENCE and WRITE-SEQUENCE and must have
compatible element-types."
  (when (and end
             (< end start))
    (error "END is smaller than START in ~S" 'copy-stream))
  (let ((output-position 0)
        (input-position 0))
    (unless (zerop start)
      (loop while (< input-position start)
            do (let ((n (read-sequence buffer input
                                       :end (min (length buffer)
                                                 (- start input-position)))))
                 (when (zerop n)
                   (error "~@<Could not read enough bytes from the input to fulfill ~
                           the :START ~S requirement in ~S.~:@>" 'copy-stream start))
                 (incf input-position n))))
    (assert (= input-position start))
    (loop while (or (null end) (< input-position end))
          do (let ((n (read-sequence buffer input
                                     :end (when end
                                            (min (length buffer)
                                                 (- end input-position))))))
               (when (zerop n)
                 (if end
                     (error "~@<Could not read enough bytes from the input to fulfill ~
                          the :END ~S requirement in ~S.~:@>" 'copy-stream end)
                     (return)))
               (incf input-position n)
               (write-sequence buffer output :end n)
               (incf output-position n)))
    (when finish-output
      (finish-output output))
    output-position))
