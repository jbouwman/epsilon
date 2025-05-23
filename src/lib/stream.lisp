(defpackage :epsilon.lib.stream
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.type)
  (:export

   ;; NEW
   
   :buffer
   :make-output-stream
   :make-input-stream
   :copy-stream))

(in-package :epsilon.lib.stream)

(defclass binary-stream ()
  ((open :initform t
         :accessor stream-open-p))
  (:documentation "Base class for all binary streams."))

(defmethod stream-element-type ((stream binary-stream))
  'u8)

(defmacro with-open-binary-stream (stream &body body)
  "Execute BODY with STREAM, signaling an error if the stream is closed."
  `(progn
     (unless (stream-open-p ,stream)
       (error 'stream-closed :stream ,stream))
     ,@body))

;;; Binary input stream

(defclass binary-input-stream (binary-stream fundamental-binary-input-stream)
  ((input-vector :initarg :input
                :reader input-vector)
   (index :initarg :index
          :initform 0
          :reader stream-position
          :type array-index)
   (end :initarg :end
        :reader stream-end
        :type array-index
        :documentation "End of available data"))
  (:documentation "A binary input stream that reads from a byte vector."))

(defun make-input-stream (input)
  "Create a binary input stream from a byte vector."
  (make-instance 'binary-input-stream :input input :end (length input)))

(defmethod stream-read-byte ((stream binary-input-stream))
  "Read a byte from the stream."
  (with-open-binary-stream stream
    (with-slots (index end input-vector) stream
      (cond ((< index end)
             (prog1 (aref input-vector index)
               (incf index)))
            (t :eof)))))

(defmethod stream-listen ((stream binary-input-stream))
  "Check if there is data available to read."
  (with-open-binary-stream stream
    (with-slots (index end) stream
      (< index end))))

(defmethod stream-read-sequence ((stream binary-input-stream) sequence &optional (start 0) end)
  "Read a sequence of bytes from the stream."
  (with-open-binary-stream stream
    (with-slots ((vector-index index) (vector-end end) input-vector) stream
      (loop :for index :from start :below (or end (length sequence))
            :while (< vector-index vector-end)
            :do (setf (elt sequence index) (aref input-vector vector-index))
                (incf vector-index)
            :finally (return index)))))

(defmethod stream-file-position ((stream binary-input-stream) &optional position)
  "Get or set the file position."
  (with-slots (index end) stream
    (unless position
      (return-from stream-file-position index))
    (setq index
          (case position
            (:start 0)
            (:end end)
            (otherwise
             (unless (integerp position)
               (error 'stream-error
                      :format-control "Unknown file position designator: ~S."
                      :format-arguments (list position)
                      :stream stream))
             (unless (<= 0 position end)
               (error 'stream-error
                      :format-control "File position designator ~S is out of bounds."
                      :format-arguments (list position)
                      :stream stream))
             position)))
    position))

(defun make-buffer (&key (initial-length 32))
  "Create a new byte buffer."
  (make-array initial-length 
              :element-type 'u8 
              :fill-pointer 0 
              :adjustable t))

(defclass binary-output-stream (binary-stream fundamental-binary-output-stream)
  ((buffer :initform (make-buffer)
           :accessor buffer)
   (position :initform 0
             :accessor stream-position))
  (:documentation "A binary output stream that writes to an internal buffer."))

(defun make-output-stream ()
  "Create a new binary output stream."
  (make-instance 'binary-output-stream))

(defmethod stream-write-byte ((stream binary-output-stream) byte)
  "Write a byte to the stream."
  (with-open-binary-stream stream
    (vector-push-extend byte (buffer stream))
    (incf (stream-position stream))
    byte))

(defmethod stream-write-sequence ((stream binary-output-stream) seq
                                  &optional (start 0) end)
  "Write a sequence of bytes to the stream."
  (with-open-binary-stream stream
    (let* ((buffer (buffer stream))
           (end (or end (length seq)))
           (count (- end start))
           (current-size (length buffer))
           (new-size (+ current-size count)))
      (adjust-array buffer new-size :fill-pointer new-size)
      (replace buffer seq :start1 current-size :start2 start :end2 end)
      (incf (stream-position stream) count))
    seq))

(defun copy-stream (input output &key (element-type (stream-element-type input))
                   (buffer-size 4096)
                   (buffer (make-array buffer-size :element-type element-type))
                   (start 0) end
                   finish-output)
  "Copy data from one stream to another."
  (when (and end (< end start))
    (error "END is smaller than START in ~S" 'copy-stream))
  
  (let ((output-position 0)
        (input-position 0))
    ;; Skip to start position if needed
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
    
    ;; Copy data
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
