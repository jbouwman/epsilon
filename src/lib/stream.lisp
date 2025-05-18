(defpackage :epsilon.lib.stream
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.char
   :epsilon.lib.list
   :epsilon.lib.symbol
   :epsilon.lib.type)
  (:export

   ;; NEW
   
   :buffer
   :make-output-stream
   :make-input-stream
   :copy-stream))

(in-package :epsilon.lib.stream)

(defclass binary-stream ()
  ((open :initform t)))

(defmethod stream-element-type ((stream binary-stream))
  'u8)
 
(defmacro with-open-binary-stream (stream &body body)
  `(progn
     (unless (slot-value ,stream 'open)
       (error 'closed-stream :stream ,stream))
     ,@body))

(defclass binary-input-stream (binary-stream fundamental-binary-input-stream)
  ((input-vector :initarg :input)
   (index :initarg :index
          :initform 0
          :reader stream-position
          :type array-index)
   (end :initarg :end
        :reader stream-end
        :type array-index
        :documentation "end of available data")))

(defun make-input-stream (input)
  (make-instance 'binary-input-stream :input input :end (length input)))

(defmethod peek-byte ((stream binary-input-stream) &optional peek-type (eof-error-p t) eof-value)
  (with-open-binary-stream stream
    (with-slots (index) stream
      (loop :for byte := (read-byte stream eof-error-p :eof)
            :for new-index :from index
            :until (cond ((eq byte :eof)
                          (return eof-value))
                         ((null peek-type))
                         ((eq peek-type 't)
                          (plusp byte))
                         ((= byte peek-type)))
            :finally (setf index new-index)
                     (return byte)))))

(defmethod stream-read-byte ((stream binary-input-stream))
  (with-open-binary-stream stream
    (with-slots (open index end input-vector) stream
      (cond ((< index end)
             (incf index)
             (aref input-vector (1- index)))
            (t :eof)))))

(defmethod stream-listen ((stream binary-input-stream))
  (with-open-binary-stream stream
    (with-slots (index end) stream
      (< index end))))
  
(defmethod stream-read-sequence ((stream binary-input-stream) sequence &optional (start 0) end)
  (with-open-binary-stream stream
    (with-slots ((vector-index index) (vector-end end) input-vector) stream
      (loop :for index :from start :below (or end (length sequence))
            :while (< vector-index vector-end)
            :do (setf (elt sequence index) (aref input-vector vector-index))
                (incf vector-index)
            :finally (return index)))))

(defmethod stream-file-position ((stream binary-input-stream) &optional position)
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
  (make-array initial-length :element-type 'u8 
                             :fill-pointer 0 :adjustable t))

(defclass binary-output-stream (fundamental-binary-output-stream)
  ((buffer :initform (make-buffer)
           :accessor buffer)
   (position :initform 0
             :accessor stream-position)))

(defun make-output-stream ()
  (make-instance 'binary-output-stream))

(defmethod stream-write-byte ((stream binary-output-stream) byte)
  (vector-push-extend byte (buffer stream))
  (incf (stream-position stream))
  byte)

(defmethod stream-write-sequence ((stream binary-output-stream) seq
                                  &optional (start 0) end)
  (let* ((buffer (buffer stream))
         (end (or end (length seq)))
         (count (- end start))
         (current-size (length buffer))
         (new-size (+ current-size count)))
    (adjust-array buffer new-size :fill-pointer new-size)
    (replace buffer seq :start1 current-size :start2 start :end2 end)
    (incf (stream-position stream) count))
  seq)

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
      ;; FIXME add platform specific optimization to skip seekable streams
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
