(defpackage #:epsilon.lib.stream-2
  (:use #:cl
        #:sb-gray)
  (:local-nicknames
   (#:type #:epsilon.lib.type))
  (:export
   #:buffer
   #:make-binary-stream))

(in-package #:epsilon.lib.stream-2)

;; row/column position tracking

(defclass positioned-stream (fundamental-character-output-stream)
  ((column :initform 0
           :accessor output-column)
   (stream :initarg :stream
           :initform (error "missing stream"))))

(defmethod stream-write-sequence ((s positioned-stream) seq &optional start end)
  "Write SEQ to stream S."
  (let ((newline-pos (position #\Newline seq :from-end t)))
    (when newline-pos
      (setf (output-column s) (- (length seq) newline-pos 1))))
  (write-sequence seq (slot-value s 'stream) :start start :end end))

(defmethod stream-line-column ((s positioned-stream))
  "Tell column number that stream S is currently at."
  (output-column s))

(defmethod stream-start-line-p ((s positioned-stream))
  "Tell if stream S is already at start of fresh new line."
  (zerop (output-column s)))

(defmethod stream-write-char ((s positioned-stream) char)
  "Write CHAR to stream S."
  (if (char= char #\Newline)
      (setf (output-column s) 0)
      (incf (output-column s)))
  (write-char char (slot-value s 'stream)))

;; general purpose binary queue

(defclass binary-stream (fundamental-binary-stream)
  ((buffer :accessor buffer)
   (read-position :initform 0 
                  :accessor read-position)))

(defun make-binary-stream (&key initial-size)
  (make-instance 'binary-stream
                 :initial-size initial-size))

(defmethod initialize-instance :after ((stream binary-stream) &key initial-size)
  (setf (buffer stream)
        (make-array (or initial-size 16)
                    :fill-pointer 0
                    :element-type 'u8)))

(defmethod stream-write-byte ((stream binary-stream) byte)
  (vector-push-extend byte (buffer stream))
  byte)

(defmethod stream-write-sequence ((stream binary-stream) sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (loop for i from start below end
          do (vector-push-extend (elt sequence i)
                                 (buffer stream)))
    sequence))

(defmethod read-byte-from-stream ((stream binary-stream) &optional (eof-error-p t) eof-value)
  (with-slots (buffer read-position) stream
    (if (< read-position (length buffer))
        (prog1 (aref buffer read-position)
          (incf read-position))
        (if eof-error-p
            (error "End of binary stream")
            eof-value))))

(defmethod available-bytes ((stream binary-stream))
  (- (length (buffer stream)) (read-position stream)))


;; vvv wrong TODO
(defmethod read-sequence-from-stream ((stream binary-stream) sequence &key (start 0) end)
  (let* ((end (or end (length sequence)))
         (available (available-bytes stream))
         (to-read (min available (- end start))))
    (with-slots (buffer read-position) stream
      (loop for i from 0 below to-read
            for seq-pos = (+ start i)
            do (setf (elt sequence seq-pos) 
                     (aref buffer (+ read-position i))))
      (incf read-position to-read)
      (+ start to-read))))

(defmethod reset-stream ((stream binary-stream))
  (setf (read-position stream) 0)
  (setf (fill-pointer (buffer stream)) 0))

(defmethod compact-stream ((stream binary-stream))
  "Remove already-read bytes from the buffer"
  (with-slots (buffer read-position) stream
    (when (> read-position 0)
      (let ((remaining (- (length buffer) read-position)))
        (loop for i from 0 below remaining
              do (setf (aref buffer i) (aref buffer (+ i read-position))))
        (setf (fill-pointer buffer) remaining)
        (setf read-position 0)))))
