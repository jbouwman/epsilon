(defpackage #:epsilon.lib.stream
  (:use
   #:cl
   #:sb-gray
   #:epsilon.lib.type)
  (:local-nicknames
   (#:char #:epsilon.lib.char))
  (:export
   #:buffer
   #:make-binary-input-stream
   #:make-binary-output-stream
   #:make-character-input-stream
   #:make-character-output-stream))

(in-package #:epsilon.lib.stream)

;; row,col position tracking

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

;; character stream

(defclass character-stream (fundamental-character-output-stream)
  ((stream :initarg :stream)
   (encoding :initarg :encoding)))

(defmethod stream-write-char ((stream character-stream) char)
  ;; TODO writeme
  )

(defmethod stream-write-string ((stream character-stream) string &optional (start 0) end)
  ;; TODO writeme
  )

(defun make-character-stream (stream &key (encoding (char:make-encoding :utf-8)))
  (unless (streamp stream)
    (error "not a stream"))
  (unless (open-stream-p stream)
    (error 'stream-closed :stream stream))
  (make-instance 'character-stream
                 :stream stream
                 :encoding encoding))

(defclass binary-stream ()
  ((buffer :accessor buffer
           :initarg :buffer
           :type ->u8)))

(defmethod sb-gray::stream-element-type ((stream binary-stream))
  'u8)

;;; input streams

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

(defun make-binary-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with bytes instead of characters."
  (declare (type ->u8 buffer)
           (type array-index start)
           (type (or array-index null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'binary-input-stream
                   :buffer buffer :index start :end end)))

(defmacro with-binary-input-stream ((var buffer &optional (start 0) end) &body body)
  `(with-open-stream (,var (make-binary-input-stream ,buffer ,start ,end))
     ,@body))


;;; output streams

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

(defun make-binary-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with bytes instead of characters."
  (make-instance 'binary-output-stream
                 :buffer (make-array 128 :element-type 'u8)))

(defmacro with-binary-output-stream ((var) &body body)
  `(with-open-stream (,var (make-binary-output-stream))
     ,@body
     (get-output-stream-bytes ,var)))
