;;;;  TODO
;;;;
;;;;  1. Limited Gray Stream Implementation
;;;;  Currently only implements binary streams, but many epsilon modules work with
;;;;  character data. Consider adding character stream support.
;;;;
;;;;  2. Minimal Stream Types
;;;;  Only provides basic input/output streams. Could be extended with specialized streams
;;;;  for common patterns (buffering, filtering, transformation).
;;;;
;;;;  3. Buffer Management
;;;;  The buffer export suggests buffer management capabilities that could be more
;;;;  fully developed for memory-efficient stream processing.
;;;;
;;;;  Reusable Components for Epsilon System
;;;;
;;;;  Character Stream Support
;;;;  Adding character stream implementations would benefit JSON, YAML, regex, and
;;;;  other text processing modules currently using string-based parsing.
;;;;
;;;;  Stream Transformation Pipeline
;;;;  A composable stream transformation system could support codec, compression,
;;;;  and other data processing pipelines throughout epsilon.
;;;;
;;;;  Memory-Mapped Stream Support
;;;;  For large file processing in archive, codec, and other modules, memory-mapped
;;;;  stream support could improve performance.
;;;;
;;;;  Stream Position Management
;;;;  Enhanced position tracking and seeking could benefit parsers that need
;;;;  backtracking or multi-pass processing.
;;;;

(defpackage :epsilon.stream
  (:use
   :cl
   :sb-gray
   :epsilon.syntax
   :epsilon.char
   :epsilon.list
   :epsilon.symbol
   :epsilon.type)
  (:export

   ;; NEW
   
   :buffer
   :make-output-stream
   :make-input-stream))

(in-package :epsilon.stream)

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
