(defpackage #:lib.stream
  (:use
   #:cl
   #:sb-gray
   #:lib.binding
   #:lib.char
   #:lib.symbol
   #:lib.type)
  (:export
   #:binary-stream
   #:char-stream
   #:char-stream-column
   #:output-stream-vector
   #:make-char-stream
   #:make-vector-stream
   #:peek-byte
   #:stream-closed
   #:stream-encoding
   #:stream-exhausted
   #:stream-position
   #:unread-byte
   #:vector-stream
   #:with-input-from-chars
   #:with-input-from-vector))

(in-package #:lib.stream)

(defgeneric char-to-octets (format char writer))

(defgeneric write-sequence* (format stream sequence start end))

(defgeneric string-to-octets* (format string start end))

(defgeneric write-byte* (byte stream))

(defgeneric peek-byte (stream &optional peek-type eof-err-p eof-value))

(defconstant +lf+ (char-code #\Linefeed))

(defconstant +cr+ (char-code #\Return))

(defvar *current-unreader* nil
  "A unary function which might be called to `unread' a character
\(i.e. the sequence of octets it represents).

Used by the function OCTETS-TO-CHAR-CODE and must always be bound to a
suitable functional object when this function is called.")
    
(defvar *default-eol-style*
  #+:win32 :crlf
  #-:win32 :lf
  "The end-of-line style used by external formats if none is
explicitly given.  Depends on the OS the code is compiled on.")

(defvar *default-little-endian*
  #+:little-endian t
  #-:little-endian nil
  "Whether external formats are little-endian by default
\(i.e. unless explicitly specified).  Depends on the platform
the code is compiled on.")

(defconstant +buffer-size+ 8192
  "Default size for buffers used for internal purposes.")

(define-condition stream-closed (stream-error)
  ()
  (:documentation "Signaled on write to a cloaed stream.")
  (:report
   (lambda (condition stream)
     (format stream "Stream ~S is closed."
             (stream-error-stream condition)))))

(define-condition stream-exhausted (stream-error)
  ()
  (:documentation "Signaled on read form an empty stream.")
  (:report
   (lambda (condition stream)
     (format stream "Stream ~S exhausted."
             (stream-error-stream condition)))))

(defclass vector-stream ()
  ((open :initform t)))

(defmethod stream-element-type ((stream vector-stream))
  'u8)
 
(defmacro with-open-vector-stream (stream &body body)
  `(progn
     (unless (slot-value ,stream 'open)
       (error 'closed-stream :stream ,stream))
     ,@body))

(defclass vector-input-stream (vector-stream fundamental-binary-input-stream)
  ((input-vector :initarg :input)
   (index :initarg :index
          :reader stream-position
          :type array-index)
   (end :initarg :end
        :reader stream-end
        :type array-index
        :documentation "end of available data")))

(defmethod peek-byte ((stream vector-input-stream) &optional peek-type (eof-error-p t) eof-value)
  (with-open-vector-stream stream
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

(defmethod stream-read-byte ((stream vector-input-stream))
  (with-open-vector-stream stream
    (with-slots (open index end input-vector) stream
      (cond ((< index end)
             (incf index)
             (aref input-vector (1- index)))
            (t :eof)))))

(defmethod stream-listen ((stream vector-input-stream))
  (with-open-vector-stream stream
    (with-slots (index end) stream
      (< index end))))
  
(defmethod stream-read-sequence ((stream vector-input-stream) sequence &optional (start 0) end)
  (with-open-vector-stream stream
    (with-slots ((vector-index index) (vector-end end) input-vector) stream
      (loop :for index :from start :below end
            :while (< vector-index vector-end)
            :do (setf (elt sequence index) (aref input-vector vector-index))
                (incf vector-index)
            :finally (return index)))))

(defmethod stream-file-position ((stream vector-input-stream) &optional position)
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

(defun %output-vector ()
  (make-array 0 :adjustable t
                :fill-pointer 0
                :element-type 'u8))

(defclass vector-output-stream (vector-stream fundamental-binary-output-stream)
  ((output-vector :initform (%output-vector))))

(defmethod stream-write-byte ((stream vector-output-stream) byte)
  (with-open-vector-stream stream
    (vector-push-extend byte (slot-value stream 'output-vector))))

(defmethod stream-write-sequence ((stream vector-output-stream) sequence
                                  &optional (start 0) (end (length sequence)))
  (with-open-vector-stream stream
    (with-slots (output-vector) stream
      (loop :for index :from start :below end 
            :do (vector-push-extend (elt sequence index) output-vector)))
    sequence))

(defmethod stream-file-position ((stream vector-output-stream) &optional position)
  (with-slots (output-vector) stream
    (unless position
      (return-from stream-file-position
        (fill-pointer output-vector)))
    (let* ((total-size (array-total-size output-vector))
           (new-fill-pointer
             (case position
               (:start 0)
               (:end (return-from stream-file-position total-size))
               (otherwise
                (unless (integerp position)
                  (error 'stream-error
                         :format-control "Unknown file position designator: ~S."
                         :format-arguments (list position)
                         :stream stream))
                (unless (<= 0 position array-total-size-limit)
                  (error 'stream-error
                         :format-control "File position designator ~S is out of bounds."
                         :format-arguments (list position)
                         :stream stream))
                position))))
      (when (> new-fill-pointer total-size)
        (adjust-array output-vector new-fill-pointer))
      (setf (fill-pointer output-vector) new-fill-pointer)
      position)))

(defmethod output-stream-vector ((stream vector-output-stream))
  (with-slots (output-vector) stream
    (prog1 output-vector
      (setf output-vector (%output-vector)))))

(defclass vector-bidirectional-stream (vector-output-stream vector-input-stream)
  ())

(defclass char-stream ()
  ((stream :initarg :stream
           :reader binary-stream)
   (encoding :initform (make-encoding :iso-8859-1)
             :initarg :encoding
             :reader stream-encoding)))

(defmethod stream-element-type ((stream char-stream))
  'character)

(defmethod close ((stream char-stream) &key abort)
  (with-slots (stream) stream
    (cond ((open-stream-p stream)
           (close stream :abort abort))
          (t nil))))

(defmethod open-stream-p ((stream char-stream))
  (with-slots (stream) stream
    (open-stream-p stream)))

(defmethod stream-file-position ((stream char-stream) &optional position)
  (with-slots (stream) stream
    (file-position stream position)))

(defclass output-stream (char-stream fundamental-binary-output-stream
                         fundamental-character-output-stream)
  ((column :initform 0
           :accessor char-stream-column)))

(defmethod write-byte* (byte (stream output-stream)) ; write-u8
  (with-slots (binary-stream) stream
    (write-byte byte binary-stream)))

(defmethod stream-write-char ((stream output-stream) char)
  (with-slots (encoding) stream
    (flet ((writer (octet)
             (write-byte* octet stream)))
      (declare (dynamic-extent (function writer)))
      (char-to-octets encoding char #'writer))))

;; update the column unless we're in the middle of the line and
;; the current value is NIL

(defmethod stream-write-char :after ((stream output-stream) char)
  (with-slots (column) stream
    (cond ((char= char #\Newline) (setq column 0))
          (column (incf (the integer column))))))

(defmethod stream-clear-output ((stream output-stream))
  (with-slots (binary-stream) stream
    (clear-output binary-stream)))

(defmethod stream-finish-output ((stream output-stream))
  (with-slots (binary-stream) stream
    (finish-output binary-stream)))

(defmethod stream-force-output ((stream output-stream))
  (with-slots (binary-stream) stream
    (force-output binary-stream)))

(defmethod stream-line-column ((stream output-stream))
  (slot-value stream 'column))

(defmethod stream-write-byte ((stream output-stream) byte)
  (with-slots (column) stream
    ;; set column to NIL because we don't know how to handle binary
    ;; output mixed with character output
    (setq column nil)
    (write-byte* byte stream)))

(defmethod stream-write-sequence ((stream output-stream) sequence
                                  &optional (start 0) (end (length sequence)))
  (with-slots (column encoding binary-stream) stream
    (when (and end (>= start end))
      (return-from stream-write-sequence sequence))
    (write-sequence* encoding stream sequence start end)) ; <-- delegation to the encoding; replace with #:lib.char
  sequence)

(defmethod stream-write-string ((stream output-stream) string &optional (start 0) (end (length string)))
  (stream-write-sequence stream string start (or end (length string))))

(defclass input-stream (char-stream
                        fundamental-binary-input-stream
                        fundamental-character-input-stream)
  ((last-char-code :initform nil
                   :accessor char-stream-last-char-code
                   :documentation "This slot either holds NIL or the
last character \(code) read successfully.  This is mainly used for
UNREAD-CHAR sanity checks.")
   (last-octet :initform nil
               :accessor char-stream-last-octet
               :documentation "This slot either holds NIL or the last
octet read successfully from the stream using a `binary' operation
such as READ-BYTE.  This is mainly used for UNREAD-BYTE sanity
checks.")
   (octet-stack :initform nil
                :accessor char-stream-octet-stack
                :documentation "A small buffer which holds octets
that were already read from the underlying stream but not yet
used to produce characters.  This is mainly used if we have to
look ahead for a CR/LF line ending.")
   (position :initform 0
             :initarg :position
             :type integer
             :accessor char-stream-position
             :documentation "The position within the stream where each
octet read counts as one.")))

(defmethod read-byte* ((stream input-stream))
  (with-slots (position octet-stack binary-stream)
      stream
    (declare (integer position))
    (incf position)
    (or (pop octet-stack)
        (read-byte binary-stream nil nil)
        (progn (decf position) nil))))

(defmethod stream-clear-input ((stream input-stream))
  "Calls the corresponding method for the underlying input stream
and also clears the value of the OCTET-STACK slot."
  (with-slots (octet-stack binary-stream) stream
    (setq octet-stack nil)
    (clear-input binary-stream)))

(defmethod stream-listen ((stream input-stream))
  "Calls the corresponding method for the underlying input stream
but first checks if \(old) input is available in the OCTET-STACK
slot."
  (with-slots (position octet-stack binary-stream)
      stream
    (declare (integer position))
    (or octet-stack (listen binary-stream))))

(defmethod stream-read-byte ((stream input-stream))
  "Reads one byte \(octet) from the underlying stream."
  (with-slots (last-char-code last-octet) stream
    (setq last-char-code nil)
    (let ((octet (read-byte* stream)))
      (setq last-octet octet)
      (or octet :eof))))

(defun unread-char% (char stream)
  "Used internally to put a character CHAR which was already read back
on the stream.  Uses the OCTET-STACK slot and decrements the POSITION
slot accordingly."
  (with-slots (position octet-stack encoding) stream
    (let ((counter 0) octets-reversed)
      (declare (fixnum counter))
      (flet ((writer (octet)
               (incf counter)
               (push octet octets-reversed)))
        (declare (dynamic-extent (function writer)))
        (char-to-octets encoding char #'writer)
        (decf position counter)
        (setq octet-stack (nreconc octets-reversed octet-stack))))))

(defmethod stream-read-char ((stream input-stream))
  (with-slots (encoding last-octet last-char-code) stream
    (setq last-octet nil)
    (flet ((reader ()
             (read-byte* stream))
           (unreader (char)
             (unread-char% char stream)))
      (declare (dynamic-extent (function reader) (function unreader)))
      (let* ((*current-unreader* #'unreader)
             (char-code (or (octets-to-char-code encoding #'reader)
                            (return-from stream-read-char :eof))))
        ;; remember this character and its char code for UNREAD-CHAR
        (setq last-char-code char-code)
        (or (code-char char-code) char-code)))))

(defmethod stream-read-char-no-hang ((stream input-stream))
  "Reads one character if the underlying stream has at least one
octet available."
  (and (stream-listen stream)
       (stream-read-char stream)))

(defmethod stream-read-sequence ((stream input-stream) sequence
                                 &optional (start 0) (end (length sequence)))
  (with-slots (encoding binary-stream) stream
    (when (>= start end)
      (return-from stream-read-sequence start))
    (read-sequence* encoding binary-stream sequence start end)))

(defmethod stream-unread-char ((stream input-stream) char)
  "Implements UNREAD-CHAR for streams of type INPUT-STREAM.
Makes sure CHAR will only be unread if it was the last character
read and if it was read with the same encoding that's currently
being used by the stream."
  (with-slots (last-char-code) stream
    (unless last-char-code
      (error 'stream-error
             :stream stream
             :format-control "No character to unread from this stream \(or external format has changed or last reading operation was binary)."))
    (unless (= (char-code char) last-char-code)
      (error 'stream-error
             :stream stream
             :format-control "Last character read (~S) was different from ~S."
             :format-arguments (list (code-char last-char-code) char)))
    (unread-char% char stream)
    (setq last-char-code nil)
    nil))

(defmethod unread-byte (byte (stream input-stream))
  "Similar to UNREAD-CHAR in that it `unreads' the last octet from
STREAM.  Note that you can only call UNREAD-BYTE after a corresponding
READ-BYTE."
  (with-slots (last-octet octet-stack position) stream
    (unless last-octet
      (error 'stream-error
             :stream stream
             :format-control "No byte to unread from this stream \(or last reading operation read a character)."))
    (unless (= byte last-octet)
      (error 'stream-error
             :stream stream
             :format-control "Last byte read was different from #x~X."
             :format-arguments (list byte)))
    (setq last-octet nil)
    (decf (the integer position))
    (push byte octet-stack)
    nil))

(defmethod peek-byte ((stream input-stream)
                      &optional peek-type (eof-error-p t) eof-value)
  "Returns an octet from STREAM without actually removing it."
  (loop :for octet := (read-byte stream eof-error-p :eof)
        :until (cond ((eq octet :eof)
                      (return eof-value))
                     ((null peek-type))
                     ((eq peek-type t)
                      (plusp octet))
                     ((= octet peek-type)))
        :finally (unread-byte octet stream)
                 (return octet)))

(defclass bidirectional-stream (input-stream output-stream)
  ())

(defun maybe-rewind (stream octets)
  "Tries to `rewind' the \(binary) stream STREAM by OCTETS octets.
Returns T if it succeeds, otherwise NIL."
  (when-let (position (file-position stream))
    (if (file-position stream (- position octets)) t nil)))

(defmethod reset-input-state ((stream bidirectional-stream))
  (with-slots (last-char-code last-octet octet-stack binary-stream)
      stream
    (when octet-stack
      (unless (maybe-rewind stream (length octet-stack))
        (error 'char-stream-out-of-sync-error
               :stream binary-stream))
      (setq octet-stack nil))
    (setq last-octet nil
          last-char-code nil)))

(defmethod stream-write-byte :before ((stream bidirectional-stream) byte)
  (declare (ignore byte))
  (reset-input-state stream))
  
(defmethod stream-write-char :before ((stream bidirectional-stream) char)
  (declare (ignore char))
  (reset-input-state stream))
  
(defmethod stream-write-sequence :before ((stream bidirectional-stream) sequence &optional start end)
  (declare (ignore sequence start end))
  (reset-input-state stream))
  
(defmethod stream-clear-output :before ((stream bidirectional-stream))
  (reset-input-state stream))

(defmethod reset-output-state ((stream bidirectional-stream))
  (with-slots (column) stream
    (setq column nil)))
  
(defmethod stream-read-byte :before ((stream bidirectional-stream))
  (reset-output-state stream))
  
(defmethod stream-read-char :before ((stream bidirectional-stream))
  (reset-output-state stream))

(defmethod stream-read-sequence :before ((stream bidirectional-stream) sequence &optional start end)
  (declare (ignore sequence start end))
  (reset-output-state stream))

(defmethod stream-unread-char :before ((stream bidirectional-stream) char)
  (declare (ignore char))
  (reset-output-state stream))
  
(defmethod unread-byte :before (byte (stream bidirectional-stream))
  (declare (ignore byte))
  (reset-output-state stream))
  
(defmethod stream-clear-input :before ((stream bidirectional-stream))
  (reset-output-state stream))

(defmethod write-byte* :after (byte (stream bidirectional-stream))
  "Keep POSITION slot up to date even when performing output."
  (declare (ignore byte))
  (with-slots (stream-position) stream
    (incf stream-position)))

(defun make-char-stream (stream &key (encoding (make-encoding :ucs-2)))
  (unless (streamp stream)
    (error "not a stream"))
  (unless (open-stream-p stream)
    (error 'stream-closed :stream stream))
  (let ((in (input-stream-p stream))
        (out (output-stream-p stream)))
    (apply #'make-instance
           (cond ((and in out)
                  'bidirectional-stream)
                 (in
                  'input-stream)
                 (out
                  'output-stream))
           :stream stream
           :encoding encoding)))

(defun make-vector-stream (&key input output)
  (cond ((and input output)
         (make-instance 'vector-bidirectional-stream :input input))
        (input
         (make-instance 'vector-input-stream :input input))
        (t
         (make-instance 'vector-output-stream))))

(defmacro with-input-from-vector ((stream vector) &body body)
  `(let ((,stream (make-vector-stream :input ,vector)))
     (unwind-protect
          (progn ,@body)
       (close ,stream))))

(defmacro with-input-from-chars ((stream string) &body body)
  `(with-input-from-string (,stream ,string)
     (let ((,stream (make-char-stream :input ,stream)))
       (unwind-protect
            (progn ,@body)
         (close ,stream)))))
