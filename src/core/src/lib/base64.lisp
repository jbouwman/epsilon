(defpackage #:epsilon.base64
  (:use
   #:cl
   #:epsilon.type)
  (:export
   #:base64-stream-to-integer
   #:base64-stream-to-string
   #:base64-stream-to-stream
   #:base64-stream-to-usb8-array
   #:base64-string-to-integer
   #:base64-string-to-string
   #:base64-string-to-stream
   #:base64-string-to-usb8-array
   #:string-to-base64-string
   #:string-to-base64-stream
   #:usb8-array-to-base64-string
   #:usb8-array-to-base64-stream
   #:stream-to-base64-string
   #:stream-to-base64-stream
   #:integer-to-base64-string
   #:integer-to-base64-stream

   ;; Conditions.
   #:base64-error
   #:bad-base64-character
   #:incomplete-base64-data

   ;; For creating custom encode/decode tables.
   #:make-decode-table
   #:+decode-table+
   #:+uri-decode-table+))

(in-package #:epsilon.base64)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (declaim (type simple-string *encode-table*))

  (defvar *uri-encode-table*
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
  (declaim (type simple-string *uri-encode-table*))

  (defvar *pad-char* #\=)
  (defvar *uri-pad-char* #\.)
  (declaim (type character *pad-char* *uri-pad-char*))

  (deftype decode-table () '(simple-array (signed-byte 8) (128)))
  (defun make-decode-table (encode-table pad-char
                            &key (whitespace-chars
                                  '(#\Linefeed #\Return #\Space #\Tab)))
    (assert (< (length encode-table) 128)
            (encode-table)
            "Encode table too big: ~S" encode-table)
    (let ((dt (make-array 128 :element-type '(signed-byte 8)
                              :initial-element -1)))
      (declare (type decode-table dt))
      (loop for char across encode-table
            for index upfrom 0
            do (setf (aref dt (char-code char)) index))
      (setf (aref dt (char-code pad-char)) -2)
      (loop for char in whitespace-chars
            do (setf (aref dt (char-code char)) -3))
      dt)))

(defconstant +decode-table+
  (if (boundp '+decode-table+)
      (symbol-value '+decode-table+)
      (make-decode-table *encode-table* *pad-char*)))
(defvar *decode-table* +decode-table+ "Deprecated.")
(declaim (type decode-table +decode-table+ *decode-table*))

(defconstant +uri-decode-table+
  (if (boundp '+uri-decode-table+)
      (symbol-value '+uri-decode-table+)
      (make-decode-table *uri-encode-table* *uri-pad-char*)))
(defvar *uri-decode-table* +uri-decode-table+ "Deprecated.")
(declaim (type decode-table +uri-decode-table+ *uri-decode-table*))

(define-condition base64-error (error)
  ((input
    :initarg :input
    :reader base64-error-input)
   (position
    :initarg :position
    :reader base64-error-position
    :type unsigned-byte)))

(define-condition bad-base64-character (base64-error)
  ((code :initarg :code :reader bad-base64-character-code))
  (:report (lambda (condition stream)
             (format stream "Bad character ~S at index ~D of ~S"
                     (code-char (bad-base64-character-code condition))
                     (base64-error-position condition)
                     (base64-error-input condition)))))

(define-condition incomplete-base64-data (base64-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Unexpected end of Base64 data at index ~D of ~S"
                     (base64-error-position condition)
                     (base64-error-input condition)))))

(deftype character-code ()
  `(integer 0 (,char-code-limit)))

(defmacro etypecase/unroll ((var &rest types) &body body)
  `(etypecase ,var
     ,@(loop for type in types
             collect `(,type ,@body))))

(defmacro let/typed ((&rest vars) &body body)
  `(let ,(loop for (var value) in vars
               collect (list var value))
     (declare ,@(loop for (var nil type) in vars
                      when type
                        collect (list 'type type var)))
     ,@body))

(defmacro define-base64-decoder (hose sink)
  `(defun ,(intern (format nil "~A-~A-~A-~A" '#:base64 hose '#:to sink))
       (input &key (table +decode-table+)
                (uri nil)
                ,@(when (eq sink :stream) `(stream))
                (whitespace :ignore))
     ,(format nil "~
Decode Base64 ~(~A~) to ~(~A~).

TABLE is the decode table to use.  Two decode tables are provided:
+DECODE-TABLE+ (used by default) and +URI-DECODE-TABLE+.  See
MAKE-DECODE-TABLE.

For backwards-compatibility the URI parameter is supported.  If it is
true, then +URI-DECODE-TABLE+ is used, and the value for TABLE
parameter is ignored.

WHITESPACE can be one of:

  :ignore - Whitespace characters are ignored (default).
  :signal - Signal a BAD-BASE64-CHARACTER condition using SIGNAL.
  :error  - Signal a BAD-BASE64-CHARACTER condition using ERROR."
              hose sink)
     (declare (type decode-table table)
              (type ,(ecase hose
                       (:stream 'stream)
                       (:string 'string))
                    input))
     (let/typed ((decode-table (if uri +uri-decode-table+ table)
                               decode-table)
                 ,@(ecase sink
                     (:stream)
                     (:usb8-array
                      (ecase hose
                        (:stream
                         `((result (make-array 1024
                                               :element-type 'u8
                                               :adjustable t
                                               :fill-pointer 0)
                                   (array u8 (*)))))
                        (:string
                         `((result (make-array (* 3 (ceiling (length input) 4))
                                               :element-type 'u8)
                                   ->u8)
                           (rpos 0 array-index)))))
                     (:string
                      (case hose
                        (:stream
                         `((result (make-array 1024
                                               :element-type 'character
                                               :adjustable t
                                               :fill-pointer 0)
                                   (array character (*)))))
                        (:string
                         `((result (make-array (* 3 (ceiling (length input) 4))
                                               :element-type 'character)
                                   (simple-array character (*)))
                           (rpos 0 array-index)))))
                     (:integer
                      `((result 0 unsigned-byte)))))
       (flet ((bad-char (pos code &optional (action :error))
                (let ((args (list 'bad-base64-character
                                  :input input
                                  :position pos
                                  :code code)))
                  (ecase action
                    (:error
                     (apply #'error args))
                    (:cerror
                     (apply #'cerror "Ignore the error and continue." args))
                    (:signal
                     (apply #'signal args)))))
              (incomplete-input (pos)
                (error 'incomplete-base64-data :input input :position pos)))
         ,(let ((body
                  `(let/typed ((ipos 0 array-index)
                               (bitstore 0 (unsigned-byte 24))
                               (bitcount 0 (integer 0 14))
                               (svalue -1 (signed-byte 8))
                               (padchar 0 (integer 0 3))
                               (code 0 fixnum))
                     (loop
                       ,@(ecase hose
                           (:string
                            `((if (< ipos length)
                                  (setq code (char-code (aref input ipos)))
                                  (return))))
                           (:stream
                            `((let ((char (read-char input nil nil)))
                                (if char
                                    (setq code (char-code char))
                                    (return))))))
                         (cond
                           ((or (< 127 code)
                                (= -1 (setq svalue (aref decode-table code))))
                            (bad-char ipos code))
                           ((= -2 svalue)
                            (cond ((<= (incf padchar) 2)
                                   (unless (<= 2 bitcount)
                                     (bad-char ipos code))
                                   (decf bitcount 2))
                                  (t
                                   (bad-char ipos code))))
                           ((= -3 svalue)
                            (ecase whitespace
                              (:ignore
                               ;; Do nothing.
                               )
                              (:error
                               (bad-char ipos code :error))
                              (:signal
                               (bad-char ipos code :signal))))
                           ((not (zerop padchar))
                            (bad-char ipos code))
                           (t
                            (setf bitstore (logior (the (unsigned-byte 24)
                                                        (ash bitstore 6))
                                                   svalue))
                            (incf bitcount 6)
                            (when (>= bitcount 8)
                              (decf bitcount 8)
                              (let ((byte (logand (the (unsigned-byte 24)
                                                       (ash bitstore (- bitcount)))
                                                  #xFF)))
                                (declare (type u8 byte))
                                ,@(ecase sink
                                    (:usb8-array
                                     (ecase hose
                                       (:string
                                        `((setf (aref result rpos) byte)
                                          (incf rpos)))
                                       (:stream
                                        `((vector-push-extend byte result)))))
                                    (:string
                                     (ecase hose
                                       (:string
                                        `((setf (schar result rpos)
                                                (code-char byte))
                                          (incf rpos)))
                                       (:stream
                                        `((vector-push-extend (code-char byte)
                                                              result)))))
                                    (:integer
                                     `((setq result
                                             (logior (ash result 8) byte))))
                                    (:stream
                                     '((write-char (code-char byte) stream)))))
                              (setf bitstore (logand bitstore #xFF)))))
                         (incf ipos))
                     (unless (zerop bitcount)
                       (incomplete-input ipos))
                     ,(ecase sink
                        ((:string :usb8-array)
                         (ecase hose
                           (:string
                            `(if (= rpos (length result))
                                 result
                                 (subseq result 0 rpos)))
                           (:stream
                            `(copy-seq result))))
                        (:integer
                         'result)
                        (:stream
                         'stream)))))
            (ecase hose
              (:string
               `(let ((length (length input)))
                  (declare (type array-index+1 length))
                  (etypecase/unroll (input simple-base-string
                                           simple-string
                                           string)
                    ,body)))
              (:stream
               body)))))))

(define-base64-decoder :string :usb8-array)
(define-base64-decoder :string :string)
(define-base64-decoder :string :integer)
(define-base64-decoder :string :stream)

(define-base64-decoder :stream :usb8-array)
(define-base64-decoder :stream :string)
(define-base64-decoder :stream :integer)
(define-base64-decoder :stream :stream)

(defun round-next-multiple (x n)
  "Round x up to the next highest multiple of n."
  (declare (fixnum n))
  (let ((remainder (mod x n)))
    (declare (fixnum remainder))
    (if (zerop remainder)
        x
        (the fixnum (+ x (the fixnum (- n remainder)))))))

(defmacro def-*-to-base64-* (input-type output-type)
  `(defun ,(intern (concatenate 'string (symbol-name input-type)
                                (symbol-name :-to-base64-)
                                (symbol-name output-type)))
       (input
        ,@(when (eq output-type :stream)
            '(output))
        &key (uri nil) (columns 0))
     "Encode a string array to base64. If columns is > 0, designates
maximum number of columns in a line and the string will be terminated
with a #\Newline."
     (declare ,@(case input-type
                  (:string
                   '((string input)))
                  (:usb8-array
                   '((type (array u8 (*)) input))))
              (fixnum columns))
     (let ((pad (if uri *uri-pad-char* *pad-char*))
           (encode-table (if uri *uri-encode-table* *encode-table*)))
       (declare (simple-string encode-table)
                (character pad))
       (let* ((string-length (length input))
              (complete-group-count (truncate string-length 3))
              (remainder (nth-value 1 (truncate string-length 3)))
              (padded-length (* 4 (truncate (+ string-length 2) 3)))
              ,@(when (eq output-type :string)
                  '((num-lines (if (plusp columns)
                                   (truncate (+ padded-length (1- columns)) columns)
                                   0))
                    (num-breaks (if (plusp num-lines)
                                    (1- num-lines)
                                    0))
                    (strlen (+ padded-length num-breaks))
                    (result (make-string strlen))
                    (ioutput 0)))
              (col (if (plusp columns)
                       0
                       (the fixnum (1+ padded-length)))))
         (declare (fixnum string-length padded-length col
                          ,@(when (eq output-type :string)
                              '(ioutput)))
                  ,@(when (eq output-type :string)
                      '((simple-string result))))
         (labels ((output-char (ch)
                    (if (= col columns)
                        (progn
                          ,@(case output-type
                              (:stream
                               '((write-char #\Newline output)))
                              (:string
                               '((setf (schar result ioutput) #\Newline)
                                 (incf ioutput))))
                          (setq col 1))
                        (incf col))
                    ,@(case output-type
                        (:stream
                         '((write-char ch output)))
                        (:string
                         '((setf (schar result ioutput) ch)
                           (incf ioutput)))))
                  (output-group (svalue chars)
                    (declare (fixnum svalue chars))
                    (output-char
                     (schar encode-table
                            (the fixnum
                                 (logand #x3f
                                         (the fixnum (ash svalue -18))))))
                    (output-char
                     (schar encode-table
                            (the fixnum
                                 (logand #x3f
                                         (the fixnum (ash svalue -12))))))
                    (if (> chars 2)
                        (output-char
                         (schar encode-table
                                (the fixnum
                                     (logand #x3f
                                             (the fixnum (ash svalue -6))))))
                        (output-char pad))
                    (if (> chars 3)
                        (output-char
                         (schar encode-table
                                (the fixnum
                                     (logand #x3f svalue))))
                        (output-char pad))))
           (do ((igroup 0 (the fixnum (1+ igroup)))
                (isource 0 (the fixnum (+ isource 3))))
               ((= igroup complete-group-count)
                (cond
                  ((= remainder 2)
                   (output-group
                    (the fixnum
                         (+
                          (the fixnum
                               (ash
                                ,(case input-type
                                   (:string
                                    '(char-code (the character (char input isource))))
                                   (:usb8-array
                                    '(the fixnum (aref input isource))))
                                16))
                          (the fixnum
                               (ash
                                ,(case input-type
                                   (:string
                                    '(char-code (the character (char input
                                                                (the fixnum (1+ isource))))))
                                   (:usb8-array
                                    '(the fixnum (aref input (the fixnum
                                                              (1+ isource))))))
                                8))))
                    3))
                  ((= remainder 1)
                   (output-group
                    (the fixnum
                         (ash
                          ,(case input-type
                             (:string
                              '(char-code (the character (char input isource))))
                             (:usb8-array
                              '(the fixnum (aref input isource))))
                          16))
                    2)))
                ,(case output-type
                   (:string
                    'result)
                   (:stream
                    'output)))
             (declare (fixnum igroup isource))
             (output-group
              (the fixnum
                   (+
                    (the fixnum
                         (ash
                          (the fixnum
                               ,(case input-type
                                  (:string
                                   '(char-code (the character (char input isource))))
                                  (:usb8-array
                                   '(aref input isource))))
                          16))
                    (the fixnum
                         (ash
                          (the fixnum
                               ,(case input-type
                                  (:string
                                   '(char-code (the character (char input
                                                               (the fixnum (1+ isource))))))
                                  (:usb8-array
                                   '(aref input (1+ isource)))))
                          8))
                    (the fixnum
                         ,(case input-type
                            (:string
                             '(char-code (the character (char input
                                                         (the fixnum (+ 2 isource))))))
                            (:usb8-array
                             '(aref input (+ 2 isource))))
                         )))
              4)))))))

(def-*-to-base64-* :string :string)

(def-*-to-base64-* :string :stream)

(def-*-to-base64-* :usb8-array :string)

(def-*-to-base64-* :usb8-array :stream)

(defun integer-to-base64-string (input &key (uri nil) (columns 0))
  "Encode an integer to base64 format."
  (declare (integer input)
           (fixnum columns))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
        (encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
             (character pad))
    (let* ((input-bits (integer-length input))
           (byte-bits (round-next-multiple input-bits 8))
           (padded-bits (round-next-multiple byte-bits 6))
           (remainder-padding (mod padded-bits 24))
           (padding-bits (if (zerop remainder-padding)
                             0
                             (- 24 remainder-padding)))
           (padding-chars (/ padding-bits 6))
           (padded-length (/ (+ padded-bits padding-bits) 6))
           (last-line-len (if (plusp columns)
                              (- padded-length (* columns
                                                  (truncate
                                                   padded-length columns)))
                              0))
           (num-lines (if (plusp columns)
                          (truncate (+ padded-length (1- columns)) columns)
                          0))
           (num-breaks (if (plusp num-lines)
                           (1- num-lines)
                           0))
           (strlen (+ padded-length num-breaks))
           (last-char (1- strlen))
           (str (make-string strlen))
           (col (if (zerop last-line-len)
                    columns
                    last-line-len)))
      (declare (fixnum padded-length num-lines col last-char
                       padding-chars last-line-len))
      (unless (plusp columns)
        (setq col -1)) ;; set to flag to optimize in loop

      (dotimes (i padding-chars)
        (declare (fixnum i))
        (setf (schar str (the fixnum (- last-char i))) pad))

      (do* ((strpos (- last-char padding-chars) (1- strpos))
            (int (ash input (/ padding-bits 3))))
           ((minusp strpos)
            str)
        (declare (fixnum strpos) (integer int))
        (cond
          ((zerop col)
           (setf (schar str strpos) #\Newline)
           (setq col columns))
          (t
           (setf (schar str strpos)
                 (schar encode-table (the fixnum (logand int #x3f))))
           (setq int (ash int -6))
           (decf col)))))))

(defun integer-to-base64-stream (input stream &key (uri nil) (columns 0))
  "Encode an integer to base64 format."
  (declare (integer input)
           (fixnum columns))
  (let ((pad (if uri *uri-pad-char* *pad-char*))
        (encode-table (if uri *uri-encode-table* *encode-table*)))
    (declare (simple-string encode-table)
             (character pad))
    (let* ((input-bits (integer-length input))
           (byte-bits (round-next-multiple input-bits 8))
           (padded-bits (round-next-multiple byte-bits 6))
           (remainder-padding (mod padded-bits 24))
           (padding-bits (if (zerop remainder-padding)
                             0
                             (- 24 remainder-padding)))
           (padding-chars (/ padding-bits 6))
           (padded-length (/ (+ padded-bits padding-bits) 6))
           (strlen padded-length)
           (nonpad-chars (- strlen padding-chars))
           (last-nonpad-char (1- nonpad-chars))
           (str (make-string strlen)))
      (declare (fixnum padded-length last-nonpad-char))
      (do* ((strpos 0 (the fixnum (1+ strpos)))
            (int (ash input (/ padding-bits 3)) (ash int -6))
            (6bit-value (the fixnum (logand int #x3f))
                        (the fixnum (logand int #x3f))))
           ((= strpos nonpad-chars)
            (let ((col 0))
              (declare (fixnum col))
              (dotimes (i nonpad-chars)
                (declare (fixnum i))
                (write-char (schar str i) stream)
                (when (plusp columns)
                  (incf col)
                  (when (= col columns)
                    (write-char #\Newline stream)
                    (setq col 0))))
              (dotimes (ipad padding-chars)
                (declare (fixnum ipad))
                (write-char pad stream)
                (when (plusp columns)
                  (incf col)
                  (when (= col columns)
                    (write-char #\Newline stream)
                    (setq col 0)))))
            stream)
        (declare (fixnum 6bit-value strpos)
                 (integer int))
        (setf (schar str (- last-nonpad-char strpos))
              (schar encode-table 6bit-value))))))
