;;;; Redis Protocol Implementation (RESP3)
;;;;
;;;; Implements the Redis Serialization Protocol version 3
;;;; for communication with Redis servers.

(defpackage :epsilon.redis.protocol
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:map #:epsilon.map))
  (:export
   #:encode-command
   #:decode-response
   #:redis-protocol-error
   #:parse-error-response))

(in-package :epsilon.redis.protocol)

;;; Protocol constants

(defparameter +cr+ #\Return)
(defparameter +lf+ #\Linefeed)
(defparameter +crlf+ (format nil "~C~C" +cr+ +lf+))

;; RESP3 type markers
(defconstant +simple-string+ #\+)
(defconstant +error+ #\-)
(defconstant +integer+ #\:)
(defconstant +bulk-string+ #\$)
(defconstant +array+ #\*)
(defconstant +null+ #\_)
(defconstant +boolean+ #\#)
(defconstant +double+ #\,)
(defconstant +big-number+ #\()
(defconstant +bulk-error+ #\!)
(defconstant +verbatim-string+ #\=)
(defconstant +map+ #\%)
(defconstant +set+ #\~)
(defconstant +push+ #\>)

;;; Conditions

(define-condition redis-protocol-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "Redis protocol error: ~A" 
                     (error-message condition)))))

;;; Protocol encoding

(defgeneric encode-command (command &rest args)
  (:documentation "Encode a Redis command with arguments into RESP3 format"))

(defmethod encode-command ((command string) &rest args)
  "Encode command and arguments as RESP3 array"
  (with-output-to-string (stream)
			 (let ((total-args (1+ (length args))))
			   (format stream "~C~D~A" +array+ total-args +crlf+)
			   (encode-bulk-string command stream)
			   (dolist (arg args)
			     (encode-value arg stream)))))

(defun encode-value (value stream)
  "Encode a Lisp value to RESP3 format"
  (etypecase value
	     (string (encode-bulk-string value stream))
	     (integer (encode-bulk-string (princ-to-string value) stream))
	     (float (encode-bulk-string (princ-to-string value) stream))
	     (null (encode-null stream))
	     (vector (encode-bulk-string-bytes value stream))
	     ((eql t) (encode-bulk-string "1" stream))
	     (symbol (encode-bulk-string (string-downcase (symbol-name value)) stream))))

(defun encode-bulk-string (string stream)
  "Encode a string as RESP3 bulk string"
  (let ((bytes (to-utf8-bytes string)))
    (format stream "~C~D~A" +bulk-string+ (length bytes) +crlf+)
    (write-sequence bytes stream)
    (write-string +crlf+ stream)))

(defun encode-bulk-string-bytes (bytes stream)
  "Encode byte vector as RESP3 bulk string"
  (format stream "~C~D~A" +bulk-string+ (length bytes) +crlf+)
  (write-sequence bytes stream)
  (write-string +crlf+ stream))

(defun encode-null (stream)
  "Encode null value"
  (format stream "~C~A" +null+ +crlf+))

;;; Protocol decoding

(defun read-line-crlf (stream)
  "Read a line terminated by CRLF"
  (let ((line (make-array 0 :element-type 'character 
                          :adjustable t 
                          :fill-pointer 0)))
    (loop for char = (read-char stream nil nil)
          while char
          do (cond
              ((char= char +cr+)
               (let ((next (read-char stream nil nil)))
                 (if (and next (char= next +lf+))
                     (return line)
                   (error 'redis-protocol-error 
                          :message "Expected LF after CR"))))
              (t (vector-push-extend char line))))))

(defun decode-response (stream)
  "Decode a RESP3 response from stream"
  (let ((type-char (read-char stream nil nil)))
    (unless type-char
      (error 'redis-protocol-error :message "Unexpected end of stream"))
    
    (case type-char
	  (#.+simple-string+ (decode-simple-string stream))
	  (#.+error+ (decode-error stream))
	  (#.+integer+ (decode-integer stream))
	  (#.+bulk-string+ (decode-bulk-string stream))
	  (#.+array+ (decode-array stream))
	  (#.+null+ (decode-null stream))
	  (#.+boolean+ (decode-boolean stream))
	  (#.+double+ (decode-double stream))
	  (#.+map+ (decode-map stream))
	  (#.+set+ (decode-set stream))
	  (otherwise 
	   (error 'redis-protocol-error 
		  :message (format nil "Unknown type marker: ~C" type-char))))))

(defun decode-simple-string (stream)
  "Decode a simple string response"
  (read-line-crlf stream))

(defun decode-error (stream)
  "Decode an error response"
  (let ((error-string (read-line-crlf stream)))
    (error 'redis-protocol-error :message error-string)))

(defun decode-integer (stream)
  "Decode an integer response"
  (parse-integer (read-line-crlf stream)))

(defun decode-bulk-string (stream)
  "Decode a bulk string response"
  (let ((length (parse-integer (read-line-crlf stream))))
    (cond
     ((< length 0) nil)  ; Null bulk string
     (t
      (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        ;; Read trailing CRLF
        (read-char stream)  ; CR
        (read-char stream)  ; LF
        (from-utf8-bytes bytes))))))

(defun decode-array (stream)
  "Decode an array response"
  (let ((length (parse-integer (read-line-crlf stream))))
    (cond
     ((< length 0) nil)  ; Null array
     (t
      (loop repeat length
            collect (decode-response stream))))))

(defun decode-null (stream)
  "Decode a null response"
  (read-line-crlf stream)  ; Read the CRLF
  nil)

(defun decode-boolean (stream)
  "Decode a boolean response"
  (let ((value (read-char stream)))
    (read-char stream)  ; CR
    (read-char stream)  ; LF
    (case value
	  (#\t t)
	  (#\f nil)
	  (otherwise 
	   (error 'redis-protocol-error 
		  :message (format nil "Invalid boolean value: ~C" value))))))

(defun decode-double (stream)
  "Decode a double response"
  (let ((value-string (read-line-crlf stream)))
    (cond
     ((string= value-string "inf") 'positive-infinity)
     ((string= value-string "-inf") 'negative-infinity)
     ((string= value-string "nan") 'not-a-number)
     (t (parse-float value-string)))))

(defun decode-map (stream)
  "Decode a map response"
  (let ((length (parse-integer (read-line-crlf stream))))
    (cond
     ((< length 0) nil)  ; Null map
     (t
      (let ((result (map:make-map)))
        (loop repeat length
              do (let ((key (decode-response stream))
                       (value (decode-response stream)))
                   (setf result (map:assoc result key value))))
        result)))))

(defun decode-set (stream)
  "Decode a set response"
  (let ((length (parse-integer (read-line-crlf stream))))
    (cond
     ((< length 0) nil)  ; Null set
     (t
      (loop repeat length
            collect (decode-response stream) into items
            finally (return (remove-duplicates items :test #'equal)))))))

;;; Utility functions

(defun parse-float (string)
  "Parse a floating point number from string"
  (let ((*read-default-float-format* 'double-float))
    (read-from-string string)))

  ;; Helper functions for string/byte conversion
(defun to-utf8-bytes (string)
  "Convert string to UTF-8 byte array"
  (str:string-to-octets string :encoding :utf-8))

(defun from-utf8-bytes (bytes)
  "Convert UTF-8 byte array to string"
  (str:octets-to-string bytes :encoding :utf-8))
