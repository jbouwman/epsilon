;;;; PostgreSQL Wire Protocol v3 Implementation
;;;;
;;;; Implements the PostgreSQL frontend/backend protocol for
;;;; communication with PostgreSQL servers.

(defpackage :epsilon.postgres.protocol
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:bin #:epsilon.binary)
   (#:map #:epsilon.map)
   (#:conn #:epsilon.postgres.connection))
  (:export
   #:postgres-protocol-error
   #:make-startup-message
   #:make-query-message
   #:read-message
   #:parse-error-response
   #:parse-row-description
   #:parse-data-row
   #:encode-parameter))

(in-package :epsilon.postgres.protocol)

;;; Protocol constants

(defconstant +protocol-version+ #x00030000)  ; 3.0

;; Message type bytes (frontend to backend)
(defconstant +startup-message+ nil)  ; No type byte
(defconstant +password-message+ #\p)
(defconstant +query+ #\Q)
(defconstant +parse+ #\P)
(defconstant +bind+ #\B)
(defconstant +execute+ #\E)
(defconstant +describe+ #\D)
(defconstant +close+ #\C)
(defconstant +sync+ #\S)
(defconstant +flush+ #\H)
(defconstant +terminate+ #\X)
(defconstant +copy-data+ #\d)
(defconstant +copy-done+ #\c)
(defconstant +copy-fail+ #\f)

;; Message type bytes (backend to frontend)
(defconstant +authentication+ #\R)
(defconstant +backend-key-data+ #\K)
(defconstant +bind-complete+ #\2)
(defconstant +close-complete+ #\3)
(defconstant +command-complete+ #\C)
(defconstant +copy-in-response+ #\G)
(defconstant +copy-out-response+ #\H)
(defconstant +copy-both-response+ #\W)
(defconstant +data-row+ #\D)
(defconstant +empty-query-response+ #\I)
(defconstant +error-response+ #\E)
(defconstant +notice-response+ #\N)
(defconstant +notification-response+ #\A)
(defconstant +parameter-description+ #\t)
(defconstant +parameter-status+ #\S)
(defconstant +parse-complete+ #\1)
(defconstant +portal-suspended+ #\s)
(defconstant +ready-for-query+ #\Z)
(defconstant +row-description+ #\T)

;; Authentication types
(defconstant +auth-ok+ 0)
(defconstant +auth-kerberos-v5+ 2)
(defconstant +auth-cleartext-password+ 3)
(defconstant +auth-md5-password+ 5)
(defconstant +auth-scm-credential+ 6)
(defconstant +auth-gss+ 7)
(defconstant +auth-gss-continue+ 8)
(defconstant +auth-sspi+ 9)
(defconstant +auth-sasl+ 10)
(defconstant +auth-sasl-continue+ 11)
(defconstant +auth-sasl-final+ 12)

;;; Conditions

(define-condition postgres-protocol-error (conn:postgres-error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "PostgreSQL protocol error: ~A" 
                     (error-message condition)))))

;;; Binary I/O utilities

(defun write-int32 (value stream)
  "Write 32-bit integer in network byte order"
  (write-byte (ldb (byte 8 24) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun read-int32 (stream)
  "Read 32-bit integer in network byte order"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)))

(defun write-int16 (value stream)
  "Write 16-bit integer in network byte order"
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun read-int16 (stream)
  "Read 16-bit integer in network byte order"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream)))
    (logior (ash b0 8) b1)))

(defun octets-to-uint32 (octets)
  "Convert 4 octets to 32-bit unsigned integer (big-endian)"
  (logior (ash (aref octets 0) 24)
          (ash (aref octets 1) 16)
          (ash (aref octets 2) 8)
          (aref octets 3)))

(defun octets-to-uint16 (octets)
  "Convert 2 octets to 16-bit unsigned integer (big-endian)"
  (logior (ash (aref octets 0) 8)
          (aref octets 1)))

(defun write-cstring (string stream)
  "Write null-terminated string"
  (write-sequence (str:string-to-octets string) stream)
  (write-byte 0 stream))

(defun read-cstring (stream)
  "Read null-terminated string"
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
    (loop for byte = (read-byte stream)
          until (zerop byte)
          do (vector-push-extend byte bytes))
    (str:octets-to-string bytes)))

(defun write-bytes (bytes stream)
  "Write byte array with length prefix"
  (write-int32 (length bytes) stream)
  (write-sequence bytes stream))

(defun read-bytes (stream length)
  "Read specified number of bytes"
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence bytes stream)
    bytes))

;;; Message construction

(defun make-message (type &rest contents)
  "Create a protocol message with type byte and length"
  (let ((data (with-output-to-string (s)
                (dolist (item contents)
                  (princ item s)))))
    (with-output-to-string (message)
      (when type
        (write-char type message))
      (write-int32 (+ 4 (length data)) message)
      (write-string data message))))

(defun make-startup-message (parameters)
  "Create startup message with parameters"
  (let ((param-data (with-output-to-string (s)
                      (map:each (lambda (k v)
                                  (write-cstring k s)
                                  (write-cstring v s))
                                parameters)
                      (write-byte 0 s))))  ; Final null terminator
    (with-output-to-string (message)
      ;; Length includes version and parameters
      (write-int32 (+ 8 (length param-data)) message)
      (write-int32 +protocol-version+ message)
      (write-string param-data message))))

(defun make-query-message (query)
  "Create simple query message"
  (make-message +query+ 
                (with-output-to-string (s)
                  (write-cstring query s))))

(defun make-parse-message (name query param-types)
  "Create parse message for prepared statement"
  (make-message +parse+
                (with-output-to-string (s)
                  (write-cstring name s)
                  (write-cstring query s)
                  (write-int16 (length param-types) s)
                  (dolist (type param-types)
                    (write-int32 type s)))))

(defun make-bind-message (portal statement param-values result-formats)
  "Create bind message"
  (make-message +bind+
                (with-output-to-string (s)
                  (write-cstring portal s)
                  (write-cstring statement s)
                  ;; Parameter format codes (0 = text, 1 = binary)
                  (write-int16 0 s)  ; All text for now
                  ;; Parameter values
                  (write-int16 (length param-values) s)
                  (dolist (value param-values)
                    (if value
                        (let ((bytes (encode-parameter value)))
                          (write-int32 (length bytes) s)
                          (write-sequence bytes s))
                        (write-int32 -1 s)))  ; NULL
                  ;; Result format codes
                  (write-int16 (length result-formats) s)
                  (dolist (format result-formats)
                    (write-int16 format s)))))

(defun make-execute-message (portal max-rows)
  "Create execute message"
  (make-message +execute+
                (with-output-to-string (s)
                  (write-cstring portal s)
                  (write-int32 max-rows s))))

(defun make-describe-message (type name)
  "Create describe message"
  (make-message +describe+
                (with-output-to-string (s)
                  (write-char type s)  ; 'S' for statement, 'P' for portal
                  (write-cstring name s))))

(defun make-close-message (type name)
  "Create close message"
  (make-message +close+
                (with-output-to-string (s)
                  (write-char type s)  ; 'S' for statement, 'P' for portal
                  (write-cstring name s))))

(defun make-sync-message ()
  "Create sync message"
  (make-message +sync+))

(defun make-terminate-message ()
  "Create terminate message"
  (make-message +terminate+))

;;; Message parsing

(defun read-message (stream)
  "Read a backend message"
  (let* ((type (read-char stream))
         (length (read-int32 stream))
         (data-length (- length 4)))  ; Subtract length field itself
    (when (< data-length 0)
      (error 'postgres-protocol-error 
             :message "Invalid message length"))
    (let ((data (when (> data-length 0)
                  (read-bytes stream data-length))))
      (values type data))))

(defun parse-error-response (data)
  "Parse error or notice response"
  (let ((fields (map:make-map))
        (stream (make-string-input-stream data))
        (pos 0))
    (loop while (< pos (length data))
          do (let ((field-type (char data pos)))
               (when (char= field-type #\Nul)
                 (return))
               (incf pos)
               (let ((end (position #\Nul data :start pos)))
                 (unless end
                   (error 'postgres-protocol-error
                          :message "Unterminated error field"))
                 (let ((value (subseq data pos end)))
                   (setf fields (map:assoc fields 
                                           (string field-type) 
                                           value))
                   (setf pos (1+ end))))))
    fields))

(defun parse-row-description (data)
  "Parse row description message"
  (let* ((stream (make-string-input-stream data))
         (pos 0)
         (field-count (octets-to-uint16 
                       (vector (char-code (char data 0))
                               (char-code (char data 1)))))
         (fields '()))
    (setf pos 2)
    (dotimes (i field-count)
      ;; Read field name
      (let ((name-end (position #\Nul data :start pos)))
        (unless name-end
          (error 'postgres-protocol-error
                 :message "Unterminated field name"))
        (let* ((name (subseq data pos name-end))
               (pos2 (+ name-end 1))
               ;; Read field info (18 bytes after name)
               (table-oid (octets-to-uint32 
                           (subseq data pos2 (+ pos2 4))))
               (column-num (octets-to-uint16 
                            (subseq data (+ pos2 4) (+ pos2 6))))
               (type-oid (octets-to-uint32 
                          (subseq data (+ pos2 6) (+ pos2 10))))
               (type-size (octets-to-uint16 
                           (subseq data (+ pos2 10) (+ pos2 12))))
               (type-modifier (octets-to-uint32 
                               (subseq data (+ pos2 12) (+ pos2 16))))
               (format-code (octets-to-uint16 
                             (subseq data (+ pos2 16) (+ pos2 18)))))
          (push (list :name name
                      :table-oid table-oid
                      :column-num column-num
                      :type-oid type-oid
                      :type-size type-size
                      :type-modifier type-modifier
                      :format-code format-code)
                fields)
          (setf pos (+ pos2 18)))))
    (nreverse fields)))

(defun parse-data-row (data field-count)
  "Parse data row message"
  (let ((pos 0)
        (values '()))
    ;; Skip field count (already known)
    (incf pos 2)
    (dotimes (i field-count)
      (let ((length (octets-to-uint32
                     (subseq data pos (+ pos 4)))))
        (incf pos 4)
        (if (= length -1)
            (push nil values)  ; NULL
            (progn
              (push (subseq data pos (+ pos length)) values)
              (incf pos length)))))
    (nreverse values)))

(defun parse-command-complete (data)
  "Parse command complete message"
  (let ((tag-end (position #\Nul data)))
    (when tag-end
      (subseq data 0 tag-end))))

(defun parse-ready-for-query (data)
  "Parse ready for query message"
  (when (> (length data) 0)
    (char data 0)))

;;; Parameter encoding

(defun encode-parameter (value)
  "Encode parameter value for PostgreSQL"
  (etypecase value
    (string (str:string-to-octets value))
    (integer (str:string-to-octets (princ-to-string value)))
    (float (str:string-to-octets (princ-to-string value)))
    ((eql t) (str:string-to-octets "t"))
    ((eql nil) (str:string-to-octets "f"))
    (vector value)))  ; Already bytes

;;; Authentication

(defun handle-authentication (stream auth-type data)
  "Handle authentication request"
  (case auth-type
    (#.+auth-ok+ t)  ; Success
    (#.+auth-cleartext-password+
     (error 'postgres-connection-error
            :message "Cleartext password authentication required"))
    (#.+auth-md5-password+
     (error 'postgres-connection-error
            :message "MD5 password authentication required"))
    (otherwise
     (error 'postgres-connection-error
            :message (format nil "Unsupported authentication type: ~D" 
                             auth-type)))))
