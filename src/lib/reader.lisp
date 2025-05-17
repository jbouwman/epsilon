(defpackage #:epsilon.lib.reader
  (:use
   #:cl)
  (:shadow
   #:read-char
   #:read-string)
  (:local-nicknames
   (#:char #:epsilon.lib.char)
   (#:stream #:epsilon.lib.stream))
  (:export
   #:make-reader
   #:read-char
   #:read-string))

(in-package #:epsilon.lib.reader)

(defclass stream-reader ()
  ((stream :initarg :stream
           :initform (error "missing stream")
           :reader reader-stream)
   (encoding :initarg :encoding
             :initform (error "missing encoding")
             :reader reader-encoding)
   (buffer :initform (make-array 4 :element-type '(unsigned-byte 8))
           :accessor reader-buffer)
   (saved-char :initform nil
               :accessor reader-saved-char)))

(defun make-reader (stream &key (encoding (char:make-encoding :utf-8)))
  "Create a character reader from the given stream with specified encoding."
  (unless (streamp stream)
    (error "not a stream"))
  (unless (open-stream-p stream)
    (error 'stream-closed :stream stream))
  (make-instance 'stream-reader
                 :stream stream
                 :encoding encoding))

(defun decode-utf8-char (bytes length)
  "Decode a UTF-8 encoded character from bytes."
  (cond
    ;; 1 byte (ASCII)
    ((= length 1)
     (code-char (aref bytes 0)))
    
    ;; 2 bytes
    ((= length 2)
     (let ((code-point (logior (ash (logand #x1f (aref bytes 0)) 6)
                               (logand #x3f (aref bytes 1)))))
       (code-char code-point)))
    
    ;; 3 bytes
    ((= length 3)
     (let ((code-point (logior (ash (logand #x0f (aref bytes 0)) 12)
                               (ash (logand #x3f (aref bytes 1)) 6)
                               (logand #x3f (aref bytes 2)))))
       (code-char code-point)))
    
    ;; 4 bytes
    ((= length 4)
     (let ((code-point (logior (ash (logand #x07 (aref bytes 0)) 18)
                               (ash (logand #x3f (aref bytes 1)) 12)
                               (ash (logand #x3f (aref bytes 2)) 6)
                               (logand #x3f (aref bytes 3)))))
       (code-char code-point)))
    
    (t nil)))

(defmethod read-char ((reader stream-reader))
  "Read a single character from the stream using the reader's encoding."
  ;; Check for a saved character first
  (let ((saved (reader-saved-char reader)))
    (when saved
      (setf (reader-saved-char reader) nil)
      (return-from read-char saved)))
  
  (let* ((stream (reader-stream reader))
         (encoding (reader-encoding reader))
         (enc (char:encoding-encoding encoding))
         (char-name (char:enc-name enc))
         (buffer (reader-buffer reader)))
    
    (case char-name
      (:utf-8
       ;; Read first byte to determine character length
       (let ((first-byte (read-byte stream nil nil)))
         (unless first-byte
           (return-from read-char nil)) ; EOF
         
         (setf (aref buffer 0) first-byte)
         
         (cond
           ;; ASCII character (0xxxxxxx)
           ((< first-byte #x80)
            (code-char first-byte))
           
           ;; 2-byte sequence (110xxxxx 10xxxxxx)
           ((< first-byte #xE0)
            (let ((second-byte (read-byte stream nil nil)))
              (unless (and second-byte (= (logand second-byte #xC0) #x80))
                (error "Invalid UTF-8 sequence"))
              (setf (aref buffer 1) second-byte)
              (decode-utf8-char buffer 2)))
           
           ;; 3-byte sequence (1110xxxx 10xxxxxx 10xxxxxx)
           ((< first-byte #xF0)
            (let ((second-byte (read-byte stream nil nil))
                  (third-byte (read-byte stream nil nil)))
              (unless (and second-byte third-byte
                           (= (logand second-byte #xC0) #x80)
                           (= (logand third-byte #xC0) #x80))
                (error "Invalid UTF-8 sequence"))
              (setf (aref buffer 1) second-byte
                    (aref buffer 2) third-byte)
              (decode-utf8-char buffer 3)))
           
           ;; 4-byte sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
           ((< first-byte #xF8)
            (let ((second-byte (read-byte stream nil nil))
                  (third-byte (read-byte stream nil nil))
                  (fourth-byte (read-byte stream nil nil)))
              (unless (and second-byte third-byte fourth-byte
                           (= (logand second-byte #xC0) #x80)
                           (= (logand third-byte #xC0) #x80)
                           (= (logand fourth-byte #xC0) #x80))
                (error "Invalid UTF-8 sequence"))
              (setf (aref buffer 1) second-byte
                    (aref buffer 2) third-byte
                    (aref buffer 3) fourth-byte)
              (decode-utf8-char buffer 4)))
           
           (t (error "Invalid UTF-8 start byte: ~X" first-byte)))))
      
      (:ascii
       (let ((byte (read-byte stream nil nil)))
         (if byte
             (if (< byte 128)
                 (code-char byte)
                 (error "Invalid ASCII byte: ~X" byte))
             nil))) ; EOF
      
      (t (error "Encoding not supported directly by reader: ~A" char-name)))))

(defmethod read-string ((reader stream-reader) &optional (length nil))
  "Read a string from the stream using the reader's encoding.
   If LENGTH is provided, read at most that many characters.
   If LENGTH is nil, read until EOF."
  (let ((result (make-string 0))
        (char nil))
    (if length
        ;; Read a specific number of characters
        (dotimes (i length)
          (setf char (read-char reader))
          (unless char 
            (return result))
          (setf result (concatenate 'string result (string char))))
        
        ;; Read until EOF
        (loop
          (setf char (read-char reader))
          (unless char 
            (return result))
          (setf result (concatenate 'string result (string char)))))
    result))
