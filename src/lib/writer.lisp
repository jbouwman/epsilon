(defpackage #:epsilon.lib.writer
  (:use
   #:cl)
  (:shadow
   #:write-char
   #:write-string)
  (:local-nicknames
   (#:char #:epsilon.lib.char)
   (#:stream #:epsilon.lib.stream))
  (:export
   #:make-writer
   #:write-char
   #:write-string))

(in-package #:epsilon.lib.writer)

(defclass stream-writer ()
  ((stream :initarg :stream
           :initform (error "missing stream")
           :reader writer-stream)
   (encoding :initarg :encoding
             :initform (error "missing encoding")
             :reader writer-encoding)))

(defun make-writer (stream &key (encoding (char:make-encoding :utf-8)))
  "Create a character writer for the given stream with specified encoding."
  (unless (streamp stream)
    (error "not a stream"))
  (unless (open-stream-p stream)
    (error 'stream-closed :stream stream))
  (make-instance 'stream-writer
                 :stream stream
                 :encoding encoding))

(defmethod write-char ((writer stream-writer) character)
  "Write a single character to the stream using the writer's encoding."
  (let* ((stream (writer-stream writer))
         (encoding (writer-encoding writer))
         (code-point (char-code character))
         (buffer (make-array 4 :element-type '(unsigned-byte 8)))
         (bytes-written 0))
    
    ;; Convert the character to bytes according to the encoding
    (let* ((enc (char:encoding-encoding encoding))
           (char-name (char:enc-name enc)))
      (cond
        ;; ASCII/single-byte encodings
        ((and (< code-point 128) (member char-name '(:ascii :latin-1 :utf-8)))
         (setf (aref buffer 0) code-point)
         (setf bytes-written 1))
        
        ;; UTF-8 encoding
        ((eq char-name :utf-8)
         (cond
           ((< code-point #x80)
            (setf (aref buffer 0) code-point)
            (setf bytes-written 1))
           ((< code-point #x800)
            (setf (aref buffer 0) (logior #xc0 (ash code-point -6)))
            (setf (aref buffer 1) (logior #x80 (logand code-point #x3f)))
            (setf bytes-written 2))
           ((< code-point #x10000)
            (setf (aref buffer 0) (logior #xe0 (ash code-point -12)))
            (setf (aref buffer 1) (logior #x80 (logand #x3f (ash code-point -6))))
            (setf (aref buffer 2) (logior #x80 (logand code-point #x3f)))
            (setf bytes-written 3))
           (t
            (setf (aref buffer 0) (logior #xf0 (logand #x07 (ash code-point -18))))
            (setf (aref buffer 1) (logior #x80 (logand #x3f (ash code-point -12))))
            (setf (aref buffer 2) (logior #x80 (logand #x3f (ash code-point -6))))
            (setf (aref buffer 3) (logand #x3f code-point))
            (setf bytes-written 4))))
        
        ;; Default - error if not supported
        (t (error "Encoding not supported directly by writer: ~A" char-name))))
    
    ;; Write the encoded bytes to the stream
    (write-sequence buffer stream :end bytes-written)
    character))

(defmethod write-string ((writer stream-writer) string)
  "Write a string to the stream using the writer's encoding."
  (loop for char across string
        do (write-char writer char))
  string)
