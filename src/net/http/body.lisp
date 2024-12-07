(defpackage #:epsilon.net.http.body
  (:use
   #:cl
   #:epsilon.lib.char
   #:epsilon.lib.list
   #:epsilon.lib.stream
   #:epsilon.lib.type
   #:epsilon.net.http.encoding
   #:epsilon.net.http.util)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri))
  (:export
   #:decode-body
   #:write-multipart-content
   #:multipart-value-content-type
   #:decompress-body
   #:write-as-octets
   #:multipart-content-length
   #:content-length
   #:with-content-caches
   #:content-type))

(in-package #:epsilon.net.http.body)

(defun decode-body (content-type body &key default-charset on-close)
  (let ((charset (or (and content-type
                          (detect-charset content-type body))
                     default-charset))
        (epsilon.lib.char:*suppress-character-coding-errors* t))
    (if charset
        (handler-case
            (if (streamp body)
                (make-input-stream body :encoding charset :on-close on-close)
                (epsilon.lib.char:u8-to-string body :encoding charset))
          (epsilon.lib.char:character-decoding-error (e)
            (warn (format nil "Failed to decode the body to ~S due to the following error (falling back to binary):~%  ~A"
                          charset
                          e))
            (return-from decode-body body)))
        body)))

(defun content-disposition (key val)
  (typecase val
    (cons (content-disposition key (first val)))
    (pathname
     (let* ((filename (file-namestring val))
            (utf8-filename-p (find-if (lambda (char)
                                        (< 127 (char-code char)))
                                      filename)))
       (format nil "Content-Disposition: form-data; name=\"~A\"; ~:[filename=\"~A\"~;filename*=UTF-8''~A~]~C~C"
               key
               utf8-filename-p
               (if utf8-filename-p
                   (uri:url-encode filename :encoding :utf-8)
                   filename)
               #\Return #\Newline)))
    (otherwise
      (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
              key
              #\Return #\Newline))))

(defmacro define-alist-cache (cache-name)
  (let ((var (intern (format nil "*~A*" cache-name))))
  `(progn
     (defvar ,var)
     (defun ,(intern (format nil "LOOKUP-IN-~A" cache-name)) (elt)
       (when (boundp ',var)
         (assoc-value ,var elt)))
     (defun (setf ,(intern (format nil "LOOKUP-IN-~A" cache-name))) (val elt)
       (when (boundp ',var)
         (setf (assoc-value ,var elt) val))
       val))))

;; If bound, an alist mapping content to content-type,
;; used to avoid determining content type multiple times
(define-alist-cache content-type-cache)
;; If bound, an alist mapping content to encoded content, to avoid
;; double converting content when we must calculate its length first
(define-alist-cache content-encoding-cache)

(defmacro with-content-caches (&body body)
  `(let ((*content-type-cache* nil)
         (*content-encoding-cache* nil))
     ,@body))

(defun content-type (value)
  (typecase value
    (pathname (or (lookup-in-content-type-cache value)
                  (setf (lookup-in-content-type-cache value) "application/octet-stream")))
    (otherwise nil)))

(defun multipart-value-content-type (value)
  (typecase value
    (cons
     (destructuring-bind (val &key content-type)
         value
       (or content-type (content-type val))))
    (otherwise (content-type value))))

(defun convert-to-octets (val)
  (or (lookup-in-content-encoding-cache val)
      (setf (lookup-in-content-encoding-cache val)
            (typecase val
              (string (epsilon.lib.char:string-to-u8 val))
              (->u8 val)
              (symbol (epsilon.lib.char:string-to-u8 (princ-to-string val)))
              (cons (convert-to-octets (first val)))
              (otherwise (epsilon.lib.char:string-to-u8 (princ-to-string val)))))))

(defun write-as-octets (stream val)
  (typecase val
    (->u8 (write-sequence val stream))
    (pathname
     (with-open-file (in val :element-type 'u8)
       (copy-stream in stream)))
    (string
     (write-sequence (convert-to-octets val) stream))
    (cons (write-as-octets stream (first val)))
    (otherwise (write-sequence (convert-to-octets val) stream))))

(defun content-length (val)
  (typecase val
    (pathname (with-open-file (in val)
                (file-length in)))
    (cons (content-length (first val)))
    (otherwise (length (convert-to-octets val)))))

(defun multipart-content-length (content boundary)
  (declare (type simple-string boundary))
  (let ((boundary-length (length boundary)))
    (+ (loop for (key . val) in content
             sum (+ 2 ;; --
                    boundary-length
                    2 ;; CR LF
                    (length (the simple-string (content-disposition key val)))
                    (let ((content-type (multipart-value-content-type val)))
                      (if content-type
                          (+ #.(length "Content-Type: ") (length content-type) 2)
                          0))
                    2
                    (content-length val)
                    2)
               into total-length
             finally (return total-length))
       2 boundary-length 2 2)))

(defun write-multipart-content (content boundary stream)
  (let ((boundary (string->u8 boundary)))
    (labels ((boundary-line (&optional endp)
               (write-sequence (string->u8 "--") stream)
               (write-sequence boundary stream)
               (when endp
                 (write-sequence (string->u8 "--") stream))
               (crlf))
             (crlf () (write-sequence +crlf+ stream)))
      (loop for (key . val) in content
            do (boundary-line)
               (write-sequence (string->u8 (content-disposition key val)) stream)
               (let ((content-type (multipart-value-content-type val)))
                 (when content-type
                   (write-sequence
                     (string->u8
                       (format nil "Content-Type: ~A~C~C" content-type #\Return #\Newline))
                     stream)))
               (crlf)
               (write-as-octets stream val)
               (crlf)
            finally
               (boundary-line t)))))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))

  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (epsilon.lib.codec:make-decompressing-stream :gzip body)
         (epsilon.lib.codec::decompress nil (epsilon.lib.codec::make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (epsilon.lib.codec:make-decompressing-stream :zlib body)
         (epsilon.lib.codec::decompress nil (epsilon.lib.codec::make-dstate :zlib) body)))
    (t body)))
