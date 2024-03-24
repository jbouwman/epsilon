(defpackage #:net.http.util
  (:use
   #:cl  
   #:lib.stream
   #:lib.type)
  (:local-nicknames
   (#:uri #:lib.uri))
  (:export
   #:*default-connect-timeout*
   #:*default-read-timeout*
   #:*verbose*
   #:*default-proxy*
   #:*not-verify-ssl*
   #:string->u8
   #:+crlf+
   #:*default-user-agent*
   #:write-first-line
   #:write-header
   #:with-header-output
   #:write-connect-header))

(in-package #:net.http.util)

(defvar *default-connect-timeout* 10)
(defvar *default-read-timeout* 10)
(defvar *verbose* nil)
(defvar *not-verify-ssl* nil)
(defvar *default-proxy* (or #-windows (sys.env:getenv "HTTPS_PROXY")
                            #-windows (sys.env:getenv "HTTP_PROXY"))
  "If specified will be used as the default value of PROXY in calls to net.http.  Defaults to
 the value of the environment variable HTTPS_PROXY or HTTP_PROXY if not on Windows.")

(declaim (ftype (function (simple-string) ->u8) string->u8))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %string->u8 (string)
    (let ((result (make-array (length string) :element-type 'u8)))
      (declare (type ->u8 result))
      (dotimes (i (length string) result)
        (declare (type fixnum i))
        (setf (aref result i)
              (char-code (aref string i))))))

  (defun string->u8 (string)
    (%string->u8 string))

  ;;FIXME merge
  (define-compiler-macro string->u8 (&whole form string)
    (if (constantp string)
        (%string->u8 string)
        form))

  (declaim (type ->u8 +crlf+))
  (defvar +crlf+ (string->u8 (format nil "~C~C" #\Return #\Newline))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *net.http-version*
    "3.1")

  ;; The user agent reported by an Applie iPhone 6
  
  (defparameter *default-user-agent*
    "Mozilla/5.0 (Apple-iPhone7C2/1202.466; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A543 Safari/419.3"))

(defparameter *header-buffer* nil)

(defun write-first-line (method uri version &optional (buffer *header-buffer*))
  (fast-write-sequence (string->u8 (string method)) buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8
                         (format nil "~A~:[~;~:*?~A~]"
                                 (or (uri:path uri) "/")
                                 (uri:query uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string->u8 "HTTP/1.1"))
                         (1.0 (string->u8 "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer))

(defun write-header-field (name buffer)
  (fast-write-sequence (if (typep name '->u8)
                           name
                           (string->u8 (string-capitalize name)))
                       buffer))

(defun write-header-value (value buffer)
  (fast-write-sequence (if (typep value '->u8)
                           value
                           (string->u8 (princ-to-string value)))
                       buffer))

(defun write-header (name value &optional (buffer *header-buffer*))
  (write-header-field name buffer)
  (fast-write-sequence (string->u8 ": ") buffer)
  (write-header-value value buffer)
  (fast-write-sequence +crlf+ buffer))

(define-compiler-macro write-header (name value &optional (buffer '*header-buffer*))
  `(progn
     ,(if (and (constantp name)
               (typep name '(or keyword string)))
          `(fast-write-sequence (string->u8 ,(string-capitalize name)) ,buffer)
          `(write-header-field ,name ,buffer))
     (fast-write-sequence (string->u8 ": ") ,buffer)
     ,(if (constantp value)
          `(fast-write-sequence (string->u8 ,(string value)) ,buffer)
          `(write-header-value ,value ,buffer))
     (fast-write-sequence +crlf+ ,buffer)))

(defmacro with-header-output ((buffer &optional output) &body body)
  `(with-fast-output (,buffer ,output)
     (declare (ignorable ,buffer))
     (let ((*header-buffer* ,buffer))
       ,@body)))

(defun write-connect-header (uri version buffer &optional proxy-auth)
  (fast-write-sequence (string->u8 "CONNECT") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8 (format nil "~A:~A"
                                           (uri:host uri)
                                           (uri:port uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string->u8 "HTTP/1.1"))
                         (1.0 (string->u8 "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence (string->u8 "Host:") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8 (format nil "~A:~A"
                                           (uri:host uri)
                                           (uri:port uri)))
                       buffer)
  (when proxy-auth
    (fast-write-sequence +crlf+ buffer)
    (fast-write-sequence (string->u8 "Proxy-Authorization:") buffer)
    (fast-write-byte #.(char-code #\Space) buffer)
    (fast-write-sequence (string->u8 proxy-auth) buffer))
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence +crlf+ buffer))
