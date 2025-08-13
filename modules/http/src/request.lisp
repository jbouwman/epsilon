(defpackage :epsilon.http.request
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:map #:epsilon.map)
   (#:seq #:epsilon.sequence))
  (:export
   #:make-request
   #:request-method
   #:request-path
   #:request-headers
   #:request-body
   #:request-params
   #:request-path-params
   #:request-session
   #:parse-query-string
   #:parse-form-data
   #:parse-http-request))

(in-package :epsilon.http.request)

(defclass http-request ()
  ((method :initarg :method :accessor request-method)
   (path :initarg :path :accessor request-path)
   (headers :initarg :headers :accessor request-headers :initform map:+empty+)
   (body :initarg :body :accessor request-body :initform nil)
   (params :initarg :params :accessor request-params :initform map:+empty+)
   (path-params :initarg :path-params :accessor request-path-params :initform map:+empty+)
   (session :initarg :session :accessor request-session :initform nil)))

(defun make-request (method path &key headers body params)
  "Create an HTTP request object"
  (make-instance 'http-request
                 :method method
                 :path path
                 :headers (or headers map:+empty+)
                 :body body
                 :params (or params map:+empty+)))

(defun parse-query-string (query-string)
  "Parse URL query string into parameters map"
  (if (and query-string (> (length query-string) 0))
      (let ((params map:+empty+))
        (dolist (pair (seq:realize (str:split #\& query-string)))
          (let ((eq-pos (position #\= pair)))
            (if eq-pos
                (let ((key (subseq pair 0 eq-pos))
                      (value (subseq pair (1+ eq-pos))))
                  (setf params (map:assoc params 
                                          (url-decode key)
                                          (url-decode value))))
                (setf params (map:assoc params 
                                        (url-decode pair)
                                        "")))))
        params)
      map:+empty+))

(defun parse-form-data (form-string)
  "Parse application/x-www-form-urlencoded data"
  (parse-query-string form-string))

(defun url-decode (string)
  "Decode URL-encoded string"
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for char = (char string i)
          do (cond
               ((char= char #\+)
                (write-char #\Space out))
               ((char= char #\%)
                (when (< (+ i 2) (length string))
                  (let ((hex (subseq string (1+ i) (+ i 3))))
                    (write-char (code-char (parse-integer hex :radix 16)) out)
                    (incf i 2))))
               (t
                (write-char char out))))))

(defun parse-http-request (request-string)
  "Parse raw HTTP request string into http-request object"
  (let ((lines (mapcar (lambda (line) (string-right-trim '(#\Return) line))
                       (seq:realize (str:split #\Newline request-string)))))
    (when (< (length lines) 1)
      (error "Invalid HTTP request: no request line"))
    
    ;; Parse request line (GET /path HTTP/1.1)
    (let* ((request-line (first lines))
           (request-parts (seq:realize (str:split #\Space request-line))))
      (when (< (length request-parts) 3)
        (error "Invalid HTTP request line: ~A" request-line))
      
      (let ((method (string-upcase (first request-parts)))
            (path-with-query (second request-parts))
            (headers map:+empty+)
            (body nil))
        
        ;; Split path and query string
        (let* ((query-pos (position #\? path-with-query))
               (path (if query-pos
                         (subseq path-with-query 0 query-pos)
                         path-with-query))
               (query-string (when query-pos
                               (subseq path-with-query (1+ query-pos))))
               (params (if query-string
                           (parse-query-string query-string)
                           map:+empty+)))
          
          ;; Parse headers
          (loop for i from 1 below (length lines)
                for line = (nth i lines)
                while (and line (> (length line) 0))
                do (let ((colon-pos (position #\: line)))
                     (when colon-pos
                       (let ((header-name (string-downcase (str:trim (subseq line 0 colon-pos))))
                             (header-value (str:trim (subseq line (1+ colon-pos)))))
                         (setf headers (map:assoc headers header-name header-value))))))
          
          ;; For POST requests, parse body (simplified)
          (when (string= method "POST")
            (let ((content-length (map:get headers "content-length")))
              (when content-length
                ;; Find body start (after double CRLF)
                (let ((body-start (search (format nil "~C~C~C~C" #\Return #\Newline #\Return #\Newline) request-string)))
                  (when body-start
                    (setf body (subseq request-string (+ body-start 4))))))))
          
          (make-request method path :headers headers :body body :params params))))))