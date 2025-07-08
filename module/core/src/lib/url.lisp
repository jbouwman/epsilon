;;;; URL Parsing and Manipulation
;;;;
;;;; This module provides comprehensive URL parsing, construction, and manipulation
;;;; with support for common Internet protocols. Includes protocol handler registry
;;;; and path/URL conversion utilities for filesystem integration.
;;;;
;;;; Key Features:
;;;; - URL parsing and construction for HTTP, HTTPS, FTP, etc.
;;;; - Protocol handler registry for extensible scheme support
;;;; - URL encoding/decoding with proper character handling  
;;;; - Query parameter parsing and manipulation
;;;; - URL resolution and normalization
;;;; - File system path <-> URL conversion
;;;; - Validation for URL components
;;;;
;;;; Dependencies: epsilon.lib.path (for path conversion)
;;;; Standards: RFC 3986 (URI), RFC 1738 (URL)

(defpackage :epsilon.lib.url
  (:use :cl)
  (:local-nicknames
   (:path :epsilon.lib.path))
  (:export
   ;; Core URL type
   :url
   :make-url
   :parse-url
   :url-string
   :url-scheme
   :url-host
   :url-port
   :url-path
   :url-query
   :url-fragment
   :url-userinfo
   
   ;; URL manipulation
   :url-join
   :url-resolve
   :url-normalize
   :url-equal
   :url-absolute-p
   :url-relative-p
   
   ;; URL encoding/decoding
   :url-encode
   :url-decode
   :url-encode-component
   :url-decode-component
   
   ;; Query parameter handling
   :parse-query-string
   :build-query-string
   :get-query-param
   :set-query-param
   :remove-query-param
   :query-params
   
   ;; Path <-> URL conversion
   :path-to-url
   :url-to-path
   :file-url-p
   
   ;; Protocol handlers
   :register-protocol-handler
   :unregister-protocol-handler
   :get-protocol-handler
   :supported-schemes
   :default-port
   
   ;; Validation
   :valid-url-p
   :valid-scheme-p
   :valid-host-p
   
   ;; Common schemes
   :http-url-p
   :https-url-p
   :ftp-url-p
   :mailto-url-p))

(in-package :epsilon.lib.url)

;;;; Platform detection
(defparameter *platform* 
  #+darwin :darwin
  #+linux :linux  
  #+win32 :windows
  #-(or darwin linux win32) :unknown)

;;;; ==========================================================================
;;;; URL Encoding/Decoding
;;;; ==========================================================================

(defparameter *url-unreserved-chars*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")

(defparameter *url-reserved-chars*
  ":/?#[]@!$&'()*+,;=")

(defun char-unreserved-p (char)
  "Check if character is unreserved in URLs"
  (find char *url-unreserved-chars*))

(defun char-to-hex (char)
  "Convert character to %XX hex encoding"
  (format nil "%~2,'0X" (char-code char)))

(defun hex-to-char (hex-string)
  "Convert %XX hex encoding to character"
  (code-char (parse-integer hex-string :radix 16)))

(defun url-encode-component (string &key (encoding :utf-8))
  "URL encode a string component"
  (declare (ignore encoding)) ; UTF-8 assumed for now
  (when string
    (with-output-to-string (out)
      (loop for char across string do
        (if (char-unreserved-p char)
            (write-char char out)
            (write-string (char-to-hex char) out))))))

(defun url-decode-component (string &key (encoding :utf-8))
  "URL decode a string component"
  (declare (ignore encoding)) ; UTF-8 assumed for now
  (when string
    (with-output-to-string (out)
      (loop for i from 0 below (length string) do
        (let ((char (char string i)))
          (if (char= char #\%)
              (if (< (+ i 2) (length string))
                  (progn
                    (write-char (hex-to-char (subseq string (1+ i) (+ i 3))) out)
                    (incf i 2))
                  (write-char char out))
              (if (char= char #\+)
                  (write-char #\Space out)
                  (write-char char out))))))))

(defun url-encode (string)
  "URL encode a full string (less aggressive than component encoding)"
  (when string
    (with-output-to-string (out)
      (loop for char across string do
        (if (or (char-unreserved-p char) (find char ":/?#[]@!$&'()*+,;="))
            (write-char char out)
            (write-string (char-to-hex char) out))))))

(defun url-decode (string)
  "URL decode a full string"
  (url-decode-component string))

;;;; ==========================================================================
;;;; Core URL Type
;;;; ==========================================================================

(defclass url ()
  ((scheme :initarg :scheme :accessor url-scheme
           :documentation "URL scheme (http, https, ftp, etc.)")
   (userinfo :initarg :userinfo :accessor url-userinfo
             :documentation "User information (user:pass)")
   (host :initarg :host :accessor url-host
         :documentation "Hostname or IP address")
   (port :initarg :port :accessor url-port
         :documentation "Port number")
   (path :initarg :path :accessor url-path
         :documentation "Path component")
   (query :initarg :query :accessor url-query
          :documentation "Query string")
   (fragment :initarg :fragment :accessor url-fragment
             :documentation "Fragment identifier"))
  (:documentation "URL object representing a Uniform Resource Locator"))

;; Note: print-object method defined after url-string method below

;;;; ==========================================================================
;;;; Protocol Handler Registry
;;;; ==========================================================================

(defparameter *protocol-handlers* (make-hash-table :test 'equal)
  "Registry of protocol handlers")

(defparameter *default-ports* 
  '(("http" . 80)
    ("https" . 443)
    ("ftp" . 21)
    ("ftps" . 990)
    ("ssh" . 22)
    ("telnet" . 23)
    ("smtp" . 25)
    ("dns" . 53)
    ("pop3" . 110)
    ("imap" . 143)
    ("ldap" . 389)
    ("ldaps" . 636))
  "Default port numbers for common schemes")

(defun register-protocol-handler (scheme handler)
  "Register a protocol handler for a scheme"
  (setf (gethash (string-downcase scheme) *protocol-handlers*) handler))

(defun unregister-protocol-handler (scheme)
  "Unregister a protocol handler"
  (remhash (string-downcase scheme) *protocol-handlers*))

(defun get-protocol-handler (scheme)
  "Get the protocol handler for a scheme"
  (gethash (string-downcase scheme) *protocol-handlers*))

(defun supported-schemes ()
  "Get list of supported schemes"
  (loop for scheme being the hash-keys of *protocol-handlers*
        collect scheme))

(defun default-port (scheme)
  "Get default port for a scheme"
  (cdr (assoc (string-downcase scheme) *default-ports* :test 'string=)))

;;;; ==========================================================================
;;;; URL Construction and Parsing
;;;; ==========================================================================

(defun make-url (&key scheme userinfo host port path query fragment)
  "Create a URL object"
  (make-instance 'url
                 :scheme (when scheme (string-downcase scheme))
                 :userinfo userinfo
                 :host (when host (string-downcase host))
                 :port port
                 :path (or path "/")
                 :query query
                 :fragment fragment))

(defun parse-url (url-string)
  "Parse a URL string into a URL object"
  (when (and url-string (> (length url-string) 0))
    (let ((scheme nil)
          (userinfo nil)
          (host nil)
          (port nil)
          (path nil)
          (query nil)
          (fragment nil)
          (remaining url-string))
      
      ;; Parse fragment
      (let ((fragment-pos (position #\# remaining)))
        (when fragment-pos
          (setf fragment (subseq remaining (1+ fragment-pos))
                remaining (subseq remaining 0 fragment-pos))))
      
      ;; Parse query
      (let ((query-pos (position #\? remaining)))
        (when query-pos
          (setf query (subseq remaining (1+ query-pos))
                remaining (subseq remaining 0 query-pos))))
      
      ;; Parse scheme
      (let ((scheme-pos (position #\: remaining)))
        (when (and scheme-pos (> scheme-pos 0))
          (setf scheme (subseq remaining 0 scheme-pos)
                remaining (subseq remaining (1+ scheme-pos)))))
      
      ;; Parse authority (//host:port or //userinfo@host:port)
      (when (and (>= (length remaining) 2)
                 (string= "//" remaining :end2 2))
        (setf remaining (subseq remaining 2))
        (let ((path-pos (position #\/ remaining)))
          (let ((authority (if path-pos
                               (subseq remaining 0 path-pos)
                               remaining)))
            (when path-pos
              (setf path (subseq remaining path-pos)))
            
            ;; Parse userinfo
            (let ((at-pos (position #\@ authority)))
              (when at-pos
                (setf userinfo (subseq authority 0 at-pos)
                      authority (subseq authority (1+ at-pos)))))
            
            ;; Parse host and port
            (let ((port-pos (position #\: authority :from-end t)))
              (if port-pos
                  (progn
                    (setf host (subseq authority 0 port-pos))
                    (let ((port-str (subseq authority (1+ port-pos))))
                      (when (> (length port-str) 0)
                        (setf port (parse-integer port-str :junk-allowed t)))))
                  (setf host authority)))
            
            ;; Clear remaining if no path was found
            (unless path-pos
              (setf remaining "")))))
      
      ;; If no path was found in authority parsing, use remaining
      (unless path
        (setf path remaining))
      
      ;; Default empty path to "/" only if we have a scheme and host
      (when (and (string= path "") scheme host)
        (setf path "/"))
      
      (make-url :scheme scheme
                :userinfo userinfo
                :host host
                :port port
                :path path
                :query query
                :fragment fragment))))

(defmethod url-string ((url url))
  "Convert URL object to string representation"
  (with-slots (scheme userinfo host port path query fragment) url
    (concatenate 'string
                 ;; Scheme
                 (if scheme (concatenate 'string scheme ":") "")
                 ;; Authority
                 (if (or host userinfo)
                     (concatenate 'string 
                                  "//"
                                  (if userinfo (concatenate 'string userinfo "@") "")
                                  (or host "")
                                  (if (and port (not (equal port (default-port scheme))))
                                      (format nil ":~D" port)
                                      ""))
                     "")
                 ;; Path
                 (or path "")
                 ;; Query
                 (if query (concatenate 'string "?" query) "")
                 ;; Fragment
                 (if fragment (concatenate 'string "#" fragment) ""))))

(defmethod print-object ((url url) stream)
  (print-unreadable-object (url stream :type t)
    (format stream "~S" (url-string url))))

;;;; ==========================================================================
;;;; Query Parameter Handling
;;;; ==========================================================================

(defun parse-query-string (query-string)
  "Parse query string into alist of key-value pairs"
  (when query-string
    (loop for param in (loop for start = 0 then (1+ pos)
                             for pos = (position #\& query-string :start start)
                             collect (subseq query-string start pos)
                             while pos)
          collect (let ((eq-pos (position #\= param)))
                    (if eq-pos
                        (cons (url-decode-component (subseq param 0 eq-pos))
                              (url-decode-component (subseq param (1+ eq-pos))))
                        (cons (url-decode-component param) nil))))))

(defun build-query-string (params)
  "Build query string from alist of key-value pairs"
  (when params
    (reduce (lambda (a b) (concatenate 'string a "&" b))
            (mapcar (lambda (param)
                      (if (cdr param)
                          (concatenate 'string 
                                       (url-encode-component (car param))
                                       "="
                                       (url-encode-component (cdr param)))
                          (url-encode-component (car param))))
                    params))))

(defun query-params (url)
  "Get query parameters as alist"
  (parse-query-string (url-query url)))

(defun get-query-param (url param-name)
  "Get value of query parameter"
  (cdr (assoc param-name (query-params url) :test 'string=)))

(defun set-query-param (url param-name value)
  "Set query parameter value, returns new URL"
  (let ((params (query-params url)))
    (setf params (remove param-name params :key 'car :test 'string=))
    (push (cons param-name value) params)
    (make-url :scheme (url-scheme url)
              :userinfo (url-userinfo url)
              :host (url-host url)
              :port (url-port url)
              :path (url-path url)
              :query (build-query-string params)
              :fragment (url-fragment url))))

(defun remove-query-param (url param-name)
  "Remove query parameter, returns new URL"
  (let ((params (remove param-name (query-params url) :key 'car :test 'string=)))
    (make-url :scheme (url-scheme url)
              :userinfo (url-userinfo url)
              :host (url-host url)
              :port (url-port url)
              :path (url-path url)
              :query (build-query-string params)
              :fragment (url-fragment url))))

;;;; ==========================================================================
;;;; URL Manipulation
;;;; ==========================================================================

(defun url-absolute-p (url)
  "Check if URL is absolute (has scheme)"
  (and (url-scheme url) t))

(defun url-relative-p (url)
  "Check if URL is relative (no scheme)"
  (not (url-absolute-p url)))

(defun url-equal (url1 url2)
  "Compare two URLs for equality"
  (let ((u1 (if (typep url1 'url) url1 (parse-url url1)))
        (u2 (if (typep url2 'url) url2 (parse-url url2))))
    (and u1 u2
         (string= (or (url-scheme u1) "") (or (url-scheme u2) ""))
         (string= (or (url-host u1) "") (or (url-host u2) ""))
         (equal (url-port u1) (url-port u2))
         (string= (or (url-path u1) "") (or (url-path u2) ""))
         (string= (or (url-query u1) "") (or (url-query u2) ""))
         (string= (or (url-fragment u1) "") (or (url-fragment u2) "")))))

(defun url-normalize (url)
  "Normalize URL by removing redundant components"
  (when url
    (let ((path (url-path url)))
      ;; Normalize path by removing . and .. components
      (when path
        (let ((segments (remove-if (lambda (s) (string= s ""))
                                   (loop for start = 0 then (1+ pos)
                                         for pos = (position #\/ path :start start)
                                         collect (subseq path start pos)
                                         while pos)))
              (normalized-segments '()))
          (loop for segment in segments do
            (cond
              ((string= segment ".") 
               ;; Skip current directory
               )
              ((string= segment "..")
               ;; Go up one directory
               (when normalized-segments
                 (setf normalized-segments (butlast normalized-segments))))
              (t
               (setf normalized-segments (append normalized-segments (list segment))))))
          
          (setf path (if normalized-segments
                         (concatenate 'string "/" (reduce (lambda (a b) (concatenate 'string a "/" b))
                                                          normalized-segments))
                         "/"))))
      
      (make-url :scheme (url-scheme url)
                :userinfo (url-userinfo url)
                :host (url-host url)
                :port (url-port url)
                :path path
                :query (url-query url)
                :fragment (url-fragment url)))))

(defun url-resolve (base-url relative-url)
  "Resolve relative URL against base URL"
  (let ((base (if (typep base-url 'url) base-url (parse-url base-url)))
        (rel (if (typep relative-url 'url) relative-url (parse-url relative-url))))
    (cond
      ;; Relative URL is actually absolute
      ((url-absolute-p rel) rel)
      ;; Base URL is not absolute - can't resolve
      ((not (url-absolute-p base)) nil)
      ;; Resolve relative components
      (t
       (make-url :scheme (url-scheme base)
                 :userinfo (url-userinfo base)
                 :host (url-host base)
                 :port (url-port base)
                 :path (if (and (url-path rel) (char= (char (url-path rel) 0) #\/))
                           (url-path rel) ; Absolute path
                           (let ((base-path (or (url-path base) "/"))
                                 (rel-path (or (url-path rel) "")))
                             ;; Combine paths
                             (if (string= rel-path "")
                                 base-path
                                 (concatenate 'string
                                              (subseq base-path 0 (1+ (position #\/ base-path :from-end t)))
                                              rel-path))))
                 :query (url-query rel)
                 :fragment (url-fragment rel))))))

(defun url-join (base-url &rest path-components)
  "Join URL with additional path components"
  (let ((url (if (typep base-url 'url) base-url (parse-url base-url))))
    (when url
      (let ((base-path (url-path url)))
        (when (and base-path (not (char= (char base-path (1- (length base-path))) #\/)))
          (setf base-path (concatenate 'string base-path "/")))
        (make-url :scheme (url-scheme url)
                  :userinfo (url-userinfo url)
                  :host (url-host url)
                  :port (url-port url)
                  :path (concatenate 'string base-path
                                     (reduce (lambda (a b) (concatenate 'string a "/" b))
                                             path-components))
                  :query (url-query url)
                  :fragment (url-fragment url))))))

;;;; ==========================================================================
;;;; Path <-> URL Conversion
;;;; ==========================================================================

(defun file-url-p (url)
  "Check if URL is a file URL"
  (string= (url-scheme url) "file"))

(defun path-to-url (path)
  "Convert filesystem path to file:// URL"
  (let* ((path-obj (if (typep path 'path:path) 
                       path 
                       (path:make-path path)))
         (path-str (path:path-string path-obj))
         (is-absolute (path:path-absolute-p path-obj)))
    (when path-obj
      (make-url :scheme "file"
                :host (if is-absolute "" "localhost")
                :path (if is-absolute
                          ;; Absolute path
                          #+win32 (concatenate 'string "/" (substitute #\/ #\\ path-str))
                          #-win32 path-str
                          ;; Relative path
                          (concatenate 'string "/" path-str))))))

(defun url-to-path (url)
  "Convert file:// URL to filesystem path"
  (let ((url-obj (if (typep url 'url) url (parse-url url))))
    (when (and url-obj (file-url-p url-obj))
      (let ((path-str (url-path url-obj)))
        (when path-str
          ;; Remove leading / for Windows absolute paths
          #+win32 (when (and (> (length path-str) 3)
                             (char= (char path-str 0) #\/)
                             (char= (char path-str 2) #\:))
                    (setf path-str (subseq path-str 1)))
          (path:make-path (url-decode path-str)))))))

;;;; ==========================================================================
;;;; Validation
;;;; ==========================================================================

(defun valid-scheme-p (scheme)
  "Check if scheme is valid"
  (and scheme
       (> (length scheme) 0)
       (every (lambda (c) (or (alphanumericp c) (find c "+-."))) scheme)
       (alpha-char-p (char scheme 0))))

(defun valid-host-p (host)
  "Check if host is valid (basic validation)"
  (and host
       (> (length host) 0)
       (not (find #\Space host))
       (not (string= host ""))))

(defun valid-url-p (url)
  "Check if URL is valid"
  (let ((url-obj (if (typep url 'url) url (parse-url url))))
    (and url-obj
         (or (not (url-scheme url-obj)) (valid-scheme-p (url-scheme url-obj)))
         (or (not (url-host url-obj)) (valid-host-p (url-host url-obj)))
         (or (not (url-port url-obj)) (and (integerp (url-port url-obj))
                                           (> (url-port url-obj) 0)
                                           (< (url-port url-obj) 65536))))))

;;;; ==========================================================================
;;;; Common Scheme Predicates
;;;; ==========================================================================

(defun http-url-p (url)
  "Check if URL is HTTP"
  (string= (url-scheme url) "http"))

(defun https-url-p (url)
  "Check if URL is HTTPS"
  (string= (url-scheme url) "https"))

(defun ftp-url-p (url)
  "Check if URL is FTP"
  (string= (url-scheme url) "ftp"))

(defun mailto-url-p (url)
  "Check if URL is mailto"
  (string= (url-scheme url) "mailto"))
