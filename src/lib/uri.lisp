(defpackage #:epsilon.lib.uri
  (:use
   #:cl)
  (:shadow
   #:merge)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string))
  (:export
   #:make-uri
   #:uri
   #:file-uri
   #:uri-p
   #:authority
   #:merge
   #:scheme
   #:userinfo
   #:host
   #:port
   #:path
   #:parent
   #:query
   #:fragment
   #:to-string
   #:url-decode
   #:url-encode
   #:url-encode-params
   ;; Path utilities
   #:ensure-directory-path
   #:ensure-file-path
   #:path-join))

(in-package #:epsilon.lib.uri)

;; URI structure
(defstruct (uri (:constructor make-uri)
                (:conc-name nil))
  "URI structure containing all components of a URI"
  (scheme nil :type (or string null))
  (userinfo nil :type (or string null))
  (host nil :type (or string null))
  (port nil :type (or integer null))
  (path nil :type (or string null))
  (query nil :type (or string null))
  (fragment nil :type (or string null)))

;; Constants
(defvar +scheme-ports+
  (map:make-map "ssh" 22
                "http" 80
                "https" 443
                "ws" 80
                "wss" 443))

(defun file-uri (path)
  (make-uri :scheme "file"
            :path path))

;; Helper functions
(defun scheme-default-port (scheme)
  "Return default port for a given scheme, or NIL if unknown"
  (map:get +scheme-ports+ scheme))

(defun normalize-path (path)
  (str:join #\/
            (seq:seq
             (reverse
              (seq:reduce (lambda (stack segment)
                            (cond ((string= segment ".")
                                   stack)
                                  ((string= segment "..")
                                   (cdr stack))
                                  (t
                                   (cons segment stack))))
                          (str:split #\/ path)
                          :initial-value nil)))))

(defun path-parent (path)
  "Get the parent of a path"
  (let ((last-slash (position #\/ path :from-end t)))
    (if last-slash
        (subseq path 0 last-slash)
        "")))

(defun parent (uri)
  "Get the parent URI (one directory level up)"
  (let ((parent-path (path-parent (path uri))))
    (make-uri :scheme (scheme uri)
              :userinfo (userinfo uri)
              :host (host uri)
              :port (port uri)
              :path parent-path
              :query nil
              :fragment nil)))

(defun ensure-path-slash (path)
  "Ensure path starts with a slash if not empty"
  (if (and path (plusp (length path)) (char/= (char path 0) #\/))
      (concatenate 'string "/" path)
      path))

(defun parse-uri-authority (authority)
  "Parse authority part into userinfo, host, and port"
  (let* ((userinfo nil)
         (host nil)
         (port nil)
         (user-end (position #\@ authority)))
    
    ;; Extract userinfo if present
    (when user-end
      (setf userinfo (subseq authority 0 user-end)
            authority (subseq authority (1+ user-end))))
    
    ;; Extract port if present
    (let ((port-start (position #\: authority)))
      (if port-start
          (progn
            (setf host (subseq authority 0 port-start))
            (let ((port-str (subseq authority (1+ port-start))))
              (when (plusp (length port-str))
                (setf port (parse-integer port-str :junk-allowed t)))))
          (setf host authority)))
    
    (values userinfo host port)))

(defun uri (uri-string)
  "Parse a URI string into a URI structure"
  (let* ((scheme-end (position #\: uri-string))
         (scheme (when scheme-end (subseq uri-string 0 scheme-end)))
         (rest (if scheme-end (subseq uri-string (1+ scheme-end)) uri-string))
         
         ;; Check for authority part (//...)
         (has-authority (and (> (length rest) 2)
                           (string= (subseq rest 0 2) "//")))
         (authority-start (if has-authority 2 0))
         
         ;; Find end of authority
         (path-start (and has-authority
                        (or (position #\/ rest :start authority-start)
                            (length rest))))
         
         ;; Extract authority if present
         (authority (when has-authority
                      (subseq rest authority-start path-start)))
         
         ;; Extract path
         (path-end (or (position #\? rest) 
                       (position #\# rest)
                       (length rest)))
         (path (if has-authority
                   (if (= path-start (length rest))
                       "/"
                       (subseq rest path-start path-end))
                   (subseq rest authority-start path-end)))
         
         ;; Extract query
         (query-start (position #\? rest))
         (query-end (or (and query-start (position #\# rest :start query-start))
                        (length rest)))
         (query (when query-start
                  (subseq rest (1+ query-start) query-end)))
         
         ;; Extract fragment
         (fragment-start (position #\# rest))
         (fragment (when fragment-start
                     (subseq rest (1+ fragment-start)))))
    
    ;; Parse authority into userinfo, host, port
    (multiple-value-bind (userinfo host port)
        (if authority
            (parse-uri-authority authority)
            (values nil nil nil))
      
      ;; Return URI structure
      (make-uri :scheme scheme
                :userinfo userinfo
                :host host
                :port (or port 
                          (and scheme (scheme-default-port scheme)))
                :path (ensure-path-slash path)
                :query query
                :fragment fragment))))

(defun authority (uri)
  "Get the authority part of a URI"
  (when (host uri)
    (let ((result (host uri)))
      (when (userinfo uri)
        (setf result (concatenate 'string (userinfo uri) "@" result)))
      (when (and (port uri)
                 (not (eql (port uri)
                          (scheme-default-port (scheme uri)))))
        (setf result (concatenate 'string result ":" (write-to-string (port uri)))))
      result)))

(defun trim-front (s c)
  (if (char= (char s 0) c)
      (subseq s 1)))

(defun merge (uri rel-path)
  "Merge a URI with a path string. Discards query and fragment.
   
   URI Path Handling Best Practice:
   - Directories should end with '/' in paths 
   - Files should NOT end with '/'
   - Double slashes '//' are avoided by checking separators
   - Empty relative paths are handled gracefully"
  (let ((result-path (if (and (> (length rel-path) 0)
                              (char= (char rel-path 0) #\/))
                         ;; Absolute path in rel-uri
                         rel-path
                         ;; Relative path - combine with base path
                         (let* ((base-path (or (path uri) "/"))
                                (needs-separator (and (> (length base-path) 0)
                                                      (> (length rel-path) 0)
                                                      (not (char= (char base-path (1- (length base-path))) #\/))
                                                      (not (char= (char rel-path 0) #\/)))))
                           (normalize-path (concatenate 'string 
                                                        base-path
                                                        (if needs-separator "/" "")
                                                        rel-path))))))
    (make-uri :scheme (scheme uri)
              :userinfo (userinfo uri)
              :host (host uri)
              :port (port uri)
              :path result-path)))

(defun to-string (uri)
  "Convert a URI structure to a string"
  (with-output-to-string (s)
    ;; Scheme
    (when (scheme uri)
      (write-string (scheme uri) s)
      (write-char #\: s))
    
    ;; Authority
    (when (host uri)
      (write-string "//" s)
      (when (userinfo uri)
        (write-string (userinfo uri) s)
        (write-char #\@ s))
      (write-string (host uri) s)
      (when (and (port uri)
                (or (null (scheme uri))
                    (not (eql (port uri)
                             (scheme-default-port (scheme uri))))))
        (write-char #\: s)
        (write-string (write-to-string (port uri)) s)))
    
    ;; Path
    (when (path uri)
      (write-string (path uri) s))
    
    ;; Query
    (when (query uri)
      (write-char #\? s)
      (write-string (query uri) s))
    
    ;; Fragment
    (when (fragment uri)
      (write-char #\# s)
      (write-string (fragment uri) s))))

;; URL encoding/decoding functions
(defun url-encode (string)
  "URL encode a string"
  (with-output-to-string (s)
    (loop for char across string
          do (cond
               ;; Unreserved characters according to RFC 3986
               ((or (char<= #\a char #\z)
                    (char<= #\A char #\Z)
                    (char<= #\0 char #\9)
                    (member char '(#\- #\_ #\. #\~)))
                (write-char char s))
               ;; Space gets special treatment
               ((char= char #\Space)
                (write-char #\+ s))
               ;; All other characters get percent-encoded
               (t (format s "%~2,'0X" (char-code char)))))))

(defun url-decode (string)
  "URL decode a string"
  (with-output-to-string (s)
    (let ((i 0)
          (len (length string)))
      (loop while (< i len) do
        (let ((char (char string i)))
          (cond
            ;; Handle + as space
            ((char= char #\+)
             (write-char #\Space s))
            ;; Handle percent encoding
            ((and (char= char #\%)
                  (< (+ i 2) len))
             (write-char (code-char (parse-integer (subseq string (1+ i) (+ i 3)) :radix 16)) s)
             (incf i 2))
            ;; Normal character
            (t (write-char char s))))
        (incf i)))))

(defun url-encode-params (params)
  "Encode a list of key-value pairs for use in URL query strings"
  (with-output-to-string (s)
    (loop for (key value) on params by #'cddr
          for i from 0
          do (unless (zerop i)
               (write-char #\& s))
             (write-string (url-encode (string key)) s)
             (write-char #\= s)
             (write-string (url-encode (string value)) s))))

;;;;  URI Path Utilities
;;;;
;;;;  Best Practices for URI Path Handling:
;;;;
;;;;  1. Directory paths SHOULD end with '/' to indicate they are directories
;;;;  2. File paths SHOULD NOT end with '/' 
;;;;  3. Double slashes '//' MUST be avoided in paths (except in scheme://authority)
;;;;  4. Empty path components should be handled gracefully
;;;;  5. Relative path merging should preserve the above conventions
;;;;
;;;;  Examples:
;;;;    Directory: "src/lib/"     (ends with /)
;;;;    File:      "src/lib/uri.lisp"  (no trailing /)
;;;;    Merge:     merge("src/lib/", "uri.lisp") → "src/lib/uri.lisp"

(defun ensure-directory-path (path)
  "Ensure path ends with '/' to indicate it's a directory.
   Returns the path with trailing '/' added if not present."
  (if (and (> (length path) 0)
           (not (char= (char path (1- (length path))) #\/)))
      (concatenate 'string path "/")
      path))

(defun ensure-file-path (path)
  "Ensure path does not end with '/' to indicate it's a file.
   Returns the path with trailing '/' removed if present."
  (if (and (> (length path) 0)
           (char= (char path (1- (length path))) #\/))
      (subseq path 0 (1- (length path)))
      path))

(defun path-join (&rest components)
  "Join path components with proper separator handling.
   Avoids double slashes and handles empty components gracefully.
   
   Example: (path-join \"src\" \"lib/\" \"uri.lisp\") → \"src/lib/uri.lisp\""
  (when components
    (let ((result (first components)))
      (dolist (component (rest components))
        (when (and component (> (length component) 0))
          (let ((needs-separator (and (> (length result) 0)
                                      (not (char= (char result (1- (length result))) #\/))
                                      (not (char= (char component 0) #\/)))))
            (setf result (concatenate 'string 
                                      result
                                      (if needs-separator "/" "")
                                      component)))))
      result)))
