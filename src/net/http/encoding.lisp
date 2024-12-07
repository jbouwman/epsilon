(defpackage epsilon.net.http.encoding
  (:use
   #:cl
   #:epsilon.lib.char
   #:epsilon.lib.type)
  (:export :detect-charset))

(in-package :epsilon.net.http.encoding)

(defun parse-content-type (content-type)
  (let ((types
          (nth-value 1
                     (epsilon.lib.regex:scan-to-strings "^\\s*?(\\w+)/([^;\\s]+)(?:\\s*;\\s*charset=([A-Za-z0-9_-]+))?"
                                            content-type))))
    (when types
      (values (aref types 0)
              (aref types 1)
              (aref types 2)))))

(defun charset-to-encoding (charset &optional
                                      (default epsilon.lib.char:*default-character-encoding*))
  (cond
    ((null charset)
     default)
    ((string-equal charset "utf-8")
     :utf-8)
    ((string-equal charset "euc-jp")
     :eucjp)
    ((or (string-equal charset "shift_jis")
         (string-equal charset "shift-jis"))
     :cp932)
    ((string-equal charset "windows-31j")
     :cp932)
    (t (or (find charset (epsilon.lib.char:list-character-encodings)
                 :test #'string-equal)
           default))))

(defun detect-charset (content-type body)
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (cond
      ((charset-to-encoding charset nil))
      ((string-equal type "text")
       (or (charset-to-encoding charset nil)
           (if (and (string-equal subtype "html")
                    (typep body '->u8))
               (charset-to-encoding (detect-charset-from-html body) nil)
               nil)
           :utf-8))
      ((and (string-equal type "application")
            (or (string-equal subtype "json")
                (string-equal subtype "javascript")))
       ;; According to RFC4627 (http://www.ietf.org/rfc/rfc4627.txt),
       ;; JSON text SHALL be encoded in Unicode. The default encoding is UTF-8.
       ;; It's possible to determine if the encoding is UTF-16 or UTF-36
       ;; by looking at the first four octets, however, I leave it to the future.
       ;;
       ;; According to RFC4329 (https://datatracker.ietf.org/doc/html/rfc4329),
       ;; javascript also is specified by charset, or defaults to UTF-8
       ;; It's also possible to specify in the first four octets, but
       ;; like application/json I leave it to the future.
       (charset-to-encoding charset :utf-8))
      ((and (string-equal type "application")
            (epsilon.lib.regex:scan "(?:[^+]+\\+)?xml" subtype))
       (charset-to-encoding charset)))))

(defun detect-charset-from-html (body)
  "Detect the body's charset by (roughly) searching meta tags which has \"charset\" attribute."
  (labels ((find-meta (start)
             (search #.(epsilon.lib.char:string-to-u8 "<meta ") body :start2 start))
           (main (start)
             (let ((start (find-meta start)))
               (unless start
                 (return-from main nil))
               (let ((end (position (char-code #\>) body :start start :test #'=)))
                 (unless end
                   (return-from main nil))
                 (incf end)
                 (let ((match (nth-value 1 (epsilon.lib.regex:scan-to-strings
                                            "charset=[\"']?([^\\s\"'>]+)[\"']?"
                                            (epsilon.lib.char:u8-to-string body :start start :end end :errorp nil)))))
                   (if match
                       (aref match 0)
                       (main end)))))))
    (main 0)))
