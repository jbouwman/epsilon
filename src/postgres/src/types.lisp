;;;; PostgreSQL Type Mapping
;;;;
;;;; Handles conversion between PostgreSQL types and Lisp values

(defpackage :epsilon.postgres.types
  (:use :cl)
  (:local-nicknames
   (#:str #:epsilon.string)
   (#:bin #:epsilon.binary))
  (:export
   #:*type-mappings*
   #:register-type-mapping
   #:decode-column-value
   #:encode-parameter
   ;; OID constants
   #:+oid-bool+ #:+oid-int2+ #:+oid-int4+ #:+oid-int8+
   #:+oid-float4+ #:+oid-float8+ #:+oid-text+ #:+oid-varchar+
   #:+oid-json+ #:+oid-jsonb+ #:+oid-uuid+))

(in-package :epsilon.postgres.types)

;;; PostgreSQL type OIDs (Object Identifiers)

(defconstant +oid-bool+ 16)
(defconstant +oid-bytea+ 17)
(defconstant +oid-char+ 18)
(defconstant +oid-name+ 19)
(defconstant +oid-int8+ 20)      ; bigint
(defconstant +oid-int2+ 21)      ; smallint
(defconstant +oid-int2vector+ 22)
(defconstant +oid-int4+ 23)      ; integer
(defconstant +oid-regproc+ 24)
(defconstant +oid-text+ 25)
(defconstant +oid-oid+ 26)
(defconstant +oid-tid+ 27)
(defconstant +oid-xid+ 28)
(defconstant +oid-cid+ 29)
(defconstant +oid-oidvector+ 30)
(defconstant +oid-json+ 114)
(defconstant +oid-xml+ 142)
(defconstant +oid-pgnodetree+ 194)
(defconstant +oid-point+ 600)
(defconstant +oid-lseg+ 601)
(defconstant +oid-path+ 602)
(defconstant +oid-box+ 603)
(defconstant +oid-polygon+ 604)
(defconstant +oid-line+ 628)
(defconstant +oid-float4+ 700)   ; real
(defconstant +oid-float8+ 701)   ; double precision
(defconstant +oid-abstime+ 702)
(defconstant +oid-reltime+ 703)
(defconstant +oid-tinterval+ 704)
(defconstant +oid-unknown+ 705)
(defconstant +oid-circle+ 718)
(defconstant +oid-cash+ 790)     ; money
(defconstant +oid-macaddr+ 829)
(defconstant +oid-inet+ 869)
(defconstant +oid-cidr+ 650)
(defconstant +oid-macaddr8+ 774)
(defconstant +oid-aclitem+ 1033)
(defconstant +oid-bpchar+ 1042)  ; char(n)
(defconstant +oid-varchar+ 1043) ; varchar(n)
(defconstant +oid-date+ 1082)
(defconstant +oid-time+ 1083)
(defconstant +oid-timestamp+ 1114)
(defconstant +oid-timestamptz+ 1184)
(defconstant +oid-interval+ 1186)
(defconstant +oid-timetz+ 1266)
(defconstant +oid-bit+ 1560)
(defconstant +oid-varbit+ 1562)
(defconstant +oid-numeric+ 1700)
(defconstant +oid-refcursor+ 1790)
(defconstant +oid-regprocedure+ 2202)
(defconstant +oid-regoper+ 2203)
(defconstant +oid-regoperator+ 2204)
(defconstant +oid-regclass+ 2205)
(defconstant +oid-regtype+ 2206)
(defconstant +oid-record+ 2249)
(defconstant +oid-cstring+ 2275)
(defconstant +oid-any+ 2276)
(defconstant +oid-anyarray+ 2277)
(defconstant +oid-void+ 2278)
(defconstant +oid-trigger+ 2279)
(defconstant +oid-language-handler+ 2280)
(defconstant +oid-internal+ 2281)
(defconstant +oid-opaque+ 2282)
(defconstant +oid-anyelement+ 2283)
(defconstant +oid-anynonarray+ 2776)
(defconstant +oid-anyenum+ 3500)
(defconstant +oid-fdw-handler+ 3115)
(defconstant +oid-anyrange+ 3831)
(defconstant +oid-uuid+ 2950)
(defconstant +oid-jsonb+ 3802)

;;; Array type OIDs (base type OID + 1000 for most types)
(defconstant +oid-bool-array+ 1000)
(defconstant +oid-int2-array+ 1005)
(defconstant +oid-int4-array+ 1007)
(defconstant +oid-int8-array+ 1016)
(defconstant +oid-float4-array+ 1021)
(defconstant +oid-float8-array+ 1022)
(defconstant +oid-text-array+ 1009)
(defconstant +oid-varchar-array+ 1015)
(defconstant +oid-numeric-array+ 1231)

;;; Global type mapping registry

(defparameter *type-mappings* (make-hash-table)
  "Global registry of type decoders and encoders")

(defstruct type-mapping
  "Type conversion mapping"
  (decoder nil :type (or null function))
  (encoder nil :type (or null function))
  (lisp-type t)
  (description "" :type string))

(defun register-type-mapping (oid decoder encoder &key lisp-type description)
  "Register a type mapping for a PostgreSQL type OID"
  (setf (gethash oid *type-mappings*)
        (make-type-mapping :decoder decoder
                           :encoder encoder
                           :lisp-type (or lisp-type t)
                           :description (or description ""))))

;;; Column value decoding

(defun decode-column-value (bytes type-oid format-code)
  "Decode a column value from PostgreSQL format"
  (when (null bytes)
    (return-from decode-column-value nil))
  
  (let ((mapping (gethash type-oid *type-mappings*)))
    (if (and mapping (type-mapping-decoder mapping))
        ;; Use registered decoder
        (funcall (type-mapping-decoder mapping) bytes format-code)
        ;; Use default decoder
        (default-decode-value bytes type-oid format-code))))

(defun default-decode-value (bytes type-oid format-code)
  "Default decoding for unknown types"
  (case format-code
    (0 ;; Text format
     (let ((text (str:octets-to-string bytes)))
       (case type-oid
         ((#.+oid-bool+)
          (string= text "t"))
         
         ((#.+oid-int2+ #.+oid-int4+ #.+oid-int8+)
          (parse-integer text))
         
         ((#.+oid-float4+ #.+oid-float8+)
          (parse-float text))
         
         ((#.+oid-numeric+)
          ;; Parse numeric as rational or float
          (if (position #\. text)
              (parse-float text)
              (parse-integer text)))
         
         ((#.+oid-text+ #.+oid-varchar+ #.+oid-bpchar+ #.+oid-name+)
          text)
         
         ((#.+oid-bytea+)
          ;; Decode hex format (\x followed by hex digits)
          (if (str:starts-with-p text "\\x")
              (decode-hex-bytea (subseq text 2))
              (decode-escape-bytea text)))
         
         ((#.+oid-json+ #.+oid-jsonb+)
          ;; Return as string for now, could parse JSON
          text)
         
         ((#.+oid-date+)
          (parse-date text))
         
         ((#.+oid-time+ #.+oid-timetz+)
          (parse-time text))
         
         ((#.+oid-timestamp+ #.+oid-timestamptz+)
          (parse-timestamp text))
         
         ((#.+oid-uuid+)
          text)  ; Return UUID as string
         
         ;; Array types
         ((#.+oid-int4-array+ #.+oid-int2-array+ #.+oid-int8-array+)
          (parse-array text #'parse-integer))
         
         ((#.+oid-float4-array+ #.+oid-float8-array+)
          (parse-array text #'parse-float))
         
         ((#.+oid-text-array+ #.+oid-varchar-array+)
          (parse-array text #'identity))
         
         ((#.+oid-bool-array+)
          (parse-array text (lambda (s) (string= s "t"))))
         
         (otherwise
          ;; Unknown type, return as string
          text))))
    
    (1 ;; Binary format
     ;; Binary format decoding would go here
     ;; For now, return as byte array
     bytes)
    
    (otherwise
     (error 'postgres-protocol-error
            :message (format nil "Unknown format code: ~D" format-code)))))

;;; Type-specific parsers

(defun parse-float (text)
  "Parse floating point number from text"
  (cond
    ((string= text "NaN") :nan)
    ((string= text "Infinity") :positive-infinity)
    ((string= text "-Infinity") :negative-infinity)
    (t (read-from-string text))))

(defun parse-date (text)
  "Parse PostgreSQL date (YYYY-MM-DD)"
  ;; Return as string for now, could return date objects
  text)

(defun parse-time (text)
  "Parse PostgreSQL time"
  ;; Return as string for now, could return time objects
  text)

(defun parse-timestamp (text)
  "Parse PostgreSQL timestamp"
  ;; Return as string for now, could return timestamp objects
  text)

(defun parse-array (text element-parser)
  "Parse PostgreSQL array text representation"
  ;; Basic array parsing for {elem1,elem2,elem3} format
  (when (and (str:starts-with-p text "{")
             (str:ends-with-p text "}"))
    (let ((content (subseq text 1 (1- (length text)))))
      (if (string= content "")
          '()  ; Empty array
          (mapcar (lambda (elem)
                    (let ((trimmed (str:trim elem)))
                      (if (string= trimmed "NULL")
                          nil
                          (funcall element-parser trimmed))))
                  (str:split #\, content))))))

(defun decode-hex-bytea (hex-string)
  "Decode hex-encoded bytea"
  (let ((bytes (make-array (/ (length hex-string) 2) 
                           :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length bytes)
          for hex-pos = (* i 2)
          do (setf (aref bytes i)
                   (parse-integer (subseq hex-string hex-pos (+ hex-pos 2))
                                  :radix 16)))
    bytes))

(defun decode-escape-bytea (text)
  "Decode escape-encoded bytea"
  ;; Simple implementation for common escape sequences
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop for i from 0 below (length text)
          for char = (char text i)
          do (cond
               ((char= char #\\)
                (if (< (+ i 3) (length text))
                    ;; Octal escape \nnn
                    (let ((octal (subseq text (1+ i) (+ i 4))))
                      (when (every (lambda (c) (char<= #\0 c #\7)) octal)
                        (vector-push-extend (parse-integer octal :radix 8) result)
                        (incf i 3)))
                    (vector-push-extend (char-code char) result)))
               (t
                (vector-push-extend (char-code char) result))))
    result))

;;; Parameter encoding

(defun encode-parameter (value type-oid)
  "Encode a Lisp value for PostgreSQL"
  (let ((mapping (gethash type-oid *type-mappings*)))
    (if (and mapping (type-mapping-encoder mapping))
        ;; Use registered encoder
        (funcall (type-mapping-encoder mapping) value)
        ;; Use default encoder
        (default-encode-value value type-oid))))

(defun default-encode-value (value type-oid)
  "Default encoding for parameters"
  (typecase value
    (null 
     nil)  ; NULL
    
    (string 
     (str:string-to-octets value))
    
    (integer 
     (str:string-to-octets (princ-to-string value)))
    
    (float 
     (str:string-to-octets (princ-to-string value)))
    
    ((eql t) 
     (str:string-to-octets "true"))
    
    ((eql nil) 
     (str:string-to-octets "false"))
    
    (vector 
     ;; Assume byte array
     value)
    
    (list
     ;; Encode as array
     (str:string-to-octets 
      (format nil "{~{~A~^,~}}" 
              (mapcar (lambda (elem)
                        (if (null elem)
                            "NULL"
                            (typecase elem
                              (string (format nil "\"~A\"" 
                                              (str:replace-all "\"" "\\\"" elem)))
                              (otherwise (princ-to-string elem)))))
                      value))))
    
    (otherwise
     ;; Convert to string representation
     (str:string-to-octets (princ-to-string value)))))

;;; Initialize built-in type mappings

(defun initialize-type-mappings ()
  "Initialize built-in PostgreSQL type mappings"
  
  ;; Boolean
  (register-type-mapping +oid-bool+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (string= (str:octets-to-string bytes) "t"))
                         (lambda (value)
                           (str:string-to-octets (if value "true" "false")))
                         :lisp-type 'boolean
                         :description "Boolean")
  
  ;; Integers
  (register-type-mapping +oid-int2+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (parse-integer (str:octets-to-string bytes)))
                         (lambda (value)
                           (str:string-to-octets (princ-to-string value)))
                         :lisp-type 'integer
                         :description "16-bit integer")
  
  (register-type-mapping +oid-int4+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (parse-integer (str:octets-to-string bytes)))
                         (lambda (value)
                           (str:string-to-octets (princ-to-string value)))
                         :lisp-type 'integer
                         :description "32-bit integer")
  
  (register-type-mapping +oid-int8+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (parse-integer (str:octets-to-string bytes)))
                         (lambda (value)
                           (str:string-to-octets (princ-to-string value)))
                         :lisp-type 'integer
                         :description "64-bit integer")
  
  ;; Floating point
  (register-type-mapping +oid-float4+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (parse-float (str:octets-to-string bytes)))
                         (lambda (value)
                           (str:string-to-octets (princ-to-string value)))
                         :lisp-type 'single-float
                         :description "Single precision float")
  
  (register-type-mapping +oid-float8+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (parse-float (str:octets-to-string bytes)))
                         (lambda (value)
                           (str:string-to-octets (princ-to-string value)))
                         :lisp-type 'double-float
                         :description "Double precision float")
  
  ;; Text types
  (register-type-mapping +oid-text+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (str:octets-to-string bytes))
                         (lambda (value)
                           (str:string-to-octets value))
                         :lisp-type 'string
                         :description "Variable length text")
  
  (register-type-mapping +oid-varchar+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (str:octets-to-string bytes))
                         (lambda (value)
                           (str:string-to-octets value))
                         :lisp-type 'string
                         :description "Variable length character")
  
  ;; JSON types
  (register-type-mapping +oid-json+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           ;; Return as string, could parse with JSON library
                           (str:octets-to-string bytes))
                         (lambda (value)
                           (str:string-to-octets
                            (typecase value
                              (string value)
                              (otherwise (princ-to-string value)))))
                         :lisp-type 't
                         :description "JSON data")
  
  (register-type-mapping +oid-jsonb+
                         (lambda (bytes format-code)
                           (declare (ignore format-code))
                           (str:octets-to-string bytes))
                         (lambda (value)
                           (str:string-to-octets
                            (typecase value
                              (string value)
                              (otherwise (princ-to-string value)))))
                         :lisp-type 't
                         :description "Binary JSON data"))

;; Initialize type mappings when module loads
(eval-when (:load-toplevel :execute)
  (initialize-type-mappings))