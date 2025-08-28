(defpackage epsilon.foreign.marshalling
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (trampoline epsilon.foreign.trampoline))
  (:export
   ;; Type inference
   #:infer-function-signature
   #:register-known-signature
   #:*known-signatures*
   
   ;; Array handling
   #:with-pinned-array
   #:with-string-array
   #:with-output-array
   
   ;; Enum support
   #:define-enum
   #:enum-value
   #:enum-keyword
   
   
   ;; Smart marshalling
   #:defshared-auto
   
   ;; Type definitions
   #:define-c-type
   
   ;; Error handling  
   #:foreign-error
   #:foreign-error-p
   #:foreign-error-code
   #:foreign-error-function
   
   ;; Converters
   #:bool-to-foreign
   #:foreign-to-bool))

(in-package :epsilon.foreign.marshalling)

;;;; Automatic Type Marshalling for FFI

;;; Known function signatures database

(defvar *known-signatures* map:+empty+
  "Database of known C function signatures")

(defun register-known-signature (name return-type arg-types)
  "Register a known C function signature"
  (setf *known-signatures* (map:assoc *known-signatures* name
                                      (trampoline:make-ffi-signature :return-type return-type
                                                                     :arg-types arg-types))))

;; Initialize signatures lazily when needed
(defvar *signatures-initialized-p* nil
  "Whether we've initialized the signature database")

(defun ensure-signatures-initialized ()
  "Ensure signature database is populated"
  (unless *signatures-initialized-p*
    (initialize-standard-signatures)
    (setf *signatures-initialized-p* t)))

(defun initialize-standard-signatures ()
  "Initialize with a minimal set of standard signatures"
  ;; Just the most essential ones for bootstrapping
  (register-known-signature "strlen" :size-t '(:string))
  (register-known-signature "strcpy" :pointer '(:pointer :string))
  (register-known-signature "strcmp" :int '(:string :string))
  (register-known-signature "malloc" :pointer '(:size-t))
  (register-known-signature "free" :void '(:pointer))
  (register-known-signature "puts" :int '(:string))
  (register-known-signature "getenv" :string '(:string))
  (register-known-signature "getpid" :pid-t '())
  ;; Remove the rest - they'll be extracted on demand
  )

(defun extract-and-register-signature (function-name library)
  "Try to extract and register a function signature"
  (declare (ignore library)) ; For now
  ;; Try multiple methods to get the signature
  (let ((signature 
         (or
          ;; Method 1: Try parsing from cached headers
          (extract-from-parsed-headers function-name)
          
          ;; Method 2: Fall back to pattern matching
          (let ((pattern-sig (infer-from-name-pattern function-name)))
            (when pattern-sig
              (trampoline:make-ffi-signature 
               :return-type (getf pattern-sig :return-type)
               :arg-types (getf pattern-sig :arg-types)))))))
    
    (when signature
      (setf *known-signatures* (map:assoc *known-signatures* function-name signature))
      signature)))

(defvar *parsed-headers-cache* nil
  "Cache of parsed standard headers")

(defun ensure-headers-parsed ()
  "Parse standard headers once"
  (unless *parsed-headers-cache*
    (setf *parsed-headers-cache* (parse-standard-headers)))
  *parsed-headers-cache*)

(defun parse-standard-headers ()
  "Parse common header declarations using clang parser"
  (let ((sigs map:+empty+))
    ;; Try to parse actual headers if clang parser is available
    (when (find-package "EPSILON.CLANG")
      (let ((parsed-sigs (parse-c-declarations)))
        (when parsed-sigs
          (return-from parse-standard-headers parsed-sigs))))
    ;; Fall back to hardcoded signatures
    ;; String functions from string.h
    (setf sigs (map:assoc sigs "strlen"
                          (trampoline:make-ffi-signature :return-type :size-t :arg-types '(:string))))
    (setf sigs (map:assoc sigs "strcpy"
                          (trampoline:make-ffi-signature :return-type :string :arg-types '(:string :string))))
    (setf sigs (map:assoc sigs "strcmp"
                          (trampoline:make-ffi-signature :return-type :int :arg-types '(:string :string))))
    (setf sigs (map:assoc sigs "strcat"
                          (trampoline:make-ffi-signature :return-type :string :arg-types '(:string :string))))
    
    ;; Memory functions from stdlib.h
    (setf sigs (map:assoc sigs "malloc"
                          (trampoline:make-ffi-signature :return-type :pointer :arg-types '(:size-t))))
    (setf sigs (map:assoc sigs "free"
                          (trampoline:make-ffi-signature :return-type :void :arg-types '(:pointer))))
    (setf sigs (map:assoc sigs "memcpy"
                          (trampoline:make-ffi-signature :return-type :pointer :arg-types '(:pointer :pointer :size-t))))
    
    ;; I/O functions from stdio.h
    (setf sigs (map:assoc sigs "printf"
                          (trampoline:make-ffi-signature :return-type :int :arg-types '(:string &rest))))
    (setf sigs (map:assoc sigs "puts"
                          (trampoline:make-ffi-signature :return-type :int :arg-types '(:string))))
    (setf sigs (map:assoc sigs "getchar"
                          (trampoline:make-ffi-signature :return-type :int :arg-types '())))
    
    ;; System functions
    (setf sigs (map:assoc sigs "getpid"
                          (trampoline:make-ffi-signature :return-type :pid-t :arg-types '())))
    (setf sigs (map:assoc sigs "getenv"
                          (trampoline:make-ffi-signature :return-type :string :arg-types '(:string))))
    
    sigs))

(defun extract-from-parsed-headers (function-name)
  "Look up function in parsed headers"
  (let ((cache (ensure-headers-parsed)))
    (map:get cache function-name)))

(defun parse-c-declarations ()
  "Parse C function declarations from standard headers"
  (let ((sigs map:+empty+)
        (clang-pkg (find-package "EPSILON.CLANG")))
    (when clang-pkg
      ;; Parse some common function declarations
      (dolist (decl '("size_t strlen(const char *s);"
                      "char *strcpy(char *dest, const char *src);"
                      "int strcmp(const char *s1, const char *s2);"
                      "void *malloc(size_t size);"
                      "void free(void *ptr);"
                      "int puts(const char *s);"
                      "int printf(const char *format, ...);"
                      "char *getenv(const char *name);"
                      "pid_t getpid(void);"))
        (handler-case
            (let* ((tokenize-fn (find-symbol "TOKENIZE" clang-pkg))
                   (parser-pkg (find-package "EPSILON.PARSER"))
                   (run-fn (when parser-pkg (find-symbol "RUN" parser-pkg)))
                   (func-decl-fn (find-symbol "FUNCTION-DECLARATION" clang-pkg)))
              (when (and tokenize-fn run-fn func-decl-fn)
                (let* ((stream (make-string-input-stream decl))
                       (tokens (funcall tokenize-fn stream))
                       (ast (funcall run-fn (funcall func-decl-fn) tokens)))
                  (when (and ast (epsilon.parser:success-p ast))
                    (let ((parsed (epsilon.parser:success-value ast)))
                      (when parsed
                        (let ((sig (extract-signature-from-ast parsed)))
                          (when sig
                            (setf sigs (map:assoc sigs (first sig) (second sig)))))))))))
          (error (e) 
		 (declare (ignore e))
		 nil))))
    sigs))

(defun extract-signature-from-ast (ast)
  "Extract function signature from parsed AST"
  (when (and (listp ast) (getf ast :type))
    (case (getf ast :type)
	  (:function
	   (let ((name (getf ast :name))
		 (return-type (ast-type-to-ffi (getf ast :return-type)))
		 (params (mapcar #'ast-param-to-ffi (getf ast :parameters))))
             (when name
               (list name
                     (trampoline:make-ffi-signature 
                      :return-type return-type
                      :arg-types params)))))
	  (otherwise nil))))

(defun ast-type-to-ffi (type-spec)
  "Convert AST type to FFI type"
  (cond
   ((null type-spec) :void)
   ((stringp type-spec)
    (c-string-to-ffi-type type-spec))
   ((listp type-spec)
    (let ((type-str (if (every #'stringp type-spec)
                        (format nil "~{~A~^ ~}" type-spec)
                      (format nil "~{~A~^ ~}" 
                              (mapcar #'ast-token-to-string type-spec)))))
      (c-string-to-ffi-type type-str)))
   (t :pointer)))

(defun ast-token-to-string (token)
  "Convert AST token to string"
  (cond
   ((stringp token) token)
   ((and (listp token) (eq (first token) :token))
    (third token))
   (t "")))

(defun c-string-to-ffi-type (type-str)
  "Convert C type string to FFI type"
  (cond
   ((search "void" type-str) :void)
   ((search "char *" type-str) :string)
   ((search "const char *" type-str) :string)
   ((search "size_t" type-str) :size-t)
   ((search "pid_t" type-str) :pid-t)
   ((search "int" type-str) :int)
   ((search "*" type-str) :pointer)
   (t :pointer)))

(defun ast-param-to-ffi (param)
  "Convert AST parameter to FFI type"
  (cond
   ((null param) :void)
   ((stringp param) (c-string-to-ffi-type param))
   ((and (listp param) (getf param :specifiers))
    (ast-type-to-ffi (getf param :specifiers)))
   ((listp param)
    (ast-type-to-ffi param))
   (t :pointer)))

(defun infer-from-name-pattern (name)
  "Infer signature from function name patterns"
  (cond
   ;; String functions
   ((search "str" name)
    (cond
     ((search "len" name) '(:return-type :size-t :arg-types (:string)))
     ((search "cpy" name) '(:return-type :pointer :arg-types (:pointer :string)))
     ((search "cmp" name) '(:return-type :int :arg-types (:string :string)))
     ((search "cat" name) '(:return-type :pointer :arg-types (:pointer :string)))
     ((search "chr" name) '(:return-type :pointer :arg-types (:string :int)))
     (t '(:return-type :pointer :arg-types (:pointer)))))
   
   ;; Memory functions
   ((search "mem" name)
    (cond
     ((search "cpy" name) '(:return-type :pointer :arg-types (:pointer :pointer :size-t)))
     ((search "set" name) '(:return-type :pointer :arg-types (:pointer :int :size-t)))
     ((search "cmp" name) '(:return-type :int :arg-types (:pointer :pointer :size-t)))
     (t '(:return-type :pointer :arg-types (:pointer :size-t)))))
   
   ;; Allocation functions
   ((or (string= name "malloc") (string= name "calloc"))
    '(:return-type :pointer :arg-types (:size-t)))
   ((string= name "free")
    '(:return-type :void :arg-types (:pointer)))
   
   ;; I/O functions
   ((string= name "puts")
    '(:return-type :int :arg-types (:string)))
   ((string= name "gets")
    '(:return-type :string :arg-types (:pointer)))
   ((search "printf" name)
    '(:return-type :int :arg-types (:string &rest)))
   
   ;; Math functions
   ((or (member name '("sin" "cos" "tan" "exp" "log" "sqrt") :test #'string=))
    '(:return-type :double :arg-types (:double)))
   ((string= name "pow")
    '(:return-type :double :arg-types (:double :double)))
   
   ;; System functions
   ((string= name "getpid")
    '(:return-type :pid-t :arg-types ()))
   ((string= name "getenv")
    '(:return-type :string :arg-types (:string)))
   
   ;; Predicates
   ((and (> (length name) 2) (string= (subseq name 0 2) "is"))
    '(:return-type :int :arg-types (:int)))
   
   ;; Conversion functions
   ((and (> (length name) 2) (string= (subseq name 0 2) "to"))
    '(:return-type :int :arg-types (:int)))
   
   ;; Default - unknown
   (t nil)))

;; All hardcoded signatures removed - they're now extracted on demand

(register-known-signature "realloc" :pointer '(:pointer :size-t))
(register-known-signature "free" :void '(:pointer))

(register-known-signature "memcpy" :pointer '(:pointer :pointer :size-t))
(register-known-signature "memmove" :pointer '(:pointer :pointer :size-t))
(register-known-signature "memset" :pointer '(:pointer :int :size-t))
(register-known-signature "memcmp" :int '(:pointer :pointer :size-t))
(register-known-signature "memchr" :pointer '(:pointer :int :size-t))

(register-known-signature "printf" :int '(:string &rest))
(register-known-signature "sprintf" :int '(:pointer :string &rest))
(register-known-signature "snprintf" :int '(:pointer :size-t :string &rest))
(register-known-signature "fprintf" :int '(:pointer :string &rest))
(register-known-signature "scanf" :int '(:string &rest))
(register-known-signature "sscanf" :int '(:string :string &rest))
(register-known-signature "fscanf" :int '(:pointer :string &rest))

(register-known-signature "fopen" :pointer '(:string :string))
(register-known-signature "fclose" :int '(:pointer))
(register-known-signature "fread" :size-t '(:pointer :size-t :size-t :pointer))
(register-known-signature "fwrite" :size-t '(:pointer :size-t :size-t :pointer))
(register-known-signature "fseek" :int '(:pointer :long :int))
(register-known-signature "ftell" :long '(:pointer))
(register-known-signature "rewind" :void '(:pointer))
(register-known-signature "fflush" :int '(:pointer))
(register-known-signature "feof" :int '(:pointer))
(register-known-signature "ferror" :int '(:pointer))

(register-known-signature "getenv" :string '(:string))
(register-known-signature "setenv" :int '(:string :string :int))
(register-known-signature "unsetenv" :int '(:string))
(register-known-signature "putenv" :int '(:string))

(register-known-signature "exit" :void '(:int))
(register-known-signature "abort" :void '())
(register-known-signature "atexit" :int '(:pointer))
(register-known-signature "system" :int '(:string))

(register-known-signature "abs" :int '(:int))
(register-known-signature "labs" :long '(:long))
(register-known-signature "div" :pointer '(:int :int))
(register-known-signature "ldiv" :pointer '(:long :long))

(register-known-signature "qsort" :void '(:pointer :size-t :size-t :pointer))
(register-known-signature "bsearch" :pointer '(:pointer :pointer :size-t :size-t :pointer))

(register-known-signature "time" :time-t '(:pointer))
(register-known-signature "clock" :clock-t '())
(register-known-signature "difftime" :double '(:time-t :time-t))
(register-known-signature "mktime" :time-t '(:pointer))
(register-known-signature "strftime" :size-t '(:pointer :size-t :string :pointer))
(register-known-signature "localtime" :pointer '(:pointer))
(register-known-signature "gmtime" :pointer '(:pointer))

(register-known-signature "isalpha" :int '(:int))
(register-known-signature "isdigit" :int '(:int))
(register-known-signature "isalnum" :int '(:int))
(register-known-signature "isspace" :int '(:int))
(register-known-signature "isupper" :int '(:int))
(register-known-signature "islower" :int '(:int))
(register-known-signature "toupper" :int '(:int))
(register-known-signature "tolower" :int '(:int))

(register-known-signature "sin" :double '(:double))
(register-known-signature "cos" :double '(:double))
(register-known-signature "tan" :double '(:double))
(register-known-signature "asin" :double '(:double))
(register-known-signature "acos" :double '(:double))
(register-known-signature "atan" :double '(:double))
(register-known-signature "atan2" :double '(:double :double))
(register-known-signature "sinh" :double '(:double))
(register-known-signature "cosh" :double '(:double))
(register-known-signature "tanh" :double '(:double))
(register-known-signature "exp" :double '(:double))
(register-known-signature "log" :double '(:double))
(register-known-signature "log10" :double '(:double))
(register-known-signature "pow" :double '(:double :double))
(register-known-signature "sqrt" :double '(:double))
(register-known-signature "ceil" :double '(:double))
(register-known-signature "floor" :double '(:double))
(register-known-signature "fabs" :double '(:double))
(register-known-signature "fmod" :double '(:double :double))

;; POSIX functions
(register-known-signature "open" :int '(:string :int &rest))
(register-known-signature "close" :int '(:int))
(register-known-signature "read" :ssize-t '(:int :pointer :size-t))
(register-known-signature "write" :ssize-t '(:int :pointer :size-t))
(register-known-signature "lseek" :off-t '(:int :off-t :int))
(register-known-signature "pipe" :int '(:pointer)) ; int[2]
(register-known-signature "dup" :int '(:int))
(register-known-signature "dup2" :int '(:int :int))

(register-known-signature "fork" :pid-t '())
(register-known-signature "execve" :int '(:string :pointer :pointer))
(register-known-signature "wait" :pid-t '(:pointer))
(register-known-signature "waitpid" :pid-t '(:pid-t :pointer :int))
(register-known-signature "kill" :int '(:pid-t :int))

(register-known-signature "getpid" :pid-t '())
(register-known-signature "getppid" :pid-t '())
(register-known-signature "getuid" :uid-t '())
(register-known-signature "geteuid" :uid-t '())
(register-known-signature "getgid" :gid-t '())
(register-known-signature "getegid" :gid-t '())

(register-known-signature "getcwd" :string '(:pointer :size-t))
(register-known-signature "chdir" :int '(:string))
(register-known-signature "mkdir" :int '(:string :mode-t))
(register-known-signature "rmdir" :int '(:string))
(register-known-signature "unlink" :int '(:string))
(register-known-signature "rename" :int '(:string :string))

(register-known-signature "stat" :int '(:string :pointer))
(register-known-signature "fstat" :int '(:int :pointer))
(register-known-signature "lstat" :int '(:string :pointer))

(register-known-signature "chmod" :int '(:string :mode-t))
(register-known-signature "chown" :int '(:string :uid-t :gid-t))

(register-known-signature "puts" :int '(:string))
(register-known-signature "gets" :string '(:pointer))
(register-known-signature "getchar" :int '())
(register-known-signature "putchar" :int '(:int))

(register-known-signature "strtol" :long '(:string :pointer :int))
(register-known-signature "strtoul" :unsigned-long '(:string :pointer :int))
(register-known-signature "strtod" :double '(:string :pointer))
(register-known-signature "atoi" :int '(:string))
(register-known-signature "atol" :long '(:string))
(register-known-signature "atof" :double '(:string))

(defun infer-function-signature (name)
  "Infer the signature of a C function by name"
  (or (map:get *known-signatures* name)
      ;; If not in database, make educated guesses based on naming patterns
      (cond
       ;; Functions starting with 'is' usually return int (boolean)
       ((and (>= (length name) 2) (string= (subseq name 0 2) "is"))
        (trampoline:make-ffi-signature :return-type :int 
                                       :arg-types '(:int)))
       ;; Functions starting with 'str' usually work with strings
       ((and (>= (length name) 3) (string= (subseq name 0 3) "str"))
        (trampoline:make-ffi-signature :return-type :pointer
                                       :arg-types '(:string :string)))
       ;; Functions starting with 'mem' usually work with memory
       ((and (>= (length name) 3) (string= (subseq name 0 3) "mem"))
        (trampoline:make-ffi-signature :return-type :pointer
                                       :arg-types '(:pointer :pointer :size-t)))
       ;; Functions ending with 'printf' are variadic
       ((and (>= (length name) 6) 
             (string= (subseq name (- (length name) 6)) "printf"))
        (trampoline:make-ffi-signature :return-type :int
                                       :arg-types '(:string &rest)))
       ;; Default: assume returns int with no args
       (t (trampoline:make-ffi-signature :return-type :int
                                         :arg-types '())))))

;;; Array handling

(defmacro with-pinned-array ((var array) &body body)
  "Pin a Lisp array and get its pointer"
  `(sb-sys:with-pinned-objects (,array)
			       (let ((,var (sb-sys:vector-sap ,array)))
				 ,@body)))

(defmacro with-string-array ((var strings) &body body)
  "Convert a list of strings to char** for C"
  (let ((string-ptrs (gensym "STRING-PTRS"))
        (sap-array (gensym "SAP-ARRAY"))
        (count (gensym "COUNT")))
    `(let* ((,count (length ,strings))
            (,string-ptrs (make-array (1+ ,count)))
            (,sap-array (make-array (* (1+ ,count) ,sb-vm:n-word-bytes) :element-type '(unsigned-byte 8))))
       ;; Convert each string to alien string and store SAP
       (loop for i from 0
             for string in ,strings
             do (let ((alien-str (sb-alien:make-alien-string string)))
                  (setf (aref ,string-ptrs i) alien-str)
                  ;; Store the SAP address in the sap-array
                  (setf (sb-sys:sap-ref-sap 
                         (sb-sys:vector-sap ,sap-array)
                         (* i ,sb-vm:n-word-bytes))
                        (sb-alien:alien-sap alien-str))))
       ;; Add NULL terminator
       (setf (sb-sys:sap-ref-sap 
              (sb-sys:vector-sap ,sap-array)
              (* ,count ,sb-vm:n-word-bytes))
             (sb-sys:int-sap 0))
       ;; Use the sap-array
       (let ((,var (sb-sys:vector-sap ,sap-array)))
         (unwind-protect
             (progn ,@body)
           ;; Clean up alien strings
           (loop for i from 0 below ,count
                 do (sb-alien:free-alien (aref ,string-ptrs i))))))))

(defmacro with-output-array ((var count type) &body body)
  "Create an array for output parameters"
  (let ((array (gensym "ARRAY")))
    `(let ((,array (make-array ,count 
                               :element-type ',(case type
                                                    (:int '(signed-byte 32))
                                                    (:uint '(unsigned-byte 32))
                                                    (:long '(signed-byte 64))
                                                    (:ulong '(unsigned-byte 64))
                                                    (t '(unsigned-byte 8))))))
       (sb-sys:with-pinned-objects (,array)
         (let ((,var (sb-sys:vector-sap ,array)))
           ,@body))
       ;; After the body, extract values from the array
       (loop for i from 0 below ,count
             collect (ecase ,type
                           (:int (sb-sys:signed-sap-ref-32 
                                  (sb-sys:vector-sap ,array) (* i 4)))
                           (:uint (sb-sys:sap-ref-32 
                                   (sb-sys:vector-sap ,array) (* i 4)))
                           (:long (sb-sys:signed-sap-ref-64 
                                   (sb-sys:vector-sap ,array) (* i 8)))
                           (:ulong (sb-sys:sap-ref-64 
                                    (sb-sys:vector-sap ,array) (* i 8)))
                           (:pointer (sb-sys:sap-ref-sap 
                                      (sb-sys:vector-sap ,array) (* i 8))))))))

;;; Enum support

(defvar *enum-definitions* map:+empty+
  "Registry of enum definitions")

(defun define-enum (name mappings)
  "Define a C enum type with keyword mappings"
  (setf *enum-definitions* (map:assoc *enum-definitions* name
                                      (map:from-pairs mappings)))
  ;; Register converters - create closures that capture the enum name
  (let ((enum-name name))
    (trampoline:register-c-type name
                                :base :int
                                :size 4
                                :alignment 4
                                :converter-to (lambda (value type)
						(declare (ignore type))
						(enum-value enum-name value))
                                :converter-from (lambda (value type)
                                                  (declare (ignore type))
                                                  (enum-keyword enum-name value)))))

(defun enum-value (enum-name keyword)
  "Get the integer value for an enum keyword"
  (let ((enum-map (map:get *enum-definitions* enum-name)))
    (or (map:get enum-map keyword)
        (error "Unknown enum value ~A for enum ~A" keyword enum-name))))

(defun enum-keyword (enum-name value)
  "Get the keyword for an enum integer value"
  (let ((enum-map (map:get *enum-definitions* enum-name)))
    (block found
      (map:each (lambda (k v)
                  (when (= v value)
                    (return-from found k)))
                enum-map)
      value))) ; Return value if no keyword found


;;; Enhanced convert-to-foreign for arrays and buffers

(defmethod trampoline:convert-to-foreign ((value vector) (type (eql :buffer)))
  "Convert byte vector to pointer"
  (sb-sys:with-pinned-objects (value)
			      (sb-sys:vector-sap value)))

(defmethod trampoline:convert-to-foreign ((value vector) (type (eql :pointer)))
  "Convert any vector to pointer"
  (sb-sys:with-pinned-objects (value)
			      (sb-sys:vector-sap value)))

;;; Error handling

(define-condition foreign-error (error)
  ((code :initarg :code :reader foreign-error-code)
   (function :initarg :function :reader foreign-error-function))
  (:report (lambda (condition stream)
             (format stream "Foreign function ~A failed with error code ~A"
                     (foreign-error-function condition)
                     (foreign-error-code condition)))))

;;; Bool type support

(defun bool-to-foreign (value &optional type)
  "Convert Lisp boolean to C bool (0 or 1)"
  (declare (ignore type))
  (if value 1 0))

(defun foreign-to-bool (value &optional type)
  "Convert C bool to Lisp boolean"
  (declare (ignore type))
  (not (zerop value)))

;; Export these for use in tests
(export 'bool-to-foreign)
(export 'foreign-to-bool)

(defun define-c-type (name size &key converter-to converter-from)
  "Define a new C type"
  (trampoline:register-c-type name
			      :base name
			      :size size
			      :alignment size
			      :converter-to converter-to
			      :converter-from converter-from))

;;; Smart defshared macros

#|
;; REMOVED: defshared-auto - use defshared directly
(defmacro defshared-auto (name c-name library &rest options)
  "Define a foreign function with automatic type marshalling"
  (declare (ignore options))
  ;; Create a function that looks up the signature at runtime
  `(defun ,name (&rest args)
     ,(format nil "Auto-marshalled function ~A" c-name)
     ;; Try to ensure signatures are initialized
     (ensure-signatures-initialized)
     ;; Look up or extract signature
     (let ((signature (or (map:get *known-signatures* ,c-name)
                          (extract-and-register-signature ,c-name ,library))))
       (if signature
           (let ((return-type (trampoline:ffi-signature-return-type signature))
                 (arg-types (trampoline:ffi-signature-arg-types signature)))
             ;; Handle variadic functions
             (if (member '&rest arg-types)
                 ;; For variadic functions, pass all args
                 (apply (find-symbol "SHARED-CALL" "EPSILON.FOREIGN")
                        (list ,c-name ,library)
                        return-type
                        (remove '&rest arg-types)
                        args)
               ;; Non-variadic - check arg count and call
               (if (= (length args) (length arg-types))
                   (apply (find-symbol "SHARED-CALL" "EPSILON.FOREIGN")
                          (list ,c-name ,library)
                          return-type
                          arg-types
                          args)
                 (error "Wrong number of arguments for ~A: expected ~D, got ~D"
                        ,c-name (length arg-types) (length args)))))
         (error "Could not determine signature for function ~A" ,c-name)))))
|#

