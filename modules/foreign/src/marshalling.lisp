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
   #:defshared-smart
   
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

(defvar *known-signatures* (make-hash-table :test 'equal)
  "Database of known C function signatures")

(defun register-known-signature (name return-type arg-types)
  "Register a known C function signature"
  (setf (gethash name *known-signatures*)
        (trampoline:make-ffi-signature :return-type return-type
                                       :arg-types arg-types)))

;; Register common C library functions
(register-known-signature "strlen" :size-t '(:string))
(register-known-signature "strcpy" :pointer '(:pointer :string))
(register-known-signature "strncpy" :pointer '(:pointer :string :size-t))
(register-known-signature "strcmp" :int '(:string :string))
(register-known-signature "strncmp" :int '(:string :string :size-t))
(register-known-signature "strcat" :pointer '(:pointer :string))
(register-known-signature "strncat" :pointer '(:pointer :string :size-t))
(register-known-signature "strchr" :pointer '(:string :int))
(register-known-signature "strrchr" :pointer '(:string :int))
(register-known-signature "strstr" :pointer '(:string :string))

(register-known-signature "malloc" :pointer '(:size-t))
(register-known-signature "calloc" :pointer '(:size-t :size-t))
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
  (or (gethash name *known-signatures*)
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
        (count (gensym "COUNT")))
    `(let* ((,count (length ,strings))
            (,string-ptrs (make-array (1+ ,count))))
       ;; Convert each string
       (loop for i from 0
             for string in ,strings
             do (setf (aref ,string-ptrs i)
                     (sb-alien:make-alien-string string)))
       ;; Add NULL terminator
       (setf (aref ,string-ptrs ,count) (sb-sys:int-sap 0))
       ;; Pin the array of pointers
       (sb-sys:with-pinned-objects (,string-ptrs)
         (let ((,var (sb-sys:vector-sap ,string-ptrs)))
           (unwind-protect
                (progn ,@body)
             ;; Clean up alien strings
             (loop for i from 0 below ,count
                   do (sb-alien:free-alien (aref ,string-ptrs i)))))))))

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
           ,@body
           ;; Return the filled array as a list
           (loop for i from 0 below ,count
                 collect (ecase ,type
                          (:int (sb-sys:signed-sap-ref-32 ,var (* i 4)))
                          (:uint (sb-sys:sap-ref-32 ,var (* i 4)))
                          (:long (sb-sys:signed-sap-ref-64 ,var (* i 8)))
                          (:ulong (sb-sys:sap-ref-64 ,var (* i 8)))
                          (:pointer (sb-sys:sap-ref-sap ,var (* i 8))))))))))

;;; Enum support

(defvar *enum-definitions* (make-hash-table :test 'eq)
  "Registry of enum definitions")

(defun define-enum (name mappings)
  "Define a C enum type with keyword mappings"
  (setf (gethash name *enum-definitions*) 
        (map:from-pairs mappings))
  ;; Register converters
  (trampoline:register-c-type name
                              :base :int
                              :size 4
                              :alignment 4
                              :converter-to (lambda (value type)
                                             (declare (ignore type))
                                             (enum-value name value))
                              :converter-from (lambda (value type)
                                               (declare (ignore type))
                                               (enum-keyword name value))))

(defun enum-value (enum-name keyword)
  "Get the integer value for an enum keyword"
  (let ((enum-map (gethash enum-name *enum-definitions*)))
    (or (map:get enum-map keyword)
        (error "Unknown enum value ~A for enum ~A" keyword enum-name))))

(defun enum-keyword (enum-name value)
  "Get the keyword for an enum integer value"
  (let ((enum-map (gethash enum-name *enum-definitions*)))
    (block found
      (map:each enum-map
                (lambda (k v)
                  (when (= v value)
                    (return-from found k))))
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

(defun bool-to-foreign (value)
  "Convert Lisp boolean to C bool (0 or 1)"
  (if value 1 0))

(defun foreign-to-bool (value)
  "Convert C bool to Lisp boolean"
  (not (zerop value)))

(defun define-c-type (name size &key converter-to converter-from)
  "Define a new C type"
  (trampoline:register-c-type name
                              :base name
                              :size size
                              :alignment size
                              :converter-to converter-to
                              :converter-from converter-from))

;;; Smart defshared macros

(defmacro defshared-auto (name c-name library &rest options)
  "Define a foreign function with automatic type marshalling"
  (declare (ignore options))
  ;; For now, just create a simple wrapper that will be properly expanded
  ;; when the macro is used from a context where epsilon.foreign is loaded
  `(defun ,name (&rest args)
     ,(format nil "Auto-marshalled function ~A" c-name)
     ;; This will be expanded in the context where the macro is used
     (error "defshared-auto not yet fully implemented - placeholder for ~A" ,c-name)))

(defmacro defshared-smart (name c-name &optional (library "libc"))
  "Define a foreign function with fully automatic signature inference"
  `(defshared-auto ,name ,c-name ,library))