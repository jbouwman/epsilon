;;;; signatures.lisp - C Function Signature Discovery
;;;;
;;;; Provides automatic signature discovery from C headers using libclang,
;;;; enabling Julia-style "just call the function" FFI.

(defpackage epsilon.foreign.signatures
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit)
   (lc epsilon.foreign.libclang)
   (struct epsilon.foreign.jit.struct)
   (var epsilon.foreign.jit.variadic)
   (fs epsilon.file))
  (:export
   ;; Automatic JIT from headers
   #:auto-jit
   #:defauto-jit
   #:make-auto-jit-caller

   ;; Signature discovery (authoritative API)
   #:discover-signature
   #:discover-signature-auto
   #:discover-signature-from-headers
   #:signature-to-jit-types
   #:function-variadic-p

   ;; Function-to-header mapping
   #:*function-header-map*
   #:*standard-headers*
   #:detect-function-headers

   ;; Variadic support
   #:make-auto-variadic-caller
   #:defauto-variadic

   ;; Struct discovery
   #:discover-struct
   #:discover-struct-from-headers
   #:defstruct-from-header

   ;; Header management
   #:*sdk-include-path*
   #:*default-include-paths*
   #:add-include-path
   #:find-header

   ;; Cache management
   #:*signature-cache*
   #:*struct-cache*
   #:clear-signature-cache
   #:clear-struct-cache
   #:clear-all-caches

   ;; Utilities
   #:describe-function
   #:describe-struct
   #:list-functions
   #:list-structs)
  (:enter t))

;;; ============================================================================
;;; Include Path Management
;;; ============================================================================

(defvar *sdk-include-path*
  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
  "Path to macOS SDK headers")

(defvar *default-include-paths*
  (list *sdk-include-path*
        "/usr/local/include"
        "/opt/homebrew/include")
  "Default include paths for header search")

(defun add-include-path (path)
  "Add a path to the include search paths"
  (pushnew path *default-include-paths* :test #'string=))

(defun find-header (header-name &optional additional-paths)
  "Find a header file in the include paths.
   Returns the full path or NIL if not found."
  (let ((paths (append additional-paths *default-include-paths*)))
    (loop for dir in paths
          for full-path = (fs:join-paths dir header-name)
          when (probe-file full-path)
            return full-path)))

;;; ============================================================================
;;; Signature Cache
;;; ============================================================================

(defvar *signature-cache* (make-hash-table :test 'equal)
  "Cache of discovered function signatures")

(defun clear-signature-cache ()
  "Clear the signature cache"
  (clrhash *signature-cache*))

(defun cache-signature (function-name header signature)
  "Cache a discovered signature"
  (setf (gethash (list function-name header) *signature-cache*) signature))

(defun get-cached-signature (function-name header)
  "Get a cached signature, or NIL if not cached"
  (gethash (list function-name header) *signature-cache*))

;;; ============================================================================
;;; Type Conversion
;;; ============================================================================

(defun libclang-type-to-jit-type (type-kind)
  "Convert libclang type kind keyword to JIT type keyword."
  (case type-kind
    (:void :void)
    (:bool :int)  ; C99 _Bool
    (:char :char)
    (:signed-char :char)
    (:unsigned-char :uchar)
    (:short :short)
    (:unsigned-short :ushort)
    (:int :int)
    (:unsigned-int :uint)
    (:long :long)
    (:unsigned-long :ulong)
    (:long-long :long)  ; Treat as long on 64-bit
    (:unsigned-long-long :ulong)
    (:float :float)
    (:double :double)
    (:long-double :double)  ; Approximate as double
    (:pointer :pointer)
    (:record :pointer)  ; Struct/union - pass as pointer
    (:enum :int)
    (otherwise :pointer)))  ; Default unknown types to pointer

(defun signature-to-jit-types (signature)
  "Convert a libclang signature to JIT types.
   Returns (values return-type arg-types)."
  (when signature
    (let* ((return-kind (getf signature :return-type-kind))
           (params (getf signature :params))
           (return-type (libclang-type-to-jit-type return-kind))
           (arg-types (mapcar (lambda (p)
                                (libclang-type-to-jit-type (getf p :type-kind)))
                              params)))
      (values return-type arg-types))))

;;; ============================================================================
;;; Signature Discovery
;;; ============================================================================

(defun discover-signature (function-name header-path)
  "Discover a function's signature from a header file using libclang.
   Returns the signature plist or NIL if not found."
  (unless (lc:libclang-available-p)
    (error "libclang not available"))

  ;; Check cache first
  (let ((cached (get-cached-signature function-name header-path)))
    (when cached
      (return-from discover-signature cached)))

  ;; Parse header and extract signature
  (lc:with-parsed-header (tu header-path)
    (let ((signature (lc:extract-function-info tu function-name)))
      (when signature
        (cache-signature function-name header-path signature))
      signature)))

(defun discover-signature-from-headers (function-name headers)
  "Try to discover a function signature from a list of headers.
   Returns (values signature header-path) or NIL."
  (dolist (header headers)
    (let ((full-path (if (char= (char header 0) #\/)
                         header
                         (find-header header))))
      (when full-path
        (let ((sig (discover-signature function-name full-path)))
          (when sig
            (return-from discover-signature-from-headers
              (values sig full-path)))))))
  nil)

;;; ============================================================================
;;; Auto-JIT Function Creation
;;; ============================================================================

(defun make-auto-jit-caller (function-name header-or-headers)
  "Create a JIT-compiled caller for a function, automatically discovering
   the signature from headers.

   FUNCTION-NAME - String name of the C function
   HEADER-OR-HEADERS - A header path or list of header paths to search

   Returns a callable function."
  (let* ((headers (if (listp header-or-headers)
                      header-or-headers
                      (list header-or-headers)))
         (signature (discover-signature-from-headers function-name headers)))
    (unless signature
      (error "Could not find signature for ~A in headers: ~A"
             function-name headers))

    (multiple-value-bind (return-type arg-types)
        (signature-to-jit-types signature)
      (let ((addr (sb-sys:find-foreign-symbol-address function-name)))
        (unless addr
          (error "Foreign symbol not found: ~A" function-name))
        (let ((addr-int (etypecase addr
                          (integer addr)
                          (sb-sys:system-area-pointer (sb-sys:sap-int addr)))))
          (jit:make-jit-caller addr-int return-type arg-types))))))

(defmacro auto-jit (function-name header-or-headers)
  "Create a JIT-compiled caller from header signature at load time.

   Example:
   (funcall (auto-jit \"sin\" \"math.h\") 1.0d0)

   Returns a compiled function that can be called with the appropriate args."
  `(make-auto-jit-caller ,function-name ,header-or-headers))

(defmacro defauto-jit (lisp-name c-name header-or-headers &key documentation)
  "Define a JIT-compiled function with automatic signature discovery.

   LISP-NAME - Symbol naming the Lisp function
   C-NAME - String name of the C function
   HEADER-OR-HEADERS - Header path(s) to search for the signature

   Example:
   (defauto-jit lisp-sin \"sin\" \"math.h\"
     :documentation \"Compute sine of a double\")

   The function signature is discovered from the header at load time,
   and the JIT caller is created lazily on first call."
  (let ((caller-var (gensym "CALLER")))
    `(let ((,caller-var nil))
       (defun ,lisp-name (&rest args)
         ,@(when documentation (list documentation))
         (unless ,caller-var
           (setf ,caller-var (make-auto-jit-caller ,c-name ,header-or-headers)))
         (apply ,caller-var args)))))

;;; ============================================================================
;;; Standard Library Headers
;;; ============================================================================

(defparameter *standard-headers*
  '(;; C standard library
    "stdlib.h" "stdio.h" "string.h" "math.h" "time.h" "ctype.h"
    ;; POSIX
    "unistd.h" "fcntl.h" "sys/stat.h" "sys/types.h"
    ;; Network
    "sys/socket.h" "netinet/in.h" "arpa/inet.h")
  "List of standard headers to search")

;;; ============================================================================
;;; Function-to-Header Mapping (for automatic header discovery)
;;; ============================================================================
;;;
;;; This mapping enables automatic discovery of headers for common C functions.
;;; When a function name is provided without a header, this table provides
;;; likely header candidates to search.

(defparameter *function-header-map*
  '(;; String functions
    ("strlen" . ("string.h"))
    ("strcpy" . ("string.h"))
    ("strncpy" . ("string.h"))
    ("strcmp" . ("string.h"))
    ("strncmp" . ("string.h"))
    ("strcat" . ("string.h"))
    ("strncat" . ("string.h"))
    ("memcpy" . ("string.h"))
    ("memmove" . ("string.h"))
    ("memset" . ("string.h"))
    ("memcmp" . ("string.h"))
    ("strchr" . ("string.h"))
    ("strrchr" . ("string.h"))
    ("strstr" . ("string.h"))
    ("strtok" . ("string.h"))

    ;; Memory management
    ("malloc" . ("stdlib.h"))
    ("calloc" . ("stdlib.h"))
    ("realloc" . ("stdlib.h"))
    ("free" . ("stdlib.h"))
    ("aligned_alloc" . ("stdlib.h"))

    ;; I/O functions
    ("printf" . ("stdio.h"))
    ("fprintf" . ("stdio.h"))
    ("sprintf" . ("stdio.h"))
    ("snprintf" . ("stdio.h"))
    ("scanf" . ("stdio.h"))
    ("fscanf" . ("stdio.h"))
    ("sscanf" . ("stdio.h"))
    ("fopen" . ("stdio.h"))
    ("fclose" . ("stdio.h"))
    ("fread" . ("stdio.h"))
    ("fwrite" . ("stdio.h"))
    ("fgets" . ("stdio.h"))
    ("fputs" . ("stdio.h"))
    ("fgetc" . ("stdio.h"))
    ("fputc" . ("stdio.h"))
    ("fseek" . ("stdio.h"))
    ("ftell" . ("stdio.h"))
    ("rewind" . ("stdio.h"))
    ("fflush" . ("stdio.h"))
    ("feof" . ("stdio.h"))
    ("ferror" . ("stdio.h"))
    ("clearerr" . ("stdio.h"))
    ("perror" . ("stdio.h"))
    ("remove" . ("stdio.h"))
    ("rename" . ("stdio.h"))
    ("tmpfile" . ("stdio.h"))
    ("tmpnam" . ("stdio.h"))

    ;; System calls (POSIX)
    ("open" . ("fcntl.h" "sys/stat.h" "sys/types.h"))
    ("close" . ("unistd.h"))
    ("read" . ("unistd.h"))
    ("write" . ("unistd.h"))
    ("lseek" . ("unistd.h"))
    ("getpid" . ("unistd.h"))
    ("getppid" . ("unistd.h"))
    ("getuid" . ("unistd.h"))
    ("geteuid" . ("unistd.h"))
    ("getgid" . ("unistd.h"))
    ("getegid" . ("unistd.h"))
    ("fork" . ("unistd.h"))
    ("execve" . ("unistd.h"))
    ("pipe" . ("unistd.h"))
    ("dup" . ("unistd.h"))
    ("dup2" . ("unistd.h"))
    ("chdir" . ("unistd.h"))
    ("getcwd" . ("unistd.h"))
    ("sleep" . ("unistd.h"))
    ("usleep" . ("unistd.h"))
    ("access" . ("unistd.h"))
    ("unlink" . ("unistd.h"))
    ("rmdir" . ("unistd.h"))
    ("link" . ("unistd.h"))
    ("symlink" . ("unistd.h"))
    ("readlink" . ("unistd.h"))

    ;; File status
    ("stat" . ("sys/stat.h"))
    ("fstat" . ("sys/stat.h"))
    ("lstat" . ("sys/stat.h"))
    ("chmod" . ("sys/stat.h"))
    ("mkdir" . ("sys/stat.h"))

    ;; Math functions
    ("sin" . ("math.h"))
    ("cos" . ("math.h"))
    ("tan" . ("math.h"))
    ("asin" . ("math.h"))
    ("acos" . ("math.h"))
    ("atan" . ("math.h"))
    ("atan2" . ("math.h"))
    ("sinh" . ("math.h"))
    ("cosh" . ("math.h"))
    ("tanh" . ("math.h"))
    ("sqrt" . ("math.h"))
    ("cbrt" . ("math.h"))
    ("pow" . ("math.h"))
    ("exp" . ("math.h"))
    ("exp2" . ("math.h"))
    ("log" . ("math.h"))
    ("log2" . ("math.h"))
    ("log10" . ("math.h"))
    ("ceil" . ("math.h"))
    ("floor" . ("math.h"))
    ("trunc" . ("math.h"))
    ("round" . ("math.h"))
    ("fabs" . ("math.h"))
    ("fmod" . ("math.h"))
    ("remainder" . ("math.h"))
    ("copysign" . ("math.h"))
    ("fmax" . ("math.h"))
    ("fmin" . ("math.h"))
    ("hypot" . ("math.h"))
    ("ldexp" . ("math.h"))
    ("frexp" . ("math.h"))
    ("modf" . ("math.h"))

    ;; Time functions
    ("time" . ("time.h"))
    ("clock" . ("time.h"))
    ("difftime" . ("time.h"))
    ("mktime" . ("time.h"))
    ("gmtime" . ("time.h"))
    ("gmtime_r" . ("time.h"))
    ("localtime" . ("time.h"))
    ("localtime_r" . ("time.h"))
    ("strftime" . ("time.h"))
    ("strptime" . ("time.h"))
    ("clock_gettime" . ("time.h"))
    ("clock_getres" . ("time.h"))
    ("nanosleep" . ("time.h"))

    ;; Network functions (POSIX)
    ("socket" . ("sys/socket.h"))
    ("bind" . ("sys/socket.h"))
    ("listen" . ("sys/socket.h"))
    ("accept" . ("sys/socket.h"))
    ("connect" . ("sys/socket.h"))
    ("send" . ("sys/socket.h"))
    ("recv" . ("sys/socket.h"))
    ("sendto" . ("sys/socket.h"))
    ("recvfrom" . ("sys/socket.h"))
    ("setsockopt" . ("sys/socket.h"))
    ("getsockopt" . ("sys/socket.h"))
    ("shutdown" . ("sys/socket.h"))
    ("getaddrinfo" . ("netdb.h"))
    ("freeaddrinfo" . ("netdb.h"))
    ("getnameinfo" . ("netdb.h"))
    ("inet_pton" . ("arpa/inet.h"))
    ("inet_ntop" . ("arpa/inet.h"))
    ("htons" . ("arpa/inet.h"))
    ("ntohs" . ("arpa/inet.h"))
    ("htonl" . ("arpa/inet.h"))
    ("ntohl" . ("arpa/inet.h"))

    ;; Character classification (ctype.h)
    ("isalpha" . ("ctype.h"))
    ("isdigit" . ("ctype.h"))
    ("isalnum" . ("ctype.h"))
    ("isspace" . ("ctype.h"))
    ("isupper" . ("ctype.h"))
    ("islower" . ("ctype.h"))
    ("toupper" . ("ctype.h"))
    ("tolower" . ("ctype.h"))

    ;; Stdlib conversions
    ("atoi" . ("stdlib.h"))
    ("atol" . ("stdlib.h"))
    ("atof" . ("stdlib.h"))
    ("strtol" . ("stdlib.h"))
    ("strtoul" . ("stdlib.h"))
    ("strtod" . ("stdlib.h"))
    ("strtof" . ("stdlib.h"))
    ("abs" . ("stdlib.h"))
    ("labs" . ("stdlib.h"))
    ("rand" . ("stdlib.h"))
    ("srand" . ("stdlib.h"))
    ("qsort" . ("stdlib.h"))
    ("bsearch" . ("stdlib.h"))
    ("getenv" . ("stdlib.h"))
    ("setenv" . ("stdlib.h"))
    ("system" . ("stdlib.h"))
    ("exit" . ("stdlib.h"))
    ("abort" . ("stdlib.h"))
    ("atexit" . ("stdlib.h"))

    ;; Error handling
    ("strerror" . ("string.h"))
    ("errno" . ("errno.h")))
  "Mapping of C function names to their likely headers.
   Used for automatic header discovery when calling C functions.")

(defun detect-function-headers (function-name)
  "Detect likely headers for a C function by name.
   Returns a list of header file names to search.

   This function provides automatic header discovery for common C functions,
   enabling Julia-style 'just call the function' FFI usage."
  (or (cdr (assoc function-name *function-header-map* :test #'string=))
      ;; Default headers for unknown functions
      *standard-headers*))

(defun discover-in-standard-headers (function-name)
  "Try to discover a function signature in standard headers.
   Returns (values signature header-path) or NIL."
  (let ((full-headers (mapcar (lambda (h)
                                (or (find-header h) h))
                              *standard-headers*)))
    (discover-signature-from-headers function-name
                                      (remove nil full-headers))))

(defun discover-signature-auto (function-name)
  "Automatically discover a function signature by trying known header mappings.
   This is the primary entry point for Julia-style 'just call the function' FFI.

   FUNCTION-NAME - String name of the C function

   Returns (values signature header-path) or NIL if not found.

   This function first checks the *function-header-map* for known function-to-header
   mappings, then falls back to searching all standard headers."
  (let ((known-headers (detect-function-headers function-name)))
    (or (discover-signature-from-headers function-name known-headers)
        ;; Fall back to searching all standard headers if known mapping failed
        (unless (equal known-headers *standard-headers*)
          (discover-in-standard-headers function-name)))))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun describe-function (function-name header-path)
  "Describe a C function's signature from its header."
  (let ((sig (discover-signature function-name header-path)))
    (if sig
        (format t "~%Function: ~A~%  Returns: ~A (~A)~%  Parameters:~%"
                (getf sig :name)
                (getf sig :return-type)
                (getf sig :return-type-kind))
        (format t "~%Function ~A not found in ~A~%" function-name header-path))
    (dolist (param (getf sig :params))
      (format t "    ~A (~A)~%" (getf param :type) (getf param :type-kind)))
    sig))

(defun list-functions (header-path &key (limit 20))
  "List functions declared in a header file."
  (unless (lc:libclang-available-p)
    (error "libclang not available"))
  (lc:with-parsed-header (tu header-path)
    (let ((decls (lc:extract-all-declarations tu))
          (count 0))
      (dolist (decl decls)
        (when (eq (getf decl :kind) :function)
          (format t "  ~A~%" (getf decl :name))
          (incf count)
          (when (and limit (>= count limit))
            (format t "  ... (showing first ~D)~%" limit)
            (return))))
      count)))

;;; ============================================================================
;;; Struct Cache
;;; ============================================================================

(defvar *struct-cache* (make-hash-table :test 'equal)
  "Cache of discovered struct layouts")

(defun clear-struct-cache ()
  "Clear the struct cache"
  (clrhash *struct-cache*))

(defun clear-all-caches ()
  "Clear all FFI caches (signatures and structs).
   Call this to force re-parsing of headers."
  (clear-signature-cache)
  (clear-struct-cache)
  (values))

(defun cache-struct (struct-name header struct-info)
  "Cache a discovered struct"
  (setf (gethash (list struct-name header) *struct-cache*) struct-info))

(defun get-cached-struct (struct-name header)
  "Get a cached struct, or NIL if not cached"
  (gethash (list struct-name header) *struct-cache*))

;;; ============================================================================
;;; Struct Type Conversion
;;; ============================================================================

(defun libclang-type-kind-to-jit-type (type-kind)
  "Convert libclang type kind keyword to JIT struct field type."
  (case type-kind
    (:char :char)
    (:signed-char :char)
    (:unsigned-char :uchar)
    (:short :short)
    (:unsigned-short :ushort)
    (:int :int)
    (:unsigned-int :uint)
    (:long :long)
    (:unsigned-long :ulong)
    (:long-long :long)
    (:unsigned-long-long :ulong)
    (:float :float)
    (:double :double)
    (:pointer :pointer)
    (otherwise :pointer)))

;;; ============================================================================
;;; Struct Discovery
;;; ============================================================================

(defun discover-struct (struct-name header-path)
  "Discover a struct's layout from a header file using libclang.
   Returns the struct info plist or NIL if not found.

   The returned plist has keys:
   :name - Struct name
   :size - Size in bytes
   :alignment - Alignment in bytes
   :fields - List of field plists with :name, :type, :offset, :size"
  (unless (lc:libclang-available-p)
    (error "libclang not available"))

  ;; Check cache first
  (let ((cached (get-cached-struct struct-name header-path)))
    (when cached
      (return-from discover-struct cached)))

  ;; Parse header and extract struct info
  (lc:with-parsed-header (tu header-path)
    (let ((struct-info (lc:extract-struct-info tu struct-name)))
      (when struct-info
        (cache-struct struct-name header-path struct-info))
      struct-info)))

(defun discover-struct-from-headers (struct-name headers)
  "Try to discover a struct from a list of headers.
   Returns (values struct-info header-path) or NIL."
  (dolist (header headers)
    (let ((full-path (if (char= (char header 0) #\/)
                         header
                         (find-header header))))
      (when full-path
        (let ((info (discover-struct struct-name full-path)))
          (when info
            (return-from discover-struct-from-headers
              (values info full-path)))))))
  nil)

;;; ============================================================================
;;; Struct Definition from Header
;;; ============================================================================

(defun struct-info-to-jit-struct (struct-info lisp-name)
  "Convert libclang struct info to JIT struct definition.
   Returns a form suitable for evaluation."
  (let ((fields (getf struct-info :fields)))
    `(struct:define-jit-struct ,lisp-name
       ,@(mapcar (lambda (field)
                   (list (intern (string-upcase (getf field :name)))
                         (libclang-type-kind-to-jit-type (getf field :type-kind))))
                 fields))))

(defmacro defstruct-from-header (lisp-name c-name header-or-headers)
  "Define a JIT struct from a C struct discovered in a header.

   LISP-NAME - Symbol naming the Lisp struct
   C-NAME - String name of the C struct
   HEADER-OR-HEADERS - Header path(s) to search

   Example:
   (defstruct-from-header timespec \"timespec\" \"time.h\")

   After this, you can use:
   (struct:jit-struct-size 'timespec)
   (struct:jit-struct-fields 'timespec)"
  (let ((headers (if (listp header-or-headers)
                     header-or-headers
                     (list header-or-headers))))
    `(progn
       (let ((struct-info (discover-struct-from-headers ,c-name ',headers)))
         (unless struct-info
           (error "Could not find struct ~A in headers: ~A" ,c-name ',headers))
         ;; Generate and evaluate the struct definition
         (eval (struct-info-to-jit-struct struct-info ',lisp-name)))
       ',lisp-name)))

;;; ============================================================================
;;; Struct Utilities
;;; ============================================================================

(defun describe-struct (struct-name header-path)
  "Describe a C struct's layout from its header."
  (let ((info (discover-struct struct-name header-path)))
    (if info
        (progn
          (format t "~%Struct: ~A~%" (getf info :name))
          (format t "  Size: ~D bytes~%" (getf info :size))
          (format t "  Alignment: ~D bytes~%" (getf info :alignment))
          (format t "  Fields:~%")
          (dolist (field (getf info :fields))
            (format t "    ~A: ~A at offset ~D (~D bytes)~%"
                    (getf field :name)
                    (getf field :type)
                    (floor (getf field :offset) 8)  ; offset is in bits
                    (getf field :size))))
        (format t "~%Struct ~A not found in ~A~%" struct-name header-path))
    info))

(defun list-structs (header-path &key (limit 20))
  "List structs declared in a header file."
  (unless (lc:libclang-available-p)
    (error "libclang not available"))
  (lc:with-parsed-header (tu header-path)
    (let ((decls (lc:extract-all-declarations tu))
          (count 0))
      (dolist (decl decls)
        (when (eq (getf decl :kind) :struct)
          (format t "  ~A~%" (getf decl :name))
          (incf count)
          (when (and limit (>= count limit))
            (format t "  ... (showing first ~D)~%" limit)
            (return))))
      count)))

;;; ============================================================================
;;; Variadic Function Support
;;; ============================================================================

(defun function-variadic-p (signature)
  "Check if a function signature indicates a variadic function.
   Returns T if the function is variadic."
  (when signature
    (getf signature :variadic)))

(defun extract-fixed-arg-types (signature)
  "Extract the fixed (non-variadic) argument types from a signature."
  (multiple-value-bind (return-type arg-types)
      (signature-to-jit-types signature)
    (declare (ignore return-type))
    ;; All declared params are fixed args for variadic functions
    arg-types))

(defun make-auto-variadic-caller (function-name header-or-headers)
  "Create a variadic caller for a function, discovering the fixed arg types
   from headers.

   FUNCTION-NAME - String name of the C function
   HEADER-OR-HEADERS - Header path(s) to search

   Returns a function that accepts fixed args followed by variadic args.

   Example:
     (let ((printf (make-auto-variadic-caller \"printf\" \"stdio.h\")))
       (funcall printf \"%d + %d = %d\" 1 2 3))"
  (let* ((headers (if (listp header-or-headers)
                      header-or-headers
                      (list header-or-headers)))
         (signature (discover-signature-from-headers function-name headers)))
    (unless signature
      (error "Could not find signature for ~A in headers: ~A"
             function-name headers))

    ;; Verify it's actually variadic
    (unless (function-variadic-p signature)
      (warn "Function ~A does not appear to be variadic in headers" function-name))

    (multiple-value-bind (return-type arg-types)
        (signature-to-jit-types signature)
      (let ((addr (sb-sys:find-foreign-symbol-address function-name)))
        (unless addr
          (error "Foreign symbol not found: ~A" function-name))
        (var:make-variadic-caller addr return-type arg-types)))))

(defmacro defauto-variadic (lisp-name c-name header-or-headers &key documentation)
  "Define a variadic function with automatic fixed-arg type discovery.

   LISP-NAME - Symbol naming the Lisp function
   C-NAME - String name of the C function
   HEADER-OR-HEADERS - Header path(s) to search for the signature

   Example:
     (defauto-variadic lisp-printf \"printf\" \"stdio.h\"
       :documentation \"Print formatted output to stdout\")

     (lisp-printf \"%d + %d = %d~%\" 1 2 3)"
  (let ((caller-var (gensym "CALLER")))
    `(let ((,caller-var nil))
       (defun ,lisp-name (&rest args)
         ,@(when documentation (list documentation))
         (unless ,caller-var
           (setf ,caller-var (make-auto-variadic-caller ,c-name ,header-or-headers)))
         (apply ,caller-var args)))))
