;;;; auto-binding.lisp - Library-level auto-binding patterns for FFI
;;;;
;;;; This module provides macros and utilities for automatically generating
;;;; FFI bindings from C headers using libclang. It establishes patterns for
;;;; when to use auto-binding vs manual binding.

(defpackage epsilon.foreign.auto-binding
  (:use cl)
  (:local-nicknames
   (lc epsilon.foreign.libclang)
   (sigs epsilon.foreign.signatures)
   (ffi epsilon.foreign))
  (:export
   ;; Main macro
   #:deflib
   #:define-library-bindings

   ;; Header caching
   #:*header-cache*
   #:*header-cache-enabled*
   #:clear-header-cache
   #:cache-header-info
   #:get-cached-header-info

   ;; Binding generation
   #:generate-bindings-from-header
   #:generate-binding-for-function

   ;; Macro classification and binding
   #:classify-macro
   #:generate-macro-bindings
   #:describe-header-macros
   #:macro-name-to-constant

   ;; Macro constant evaluation (two-pass)
   #:evaluate-macro-constants
   #:evaluate-header-macro-constants
   #:generate-defconstants-from-header

   ;; Utilities
   #:list-header-functions
   #:describe-library-api
   #:binding-decision-matrix)
  (:enter t))

;;; ============================================================================
;;; Binding Decision Criteria
;;; ============================================================================
;;;
;;; When to use AUTO-BINDING:
;;;   - Simple C functions with primitive types (int, long, double, pointer)
;;;   - Functions with straightforward return values
;;;   - Opaque pointer types (handles, contexts)
;;;   - Stable, well-documented APIs
;;;   - Functions that don't require custom memory management
;;;   - Libraries where header parsing is reliable
;;;
;;; When to use MANUAL BINDING:
;;;   - Functions that are actually macros (invisible to libclang)
;;;   - Complex memory ownership patterns requiring custom wrappers
;;;   - Error handling integration with Lisp conditions
;;;   - Functions requiring string marshalling (C string <-> Lisp string)
;;;   - Callbacks with complex signatures
;;;   - Performance-critical hot paths needing optimization
;;;   - Version-specific API differences (e.g., OpenSSL 1.1 vs 3.0)
;;;
;;; Decision Matrix:
;;; | Criteria                      | Auto | Manual |
;;; |-------------------------------|------|--------|
;;; | Simple C functions            | Yes  | -      |
;;; | Macro-based "functions"       | -    | Yes    |
;;; | Opaque pointer types          | Yes  | -      |
;;; | Complex memory ownership      | -    | Yes    |
;;; | Error condition integration   | -    | Yes    |
;;; | String parameter handling     | -    | Yes    |
;;; | Stable API                    | Yes  | -      |
;;; | Rapidly evolving API          | Yes  | -      |
;;; | Hot path performance          | JIT  | Static |

(defun binding-decision-matrix ()
  "Return the binding decision matrix as a list of criteria."
  '((:criteria "Simple C functions"           :auto t   :manual nil)
    (:criteria "Macro-based functions"        :auto nil :manual t)
    (:criteria "Opaque pointer types"         :auto t   :manual nil)
    (:criteria "Complex memory ownership"     :auto nil :manual t)
    (:criteria "Error condition integration"  :auto nil :manual t)
    (:criteria "String parameter handling"    :auto nil :manual t)
    (:criteria "Callback functions"           :auto nil :manual t)
    (:criteria "Stable well-documented API"   :auto t   :manual nil)
    (:criteria "Rapidly evolving API"         :auto t   :manual nil)
    (:criteria "Performance-critical paths"   :auto t   :manual t)))

;;; ============================================================================
;;; Header Cache
;;; ============================================================================
;;;
;;; Header parsing can be slow, especially for large headers like OpenSSL.
;;; The cache stores parsed header information keyed by (path . mtime).

(defvar *header-cache* (make-hash-table :test 'equal)
  "Cache of parsed header information.
   Key: (header-path . file-modification-time)
   Value: plist with :functions :structs :enums :typedefs")

(defvar *header-cache-enabled* t
  "Whether header caching is enabled.")

(defun clear-header-cache ()
  "Clear the header cache."
  (clrhash *header-cache*)
  (values))

(defun header-cache-key (header-path)
  "Generate cache key for a header file."
  (let ((truename (probe-file header-path)))
    (when truename
      (cons (namestring truename)
            (file-write-date truename)))))

(defun cache-header-info (header-path info)
  "Cache parsed header information."
  (when *header-cache-enabled*
    (let ((key (header-cache-key header-path)))
      (when key
        (setf (gethash key *header-cache*) info))))
  info)

(defun get-cached-header-info (header-path)
  "Get cached header information, or NIL if not cached or stale."
  (when *header-cache-enabled*
    (let ((key (header-cache-key header-path)))
      (when key
        (gethash key *header-cache*)))))

;;; ============================================================================
;;; Header Analysis
;;; ============================================================================

(defun parse-header-with-cache (header-path &key include-paths defines)
  "Parse a header file, using cache if available."
  (or (get-cached-header-info header-path)
      (let ((info (parse-header-fresh header-path
                                      :include-paths include-paths
                                      :defines defines)))
        (cache-header-info header-path info)
        info)))

(defun parse-header-fresh (header-path &key include-paths defines)
  "Parse a header file and extract all declarations."
  (unless (lc:libclang-available-p)
    (error "libclang not available for header parsing"))
  (lc:with-parsed-header (tu header-path
                             :include-paths include-paths
                             :defines defines)
    (let ((decls (lc:extract-all-declarations tu))
          (functions nil)
          (structs nil)
          (enums nil)
          (typedefs nil))
      ;; Categorize declarations
      (dolist (decl decls)
        (case (getf decl :kind)
          (:function (push decl functions))
          (:struct (push decl structs))
          (:enum (push decl enums))
          (:typedef (push decl typedefs))))
      (list :header header-path
            :functions (nreverse functions)
            :structs (nreverse structs)
            :enums (nreverse enums)
            :typedefs (nreverse typedefs)
            :parse-time (get-universal-time)))))

(defun list-header-functions (header-path &key include-paths defines prefix)
  "List all functions in a header file.
   If PREFIX is provided, only list functions starting with that prefix."
  (let* ((info (parse-header-with-cache header-path
                                        :include-paths include-paths
                                        :defines defines))
         (functions (getf info :functions)))
    (if prefix
        (remove-if-not (lambda (f)
                         (let ((name (getf f :name)))
                           (and name
                                (>= (length name) (length prefix))
                                (string= prefix name :end2 (length prefix)))))
                       functions)
        functions)))

;;; ============================================================================
;;; Binding Generation
;;; ============================================================================

(defun c-type-to-ffi-type (type-kind)
  "Convert libclang type kind to epsilon.foreign type keyword."
  (case type-kind
    (:void :void)
    (:bool :int)
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
    (:long-double :double)
    (:pointer :pointer)
    (:record :pointer)
    (:enum :int)
    (otherwise :pointer)))

(defun generate-binding-for-function (function-name header-path library
                                       &key lisp-name include-paths defines)
  "Generate a defshared form for a function discovered from header.

   FUNCTION-NAME - C function name (string)
   HEADER-PATH - Path to header file
   LIBRARY - Library keyword or name for defshared
   LISP-NAME - Optional Lisp function name (defaults to kebab-case of C name)

   Returns a form like:
   (ffi:defshared lisp-name \"c_name\" library return-type (args...))"
  (declare (ignore include-paths defines))
  (let ((sig (sigs:discover-signature function-name header-path)))
    (unless sig
      (error "Could not find function ~A in ~A" function-name header-path))
    (let* ((c-name (getf sig :name))
           (return-type-kind (getf sig :return-type-kind))
           (return-type (c-type-to-ffi-type return-type-kind))
           (params (getf sig :params))
           (lisp-fn-name (or lisp-name
                             (c-name-to-lisp-name c-name)))
           (ffi-params (loop for param in params
                             for i from 0
                             collect (list (intern (format nil "ARG~D" i))
                                          (c-type-to-ffi-type
                                           (getf param :type-kind))))))
      `(ffi:defshared ,lisp-fn-name ,c-name ,library ,return-type
         ,@ffi-params))))

(defun c-name-to-lisp-name (c-name)
  "Convert C function name to Lisp symbol.
   PQconnectdb -> pq-connectdb
   SSL_CTX_new -> ssl-ctx-new"
  (intern
   (string-upcase
    (with-output-to-string (out)
      (loop for i from 0 below (length c-name)
            for char = (char c-name i)
            for prev-char = (if (> i 0) (char c-name (1- i)) nil)
            do (cond
                 ;; Underscore becomes hyphen
                 ((char= char #\_)
                  (write-char #\- out))
                 ;; Uppercase after lowercase -> insert hyphen
                 ((and prev-char
                       (upper-case-p char)
                       (lower-case-p prev-char))
                  (write-char #\- out)
                  (write-char char out))
                 ;; Normal character
                 (t
                  (write-char char out))))))))

(defun generate-bindings-from-header (header-path library
                                       &key prefix exclude include-paths defines
                                            (manual-wrappers nil))
  "Generate binding forms for all functions in a header.

   HEADER-PATH - Path to C header file
   LIBRARY - Library keyword or path for FFI
   PREFIX - Only include functions starting with this prefix
   EXCLUDE - List of function names to exclude
   MANUAL-WRAPPERS - List of function names that need manual wrappers

   Returns a list of defshared forms."
  (let* ((info (parse-header-with-cache header-path
                                        :include-paths include-paths
                                        :defines defines))
         (functions (getf info :functions))
         (exclude-set (make-hash-table :test 'equal)))
    ;; Build exclusion set
    (dolist (name exclude)
      (setf (gethash name exclude-set) t))
    (dolist (name manual-wrappers)
      (setf (gethash name exclude-set) t))
    ;; Generate bindings
    (loop for func in functions
          for name = (getf func :name)
          when (and name
                    (or (null prefix)
                        (and (>= (length name) (length prefix))
                             (string= prefix name :end2 (length prefix))))
                    (not (gethash name exclude-set)))
            collect (handler-case
                        (generate-binding-for-function name header-path library
                                                       :include-paths include-paths
                                                       :defines defines)
                      (error (e)
                        ;; Skip functions that can't be bound
                        (warn "Skipping ~A: ~A" name e)
                        nil))
            into bindings
          finally (return (remove nil bindings)))))

;;; ============================================================================
;;; deflib Macro
;;; ============================================================================

(defmacro deflib (library-name &key headers prefix exclude manual include-paths defines
                                    documentation)
  "Define FFI bindings for a C library using automatic header parsing.

   LIBRARY-NAME - Keyword naming the library (e.g., :libpq)
   HEADERS - List of header files to parse
   PREFIX - Only bind functions starting with this prefix
   EXCLUDE - List of function names to exclude from auto-binding
   MANUAL - Alist of (c-name . wrapper-form) for functions needing manual treatment
   INCLUDE-PATHS - Additional include paths for header parsing
   DEFINES - Preprocessor defines for header parsing
   DOCUMENTATION - Documentation string for the library

   Example:
   (deflib :libpq
     :headers (\"/usr/include/libpq-fe.h\")
     :prefix \"PQ\"
     :exclude (\"PQprint\" \"PQdisplayTuples\")  ; deprecated
     :manual ((\"PQconnectdb\" . (defun pq-connectdb (conninfo)
                                   \"Connect with string conversion\"
                                   ...)))
     :documentation \"PostgreSQL client library bindings\")"
  (let ((header-list (if (listp headers) headers (list headers)))
        (exclude-list (if (listp exclude) exclude nil))
        (manual-names (mapcar #'car manual)))
    `(progn
       ;; Documentation
       ,@(when documentation
           `((defparameter ,(intern (format nil "*~A-DOCUMENTATION*"
                                           (symbol-name library-name)))
               ,documentation)))

       ;; Auto-generated bindings
       ,@(loop for header in header-list
               append (generate-bindings-from-header
                       header library-name
                       :prefix prefix
                       :exclude (append exclude-list manual-names)
                       :include-paths include-paths
                       :defines defines))

       ;; Manual wrappers
       ,@(mapcar #'cdr manual)

       ;; Return library name
       ',library-name)))

(defmacro define-library-bindings (package-name library-name header-path
                                   &key prefix exclude manual-wrappers
                                        include-paths defines exports)
  "Define a complete FFI binding package for a C library.

   Creates a package with auto-generated bindings plus optional manual wrappers.

   PACKAGE-NAME - Name of the package to create
   LIBRARY-NAME - Library keyword or path for FFI calls
   HEADER-PATH - Path to the main header file
   PREFIX - Function name prefix filter
   EXCLUDE - Functions to exclude from auto-binding
   MANUAL-WRAPPERS - List of manual wrapper forms to include
   EXPORTS - Additional symbols to export

   Example:
   (define-library-bindings :my-libpq-bindings
     :libpq \"/usr/include/libpq-fe.h\"
     :prefix \"PQ\"
     :exclude (\"PQprint\")
     :manual-wrappers ((defun pq-connect-safe (...) ...))
     :exports (pq-connect-safe))"
  `(progn
     (defpackage ,package-name
       (:use cl)
       (:export ,@exports
                ;; Auto-export generated bindings based on prefix
                ,@(let ((info (parse-header-with-cache header-path
                                                       :include-paths include-paths
                                                       :defines defines)))
                    (loop for func in (getf info :functions)
                          for name = (getf func :name)
                          when (and name
                                    (or (null prefix)
                                        (and (>= (length name) (length prefix))
                                             (string= prefix name
                                                     :end2 (length prefix))))
                                    (not (member name exclude :test #'string=)))
                            collect (c-name-to-lisp-name name)))))
     (in-package ,package-name)

     ;; Generate auto-bindings
     ,@(generate-bindings-from-header header-path library-name
                                      :prefix prefix
                                      :exclude exclude
                                      :include-paths include-paths
                                      :defines defines)

     ;; Include manual wrappers
     ,@manual-wrappers))

;;; ============================================================================
;;; Library Description
;;; ============================================================================

(defun describe-library-api (header-path &key prefix include-paths defines)
  "Print a description of the C API defined in a header.
   Useful for planning which functions need manual wrappers."
  (let* ((info (parse-header-with-cache header-path
                                        :include-paths include-paths
                                        :defines defines))
         (functions (getf info :functions))
         (structs (getf info :structs))
         (enums (getf info :enums)))
    (format t "~%=== Library API: ~A ===~%" header-path)

    ;; Functions
    (format t "~%Functions (~D total):~%" (length functions))
    (dolist (func (if prefix
                      (remove-if-not
                       (lambda (f)
                         (let ((name (getf f :name)))
                           (and name
                                (>= (length name) (length prefix))
                                (string= prefix name :end2 (length prefix)))))
                       functions)
                      functions))
      (format t "  ~A~%" (getf func :name)))

    ;; Structs
    (format t "~%Structs (~D total):~%" (length structs))
    (dolist (s structs)
      (format t "  ~A~%" (getf s :name)))

    ;; Enums
    (format t "~%Enums (~D total):~%" (length enums))
    (dolist (e enums)
      (format t "  ~A~%" (getf e :name)))

    (values)))

;;; ============================================================================
;;; Macro Classification and Binding Generation
;;; ============================================================================
;;;
;;; C preprocessor macros are invisible to libclang's AST parsing, but can be
;;; discovered via macro cursor kinds when parsing with DetailedPreprocessingRecord.
;;; This section provides utilities to classify macros and generate appropriate
;;; bindings.
;;;
;;; Macro Categories:
;;; - :constant - Simple value definition (#define FOO 42)
;;; - :function-wrapper - Macro that wraps a function call (#define foo(x) bar(x))
;;; - :accessor - Struct field accessor macro
;;; - :complex - Requires manual analysis/wrapping

(defun classify-macro (macro-info)
  "Classify a macro into a binding category.

   MACRO-INFO should be a plist with :name, :body-string, :is-function-like
   (as returned by lc:extract-macro-definitions)

   Returns one of:
   - :constant - No params, evaluates to a constant value
   - :function-wrapper - Wraps a function call (bindable via underlying function)
   - :accessor - Struct field access pattern
   - :complex - Requires manual analysis

   Also returns secondary value with classification metadata."
  (let ((body (or (getf macro-info :body-string)
                  (getf macro-info :body)))
        (params (getf macro-info :params))
        (is-function-like (getf macro-info :is-function-like)))
    (cond
      ;; No parameters = likely constant
      ((not is-function-like)
       (let ((const-value (parse-constant-body body)))
         (if const-value
             (values :constant (list :value const-value))
             (values :complex (list :reason "Non-constant object-like macro")))))

      ;; Function-like macro that calls another function
      ((multiple-value-bind (fn-name fn-args)
           (macro-calls-function-p body params)
         (when fn-name
           (return-from classify-macro
             (values :function-wrapper
                     (list :target-function fn-name
                           :argument-mapping fn-args))))))

      ;; Struct field accessor pattern
      ((macro-is-accessor-p body params)
       (values :accessor (list :pattern "struct-field")))

      ;; Complex - needs manual review
      (t
       (values :complex (list :reason "Complex macro body"))))))

(defun parse-constant-body (body)
  "Try to parse a macro body as a constant value.
   Returns the value if parseable, NIL otherwise."
  (when (and body (stringp body))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) body)))
      (cond
        ;; Integer literal
        ((every #'digit-char-p trimmed)
         (parse-integer trimmed))

        ;; Hex literal
        ((and (>= (length trimmed) 2)
              (string= "0x" trimmed :end2 2))
         (parse-integer trimmed :start 2 :radix 16))

        ;; Negative integer
        ((and (>= (length trimmed) 2)
              (char= (char trimmed 0) #\-)
              (every #'digit-char-p (subseq trimmed 1)))
         (parse-integer trimmed))

        ;; String literal
        ((and (>= (length trimmed) 2)
              (char= (char trimmed 0) #\")
              (char= (char trimmed (1- (length trimmed))) #\"))
         (subseq trimmed 1 (1- (length trimmed))))

        ;; Character literal
        ((and (>= (length trimmed) 3)
              (char= (char trimmed 0) #\')
              (char= (char trimmed (1- (length trimmed))) #\'))
         (char-code (char trimmed 1)))

        ;; Could not parse as constant
        (t nil)))))

(defun macro-calls-function-p (body params)
  "Check if a macro body is a simple function call wrapper.

   Returns two values:
   - Function name being called (or NIL)
   - Argument mapping alist ((param-index . arg-position) ...)

   Recognizes patterns like:
   - FOO(a, b) -> bar(a, b)
   - SET_X(s, v) -> some_ctrl((s), CTRL_X, (v), NULL)"
  (when (and body (stringp body))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) body)))
      ;; Look for function call pattern: IDENTIFIER(...)
      (multiple-value-bind (match groups)
          (scan-function-call trimmed)
        (when match
          (let ((fn-name (first groups))
                (args-str (second groups)))
            ;; Check if this looks like a real function (not another macro pattern)
            (when (and fn-name
                       (not (string= fn-name "sizeof"))
                       (not (string= fn-name "offsetof"))
                       (not (string= fn-name "typeof")))
              (values fn-name
                      (parse-call-arguments args-str params)))))))))

(defun scan-function-call (body)
  "Scan for a function call pattern in macro body.
   Returns (T (fn-name args-string)) or NIL."
  ;; Simple scanner: find IDENTIFIER(...) pattern
  (let ((paren-pos (position #\( body)))
    (when (and paren-pos (> paren-pos 0))
      (let ((fn-name (subseq body 0 paren-pos)))
        ;; Validate function name is an identifier
        (when (and (alpha-char-p (char fn-name 0))
                   (every (lambda (c)
                            (or (alphanumericp c) (char= c #\_)))
                          fn-name))
          ;; Find matching close paren
          (let ((close-paren (find-matching-paren body paren-pos)))
            (when close-paren
              (values t
                      (list fn-name
                            (subseq body (1+ paren-pos) close-paren))))))))))

(defun find-matching-paren (str start)
  "Find position of matching close paren starting from open paren at START."
  (let ((depth 1))
    (loop for i from (1+ start) below (length str)
          for c = (char str i)
          do (cond
               ((char= c #\() (incf depth))
               ((char= c #\))
                (decf depth)
                (when (zerop depth)
                  (return i)))))))

(defun parse-call-arguments (args-str params)
  "Parse function call arguments and map to macro parameters.
   Returns alist of (param-name . arg-position)."
  (declare (ignore args-str params))
  ;; Simplified: return nil for now, full implementation would parse
  ;; the argument list and correlate with macro params
  nil)

(defun macro-is-accessor-p (body params)
  "Check if macro body is a struct field accessor pattern.

   Recognizes patterns like:
   - ((s)->field)
   - ((type*)(p))->member"
  (when (and body (stringp body) params (= (length params) 1))
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\( #\)) body)))
      ;; Look for -> operator indicating struct access
      (search "->" trimmed))))

;;; ============================================================================
;;; Macro Constant Evaluation (Two-Pass Technique)
;;; ============================================================================
;;;
;;; C preprocessor macros that define constants (like #define Z_OK 0) are
;;; invisible to libclang's normal AST. The two-pass technique extracts their
;;; values by generating synthetic C code that assigns the macro to a const
;;; variable, then parsing that to get the evaluated value.
;;;
;;; This technique is used by c2ffi, Dart ffigen, Rust bindgen, and others.

(defun evaluate-macro-constants (header-path macros &key include-paths defines)
  "Evaluate macro constants using the two-pass technique.

   HEADER-PATH - Path to the original header containing the macros
   MACROS - List of macro names (strings) to evaluate
   INCLUDE-PATHS - Additional include paths for parsing
   DEFINES - Preprocessor defines

   Returns an alist of (name . value) for successfully evaluated macros.
   Macros that can't be evaluated (non-constant expressions) are omitted.

   Example:
   (evaluate-macro-constants \"/usr/include/zlib.h\" '(\"Z_OK\" \"Z_BEST_COMPRESSION\"))
   => ((\"Z_OK\" . 0) (\"Z_BEST_COMPRESSION\" . 9))"
  (unless macros
    (return-from evaluate-macro-constants nil))
  (unless (lc:libclang-available-p)
    (error "libclang not available for macro evaluation"))

  ;; Generate synthetic C code
  (let ((temp-file (make-temp-c-file-for-macros header-path macros)))
    (unwind-protect
         (parse-synthetic-constants temp-file include-paths defines)
      ;; Clean up temp file
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defun make-temp-c-file-for-macros (header-path macros)
  "Generate a temporary C file that assigns macros to const variables.

   Creates a file like:
   #include \"original_header.h\"
   const long long _MACRO_NAME_0 = MACRO_NAME;
   const long long _MACRO_NAME_1 = OTHER_MACRO;
   ...

   Returns the path to the temp file."
  (let* ((temp-dir (or (sb-ext:posix-getenv "TMPDIR")
                       (sb-ext:posix-getenv "TMP")
                       "/tmp"))
         (temp-path (merge-pathnames
                     (format nil "epsilon-macro-eval-~A.c"
                             (random 1000000))
                     temp-dir)))
    (with-open-file (out temp-path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
      ;; Include the original header
      (format out "/* Generated by epsilon.foreign for macro evaluation */~%")
      (format out "#include \"~A\"~%~%" (namestring (truename header-path)))

      ;; Generate const declarations for each macro
      ;; We use long long to handle both signed and large unsigned values
      ;; We prefix with _ and suffix with index to avoid name collisions
      (loop for macro in macros
            for i from 0
            do (format out "const long long _eval_~D = ~A;~%" i macro)))
    ;; Return as string for libclang compatibility
    (namestring temp-path)))

(defun parse-synthetic-constants (temp-file include-paths defines)
  "Parse the synthetic C file and extract the evaluated constant values.

   Returns an alist of (index . value) where index corresponds to the
   macro position in the original list."
  (let ((results nil))
    (lc:with-parsed-header (tu temp-file
                               :include-paths include-paths
                               :defines defines)
      ;; Traverse to find global variable declarations
      (let ((root (lc:get-translation-unit-cursor tu)))
        (lc:visit-children root
          (lambda (cursor)
            (when (= (lc:cursor-kind cursor) lc:+cxcursor-var-decl+)
              (let ((name (lc:cursor-spelling cursor)))
                ;; Match our _eval_N pattern
                (when (and name (>= (length name) 6)
                           (string= "_eval_" name :end2 6))
                  ;; Extract the literal value from the cursor's tokens
                  (handler-case
                      (let ((value (extract-var-initializer-value tu cursor)))
                        (when value
                          ;; Extract index from name
                          (let ((index (parse-integer name :start 6)))
                            (push (cons index value) results))))
                    (error () nil)))))
            lc:+cxchildvisit-continue+))))
    ;; Results are (index . value), reverse to get correct order
    (nreverse results)))

(defun extract-var-initializer-value (tu cursor)
  "Extract the integer value from a variable initializer.

   For a declaration like 'const long long _eval_0 = 42;',
   returns 42.

   Uses clang_Cursor_Evaluate to get the compile-time constant value."
  (declare (ignore tu))
  (multiple-value-bind (value kind)
      (lc:cursor-evaluate cursor)
    (when (eq kind :int)
      value)))

(defun parse-c-integer-literal (str)
  "Parse a C integer literal string to a Lisp integer.

   Handles:
   - Decimal: 42
   - Hex: 0xFF, 0XFF
   - Octal: 0777
   - Binary: 0b101 (C++14/C23)
   - Suffixes: 42L, 42UL, 42LL, 42ULL"
  (when (and str (> (length str) 0))
    ;; Remove any suffix (L, U, LL, ULL, etc.)
    (let ((clean (string-right-trim "LlUu" str)))
      (when (> (length clean) 0)
        (handler-case
            (cond
              ;; Hex
              ((and (>= (length clean) 2)
                    (char= (char clean 0) #\0)
                    (member (char clean 1) '(#\x #\X)))
               (parse-integer clean :start 2 :radix 16))
              ;; Binary
              ((and (>= (length clean) 2)
                    (char= (char clean 0) #\0)
                    (member (char clean 1) '(#\b #\B)))
               (parse-integer clean :start 2 :radix 2))
              ;; Octal (leading 0 but not 0x/0b)
              ((and (> (length clean) 1)
                    (char= (char clean 0) #\0)
                    (digit-char-p (char clean 1)))
               (parse-integer clean :start 1 :radix 8))
              ;; Decimal
              (t
               (parse-integer clean)))
          (error () nil))))))

(defun evaluate-header-macro-constants (header-path &key prefix include-paths defines)
  "High-level function to extract and evaluate all macro constants from a header.

   Combines macro extraction with two-pass evaluation.

   HEADER-PATH - Path to C header file
   PREFIX - Only include macros starting with this prefix
   INCLUDE-PATHS - Additional include directories
   DEFINES - Preprocessor definitions

   Returns an alist of (macro-name . value) for all evaluable constants."
  (unless (lc:libclang-available-p)
    (return-from evaluate-header-macro-constants nil))

  ;; First pass: extract macro names
  (let ((macro-names nil))
    (lc:with-parsed-header (tu header-path
                               :include-paths include-paths
                               :defines defines
                               :options lc:+cxtranslationunit-detailed-preprocessing-record+)
      (let ((macros (lc:extract-macro-definitions tu :prefix prefix)))
        ;; Filter to non-function-like macros (likely constants)
        (dolist (macro macros)
          (unless (getf macro :is-function-like)
            (push (getf macro :name) macro-names)))))

    ;; Second pass: evaluate the macros
    (when macro-names
      (let* ((macro-list (nreverse macro-names))
             (indexed-values (evaluate-macro-constants header-path macro-list
                                                       :include-paths include-paths
                                                       :defines defines)))
        ;; Correlate indices back to names
        (loop for (index . value) in indexed-values
              for name = (nth index macro-list)
              when name
                collect (cons name value))))))

(defun generate-defconstants-from-header (header-path &key prefix include-paths defines
                                                           (package *package*))
  "Generate defconstant forms for all evaluable macro constants in a header.

   Returns a list of (defconstant +NAME+ value) forms.

   Example:
   (generate-defconstants-from-header \"/usr/include/zlib.h\" :prefix \"Z_\")
   => ((DEFCONSTANT +Z-OK+ 0)
       (DEFCONSTANT +Z-STREAM-END+ 1)
       ...)"
  (let ((constants (evaluate-header-macro-constants header-path
                                                     :prefix prefix
                                                     :include-paths include-paths
                                                     :defines defines)))
    (loop for (name . value) in constants
          collect `(defconstant ,(intern (format nil "+~A+"
                                                  (substitute #\- #\_ (string-upcase name)))
                                          package)
                      ,value
                      ,(format nil "Auto-generated from ~A" name)))))

;;; ============================================================================
;;; Macro Binding Generation
;;; ============================================================================

(defun generate-macro-bindings (header-path library &key prefix include-paths defines)
  "Generate bindings for macros in a header file.

   HEADER-PATH - Path to header file (parsed with DetailedPreprocessingRecord)
   LIBRARY - Library for FFI calls
   PREFIX - Only include macros starting with this prefix

   Returns a list of binding forms and metadata:
   ((:type :constant :name \"FOO\" :form (defconstant +FOO+ 42))
    (:type :function-wrapper :name \"SSL_set_mode\" :target \"SSL_ctrl\" :form ...)
    (:type :manual-required :name \"COMPLEX_MACRO\" :reason \"...\"))"
  (declare (ignore library))
  (unless (lc:libclang-available-p)
    (error "libclang not available for macro extraction"))

  (lc:with-parsed-header (tu header-path
                             :include-paths include-paths
                             :defines defines
                             :options lc:+cxtranslationunit-detailed-preprocessing-record+)
    (let ((macros (lc:extract-macro-definitions tu :prefix prefix))
          (results nil))
      (dolist (macro macros)
        (multiple-value-bind (kind metadata)
            (classify-macro macro)
          (let ((name (getf macro :name))
                (binding nil))
            (case kind
              (:constant
               (let ((value (getf metadata :value)))
                 (when value
                   (setf binding
                         `(defconstant ,(macro-name-to-constant name)
                            ,value)))))

              (:function-wrapper
               (let ((target-fn (getf metadata :target-function)))
                 ;; For function wrappers, we note the target function
                 ;; The actual binding would call the underlying function
                 (setf binding
                       `(:wrapper-for ,target-fn
                                      :macro-name ,name
                                      :params ,(getf macro :params)))))

              (:accessor
               ;; Accessors are usually handled by struct definitions
               (setf binding `(:accessor ,name)))

              (:complex
               (setf binding `(:manual-required ,name
                                                :reason ,(getf metadata :reason)
                                                :body ,(or (getf macro :body-string)
                                                           (getf macro :body))))))

            (push (list :type kind
                        :name name
                        :form binding)
                  results))))
      (nreverse results))))

(defun macro-name-to-constant (name)
  "Convert C macro name to Lisp constant symbol.
   SSL_CTRL_MODE -> +SSL-CTRL-MODE+"
  (intern
   (format nil "+~A+"
           (substitute #\- #\_ (string-upcase name)))))

(defun describe-header-macros (header-path &key prefix include-paths defines)
  "Print a description of macros in a header file.
   Useful for discovering which macros exist and how they're classified."
  (unless (lc:libclang-available-p)
    (format t "libclang not available~%")
    (return-from describe-header-macros nil))

  (handler-case
      (lc:with-parsed-header (tu header-path
                                 :include-paths include-paths
                                 :defines defines
                                 :options lc:+cxtranslationunit-detailed-preprocessing-record+)
        (let ((macros (lc:extract-macro-definitions tu :prefix prefix))
              (constants 0)
              (wrappers 0)
              (accessors 0)
              (complex 0))
          (format t "~%=== Macros in ~A ===~%" header-path)
          (when prefix
            (format t "(Filtered by prefix: ~A)~%" prefix))
          (format t "~%")

          (dolist (macro macros)
            (multiple-value-bind (kind metadata)
                (classify-macro macro)
              (let ((name (getf macro :name))
                    (body (or (getf macro :body-string)
                              (getf macro :body))))
                (format t "~A: ~A~%" kind name)
                (when body
                  (format t "  Body: ~A~%"
                          (if (> (length body) 60)
                              (concatenate 'string (subseq body 0 57) "...")
                              body)))
                (when (getf metadata :target-function)
                  (format t "  Target: ~A~%" (getf metadata :target-function)))
                (case kind
                  (:constant (incf constants))
                  (:function-wrapper (incf wrappers))
                  (:accessor (incf accessors))
                  (:complex (incf complex))))))

          (format t "~%Summary:~%")
          (format t "  Constants: ~D~%" constants)
          (format t "  Function wrappers: ~D~%" wrappers)
          (format t "  Accessors: ~D~%" accessors)
          (format t "  Complex (manual): ~D~%" complex)
          (format t "  Total: ~D~%" (length macros))))
    (error (e)
      (format t "Error parsing header: ~A~%" e)))
  (values))
