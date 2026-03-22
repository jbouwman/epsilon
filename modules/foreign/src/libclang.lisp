;;;; libclang.lisp - Bindings to LLVM's libclang for C introspection
;;;;
;;;; This module provides direct access to libclang's C API for full AST
;;;; traversal, type extraction, and header analysis.
;;;;
;;;; The vendored SBCL (srbv fork) supports struct-by-value returns, enabling
;;;; direct use of libclang functions that return CXCursor, CXType, CXString.

(defpackage epsilon.foreign.libclang
  (:use cl)
  (:export
   ;; Library management
   #:*libclang*
   #:*libclang-path*
   #:load-libclang
   #:libclang-available-p

   ;; Index management
   #:create-index
   #:dispose-index

   ;; Translation unit management
   #:parse-translation-unit
   #:dispose-translation-unit
   #:parse-header
   #:parse-header-string
   #:with-parsed-header

   ;; Cursor operations
   #:get-translation-unit-cursor
   #:cursor-null-p
   #:cursor-kind
   #:cursor-kind-spelling
   #:cursor-spelling
   #:cursor-type
   #:cursor-location
   #:cursor-extent
   #:visit-children

   ;; Type operations
   #:type-kind
   #:type-spelling
   #:type-size
   #:type-alignment
   #:type-canonical
   #:type-pointee
   #:type-result
   #:type-num-arg-types
   #:type-arg-type

   ;; High-level extraction
   #:extract-struct-info
   #:extract-function-info
   #:extract-enum-info
   #:extract-typedef-info
   #:extract-all-declarations
   #:extract-macro-definitions
   #:extract-macro-info

   ;; Token operations
   #:tokenize-range
   #:get-token-at-index
   #:token-kind
   #:token-spelling
   #:dispose-tokens
   #:get-macro-body-tokens
   #:tokens-to-string

   ;; Macro utilities
   #:macro-definition-p
   #:macro-function-like-p

   ;; Cursor evaluation
   #:cursor-evaluate
   #:+cxeval-int+
   #:+cxeval-float+
   #:+cxeval-unexposed+

   ;; Cursor kind constants
   #:+cxcursor-unexposed-decl+
   #:+cxcursor-struct-decl+
   #:+cxcursor-union-decl+
   #:+cxcursor-enum-decl+
   #:+cxcursor-field-decl+
   #:+cxcursor-enum-constant-decl+
   #:+cxcursor-function-decl+
   #:+cxcursor-var-decl+
   #:+cxcursor-parm-decl+
   #:+cxcursor-typedef-decl+
   #:+cxcursor-type-ref+
   #:+cxcursor-invalid-file+

   ;; Preprocessing cursor kind constants
   #:+cxcursor-preprocessing-directive+
   #:+cxcursor-macro-definition+
   #:+cxcursor-macro-expansion+
   #:+cxcursor-inclusion-directive+

   ;; Token kind constants
   #:+cxtoken-punctuation+
   #:+cxtoken-keyword+
   #:+cxtoken-identifier+
   #:+cxtoken-literal+
   #:+cxtoken-comment+

   ;; Type kind constants
   #:+cxtype-invalid+
   #:+cxtype-void+
   #:+cxtype-bool+
   #:+cxtype-char-u+
   #:+cxtype-char-s+
   #:+cxtype-schar+
   #:+cxtype-uchar+
   #:+cxtype-short+
   #:+cxtype-ushort+
   #:+cxtype-int+
   #:+cxtype-uint+
   #:+cxtype-long+
   #:+cxtype-ulong+
   #:+cxtype-longlong+
   #:+cxtype-ulonglong+
   #:+cxtype-float+
   #:+cxtype-double+
   #:+cxtype-pointer+
   #:+cxtype-record+
   #:+cxtype-enum+
   #:+cxtype-typedef+
   #:+cxtype-functionproto+
   #:+cxtype-constantarray+
   #:+cxtype-elaborated+

   ;; Translation unit option constants
   #:+cxtranslationunit-none+
   #:+cxtranslationunit-detailed-preprocessing-record+
   #:+cxtranslationunit-incomplete+
   #:+cxtranslationunit-precompiled-preamble+
   #:+cxtranslationunit-skip-function-bodies+
   #:+cxtranslationunit-keep-going+

   ;; Child visit result constants
   #:+cxchildvisit-break+
   #:+cxchildvisit-continue+
   #:+cxchildvisit-recurse+

   ;; Cleanup
   #:cleanup)
  (:enter t))

;;; ============================================================================
;;; Signal Handler Management
;;; ============================================================================
;;;
;;; The vendored SBCL fork (jbouwman/sbcl) now preserves signal handlers
;;; established by foreign libraries at load time (PR #22). LLVM/libclang's
;;; handlers chain correctly to SBCL's handlers, so no manual save/restore
;;; is needed. This macro is retained as a no-op for call-site compatibility.

(defmacro with-sbcl-signal-handlers (&body body)
  "No-op -- the vendored SBCL fork handles signal handler chaining natively."
  `(progn ,@body))

;;; ============================================================================
;;; Library Loading
;;; ============================================================================

(defvar *libclang* nil
  "Handle to loaded libclang library")

(defvar *libclang-loaded* nil
  "Whether libclang has been successfully loaded")

(defvar *libclang-path*
  #+linux "libclang.so"
  #+darwin "/Library/Developer/CommandLineTools/usr/lib/libclang.dylib"
  #-(or linux darwin) nil
  "Path to libclang library")

(defun find-libclang-path ()
  "Try to find libclang on the system.
   Search order:
   1. LIBCLANG_PATH environment variable
   2. *libclang-path* if already set and valid
   4. Well-known system paths"
  (or
   ;; 1. Environment variable takes precedence
   (let ((env-path (sb-ext:posix-getenv "LIBCLANG_PATH")))
     (when (and env-path (probe-file env-path))
       env-path))
   ;; 2. Previously set path
   (when (and *libclang-path* (probe-file *libclang-path*))
     *libclang-path*)
   ;; 4. Well-known system paths as fallback
   (loop for path in '(;; macOS - Xcode and CommandLineTools
                       "/Library/Developer/CommandLineTools/usr/lib/libclang.dylib"
                       "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"
                       "/Applications/Xcode.app/Contents/Frameworks/libclang.dylib"
                       ;; macOS - Homebrew
                       "/opt/homebrew/opt/llvm/lib/libclang.dylib"
                       "/usr/local/opt/llvm/lib/libclang.dylib"
                       ;; Linux - versioned libraries (common on Debian/Ubuntu)
                       "/usr/lib/x86_64-linux-gnu/libclang-18.so.18"
                       "/usr/lib/x86_64-linux-gnu/libclang-17.so.17"
                       "/usr/lib/x86_64-linux-gnu/libclang-16.so.16"
                       "/usr/lib/x86_64-linux-gnu/libclang-15.so.15"
                       "/usr/lib/x86_64-linux-gnu/libclang-14.so.14"
                       ;; Linux - llvm directory structure
                       "/usr/lib/llvm-18/lib/libclang.so.1"
                       "/usr/lib/llvm-18/lib/libclang.so"
                       "/usr/lib/llvm-17/lib/libclang.so"
                       "/usr/lib/llvm-16/lib/libclang.so"
                       "/usr/lib/llvm-15/lib/libclang.so"
                       "/usr/lib/llvm-14/lib/libclang.so"
                       "/usr/lib/x86_64-linux-gnu/libclang-15.so"
                       "/usr/lib/libclang.so")
         when (probe-file path)
         return path)))

(defun load-libclang ()
  "Load libclang library. Returns T on success, NIL on failure."
  (when *libclang-loaded*
    (return-from load-libclang t))
  (let ((path (find-libclang-path)))
    (unless path
      (return-from load-libclang nil))
    (setf *libclang-path* path)
    (handler-case
        (progn
          (setf *libclang* (sb-alien:load-shared-object path))
          (setf *libclang-loaded* t)
          t)
      (error (e)
        (format *error-output* "Failed to load libclang: ~A~%" e)
        nil))))

(defun libclang-available-p ()
  "Check if libclang is available on this system."
  (or *libclang-loaded*
      (load-libclang)))

;;; ============================================================================
;;; Struct Definitions (with explicit sizes for struct-by-value returns)
;;; ============================================================================

;; CXCursor: 32 bytes
;; - kind: enum (4 bytes)
;; - xdata: int (4 bytes)
;; - data: void*[3] (24 bytes)
(sb-alien:define-alien-type nil
  (sb-alien:struct cxcursor
    (kind (sb-alien:signed 32))
    (xdata (sb-alien:signed 32))
    (data0 (sb-alien:signed 64))
    (data1 (sb-alien:signed 64))
    (data2 (sb-alien:signed 64))))

;; CXType: 24 bytes
;; - kind: enum (4 bytes)
;; - padding: 4 bytes
;; - data: void*[2] (16 bytes)
(sb-alien:define-alien-type nil
  (sb-alien:struct cxtype
    (kind (sb-alien:signed 32))
    (padding (sb-alien:signed 32))
    (data0 (sb-alien:signed 64))
    (data1 (sb-alien:signed 64))))

;; CXString: 16 bytes
;; - data: const void* (8 bytes)
;; - private_flags: unsigned (4 bytes)
;; - padding: 4 bytes
(sb-alien:define-alien-type nil
  (sb-alien:struct cxstring
    (data (sb-alien:signed 64))
    (private-flags (sb-alien:unsigned 32))
    (padding (sb-alien:unsigned 32))))

;; CXSourceLocation: 24 bytes
(sb-alien:define-alien-type nil
  (sb-alien:struct cxsourcelocation
    (ptr-data0 (sb-alien:signed 64))
    (ptr-data1 (sb-alien:signed 64))
    (int-data (sb-alien:unsigned 32))
    (padding (sb-alien:unsigned 32))))

;; CXSourceRange: 24 bytes (contains begin/end packed)
(sb-alien:define-alien-type nil
  (sb-alien:struct cxsourcerange
    (ptr-data0 (sb-alien:signed 64))
    (ptr-data1 (sb-alien:signed 64))
    (begin-int-data (sb-alien:unsigned 32))
    (end-int-data (sb-alien:unsigned 32))))

;;; ============================================================================
;;; Cursor Kind Constants
;;; ============================================================================

(defconstant +cxcursor-unexposed-decl+ 1)
(defconstant +cxcursor-struct-decl+ 2)
(defconstant +cxcursor-union-decl+ 3)
(defconstant +cxcursor-class-decl+ 4)
(defconstant +cxcursor-enum-decl+ 5)
(defconstant +cxcursor-field-decl+ 6)
(defconstant +cxcursor-enum-constant-decl+ 7)
(defconstant +cxcursor-function-decl+ 8)
(defconstant +cxcursor-var-decl+ 9)
(defconstant +cxcursor-parm-decl+ 10)
(defconstant +cxcursor-typedef-decl+ 20)
(defconstant +cxcursor-type-ref+ 43)
(defconstant +cxcursor-invalid-file+ 70)

;;; ============================================================================
;;; Preprocessing Cursor Kind Constants
;;; ============================================================================
;;;
;;; These cursor kinds represent preprocessor constructs. To see them,
;;; parse with +cxtranslationunit-detailed-preprocessing-record+.

(defconstant +cxcursor-preprocessing-directive+ 500)
(defconstant +cxcursor-macro-definition+ 501)
(defconstant +cxcursor-macro-expansion+ 502)
(defconstant +cxcursor-inclusion-directive+ 503)

;;; ============================================================================
;;; Type Kind Constants
;;; ============================================================================

(defconstant +cxtype-invalid+ 0)
(defconstant +cxtype-unexposed+ 1)
(defconstant +cxtype-void+ 2)
(defconstant +cxtype-bool+ 3)
(defconstant +cxtype-char-u+ 4)
(defconstant +cxtype-uchar+ 5)
(defconstant +cxtype-char16+ 6)
(defconstant +cxtype-char32+ 7)
(defconstant +cxtype-ushort+ 8)
(defconstant +cxtype-uint+ 9)
(defconstant +cxtype-ulong+ 10)
(defconstant +cxtype-ulonglong+ 11)
(defconstant +cxtype-char-s+ 13)
(defconstant +cxtype-schar+ 14)
(defconstant +cxtype-wchar+ 15)
(defconstant +cxtype-short+ 16)
(defconstant +cxtype-int+ 17)
(defconstant +cxtype-long+ 18)
(defconstant +cxtype-longlong+ 19)
(defconstant +cxtype-float+ 21)
(defconstant +cxtype-double+ 22)
(defconstant +cxtype-longdouble+ 23)
(defconstant +cxtype-pointer+ 101)
(defconstant +cxtype-record+ 105)
(defconstant +cxtype-enum+ 106)
(defconstant +cxtype-typedef+ 107)
(defconstant +cxtype-functionproto+ 111)
(defconstant +cxtype-constantarray+ 112)
(defconstant +cxtype-elaborated+ 119)

;;; ============================================================================
;;; Child Visit Result Constants
;;; ============================================================================

(defconstant +cxchildvisit-break+ 0)
(defconstant +cxchildvisit-continue+ 1)
(defconstant +cxchildvisit-recurse+ 2)

;;; ============================================================================
;;; Token Kind Constants (CXTokenKind)
;;; ============================================================================

(defconstant +cxtoken-punctuation+ 0)
(defconstant +cxtoken-keyword+ 1)
(defconstant +cxtoken-identifier+ 2)
(defconstant +cxtoken-literal+ 3)
(defconstant +cxtoken-comment+ 4)

;;; ============================================================================
;;; Translation Unit Options
;;; ============================================================================

(defconstant +cxtranslationunit-none+ #x0)
(defconstant +cxtranslationunit-detailed-preprocessing-record+ #x01)
(defconstant +cxtranslationunit-incomplete+ #x02)
(defconstant +cxtranslationunit-precompiled-preamble+ #x04)
(defconstant +cxtranslationunit-skip-function-bodies+ #x40)
(defconstant +cxtranslationunit-keep-going+ #x200)

;;; ============================================================================
;;; Low-level FFI Bindings - Simple Return Types
;;; ============================================================================

(sb-alien:define-alien-routine "clang_createIndex"
    sb-alien:system-area-pointer
  (exclude-declarations-from-pch sb-alien:int)
  (display-diagnostics sb-alien:int))

(sb-alien:define-alien-routine "clang_disposeIndex"
    sb-alien:void
  (index sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_parseTranslationUnit"
    sb-alien:system-area-pointer
  (index sb-alien:system-area-pointer)
  (source-filename sb-alien:c-string)
  (command-line-args sb-alien:system-area-pointer)
  (num-command-line-args sb-alien:int)
  (unsaved-files sb-alien:system-area-pointer)
  (num-unsaved-files sb-alien:unsigned-int)
  (options sb-alien:unsigned-int))

(sb-alien:define-alien-routine "clang_disposeTranslationUnit"
    sb-alien:void
  (tu sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_getNumDiagnostics"
    sb-alien:unsigned-int
  (tu sb-alien:system-area-pointer))

;;; ============================================================================
;;; Low-level FFI Bindings - Struct-by-Value Returns (requires srbv SBCL)
;;; ============================================================================

;; Cursor functions
(sb-alien:define-alien-routine "clang_getTranslationUnitCursor"
    (sb-alien:struct cxcursor)
  (tu sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_getNullCursor"
    (sb-alien:struct cxcursor))

(sb-alien:define-alien-routine "clang_Cursor_isNull"
    sb-alien:int
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorKind"
    sb-alien:int
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorSpelling"
    (sb-alien:struct cxstring)
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorType"
    (sb-alien:struct cxtype)
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorLocation"
    (sb-alien:struct cxsourcelocation)
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorExtent"
    (sb-alien:struct cxsourcerange)
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_getCursorKindSpelling"
    (sb-alien:struct cxstring)
  (kind sb-alien:int))

;; Type functions
(sb-alien:define-alien-routine "clang_getTypeSpelling"
    (sb-alien:struct cxstring)
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_Type_getSizeOf"
    sb-alien:long
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_Type_getAlignOf"
    sb-alien:long
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_getCanonicalType"
    (sb-alien:struct cxtype)
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_getPointeeType"
    (sb-alien:struct cxtype)
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_getResultType"
    (sb-alien:struct cxtype)
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_getNumArgTypes"
    sb-alien:int
  (ct (sb-alien:struct cxtype)))

(sb-alien:define-alien-routine "clang_getArgType"
    (sb-alien:struct cxtype)
  (ct (sb-alien:struct cxtype))
  (i sb-alien:unsigned-int))

;; Field offset
(sb-alien:define-alien-routine "clang_Cursor_getOffsetOfField"
    sb-alien:long
  (cursor (sb-alien:struct cxcursor)))

;; Enum value
(sb-alien:define-alien-routine "clang_getEnumConstantDeclValue"
    sb-alien:long
  (cursor (sb-alien:struct cxcursor)))

;; Cursor evaluation (for compile-time constant expressions)
;; CXEvalResult is an opaque pointer
(sb-alien:define-alien-routine "clang_Cursor_Evaluate"
    sb-alien:system-area-pointer
  (cursor (sb-alien:struct cxcursor)))

;; CXEvalResultKind enum values
(defconstant +cxeval-int+ 1)
(defconstant +cxeval-float+ 2)
(defconstant +cxeval-objcliteral+ 3)
(defconstant +cxeval-strliteral+ 4)
(defconstant +cxeval-cfstr+ 5)
(defconstant +cxeval-other+ 6)
(defconstant +cxeval-unexposed+ 0)

(sb-alien:define-alien-routine "clang_EvalResult_getKind"
    sb-alien:int
  (result sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_EvalResult_getAsLongLong"
    sb-alien:long-long
  (result sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_EvalResult_getAsDouble"
    sb-alien:double
  (result sb-alien:system-area-pointer))

(sb-alien:define-alien-routine "clang_EvalResult_dispose"
    sb-alien:void
  (result sb-alien:system-area-pointer))

;; Typedef underlying type
(sb-alien:define-alien-routine "clang_getTypedefDeclUnderlyingType"
    (sb-alien:struct cxtype)
  (cursor (sb-alien:struct cxcursor)))

;; String disposal
(sb-alien:define-alien-routine "clang_disposeString"
    sb-alien:void
  (str (sb-alien:struct cxstring)))

(sb-alien:define-alien-routine "clang_getCString"
    sb-alien:c-string
  (str (sb-alien:struct cxstring)))

;;; ============================================================================
;;; CXString Helper
;;; ============================================================================

(defun cxstring-to-string (cxstr)
  "Convert a CXString to a Lisp string, disposing the CXString."
  (let ((cstr (clang-getcstring cxstr)))
    (prog1
        (when cstr (copy-seq cstr))
      (clang-disposestring cxstr))))

;;; ============================================================================
;;; Index Management
;;; ============================================================================

(defvar *default-index* nil
  "Default libclang index for convenience functions")

(defun create-index (&key (exclude-declarations-from-pch nil)
                          (display-diagnostics nil))
  "Create a new libclang index for parsing translation units."
  (unless (libclang-available-p)
    (error "libclang not available"))
  (with-sbcl-signal-handlers
    (clang-createindex (if exclude-declarations-from-pch 1 0)
                       (if display-diagnostics 1 0))))

(defun dispose-index (index)
  "Dispose of a libclang index."
  (when (and index (not (sb-sys:sap= index (sb-sys:int-sap 0))))
    (clang-disposeindex index)))

;;; ============================================================================
;;; Translation Unit Management
;;; ============================================================================

(defun make-c-string-array (strings)
  "Allocate an array of C strings and return (values pointer cleanup-fn)."
  (let* ((n (length strings))
         (ptr-array-size (* 8 (max n 1)))
         (ptr-array-alien (sb-alien:make-alien (sb-alien:unsigned 8) ptr-array-size))
         (ptr-array-sap (sb-alien:alien-sap ptr-array-alien))
         (allocated-aliens nil))
    (handler-case
        (progn
          (loop for str in strings
                for i from 0
                do (let ((cstr-alien (sb-alien:make-alien-string str)))
                     (push cstr-alien allocated-aliens)
                     (setf (sb-sys:sap-ref-64 ptr-array-sap (* i 8))
                           (sb-sys:sap-int (sb-alien:alien-sap cstr-alien)))))
          (values ptr-array-sap
                  (lambda ()
                    (dolist (alien allocated-aliens)
                      (sb-alien:free-alien alien))
                    (sb-alien:free-alien ptr-array-alien))))
      (error (e)
        (dolist (alien allocated-aliens)
          (sb-alien:free-alien alien))
        (sb-alien:free-alien ptr-array-alien)
        (error e)))))

(defun parse-translation-unit (index source-file &key
                                      (command-line-args nil)
                                      (unsaved-files nil)
                                      (options +cxtranslationunit-none+))
  "Parse a source file into a translation unit."
  (declare (ignore unsaved-files))
  (unless index
    (error "Index is required"))
  (with-sbcl-signal-handlers
    (let ((num-args (length command-line-args)))
      (if (zerop num-args)
          (clang-parsetranslationunit
           index source-file
           (sb-sys:int-sap 0) 0
           (sb-sys:int-sap 0) 0
           options)
          (multiple-value-bind (args-sap cleanup)
              (make-c-string-array command-line-args)
            (unwind-protect
                (clang-parsetranslationunit
                 index source-file
                 args-sap num-args
                 (sb-sys:int-sap 0) 0
                 options)
              (funcall cleanup)))))))

(defun dispose-translation-unit (tu)
  "Dispose of a translation unit."
  (when (and tu (not (sb-sys:sap= tu (sb-sys:int-sap 0))))
    (clang-disposetranslationunit tu)))

(defun translation-unit-valid-p (tu)
  "Check if translation unit was parsed successfully."
  (and tu
       (not (sb-sys:sap= tu (sb-sys:int-sap 0)))
       (zerop (clang-getnumdiagnostics tu))))

(defvar *system-include-paths* :undetected
  "Cached list of system include paths for libclang.
   Set to :undetected before first use, then auto-populated.")

(defun find-clang-binary ()
  "Find the clang binary on PATH without forking a process.
   Returns the resolved real path, or NIL."
  (let ((cc (or (sb-ext:posix-getenv "CC") "clang"))
        (path-env (sb-ext:posix-getenv "PATH")))
    (when path-env
      (flet ((try-path (p)
               (when (and (probe-file p)
                          ;; Ensure it's not a directory
                          (not (let ((name (file-namestring (truename p))))
                                 (or (null name) (string= name "")))))
                 (namestring (truename p)))))
        ;; If CC is absolute, use it directly
        (if (and (> (length cc) 0) (char= (char cc 0) #\/))
            (try-path cc)
            ;; Search PATH directories
            (loop for start = 0 then (1+ end)
                  for end = (position #\: path-env :start start)
                  for dir = (subseq path-env start (or end (length path-env)))
                  when (> (length dir) 0)
                  do (let ((result (try-path (format nil "~A/~A" dir cc))))
                       (when result (return result)))
                  while end))))))

(defun find-resource-dir-from-binary (clang-path)
  "Derive the clang resource directory from the binary path without forking.
   Handles standard layouts (lib/clang/VERSION/).
   Returns the resource dir include path, or NIL."
  (when clang-path
    (let* ((real (truename clang-path))
           (dir (directory-namestring real)))
      ;; Walk up from the bin/ directory to the prefix
      (when (and dir (> (length dir) 4))
        (let ((prefix (if (string= "bin/" (subseq dir (- (length dir) 4)))
                          (subseq dir 0 (- (length dir) 4))
                          ;; Try parent of whatever directory contains clang
                          (let ((parent (directory-namestring
                                         (make-pathname :directory
                                           (butlast (pathname-directory real))))))
                            (when parent parent)))))
          (when prefix
            ;; NixOS layout: <prefix>/resource-root/include/
            (let ((nix-inc (format nil "~Aresource-root/include" prefix)))
              (when (probe-file (format nil "~A/stdint.h" nix-inc))
                (return-from find-resource-dir-from-binary nix-inc)))
            ;; Standard layout: <prefix>/lib/clang/<version>/include/
            (let ((lib-clang (format nil "~Alib/clang/" prefix)))
              (when (probe-file lib-clang)
                (let ((entries (directory (format nil "~A*/" lib-clang))))
                  (dolist (entry entries)
                    (let ((inc (format nil "~Ainclude" (namestring entry))))
                      (when (probe-file (format nil "~A/stdint.h" inc))
                        (return-from find-resource-dir-from-binary inc)))))))))))))

(defun detect-system-include-paths ()
  "Detect system include paths needed for libclang to resolve standard headers.
   Derives the clang resource directory from the binary path on disk (no fork).
   Results are cached in *system-include-paths*."
  (when (eq *system-include-paths* :undetected)
    (setf *system-include-paths*
          (handler-case
              (let ((paths nil))
                ;; Get resource directory without forking -- fork() after
                ;; libclang index creation corrupts libclang's internal state.
                (let ((resource-inc (find-resource-dir-from-binary
                                      (find-clang-binary))))
                  (when resource-inc
                    (push resource-inc paths)))
                ;; Check well-known system header locations
                (dolist (candidate '("/usr/include"
                                     "/usr/local/include"))
                  (when (and (probe-file (format nil "~A/stdio.h" candidate))
                             (not (member candidate paths :test #'string=)))
                    (push candidate paths)))
                ;; NixOS: find glibc-dev via C_INCLUDE_PATH
                (let ((c-inc (sb-ext:posix-getenv "C_INCLUDE_PATH")))
                  (when c-inc
                    (dolist (dir (loop for start = 0 then (1+ end)
                                      for end = (position #\: c-inc :start start)
                                      collect (subseq c-inc start (or end (length c-inc)))
                                      while end))
                      (when (and (> (length dir) 0)
                                 (probe-file dir)
                                 (not (member dir paths :test #'string=)))
                        (push dir paths)))))
                (nreverse paths))
            (error () nil))))
  (if (eq *system-include-paths* :undetected)
      nil
      *system-include-paths*))

(defun parse-header (header-path &key include-paths defines
                                      (options +cxtranslationunit-skip-function-bodies+))
  "Convenience function to parse a C header file.
   Automatically includes system header paths (clang resource dir, glibc)
   so that #include <stdint.h> and similar resolve correctly.
   OPTIONS defaults to +cxtranslationunit-skip-function-bodies+.
   Use +cxtranslationunit-detailed-preprocessing-record+ to extract macros."
  (unless (libclang-available-p)
    (error "libclang not available"))
  ;; Detect system include paths BEFORE creating the index.
  ;; sb-ext:run-program (used by detect) calls fork(), which corrupts
  ;; libclang's internal state if an index already exists.
  (let* ((sys-paths (detect-system-include-paths))
         (index (or *default-index*
                    (setf *default-index* (create-index))))
         (args (append
                (mapcar (lambda (p) (format nil "-isystem~A" p)) sys-paths)
                (mapcar (lambda (p) (format nil "-I~A" p)) include-paths)
                (mapcar (lambda (d)
                          (if (consp d)
                              (format nil "-D~A=~A" (car d) (cdr d))
                              (format nil "-D~A" d)))
                        defines)
                '("-x" "c"))))
    (parse-translation-unit index header-path
                           :command-line-args args
                           :options options)))

(defun parse-header-string (code &key include-paths defines)
  "Parse C code from a string."
  (let ((temp-file (format nil "~A/epsilon-clang-~A.h"
                           (or (sb-ext:posix-getenv "TMPDIR")
                               (sb-ext:posix-getenv "TMP")
                               "/tmp")
                           (random 1000000))))
    (unwind-protect
        (progn
          (with-open-file (out temp-file :direction :output :if-exists :supersede)
            (write-string code out))
          (parse-header temp-file :include-paths include-paths :defines defines))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defmacro with-parsed-header ((tu-var header-path &key include-paths defines options) &body body)
  "Parse a header and execute body with the translation unit bound to TU-VAR.
   OPTIONS can be a translation unit option constant like
   +cxtranslationunit-detailed-preprocessing-record+ for macro extraction."
  `(let ((,tu-var (parse-header ,header-path
                                :include-paths ,include-paths
                                :defines ,defines
                                ,@(when options `(:options ,options)))))
     (unwind-protect
         (progn ,@body)
       (when ,tu-var
         (dispose-translation-unit ,tu-var)))))

;;; ============================================================================
;;; Cursor Operations
;;; ============================================================================

(defun get-translation-unit-cursor (tu)
  "Get the cursor for the root of the translation unit."
  (with-sbcl-signal-handlers
    (clang-gettranslationunitcursor tu)))

(defun cursor-null-p (cursor)
  "Check if cursor is null."
  (with-sbcl-signal-handlers
    (not (zerop (clang-cursor-isnull cursor)))))

(defun cursor-kind (cursor)
  "Get the kind of a cursor as an integer."
  (with-sbcl-signal-handlers
    (clang-getcursorkind cursor)))

(defun cursor-kind-spelling (kind)
  "Get the string name of a cursor kind."
  (with-sbcl-signal-handlers
    (cxstring-to-string (clang-getcursorkindspelling kind))))

(defun cursor-spelling (cursor)
  "Get the name/spelling of a cursor."
  (with-sbcl-signal-handlers
    (cxstring-to-string (clang-getcursorspelling cursor))))

(defun cursor-type (cursor)
  "Get the type of a cursor."
  (with-sbcl-signal-handlers
    (clang-getcursortype cursor)))

(defun cursor-location (cursor)
  "Get the source location of a cursor."
  (with-sbcl-signal-handlers
    (clang-getcursorlocation cursor)))

(defun cursor-extent (cursor)
  "Get the source range of a cursor."
  (with-sbcl-signal-handlers
    (clang-getcursorextent cursor)))

(defun cursor-evaluate (cursor)
  "Evaluate a cursor as a compile-time constant expression.
   Returns (values value kind) where:
   - value is the evaluated integer (for int) or float (for float) or nil
   - kind is :int, :float, or nil if evaluation failed

   Useful for evaluating variable initializers like 'const int x = 42;'"
  (with-sbcl-signal-handlers
    (let ((result (clang-cursor-evaluate cursor)))
      (when (and result (not (sb-sys:sap= result (sb-sys:int-sap 0))))
        (unwind-protect
             (let ((kind (clang-evalresult-getkind result)))
               (cond
                 ((= kind +cxeval-int+)
                  (values (clang-evalresult-getaslonglong result) :int))
                 ((= kind +cxeval-float+)
                  (values (clang-evalresult-getasdouble result) :float))
                 (t
                  (values nil nil))))
          (clang-evalresult-dispose result))))))

;;; ============================================================================
;;; Child Visitor Callback Support
;;; ============================================================================
;;;
;;; The with-alien-callable trampoline for struct-by-value callbacks is
;;; vulnerable to GC corruption: if GC runs during the C->Lisp callback,
;;; the trampoline or its stack frame can be relocated, causing memory faults.
;;;
;;; Fix: wrap the entire visit-children call in sb-sys:without-gcing so the
;;; trampoline cannot be disturbed.  The traversals are short (tens of nodes)
;;; so deferring GC for the duration is acceptable.

(defvar *visitor-callback* nil
  "Current visitor callback function")

(sb-alien:define-alien-routine "clang_visitChildren"
    sb-alien:unsigned-int
  (parent (sb-alien:struct cxcursor))
  (visitor sb-alien:system-area-pointer)
  (client-data sb-alien:system-area-pointer))

(defun visit-children (cursor callback)
  "Visit all children of CURSOR, calling CALLBACK with each child cursor.

   CALLBACK receives a single argument (the child cursor) and should return:
   - +cxchildvisit-break+ (0) to stop visiting
   - +cxchildvisit-continue+ (1) to continue to siblings
   - +cxchildvisit-recurse+ (2) to recurse into children"
  (let ((saved-callback *visitor-callback*))
    (setf *visitor-callback* callback)
    (unwind-protect
         (sb-sys:without-gcing
           (with-sbcl-signal-handlers
             (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
               (sb-alien:with-alien-callable
                   ((visitor-fn sb-alien:int
                                ((child (sb-alien:struct cxcursor))
                                 (_parent (sb-alien:struct cxcursor))
                                 (_client-data sb-alien:system-area-pointer))
                                (handler-case
                                    (progn
                                      _parent _client-data
                                      (let ((result (funcall (the function *visitor-callback*) child)))
                                        (if (integerp result) result +cxchildvisit-continue+)))
                                  (error ()
                                    +cxchildvisit-break+))))
                 (clang-visitchildren cursor
                                      (sb-alien:alien-sap visitor-fn)
                                      (sb-sys:int-sap 0))))))
      (setf *visitor-callback* saved-callback))))

;;; ============================================================================
;;; Type Operations
;;; ============================================================================

(defun type-kind (cxtype)
  "Get the kind of a type as an integer."
  (sb-alien:slot cxtype 'kind))

(defun type-spelling (cxtype)
  "Get the string representation of a type."
  (with-sbcl-signal-handlers
    (cxstring-to-string (clang-gettypespelling cxtype))))

(defun type-size (cxtype)
  "Get the size of a type in bytes. Returns -1 on error."
  (with-sbcl-signal-handlers
    (clang-type-getsizeof cxtype)))

(defun type-alignment (cxtype)
  "Get the alignment of a type in bytes. Returns -1 on error."
  (with-sbcl-signal-handlers
    (clang-type-getalignof cxtype)))

(defun type-canonical (cxtype)
  "Get the canonical (underlying) type."
  (with-sbcl-signal-handlers
    (clang-getcanonicaltype cxtype)))

(defun type-pointee (cxtype)
  "Get the pointee type for a pointer type."
  (with-sbcl-signal-handlers
    (clang-getpointeetype cxtype)))

(defun type-result (cxtype)
  "Get the result type of a function type."
  (with-sbcl-signal-handlers
    (clang-getresulttype cxtype)))

(defun type-num-arg-types (cxtype)
  "Get the number of argument types for a function type."
  (with-sbcl-signal-handlers
    (clang-getnumargtypes cxtype)))

(defun type-arg-type (cxtype index)
  "Get the argument type at INDEX for a function type."
  (with-sbcl-signal-handlers
    (clang-getargtype cxtype index)))

;;; ============================================================================
;;; Type Kind to Keyword Mapping
;;; ============================================================================

(defun type-kind-to-keyword (kind)
  "Convert a CXTypeKind integer to a keyword."
  (case kind
    (#.+cxtype-void+ :void)
    (#.+cxtype-bool+ :bool)
    (#.+cxtype-char-u+ :char)
    (#.+cxtype-char-s+ :char)
    (#.+cxtype-schar+ :signed-char)
    (#.+cxtype-uchar+ :unsigned-char)
    (#.+cxtype-short+ :short)
    (#.+cxtype-ushort+ :unsigned-short)
    (#.+cxtype-int+ :int)
    (#.+cxtype-uint+ :unsigned-int)
    (#.+cxtype-long+ :long)
    (#.+cxtype-ulong+ :unsigned-long)
    (#.+cxtype-longlong+ :long-long)
    (#.+cxtype-ulonglong+ :unsigned-long-long)
    (#.+cxtype-float+ :float)
    (#.+cxtype-double+ :double)
    (#.+cxtype-longdouble+ :long-double)
    (#.+cxtype-pointer+ :pointer)
    (#.+cxtype-record+ :record)
    (#.+cxtype-enum+ :enum)
    (#.+cxtype-typedef+ :typedef)
    (#.+cxtype-functionproto+ :function)
    (#.+cxtype-constantarray+ :array)
    (#.+cxtype-elaborated+ :elaborated)
    (otherwise :unknown)))

;;; ============================================================================
;;; High-Level Extraction Functions
;;; ============================================================================

(defun extract-struct-info (tu struct-name)
  "Extract information about a struct from a translation unit.
   Returns a plist with :name, :size, :alignment, :fields."
  (let ((root (get-translation-unit-cursor tu))
        (result nil))
    (visit-children
     root
     (lambda (cursor)
       (let ((kind (cursor-kind cursor)))
         (when (and (= kind +cxcursor-struct-decl+)
                    (string= (cursor-spelling cursor) struct-name))
           (let ((ctype (cursor-type cursor))
                 (fields nil))
             ;; Visit struct fields
             (visit-children
              cursor
              (lambda (field-cursor)
                (when (= (cursor-kind field-cursor) +cxcursor-field-decl+)
                  (let ((field-type (cursor-type field-cursor)))
                    (push (list :name (cursor-spelling field-cursor)
                                :type (type-spelling field-type)
                                :type-kind (type-kind-to-keyword (type-kind field-type))
                                :offset (clang-cursor-getoffsetoffield field-cursor)
                                :size (type-size field-type))
                          fields)))
                +cxchildvisit-continue+))
             (setf result
                   (list :name struct-name
                         :size (type-size ctype)
                         :alignment (type-alignment ctype)
                         :fields (nreverse fields))))
           +cxchildvisit-break+))
       +cxchildvisit-continue+))
    result))

(defun extract-function-info (tu function-name)
  "Extract information about a function from a translation unit.
   Returns a plist with :name, :return-type, :params.
   Uses cursor-based parameter iteration to preserve typedef names
   (int64_t, uint32_t, etc.) rather than canonical types."
  (let ((root (get-translation-unit-cursor tu))
        (result nil))
    (visit-children
     root
     (lambda (cursor)
       (let ((kind (cursor-kind cursor)))
         (when (and (= kind +cxcursor-function-decl+)
                    (string= (cursor-spelling cursor) function-name))
           (let* ((ctype (cursor-type cursor))
                  (ret-type (type-result ctype))
                  (params nil))
             ;; Iterate over child cursors to find ParmDecl entries.
             ;; This preserves typedef names (int64_t, uint32_t) that
             ;; clang_getArgType resolves to canonical platform types.
             (visit-children
              cursor
              (lambda (child)
                (when (= (cursor-kind child) +cxcursor-parm-decl+)
                  (let ((param-type (cursor-type child)))
                    (push (list :type (type-spelling param-type)
                                :type-kind (type-kind-to-keyword (type-kind param-type)))
                          params)))
                +cxchildvisit-continue+))
             (setf result
                   (list :name function-name
                         :return-type (type-spelling ret-type)
                         :return-type-kind (type-kind-to-keyword (type-kind ret-type))
                         :params (nreverse params))))
           ;; Signal libclang to stop visiting -- do NOT use return-from
           ;; here as it would perform a non-local exit through C frames,
           ;; leaving libclang's internal state corrupted.
           +cxchildvisit-break+))
       +cxchildvisit-continue+))
    result))

(defun extract-enum-info (tu enum-name)
  "Extract information about an enum from a translation unit.
   Returns a plist with :name, :constants."
  (let ((root (get-translation-unit-cursor tu))
        (result nil))
    (visit-children
     root
     (lambda (cursor)
       (let ((kind (cursor-kind cursor)))
         (when (and (= kind +cxcursor-enum-decl+)
                    (string= (cursor-spelling cursor) enum-name))
           (let ((constants nil))
             (visit-children
              cursor
              (lambda (const-cursor)
                (when (= (cursor-kind const-cursor) +cxcursor-enum-constant-decl+)
                  (push (list :name (cursor-spelling const-cursor)
                              :value (clang-getenumconstantdeclvalue const-cursor))
                        constants))
                +cxchildvisit-continue+))
             (setf result
                   (list :name enum-name
                         :constants (nreverse constants))))
           +cxchildvisit-break+))
       +cxchildvisit-continue+))
    result))

(defun extract-typedef-info (tu typedef-name)
  "Extract information about a typedef from a translation unit.
   Returns a plist with :name, :underlying-type, :underlying-type-kind,
   :underlying-canonical, and size/alignment if applicable."
  (let ((root (get-translation-unit-cursor tu))
        (result nil))
    (visit-children
     root
     (lambda (cursor)
       (let ((kind (cursor-kind cursor)))
         (when (and (= kind +cxcursor-typedef-decl+)
                    (string= (cursor-spelling cursor) typedef-name))
           (let* ((underlying (clang-gettypedefdeclunderlyingtype cursor))
                  (canonical (type-canonical underlying))
                  (underlying-kind (type-kind underlying))
                  (canonical-kind (type-kind canonical)))
             (setf result
                   (list :name typedef-name
                         :underlying-type (type-spelling underlying)
                         :underlying-type-kind (type-kind-to-keyword underlying-kind)
                         :canonical-type (type-spelling canonical)
                         :canonical-type-kind (type-kind-to-keyword canonical-kind)
                         :size (let ((s (type-size canonical)))
                                 (if (< s 0) nil s))
                         :alignment (let ((a (type-alignment canonical)))
                                      (if (< a 0) nil a)))))
           +cxchildvisit-break+))
       +cxchildvisit-continue+))
    result))

(defun extract-all-declarations (tu)
  "Extract all top-level declarations from a translation unit.
   Returns a list of plists, each with :kind and :name."
  (let ((root (get-translation-unit-cursor tu))
        (results nil))
    (visit-children
     root
     (lambda (cursor)
       (let ((kind (cursor-kind cursor))
             (name (cursor-spelling cursor)))
         (when (and name (> (length name) 0))
           (cond
             ((= kind +cxcursor-struct-decl+)
              (push (list :kind :struct :name name) results))
             ((= kind +cxcursor-union-decl+)
              (push (list :kind :union :name name) results))
             ((= kind +cxcursor-enum-decl+)
              (push (list :kind :enum :name name) results))
             ((= kind +cxcursor-function-decl+)
              (push (list :kind :function :name name) results))
             ((= kind +cxcursor-typedef-decl+)
              (push (list :kind :typedef :name name) results))
             ((= kind +cxcursor-var-decl+)
              (push (list :kind :variable :name name) results)))))
       +cxchildvisit-continue+))
    (nreverse results)))

;;; ============================================================================
;;; Token API FFI Bindings
;;; ============================================================================
;;;
;;; The token API allows extracting raw tokens from source ranges,
;;; which is needed to get macro body text.

;; CXToken: 24 bytes
;; - int_data: unsigned int[4] (16 bytes)
;; - ptr_data: void* (8 bytes)
(sb-alien:define-alien-type nil
  (sb-alien:struct cxtoken
    (int-data0 sb-alien:unsigned-int)
    (int-data1 sb-alien:unsigned-int)
    (int-data2 sb-alien:unsigned-int)
    (int-data3 sb-alien:unsigned-int)
    (ptr-data sb-alien:system-area-pointer)))

(defconstant +cxtoken-size+ 24)

(sb-alien:define-alien-routine "clang_tokenize" sb-alien:void
  (tu sb-alien:system-area-pointer)
  (range (sb-alien:struct cxsourcerange))
  (tokens (* sb-alien:system-area-pointer))
  (num-tokens (* sb-alien:unsigned-int)))

;; Token functions take CXToken by value
(sb-alien:define-alien-routine "clang_getTokenKind" sb-alien:int
  (token (sb-alien:struct cxtoken)))

(sb-alien:define-alien-routine "clang_getTokenSpelling"
    (sb-alien:struct cxstring)
  (tu sb-alien:system-area-pointer)
  (token (sb-alien:struct cxtoken)))

(sb-alien:define-alien-routine "clang_getTokenExtent"
    (sb-alien:struct cxsourcerange)
  (tu sb-alien:system-area-pointer)
  (token (sb-alien:struct cxtoken)))

(sb-alien:define-alien-routine "clang_disposeTokens" sb-alien:void
  (tu sb-alien:system-area-pointer)
  (tokens sb-alien:system-area-pointer)
  (num-tokens sb-alien:unsigned-int))

;; Check if cursor is a macro definition
(sb-alien:define-alien-routine "clang_Cursor_isMacroFunctionLike" sb-alien:int
  (cursor (sb-alien:struct cxcursor)))

(sb-alien:define-alien-routine "clang_Cursor_isMacroBuiltin" sb-alien:int
  (cursor (sb-alien:struct cxcursor)))

;;; ============================================================================
;;; Token Helper Functions
;;; ============================================================================

(defun tokenize-range (tu range)
  "Tokenize a source range. Returns (values token-ptr num-tokens).
   Caller must call dispose-tokens when done."
  (let ((tokens-ptr (sb-alien:make-alien sb-alien:system-area-pointer))
        (num-tokens-ptr (sb-alien:make-alien sb-alien:unsigned-int)))
    (clang-tokenize tu range tokens-ptr num-tokens-ptr)
    (let ((tokens (sb-alien:deref tokens-ptr))
          (num (sb-alien:deref num-tokens-ptr)))
      (sb-alien:free-alien tokens-ptr)
      (sb-alien:free-alien num-tokens-ptr)
      (values tokens num))))

(defun get-token-at-index (tokens index)
  "Get token at index in token array as an alien struct.
   Returns an alien value that can be passed to token functions."
  (sb-alien:sap-alien (sb-sys:sap+ tokens (* index +cxtoken-size+))
                      (sb-alien:struct cxtoken)))

(defun token-spelling (tu token)
  "Get the spelling (text) of a token."
  (with-sbcl-signal-handlers
    (cxstring-to-string (clang-gettokenspelling tu token))))

(defun token-kind (token)
  "Get the kind of a token."
  (with-sbcl-signal-handlers
    (clang-gettokenkind token)))

(defun token-kind-name (kind)
  "Get human-readable name for token kind."
  (case kind
    (#.+cxtoken-punctuation+ :punctuation)
    (#.+cxtoken-keyword+ :keyword)
    (#.+cxtoken-identifier+ :identifier)
    (#.+cxtoken-literal+ :literal)
    (#.+cxtoken-comment+ :comment)
    (otherwise :unknown)))

(defun tokens-to-string (tu tokens num-tokens)
  "Convert tokens to a single string (the macro body)."
  (with-output-to-string (out)
    (dotimes (i num-tokens)
      (let ((token (get-token-at-index tokens i)))
        (when (> i 0)
          (write-char #\Space out))
        (write-string (token-spelling tu token) out)))))

(defun dispose-tokens (tu tokens num-tokens)
  "Dispose of tokens allocated by tokenize-range."
  (when (and tokens (not (sb-sys:sap= tokens (sb-sys:int-sap 0))))
    (clang-disposetokens tu tokens num-tokens)))

;;; ============================================================================
;;; Macro Utilities
;;; ============================================================================

(defun macro-definition-p (cursor)
  "Check if cursor is a macro definition."
  (= (cursor-kind cursor) +cxcursor-macro-definition+))

(defun macro-function-like-p (cursor)
  "Check if macro is function-like (has parameters)."
  (with-sbcl-signal-handlers
    (not (zerop (clang-cursor-ismacrofunctionlike cursor)))))

(defun macro-builtin-p (cursor)
  "Check if macro is a compiler builtin."
  (with-sbcl-signal-handlers
    (not (zerop (clang-cursor-ismacrobuiltin cursor)))))

;;; ============================================================================
;;; Macro Extraction Functions
;;; ============================================================================

(defun get-macro-body-tokens (tu cursor)
  "Get the tokens that make up a macro body.
   Returns list of plists: ((:kind :spelling) ...)"
  (let* ((extent (cursor-extent cursor))
         (result nil))
    (multiple-value-bind (tokens num-tokens)
        (tokenize-range tu extent)
      (when (and tokens (> num-tokens 0))
        (unwind-protect
            (progn
              ;; Skip the macro name (first token) and collect the rest
              (loop for i from 1 below num-tokens
                    for token = (get-token-at-index tokens i)
                    for kind = (token-kind token)
                    for spelling = (token-spelling tu token)
                    ;; Skip opening paren of function-like macros in params
                    collect (list :kind (token-kind-name kind)
                                  :spelling spelling)
                    into body-tokens
                    finally (setf result body-tokens)))
          (dispose-tokens tu tokens num-tokens))))
    result))

(defun extract-macro-info (tu macro-name)
  "Extract information about a specific macro.
   Returns plist with :name :is-function-like :is-builtin :body-tokens :body-string"
  (let ((root (get-translation-unit-cursor tu))
        (result nil))
    (visit-children
     root
     (lambda (cursor)
       (when (and (macro-definition-p cursor)
                  (string= (cursor-spelling cursor) macro-name))
         (let ((body-tokens (get-macro-body-tokens tu cursor)))
           (setf result
                 (list :name macro-name
                       :is-function-like (macro-function-like-p cursor)
                       :is-builtin (macro-builtin-p cursor)
                       :body-tokens body-tokens
                       :body-string (format nil "~{~A~^ ~}"
                                           (mapcar (lambda (tok) (getf tok :spelling))
                                                   body-tokens)))))
         +cxchildvisit-break+)
       +cxchildvisit-continue+))
    result))

(defun extract-macro-definitions (tu &key prefix (include-builtins nil))
  "Extract all macro definitions from a translation unit.
   TU must be parsed with +cxtranslationunit-detailed-preprocessing-record+.
   Returns list of plists with :name :is-function-like :is-builtin :body-string"
  (let ((root (get-translation-unit-cursor tu))
        (results nil))
    (visit-children
     root
     (lambda (cursor)
       (when (macro-definition-p cursor)
         (let ((name (cursor-spelling cursor))
               (is-builtin (macro-builtin-p cursor)))
           (when (and name
                      (> (length name) 0)
                      (or include-builtins (not is-builtin))
                      (or (null prefix)
                          (and (>= (length name) (length prefix))
                               (string= prefix name :end2 (length prefix)))))
             (let ((body-tokens (get-macro-body-tokens tu cursor)))
               (push (list :name name
                          :is-function-like (macro-function-like-p cursor)
                          :is-builtin is-builtin
                          :body-string (format nil "~{~A~^ ~}"
                                              (mapcar (lambda (tok) (getf tok :spelling))
                                                      body-tokens)))
                     results)))))
       +cxchildvisit-continue+))
    (nreverse results)))

;;; ============================================================================
;;; Cleanup
;;; ============================================================================

(defun cleanup ()
  "Clean up libclang resources."
  (when *default-index*
    (dispose-index *default-index*)
    (setf *default-index* nil))
  (setf *libclang* nil
        *libclang-loaded* nil))

;;; ============================================================================
;;; Module Initialization
;;; ============================================================================

;; Auto-load libclang when first needed
(defun ensure-libclang ()
  "Ensure libclang is loaded. Called automatically by functions that need it."
  (unless *libclang-loaded*
    (load-libclang))
  (unless *libclang-loaded*
    (error "libclang not available. Check that libclang is installed.")))
