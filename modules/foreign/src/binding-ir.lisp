;;;; binding-ir.lisp - Binding Intermediate Representation
;;;;
;;;; This module defines a persistent, human-readable intermediate representation
;;;; for parsed C header information. The BIR (Binding IR) format enables:
;;;;
;;;; 1. Compile-time header groveling with persistent output
;;;; 2. Portable binding definitions across platforms
;;;; 3. Version-aware format for future compatibility
;;;; 4. Human-readable S-expression format for inspection/debugging
;;;;
;;;; Usage:
;;;;   ;; Development: Generate BIR from headers
;;;;   (grovel-to-file "/usr/include/libpq-fe.h" "bindings/libpq.bir.lisp"
;;;;                   :prefix "PQ")
;;;;
;;;;   ;; Load time: Use BIR file
;;;;   (load-binding-ir "bindings/libpq.bir.lisp")
;;;;
;;;; See docs/implement/182_epsilon-ffi-binding-improvement.md for architecture.

(defpackage epsilon.foreign.binding-ir
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (lc epsilon.foreign.libclang)
   (ab epsilon.foreign.auto-binding)
   (sigs epsilon.foreign.signatures)
   (grovel epsilon.foreign.grovel)
   (log epsilon.log)
   (fs epsilon.file))
  (:export
   ;; BIR Format Constants
   #:+bir-version+
   #:+bir-magic+

   ;; BIR Data Structures
   #:make-binding-ir
   #:binding-ir-p
   #:binding-ir-version
   #:binding-ir-source-headers
   #:binding-ir-parse-time
   #:binding-ir-platform
   #:binding-ir-functions
   #:binding-ir-structs
   #:binding-ir-enums
   #:binding-ir-typedefs
   #:binding-ir-macros

   ;; Function Info
   #:make-bir-function
   #:bir-function-p
   #:bir-function-name
   #:bir-function-return-type
   #:bir-function-params
   #:bir-function-variadic-p
   #:bir-function-header
   #:bir-function-line

   ;; Param Info
   #:make-bir-param
   #:bir-param-p
   #:bir-param-name
   #:bir-param-type

   ;; Struct Info
   #:make-bir-struct
   #:bir-struct-p
   #:bir-struct-name
   #:bir-struct-size
   #:bir-struct-alignment
   #:bir-struct-fields
   #:bir-struct-opaque-p

   ;; Field Info
   #:make-bir-field
   #:bir-field-p
   #:bir-field-name
   #:bir-field-type
   #:bir-field-offset
   #:bir-field-size

   ;; Enum Info
   #:make-bir-enum
   #:bir-enum-p
   #:bir-enum-name
   #:bir-enum-values

   ;; Enum Value Info
   #:make-bir-enum-value
   #:bir-enum-value-p
   #:bir-enum-value-name
   #:bir-enum-value-value

   ;; Typedef Info
   #:make-bir-typedef
   #:bir-typedef-p
   #:bir-typedef-name
   #:bir-typedef-underlying

   ;; Macro Info
   #:make-bir-macro
   #:bir-macro-p
   #:bir-macro-name
   #:bir-macro-kind
   #:bir-macro-value
   #:bir-macro-target

   ;; File I/O
   #:write-binding-ir
   #:read-binding-ir
   #:load-binding-ir

   ;; Generation from headers
   #:grovel-to-ir
   #:grovel-to-file

   ;; Validation
   #:validate-binding-ir
   #:bir-compatible-p

   ;; Code Generation
   #:generate-defshared-forms
   #:generate-struct-forms

   ;; Integration Macros
   #:define-library-from-ir
   #:with-binding-ir

   ;; Constant and Struct Utilities
   #:define-constants-from-bir
   #:define-struct-offsets-from-bir
   #:define-enum-constants-from-bir
   #:get-bir-constant-value
   #:get-bir-struct-field-offset
   #:get-bir-struct-size
   #:get-bir-enum-value

   ;; CLI Entry Point
   #:grovel)
  (:enter t))

;;; ============================================================================
;;; BIR Format Constants
;;; ============================================================================

(defconstant +bir-version+ 1
  "Current version of the Binding IR format.
   Increment when making incompatible changes to the format.")

(defparameter +bir-magic+ :binding-ir
  "Magic keyword identifying BIR files.")

;;; ============================================================================
;;; Platform Detection
;;; ============================================================================

(defun current-platform ()
  "Return a keyword identifying the current platform."
  (let* ((machine (machine-type))
         (os #+linux :linux
             #+darwin :darwin
             #+windows :windows
             #-(or linux darwin windows) :unknown))
    (intern (format nil "~A-~A" os machine) :keyword)))

;;; ============================================================================
;;; BIR Data Structures
;;; ============================================================================

(defstruct binding-ir
  "Root structure for Binding Intermediate Representation."
  (version +bir-version+ :type integer)
  (source-headers nil :type list)
  (parse-time (get-universal-time) :type integer)
  (platform (current-platform) :type keyword)
  (include-paths nil :type list)
  (defines nil :type list)
  (functions nil :type list)
  (structs nil :type list)
  (enums nil :type list)
  (typedefs nil :type list)
  (macros nil :type list))

(defstruct bir-function
  "Function signature information."
  (name "" :type string)
  (return-type :void :type keyword)
  (params nil :type list)
  (variadic-p nil :type boolean)
  (header nil :type (or string null))
  (line 0 :type integer))

(defstruct bir-param
  "Function parameter information."
  (name "" :type string)
  (type :pointer :type keyword))

(defstruct bir-struct
  "Struct layout information."
  (name "" :type string)
  (size 0 :type integer)
  (alignment 1 :type integer)
  (fields nil :type list)
  (opaque-p nil :type boolean))

(defstruct bir-field
  "Struct field information."
  (name "" :type string)
  (type :pointer :type keyword)
  (offset 0 :type integer)
  (size 0 :type integer))

(defstruct bir-enum
  "Enum type information."
  (name "" :type string)
  (values nil :type list))

(defstruct bir-enum-value
  "Enum constant information."
  (name "" :type string)
  (value 0 :type integer))

(defstruct bir-typedef
  "Typedef information."
  (name "" :type string)
  (underlying "" :type string))

(defstruct bir-macro
  "Macro definition information."
  (name "" :type string)
  (kind :constant :type keyword)
  (value nil)
  (target nil :type (or string null)))

;;; ============================================================================
;;; Type Conversion
;;; ============================================================================

(defun libclang-type-to-bir-type (type-kind)
  "Convert libclang type kind to BIR type keyword."
  (case type-kind
    (:void :void)
    (:bool :bool)
    (:char :char)
    (:signed-char :schar)
    (:unsigned-char :uchar)
    (:short :short)
    (:unsigned-short :ushort)
    (:int :int)
    (:unsigned-int :uint)
    (:long :long)
    (:unsigned-long :ulong)
    (:long-long :llong)
    (:unsigned-long-long :ullong)
    (:float :float)
    (:double :double)
    (:long-double :ldouble)
    (:pointer :pointer)
    (:record :pointer)  ; struct/union passed as pointer
    (:enum :int)
    (otherwise :pointer)))

;;; ============================================================================
;;; Generation from Headers
;;; ============================================================================

(defun grovel-to-ir (header-paths &key prefix macro-prefix include-paths defines (include-macros t))
  "Parse headers and generate a Binding IR structure.

   HEADER-PATHS - List of header file paths to parse
   PREFIX - Only include declarations starting with this prefix
   MACRO-PREFIX - Prefix for macro constants (defaults to PREFIX if not specified)
   INCLUDE-PATHS - Additional include directories
   DEFINES - Preprocessor definitions
   INCLUDE-MACROS - If true (default), extract and evaluate macro constants

   Returns a binding-ir structure."
  (unless (lc:libclang-available-p)
    (error "libclang not available for header groveling"))

  (let ((ir (make-binding-ir
             :source-headers (if (listp header-paths) header-paths (list header-paths))
             :include-paths include-paths
             :defines defines))
        (functions nil)
        (structs nil)
        (enums nil)
        (typedefs nil)
        (macros nil))

    (dolist (header-path (binding-ir-source-headers ir))
      (lc:with-parsed-header (tu header-path
                                 :include-paths include-paths
                                 :defines defines)
        (let ((decls (lc:extract-all-declarations tu)))
          (dolist (decl decls)
            (let ((name (getf decl :name))
                  (kind (getf decl :kind)))
              (when (and name
                         (or (null prefix)
                             (and (>= (length name) (length prefix))
                                  (string= prefix name :end2 (length prefix)))))
                (case kind
                  (:function
                   (let ((sig (lc:extract-function-info tu name)))
                     (when sig
                       (push (convert-function-sig sig header-path) functions))))
                  (:struct
                   (let ((info (lc:extract-struct-info tu name)))
                     (when info
                       (push (convert-struct-info info) structs))))
                  (:enum
                   (let ((info (lc:extract-enum-info tu name)))
                     (when info
                       (push (convert-enum-info info) enums))))
                  (:typedef
                   (let ((info (lc:extract-typedef-info tu name)))
                     (when info
                       (push (convert-typedef-info info) typedefs)))))))))))

    ;; Extract and evaluate macro constants
    (when include-macros
      (let ((effective-macro-prefix (or macro-prefix prefix)))
        (dolist (header-path (binding-ir-source-headers ir))
          (handler-case
              (let ((evaluated (ab:evaluate-header-macro-constants
                                header-path
                                :prefix effective-macro-prefix
                                :include-paths include-paths
                                :defines defines)))
                (dolist (entry evaluated)
                  (push (make-bir-macro
                         :name (car entry)
                         :kind :constant
                         :value (cdr entry))
                        macros)))
            (error (e)
              ;; Log warning but don't fail - macros are optional
              (log:warn "Failed to extract macros from ~A: ~A" header-path e))))))

    (setf (binding-ir-functions ir) (nreverse functions))
    (setf (binding-ir-structs ir) (nreverse structs))
    (setf (binding-ir-enums ir) (nreverse enums))
    (setf (binding-ir-typedefs ir) (nreverse typedefs))
    (setf (binding-ir-macros ir) (nreverse macros))

    ir))

(defun convert-function-sig (sig header-path)
  "Convert libclang function signature to bir-function."
  (make-bir-function
   :name (getf sig :name)
   :return-type (libclang-type-to-bir-type (getf sig :return-type-kind))
   :params (mapcar (lambda (p)
                     (make-bir-param
                      :name (or (getf p :name) "")
                      :type (libclang-type-to-bir-type (getf p :type-kind))))
                   (getf sig :params))
   :variadic-p (getf sig :variadic)
   :header header-path
   :line (or (getf sig :line) 0)))

(defun convert-struct-info (info)
  "Convert libclang struct info to bir-struct.
   Handles incomplete/opaque types where libclang returns negative error codes:
   -2 = CXTypeLayoutError_Incomplete (forward declaration)"
  (let ((size (getf info :size))
        (alignment (getf info :alignment))
        (fields (getf info :fields)))
    ;; Negative sizes indicate libclang errors (e.g., -2 = incomplete type)
    (let ((is-opaque (or (null fields)
                         (and size (< size 0)))))
      (make-bir-struct
       :name (getf info :name)
       :size (if (and size (>= size 0)) size 0)
       :alignment (if (and alignment (>= alignment 0)) alignment 0)
       :fields (mapcar (lambda (f)
                         (make-bir-field
                          :name (or (getf f :name) "")
                          :type (libclang-type-to-bir-type (getf f :type-kind))
                          :offset (floor (or (getf f :offset) 0) 8)  ; bits to bytes
                          :size (or (getf f :size) 0)))
                       fields)
       :opaque-p is-opaque))))

(defun convert-enum-info (info)
  "Convert libclang enum info to bir-enum."
  (make-bir-enum
   :name (getf info :name)
   :values (mapcar (lambda (c)
                     (make-bir-enum-value
                      :name (or (getf c :name) "")
                      :value (or (getf c :value) 0)))
                   (getf info :constants))))

(defun convert-typedef-info (info)
  "Convert libclang typedef info to bir-typedef."
  (make-bir-typedef
   :name (getf info :name)
   :underlying (or (getf info :canonical-type)
                   (getf info :underlying-type)
                   "")))

;;; ============================================================================
;;; File I/O - Writing
;;; ============================================================================

(defun write-binding-ir (ir stream)
  "Write a binding-ir structure to a stream in S-expression format.

   The format is human-readable and editable."
  (format stream ";;; Epsilon Binding IR File~%")
  (format stream ";;; Generated: ~A~%" (format-time (binding-ir-parse-time ir)))
  (format stream ";;; Platform: ~A~%" (binding-ir-platform ir))
  (format stream ";;; Source: ~{~A~^, ~}~%~%"
          (binding-ir-source-headers ir))

  (format stream "(~S~%" +bir-magic+)
  (format stream " :version ~D~%" (binding-ir-version ir))
  (format stream " :source-headers ~S~%" (binding-ir-source-headers ir))
  (format stream " :parse-time ~D~%" (binding-ir-parse-time ir))
  (format stream " :platform ~S~%" (binding-ir-platform ir))
  (format stream " :include-paths ~S~%" (binding-ir-include-paths ir))
  (format stream " :defines ~S~%~%" (binding-ir-defines ir))

  ;; Functions
  (format stream " :functions~%")
  (format stream " (~%")
  (dolist (fn (binding-ir-functions ir))
    (write-bir-function fn stream))
  (format stream " )~%~%")

  ;; Structs
  (format stream " :structs~%")
  (format stream " (~%")
  (dolist (st (binding-ir-structs ir))
    (write-bir-struct st stream))
  (format stream " )~%~%")

  ;; Enums
  (format stream " :enums~%")
  (format stream " (~%")
  (dolist (en (binding-ir-enums ir))
    (write-bir-enum en stream))
  (format stream " )~%~%")

  ;; Typedefs
  (format stream " :typedefs~%")
  (format stream " (~%")
  (dolist (td (binding-ir-typedefs ir))
    (write-bir-typedef td stream))
  (format stream " )~%~%")

  ;; Macros
  (format stream " :macros~%")
  (format stream " (~%")
  (dolist (mc (binding-ir-macros ir))
    (write-bir-macro mc stream))
  (format stream " ))~%"))

(defun write-bir-function (fn stream)
  "Write a bir-function to stream."
  (format stream "  (:name ~S~%" (bir-function-name fn))
  (format stream "   :return-type ~S~%" (bir-function-return-type fn))
  (format stream "   :params (~{~S~^ ~})~%" (mapcar #'bir-param-to-plist (bir-function-params fn)))
  (format stream "   :variadic ~S~%" (bir-function-variadic-p fn))
  (format stream "   :header ~S~%" (bir-function-header fn))
  (format stream "   :line ~D)~%" (bir-function-line fn)))

(defun bir-param-to-plist (param)
  "Convert bir-param to a plist for serialization."
  (list :name (bir-param-name param)
        :type (bir-param-type param)))

(defun write-bir-struct (st stream)
  "Write a bir-struct to stream."
  (format stream "  (:name ~S~%" (bir-struct-name st))
  (format stream "   :size ~D~%" (bir-struct-size st))
  (format stream "   :alignment ~D~%" (bir-struct-alignment st))
  (format stream "   :opaque ~S~%" (bir-struct-opaque-p st))
  (format stream "   :fields (~%")
  (dolist (f (bir-struct-fields st))
    (format stream "    (:name ~S :type ~S :offset ~D :size ~D)~%"
            (bir-field-name f) (bir-field-type f)
            (bir-field-offset f) (bir-field-size f)))
  (format stream "   ))~%"))

(defun write-bir-enum (en stream)
  "Write a bir-enum to stream."
  (format stream "  (:name ~S~%" (bir-enum-name en))
  (format stream "   :values (~%")
  (dolist (v (bir-enum-values en))
    (format stream "    (:name ~S :value ~D)~%"
            (bir-enum-value-name v) (bir-enum-value-value v)))
  (format stream "   ))~%"))

(defun write-bir-typedef (td stream)
  "Write a bir-typedef to stream."
  (format stream "  (:name ~S :underlying ~S)~%"
          (bir-typedef-name td) (bir-typedef-underlying td)))

(defun write-bir-macro (mc stream)
  "Write a bir-macro to stream."
  (format stream "  (:name ~S :kind ~S"
          (bir-macro-name mc) (bir-macro-kind mc))
  (when (bir-macro-value mc)
    (format stream " :value ~S" (bir-macro-value mc)))
  (when (bir-macro-target mc)
    (format stream " :target ~S" (bir-macro-target mc)))
  (format stream ")~%"))

(defun format-time (universal-time)
  "Format universal time as ISO 8601 string."
  (multiple-value-bind (sec min hr day mon yr)
      (decode-universal-time universal-time)
    (format nil "~4D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            yr mon day hr min sec)))

;;; ============================================================================
;;; File I/O - Reading
;;; ============================================================================

(defun read-binding-ir (stream)
  "Read a binding-ir structure from a stream."
  (let ((data (read stream)))
    (unless (and (listp data) (eq (first data) +bir-magic+))
      (error "Invalid BIR file: missing magic header"))
    (parse-binding-ir-plist (rest data))))

(defun parse-binding-ir-plist (plist)
  "Parse a BIR plist into a binding-ir structure."
  (let ((ir (make-binding-ir
             :version (getf plist :version +bir-version+)
             :source-headers (getf plist :source-headers)
             :parse-time (getf plist :parse-time (get-universal-time))
             :platform (getf plist :platform (current-platform))
             :include-paths (getf plist :include-paths)
             :defines (getf plist :defines))))

    ;; Parse functions
    (setf (binding-ir-functions ir)
          (mapcar #'parse-bir-function (getf plist :functions)))

    ;; Parse structs
    (setf (binding-ir-structs ir)
          (mapcar #'parse-bir-struct (getf plist :structs)))

    ;; Parse enums
    (setf (binding-ir-enums ir)
          (mapcar #'parse-bir-enum (getf plist :enums)))

    ;; Parse typedefs
    (setf (binding-ir-typedefs ir)
          (mapcar #'parse-bir-typedef (getf plist :typedefs)))

    ;; Parse macros
    (setf (binding-ir-macros ir)
          (mapcar #'parse-bir-macro (getf plist :macros)))

    ir))

(defun parse-bir-function (plist)
  "Parse a function plist into bir-function."
  (make-bir-function
   :name (getf plist :name "")
   :return-type (getf plist :return-type :void)
   :params (mapcar #'parse-bir-param (getf plist :params))
   :variadic-p (getf plist :variadic)
   :header (getf plist :header)
   :line (getf plist :line 0)))

(defun parse-bir-param (plist)
  "Parse a param plist into bir-param."
  (make-bir-param
   :name (getf plist :name "")
   :type (getf plist :type :pointer)))

(defun parse-bir-struct (plist)
  "Parse a struct plist into bir-struct."
  (make-bir-struct
   :name (getf plist :name "")
   :size (getf plist :size 0)
   :alignment (getf plist :alignment 1)
   :fields (mapcar #'parse-bir-field (getf plist :fields))
   :opaque-p (getf plist :opaque)))

(defun parse-bir-field (plist)
  "Parse a field plist into bir-field."
  (make-bir-field
   :name (getf plist :name "")
   :type (getf plist :type :pointer)
   :offset (getf plist :offset 0)
   :size (getf plist :size 0)))

(defun parse-bir-enum (plist)
  "Parse an enum plist into bir-enum."
  (make-bir-enum
   :name (getf plist :name "")
   :values (mapcar #'parse-bir-enum-value (getf plist :values))))

(defun parse-bir-enum-value (plist)
  "Parse an enum value plist into bir-enum-value."
  (make-bir-enum-value
   :name (getf plist :name "")
   :value (getf plist :value 0)))

(defun parse-bir-typedef (plist)
  "Parse a typedef plist into bir-typedef."
  (make-bir-typedef
   :name (getf plist :name "")
   :underlying (getf plist :underlying "")))

(defun parse-bir-macro (plist)
  "Parse a macro plist into bir-macro."
  (make-bir-macro
   :name (getf plist :name "")
   :kind (getf plist :kind :constant)
   :value (getf plist :value)
   :target (getf plist :target)))

;;; ============================================================================
;;; High-Level File Operations
;;; ============================================================================

(defun grovel-to-file (header-paths output-path &key prefix macro-prefix include-paths defines)
  "Parse headers and write BIR to a file.

   HEADER-PATHS - Header file path(s) to parse
   OUTPUT-PATH - Path to write the .bir.lisp file
   PREFIX - Only include declarations starting with this prefix
   MACRO-PREFIX - Prefix for macro constants (defaults to PREFIX)
   INCLUDE-PATHS - Additional include directories
   DEFINES - Preprocessor definitions

   Example:
     (grovel-to-file \"/usr/include/zlib.h\"
                     \"bindings/zlib.bir.lisp\"
                     :macro-prefix \"Z_\")"
  (let ((ir (grovel-to-ir header-paths
                          :prefix prefix
                          :macro-prefix macro-prefix
                          :include-paths include-paths
                          :defines defines)))
    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede)
      (write-binding-ir ir out))
    (log:info "Generated BIR: ~A (~D functions, ~D structs, ~D enums)"
              output-path
              (length (binding-ir-functions ir))
              (length (binding-ir-structs ir))
              (length (binding-ir-enums ir)))
    output-path))

(defun load-binding-ir (path)
  "Load a BIR file and return the binding-ir structure.

   Validates the version and platform compatibility."
  (with-open-file (in path :direction :input)
    (let ((ir (read-binding-ir in)))
      (validate-binding-ir ir)
      ir)))

;;; ============================================================================
;;; Validation
;;; ============================================================================

(defun validate-binding-ir (ir)
  "Validate a binding-ir structure.
   Signals an error if validation fails."
  (unless (binding-ir-p ir)
    (error "Invalid BIR: not a binding-ir structure"))
  (unless (<= (binding-ir-version ir) +bir-version+)
    (error "BIR version ~D is newer than supported version ~D"
           (binding-ir-version ir) +bir-version+))
  t)

(defun bir-compatible-p (ir &key platform)
  "Check if a binding-ir is compatible with the current environment.

   PLATFORM - Target platform (defaults to current platform)"
  (let ((target-platform (or platform (current-platform))))
    (and (binding-ir-p ir)
         (<= (binding-ir-version ir) +bir-version+)
         (eq (binding-ir-platform ir) target-platform))))

;;; ============================================================================
;;; Code Generation
;;; ============================================================================

(defun bir-type-to-ffi-type (bir-type)
  "Convert BIR type keyword to epsilon.foreign type keyword."
  (case bir-type
    ((:void) :void)
    ((:char :schar) :char)
    ((:uchar) :unsigned-char)
    ((:short) :short)
    ((:ushort) :unsigned-short)
    ((:int :bool) :int)
    ((:uint) :unsigned-int)
    ((:long :llong) :long)
    ((:ulong :ullong) :unsigned-long)
    ((:float) :float)
    ((:double :ldouble) :double)
    ((:pointer) :pointer)
    (otherwise :pointer)))

(defun c-name-to-lisp-name (c-name)
  "Convert C name to Lisp symbol name.
   PQconnectdb -> pq-connectdb
   SSL_CTX_new -> ssl-ctx-new"
  (with-output-to-string (out)
    (loop for i from 0 below (length c-name)
          for char = (char c-name i)
          for prev-char = (if (> i 0) (char c-name (1- i)) nil)
          for prev-prev-char = (if (> i 1) (char c-name (- i 2)) nil)
          do (cond
               ;; Underscore becomes hyphen
               ((char= char #\_)
                (write-char #\- out))
               ;; Uppercase after lowercase -> insert hyphen
               ((and prev-char
                     (upper-case-p char)
                     (lower-case-p prev-char))
                (write-char #\- out)
                (write-char (char-downcase char) out))
               ;; Lowercase after multiple uppercase (e.g., PQ|connectdb)
               ((and prev-char
                     prev-prev-char
                     (lower-case-p char)
                     (upper-case-p prev-char)
                     (upper-case-p prev-prev-char))
                (write-char #\- out)
                (write-char (char-downcase char) out))
               ;; Normal character
               (t
                (write-char (char-downcase char) out))))))

(defun generate-defshared-forms (ir library)
  "Generate defshared forms from a binding-ir.

   IR - The binding-ir structure
   LIBRARY - Library name for defshared

   Returns a list of (defshared ...) forms."
  (loop for fn in (binding-ir-functions ir)
        collect (generate-defshared-form fn library)))

(defun generate-defshared-form (fn library)
  "Generate a single defshared form from a bir-function."
  (let* ((c-name (bir-function-name fn))
         (lisp-name (intern (string-upcase (c-name-to-lisp-name c-name))))
         (return-type (bir-type-to-ffi-type (bir-function-return-type fn)))
         (params (bir-function-params fn))
         (param-forms (loop for p in params
                            for i from 0
                            collect (list (intern (format nil "ARG~D" i))
                                          (bir-type-to-ffi-type (bir-param-type p))))))
    `(epsilon.foreign:defshared ,lisp-name ,c-name ,library ,return-type
                                ,@param-forms)))

(defun generate-struct-forms (ir)
  "Generate struct definition forms from a binding-ir.

   Returns a list of struct definition forms."
  (loop for st in (binding-ir-structs ir)
        unless (bir-struct-opaque-p st)
        collect (generate-struct-form st)))

(defun generate-struct-form (st)
  "Generate a struct definition form from a bir-struct.

   Uses define-ffi-struct which creates both:
   - jit-struct definition for layout queries and field access
   - sb-alien struct type for correct struct-by-value calling conventions"
  (let* ((name (bir-struct-name st))
         (lisp-name (intern (string-upcase (c-name-to-lisp-name name))))
         (fields (bir-struct-fields st)))
    `(epsilon.foreign.jit.struct:define-ffi-struct ,lisp-name
         ,@(loop for f in fields
                 collect (list (intern (string-upcase (bir-field-name f)))
                               (bir-type-to-ffi-type (bir-field-type f)))))))

;;; ============================================================================
;;; Integration Macros
;;; ============================================================================

(defmacro define-library-from-ir (library bir-path &key exclude manual-wrappers)
  "Define FFI bindings from a pre-generated BIR file.

   LIBRARY - Library name for FFI calls
   BIR-PATH - Path to the .bir.lisp file
   EXCLUDE - List of function names to exclude
   MANUAL-WRAPPERS - Alist of (c-name . wrapper-form) for manual overrides

   Example:
     (define-library-from-ir :libpq \"bindings/libpq.bir.lisp\"
       :exclude (\"PQprint\")
       :manual-wrappers ((\"PQconnectdb\" . (defun pq-connectdb (s) ...))))

   This macro loads the BIR file at compile time and generates defshared forms
   for all functions not in the exclude list or manual-wrappers list."
  (let ((ir (load-binding-ir bir-path))
        (exclude-set (make-hash-table :test 'equal)))
    ;; Build exclude set
    (dolist (name exclude)
      (setf (gethash name exclude-set) t))
    (dolist (wrapper manual-wrappers)
      (setf (gethash (car wrapper) exclude-set) t))

    ;; Generate forms
    `(progn
       ;; Generated bindings
       ,@(loop for fn in (binding-ir-functions ir)
               for name = (bir-function-name fn)
               unless (gethash name exclude-set)
               collect (generate-defshared-form fn library))

       ;; Manual wrappers
       ,@(mapcar #'cdr manual-wrappers)

       ;; Return library name
       ',library)))

(defmacro with-binding-ir ((var bir-path) &body body)
  "Load a BIR file and bind it to VAR for use in BODY.

   This is useful for programmatic inspection or custom code generation.

   Example:
     (with-binding-ir (ir \"bindings/libpq.bir.lisp\")
       (dolist (fn (binding-ir-functions ir))
         (format t \"Function: ~A~%\" (bir-function-name fn))))"
  `(let ((,var (load-binding-ir ,bir-path)))
     ,@body))

;;; ============================================================================
;;; Constant and Struct Utilities
;;; ============================================================================

(defun macro-name-to-lisp-constant (name)
  "Convert C macro name to Lisp constant name.
   Z_OK -> +Z-OK+
   Z_BEST_COMPRESSION -> +Z-BEST-COMPRESSION+"
  (format nil "+~A+"
          (string-upcase
           (substitute #\- #\_ name))))

(defun get-bir-constant-value (ir name)
  "Get the value of a macro constant from a BIR by name.
   Returns NIL if not found."
  (loop for mc in (binding-ir-macros ir)
        when (string= (bir-macro-name mc) name)
        return (bir-macro-value mc)))

(defun get-bir-struct-size (ir struct-name)
  "Get the size of a struct from a BIR by name.
   Returns NIL if not found."
  (loop for st in (binding-ir-structs ir)
        when (string= (bir-struct-name st) struct-name)
        return (bir-struct-size st)))

(defun get-bir-struct-field-offset (ir struct-name field-name)
  "Get the offset of a struct field from a BIR.
   Returns NIL if struct or field not found."
  (loop for st in (binding-ir-structs ir)
        when (string= (bir-struct-name st) struct-name)
        return (loop for f in (bir-struct-fields st)
                     when (string= (bir-field-name f) field-name)
                     return (bir-field-offset f))))

(defmacro define-constants-from-bir (bir-path &key names prefix transform)
  "Define Lisp constants from macro values in a BIR file.

   BIR-PATH - Path to the .bir.lisp file (string literal or evaluable form)
   NAMES - If provided, only define these specific macro names
   PREFIX - If provided, only define macros starting with this prefix
   TRANSFORM - Function to transform C name to Lisp constant name
               (defaults to macro-name-to-lisp-constant)

   Example:
     (define-constants-from-bir \"bindings/zlib.bir.lisp\"
       :prefix \"Z_\")

   Generates:
     (defconstant +z-ok+ 0)
     (defconstant +z-stream-end+ 1)
     ..."
  ;; Evaluate bir-path at macro expansion time if it's not a string
  (let* ((actual-path (if (stringp bir-path)
                          bir-path
                          (eval bir-path)))
         (ir (load-binding-ir actual-path))
         (transform-fn (cond
                         ((null transform) #'macro-name-to-lisp-constant)
                         ((and (consp transform) (eq (car transform) 'lambda))
                          (compile nil transform))
                         ((and (consp transform) (eq (car transform) 'function))
                          (compile nil (cadr transform)))
                         (t transform)))
         (macros (binding-ir-macros ir)))
    `(progn
       ,@(loop for mc in macros
               for name = (bir-macro-name mc)
               for value = (bir-macro-value mc)
               when (and value  ; Has a value
                         (or (null names)
                             (member name names :test #'string=))
                         (or (null prefix)
                             (and (>= (length name) (length prefix))
                                  (string= prefix name :end2 (length prefix)))))
               collect `(defconstant ,(intern (funcall transform-fn name))
                          ,value)))))

(defun field-name-to-lisp (name)
  "Convert C field name to Lisp identifier part.
   next_in -> NEXT-IN
   avail_out -> AVAIL-OUT"
  (substitute #\- #\_ (string-upcase name)))

(defmacro define-struct-offsets-from-bir (bir-path struct-name
                                          &key prefix size-constant)
  "Define Lisp constants for struct field offsets from a BIR file.

   BIR-PATH - Path to the .bir.lisp file (string literal or evaluable form)
   STRUCT-NAME - Name of the struct in the BIR (e.g., \"z_stream_s\")
   PREFIX - Prefix for generated constant names (e.g., \"+z-stream-\")
   SIZE-CONSTANT - If provided, also define this constant for struct size

   Example:
     (define-struct-offsets-from-bir \"bindings/zlib.bir.lisp\"
       \"z_stream_s\"
       :prefix \"+z-stream-\"
       :size-constant +z-stream-size+)

   Generates:
     (defconstant +z-stream-size+ 112)
     (defconstant +z-stream-next-in-offset+ 0)
     (defconstant +z-stream-avail-in-offset+ 8)
     ..."
  ;; Evaluate bir-path at macro expansion time if it's not a string
  (let* ((actual-path (if (stringp bir-path)
                          bir-path
                          (eval bir-path)))
         (ir (load-binding-ir actual-path))
         (st (find struct-name (binding-ir-structs ir)
                   :key #'bir-struct-name :test #'string=)))
    (unless st
      (error "Struct ~S not found in BIR ~A" struct-name bir-path))
    (let ((prefix-str (or prefix
                          (format nil "+~A-"
                                  (string-upcase
                                   (substitute #\- #\_ struct-name))))))
      `(progn
         ;; Size constant if requested
         ,@(when size-constant
             `((defconstant ,size-constant ,(bir-struct-size st))))
         ;; Field offsets
         ,@(loop for f in (bir-struct-fields st)
                 for field-name = (bir-field-name f)
                 for const-name = (intern
                                   (string-upcase
                                    (format nil "~A~A-offset+"
                                            prefix-str
                                            (field-name-to-lisp field-name))))
                 collect `(defconstant ,const-name ,(bir-field-offset f)))))))

(defun get-bir-enum-value (ir enum-name value-name)
  "Get the value of an enum constant from a BIR.
   ENUM-NAME is the C enum type name (e.g., \"BrotliEncoderMode\").
   VALUE-NAME is the enum constant name (e.g., \"BROTLI_MODE_TEXT\").
   Returns NIL if not found."
  (loop for en in (binding-ir-enums ir)
        when (string= (bir-enum-name en) enum-name)
        return (loop for v in (bir-enum-values en)
                     when (string= (bir-enum-value-name v) value-name)
                     return (bir-enum-value-value v))))

(defmacro define-enum-constants-from-bir (bir-path enum-name
                                          &key names prefix transform)
  "Define Lisp constants from enum values in a BIR file.

   BIR-PATH - Path to the .bir.lisp file (string literal or evaluable form)
   ENUM-NAME - Name of the enum in the BIR (e.g., \"BrotliEncoderMode\")
   NAMES - If provided, only define these specific value names
   PREFIX - Prefix to strip from C names before transformation
   TRANSFORM - Function to transform (stripped) C name to Lisp constant name
               (defaults to macro-name-to-lisp-constant)

   Example:
     (define-enum-constants-from-bir \"bindings/brotli.bir.lisp\"
       \"BrotliEncoderMode\"
       :prefix \"BROTLI_MODE_\"
       :transform (lambda (n) (format nil \"+MODE-~A+\" (substitute #\\- #\\_ n))))

   Generates:
     (defconstant +mode-generic+ 0)
     (defconstant +mode-text+ 1)
     (defconstant +mode-font+ 2)"
  ;; Evaluate bir-path at macro expansion time if it's not a string
  (let* ((actual-path (if (stringp bir-path)
                          bir-path
                          (eval bir-path)))
         (ir (load-binding-ir actual-path))
         (en (find enum-name (binding-ir-enums ir)
                   :key #'bir-enum-name :test #'string=)))
    (unless en
      (error "Enum ~S not found in BIR ~A" enum-name bir-path))
    (let ((transform-fn (cond
                          ((null transform) #'macro-name-to-lisp-constant)
                          ((and (consp transform) (eq (car transform) 'lambda))
                           (compile nil transform))
                          ((and (consp transform) (eq (car transform) 'function))
                           (compile nil (cadr transform)))
                          (t transform))))
      `(progn
         ,@(loop for v in (bir-enum-values en)
                 for name = (bir-enum-value-name v)
                 for value = (bir-enum-value-value v)
                 for stripped-name = (if (and prefix
                                              (>= (length name) (length prefix))
                                              (string= prefix name :end2 (length prefix)))
                                         (subseq name (length prefix))
                                         name)
                 when (or (null names)
                          (member name names :test #'string=))
                 collect `(defconstant ,(intern (funcall transform-fn stripped-name))
                            ,value))))))

;;; ============================================================================
;;; CLI Entry Point
;;; ============================================================================

;; TODO use packaged argument parser

(defun grovel (&rest args)
  "CLI entry point for BIR generation from C headers.

   Usage: epsilon run epsilon.foreign:grovel -- [options] HEADER OUTPUT

   Options:
     --prefix PREFIX        Only include declarations starting with PREFIX
     --macro-prefix PREFIX  Prefix for macro constants (defaults to --prefix)
     -I PATH                Add include path (can be specified multiple times)

   Examples:
     epsilon run epsilon.foreign:grovel -- /usr/include/zlib.h bindings/zlib.bir.lisp --prefix Z_"
  (let ((positional-args '())
        (prefix nil)
        (macro-prefix nil)
        (include-paths '()))

    ;; Parse arguments - collect positional args separately
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((string= arg "--prefix")
                (setf prefix (nth (1+ i) args))
                (incf i 2))
               ((string= arg "--macro-prefix")
                (setf macro-prefix (nth (1+ i) args))
                (incf i 2))
               ((or (string= arg "-I") (string= arg "--include"))
                (push (nth (1+ i) args) include-paths)
                (incf i 2))
               (t
                (push arg positional-args)
                (incf i))))

    (setf positional-args (nreverse positional-args))

    ;; Determine header-path and output-path based on mode
    (let ((header-path nil)
          (output-path nil))

      (setf header-path (first positional-args)
            output-path (second positional-args))

      ;; Validate required args
      (unless (and header-path output-path)
        (format *error-output* "Usage: epsilon run epsilon.foreign:grovel -- [options] HEADER OUTPUT~%")
        (format *error-output* "~%Options:~%")
        (format *error-output* "  --prefix PREFIX        Declaration prefix filter~%")
        (format *error-output* "  --macro-prefix PREFIX  Macro prefix filter~%")
        (format *error-output* "  -I PATH                Include path~%")
        (sb-ext:exit :code 1))

      ;; Verify header exists
      (unless (probe-file header-path)
        (format *error-output* "Error: Header file not found: ~A~%" header-path)
        (sb-ext:exit :code 1))

      ;; Ensure output directory exists
      (ensure-directories-exist output-path)

      ;; Generate BIR
      (format t "Generating BIR: ~A -> ~A~%" header-path output-path)
      (when prefix (format t "  Prefix: ~A~%" prefix))
      (when macro-prefix (format t "  Macro prefix: ~A~%" macro-prefix))
      (when include-paths (format t "  Include paths: ~{~A~^, ~}~%" (nreverse include-paths)))

      (grovel-to-file header-path output-path
                      :prefix prefix
                      :macro-prefix macro-prefix
                      :include-paths (nreverse include-paths))

      (format t "Done.~%"))))
