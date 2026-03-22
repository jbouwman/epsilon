;;;; auto-struct.lisp - Automatic Struct Generation from C Headers
;;;;
;;;; Generates Epsilon struct definitions from groveled C type information.

(defpackage epsilon.foreign.auto-struct
  (:use cl)
  (:local-nicknames
   (types epsilon.foreign.type-database)
   (grovel epsilon.foreign.grovel)
   (fs epsilon.file)
   (log epsilon.log))
  (:export
   #:generate-struct-definition
   #:define-c-struct
   #:define-c-struct-from-header
   #:generate-field-accessor
   #:generate-struct-accessors
   #:generate-bindings-to-file
   #:generate-bindings-for-library
   #:list-available-structs
   #:list-available-functions)
  (:enter t))

;;; Struct Generation

(defun generate-struct-definition (type-info)
  "Generate an epsilon struct definition from c-type-info.
   Returns a defstruct form."
  (unless (eq (types:c-type-info-kind type-info) :struct)
    (error "Expected struct type, got ~A" (types:c-type-info-kind type-info)))

  (let* ((name (intern (string-upcase (types:c-type-info-name type-info))))
         (fields (types:c-type-info-fields type-info))
         (slot-specs (mapcar #'field-to-slot-spec fields)))
    `(defstruct ,name
       ,@slot-specs)))

(defun field-to-slot-spec (field-info)
  "Convert a c-field-info to a defstruct slot specification."
  (let* ((name (intern (string-upcase (types:c-field-info-name field-info))))
         (type-kind (types:c-field-info-type-kind field-info))
         (lisp-type (c-type-kind-to-lisp-type type-kind
                                              (types:c-field-info-is-pointer field-info))))
    `(,name nil :type ,lisp-type)))

(defun c-type-kind-to-lisp-type (kind is-pointer)
  "Convert C type kind to Lisp type specifier."
  (if is-pointer
      't  ; Pointers are represented as SAPs or integers
      (case kind
        (:void 'null)
        (:bool 'boolean)
        (:char 'character)
        ((:short :int :long :long-long) 'integer)
        ((:ushort :uint :ulong :ulong-long) '(integer 0))
        ((:float :double) 'real)
        (:record 't)  ; Nested structs
        (:array 'vector)
        (otherwise 't))))

;;; Macro for Compile-Time Groveling

(defmacro define-c-struct (name header-name &key include-paths defines)
  "Define a struct by groveling it from a C header at compile time.

   Example:
     (define-c-struct stat \"sys/stat.h\")
     (define-c-struct timespec \"time.h\")"
  (let ((type-info (grovel:grovel-struct (string-downcase (symbol-name name))
                                         header-name
                                         :include-paths include-paths
                                         :defines defines)))
    (if type-info
        (generate-struct-definition type-info)
        `(progn
           (warn "Could not grovel struct ~A from ~A" ',name ,header-name)
           nil))))

(defmacro define-c-struct-from-header (struct-name header-name
                                       &key include-paths defines)
  "Alias for define-c-struct for clarity."
  `(define-c-struct ,struct-name ,header-name
                    :include-paths ,include-paths
                    :defines ,defines))

;;; FFI Accessor Generation

(defun generate-field-accessor (struct-name field-info)
  "Generate an FFI accessor function for a struct field."
  (let* ((field-name (types:c-field-info-name field-info))
         (accessor-name (intern (format nil "~A-~A"
                                        (string-upcase struct-name)
                                        (string-upcase field-name))))
         (offset (floor (types:c-field-info-offset field-info) 8))
         (type-kind (types:c-field-info-type-kind field-info))
         (alien-type (types:ffi-type-to-sbcl-alien
                      (type-kind-to-ffi-type type-kind))))
    `(defun ,accessor-name (ptr)
       "Access field from struct pointer."
       (sb-alien:deref (sb-alien:sap-alien
                        (sb-sys:sap+ ptr ,offset)
                        (* ,alien-type))))))

(defun type-kind-to-ffi-type (kind)
  "Convert type kind to FFI type."
  (case kind
    (:void :void)
    (:bool :bool)
    (:char :char)
    (:short :short)
    (:int :int)
    (:long :long)
    (:long-long :long-long)
    (:float :float)
    (:double :double)
    (:pointer :pointer)
    (otherwise :int)))

(defun generate-struct-accessors (type-info)
  "Generate all field accessors for a struct."
  (let ((struct-name (types:c-type-info-name type-info))
        (fields (types:c-type-info-fields type-info)))
    `(progn
       ,@(mapcar (lambda (f) (generate-field-accessor struct-name f))
                 fields))))

;;; Binding File Generation

(defun generate-bindings-to-file (output-file header-name library-name
                                  &key structs functions include-paths defines)
  "Generate FFI bindings and write to a Lisp source file.

   OUTPUT-FILE: Path to output file
   HEADER-NAME: C header to parse
   LIBRARY-NAME: Name of shared library
   STRUCTS: List of struct names to extract (or :all)
   FUNCTIONS: List of function names to extract (or :all)
   INCLUDE-PATHS: Additional include directories
   DEFINES: Preprocessor definitions"
  (declare (ignore functions)) ; TODO: Implement function binding generation

  (with-open-file (out output-file
                       :direction :output
                       :if-exists :supersede)
    (format out ";;;; Auto-generated FFI bindings~%")
    (format out ";;;; Source: ~A~%" header-name)
    (format out ";;;; Generated: ~A~%" (get-universal-time))
    (format out "~%")

    ;; Package definition
    (let ((pkg-name (fs:strip-extension (fs:basename output-file))))
      (format out "(defpackage ~A~%" pkg-name)
      (format out "  (:use cl)~%")
      (format out "  (:export~%")
      (format out "   ;; Struct accessors will be listed here~%")
      (format out "   ))~%~%")
      (format out "(in-package ~A)~%~%" pkg-name))

    ;; Library loading
    (format out ";;; Library Loading~%~%")
    (format out "(defvar *~A* nil)~%" library-name)
    (format out "(defun load-~A ()~%" library-name)
    (format out "  (setf *~A* (sb-alien:load-shared-object \"~A\")))~%~%"
            library-name library-name)

    ;; Struct definitions
    (format out ";;; Struct Definitions~%~%")
    (let ((struct-list (if (eq structs :all)
                           nil  ; TODO: extract all structs
                           structs)))
      (dolist (struct-name struct-list)
        (let ((type-info (grovel:grovel-struct struct-name header-name
                                               :include-paths include-paths
                                               :defines defines)))
          (when type-info
            (format out "~S~%~%" (generate-struct-definition type-info))
            (format out "~S~%~%" (generate-struct-accessors type-info))))))

    (log:info "Generated bindings to ~A" output-file)))

;;; Batch Processing

(defun generate-bindings-for-library (library-spec output-dir)
  "Generate bindings for a complete library specification.

   LIBRARY-SPEC: Plist with :name :headers :structs :functions :includes :defines
   OUTPUT-DIR: Directory for generated files"
  (let* ((name (getf library-spec :name))
         (headers (getf library-spec :headers))
         (structs (getf library-spec :structs))
         (functions (getf library-spec :functions))
         (includes (getf library-spec :includes))
         (defines (getf library-spec :defines))
         (output-file (format nil "~A/~A-bindings.lisp" output-dir name)))
    (ensure-directories-exist output-file)
    (dolist (header headers)
      (generate-bindings-to-file output-file header name
                                 :structs structs
                                 :functions functions
                                 :include-paths includes
                                 :defines defines))))

;;; Utilities

(defun list-available-structs (header-name &key include-paths defines)
  "List all structs defined in a header."
  (let ((tu (epsilon.foreign.libclang:parse-header header-name
                                                    :include-paths include-paths
                                                    :defines defines)))
    (when tu
      (unwind-protect
          (let ((decls (epsilon.foreign.libclang:extract-all-declarations tu)))
            (mapcar (lambda (d) (getf d :name))
                    (remove-if-not (lambda (d) (eq (getf d :kind) :struct)) decls)))
        (epsilon.foreign.libclang:dispose-translation-unit tu)))))

(defun list-available-functions (header-name &key include-paths defines)
  "List all functions declared in a header."
  (let ((tu (epsilon.foreign.libclang:parse-header header-name
                                                    :include-paths include-paths
                                                    :defines defines)))
    (when tu
      (unwind-protect
          (let ((decls (epsilon.foreign.libclang:extract-all-declarations tu)))
            (mapcar (lambda (d) (getf d :name))
                    (remove-if-not (lambda (d) (eq (getf d :kind) :function)) decls)))
        (epsilon.foreign.libclang:dispose-translation-unit tu)))))
