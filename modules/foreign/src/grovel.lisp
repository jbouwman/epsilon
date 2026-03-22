;;;; grovel.lisp - C Header Groveling
;;;;
;;;; High-level interface for extracting type information from C headers
;;;; using libclang.

(defpackage epsilon.foreign.grovel
  (:use cl)
  (:local-nicknames
   (clang epsilon.foreign.libclang)
   (types epsilon.foreign.type-database)
   (map epsilon.map)
   (log epsilon.log))
  (:export
   #:*default-include-paths*
   #:*default-defines*
   #:*system-headers*
   #:grovel-header
   #:grovel-headers
   #:grovel-header-string
   #:grovel-struct
   #:grovel-function
   #:grovel-enum
   #:grovel-typedef
   #:populate-database
   #:grovel-to-database)
  (:enter t))

;;; Configuration

(defvar *default-include-paths*
  '("/usr/include"
    "/usr/local/include"
    "/usr/include/x86_64-linux-gnu")
  "Default system include paths for header parsing")

(defvar *default-defines* nil
  "Default preprocessor definitions")

(defvar *system-headers*
  '("stdio.h" "stdlib.h" "string.h" "stdint.h" "stdbool.h"
    "sys/types.h" "sys/stat.h" "fcntl.h" "unistd.h"
    "errno.h" "time.h" "math.h" "limits.h")
  "Common system headers that can be groveled")

;;; Conversion from libclang plists to type-database structs

(defun plist-to-struct-info (plist)
  "Convert a struct plist from libclang to c-type-info."
  (when plist
    (types:make-c-type-info
     :name (getf plist :name)
     :kind :struct
     :size (getf plist :size)
     :alignment (getf plist :alignment)
     :fields (mapcar #'plist-to-field-info (getf plist :fields)))))

(defun plist-to-field-info (plist)
  "Convert a field plist to c-field-info."
  (types:make-c-field-info
   :name (getf plist :name)
   :type (getf plist :type)
   :type-kind (getf plist :type-kind)
   :offset (getf plist :offset)
   :size (getf plist :size)
   :is-pointer (eq (getf plist :type-kind) :pointer)))

(defun plist-to-function-info (plist)
  "Convert a function plist from libclang to c-type-info."
  (when plist
    (types:make-c-type-info
     :name (getf plist :name)
     :kind :function
     :return-type (getf plist :return-type)
     :param-types (mapcar (lambda (p) (getf p :type)) (getf plist :params)))))

(defun plist-to-enum-info (plist)
  "Convert an enum plist from libclang to c-type-info."
  (when plist
    (types:make-c-type-info
     :name (getf plist :name)
     :kind :enum
     :size 4  ; enums are typically int-sized
     :alignment 4
     :constants (mapcar (lambda (c)
                          (types:make-c-enum-constant
                           :name (getf c :name)
                           :value (getf c :value)))
                        (getf plist :constants)))))

;;; Header Parsing

(defun grovel-header (header-name &key include-paths defines database)
  "Parse a C header and extract all type information.

   HEADER-NAME: Path to header file or system header name
   INCLUDE-PATHS: Additional include directories
   DEFINES: Preprocessor definitions
   DATABASE: Type database to populate (uses global if nil)

   Returns the number of types extracted."
  (declare (ignore database))
  (unless (clang:libclang-available-p)
    (log:warn "libclang not available, cannot grovel headers")
    (return-from grovel-header 0))

  (let* ((all-includes (append include-paths *default-include-paths*))
         (all-defines (append defines *default-defines*))
         (tu (clang:parse-header header-name
                                 :include-paths all-includes
                                 :defines all-defines)))
    (unless tu
      (log:warn "Failed to parse header: ~A" header-name)
      (return-from grovel-header 0))

    (unwind-protect
        (let ((count 0))
          (log:debug "Groveling header: ~A" header-name)
          ;; Get all declarations
          (let ((decls (clang:extract-all-declarations tu)))
            (dolist (decl decls)
              (let* ((kind (getf decl :kind))
                     (name (getf decl :name))
                     (info (case kind
                             (:struct (plist-to-struct-info
                                       (clang:extract-struct-info tu name)))
                             (:function (plist-to-function-info
                                         (clang:extract-function-info tu name)))
                             (:enum (plist-to-enum-info
                                     (clang:extract-enum-info tu name)))
                             (otherwise nil))))
                (when info
                  (types:register-type info)
                  (incf count)))))
          count)
      (clang:dispose-translation-unit tu))))

(defun grovel-headers (header-names &key include-paths defines database)
  "Parse multiple C headers and extract type information.
   Returns total number of types extracted."
  (let ((total 0))
    (dolist (header header-names total)
      (incf total (grovel-header header
                                 :include-paths include-paths
                                 :defines defines
                                 :database database)))))

(defun grovel-header-string (code &key include-paths defines database)
  "Parse C code from a string and extract type information."
  (declare (ignore database))
  (unless (clang:libclang-available-p)
    (log:warn "libclang not available")
    (return-from grovel-header-string 0))

  (let ((tu (clang:parse-header-string code
                                       :include-paths include-paths
                                       :defines defines)))
    (unless tu
      (return-from grovel-header-string 0))
    (unwind-protect
        (let ((count 0))
          (let ((decls (clang:extract-all-declarations tu)))
            (dolist (decl decls)
              (let* ((kind (getf decl :kind))
                     (name (getf decl :name))
                     (info (case kind
                             (:struct (plist-to-struct-info
                                       (clang:extract-struct-info tu name)))
                             (:function (plist-to-function-info
                                         (clang:extract-function-info tu name)))
                             (:enum (plist-to-enum-info
                                     (clang:extract-enum-info tu name)))
                             (otherwise nil))))
                (when info
                  (types:register-type info)
                  (incf count)))))
          count)
      (clang:dispose-translation-unit tu))))

;;; Targeted Extraction

(defun grovel-struct (struct-name header-name &key include-paths defines)
  "Extract information for a specific struct from a header.
   Returns a c-type-info or NIL if not found."
  (unless (clang:libclang-available-p)
    (return-from grovel-struct nil))

  (let ((tu (clang:parse-header header-name
                                :include-paths (append include-paths *default-include-paths*)
                                :defines defines)))
    (unless tu
      (return-from grovel-struct nil))

    (unwind-protect
        (plist-to-struct-info (clang:extract-struct-info tu struct-name))
      (clang:dispose-translation-unit tu))))

(defun grovel-function (function-name header-name &key include-paths defines)
  "Extract information for a specific function from a header.
   Returns a c-type-info or NIL if not found."
  (unless (clang:libclang-available-p)
    (return-from grovel-function nil))

  (let ((tu (clang:parse-header header-name
                                :include-paths (append include-paths *default-include-paths*)
                                :defines defines)))
    (unless tu
      (return-from grovel-function nil))

    (unwind-protect
        (plist-to-function-info (clang:extract-function-info tu function-name))
      (clang:dispose-translation-unit tu))))

(defun grovel-enum (enum-name header-name &key include-paths defines)
  "Extract information for a specific enum from a header.
   Returns a c-type-info or NIL if not found."
  (unless (clang:libclang-available-p)
    (return-from grovel-enum nil))

  (let ((tu (clang:parse-header header-name
                                :include-paths (append include-paths *default-include-paths*)
                                :defines defines)))
    (unless tu
      (return-from grovel-enum nil))

    (unwind-protect
        (plist-to-enum-info (clang:extract-enum-info tu enum-name))
      (clang:dispose-translation-unit tu))))

(defun grovel-typedef (typedef-name header-name &key include-paths defines)
  "Extract information for a specific typedef from a header.
   Returns a c-type-info or NIL if not found.

   The returned c-type-info has:
   - :name - Typedef name
   - :kind - :typedef
   - :base-type - Canonical underlying type (string)
   - :size - Size in bytes (if applicable)
   - :alignment - Alignment in bytes (if applicable)"
  (unless (clang:libclang-available-p)
    (return-from grovel-typedef nil))

  (let ((tu (clang:parse-header header-name
                                :include-paths (append include-paths *default-include-paths*)
                                :defines defines)))
    (unless tu
      (return-from grovel-typedef nil))

    (unwind-protect
        (let ((info (clang:extract-typedef-info tu typedef-name)))
          (when info
            (types:make-c-type-info
             :name (getf info :name)
             :kind :typedef
             :size (getf info :size)
             :alignment (getf info :alignment)
             ;; Use canonical type as base-type for typedef resolution
             :base-type (getf info :canonical-type))))
      (clang:dispose-translation-unit tu))))

;;; Database Population

(defun populate-database (headers &key include-paths defines clear)
  "Populate the type database from a list of headers.

   HEADERS: List of header file paths
   INCLUDE-PATHS: Additional include directories
   DEFINES: Preprocessor definitions
   CLEAR: If true, clear database before populating

   Returns statistics about extracted types."
  (when clear
    (types:clear-database))

  (let ((total-types 0))
    (dolist (header headers)
      (incf total-types (grovel-header header
                                       :include-paths include-paths
                                       :defines defines)))

    (log:info "Populated database with ~D types from ~D headers"
              total-types (length headers))

    (types:database-statistics)))

;;; Convenience Functions

(defun grovel-to-database (header-name struct-names &key include-paths defines)
  "Grovel specific structs from a header and add to database.

   HEADER-NAME: Path to header file
   STRUCT-NAMES: List of struct names to extract

   Returns list of successfully extracted type names."
  (let ((extracted nil))
    (dolist (name struct-names (nreverse extracted))
      (let ((info (grovel-struct name header-name
                                 :include-paths include-paths
                                 :defines defines)))
        (when info
          (types:register-type info)
          (push name extracted))))))
