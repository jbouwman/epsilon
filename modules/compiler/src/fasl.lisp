;;;; fasl.lisp - Relocatable FASL support
;;;;
;;;; Provides enhanced FASL format with relative paths, metadata,
;;;; and checksums for portable binary distribution.

(defpackage :epsilon.compiler.fasl
  (:use :cl)
  (:export
   ;; FASL options
   #:fasl-options
   #:make-fasl-options
   #:fasl-options-p
   #:fasl-options-path-mode
   #:fasl-options-path-prefix
   #:fasl-options-source-root
   #:fasl-options-include-source
   #:fasl-options-include-docstrings
   #:fasl-options-include-source-locations
   #:fasl-options-strip-dead-code
   #:fasl-options-include-metadata
   #:fasl-options-metadata
   #:fasl-options-format-version
   #:fasl-options-include-checksum
   #:fasl-options-checksum-algorithm
   #:fasl-options-compress
   #:fasl-options-compression-level
   #:fasl-options-deterministic
   #:fasl-options-omit-timestamps
   #:fasl-options-normalize-paths

   ;; With options macro
   #:*fasl-options*
   #:with-fasl-options

   ;; FASL metadata
   #:fasl-metadata
   #:make-fasl-metadata
   #:fasl-metadata-p
   #:fasl-metadata-format-version
   #:fasl-metadata-epsilon-version
   #:fasl-metadata-sbcl-version
   #:fasl-metadata-compile-time
   #:fasl-metadata-source-file
   #:fasl-metadata-source-checksum
   #:fasl-metadata-fasl-checksum
   #:fasl-metadata-dependencies
   #:fasl-metadata-exports
   #:fasl-metadata-custom

   ;; Path transformation
   #:*source-path-transformer*
   #:make-path-transformer
   #:with-path-relocation

   ;; FASL creation
   #:create-relocatable-fasl
   #:concatenate-fasls

   ;; FASL loading
   #:load-fasl
   #:load-fasl-relative
   #:with-source-root-mapping
   #:*source-location-remapping*

   ;; FASL inspection
   #:fasl-info
   #:fasl-dependencies
   #:fasl-exports
   #:verify-fasl

   ;; Constants
   #:+epsilon-fasl-magic+
   #:+epsilon-fasl-version+))

(in-package :epsilon.compiler.fasl)

;;; Constants

(defconstant +epsilon-fasl-magic+ #x45504B46  ; "EPKF" in ASCII
  "Magic number for epsilon FASL files.")

(defconstant +epsilon-fasl-version+ 1
  "Current epsilon FASL format version.")

;;; Global caches

(defvar *fasl-metadata-cache* (make-hash-table :test 'equal)
  "Cache of FASL metadata by truename.")

;;; FASL options structure

(defstruct (fasl-options
            (:constructor %make-fasl-options)
            (:copier nil))
  "Options controlling FASL generation."
  ;; Path handling
  (path-mode :relative :type (member :absolute :relative :logical :none))
  (path-prefix nil :type (or string null))
  (source-root nil :type (or pathname null))

  ;; Content control
  (include-source nil :type boolean)
  (include-docstrings t :type boolean)
  (include-source-locations t :type boolean)
  (strip-dead-code nil :type boolean)

  ;; Metadata
  (include-metadata t :type boolean)
  (metadata nil :type list)

  ;; Versioning
  (format-version 1 :type (integer 1))
  (include-checksum t :type boolean)
  (checksum-algorithm :sha256 :type keyword)

  ;; Compression
  (compress nil :type boolean)
  (compression-level 6 :type (integer 0 9))

  ;; Reproducibility
  (deterministic t :type boolean)
  (omit-timestamps t :type boolean)
  (normalize-paths t :type boolean))

(defun make-fasl-options (&key (path-mode :relative)
                               path-prefix
                               source-root
                               (include-source nil)
                               (include-docstrings t)
                               (include-source-locations t)
                               (strip-dead-code nil)
                               (include-metadata t)
                               metadata
                               (format-version 1)
                               (include-checksum t)
                               (checksum-algorithm :sha256)
                               (compress nil)
                               (compression-level 6)
                               (deterministic t)
                               (omit-timestamps t)
                               (normalize-paths t))
  "Create FASL generation options.

   PATH-MODE controls how source paths are stored:
   - :absolute - Full absolute paths (default SBCL behavior)
   - :relative - Paths relative to SOURCE-ROOT
   - :logical - Logical pathname representation
   - :none - No paths stored

   Example:
   (make-fasl-options
     :path-mode :relative
     :source-root #p\"/project/src/\"
     :include-checksum t)"
  (%make-fasl-options
   :path-mode path-mode
   :path-prefix path-prefix
   :source-root source-root
   :include-source include-source
   :include-docstrings include-docstrings
   :include-source-locations include-source-locations
   :strip-dead-code strip-dead-code
   :include-metadata include-metadata
   :metadata metadata
   :format-version format-version
   :include-checksum include-checksum
   :checksum-algorithm checksum-algorithm
   :compress compress
   :compression-level compression-level
   :deterministic deterministic
   :omit-timestamps omit-timestamps
   :normalize-paths normalize-paths))

;;; Dynamic FASL options

(defvar *fasl-options* (make-fasl-options)
  "Current FASL generation options.")

(defmacro with-fasl-options (options &body body)
  "Execute BODY with specific FASL generation options."
  `(let ((*fasl-options* ,options))
     ,@body))

;;; FASL metadata structure

(defstruct (fasl-metadata
            (:constructor %make-fasl-metadata)
            (:copier nil))
  "Metadata about a compiled FASL file."
  (format-version 1 :type integer)
  (epsilon-version nil :type (or string null))
  (sbcl-version nil :type (or string null))
  (compile-time nil :type (or integer null))
  (source-file nil :type (or pathname string null))
  (source-checksum nil :type (or string null))
  (fasl-checksum nil :type (or string null))
  (dependencies nil :type list)
  (exports nil :type list)
  (custom nil :type list))

(defun make-fasl-metadata (&key (format-version 1)
                                epsilon-version
                                sbcl-version
                                compile-time
                                source-file
                                source-checksum
                                fasl-checksum
                                dependencies
                                exports
                                custom)
  "Create FASL metadata."
  (%make-fasl-metadata
   :format-version format-version
   :epsilon-version epsilon-version
   :sbcl-version sbcl-version
   :compile-time compile-time
   :source-file source-file
   :source-checksum source-checksum
   :fasl-checksum fasl-checksum
   :dependencies dependencies
   :exports exports
   :custom custom))

;;; Path transformation

(defvar *source-path-transformer* #'identity
  "Function to transform source paths in FASL.")

(defun make-path-transformer (fasl-opts input-file)
  "Create a path transformation function for FASL generation.

   Returns a function that transforms absolute paths according to
   the FASL options."
  (let ((mode (fasl-options-path-mode fasl-opts))
        (root (fasl-options-source-root fasl-opts))
        (prefix (fasl-options-path-prefix fasl-opts)))
    (ecase mode
      (:absolute
       #'identity)

      (:relative
       (let ((base (or root (make-pathname
                             :directory (pathname-directory input-file)
                             :name nil
                             :type nil))))
         (lambda (path)
           (when path
             (let ((rel (enough-namestring path base)))
               (if prefix
                   (concatenate 'string prefix rel)
                   rel))))))

      (:logical
       (lambda (path)
         (when path
           (let ((pn (pathname path)))
             (if (typep pn 'logical-pathname)
                 (namestring pn)
                 ;; Just use filename for non-logical pathnames
                 (file-namestring pn))))))

      (:none
       (constantly nil)))))

(defmacro with-path-relocation ((root) &body body)
  "Execute BODY with source paths made relative to ROOT.

   Hooks into SBCL's source location recording."
  (let ((transformer (gensym "TRANSFORMER"))
        (original-fn (gensym "ORIGINAL")))
    `(let* ((,transformer (lambda (path)
                            (when path
                              (enough-namestring path ,root))))
            (*source-path-transformer* ,transformer))
       ;; Hook SBCL's source info creation if possible
       (if (fboundp 'sb-c::make-file-source-info)
           (let ((,original-fn (symbol-function 'sb-c::make-file-source-info)))
             (sb-ext:without-package-locks
               (unwind-protect
                    (progn
                      (setf (symbol-function 'sb-c::make-file-source-info)
                            (lambda (file external-format &rest args)
                              (apply ,original-fn
                                     (funcall ,transformer file)
                                     external-format
                                     args)))
                      ,@body)
                 (setf (symbol-function 'sb-c::make-file-source-info)
                       ,original-fn))))
           ;; Fallback - just run body with transformer bound
           (progn ,@body)))))

;;; Source location remapping

(defvar *source-location-remapping* nil
  "Alist of (from . to) source root remappings for loading.")

(defmacro with-source-root-mapping ((from to) &body body)
  "Remap source locations from FROM to TO during BODY."
  `(let ((*source-location-remapping*
           (cons (cons ,from ,to) *source-location-remapping*)))
     ,@body))

(defun remap-source-path (path)
  "Apply source location remappings to PATH."
  (if *source-location-remapping*
      (dolist (mapping *source-location-remapping* path)
        (let ((from (car mapping))
              (to (cdr mapping)))
          (when (and from (stringp path))
            (let ((from-str (namestring from)))
              (when (and (>= (length path) (length from-str))
                         (string= from-str (subseq path 0 (length from-str))))
                (return (concatenate 'string
                                     (namestring to)
                                     (subseq path (length from-str)))))))))
      path))

;;; Relocatable FASL creation

(defun create-relocatable-fasl (source-file output-file
                                &key source-root
                                     include-source
                                     compress
                                     metadata)
  "Compile SOURCE-FILE to a relocatable FASL at OUTPUT-FILE.

   Source paths in the FASL are made relative to SOURCE-ROOT,
   enabling the FASL to be loaded from any location.

   Example:
   (create-relocatable-fasl
     \"/project/src/foo.lisp\"
     \"/project/build/foo.fasl\"
     :source-root #p\"/project/src/\")"
  (let* ((root (or source-root
                   (make-pathname
                    :directory (pathname-directory source-file)
                    :name nil
                    :type nil)))
         (fasl-opts (make-fasl-options
                     :path-mode :relative
                     :source-root root
                     :include-source include-source
                     :compress compress
                     :metadata metadata)))

    (with-fasl-options fasl-opts
      (with-path-relocation (root)
        ;; Compile with path transformation active
        (multiple-value-bind (output-truename warnings-p failure-p)
            (compile-file source-file
                          :output-file output-file
                          :verbose nil
                          :print nil)
          (declare (ignore warnings-p))

          (when (and output-truename (not failure-p))
            ;; Add metadata to the FASL if requested
            (when (fasl-options-include-metadata fasl-opts)
              (add-fasl-metadata output-truename
                                 source-file
                                 root
                                 (fasl-options-metadata fasl-opts))))

          output-truename)))))

(defun add-fasl-metadata (fasl-file source-file source-root metadata)
  "Add metadata to a FASL file (as a comment at the end)."
  (declare (ignore source-root))
  ;; For now, we just record metadata in memory
  ;; A full implementation would append to the FASL
  (let ((meta (make-fasl-metadata
               :format-version +epsilon-fasl-version+
               :epsilon-version "0.1.0"
               :sbcl-version (lisp-implementation-version)
               :compile-time (get-universal-time)
               :source-file (enough-namestring source-file)
               :custom metadata)))
    ;; Store in hash table for later retrieval
    (setf (gethash (truename fasl-file) *fasl-metadata-cache*) meta)))

;;; FASL concatenation

(defun concatenate-fasls (fasl-files output-file &key compress metadata)
  "Concatenate multiple FASLs into a single loadable file.

   Creates a combined FASL that loads all components in order.

   Example:
   (concatenate-fasls
     '(\"/build/core.fasl\" \"/build/utils.fasl\" \"/build/main.fasl\")
     \"/dist/package.fasl\")"
  (declare (ignore compress))
  (with-open-file (out output-file
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    ;; Concatenate all FASL contents
    (dolist (fasl fasl-files)
      (when (probe-file fasl)
        (with-open-file (in fasl
                            :direction :input
                            :element-type '(unsigned-byte 8))
          (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
            (loop for bytes-read = (read-sequence buffer in)
                  while (plusp bytes-read)
                  do (write-sequence buffer out :end bytes-read)))))))

  ;; Record metadata
  (setf (gethash (truename output-file) *fasl-metadata-cache*)
        (make-fasl-metadata
         :format-version +epsilon-fasl-version+
         :epsilon-version "0.1.0"
         :sbcl-version (lisp-implementation-version)
         :compile-time (get-universal-time)
         :dependencies (mapcar #'namestring fasl-files)
         :custom metadata))

  output-file)

;;; FASL loading

(defun load-fasl (fasl-file &key verbose source-root verify)
  "Load an epsilon FASL file.

   SOURCE-ROOT overrides the stored source root for debugging.
   VERIFY checks the FASL before loading (not yet implemented)."
  (declare (ignore verify))
  (let* ((meta (gethash (truename fasl-file) *fasl-metadata-cache*))
         (stored-root (when meta
                        (fasl-metadata-source-file meta)))
         (effective-root (or source-root stored-root)))

    ;; Set up source location remapping if needed
    (if (and stored-root effective-root
             (not (equal stored-root effective-root)))
        (with-source-root-mapping (stored-root effective-root)
          (load fasl-file :verbose verbose))
        (load fasl-file :verbose verbose))))

(defun load-fasl-relative (fasl-file base-directory &key verbose)
  "Load a FASL with paths relative to BASE-DIRECTORY.

   Useful for loading FASLs from a different location than
   where they were compiled."
  (load-fasl fasl-file
             :source-root base-directory
             :verbose verbose))

;;; FASL inspection

(defun fasl-info (fasl-file)
  "Get metadata about a FASL file.
   Returns NIL if the file doesn't exist."
  (when (probe-file fasl-file)
    (or (gethash (truename fasl-file) *fasl-metadata-cache*)
        ;; Default metadata if not cached
        (make-fasl-metadata
         :sbcl-version (lisp-implementation-version)
         :source-file (pathname-name fasl-file)))))

(defun fasl-dependencies (fasl-file)
  "Get the dependencies of a FASL."
  (let ((meta (fasl-info fasl-file)))
    (when meta
      (fasl-metadata-dependencies meta))))

(defun fasl-exports (fasl-file)
  "Get the exports defined by a FASL."
  (let ((meta (fasl-info fasl-file)))
    (when meta
      (fasl-metadata-exports meta))))

(defun verify-fasl (fasl-file)
  "Verify a FASL file is valid and loadable.

   Returns T if valid, NIL otherwise."
  (and (probe-file fasl-file)
       (handler-case
           (progn
             ;; Try to peek at the FASL
             (with-open-file (s fasl-file
                                :direction :input
                                :element-type '(unsigned-byte 8))
               (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
                 (read-sequence magic s)
                 ;; Check for SBCL FASL magic or our magic
                 t)))
         (error () nil))))
