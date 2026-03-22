;;;; Module schema validation
;;;; Validates module.plist files with error accumulation.

(defpackage epsilon.module-schema
  (:use :cl)
  (:require (epsilon.file fs)
            (epsilon.path path))
  (:export validate-module-metadata)
  (:enter t))

(defvar *valid-keys*
  '(:name :module-set :description :author :platform
    :sources :tests :benchmarks :examples :experiments
    :docs :data :requires :provides :source-type
    :resources :integration :commands :main :shape)
  "Recognized keys in module.plist files.")

;;;; Internal helpers

(defun check-plist-p (metadata)
  "True if METADATA is a well-formed property list."
  (and (listp metadata)
       (evenp (length metadata))
       (loop for key in metadata by #'cddr
             always (keywordp key))))

(defun check-string-list-p (value)
  "True if VALUE is a list of strings."
  (and (listp value) (every #'stringp value)))

(defun check-non-empty-string-list-p (value)
  "True if VALUE is a list of non-empty strings."
  (and (check-string-list-p value)
       (every (lambda (s) (> (length s) 0)) value)))

(defun check-no-whitespace-p (str)
  "True if STR contains no whitespace."
  (not (find-if (lambda (c)
                  (member c '(#\Space #\Tab #\Newline #\Return)))
                str)))

(defun check-unique-p (list)
  "True if LIST has no duplicates."
  (= (length list) (length (remove-duplicates list :test #'equal))))

(defun check-directory-exists (base-path dir-name)
  "True if DIR-NAME exists as a directory under BASE-PATH."
  (let* ((full-path (path:path-join base-path dir-name))
         (path-str (path:path-string full-path)))
    (and (probe-file path-str)
         (fs:dir-p path-str))))

;;;; Main validator

(defun validate-module-metadata (metadata filepath)
  "Validate module.plist metadata with error accumulation.
   Returns (values valid-p errors) where errors is a list of error strings."
  (let ((errors '())
        (base-path (path:path-parent (path:make-path filepath))))
    (flet ((fail (field message)
             (push (if field
                       (format nil "~A: ~A" field message)
                       message)
                   errors)))

      ;; Stage 1: Structure -- must be a plist to continue
      (unless (check-plist-p metadata)
        (fail nil (format nil "Invalid module.plist format in ~A: must be a property list"
                          filepath))
        (return-from validate-module-metadata
          (values nil (nreverse errors))))

      ;; Stage 2: Unknown keys (non-fatal)
      (loop for key in metadata by #'cddr
            unless (member key *valid-keys*)
              do (fail (string key)
                       (format nil "Unknown key ~A. Valid keys are: ~{~A~^, ~}"
                               key *valid-keys*)))

      ;; Stage 3: Field validation
      ;; :name -- required, string, no whitespace
      (let ((name (getf metadata :name)))
        (cond
          ((null name)
           (fail "name" "is required"))
          ((not (stringp name))
           (fail "name" "must be a string"))
          ((= 0 (length name))
           (fail "name" "cannot be empty"))
          ((not (check-no-whitespace-p name))
           (fail "name" "must be a valid module name (no whitespace)"))))

      ;; :description -- optional string
      (let ((description (getf metadata :description)))
        (when (and description (not (stringp description)))
          (fail "description" "must be a string")))

      ;; :author -- optional string
      (let ((author (getf metadata :author)))
        (when (and author (not (stringp author)))
          (fail "author" "must be a string")))

      ;; :platform -- optional string
      (let ((platform (getf metadata :platform)))
        (when (and platform (not (stringp platform)))
          (fail "platform" "must be a string")))

      ;; :shape -- optional string (content-addressed shape name)
      (let ((shape (getf metadata :shape)))
        (when (and shape (not (stringp shape)))
          (fail "shape" "must be a string")))

      ;; String-list fields: sources, tests, benchmarks, examples, experiments, docs, data
      (dolist (key '(:sources :tests :benchmarks :examples :experiments :docs :data))
        (let ((value (getf metadata key)))
          (when value
            (cond
              ((not (listp value))
               (fail (string-downcase (string key)) "must be a list"))
              ((not (check-string-list-p value))
               (fail (string-downcase (string key)) "all entries must be strings"))
              ((not (check-unique-p value))
               (fail (string-downcase (string key)) "must contain unique items"))))))

      ;; :requires -- optional, list of non-empty strings, unique
      (let ((requires (getf metadata :requires)))
        (when requires
          (cond
            ((not (listp requires))
             (fail "requires" "must be a list"))
            ((not (check-non-empty-string-list-p requires))
             (fail "requires" "all requirements must be non-empty strings"))
            ((not (check-unique-p requires))
             (fail "requires" "must contain unique items")))))

      ;; :provides -- optional, list of non-empty strings, unique
      (let ((provides (getf metadata :provides)))
        (when provides
          (cond
            ((not (listp provides))
             (fail "provides" "must be a list"))
            ((not (check-non-empty-string-list-p provides))
             (fail "provides" "all provided packages must be non-empty strings"))
            ((not (check-unique-p provides))
             (fail "provides" "must contain unique items")))))

      ;; :commands -- optional list of command declarations
      (let ((commands (getf metadata :commands)))
        (when commands
          (cond
            ((not (listp commands))
             (fail "commands" "must be a list"))
            (t
             (dolist (cmd commands)
               (cond
                 ((not (listp cmd))
                  (fail "commands" "each entry must be a plist"))
                 (t
                  (let ((cmd-name (getf cmd :name))
                        (handler (getf cmd :handler)))
                    (unless (and cmd-name (stringp cmd-name))
                      (fail "commands" ":name is required and must be a string"))
                    (unless (and handler (stringp handler))
                      (fail "commands" ":handler is required and must be a string"))
                    (when (and handler (stringp handler)
                               (not (position #\: handler)))
                      (fail "commands"
                            (format nil ":handler ~A must be package:function format"
                                    handler)))))))))))

      ;; Stage 4: Cross-field -- sources/tests must not overlap
      (let ((sources (getf metadata :sources))
            (tests (getf metadata :tests)))
        (when (and (listp sources) (listp tests))
          (let ((common (intersection sources tests :test #'equal)))
            (when common
              (fail "sources/tests"
                    (format nil "directories must not overlap, found: ~{~A~^, ~}"
                            common))))))

      ;; Stage 5: Filesystem -- referenced directories must exist
      (dolist (key '(:sources :tests))
        (let ((dirs (getf metadata key)))
          (when (and (listp dirs) (every #'stringp dirs))
            (dolist (dir dirs)
              (unless (check-directory-exists base-path dir)
                (fail (format nil "~A" (string-downcase (string key)))
                      (format nil "directory '~A' does not exist" dir))))))))

    (values (null errors) (nreverse errors))))
