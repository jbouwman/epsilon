;;;; Module schema validation
;;;; Defines the validation schema for module.lisp files

(defpackage epsilon.module-schema
  (:use cl)
  (:local-nicknames
   (v epsilon.validation)
   (fs epsilon.sys.fs)
   (path epsilon.path)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string))
  (:export
   ;; Schema validator
   validate-module-metadata
   
   ;; Individual validators
   validate-name
   validate-version
   validate-sources
   validate-tests
   validate-requires
   validate-provides
   
   ;; Filesystem validators
   directory-exists
   all-directories-exist
   
   ;; Cross-field validators
   no-intersection
   validate-sources-tests-disjoint))

(in-package epsilon.module-schema)

;;;; Filesystem Validators

(defun directory-exists (base-path)
  "Create a validator that checks if a directory exists relative to base-path"
  (lambda (dir-name)
    (if (stringp dir-name)
        (let* ((full-path (path:path-join base-path dir-name))
               (path-str (path:path-string full-path)))
          (if (and (probe-file path-str)
                   (fs:dir-p path-str))
              (v:make-success dir-name)
              (v:make-failure
               (list (v:make-validation-error
                      :message (format nil "directory '~A' does not exist" dir-name)
                      :type :filesystem)))))
        (v:make-failure
         (list (v:make-validation-error
                :message "must be a string path"
                :type :type-error))))))

(defun all-directories-exist (base-path)
  "Validate that all items in a list are existing directories"
  (lambda (dirs)
    (let ((dir-validator (directory-exists base-path)))
      (if (listp dirs)
          (let ((results '()))
            (loop for dir in dirs
                  for index from 0
                  do (push (v:validate dir-validator dir 
                                      (format nil "[~D]" index))
                          results))
            (let ((combined (v:combine-results (nreverse results))))
              (if (v:validation-success-p combined)
                  (v:make-success dirs)
                  combined)))
          (v:make-failure
           (list (v:make-validation-error
                  :message "must be a list"
                  :type :type-error)))))))

;;;; String List Validators

(defun string-list ()
  "Validate that value is a list of strings"
  (lambda (value)
    (cond
      ((not (listp value))
       (v:make-failure
        (list (v:make-validation-error
               :message "must be a list"
               :type :type-error))))
      ((not (every #'stringp value))
       (let ((errors '()))
         (loop for item in value
               for index from 0
               unless (stringp item)
               do (push (v:make-validation-error
                        :field (format nil "[~D]" index)
                        :message (format nil "must be a string, got ~A" 
                                        (type-of item))
                        :type :type-error)
                       errors))
         (v:make-failure (nreverse errors))))
      (t (v:make-success value)))))

;;;; Cross-field Validators

(defun no-intersection (field1-name field2-name)
  "Validate that two lists have no common elements"
  (lambda (data)
    (let ((field1 (map:get data field1-name))
          (field2 (map:get data field2-name)))
      (when (and field1 field2 (listp field1) (listp field2))
        (let ((common (intersection field1 field2 :test #'equal)))
          (if common
              (v:make-failure
               (list (v:make-validation-error
                      :field (format nil "~A/~A" field1-name field2-name)
                      :message (format nil "must not overlap, found common items: ~{~A~^, ~}" 
                                      common)
                      :type :cross-field)))
              (v:make-success data)))))))

(defun validate-sources-tests-disjoint (metadata base-path)
  "Validate that sources and tests directories don't overlap"
  (declare (ignore base-path))  ; Not used in cross-field validation
  (let ((sources (getf metadata :sources))
        (tests (getf metadata :tests)))
    (when (and sources tests)
      (let ((common (intersection sources tests :test #'equal)))
        (if common
            (v:make-failure
             (list (v:make-validation-error
                    :field "sources/tests"
                    :message (format nil "directories must not overlap, found: ~{~A~^, ~}" 
                                    common)
                    :type :cross-field)))
            (v:make-success metadata))))))

;;;; Field Validators

(defun validate-name ()
  "Validator for module name field"
  (v:chain
   #'v:required
   #'v:string-type
   (v:min-length 1)
   (v:satisfies-predicate 
    (lambda (name)
      ;; Module names should be valid package names
      (and (not (find #\Space name))
           (not (find #\Tab name))
           (not (find #\Newline name))))
    "must be a valid module name (no whitespace)")))

(defun validate-version ()
  "Validator for version field"
  (v:chain
   (v:optional #'v:string-type)
   (v:validate-when
    #'stringp
    (v:satisfies-predicate
     (lambda (version)
       ;; Simple semantic version check
       (or (string= version "")
           (find #\. version)))
     "should be a semantic version (e.g., 1.0.0)"))))

(defun validate-sources (base-path)
  "Validator for sources field"
  (v:chain
   (v:optional (string-list))
   (v:validate-when
    #'listp
    (v:all-of
     (list
      (v:unique-items)
      (all-directories-exist base-path))))))

(defun validate-tests (base-path)
  "Validator for tests field"
  (v:chain
   (v:optional (string-list))
   (v:validate-when
    #'listp
    (v:all-of
     (list
      (v:unique-items)
      (all-directories-exist base-path))))))

(defun validate-requires ()
  "Validator for requires field"
  (v:chain
   (v:optional (string-list))
   (v:validate-when
    #'listp
    (v:all-of
     (list
      (v:unique-items)
      (v:satisfies-predicate
       (lambda (requires)
         (every (lambda (req)
                  (and (stringp req)
                       (> (length req) 0)))
                requires))
       "all requirements must be non-empty strings"))))))

(defun validate-provides ()
  "Validator for provides field"
  (v:chain
   (v:optional (string-list))
   (v:validate-when
    #'listp
    (v:all-of
     (list
      (v:unique-items)
      (v:satisfies-predicate
       (lambda (provides)
         (every (lambda (prov)
                  (and (stringp prov)
                       (> (length prov) 0)))
                provides))
       "all provided packages must be non-empty strings"))))))

;;;; Main Module Validator

(defun validate-module-metadata (metadata filepath)
  "Validate module.lisp metadata with multi-stage validation.
   Returns (values valid-p errors) where errors is a list of error strings."
  
  (let* ((base-path (path:path-parent (path:make-path filepath)))
         ;; Define validation stages
         (stages (list
                  ;; Stage 1: Structural validation
                  (v:stage "structure"
                           (lambda (data)
                             (let ((result (v:plist-type data)))
                               (if (v:validation-success-p result)
                                   result
                                   (v:make-failure
                                    (cons (v:make-validation-error
                                           :message (format nil "Invalid module.lisp format in ~A: must be a property list"
                                                           filepath)
                                           :type :structure)
                                          (v:validation-errors result))))))
                           :continue-on-failure nil)
                  
                  ;; Stage 2: Known keys validation
                  (v:stage "keys"
                           (lambda (data)
                             (let ((valid-keys '(:name :version :description :author :platform
                                               :sources :tests :benchmarks :examples :experiments
                                               :docs :data :requires :optional :provides))
                                   (errors '()))
                               (loop for key in data by #'cddr
                                     unless (member key valid-keys)
                                     do (push (v:make-validation-error
                                              :field (string key)
                                              :message (format nil "Unknown key ~A. Valid keys are: ~{~A~^, ~}"
                                                             key valid-keys)
                                              :type :unknown-key)
                                             errors))
                               (if errors
                                   (v:make-failure (nreverse errors))
                                   (v:make-success data))))
                           :continue-on-failure t)
                  
                  ;; Stage 3: Type validation
                  (v:stage "types"
                           (lambda (data)
                             (let ((validators (map:make-map
                                              :name (validate-name)
                                              :version (validate-version)
                                              :description (v:optional #'v:string-type)
                                              :author (v:optional #'v:string-type)
                                              :platform (v:optional #'v:string-type)
                                              :sources (v:optional (string-list))
                                              :tests (v:optional (string-list))
                                              :benchmarks (v:optional (string-list))
                                              :examples (v:optional (string-list))
                                              :experiments (v:optional (string-list))
                                              :docs (v:optional (string-list))
                                              :data (v:optional (string-list))
                                              :requires (validate-requires)
                                              :optional (validate-requires)  ; Same format as requires
                                              :provides (validate-provides))))
                               ;; Convert plist to map for validation
                               (let ((data-map map:+empty+))
                                 (loop for (key value) on data by #'cddr
                                       do (setf data-map (map:assoc data-map key value)))
                                 (let ((result (v:validate-map validators data-map)))
                                   (if (v:validation-success-p result)
                                       (v:make-success data)
                                       result)))))
                           :continue-on-failure t)
                  
                  ;; Stage 4: Semantic validation (cross-field)
                  (v:stage "semantic"
                           (lambda (data)
                             (validate-sources-tests-disjoint data base-path))
                           :continue-on-failure t)
                  
                  ;; Stage 5: Filesystem validation
                  (v:stage "filesystem"
                           (lambda (data)
                             (let ((errors '()))
                               ;; Validate sources directories exist
                               (let ((sources (getf data :sources)))
                                 (when sources
                                   (let ((result (funcall (all-directories-exist base-path) sources)))
                                     (when (v:validation-failure-p result)
                                       (dolist (error (v:validation-errors result))
                                         (let ((new-error (copy-structure error)))
                                           (setf (v:validation-error-field new-error)
                                                 (format nil "sources~A" 
                                                        (or (v:validation-error-field error) "")))
                                           (push new-error errors)))))))
                               
                               ;; Validate tests directories exist
                               (let ((tests (getf data :tests)))
                                 (when tests
                                   (let ((result (funcall (all-directories-exist base-path) tests)))
                                     (when (v:validation-failure-p result)
                                       (dolist (error (v:validation-errors result))
                                         (let ((new-error (copy-structure error)))
                                           (setf (v:validation-error-field new-error)
                                                 (format nil "tests~A"
                                                        (or (v:validation-error-field error) "")))
                                           (push new-error errors)))))))
                               
                               (if errors
                                   (v:make-failure (nreverse errors))
                                   (v:make-success data))))
                           :continue-on-failure nil)))
         
         ;; Run validation
         (result (v:run-stages stages metadata)))
    
    ;; Convert to legacy format for compatibility
    (if (v:validation-success-p result)
        (values t nil)
        (values nil
                (mapcar (lambda (error)
                          (format nil "~@[~A: ~]~A"
                                 (v:validation-error-field error)
                                 (v:validation-error-message error)))
                        (v:validation-errors result))))))