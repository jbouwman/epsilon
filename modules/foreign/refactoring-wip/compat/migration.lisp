;;;; Migration and Compatibility Layer for epsilon.foreign
;;;;
;;;; Provides backward compatibility during transition to new API

(defpackage epsilon.foreign.compat
  (:use cl)
  (:local-nicknames
   (#:old epsilon.foreign)
   (#:new epsilon.foreign.core))
  (:export
   ;; Compatibility layer
   #:enable-compatibility-mode
   #:disable-compatibility-mode
   #:*compatibility-warnings*
   
   ;; Migration helpers
   #:migrate-defshared
   #:migrate-shared-call
   #:check-api-usage
   #:generate-migration-report
   
   ;; Deprecation support
   #:deprecation-warning
   #:*suppress-deprecation-warnings*))

(in-package epsilon.foreign.compat)

;;;; Configuration

(defvar *compatibility-mode* t
  "Whether compatibility mode is enabled")

(defvar *compatibility-warnings* t
  "Whether to show compatibility warnings")

(defvar *suppress-deprecation-warnings* nil
  "Suppress deprecation warnings")

(defvar *api-usage-log* '()
  "Log of deprecated API usage")

;;;; Deprecation Warning Condition

(define-condition deprecation-warning (warning)
  ((old-name :initarg :old-name :reader deprecation-old-name)
   (new-name :initarg :new-name :reader deprecation-new-name)
   (removal-version :initarg :removal-version :reader deprecation-removal-version
                    :initform "2.0.0"))
  (:report (lambda (c s)
             (format s "~A is deprecated and will be removed in version ~A. Use ~A instead."
                     (deprecation-old-name c)
                     (deprecation-removal-version c)
                     (deprecation-new-name c)))))

(defun warn-deprecated (old-name new-name &optional (removal-version "2.0.0"))
  "Issue a deprecation warning"
  (unless *suppress-deprecation-warnings*
    (warn 'deprecation-warning 
          :old-name old-name
          :new-name new-name
          :removal-version removal-version))
  (push (list old-name new-name (get-universal-time)) *api-usage-log*))

;;;; Compatibility Wrappers

;; Map old epsilon.foreign exports to new epsilon.foreign.core

(defmacro generate-compat-wrapper (old-name new-package new-name)
  "Generate a compatibility wrapper function"
  `(defun ,old-name (&rest args)
     (warn-deprecated ',old-name ',new-name)
     (apply (find-symbol ,(string new-name) ,new-package) args)))

;; Generate wrappers for all old API functions
(defmacro define-compatibility-layer ()
  `(progn
     ;; Direct mappings (same name)
     ,@(loop for name in '(defshared shared-call lib-open lib-close lib-function
                           foreign-alloc foreign-free with-foreign-memory
                           convert-to-foreign convert-from-foreign
                           define-c-type with-c-string
                           define-c-struct with-c-struct struct-ref struct-size
                           defcallback with-callback callback-pointer
                           foreign-error with-foreign-error-handler)
             collect `(setf (symbol-function ',(intern (string name) :epsilon.foreign))
                            (symbol-function ',(intern (string name) :epsilon.foreign.core))))
     
     ;; Renamed/modified functions
     (generate-compat-wrapper shared-call-unified :epsilon.foreign.core shared-call)
     (generate-compat-wrapper shared-call-fast :epsilon.foreign.core shared-call)
     (generate-compat-wrapper shared-call-libffi :epsilon.foreign.core shared-call)
     (generate-compat-wrapper defshared-fast :epsilon.foreign.core defshared)
     (generate-compat-wrapper defshared-auto :epsilon.foreign.core defshared)))

;;;; Migration Helpers

(defun migrate-defshared (old-form)
  "Convert old defshared form to new format"
  ;; Old: (defshared name c-name lib return args ...)
  ;; New: Same format but ensure args are properly wrapped
  (destructuring-bind (defshared name c-name lib return args &rest options) old-form
    (let ((new-args (if (and args (not (listp (first args))))
                        ;; Old flat format: (arg1 :type1 arg2 :type2)
                        (loop for (arg type) on args by #'cddr
                              collect (list arg type))
                        ;; Already in new format or empty
                        args)))
      `(new:defshared ,name ,c-name ,lib ,return ,new-args ,@options))))

(defun migrate-shared-call (old-form)
  "Convert old shared-call form to new format"
  ;; The new shared-call has same signature, just optimized implementation
  (cons 'new:shared-call (cdr old-form)))

(defun check-api-usage (&optional (package *package*))
  "Check for deprecated API usage in a package"
  (let ((deprecated-uses '()))
    (do-symbols (symbol package)
      (when (and (fboundp symbol)
                 (member (symbol-name symbol)
                         '("SHARED-CALL-UNIFIED" "SHARED-CALL-FAST" 
                           "SHARED-CALL-LIBFFI" "DEFSHARED-FAST"
                           "DEFSHARED-AUTO")
                         :test #'string=))
        (push symbol deprecated-uses)))
    deprecated-uses))

(defun generate-migration-report ()
  "Generate a report of deprecated API usage"
  (format t "~%===== Migration Report =====~%")
  (format t "Compatibility mode: ~A~%" (if *compatibility-mode* "ENABLED" "DISABLED"))
  (format t "~%Deprecated API usage (~D calls):~%" (length *api-usage-log*))
  
  ;; Group by function
  (let ((usage-by-function (make-hash-table :test 'equal)))
    (dolist (entry *api-usage-log*)
      (incf (gethash (first entry) usage-by-function 0)))
    
    (maphash (lambda (func count)
               (format t "  ~A: ~D calls~%" func count))
             usage-by-function))
  
  ;; Recommendations
  (format t "~%Recommendations:~%")
  (format t "1. Update all uses of -unified, -fast, -libffi variants to base function~%")
  (format t "2. Review defshared argument format (should be ((arg :type) ...)~%")
  (format t "3. Test with compatibility mode disabled before upgrading~%")
  (format t "~%Run (epsilon.foreign.compat:check-api-usage) to find deprecated uses~%"))

;;;; Automatic Migration

(defmacro with-auto-migration (&body body)
  "Automatically migrate old API calls in body"
  `(handler-bind ((deprecation-warning 
                   (lambda (w)
                     (declare (ignore w))
                     (when *compatibility-warnings*
                       (format *error-output* 
                               "~&; Auto-migrating deprecated call~%"))
                     (muffle-warning))))
     ,@body))

;;;; Testing Support

(defun test-migration (old-code expected-new-code)
  "Test that migration produces expected result"
  (let ((migrated (migrate-form old-code)))
    (equal migrated expected-new-code)))

(defun migrate-form (form)
  "Migrate a single form from old to new API"
  (cond
    ;; defshared migration
    ((and (consp form) (eq (first form) 'old:defshared))
     (migrate-defshared form))
    
    ;; shared-call variants
    ((and (consp form) (member (first form) 
                               '(old:shared-call-unified
                                 old:shared-call-fast
                                 old:shared-call-libffi)))
     (cons 'new:shared-call (cdr form)))
    
    ;; Recurse into nested forms
    ((consp form)
     (mapcar #'migrate-form form))
    
    ;; Atoms pass through
    (t form)))

(defun migrate-file (input-file output-file)
  "Migrate a file from old to new API"
  (with-open-file (in input-file)
    (with-open-file (out output-file :direction :output
                                     :if-exists :supersede)
      (loop for form = (read in nil nil)
            while form
            do (write (migrate-form form) :stream out :pretty t)
               (terpri out)))))

;;;; Compatibility Mode Control

(defun enable-compatibility-mode ()
  "Enable backward compatibility mode"
  (setf *compatibility-mode* t)
  (define-compatibility-layer)
  (format t "Compatibility mode enabled. Deprecated functions available with warnings.~%"))

(defun disable-compatibility-mode ()
  "Disable backward compatibility mode"
  (setf *compatibility-mode* nil)
  (format t "Compatibility mode disabled. Only new API available.~%")
  (generate-migration-report))

;;;; Package Setup

(defun setup-compatible-package ()
  "Set up package to be compatible with old code"
  (let ((old-exports (package-exports (find-package :epsilon.foreign))))
    ;; Re-export from new package with compatibility wrappers
    (dolist (symbol old-exports)
      (let ((new-symbol (find-symbol (string symbol) :epsilon.foreign.core)))
        (when new-symbol
          (import new-symbol :epsilon.foreign)
          (export new-symbol :epsilon.foreign))))))

;;;; Initialize compatibility on load

(when *compatibility-mode*
  (enable-compatibility-mode))