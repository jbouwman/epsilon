(in-package #:epsilon.sys.ffi)

;;;# Finding Foreign Libraries

(defun darwin-fallback-library-path ()
  (list (merge-pathnames #p"lib/" (user-homedir-pathname))
        #+arm64 #p"/opt/homebrew/lib/"
        #p"/opt/local/lib/"
        #p"/usr/local/lib/"
        #p"/usr/lib/"))

(defvar *foreign-library-directories*
  '()
  "List onto which user-defined library paths can be pushed.")

(defun fallback-darwin-framework-directories ()
  (list (merge-pathnames #p"Library/Frameworks/" (user-homedir-pathname))
        #p"/Library/Frameworks/"
        #p"/System/Library/Frameworks/"))

(defvar *darwin-framework-directories*
  '((fallback-darwin-framework-directories))
  "List of directories where Frameworks are searched for.")

(defun mini-eval (form)
  "Simple EVAL-like function to evaluate the elements of
*FOREIGN-LIBRARY-DIRECTORIES* and *DARWIN-FRAMEWORK-DIRECTORIES*."
  (typecase form
    (cons (apply (car form) (mapcar #'mini-eval (cdr form))))
    (symbol (symbol-value form))
    (t form)))

(defun parse-directories (list)
  (mappend (compose #'ensure-list #'mini-eval) list))

(defun find-file (path directories)
  "Searches for PATH in a list of DIRECTORIES and returns the first it finds."
  (some (lambda (directory) (probe-file (merge-pathnames path directory)))
        directories))

(defun find-darwin-framework (framework-name)
  "Searches for FRAMEWORK-NAME in *DARWIN-FRAMEWORK-DIRECTORIES*."
  (dolist (directory (parse-directories *darwin-framework-directories*))
    (let ((framework-directory
            (merge-pathnames (format nil "~A.framework/" framework-name)
                             directory)))

      (when (probe-file framework-directory)
        (let ((path (merge-pathnames framework-name framework-directory)))
          (return-from find-darwin-framework path))))))

;;;# Defining Foreign Libraries
;;;
;;; Foreign libraries can be defined using the
;;; DEFINE-FOREIGN-LIBRARY macro. Example usage:
;;;
;;; (define-foreign-library opengl
;;;   (:darwin  (:framework "OpenGL"))
;;;   (:unix    (:or "libGL.so" "libGL.so.1"
;;;                  #p"/myhome/mylibGL.so"))
;;;   (:windows "opengl32.dll")
;;;   ;; an hypothetical example of a particular platform
;;;   ((:and :some-system :some-cpu) "libGL-support.lib")
;;;   ;; if no other clauses apply, this one will and a type will be
;;;   ;; automagically appended to the name passed to :default
;;;   (t (:default "libGL")))
;;;
;;; This information is stored in the *FOREIGN-LIBRARIES* map
;;; and when the library is loaded through LOAD-FOREIGN-LIBRARY (or
;;; USE-FOREIGN-LIBRARY) the first clause matched by FEATUREP is
;;; processed.

(defvar *foreign-libraries* map:+empty+
  "Map of defined libraries.")

(defclass foreign-library ()
  ((name :initform nil :initarg :name :accessor foreign-library-name)
   (type :initform :system :initarg :type)
   (spec :initarg :spec)
   (options :initform nil :initarg :options)
   (load-state :initform nil :initarg :load-state :accessor foreign-library-load-state)
   (handle :initform nil :initarg :handle :accessor foreign-library-handle)
   (pathname :initform nil)))

(defmethod print-object ((library foreign-library) stream)
  (with-slots (name pathname) library
    (print-unreadable-object (library stream :type t)
      (when name
        (format stream "~A" name))
      (when pathname
        (format stream " ~S" (file-namestring pathname))))))

(define-condition foreign-library-undefined-error (error)
  ((name :initarg :name :reader fl-name))
  (:report (lambda (c s)
             (format s "Undefined foreign library: ~S"
                     (fl-name c)))))

(defun get-foreign-library (lib)
  "Look up a library by NAME, signalling an error if not found."
  (if (typep lib 'foreign-library)
      lib
      (or (map:get *foreign-libraries* lib)
          (error 'foreign-library-undefined-error :name lib))))

(defun (setf get-foreign-library) (value name)
  (setf *foreign-libraries*
        (map:assoc *foreign-libraries* name value)))

(defun foreign-library-type (lib)
  (slot-value (get-foreign-library lib) 'type))

(defun foreign-library-pathname (lib)
  (slot-value (get-foreign-library lib) 'pathname))

(defun %foreign-library-spec (lib)
  (assoc-if (lambda (feature)
              (or (eq feature t)
                  (featurep feature)))
            (slot-value lib 'spec)))

(defun foreign-library-spec (lib)
  (second (%foreign-library-spec lib)))

(defun foreign-library-options (lib)
  (append (cddr (%foreign-library-spec lib))
          (slot-value lib 'options)))

(defun foreign-library-search-path (lib)
  (loop for (opt val) on (foreign-library-options lib) by #'cddr
        when (eql opt :search-path)
          append (ensure-list val) into search-path
        finally (return (mapcar #'pathname search-path))))

(defun foreign-library-loaded-p (lib)
  (not (null (foreign-library-load-state (get-foreign-library lib)))))

(defun list-foreign-libraries (&key (loaded-only t) type)
  "Return a list of defined foreign libraries.
If LOADED-ONLY is non-null only loaded libraries are returned.
TYPE restricts the output to a specific library type: if NIL
all libraries are returned."
  (let ((libs (map::vals *foreign-libraries*)))
    (remove-if (lambda (lib)
                 (or (and type
                          (not (eql type (foreign-library-type lib))))
                     (and loaded-only
                          (not (foreign-library-loaded-p lib)))))
               libs)))

;; options with NULL values are removed
(defun clean-spec-up (spec)
  (mapcar (lambda (x)
            (list* (first x) (second x)
                   (let* ((opts (cddr x))
                          (convention (getf opts :convention))
                          (search-path (getf opts :search-path)))
                     (setf (getf opts :convention)
                           convention)
                     (setf (getf opts :search-path)
                           (mapcar #'pathname (ensure-list search-path)))
                     (loop for (opt val) on opts by #'cddr
                           when val append (list opt val) into new-opts
                           finally (return new-opts)))))
          spec))

(defmethod initialize-instance :after
    ((lib foreign-library) &key canary search-path
     (convention :cdecl))
  (with-slots (type options spec) lib
    (check-type type (member :system :test :grovel-wrapper))
    (setf spec (clean-spec-up spec))
    (let ((all-options
           (apply #'append options (mapcar #'cddr spec))))
      (assert (subsetp (loop for (key . nil) on all-options by #'cddr
                             collect key)
                       '(:convention :search-path)))
      (flet ((set-option (key value)
               (when value (setf (getf options key) value))))
        (set-option :convention convention)
        (set-option :search-path
                    (mapcar #'pathname (ensure-list search-path)))
        (set-option :canary canary)))))

(defun register-foreign-library (name spec &rest options)
  (let ((old-handle
         (when-let ((old-lib (map:get *foreign-libraries* name)))
           (foreign-library-handle old-lib))))
    (setf (get-foreign-library name)
          (apply #'make-instance 'foreign-library
                 :name name
                 :spec spec
                 :handle old-handle
                 options))
    name))

(defmacro define-foreign-library (name-and-options &body pairs)
  "Defines a foreign library NAME that can be posteriorly used with
the USE-FOREIGN-LIBRARY macro."
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    (check-type name symbol)
    `(register-foreign-library ',name ',pairs ,@options)))

;;;# LOAD-FOREIGN-LIBRARY-ERROR condition
;;;
;;; The various helper functions that load foreign libraries can
;;; signal this error when something goes wrong. We ignore the host's
;;; error. We should probably reuse its error message.

(define-condition load-foreign-library-error (simple-error)
  ())

(defun read-new-value ()
  (format *query-io* "~&Enter a new value (unevaluated): ")
  (force-output *query-io*)
  (read *query-io*))

(defun fl-error (control &rest arguments)
  (error 'load-foreign-library-error
         :format-control control
         :format-arguments arguments))

;;;# Loading Foreign Libraries

(defun load-darwin-framework (name framework-name)
  "Tries to find and load a darwin framework in one of the directories
in *DARWIN-FRAMEWORK-DIRECTORIES*. If unable to find FRAMEWORK-NAME,
it signals a LOAD-FOREIGN-LIBRARY-ERROR."
  (let ((framework (find-darwin-framework framework-name)))
    (if framework
        (load-foreign-library-path name framework)
        (fl-error "Unable to find framework ~A" framework-name))))

(defun report-simple-error (name error)
  (fl-error "Unable to load foreign library (~A).~%  ~A"
            name
            (format nil "~?" (simple-condition-format-control error)
                    (simple-condition-format-arguments error))))

;;; FIXME: haven't double checked whether all Lisps signal a
;;; SIMPLE-ERROR on %load-foreign-library failure.  In any case they
;;; should be throwing a more specific error.
(defun load-foreign-library-path (name path &optional search-path)
  "Tries to load PATH using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (handler-case
      (values (%load-foreign-library name path)
              (pathname path))
    (simple-error (error)
      (let ((dirs (parse-directories *foreign-library-directories*)))
        (if-let (file (find-file path (append search-path dirs)))
          (handler-case
              (values (%load-foreign-library name file)
                      file)
            (simple-error (error)
              (report-simple-error name error)))
          (report-simple-error name error))))))

(defun try-foreign-library-alternatives (name library-list &optional search-path)
  "Goes through a list of alternatives and only signals an error when
none of alternatives were successfully loaded."
  (dolist (lib library-list)
    (multiple-value-bind (handle pathname)
        (ignore-errors (load-foreign-library-helper name lib search-path))
      (when handle
        (return-from try-foreign-library-alternatives
          (values handle pathname)))))
  ;; Perhaps we should show the error messages we got for each
  ;; alternative if we can figure out a nice way to do that.
  (fl-error "Unable to load any of the alternatives:~%   ~S" library-list))

(defparameter *cffi-feature-suffix-map*
  '((:windows . ".dll")
    (:darwin . ".dylib")
    (:unix . ".so")
    (t . ".so"))
  "Mapping of OS feature keywords to shared library suffixes.")

(defun default-library-suffix ()
  "Return a string to use as default library suffix based on the
operating system.  This is used to implement the :DEFAULT option.
This will need to be extended as we test on more OSes."
  (or (cdr (assoc-if #'featurep *cffi-feature-suffix-map*))
      (fl-error "Unable to determine the default library suffix on this OS.")))

(defun load-foreign-library-helper (name thing &optional search-path)
  (etypecase thing
    ((or pathname string)
     (load-foreign-library-path name (filter-pathname thing) search-path))
    (cons
     (ecase (first thing)
       (:framework (load-darwin-framework name (second thing)))
       (:default
        (unless (stringp (second thing))
          (fl-error "Argument to :DEFAULT must be a string."))
        (let ((library-path
               (concatenate 'string
                            (second thing)
                            (default-library-suffix))))
          (load-foreign-library-path name library-path search-path)))
       (:or (try-foreign-library-alternatives name (rest thing) search-path))))))

(defun %do-load-foreign-library (library search-path)
  (flet ((%do-load (lib name spec)
           (let ((canary (getf (foreign-library-options lib) :canary)))
             (cond
               ((and canary (foreign-symbol-pointer canary))
                ;; Do nothing because the library is already loaded.
                (setf (foreign-library-load-state lib) :static))
               ((foreign-library-spec lib)
                (with-slots (handle pathname) lib
                  (setf (values handle pathname)
                        (load-foreign-library-helper
                         name spec (foreign-library-search-path lib)))
                  (setf (foreign-library-load-state lib) :external)))))
           lib))
    (etypecase library
      (symbol
       (let* ((lib (get-foreign-library library))
              (spec (foreign-library-spec lib)))
         (%do-load lib library spec)))
      ((or string list)
       (let* ((lib-name (gensym
                         (format nil "~:@(~A~)-"
                                 (if (listp library)
                                     (first library)
                                     (file-namestring library)))))
              (lib (make-instance 'foreign-library
                                  :type :system
                                  :name lib-name
                                  :spec `((t ,library))
                                  :search-path search-path)))
         ;; first try to load the anonymous library
         ;; and register it only if that worked
         (%do-load lib lib-name library)
         (setf (get-foreign-library lib-name) lib))))))

(defun filter-pathname (thing)
  (typecase thing
    (pathname (namestring thing))
    (t        thing)))

(defun load-foreign-library (library &key search-path)
  "Loads a foreign LIBRARY which can be a symbol denoting a library defined
through DEFINE-FOREIGN-LIBRARY; a pathname or string in which case we try to
load it directly first then search for it in *FOREIGN-LIBRARY-DIRECTORIES*;
or finally list: either (:or lib1 lib2) or (:framework <framework-name>).
The option :CANARY can specify a symbol that will be searched to detect if
the library is already loaded, in which case DEFINE-FOREIGN-LIBRARY will mark
the library as loaded and return."
  (let ((library (filter-pathname library)))
    (restart-case
        (progn
          ;; dlopen/dlclose does reference counting, but the SYS.FFI.SYS
          ;; API has no infrastructure to track that. Therefore if we
          ;; want to avoid increasing the internal dlopen reference
          ;; counter, and thus thwarting dlclose, then we need to try
          ;; to call CLOSE-FOREIGN-LIBRARY and ignore any signaled
          ;; errors.
          (ignore-some-conditions (foreign-library-undefined-error)
            (close-foreign-library library))
          (%do-load-foreign-library library search-path))
      ;; Offer these restarts that will retry the call to
      ;; %LOAD-FOREIGN-LIBRARY.
      (retry ()
        :report "Try loading the foreign library again."
        (load-foreign-library library :search-path search-path))
      (use-value (new-library)
        :report "Use another library instead."
        :interactive read-new-value
        (load-foreign-library new-library :search-path search-path)))))

(defmacro use-foreign-library (name)
  `(load-foreign-library ',name))

;;;# Closing Foreign Libraries

(defun close-foreign-library (library)
  "Closes a foreign library."
  (let* ((library (filter-pathname library))
         (lib (get-foreign-library library))
         (handle (foreign-library-handle lib)))
    (when handle
      (%close-foreign-library handle)
      (setf (foreign-library-handle lib) nil)
      ;; Reset the load state only when the library was externally loaded.
      (setf (foreign-library-load-state lib) nil)
      t)))

(defun reload-foreign-libraries (&key (test #'foreign-library-loaded-p))
  "(Re)load all currently loaded foreign libraries."
  (let ((libs (list-foreign-libraries)))
    (loop for l in libs
          for name = (foreign-library-name l)
          when (funcall test name)
            do (load-foreign-library name))
    libs))
