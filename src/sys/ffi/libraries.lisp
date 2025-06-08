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
          ;; dlopen/dlclose does reference counting, but the SYS.FFI
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

;;;# Built-In Types

;; NOTE: In the C standard there's a "signed-char":
;; https://stackoverflow.com/questions/436513/char-signed-char-char-unsigned-char
;; and "char" may be either signed or unsigned, i.e. treating it as a small int
;; is not wise. At the level of FFI we can safely ignore this and assume that
;; :char is mapped to "signed-char" by the CL implementation under us.

(define-built-in-foreign-type :char)
(define-built-in-foreign-type :unsigned-char)
(define-built-in-foreign-type :short)
(define-built-in-foreign-type :unsigned-short)
(define-built-in-foreign-type :int)
(define-built-in-foreign-type :unsigned-int)
(define-built-in-foreign-type :long)
(define-built-in-foreign-type :unsigned-long)
(define-built-in-foreign-type :float)
(define-built-in-foreign-type :double)
(define-built-in-foreign-type :void)
(define-built-in-foreign-type :long-long)
(define-built-in-foreign-type :unsigned-long-long)

(defparameter *possible-float-types* '(:float :double :long-double))

(defparameter *other-builtin-types* '(:pointer :void)
  "List of types other than integer or float built in to FFI.")

(defparameter *built-in-integer-types*
  (set-difference
   epsilon.sys.ffi:*built-in-foreign-types*
   (append *possible-float-types* *other-builtin-types*))
  "List of integer types supported by FFI.")

(defparameter *built-in-float-types*
  (set-difference
   epsilon.sys.ffi:*built-in-foreign-types*
   (append *built-in-integer-types* *other-builtin-types*))
  "List of real float types supported by FFI.")

;;;# Foreign Pointers

(define-compiler-macro inc-pointer (&whole form pointer offset)
  (if (and (constantp offset)
           (eql 0 (eval offset)))
      pointer
      form))

(define-modify-macro incf-pointer (&optional (offset 1)) inc-pointer)

(defun mem-ref (ptr type &optional (offset 0))
  "Return the value of TYPE at OFFSET bytes from PTR. If TYPE is aggregate,
we don't return its 'value' but a pointer to it, which is PTR itself."
  (let* ((parsed-type (parse-type type))
         (ctype (canonicalize parsed-type)))
    (if (aggregatep parsed-type)
        (if (bare-struct-type-p parsed-type)
            (inc-pointer ptr offset)
            (translate-from-foreign (inc-pointer ptr offset) parsed-type))
        (translate-from-foreign (%mem-ref ptr ctype offset) parsed-type))))

(define-compiler-macro mem-ref (&whole form ptr type &optional (offset 0))
  "Compiler macro to open-code MEM-REF when TYPE is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-type (eval type)))
             (ctype (canonicalize parsed-type)))
        (if (aggregatep parsed-type)
            (if (bare-struct-type-p parsed-type)
                `(inc-pointer ,ptr ,offset)
                (expand-from-foreign `(inc-pointer ,ptr ,offset) parsed-type))
            (expand-from-foreign `(%mem-ref ,ptr ,ctype ,offset) parsed-type)))
      form))

(defun mem-set (value ptr type &optional (offset 0))
  "Set the value of TYPE at OFFSET bytes from PTR to VALUE."
  (let* ((ptype (parse-type type))
         (ctype (canonicalize ptype)))
    (if (aggregatep ptype) ; XXX: backwards incompatible?
        (translate-into-foreign-memory value ptype (inc-pointer ptr offset))
        (%mem-set (translate-to-foreign value ptype) ptr ctype offset))))

(define-setf-expander mem-ref (ptr type &optional (offset 0) &environment env)
  "SETF expander for MEM-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on MEM-SET to be able
to open-code (SETF MEM-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; if either TYPE or OFFSET are constant, we avoid rebinding them
    ;; so that the compiler macros on MEM-SET and %MEM-SET work.
    (with-unique-names (store type-tmp offset-tmp)
      (values
       (append (unless (constantp type)   (list type-tmp))
               (unless (constantp offset) (list offset-tmp))
               dummies)
       (append (unless (constantp type)   (list type))
               (unless (constantp offset) (list offset))
               vals)
       (list store)
       `(progn
          (mem-set ,store ,getter
                   ,@(if (constantp type)   (list type)   (list type-tmp))
                   ,@(if (constantp offset) (list offset) (list offset-tmp)))
          ,store)
       `(mem-ref ,getter
                 ,@(if (constantp type)   (list type)   (list type-tmp))
                 ,@(if (constantp offset) (list offset) (list offset-tmp)))))))

(define-compiler-macro mem-set
    (&whole form value ptr type &optional (offset 0))
  "Compiler macro to open-code (SETF MEM-REF) when type is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-type (eval type)))
             (ctype (canonicalize parsed-type)))
        (if (aggregatep parsed-type)
            (expand-into-foreign-memory
             value parsed-type `(inc-pointer ,ptr ,offset))
            `(%mem-set ,(expand-to-foreign value parsed-type)
                       ,ptr ,ctype ,offset)))
      form))

;;;# Dereferencing Foreign Arrays

;;; Maybe this should be named MEM-SVREF? [2007-02-28 LO]
(defun mem-aref (ptr type &optional (index 0))
  "Like MEM-REF except for accessing 1d arrays."
  (mem-ref ptr type (* index (foreign-type-size type))))

(define-compiler-macro mem-aref (&whole form ptr type &optional (index 0))
  "Compiler macro to open-code MEM-AREF when TYPE (and eventually INDEX)."
  (if (constantp type)
      (if (constantp index)
          `(mem-ref ,ptr ,type
                    ,(* (eval index) (foreign-type-size (eval type))))
          `(mem-ref ,ptr ,type (* ,index ,(foreign-type-size (eval type)))))
      form))

(define-setf-expander mem-aref (ptr type &optional (index 0) &environment env)
  "SETF expander for MEM-AREF."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    ;; we avoid rebinding type and index, if possible (and if type is not
    ;; constant, we don't bother about the index), so that the compiler macros
    ;; on MEM-SET or %MEM-SET can work.
    (with-unique-names (store type-tmp index-tmp)
      (values
       (append (unless (constantp type)
                 (list type-tmp))
               (unless (and (constantp type) (constantp index))
                 (list index-tmp))
               dummies)
       (append (unless (constantp type)
                 (list type))
               (unless (and (constantp type) (constantp index))
                 (list index))
               vals)
       (list store)
       ;; Here we'll try to calculate the offset from the type and index,
       ;; or if not possible at least get the type size early.
       `(progn
          ,(if (constantp type)
               (if (constantp index)
                   `(mem-set ,store ,getter ,type
                             ,(* (eval index) (foreign-type-size (eval type))))
                   `(mem-set ,store ,getter ,type
                             (* ,index-tmp ,(foreign-type-size (eval type)))))
               `(mem-set ,store ,getter ,type-tmp
                         (* ,index-tmp (foreign-type-size ,type-tmp))))
          ,store)
       `(mem-aref ,getter
                  ,@(if (constantp type)
                        (list type)
                        (list type-tmp))
                  ,@(if (and (constantp type) (constantp index))
                        (list index)
                        (list index-tmp)))))))

(defmethod translate-into-foreign-memory
    (value (type foreign-pointer-type) pointer)
  (setf (mem-aref pointer :pointer) value))

(defmethod translate-into-foreign-memory
    (value (type foreign-built-in-type) pointer)
  (setf (mem-aref pointer (unparse-type type)) value))

(defun mem-aptr (ptr type &optional (index 0))
  "The pointer to the element."
  (inc-pointer ptr (* index (foreign-type-size type))))

(define-compiler-macro mem-aptr (&whole form ptr type &optional (index 0))
  "The pointer to the element."
  (cond ((not (constantp type))
         form)
        ((not (constantp index))
         `(inc-pointer ,ptr (* ,index ,(foreign-type-size (eval type)))))
        ((zerop (eval index))
         ptr)
        (t
         `(inc-pointer ,ptr ,(* (eval index)
                                (foreign-type-size (eval type)))))))

(define-foreign-type foreign-array-type ()
  ((dimensions :reader dimensions :initarg :dimensions)
   (element-type :reader element-type :initarg :element-type))
  (:actual-type :pointer))

(defmethod aggregatep ((type foreign-array-type))
  t)

(defmethod print-object ((type foreign-array-type) stream)
  "Print a FOREIGN-ARRAY-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S ~S" (element-type type) (dimensions type))))

(defun array-element-size (array-type)
  (foreign-type-size (element-type array-type)))

(defmethod foreign-type-size ((type foreign-array-type))
  (* (array-element-size type) (reduce #'* (dimensions type))))

(defmethod foreign-type-alignment ((type foreign-array-type))
  (foreign-type-alignment (element-type type)))

(define-parse-method :array (element-type &rest dimensions)
  (assert (plusp (length dimensions)))
  (make-instance 'foreign-array-type
                 :element-type element-type
                 :dimensions dimensions))

(defun indexes-to-row-major-index (dimensions &rest subscripts)
  (apply #'+ (maplist (lambda (x y)
                        (* (car x) (apply #'* (cdr y))))
                      subscripts
                      dimensions)))

(defun row-major-index-to-indexes (index dimensions)
  (loop with idx = index
        with rank = (length dimensions)
        with indexes = (make-list rank)
        for dim-index from (- rank 1) downto 0 do
        (setf (values idx (nth dim-index indexes))
              (floor idx (nth dim-index dimensions)))
        finally (return indexes)))

(defun foreign-alloc (type &key (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      (count 1 count-p) null-terminated-p)
  "Allocate enough memory to hold COUNT objects of type TYPE. If
INITIAL-ELEMENT is supplied, each element of the newly allocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the
newly allocated memory."
  (let (contents-length)
    ;; Some error checking, etc...
    (when (and null-terminated-p
               (not (eq (canonicalize-foreign-type type) :pointer)))
      (error "Cannot use :NULL-TERMINATED-P with non-pointer types."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (let ((ptr (%foreign-alloc (* (foreign-type-size type)
                                  (if null-terminated-p (1+ count) count)))))
      (when initial-element-p
        (dotimes (i count)
          (setf (mem-aref ptr type i) initial-element)))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (mem-aref ptr type i) (elt initial-contents i))))
      (when null-terminated-p
        (setf (mem-aref ptr :pointer count) (null-pointer)))
      ptr)))

;;; Simple compiler macro that kicks in when TYPE is constant and only
;;; the COUNT argument is passed.  (Note: hard-coding the type's size
;;; into the fasl will likely break CLISP fasl cross-platform
;;; compatibilty.)
(define-compiler-macro foreign-alloc (&whole form type &rest args
                                      &key (count 1 count-p) &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(%foreign-alloc ,(* (eval count) (foreign-type-size (eval type)))))
        ((constantp type)
         `(%foreign-alloc (* ,count ,(foreign-type-size (eval type)))))
        (t form))
      form))

(defun lisp-array-to-foreign (array pointer array-type)
  "Copy elements from a Lisp array to POINTER. ARRAY-TYPE must be a FFI array
type."
  (let* ((type (ensure-parsed-base-type array-type))
         (el-type (element-type type))
         (dimensions (dimensions type)))
    (loop with foreign-type-size = (array-element-size type)
          with size = (reduce #'* dimensions)
          for i from 0 below size
          for offset = (* i foreign-type-size)
          for element = (apply #'aref array
                               (row-major-index-to-indexes i dimensions))
          do (setf (mem-ref pointer el-type offset) element))))

(defun foreign-array-to-lisp (pointer array-type &rest make-array-args)
  "Copy elements from pointer into a Lisp array. ARRAY-TYPE must be a FFI array
type; the type of the resulting Lisp array can be defined in MAKE-ARRAY-ARGS
that are then passed to MAKE-ARRAY. If POINTER is a null pointer, returns NIL."
  (unless (null-pointer-p pointer)
    (let* ((type (ensure-parsed-base-type array-type))
           (el-type (element-type type))
           (dimensions (dimensions type))
           (array (apply #'make-array dimensions make-array-args)))
      (loop with foreign-type-size = (array-element-size type)
            with size = (reduce #'* dimensions)
            for i from 0 below size
            for offset = (* i foreign-type-size)
            for element = (mem-ref pointer el-type offset)
            do (setf (apply #'aref array
                            (row-major-index-to-indexes i dimensions))
                     element))
      array)))

(defun foreign-array-alloc (array array-type)
  "Allocate a foreign array containing the elements of lisp array.
The foreign array must be freed with foreign-array-free."
  (check-type array array)
  (let* ((type (ensure-parsed-base-type array-type))
         (ptr (foreign-alloc (element-type type)
                             :count (reduce #'* (dimensions type)))))
    (lisp-array-to-foreign array ptr array-type)
    ptr))

(defun foreign-array-free (ptr)
  "Free a foreign array allocated by foreign-array-alloc."
  (foreign-free ptr))

(defmacro with-foreign-array ((var lisp-array array-type) &body body)
  "Bind var to a foreign array containing lisp-array elements in body."
  (with-unique-names (type)
    `(let ((,type (ensure-parsed-base-type ,array-type)))
       (with-foreign-pointer (,var (* (reduce #'* (dimensions ,type))
                                      (array-element-size ,type)))
         (lisp-array-to-foreign ,lisp-array ,var ,array-type)
         ,@body))))

(defun foreign-aref (ptr array-type &rest indexes)
  (let* ((type (ensure-parsed-base-type array-type))
         (offset (* (array-element-size type)
                    (apply #'indexes-to-row-major-index
                           (dimensions type) indexes))))
    (mem-ref ptr (element-type type) offset)))

(defun (setf foreign-aref) (value ptr array-type &rest indexes)
  (let* ((type (ensure-parsed-base-type array-type))
         (offset (* (array-element-size type)
                    (apply #'indexes-to-row-major-index
                           (dimensions type) indexes))))
    (setf (mem-ref ptr (element-type type) offset) value)))

;;; Automatic translations for the :ARRAY type. Notice that these
;;; translators will also invoke the appropriate translators for for
;;; each of the array's elements since that's the normal behaviour of
;;; the FOREIGN-ARRAY-* operators, but there's a FIXME: **it doesn't
;;; free them yet**

;;; This used to be in a separate type but let's experiment with just
;;; one type for a while. [2008-12-30 LO]

;;; FIXME: those ugly invocations of UNPARSE-TYPE suggest that these
;;; foreign array operators should take the type and dimention
;;; arguments "unboxed". [2008-12-31 LO]

(defmethod translate-to-foreign (array (type foreign-array-type))
  (foreign-array-alloc array (unparse-type type)))

(defmethod translate-aggregate-to-foreign (ptr value (type foreign-array-type))
  (lisp-array-to-foreign value ptr (unparse-type type)))

(defmethod translate-from-foreign (pointer (type foreign-array-type))
  (foreign-array-to-lisp pointer (unparse-type type)))

(defmethod free-translated-object (pointer (type foreign-array-type) param)
  (declare (ignore param))
  (foreign-array-free pointer))

;;;# Foreign Structures

;;;## Foreign Structure Slots

(defgeneric foreign-struct-slot-pointer (ptr slot)
  (:documentation
   "Get the address of SLOT relative to PTR."))

(defgeneric foreign-struct-slot-pointer-form (ptr slot)
  (:documentation
   "Return a form to get the address of SLOT in PTR."))

(defgeneric foreign-struct-slot-value (ptr slot)
  (:documentation
   "Return the value of SLOT in structure PTR."))

(defgeneric (setf foreign-struct-slot-value) (value ptr slot)
  (:documentation
   "Set the value of a SLOT in structure PTR."))

(defgeneric foreign-struct-slot-value-form (ptr slot)
  (:documentation
   "Return a form to get the value of SLOT in struct PTR."))

(defgeneric foreign-struct-slot-set-form (value ptr slot)
  (:documentation
   "Return a form to set the value of SLOT in struct PTR."))

(defclass foreign-struct-slot ()
  ((name   :initarg :name   :reader   slot-name)
   (offset :initarg :offset :accessor slot-offset)
   ;; FIXME: the type should probably be parsed?
   (type   :initarg :type   :accessor slot-type))
  (:documentation "Base class for simple and aggregate slots."))

(defmethod foreign-struct-slot-pointer (ptr (slot foreign-struct-slot))
  "Return the address of SLOT relative to PTR."
  (inc-pointer ptr (slot-offset slot)))

(defmethod foreign-struct-slot-pointer-form (ptr (slot foreign-struct-slot))
  "Return a form to get the address of SLOT relative to PTR."
  (let ((offset (slot-offset slot)))
    (if (zerop offset)
        ptr
        `(inc-pointer ,ptr ,offset))))

(defun foreign-slot-names (type)
  "Returns a list of TYPE's slot names in no particular order."
  (loop for value being the hash-values
        in (slots (ensure-parsed-base-type type))
        collect (slot-name value)))

;;;### Simple Slots

(defclass simple-struct-slot (foreign-struct-slot)
  ()
  (:documentation "Non-aggregate structure slots."))

(defmethod foreign-struct-slot-value (ptr (slot simple-struct-slot))
  "Return the value of a simple SLOT from a struct at PTR."
  (mem-ref ptr (slot-type slot) (slot-offset slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot simple-struct-slot))
  "Return a form to get the value of a slot from PTR."
  `(mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)))

(defmethod (setf foreign-struct-slot-value) (value ptr (slot simple-struct-slot))
  "Set the value of a simple SLOT to VALUE in PTR."
  (setf (mem-ref ptr (slot-type slot) (slot-offset slot)) value))

(defmethod foreign-struct-slot-set-form (value ptr (slot simple-struct-slot))
  "Return a form to set the value of a simple structure slot."
  `(setf (mem-ref ,ptr ',(slot-type slot) ,(slot-offset slot)) ,value))

;;;### Aggregate Slots

(defclass aggregate-struct-slot (foreign-struct-slot)
  ((count :initarg :count :accessor slot-count))
  (:documentation "Aggregate structure slots."))

;;; Since MEM-REF returns a pointer for struct types we are able to
;;; chain together slot names when accessing slot values in nested
;;; structures.
(defmethod foreign-struct-slot-value (ptr (slot aggregate-struct-slot))
  "Return a pointer to SLOT relative to PTR."
  (convert-from-foreign (inc-pointer ptr (slot-offset slot))
                        (slot-type slot)))

(defmethod foreign-struct-slot-value-form (ptr (slot aggregate-struct-slot))
  "Return a form to get the value of SLOT relative to PTR."
  `(convert-from-foreign (inc-pointer ,ptr ,(slot-offset slot))
                         ',(slot-type slot)))

(defmethod translate-aggregate-to-foreign (ptr value (type foreign-struct-type))
  ;;; FIXME: use the block memory interface instead.
  (loop for i below (foreign-type-size type)
        do (%mem-set (%mem-ref value :char i) ptr :char i)))

(defmethod (setf foreign-struct-slot-value)
    (value ptr (slot aggregate-struct-slot))
  "Set the value of an aggregate SLOT to VALUE in PTR."
  (translate-aggregate-to-foreign (inc-pointer ptr (slot-offset slot))
                                  value
                                  (parse-type (slot-type slot))))

(defmethod foreign-struct-slot-set-form (value ptr (slot aggregate-struct-slot))
  "Return a form to get the value of an aggregate SLOT relative to PTR."
  `(translate-aggregate-to-foreign (inc-pointer ,ptr ,(slot-offset slot))
                                   ,value
                                   ,(parse-type (slot-type slot))))

;;;## Defining Foreign Structures

(defun make-struct-slot (name offset type count)
  "Make the appropriate type of structure slot."
  ;; If TYPE is an aggregate type or COUNT is >1, create an
  ;; AGGREGATE-STRUCT-SLOT, otherwise a SIMPLE-STRUCT-SLOT.
  (if (or (> count 1) (aggregatep (parse-type type)))
      (make-instance 'aggregate-struct-slot :offset offset :type type
                     :name name :count count)
      (make-instance 'simple-struct-slot :offset offset :type type
                     :name name)))

(defun parse-deprecated-struct-type (name struct-or-union)
  (check-type struct-or-union (member :struct :union))
  (let* ((struct-type-name `(,struct-or-union ,name))
         (struct-type (parse-type struct-type-name)))
    (simple-style-warning
     "bare references to struct types are deprecated. ~
      Please use ~S or ~S instead."
     `(:pointer ,struct-type-name) struct-type-name)
    (make-instance (class-of struct-type)
                   :alignment (alignment struct-type)
                   :size (size struct-type)
                   :slots (slots struct-type)
                   :name (name struct-type)
                   :bare t)))

;;; Regarding structure alignment, the following ABIs were checked:
;;;   - System-V ABI: x86, x86-64, ppc, arm, mips and itanium. (more?)
;;;   - Mac OS X ABI Function Call Guide: ppc32, ppc64 and x86.
;;;
;;; Rules used here:
;;;
;;;   1. "An entire structure or union object is aligned on the same
;;;       boundary as its most strictly aligned member."
;;;
;;;   2. "Each member is assigned to the lowest available offset with
;;;       the appropriate alignment. This may require internal
;;;       padding, depending on the previous member."
;;;
;;;   3. "A structure's size is increased, if necessary, to make it a
;;;       multiple of the alignment. This may require tail padding,
;;;       depending on the last member."
;;;
;;; Special cases from darwin/ppc32's ABI:
;;; http://developer.apple.com/documentation/DeveloperTools/Conceptual/LowLevelABI/index.html
;;;
;;;   4. "The embedding alignment of the first element in a data
;;;       structure is equal to the element's natural alignment."
;;;
;;;   5. "For subsequent elements that have a natural alignment
;;;       greater than 4 bytes, the embedding alignment is 4, unless
;;;       the element is a vector."  (note: this applies for
;;;       structures too)

;; FIXME: get a better name for this. --luis
(defun get-alignment (type alignment-type firstp)
  "Return alignment for TYPE according to ALIGNMENT-TYPE."
  (declare (ignorable firstp))
  (ecase alignment-type
    (:normal #-(and darwin ppc)
             (foreign-type-alignment type)
             #+(and darwin ppc)
             (if firstp
                 (foreign-type-alignment type)
                 (min 4 (foreign-type-alignment type))))))

(defun adjust-for-alignment (type offset alignment-type firstp)
  "Return OFFSET aligned properly for TYPE according to ALIGNMENT-TYPE."
  (let* ((align (get-alignment type alignment-type firstp))
         (rem (mod offset align)))
    (if (zerop rem)
        offset
        (+ offset (- align rem)))))

(defmacro with-tentative-type-definition ((name value namespace) &body body)
  (once-only (name namespace)
    `(unwind-protect-case ()
          (progn
            (notice-foreign-type ,name ,value ,namespace)
            ,@body)
       (:abort (undefine-foreign-type ,name ,namespace)))))

(defun notice-foreign-struct-definition (name options slots)
  "Parse and install a foreign structure definition."
  (destructuring-bind (&key size (class 'foreign-struct-type))
      options
    (let ((struct (make-instance class :name name))
          (current-offset 0)
          (max-align 1)
          (firstp t))
      (with-tentative-type-definition (name struct :struct)
        ;; determine offsets
        (dolist (slotdef slots)
          (destructuring-bind (slotname type &key (count 1) offset) slotdef
            (when (eq (canonicalize-foreign-type type) :void)
              (simple-foreign-type-error type :struct
                                         "In struct ~S: void type not allowed in field ~S"
                                         name slotdef))
            (setq current-offset
                  (or offset
                      (adjust-for-alignment type current-offset :normal firstp)))
            (let* ((slot (make-struct-slot slotname current-offset type count))
                   (align (get-alignment (slot-type slot) :normal firstp)))
              (setf (gethash slotname (slots struct)) slot)
              (when (> align max-align)
                (setq max-align align)))
            (incf current-offset (* count (foreign-type-size type))))
          (setq firstp nil))
        ;; calculate padding and alignment
        (setf (alignment struct) max-align) ; See point 1 above.
        (let ((tail-padding (- max-align (rem current-offset max-align))))
          (unless (= tail-padding max-align) ; See point 3 above.
            (incf current-offset tail-padding)))
        (setf (size struct) (or size current-offset))))))

(defun generate-struct-accessors (name conc-name slot-names)
  (loop with pointer-arg = (symbolicate '#:pointer-to- name)
        for slot in slot-names
        for accessor = (symbolicate conc-name slot)
        collect `(defun ,accessor (,pointer-arg)
                   (foreign-slot-value ,pointer-arg '(:struct ,name) ',slot))
        collect `(defun (setf ,accessor) (value ,pointer-arg)
                   (foreign-slot-set value ,pointer-arg '(:struct ,name) ',slot))))

(define-parse-method :struct (name)
  (funcall (find-type-parser name :struct)))

(defvar *defcstruct-hook* nil)

(defmacro defcstruct (name-and-options &body fields)
  "Define the layout of a foreign structure."
  (discard-docstring fields)
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    (let ((conc-name (getf options :conc-name)))
      (remf options :conc-name)
      (unless (getf options :class) (setf (getf options :class) (symbolicate name '-tclass)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; m-f-s-t could do with this with mop:ensure-class.
         ,(when-let (class (getf options :class))
            `(defclass ,class (foreign-struct-type
                               translatable-foreign-type)
               ()))
         (notice-foreign-struct-definition ',name ',options ',fields)
         ,@(when conc-name
             (generate-struct-accessors name conc-name
                                        (mapcar #'car fields)))
         ,@(when *defcstruct-hook*
             ;; If non-nil, *defcstruct-hook* should be a function
             ;; of the arguments that returns NIL or a list of
             ;; forms to include in the expansion.
             (apply *defcstruct-hook* name-and-options fields))
         (define-parse-method ,name ()
           (parse-deprecated-struct-type ',name :struct))
         '(:struct ,name)))))

;;;## Accessing Foreign Structure Slots

(defun get-slot-info (type slot-name)
  "Return the slot info for SLOT-NAME or raise an error."
  (let* ((struct (ensure-parsed-base-type type))
         (info (gethash slot-name (slots struct))))
    (unless info
      (simple-foreign-type-error type :struct
                                 "Undefined slot ~A in foreign type ~A."
                                 slot-name type))
    info))

(defun foreign-slot-pointer (ptr type slot-name)
  "Return the address of SLOT-NAME in the structure at PTR."
  (foreign-struct-slot-pointer ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-pointer (&whole whole ptr type slot-name)
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-pointer-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      whole))

(defun foreign-slot-type (type slot-name)
  "Return the type of SLOT in a struct TYPE."
  (slot-type (get-slot-info type slot-name)))

(defun foreign-slot-offset (type slot-name)
  "Return the offset of SLOT in a struct TYPE."
  (slot-offset (get-slot-info type slot-name)))

(defun foreign-slot-count (type slot-name)
  "Return the number of items in SLOT in a struct TYPE."
  (slot-count (get-slot-info type slot-name)))

(defun foreign-slot-value (ptr type slot-name)
  "Return the value of SLOT-NAME in the foreign structure at PTR."
  (foreign-struct-slot-value ptr (get-slot-info type slot-name)))

(define-compiler-macro foreign-slot-value (&whole form ptr type slot-name)
  "Optimizer for FOREIGN-SLOT-VALUE when TYPE is constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-value-form
       ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(define-setf-expander foreign-slot-value (ptr type slot-name &environment env)
  "SETF expander for FOREIGN-SLOT-VALUE."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion ptr env)
    (declare (ignore setter newval))
    (if (and (constantp type) (constantp slot-name))
        ;; if TYPE and SLOT-NAME are constant we avoid rebinding them
        ;; so that the compiler macro on FOREIGN-SLOT-SET works.
        (with-unique-names (store)
          (values
           dummies
           vals
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type ,slot-name)
              ,store)
           `(foreign-slot-value ,getter ,type ,slot-name)))
        ;; if not...
        (with-unique-names (store slot-name-tmp type-tmp)
          (values
           (list* type-tmp slot-name-tmp dummies)
           (list* type slot-name vals)
           (list store)
           `(progn
              (foreign-slot-set ,store ,getter ,type-tmp ,slot-name-tmp)
              ,store)
           `(foreign-slot-value ,getter ,type-tmp ,slot-name-tmp))))))

(defun foreign-slot-set (value ptr type slot-name)
  "Set the value of SLOT-NAME in a foreign structure."
  (setf (foreign-struct-slot-value ptr (get-slot-info type slot-name)) value))

(define-compiler-macro foreign-slot-set
    (&whole form value ptr type slot-name)
  "Optimizer when TYPE and SLOT-NAME are constant."
  (if (and (constantp type) (constantp slot-name))
      (foreign-struct-slot-set-form
       value ptr (get-slot-info (eval type) (eval slot-name)))
      form))

(defmacro with-foreign-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE. Similar to WITH-SLOTS.
Each var can be of the form: 
  name                       name bound to slot of same name              
  (:pointer name)            name bound to pointer to slot of same name
  (name slot-name)           name bound to slot-name
  (name :pointer slot-name)  name bound to pointer to slot-name"
  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop :for var :in vars
               :collect
                 (if (listp var)
                     (let ((p1 (first var)) (p2 (second var)) (p3 (third var)))
                        (if (eq p1 :pointer)	
                           `(,p2 (foreign-slot-pointer ,ptr-var ',type ',p2))
                           (if (eq p2 :pointer)
                               `(,p1 (foreign-slot-pointer ,ptr-var ',type ',p3))
                               `(,p1 (foreign-slot-value ,ptr-var ',type ',p2)))))
                     `(,var (foreign-slot-value ,ptr-var ',type ',var))))
         ,@body))))

;;; We could add an option to define a struct instead of a class, in
;;; the unlikely event someone needs something like that.
(defmacro define-c-struct-wrapper (class-and-type supers &optional slots)
  "Define a new class with CLOS slots matching those of a foreign
struct type.  An INITIALIZE-INSTANCE method is defined which
takes a :POINTER initarg that is used to store the slots of a
foreign object.  This pointer is only used for initialization and
it is not retained.

CLASS-AND-TYPE is either a list of the form (class-name
struct-type) or a single symbol naming both.  The class will
inherit SUPERS.  If a list of SLOTS is specified, only those
slots will be defined and stored."
  (destructuring-bind (class-name &optional (struct-type (list :struct class-name)))
      (ensure-list class-and-type)
    (let ((slots (or slots (foreign-slot-names struct-type))))
      `(progn
         (defclass ,class-name ,supers
           ,(loop for slot in slots collect
                  `(,slot :reader ,(format-symbol t "~A-~A" class-name slot))))
         ;; This could be done in a parent class by using
         ;; FOREIGN-SLOT-NAMES when instantiating but then the compiler
         ;; macros wouldn't kick in.
         (defmethod initialize-instance :after ((inst ,class-name) &key pointer)
           (with-foreign-slots (,slots pointer ,struct-type)
             ,@(loop for slot in slots collect
                     `(setf (slot-value inst ',slot) ,slot))))
         ',class-name))))

;;;# Foreign Unions
;;;
;;; A union is a subclass of FOREIGN-STRUCT-TYPE in which all slots
;;; have an offset of zero.

;;; See also the notes regarding ABI requirements in
;;; NOTICE-FOREIGN-STRUCT-DEFINITION
(defun notice-foreign-union-definition (name-and-options slots)
  "Parse and install a foreign union definition."
  (destructuring-bind (name &key size)
      (ensure-list name-and-options)
    (let ((union (make-instance 'foreign-union-type :name name))
          (max-size 0)
          (max-align 0))
      (with-tentative-type-definition (name union :union)
        (dolist (slotdef slots)
          (destructuring-bind (slotname type &key (count 1)) slotdef
            (when (eq (canonicalize-foreign-type type) :void)
              (simple-foreign-type-error name :struct
                                         "In union ~S: void type not allowed in field ~S"
                                         name slotdef))
            (let* ((slot (make-struct-slot slotname 0 type count))
                   (size (* count (foreign-type-size type)))
                   (align (foreign-type-alignment (slot-type slot))))
              (setf (gethash slotname (slots union)) slot)
              (when (> size max-size)
                (setf max-size size))
              (when (> align max-align)
                (setf max-align align)))))
        (setf (size union) (or size max-size))
        (setf (alignment union) max-align)))))

(define-parse-method :union (name)
  (funcall (find-type-parser name :union)))

(defmacro defcunion (name-and-options &body fields)
  "Define the layout of a foreign union."
  (discard-docstring fields)
  (destructuring-bind (name &key size)
      (ensure-list name-and-options)
    (declare (ignore size))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-union-definition ',name-and-options ',fields)
       (define-parse-method ,name ()
         (parse-deprecated-struct-type ',name :union))
       '(:union ,name))))

;;;# Operations on Types

(defmethod foreign-type-alignment (type)
  "Return the alignment in bytes of a foreign type."
  (foreign-type-alignment (parse-type type)))

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(with-foreign-pointer
       (,var ,(if (constantp type)
                  ;; with-foreign-pointer may benefit from constant folding:
                  (if (constantp count)
                      (* (eval count) (foreign-type-size (eval type)))
                      `(* ,count ,(foreign-type-size (eval type))))
                  `(* ,count (foreign-type-size ,type))))
     ,@body))

(defmacro with-foreign-objects (bindings &body body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

;;;## Anonymous Type Translators
;;;
;;; (:wrapper :to-c some-function :from-c another-function)
;;;
;;; TODO: We will need to add a FREE function to this as well I think.
;;; --james

(define-foreign-type foreign-type-wrapper ()
  ((to-c   :initarg :to-c   :reader wrapper-to-c)
   (from-c :initarg :from-c :reader wrapper-from-c))
  (:documentation "Wrapper type."))

(define-parse-method :wrapper (base-type &key to-c from-c)
  (make-instance 'foreign-type-wrapper
                 :actual-type (parse-type base-type)
                 :to-c (or to-c 'identity)
                 :from-c (or from-c 'identity)))

(defmethod translate-to-foreign (value (type foreign-type-wrapper))
  (translate-to-foreign
   (funcall (slot-value type 'to-c) value) (actual-type type)))

(defmethod translate-from-foreign (value (type foreign-type-wrapper))
  (funcall (slot-value type 'from-c)
           (translate-from-foreign value (actual-type type))))

;;;# Other types

;;; Boolean type. Maps to an :int by default. Only accepts integer types.
(define-foreign-type foreign-boolean-type ()
  ())

(define-parse-method :boolean (&optional (base-type :int))
  (make-instance
   'foreign-boolean-type :actual-type
   (ecase (canonicalize-foreign-type base-type)
     ((:char :unsigned-char :int :unsigned-int :long :unsigned-long
       :long-long :unsigned-long-long) base-type))))

(defmethod translate-to-foreign (value (type foreign-boolean-type))
  (if value 1 0))

(defmethod translate-from-foreign (value (type foreign-boolean-type))
  (not (zerop value)))

(defmethod expand-to-foreign (value (type foreign-boolean-type))
  "Optimization for the :boolean type."
  (if (constantp value)
      (if (eval value) 1 0)
      `(if ,value 1 0)))

(defmethod expand-from-foreign (value (type foreign-boolean-type))
  "Optimization for the :boolean type."
  (if (constantp value) ; very unlikely, heh
      (not (zerop (eval value)))
      `(not (zerop ,value))))

;;; Boolean type that represents C99 _Bool
(defctype :bool (:boolean :char))

;;;# Typedefs for built-in types.

(defctype :uchar  :unsigned-char)
(defctype :ushort :unsigned-short)
(defctype :uint   :unsigned-int)
(defctype :ulong  :unsigned-long)
(defctype :llong  :long-long)
(defctype :ullong :unsigned-long-long)

(defmacro defctype-matching (name size-or-type base-types &key (match-by '=))
  (let* ((target-size (typecase size-or-type
                        (integer size-or-type)
                        (t (foreign-type-size size-or-type))))
         (matching-type (loop for type in base-types
                              for size = (foreign-type-size type)
                              when (funcall match-by target-size size)
                              return type)))
    (if matching-type
        `(defctype ,name ,matching-type)
        `(warn "Found no matching type of size ~d in~%  ~a"
               ,target-size ',base-types))))

;;; We try to define the :[u]int{8,16,32,64} types by looking at
;;; the sizes of the built-in integer types and defining typedefs.
(macrolet ((match-types (sized-types base-types)
             `(progn ,@(loop for (name size-or-type) in sized-types
                             collect `(defctype-matching ,name ,size-or-type ,base-types)))))
  ;; signed
  (match-types ((:int8 1) (:int16 2) (:int32 4) (:int64 8)
                (:intptr :pointer))
               (:char :short :int :long :long-long))
  ;; unsigned
  (match-types ((:uint8 1) (:uint16 2) (:uint32 4) (:uint64 8)
                (:uintptr :pointer))
               (:unsigned-char :unsigned-short :unsigned-int :unsigned-long
                :unsigned-long-long)))

;;; Pretty safe bets.
(defctype :size #+64-bit :uint64 #+32-bit :uint32)
(defctype :ssize #+64-bit :int64 #+32-bit :int32)
(defctype :ptrdiff :ssize)
(defctype :offset #+(or 64-bit bsd) :int64 #-(or 64-bit bsd) :int32)

;; TODO the accessors names are rather inconsistent:
;; FOREIGN-ENUM-VALUE           FOREIGN-BITFIELD-VALUE
;; FOREIGN-ENUM-KEYWORD         FOREIGN-BITFIELD-SYMBOLS
;; FOREIGN-ENUM-KEYWORD-LIST    FOREIGN-BITFIELD-SYMBOL-LIST
;; I'd rename them to: FOREIGN-*-KEY(S) and FOREIGN-*-ALL-KEYS -- attila

;; TODO bitfield is a confusing name, because the C standard calls
;; the "int foo : 3" type as a bitfield. Maybe rename to defbitmask?
;; -- attila

;;;# Foreign Constants as Lisp Keywords
;;;
;;; This module defines the DEFCENUM macro, which provides an
;;; interface for defining a type and associating a set of integer
;;; constants with keyword symbols for that type.
;;;
;;; The keywords are automatically translated to the appropriate
;;; constant for the type by a type translator when passed as
;;; arguments or a return value to a foreign function.

(defclass foreign-enum (named-foreign-type enhanced-foreign-type)
  ((keyword-values
    :initform (error "Must specify KEYWORD-VALUES.")
    :initarg :keyword-values
    :reader keyword-values)
   (value-keywords
    :initform (error "Must specify VALUE-KEYWORDS.")
    :initarg :value-keywords
    :reader value-keywords)
   (allow-undeclared-values
    :initform nil
    :initarg :allow-undeclared-values
    :reader allow-undeclared-values))
  (:documentation "Describes a foreign enumerated type."))

(deftype enum-key ()
  '(and symbol (not null)))

(defparameter +valid-enum-base-types+ *built-in-integer-types*)

(defun parse-foreign-enum-like (type-name base-type values
                                &optional field-mode-p)
  (let ((keyword-values (make-hash-table :test 'eq))
        (value-keywords (make-hash-table))
        (field-keywords (list))
        (bit-index->keyword (make-array 0 :adjustable t
                                        :element-type t))
        (default-value (if field-mode-p 1 0))
        (most-extreme-value 0)
        (has-negative-value? nil))
    (dolist (pair values)
      (destructuring-bind (keyword &optional (value default-value valuep))
          (ensure-list pair)
        (check-type keyword enum-key)
        ;;(check-type value integer)
        (when (> (abs value) (abs most-extreme-value))
          (setf most-extreme-value value))
        (when (minusp value)
          (setf has-negative-value? t))
        (if field-mode-p
            (if valuep
                (when (and (>= value default-value)
                           (single-bit-p value))
                  (setf default-value (ash value 1)))
                (setf default-value (ash default-value 1)))
            (setf default-value (1+ value)))
        (if (gethash keyword keyword-values)
            (error "A foreign enum cannot contain duplicate keywords: ~S."
                   keyword)
            (setf (gethash keyword keyword-values) value))
        ;; This is completely arbitrary behaviour: we keep the last
        ;; value->keyword mapping. I suppose the opposite would be
        ;; just as good (keeping the first). Returning a list with all
        ;; the keywords might be a solution too? Suggestions
        ;; welcome. --luis
        (setf (gethash value value-keywords) keyword)
        (when (and field-mode-p
                   (single-bit-p value))
          (let ((bit-index (1- (integer-length value))))
            (push keyword field-keywords)
            (when (<= (array-dimension bit-index->keyword 0)
                      bit-index)
              (setf bit-index->keyword
                    (adjust-array bit-index->keyword (1+ bit-index)
                                  :initial-element nil)))
            (setf (aref bit-index->keyword bit-index)
                  keyword)))))
    (if base-type
        (progn
          (setf base-type (canonicalize-foreign-type base-type))
          ;; I guess we don't lose much by not strictly adhering to
          ;; the C standard here, and some libs out in the wild are
          ;; already using e.g. :double.
          #+nil
          (assert (member base-type +valid-enum-base-types+ :test 'eq) ()
                  "Invalid base type ~S for enum type ~S. Must be one of ~S."
                  base-type type-name +valid-enum-base-types+))
        ;; details: https://stackoverflow.com/questions/1122096/what-is-the-underlying-type-of-a-c-enum
        (let ((bits (integer-length most-extreme-value)))
          (setf base-type
                (let ((most-uint-bits      (load-time-value (* (foreign-type-size :unsigned-int) 8)))
                      (most-ulong-bits     (load-time-value (* (foreign-type-size :unsigned-long) 8)))
                      (most-ulonglong-bits (load-time-value (* (foreign-type-size :unsigned-long-long) 8))))
                  (or (if has-negative-value?
                          (cond
                            ((<= (1+ bits) most-uint-bits)
                             :int)
                            ((<= (1+ bits) most-ulong-bits)
                             :long)
                            ((<= (1+ bits) most-ulonglong-bits)
                             :long-long))
                          (cond
                            ((<= bits most-uint-bits)
                             :unsigned-int)
                            ((<= bits most-ulong-bits)
                             :unsigned-long)
                            ((<= bits most-ulonglong-bits)
                             :unsigned-long-long)))
                      (error "Enum value ~S of enum ~S is too large to store."
                             most-extreme-value type-name))))))
    (values base-type keyword-values value-keywords
            field-keywords (when field-mode-p
                             (copy-array
                              bit-index->keyword :adjustable nil
                              :fill-pointer nil)))))

(defun make-foreign-enum (type-name base-type values &key allow-undeclared-values)
  "Makes a new instance of the foreign-enum class."
  (multiple-value-bind
        (base-type keyword-values value-keywords)
      (parse-foreign-enum-like type-name base-type values)
    (make-instance 'foreign-enum
                   :name type-name
                   :actual-type (parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords
                   :allow-undeclared-values allow-undeclared-values)))

(defun %defcenum-like (name-and-options enum-list type-factory)
  (discard-docstring enum-list)
  (destructuring-bind (name &optional base-type &rest args)
      (ensure-list name-and-options)
    (let ((type (apply type-factory name base-type enum-list args)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (notice-foreign-type ',name
                              ;; ,type is not enough here, someone needs to
                              ;; define it when we're being loaded from a fasl.
                              (,type-factory ',name ',base-type ',enum-list ,@args))
         ,@(remove nil
                   (mapcar (lambda (key)
                             (unless (keywordp key)
                               `(defconstant ,key ,(foreign-enum-value type key))))
                           (foreign-enum-keyword-list type)))))))

(defmacro defcenum (name-and-options &body enum-list)
  "Define an foreign enumerated type."
  (%defcenum-like name-and-options enum-list 'make-foreign-enum))

(defun hash-keys-to-list (ht)
  (loop for k being the hash-keys in ht collect k))

(defun foreign-enum-keyword-list (enum-type)
  "Return a list of KEYWORDS defined in ENUM-TYPE."
  (hash-keys-to-list (keyword-values (ensure-parsed-base-type enum-type))))

;;; These [four] functions could be good canditates for compiler macros
;;; when the value or keyword is constant.  I am not going to bother
;;; until someone has a serious performance need to do so though. --jamesjb
(defun %foreign-enum-value (type keyword &key errorp)
  (check-type keyword enum-key)
  (or (gethash keyword (keyword-values type))
      (when errorp
        (error "~S is not defined as a keyword for enum type ~S."
               keyword type))))

(defun foreign-enum-value (type keyword &key (errorp t))
  "Convert a KEYWORD into an integer according to the enum TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-enum))
      (error "~S is not a foreign enum type." type)
      (%foreign-enum-value type-obj keyword :errorp errorp))))

(defun %foreign-enum-keyword (type value &key errorp)
  (check-type value integer)
  (or (gethash value (value-keywords type))
      (when errorp
        (error "~S is not defined as a value for enum type ~S."
               value type))))

(defun foreign-enum-keyword (type value &key (errorp t))
  "Convert an integer VALUE into a keyword according to the enum TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-enum))
        (error "~S is not a foreign enum type." type)
        (%foreign-enum-keyword type-obj value :errorp errorp))))

(defmethod translate-to-foreign (value (type foreign-enum))
  (if (typep value 'enum-key)
      (%foreign-enum-value type value :errorp t)
      value))

(defmethod translate-into-foreign-memory
    (value (type foreign-enum) pointer)
  (setf (mem-aref pointer (unparse-type (actual-type type)))
        (translate-to-foreign value type)))

(defmethod translate-from-foreign (value (type foreign-enum))
  (if (allow-undeclared-values type)
      (or (%foreign-enum-keyword type value :errorp nil)
          value)
      (%foreign-enum-keyword type value :errorp t)))

(defmethod expand-to-foreign (value (type foreign-enum))
  (once-only (value)
    `(if (typep ,value 'enum-key)
         (%foreign-enum-value ,type ,value :errorp t)
         ,value)))

;;; There are two expansions necessary for an enum: first, the enum
;;; keyword needs to be translated to an int, and then the int needs
;;; to be made indirect.
(defmethod expand-to-foreign-dyn-indirect (value var body (type foreign-enum))
  (expand-to-foreign-dyn-indirect       ; Make the integer indirect
   (with-unique-names (feint)
     (call-next-method value feint (list feint) type)) ; TRANSLATABLE-FOREIGN-TYPE method
   var
   body
   (actual-type type)))

;;;# Foreign Bitfields as Lisp keywords
;;;
;;; DEFBITFIELD is an abstraction similar to the one provided by DEFCENUM.
;;; With some changes to DEFCENUM, this could certainly be implemented on
;;; top of it.

(defclass foreign-bitfield (foreign-enum)
  ((field-keywords
    :initform (error "Must specify FIELD-KEYWORDS.")
    :initarg :field-keywords
    :reader field-keywords)
   (bit-index->keyword
    :initform (error "Must specify BIT-INDEX->KEYWORD")
    :initarg :bit-index->keyword
    :reader bit-index->keyword))
  (:documentation "Describes a foreign bitfield type."))

(defun make-foreign-bitfield (type-name base-type values)
  "Makes a new instance of the foreign-bitfield class."
  (multiple-value-bind
        (base-type keyword-values value-keywords
                   field-keywords bit-index->keyword)
      (parse-foreign-enum-like type-name base-type values t)
    (make-instance 'foreign-bitfield
                   :name type-name
                   :actual-type (parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords
                   :field-keywords field-keywords
                   :bit-index->keyword bit-index->keyword)))

(defmacro defbitfield (name-and-options &body masks)
  "Define an foreign enumerated type."
  (%defcenum-like name-and-options masks 'make-foreign-bitfield))

(defun foreign-bitfield-symbol-list (bitfield-type)
  "Return a list of SYMBOLS defined in BITFIELD-TYPE."
  (field-keywords (ensure-parsed-base-type bitfield-type)))

(defun %foreign-bitfield-value (type symbols)
  (labels ((process-one (symbol)
             (check-type symbol symbol)
             (or (gethash symbol (keyword-values type))
                 (error "~S is not a valid symbol for bitfield type ~S."
                        symbol type))))
    (declare (dynamic-extent #'process-one))
    (cond
      ((consp symbols)
       (reduce #'logior symbols :key #'process-one))
      ((null symbols)
       0)
      (t
       (process-one symbols)))))

(defun foreign-bitfield-value (type symbols)
  "Convert a list of symbols into an integer according to the TYPE bitfield."
  (let ((type-obj (ensure-parsed-base-type type)))
    (assert (typep type-obj 'foreign-bitfield) ()
            "~S is not a foreign bitfield type." type)
    (%foreign-bitfield-value type-obj symbols)))

(define-compiler-macro foreign-bitfield-value (&whole form type symbols)
  "Optimize for when TYPE and SYMBOLS are constant."
  (declare (notinline foreign-bitfield-value))
  (if (and (constantp type) (constantp symbols))
      (foreign-bitfield-value (eval type) (eval symbols))
      form))

(defun %foreign-bitfield-symbols (type value)
  (check-type value integer)
  (check-type type foreign-bitfield)
  (loop
    :with bit-index->keyword = (bit-index->keyword type)
    :for bit-index :from 0 :below (array-dimension bit-index->keyword 0)
    :for mask = 1 :then (ash mask 1)
    :for key = (aref bit-index->keyword bit-index)
    :when (and key
               (= (logand value mask) mask))
    :collect key))

(defun foreign-bitfield-symbols (type value)
  "Convert an integer VALUE into a list of matching symbols according to
the bitfield TYPE."
  (let ((type-obj (ensure-parsed-base-type type)))
    (if (not (typep type-obj 'foreign-bitfield))
        (error "~S is not a foreign bitfield type." type)
        (%foreign-bitfield-symbols type-obj value))))

(define-compiler-macro foreign-bitfield-symbols (&whole form type value)
  "Optimize for when TYPE and SYMBOLS are constant."
  (declare (notinline foreign-bitfield-symbols))
  (if (and (constantp type) (constantp value))
      `(quote ,(foreign-bitfield-symbols (eval type) (eval value)))
      form))

(defmethod translate-to-foreign (value (type foreign-bitfield))
  (if (integerp value)
      value
      (%foreign-bitfield-value type (ensure-list value))))

(defmethod translate-from-foreign (value (type foreign-bitfield))
  (%foreign-bitfield-symbols type value))

(defmethod expand-to-foreign (value (type foreign-bitfield))
  (flet ((expander (value type)
           `(if (integerp ,value)
                ,value
                (%foreign-bitfield-value ,type (ensure-list ,value)))))
    (if (constantp value)
        (eval (expander value type))
        (expander value type))))

(defmethod expand-from-foreign (value (type foreign-bitfield))
  (flet ((expander (value type)
           `(%foreign-bitfield-symbols ,type ,value)))
    (if (constantp value)
        (eval (expander value type))
        (expander value type))))

;;;# Foreign String Conversion
;;;
;;; Functions for converting NULL-terminated C-strings to Lisp strings
;;; and vice versa.  The string functions accept an ENCODING keyword
;;; argument which is used to specify the encoding to use when
;;; converting to/from foreign strings.

(defvar *default-foreign-encoding* :utf-8
  "Default foreign encoding.")

(defun lisp-string-to-foreign (string buffer bufsize &key (start 0) end offset
                               (encoding *default-foreign-encoding*))
  ;; FIXME implement
  )

;;; Expands into a loop that calculates the length of the foreign
;;; string at PTR plus OFFSET, using ACCESSOR and looking for a null
;;; terminator of LENGTH bytes.
(defmacro %foreign-string-length (ptr offset type length)
  (once-only (ptr offset)
    `(do ((i 0 (+ i ,length)))
         ((zerop (mem-ref ,ptr ,type (+ ,offset i))) i)
       (declare (fixnum i)))))

;;; Return the length in octets of the null terminated foreign string
;;; at POINTER plus OFFSET octets, assumed to be encoded in ENCODING,
;;; a FFI encoding.  This should be smart enough to look for 8-bit vs
;;; 16-bit null terminators, as appropriate for the encoding.
(defun foreign-string-length (pointer &key (encoding *default-foreign-encoding*)
                              (offset 0))
  (ecase (null-terminator-len encoding)
    (1 (%foreign-string-length pointer offset :uint8 1))
    (2 (%foreign-string-length pointer offset :uint16 2))
    (4 (%foreign-string-length pointer offset :uint32 4))))

(defun foreign-string-to-lisp (pointer &key (offset 0) count
                               (max-chars (1- array-total-size-limit))
                               (encoding *default-foreign-encoding*))
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (unless (null-pointer-p pointer)
    ;; FIXME implement
    ))

;;;# Using Foreign Strings

(defun foreign-string-alloc (string &key (encoding *default-foreign-encoding*)
                             (null-terminated-p t) (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (with-checked-bounds ((string (coerce string 'char:simple-unicode-string))
                               (start start) (end end))
    (declare (type simple-string string))
    ;; FIXME implement
    ))

(defun foreign-string-free (ptr)
  "Free a foreign string allocated by FOREIGN-STRING-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-string ((var-or-vars lisp-string &rest args) &body body)
  "VAR-OR-VARS is not evaluated and should be a list of the form
\(VAR &OPTIONAL BYTE-SIZE-VAR) or just a VAR symbol.  VAR is
bound to a foreign string containing LISP-STRING in BODY.  When
BYTE-SIZE-VAR is specified then bind the C buffer size
\(including the possible null terminator\(s)) to this variable."
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(multiple-value-bind (,var ,@(when size-var (list size-var)))
         (foreign-string-alloc ,lisp-string ,@args)
       (unwind-protect
            (progn ,@body)
         (foreign-string-free ,var)))))

(defmacro with-foreign-strings (bindings &body body)
  "See WITH-FOREIGN-STRING's documentation."
  (if bindings
      `(with-foreign-string ,(first bindings)
         (with-foreign-strings ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-foreign-pointer-as-string
    ((var-or-vars size &rest args) &body body)
  "VAR-OR-VARS is not evaluated and should be a list of the form
\(VAR &OPTIONAL SIZE-VAR) or just a VAR symbol.  VAR is bound to
a foreign buffer of size SIZE within BODY.  The return value is
constructed by calling FOREIGN-STRING-TO-LISP on the foreign
buffer along with ARGS." ; fix wording, sigh
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(with-foreign-pointer (,var ,size ,size-var)
       (progn
         ,@body
         (values (foreign-string-to-lisp ,var ,@args))))))

;;;# Automatic Conversion of Foreign Strings

(define-foreign-type foreign-string-type ()
  (;; FFI encoding of this string.
   (encoding :initform nil :initarg :encoding :reader encoding)
   ;; Should we free after translating from foreign?
   (free-from-foreign :initarg :free-from-foreign
                      :reader fst-free-from-foreign-p
                      :initform nil :type boolean)
   ;; Should we free after translating to foreign?
   (free-to-foreign :initarg :free-to-foreign
                    :reader fst-free-to-foreign-p
                    :initform t :type boolean))
  (:actual-type :pointer)
  (:simple-parser :string))

;;; describe me
(defun fst-encoding (type)
  (or (encoding type) *default-foreign-encoding*))

;;; Display the encoding when printing a FOREIGN-STRING-TYPE instance.
(defmethod print-object ((type foreign-string-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~S" (fst-encoding type))))

(defmethod translate-to-foreign ((s string) (type foreign-string-type))
  (values (foreign-string-alloc s :encoding (fst-encoding type))
          (fst-free-to-foreign-p type)))

(defmethod translate-to-foreign (obj (type foreign-string-type))
  (cond
    ((pointerp obj)
     (values obj nil))
    (t (error "~A is not a Lisp string or pointer." obj))))

(defmethod translate-from-foreign (ptr (type foreign-string-type))
  (unwind-protect
       (values (foreign-string-to-lisp ptr :encoding (fst-encoding type)))
    (when (fst-free-from-foreign-p type)
      (foreign-free ptr))))

(defmethod free-translated-object (ptr (type foreign-string-type) free-p)
  (when free-p
    (foreign-string-free ptr)))

(defmethod expand-to-foreign-dyn-indirect
    (value var body (type foreign-string-type))
  (with-gensyms (str)
    (expand-to-foreign-dyn
     value
     str
     (list 
      (expand-to-foreign-dyn-indirect str var body (parse-type :pointer)))
     type)))

;;;# STRING+PTR

(define-foreign-type foreign-string+ptr-type (foreign-string-type)
  ()
  (:simple-parser :string+ptr))

(defmethod translate-from-foreign (value (type foreign-string+ptr-type))
  (list (call-next-method) value))

;;; Definitions for conversion of foreign structures.

(defmethod translate-into-foreign-memory ((object list)
                                          (type foreign-struct-type)
                                          p)
  (unless (bare-struct-type-p type)
    (loop for (name value) on object by #'cddr
          do (setf (foreign-slot-value p (unparse-type type) name)
                   (let ((slot (gethash name (structure-slots type))))
                     (convert-to-foreign value (slot-type slot)))))))

(defmethod translate-to-foreign (value (type foreign-struct-type))
  (let ((ptr (foreign-alloc type)))
    (translate-into-foreign-memory value type ptr)
    ptr))

(defmethod translate-from-foreign (p (type foreign-struct-type))
  ;; Iterate over slots, make plist
  (if (bare-struct-type-p type)
      p
      (let ((plist (list)))
        (loop for slot being the hash-value of (structure-slots type)
              for name = (slot-name slot)
              do (setf (getf plist name)
                       (foreign-struct-slot-value p slot)))
        plist)))

(defmethod free-translated-object (ptr (type foreign-struct-type) freep)
  (unless (bare-struct-type-p type)
    ;; Look for any pointer slots and free them first
    (loop for slot being the hash-value of (structure-slots type)
          when (and (listp (slot-type slot)) (eq (first (slot-type slot)) :pointer))
            do
               ;; Free if the pointer is to a specific type, not generic :pointer
               (free-translated-object
                (foreign-slot-value ptr type (slot-name slot))
                (rest (slot-type slot))
                freep))
    (foreign-free ptr)))

(defmacro define-translation-method ((object type method) &body body)
  "Define a translation method for the foreign structure type; 'method is one of :into, :from, or :to, meaning relation to foreign memory.  If :into, the variable 'pointer is the foreign pointer.  Note: type must be defined and loaded before this macro is expanded, and just the bare name (without :struct) should be specified."
  (let ((tclass (class-name (class-of (epsilon.sys.ffi::parse-type `(:struct ,type))))))
    (when (eq tclass 'foreign-struct-type)
      (error "Won't replace existing translation method for foreign-struct-type"))
    `(defmethod
         ,(case method
            (:into 'translate-into-foreign-memory)
            (:from 'translate-from-foreign)
            (:to 'translate-to-foreign))
       ;; Arguments to the method
       (,object
        (type ,tclass)
        ,@(when (eq method :into) '(pointer))) ; is intentional variable capture a good idea?
       ;; The body
       (declare (ignorable type)) ; I can't think of a reason why you'd want to use this
       ,@body)))

(defmacro translation-forms-for-class (class type-class)
  "Make forms for translation of foreign structures to and from a standard class.  The class slots are assumed to have the same name as the foreign structure."
  ;; Possible improvement: optional argument to map structure slot names to/from class slot names.
  `(progn
     (defmethod translate-from-foreign (pointer (type ,type-class))
       ;; Make the instance from the plist
       (apply 'make-instance ',class (call-next-method)))
     (defmethod translate-into-foreign-memory ((object ,class) (type ,type-class) pointer)
       (call-next-method
        ;; Translate into a plist and call the general method
        (loop for slot being the hash-value of (structure-slots type)
              for name = (slot-name slot)
              append (list slot-name (slot-value object slot-name)))
        type
        pointer))))

;;; For a class already defined and loaded, and a defcstruct already defined, use
;;; (translation-forms-for-class class type-class)
;;; to connnect the two.  It would be nice to have a macro to do all three simultaneously.
;;; (defmacro define-foreign-structure (class ))

#|
(defmacro define-structure-conversion
    (value-symbol type lisp-class slot-names to-form from-form &optional (struct-name type))
  "Define the functions necessary to convert to and from a foreign structure.  The to-form sets each of the foreign slots in succession, assume the foreign object exists.  The from-form creates the Lisp object, making it with the correct value by reference to foreign slots."
  `(flet ((map-slots (fn val)
            (maphash
             (lambda (name slot-struct)
               (funcall fn (foreign-slot-value val ',type name) (slot-type slot-struct)))
             (slots (follow-typedefs (parse-type ',type))))))
     ;; Convert this to a separate function so it doesn't have to be recomputed on the fly each time.
     (defmethod translate-to-foreign ((,value-symbol ,lisp-class) (type ,type))
       (let ((p (foreign-alloc ',struct-name)))
         ;;(map-slots #'translate-to-foreign ,value-symbol) ; recursive translation of slots
         (with-foreign-slots (,slot-names p ,struct-name)
           ,to-form)
         (values p t))) ; second value is passed to FREE-TRANSLATED-OBJECT
     (defmethod free-translated-object (,value-symbol (p ,type) freep)
       (when freep
         ;; Is this redundant?
         (map-slots #'free-translated-object value) ; recursively free slots
         (foreign-free ,value-symbol)))
     (defmethod translate-from-foreign (,value-symbol (type ,type))
       (with-foreign-slots (,slot-names ,value-symbol ,struct-name)
         ,from-form))))
|#

;;;# Calling Foreign Functions
;;;
;;; FOREIGN-FUNCALL is the main primitive for calling foreign
;;; functions.  It converts each argument based on the installed
;;; translators for its type, then passes the resulting list to
;;; FFI.SYS:%FOREIGN-FUNCALL.
;;;
;;; For implementation-specific reasons, DEFCFUN doesn't use
;;; FOREIGN-FUNCALL directly and might use something else (passed to
;;; TRANSLATE-OBJECTS as the CALL-FORM argument) instead of
;;; FFI.SYS:%FOREIGN-FUNCALL to call the foreign-function.

(defun translate-objects (syms args types rettype call-form &optional indirect)
  "Helper function for FOREIGN-FUNCALL and DEFCFUN.  If 'indirect is T, all arguments are represented by foreign pointers, even those that can be represented by CL objects."
  (if (null args)
      (expand-from-foreign call-form (parse-type rettype))
      (funcall
       (if indirect
           #'expand-to-foreign-dyn-indirect
           #'expand-to-foreign-dyn)
       (car args) (car syms)
       (list (translate-objects (cdr syms) (cdr args)
                                (cdr types) rettype call-form indirect))
       (parse-type (car types)))))

(defun parse-args-and-types (args)
  "Returns 4 values: types, canonicalized types, args and return type."
  (let* ((len (length args))
         (return-type (if (oddp len) (lastcar args) :void)))
    (loop repeat (floor len 2)
          for (type arg) on args by #'cddr
          collect type into types
          collect (canonicalize-foreign-type type) into ctypes
          collect arg into fargs
          finally (return (values types ctypes fargs return-type)))))

;;; While the options passed directly to DEFCFUN/FOREIGN-FUNCALL have
;;; precedence, we also grab its library's options, if possible.
(defun parse-function-options (options &key pointer)
  (destructuring-bind (&key (library :default libraryp)
                            (convention nil))
      options
    (list* :convention
           (or convention
               (when libraryp
                 (let ((lib-options (foreign-library-options
                                     (get-foreign-library library))))
                   (getf lib-options :convention)))
               :cdecl)
           ;; Don't pass the library option if we're dealing with
           ;; FOREIGN-FUNCALL-POINTER.
           (unless pointer
             (list :library library)))))

(defun structure-by-value-p (ctype)
  "A structure or union is to be called or returned by value."
  (let ((actual-type (ensure-parsed-base-type ctype)))
    (or (and (typep actual-type 'foreign-struct-type)
             (not (bare-struct-type-p actual-type))))))

(defun fn-call-by-value-p (argument-types return-type)
  "One or more structures in the arguments or return from the function are called by value."
  (or (some 'structure-by-value-p argument-types)
      (structure-by-value-p return-type)))

(defvar *foreign-structures-by-value*
  (lambda (&rest args)
    (declare (ignore args))
    (error "Unable to call structures by value without cffi-libffi loaded."))
  "A function that produces a form suitable for calling structures by value.")

(defun foreign-funcall-form (thing options args pointerp)
  (multiple-value-bind (types ctypes fargs rettype)
      (parse-args-and-types args)
    (let ((syms (make-gensym-list (length fargs)))
          (fsbvp (fn-call-by-value-p ctypes rettype)))
      (if fsbvp
          ;; Structures by value call through *foreign-structures-by-value*
          (funcall *foreign-structures-by-value*
                   thing
                   fargs
                   syms
                   types
                   rettype
                   ctypes
                   pointerp)
          (translate-objects
           syms fargs types rettype
           `(,(if pointerp '%foreign-funcall-pointer '%foreign-funcall)
             ;; No structures by value, direct call
             ,thing
             (,@(mapcan #'list ctypes syms)
              ,(canonicalize-foreign-type rettype))
             ,@(parse-function-options options :pointer pointerp)))))))

(defmacro foreign-funcall (name-and-options &rest args)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-form name options args nil)))

(defmacro foreign-funcall-pointer (pointer options &rest args)
  (foreign-funcall-form pointer options args t))

(defun promote-varargs-type (builtin-type)
  "Default argument promotions."
  (case builtin-type
    (:float :double)
    ((:char :short) :int)
    ((:unsigned-char :unsigned-short) :unsigned-int)
    (t builtin-type)))

;; If ffi.sys doesn't provide a %foreign-funcall-varargs macros we
;; define one that use %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp '%foreign-funcall-varargs)
    (defmacro %foreign-funcall-varargs (name fixed-args varargs
                                        &rest args &key convention library)
      (declare (ignore convention library))
      `(%foreign-funcall ,name ,(append fixed-args varargs) ,@args)))
  (unless (fboundp '%foreign-funcall-pointer-varargs)
    (defmacro %foreign-funcall-pointer-varargs (pointer fixed-args varargs
                                                &rest args &key convention)
      (declare (ignore convention))
      `(%foreign-funcall-pointer ,pointer ,(append fixed-args varargs) ,@args))))

(defun foreign-funcall-varargs-form (thing options fixed-args varargs pointerp)
  (multiple-value-bind (fixed-types fixed-ctypes fixed-fargs)
      (parse-args-and-types fixed-args)
    (multiple-value-bind (varargs-types varargs-ctypes varargs-fargs rettype)
        (parse-args-and-types varargs)
      (let ((fixed-syms (make-gensym-list (length fixed-fargs)))
            (varargs-syms (make-gensym-list (length varargs-fargs))))
        (translate-objects
         (append fixed-syms varargs-syms)
         (append fixed-fargs varargs-fargs)
         (append fixed-types varargs-types)
         rettype
         `(,(if pointerp '%foreign-funcall-pointer-varargs '%foreign-funcall-varargs)
            ,thing
            ,(mapcan #'list fixed-ctypes fixed-syms)
            ,(append
              (mapcan #'list
                      (mapcar #'promote-varargs-type varargs-ctypes)
                      (loop for sym in varargs-syms
                            and type in varargs-ctypes
                            if (eq type :float)
                              collect `(float ,sym 1.0d0)
                            else collect sym))
              (list (canonicalize-foreign-type rettype)))
            ,@options))))))

(defmacro foreign-funcall-varargs (name-and-options fixed-args
                                   &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL that translates its arguments
and does type promotion for the variadic arguments."
  (let ((name (car (ensure-list name-and-options)))
        (options (cdr (ensure-list name-and-options))))
    (foreign-funcall-varargs-form name options fixed-args varargs nil)))

(defmacro foreign-funcall-pointer-varargs (pointer options fixed-args
                                           &rest varargs)
  "Wrapper around %FOREIGN-FUNCALL-POINTER that translates its
arguments and does type promotion for the variadic arguments."
  (foreign-funcall-varargs-form pointer options fixed-args varargs t))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

;; If ffi.sys doesn't provide a defcfun-helper-forms,
;; we define one that uses %foreign-funcall.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'defcfun-helper-forms)
    (defun defcfun-helper-forms (name lisp-name rettype args types options)
      (declare (ignore lisp-name))
      (values
       '()
       `(%foreign-funcall ,name ,(append (mapcan #'list types args)
                                         (list rettype))
                          ,@options)))))

(defun %defcfun (lisp-name foreign-name return-type args options docstring)
  (let* ((arg-names (mapcar #'first args))
         (arg-types (mapcar #'second args))
         (syms (make-gensym-list (length args)))
         (call-by-value (fn-call-by-value-p arg-types return-type)))
    (multiple-value-bind (prelude caller)
        (if call-by-value
            (values nil nil)
            (defcfun-helper-forms
             foreign-name lisp-name (canonicalize-foreign-type return-type)
             syms (mapcar #'canonicalize-foreign-type arg-types) options))
      `(progn
         ,prelude
         (defun ,lisp-name ,arg-names
           #+cmucl (declare (notinline alien::%heap-alien))
           ,@(ensure-list docstring)
           ,(if call-by-value
                `(foreign-funcall
                  ,(cons foreign-name options)
                  ,@(append (mapcan #'list arg-types arg-names)
                            (list return-type)))
                (translate-objects
                 syms arg-names arg-types return-type caller)))))))

(defun %defcfun-varargs (lisp-name foreign-name return-type args options doc)
  (with-unique-names (varargs)
    (let ((arg-names (mapcar #'car args)))
      `(defmacro ,lisp-name (,@arg-names &rest ,varargs)
         ,@(ensure-list doc)
         `(foreign-funcall-varargs
           ,'(,foreign-name ,@options)
           ,,`(list ,@(loop for (name type) in args
                            collect `',type collect name))
           ,@,varargs
           ,',return-type)))))

(defgeneric translate-underscore-separated-name (name)
  (:method ((name string))
    (values (intern (canonicalize-symbol-name-case (substitute #\- #\_ name)))))
  (:method ((name symbol))
    (substitute #\_ #\- (string-downcase (symbol-name name)))))

(defun collapse-prefix (l special-words)
  (unless (null l)
    (multiple-value-bind (newpre skip) (check-prefix l special-words)
      (cons newpre (collapse-prefix (nthcdr skip l) special-words)))))

(defun check-prefix (l special-words)
  (let ((pl (loop for i from (1- (length l)) downto 0
                  collect (apply #'concatenate 'simple-string (butlast l i)))))
    (loop for w in special-words
          for p = (position-if #'(lambda (s) (string= s w)) pl)
          when p do (return-from check-prefix (values (nth p pl) (1+ p))))
    (values (first l) 1)))

;; TODO library

(defgeneric translate-camelcase-name (name &key upper-initial-p special-words)
  (:method ((name string) &key upper-initial-p special-words)
    (declare (ignore upper-initial-p))
    (values (intern (reduce #'(lambda (s1 s2)
                                (concatenate 'simple-string s1 "-" s2))
                            (mapcar #'string-upcase
                                    (collapse-prefix
                                     (split-if #'(lambda (ch)
                                                   (or (upper-case-p ch)
                                                       (digit-char-p ch)))
                                               name)
                                     special-words))))))
  (:method ((name symbol) &key upper-initial-p special-words)
    (apply #'concatenate
           'string
           (loop for str in (split-if #'(lambda (ch) (eq ch #\-))
                                          (string name)
                                      :elide)
                 for first-word-p = t then nil
                 for e = (member str special-words
                                 :test #'equal :key #'string-upcase)
                 collect (cond
                           ((and first-word-p (not upper-initial-p))
                            (string-downcase str))
                           (e (first e))
                           (t (string-capitalize str)))))))

(defgeneric translate-name-from-foreign (foreign-name package &optional varp)
  (:method (foreign-name package &optional varp)
    (declare (ignore package))
    (let ((sym (translate-underscore-separated-name foreign-name)))
      (if varp
          (values (intern (format nil "*~A*"
                                  (canonicalize-symbol-name-case
                                   (symbol-name sym)))))
          sym))))

(defgeneric translate-name-to-foreign (lisp-name package &optional varp)
  (:method (lisp-name package &optional varp)
    (declare (ignore package))
    (let ((name (translate-underscore-separated-name lisp-name)))
      (if varp
          (string-trim '(#\*) name)
          name))))

(defun lisp-name (spec varp)
  (check-type spec string)
  (translate-name-from-foreign spec *package* varp))

(defun foreign-name (spec varp)
  (check-type spec (and symbol (not null)))
  (translate-name-to-foreign spec *package* varp))

(defun foreign-options (opts varp)
  (if varp
      (funcall 'parse-defcvar-options opts)
      (parse-function-options opts)))

(defun lisp-name-p (name)
  (and name (symbolp name) (not (keywordp name))))

(defun %parse-name-and-options (spec varp)
  (cond
    ((stringp spec)
     (values (lisp-name spec varp) spec nil))
    ((symbolp spec)
     (assert (not (null spec)))
     (values spec (foreign-name spec varp) nil))
    ((and (consp spec) (stringp (first spec)))
     (destructuring-bind (foreign-name &rest options)
         spec
       (cond
         ((or (null options)
              (keywordp (first options)))
          (values (lisp-name foreign-name varp) foreign-name options))
         (t
          (assert (lisp-name-p (first options)))
          (values (first options) foreign-name (rest options))))))
    ((and (consp spec) (lisp-name-p (first spec)))
     (destructuring-bind (lisp-name &rest options)
         spec
       (cond
         ((or (null options)
              (keywordp (first options)))
          (values lisp-name (foreign-name spec varp) options))
         (t
          (assert (stringp (first options)))
          (values lisp-name (first options) (rest options))))))
    (t
     (error "Not a valid foreign function specifier: ~A" spec))))

;;; DEFCFUN's first argument has can have the following syntax:
;;;
;;;     1.  string
;;;     2.  symbol
;;;     3.  \( string [symbol] options* )
;;;     4.  \( symbol [string] options* )
;;;
;;; The string argument denotes the foreign function's name. The
;;; symbol argument is used to name the Lisp function. If one isn't
;;; present, its name is derived from the other. See the user
;;; documentation for an explanation of the derivation rules.
(defun parse-name-and-options (spec &optional varp)
  (multiple-value-bind (lisp-name foreign-name options)
      (%parse-name-and-options spec varp)
    (values lisp-name foreign-name (foreign-options options varp))))

;;; If we find a &REST token at the end of ARGS, it means this is a
;;; varargs foreign function therefore we define a lisp macro using
;;; %DEFCFUN-VARARGS. Otherwise, a lisp function is defined with
;;; %DEFCFUN.
(defmacro defcfun (name-and-options return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((docstring (when (stringp (car args)) (pop args))))
    (multiple-value-bind (lisp-name foreign-name options)
        (parse-name-and-options name-and-options)
      (if (eq (lastcar args) '&rest)
          (%defcfun-varargs lisp-name foreign-name return-type
                            (butlast args) options docstring)
          (%defcfun lisp-name foreign-name return-type args options
                    docstring)))))

;;;# Defining Callbacks

(defun inverse-translate-objects (args types declarations rettype call)
  `(let (,@(loop for arg in args and type in types
                 collect (list arg (expand-from-foreign
                                    arg (parse-type type)))))
     ,@declarations
     ,(expand-to-foreign call (parse-type rettype))))

(defun parse-defcallback-options (options)
  (destructuring-bind (&key (convention :cdecl))
      options
    (list :convention convention)))

(defmacro defcallback (name-and-options return-type args &body body)
  (multiple-value-bind (body declarations)
      (parse-body body :documentation t)
    (let ((arg-names (mapcar #'car args))
          (arg-types (mapcar #'cadr args))
          (name (car (ensure-list name-and-options)))
          (options (cdr (ensure-list name-and-options))))
      `(progn
         (%defcallback ,name ,(canonicalize-foreign-type return-type)
             ,arg-names ,(mapcar #'canonicalize-foreign-type arg-types)
           ,(inverse-translate-objects
             arg-names arg-types declarations return-type
             `(block ,name ,@body))
           ,@(parse-defcallback-options options))
         ',name))))

(declaim (inline get-callback))
(defun get-callback (symbol)
  (%callback symbol))

(defmacro callback (name)
  `(%callback ',name))
