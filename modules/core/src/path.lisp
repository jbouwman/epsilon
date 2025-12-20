;;;; This module provides cross-platform filesystem path manipulation
;;;; with handling of Windows drive letters, Unix absolute paths, and
;;;; platform-specific path separators. Includes filesystem queries.

(defpackage :epsilon.path
  (:use :cl)
  (:export
   ;; Core path type
   :path
   :make-path
   :ensure-path
   :path-string
   :path-absolute-p
   :path-segments
   :path-drive
   
   ;; Path manipulation
   :path-parent
   :path-name
   :path-stem
   :path-extension
   :path-join
   :path-equal
   :path-normalize
   
   ;; Path queries
   :path-exists-p
   :path-file-p
   :path-directory-p
   :path-type
   
   ;; System paths
   :current-directory
   :temp-directory
   :home-directory
   :make-temp-path
   
   ;; Directory operations
   :list-directory
   :walk-directory
   :find-files
   :matches-pattern
   :glob-match
   
   ;; Utilities
   :normalize-separators
   :with-temp-path
   
   ;; URI integration functions
   :path-from-uri
   :path-merge
   :string-path-join
   :uri-merge))

(in-package :epsilon.path)

;;;; Platform detection
(defparameter *platform* 
  #+darwin :darwin
  #+linux :linux  
  #+win32 :windows
  #-(or darwin linux win32) :unknown)

(defparameter *separator*
  #+win32 "\\"
  #-win32 "/")

;;;; ==========================================================================
;;;; Core Path Type
;;;; ==========================================================================

(defclass path ()
  ((segments :initarg :segments :accessor path-segments
             :documentation "List of path components")
   (absolute-p :initarg :absolute-p :accessor path-absolute-p
               :documentation "Whether this is an absolute path")
   (drive :initarg :drive :accessor path-drive
          :documentation "Drive letter on Windows (e.g. 'C'), nil on Unix"))
  (:documentation "Cross-platform filesystem path object"))

(defmethod print-object ((path path) stream)
  (print-unreadable-object (path stream :type t)
    (format stream "~S" (with-slots (segments absolute-p drive) path
                          (concatenate 'string
                                       ;; Drive letter on Windows
                                       #+win32 (if drive (concatenate 'string drive ":") "")
                                       ;; Root separator for absolute paths
                                       (if absolute-p *separator* "")
                                       ;; Join segments with platform separator
                                       (if segments
                                           (reduce (lambda (a b) (concatenate 'string a *separator* b))
                                                   segments)
                                           ""))))))

(defun normalize-separators (path-string)
  "Normalize path separators to forward slashes on all platforms"
  (substitute #\/ #\\ path-string))

(defun make-path (path-string)
  "Create a path object from a string path"
  (when (and path-string (> (length path-string) 0))
    (let* ((normalized (normalize-separators path-string))
           (absolute-p nil)
           (drive nil)
           (segments nil))
      
      ;; Check for Windows drive letter
      #+win32
      (when (and (>= (length normalized) 3)
                 (char= (char normalized 1) #\:)
                 (char= (char normalized 2) #\/))
        (setf drive (string (char normalized 0))
              normalized (subseq normalized 2)
              absolute-p t))
      
      ;; Check for absolute Unix path
      (when (and (> (length normalized) 0)
                 (char= (char normalized 0) #\/))
        (setf absolute-p t
              normalized (subseq normalized 1)))
      
      ;; Split into segments, filtering empty ones
      (setf segments (remove-if (lambda (s) (or (null s) (string= s "")))
                                (if (string= normalized "")
                                    '()
                                    (loop for start = 0 then (1+ pos)
                                          for pos = (position #\/ normalized :start start)
                                          collect (subseq normalized start pos)
                                          while pos))))
      
      (make-instance 'path
                     :segments segments
                     :absolute-p absolute-p
                     :drive drive))))

(defmethod path-string ((path path))
  "Convert path to native string representation"
  (with-slots (segments absolute-p drive) path
    (concatenate 'string
                 ;; Drive letter on Windows
                 #+win32 (if drive (concatenate 'string drive ":") "")
                 ;; Root separator for absolute paths
                 (if absolute-p *separator* "")
                 ;; Join segments with platform separator
                 (if segments
                     (reduce (lambda (a b) (concatenate 'string a *separator* b))
                             segments)
                     ""))))

(defun ensure-path (path-or-string)
  "Coerce input to a path object. If already a path, return as-is. 
   If a string, convert to path. If NIL, return NIL."
  (cond
    ((null path-or-string) nil)
    ((typep path-or-string 'path) path-or-string)
    ((stringp path-or-string) (make-path path-or-string))
    (t (error "Cannot coerce ~S to path" path-or-string))))

;;;; ==========================================================================
;;;; Path Manipulation
;;;; ==========================================================================

(defmethod path-parent ((path path))
  "Get parent directory of this path"
  (with-slots (segments absolute-p drive) path
    (when (or absolute-p (> (length segments) 0))
      (make-instance 'path
                     :segments (butlast segments)
                     :absolute-p absolute-p
                     :drive drive))))

(defmethod path-name ((path path))
  "Get the final component of the path (filename)"
  (with-slots (segments) path
    (when segments
      (first (last segments)))))

(defmethod path-stem ((path path))
  "Get filename without extension"
  (let ((name (path-name path)))
    (when name
      (let ((dot-pos (position #\. name)))
        (if (and dot-pos (> dot-pos 0))
            (subseq name 0 dot-pos)
            name)))))

(defmethod path-extension ((path path))
  "Get file extension (without the dot)"
  (let ((name (path-name path)))
    (when name
      (let ((dot-pos (position #\. name :from-end t)))
        (when (and dot-pos (> dot-pos 0) (< dot-pos (1- (length name))))
          (subseq name (1+ dot-pos)))))))

(defun path-join (&rest components)
  "Join path components into a single path"
  (let ((result-segments '())
        (result-absolute-p nil)
        (result-drive nil))
    
    (loop for component in components do
      (let ((component-path (cond
                             ((typep component 'path) component)
                             ((stringp component) (make-path component))
                             (t (make-path (string component))))))
        (when component-path
          (with-slots (segments absolute-p drive) component-path
            ;; If this component is absolute, it replaces everything before it
            (when absolute-p
              (setf result-segments '()
                    result-absolute-p t
                    result-drive drive))
            ;; Append segments
            (setf result-segments (append result-segments segments))))))
    
    (when (or result-absolute-p result-segments)
      (make-instance 'path
                     :segments result-segments
                     :absolute-p result-absolute-p
                     :drive result-drive))))

(defun path-equal (path1 path2)
  "Compare two paths for equality"
  (let ((p1 (if (typep path1 'path) path1 (make-path path1)))
        (p2 (if (typep path2 'path) path2 (make-path path2))))
    (and p1 p2
         (equal (path-segments p1) (path-segments p2))
         (equal (path-absolute-p p1) (path-absolute-p p2))
         (equal (path-drive p1) (path-drive p2)))))

(defun path-normalize (path)
  "Normalize a path by resolving . and .. components"
  (let ((p (if (typep path 'path) path (make-path path))))
    (when p
      (let ((normalized-segments '()))
        (loop for segment in (path-segments p) do
          (cond
            ((string= segment ".") 
             ;; Skip current directory references
             )
            ((string= segment "..")
             ;; Go up one directory
             (if normalized-segments
                 (setf normalized-segments (nbutlast normalized-segments))
                 (when (not (path-absolute-p p))
                   (setf normalized-segments (append normalized-segments (list segment))))))
            (t
             (setf normalized-segments (append normalized-segments (list segment))))))
        
        (make-instance 'path
                       :segments normalized-segments
                       :absolute-p (path-absolute-p p)
                       :drive (path-drive p))))))

;;;; ==========================================================================
;;;; Path Queries
;;;; ==========================================================================

(defmethod path-exists-p ((path path))
  "Check if path exists on filesystem"
  (probe-file (path-string path)))

(defmethod path-exists-p ((path-string string))
  "Check if string path exists on filesystem"
  (probe-file path-string))

(defmethod path-type ((path path))
  "Get the type of path: :file, :directory, or :none"
  (path-type (path-string path)))

(defmethod path-type ((path-string string))
  "Get the type of string path: :file, :directory, or :none"
  (cond
    ((not (probe-file path-string)) :none)
    (t 
     ;; Use portable directory detection
     (let ((truename (probe-file path-string)))
       (if (and (null (pathname-name truename))
                (null (pathname-type truename))
                (pathname-directory truename))
           :directory
           :file)))))

(defmethod path-file-p ((path path))
  "Check if path is a file"
  (eq (path-type path) :file))

(defmethod path-directory-p ((path path))
  "Check if path is a directory"
  (eq (path-type path) :directory))

;;;; ==========================================================================
;;;; System Paths
;;;; ==========================================================================

(defun current-directory ()
  "Get current working directory as path"
  (make-path 
   #+win32 (namestring (truename "."))
   #-win32 (namestring (truename "."))))

(defun user-directory ()
  (let ((home (sb-ext:posix-getenv "EPSILON_USER")))
    (cond (home
           (make-path home))
          (t
           (current-directory)))))

(defun temp-directory ()
  "Get system temporary directory as path"
  (make-path
   (or #+win32 (or (sb-ext:posix-getenv "TEMP")
                   (sb-ext:posix-getenv "TMP")
                   "C:\\Temp")
       #-win32 (or (sb-ext:posix-getenv "TMPDIR")
                   "/tmp"))))

(defun home-directory ()
  "Get user home directory as path"
  (make-path
   #+win32 (or (sb-ext:posix-getenv "USERPROFILE")
               (concatenate 'string 
                           (or (sb-ext:posix-getenv "HOMEDRIVE") "")
                           (or (sb-ext:posix-getenv "HOMEPATH") "")))
   #-win32 (or (sb-ext:posix-getenv "HOME") "/tmp")))

(defun make-temp-path (&key (suffix ".tmp") (prefix "epsilon-"))
  "Create a temporary file path with optional prefix and suffix"
  (path-join (temp-directory)
             (concatenate 'string prefix 
                         (write-to-string (random 1000000))
                         suffix)))

;;;; ==========================================================================
;;;; Pattern Matching Utilities
;;;; ==========================================================================

(defun glob-match (pattern string &key case-sensitive)
  "Simple glob pattern matching with * and ? support"
  (unless case-sensitive
    (setf pattern (string-downcase pattern)
          string (string-downcase string)))
  
  (labels ((match-chars (p s)
             (cond
               ((null p) (null s))
               ((null s) (every (lambda (c) (char= c #\*)) p))
               ((char= (first p) #\*)
                (or (match-chars (rest p) s)
                    (match-chars p (rest s))))
               ((char= (first p) #\?)
                (match-chars (rest p) (rest s)))
               ((char= (first p) (first s))
                (match-chars (rest p) (rest s)))
               (t nil))))
    (match-chars (coerce pattern 'list) (coerce string 'list))))

;;;; ==========================================================================
;;;; Directory Operations
;;;; ==========================================================================

(defun list-directory (path &key (type :all))
  "List directory contents, returning path objects. TYPE can be :all, :files, :directories"
  (let ((dir-path (if (typep path 'path) path (make-path path))))
    (when (and dir-path (path-directory-p dir-path))
      (let* ((path-string (path-string dir-path))
             ;; SBCL needs different patterns for files vs directories:
             ;; - path/* matches files only
             ;; - path/*/ matches directories only
             (file-pattern (concatenate 'string path-string *separator* "*"))
             (dir-pattern (concatenate 'string path-string *separator* "*" *separator*))
             (results '()))

        (handler-case
            (progn
              ;; Search for files (unless only directories requested)
              (unless (eq type :directories)
                (dolist (entry (directory file-pattern))
                  (let* ((entry-pathname (pathname entry))
                         (entry-name (when (pathname-name entry-pathname)
                                       (if (pathname-type entry-pathname)
                                           (concatenate 'string
                                                        (pathname-name entry-pathname)
                                                        "."
                                                        (pathname-type entry-pathname))
                                           (pathname-name entry-pathname)))))
                    (when (and entry-name
                               (not (string= entry-name "."))
                               (not (string= entry-name "..")))
                      (push (path-join dir-path entry-name) results)))))

              ;; Search for directories (unless only files requested)
              (unless (eq type :files)
                (dolist (entry (directory dir-pattern))
                  (let* ((entry-pathname (pathname entry))
                         (entry-name (car (last (pathname-directory entry-pathname)))))
                    (when (and entry-name
                               (stringp entry-name)
                               (not (string= entry-name "."))
                               (not (string= entry-name "..")))
                      (push (path-join dir-path entry-name) results))))))
          (error () nil))

        (nreverse results)))))

(defun walk-directory (root-path &key recursive (type :all) filter)
  "Walk directory tree yielding path objects. FILTER is a function of path -> boolean"
  (let ((to-visit (list (if (typep root-path 'path) 
                            root-path 
                            (make-path root-path))))
        (results '()))
    
    (loop while to-visit do
      (let* ((current (pop to-visit))
             (current-type (path-type current)))
        
        ;; Add current path if it matches criteria
        (when (and current
                   (or (eq type :all)
                       (and (eq type :files) (eq current-type :file))
                       (and (eq type :directories) (eq current-type :directory)))
                   (or (null filter) (funcall filter current)))
          (push current results))
        
        ;; If recursive and current is a directory, add children to visit
        (when (and recursive (eq current-type :directory))
          (let ((children (list-directory current :type :all)))
            ;; Add children to front of queue for depth-first traversal
            (setf to-visit (append children to-visit))))))
    
    (nreverse results)))

(defun matches-pattern (path pattern &key case-sensitive)
  "Check if path matches glob pattern (*, ?, etc.)"
  (when (null case-sensitive)
    (setf case-sensitive (eq *platform* :linux))) ; Case-sensitive on Linux, not on others
  
  (let ((name (if (typep path 'path) 
                  (path-name path)
                  (path-name (make-path path)))))
    (when name
      (glob-match pattern name :case-sensitive case-sensitive))))

(defun find-files (root-path &key patterns recursive (case-sensitive nil))
  "Find files matching patterns. PATTERNS can be a string or list of glob patterns"
  (let ((pattern-list (if (listp patterns) patterns (list patterns))))
    (walk-directory root-path
                    :recursive recursive
                    :type :files
                    :filter (lambda (path)
                              (some (lambda (pattern)
                                      (matches-pattern path pattern 
                                                      :case-sensitive case-sensitive))
                                    pattern-list)))))

;;;; ==========================================================================
;;;; URI Integration Functions
;;;; ==========================================================================

(defun path-from-uri (uri-obj)
  "Extract path string from a URI/URL object"
  (cond
    ;; Handle string paths directly
    ((stringp uri-obj) uri-obj)
    ;; Handle path objects
    ((typep uri-obj 'path) (path-string uri-obj))
    (t (error "Cannot extract path from object: ~A" uri-obj))))

(defun path-merge (base-path relative-path)
  "Merge a base path with a relative path component"
  (let ((base (if (typep base-path 'path) base-path (make-path base-path)))
        (rel (if (stringp relative-path) relative-path (string relative-path))))
    (path-join base rel)))

(defun string-path-join (&rest components)
  "Join path components into a string (like uri:path-join)"
  (when components
    (let ((result (first components)))
      (dolist (component (rest components))
        (when (and component (> (length component) 0))
          (let ((needs-separator (and (> (length result) 0)
                                      (not (char= (char result (1- (length result))) #\/))
                                      (not (char= (char component 0) #\/)))))
            (setf result (concatenate 'string 
                                      result
                                      (if needs-separator "/" "")
                                      component)))))
      result)))

(defun uri-merge (base-path relative-path)
  "Merge base path with relative path, returning a new path (transitional function)"
  (let ((base-str (cond
                   ((typep base-path 'path) (path-string base-path))
                   ((stringp base-path) base-path)
                   (t (path-from-uri base-path))))
        (relative-str (if (stringp relative-path) relative-path (string relative-path))))
    (make-path (string-path-join base-str relative-str))))

;;;; ==========================================================================
;;;; Utilities
;;;; ==========================================================================

(defmacro with-temp-path ((var &key suffix prefix) &body body)
  "Execute body with a temporary path bound to var"
  `(let ((,var (make-temp-path :suffix ,suffix :prefix ,prefix)))
     (unwind-protect
          (progn ,@body)
       (when (path-exists-p ,var)
         (delete-file (path-string ,var))))))
