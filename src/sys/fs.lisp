(defpackage #:sys.fs
  (:use
   #:cl
   #:lib.string
   #:lib.type)
  (:shadow
   #:byte)
  (:export
   #:current-dir
   #:dir-p
   #:file-p
   #:list-dir
   #:list-dirs
   #:list-file
   #:runtime-dir
   #:temp-dir
   #:with-current-dir
   #:with-temp-file

   #:copy-file
   #:create-symbolic-link
   #:delete-directory
   #:delete-file*
   #:directory*
   #:ensure-deleted
   #:file-exists-p
   #:list-contents
   #:list-directories
   #:list-files
   #:rename-file*
   #:resolve-symbolic-links
   #:symbolic-link-p

   #:access-time
   #:modification-time
   #:creation-time
   #:group
   #:owner
   #:attributes
   #:*system*
   #:encode-attributes
   #:decode-attributes
   ))

(in-package #:sys.fs)

(defun runtime-dir ()
  (lib.string:join #\/
                   (butlast (lib.string:split #\/
                                              (first sb-ext:*posix-argv*)))))

(defun temp-dir ()
  "Return a default directory to use for temporary files"
  (sys.env:getenv "TMPDIR"))

(defmacro with-temp-file ((name) &body body)
  `(let ((,name (lib.string:concat
                 (temp-dir)
                 (random-string 16)
                 ".tmp")))
     (unwind-protect
          (progn
            ,@body)
       (when (file-exists-p ,name)
         (delete-file* ,name)))))

(defun current-dir ()
  (sb-unix:posix-getcwd/))

(defun (setf current-dir) (dir)
  (sb-posix:chdir dir)
  dir)

(defun call-with-current-dir (function dir)
  (let ((current (current-dir)))
    (if (string= current dir)
        (funcall function)
        (progn
          (setf (current-dir) dir)
          (unwind-protect (funcall function)
            (setf (current-dir) current))))))

(defmacro with-current-dir ((dir) &body body)
  `(call-with-current-dir (lambda () ,@body) ,dir))

(defun dir-p (file)
  (eql :directory (sb-impl::native-file-kind file)))

(defun file-p (file)
  (eql :file (sb-impl::native-file-kind file)))

(defun ensure-deleted (pathname)
  (when (file-exists-p pathname)
    (delete-file* pathname)))

(defun file-exists-p (pathname)
  (ignore-errors
   (probe-file pathname)))

(defun directory* (directory &rest args &key &allow-other-keys)
  (apply #'directory directory :resolve-symlinks NIL args))

(defun list-dir (directory)
  (let (dir)
    (unwind-protect
         (progn
           (setf dir (sb-unix:unix-opendir directory))
           (when dir
             (loop :for ent := (sb-unix:unix-readdir dir nil)
                   :while ent
                   :for name := (sb-unix:unix-dirent-name ent)
                   :when (and (not (string= name "."))
                              (not (string= name "..")))
                     :collect name)))
        (when dir
          (sb-unix:unix-closedir dir nil)))))

(defun list-files (directory)
  (remove-if (lambda (entry)
               (not (file-p (lib.string:join #\/ (list directory entry)))))
             (list-dir directory)))

(defun list-dirs (directory)
  (remove-if (lambda (entry)
               (not (dir-p (lib.string:join #\/ (list directory entry)))))
             (list-dir directory)))

(defun resolve-symbolic-links (pathname)
  (if (or (typep pathname 'logical-pathname)
          (not (absolute-p pathname)))
      pathname
      (or (file-exists-p pathname)
          (normalize-pathname pathname))))

(defun symbolic-link-p (file)
  (eql :symlink (sb-impl::native-file-kind (native-namestring file))))

(defun create-symbolic-link (link-file destination-file)
  (sb-posix:symlink destination-file link-file))

(defun rename-file* (file to)
  (let ((file (to-physical file))
        (to (merge-pathnames (to-physical to)
                             (make-pathname :name :unspecific :type :unspecific))))
    (rename-file file to)))

(defun copy-file (file to &key replace skip-root)
  (cond ((dir-p file)
         (let ((to (if skip-root
                       to
                       (subdirectory to (directory-name file)))))
           (ensure-directories-exist to)
           (dolist (file (list-contents file))
             (copy-file file to :replace replace))))
        (T
         (let ((to (make-pathname :name (pathname-name file)
                                  :type (pathname-type file)
                                  :defaults to)))
           (when (or (not (file-exists-p to))
                     (ecase replace
                       ((T) T)
                       ((NIL) NIL)
                       (:if-newer (< (file-write-date to) (file-write-date file)))))
             (with-open-file (out to :element-type 'u8 :direction :output :if-exists :rename-and-delete)
               (with-open-file (in file :element-type 'u8 :direction :input :if-does-not-exist :error)
                 (let ((buffer (make-array 8096 :element-type 'u8)))
                   (declare (dynamic-extent buffer))
                   (loop for read = (read-sequence buffer in)
                         while (< 0 read)
                         do (write-sequence buffer out :end read))))))))))

(defun delete-directory (file)
  (sb-ext:delete-directory file :recursive T))

(defun delete-file* (file)
  (cond ((dir-p file)
         (delete-directory file))
        (t
         (delete-file file))))

(defmacro define-implementable (name args)
  `(setf (fdefinition ',name)
         (lambda ,args
           (declare (ignore ,@args))
           (error "Not implemented"))))

(defmacro define-implementation (name args &body body)
  `(progn
     (fmakunbound ',name)
     (defun ,name ,args ,@body)))

(define-implementable access-time (file))
(define-implementable (setf access-time) (value file))
(define-implementable modification-time (file))
(define-implementable (setf modification-time) (value file))
(define-implementable creation-time (file))
(define-implementable (setf creation-time) (value file))
(define-implementable group (file))
(define-implementable (setf group) (value file))
(define-implementable owner (file))
(define-implementable (setf owner) (value file))
(define-implementable attributes (file))
(define-implementable (setf attributes) (value file))

(defun enbitfield (list &rest bits)
  (let ((int 0))
    (loop for i from 0
          for bit in bits
          do (when (find bit list) (setf (ldb (cl:byte 1 i) int) 1)))
    int))

(defun debitfield (int &rest bits)
  (loop for i from 0
        for bit in bits
        when (logbitp i int)
        collect bit))

(defvar *system*
  #+unix :unix
  #+windows :windows
  #+mezzano :mezzano
  #-(or unix windows) :unknown)

(defvar *windows-attributes*
  '(:read-only :hidden :system-file NIL :directory :archived :device :normal :temporary :sparse :link :compressed :offline :not-indexed :encrypted :integrity :virtual :no-scrub :recall))

(defvar *unix-attributes*
  '(:other-execute :other-write :other-read :group-execute :group-write :group-read :owner-execute :owner-write :owner-read :sticky :set-group :set-user :fifo :device :directory :normal :link :socket))

(defun decode-bitfield (int bits)
  (loop for i from 0
        for bit in bits
        when bit collect bit
        when bit collect (logbitp i int)))

(defun encode-bitfield (field bits)
  (loop with int = 0
        for i from 0
        for bit in bits
        do (when (getf field bit)
             (setf (ldb (cl:byte 1 i) int) 1))
        finally (return int)))

(defun decode-attributes (attributes &optional (system *system*))
  (case system
    (:unix
     (decode-bitfield attributes *unix-attributes*))
    (:windows
     (decode-bitfield attributes *windows-attributes*))
    (:mezzano
     (append (decode-attributes (ldb (byte 16  0) attributes) :unix)
             (decode-attributes (ldb (byte 16 16) attributes) :windows)))))

(defun encode-attributes (attributes &optional (system *system*))
  (case system
    (:unix
     (encode-bitfield attributes *unix-attributes*))
    (:windows
     (encode-bitfield attributes *windows-attributes*))
    (:mezzano
     (let ((i 0))
       (setf (ldb (byte 16  0) i) (encode-attributes attributes :unix))
       (setf (ldb (byte 16 16) i) (encode-attributes attributes :windows))
       i))
    (T
     0)))

(defun enpath (path)
  (etypecase path
    (string (namestring (truename path)))
    (stream (namestring (truename (pathname path))))
    (pathname (namestring (truename path)))))

;; Linux 5.7.7 AMD64
#+linux
(sys.ffi:defcstruct (stat :size 144)
  (mode    :uint32 :offset 24)
  (uid     :uint32 :offset 28)
  (gid     :uint32 :offset 32)
  (size    :uint64 :offset 48)
  (atime   :uint64 :offset 72)
  (mtime   :uint64 :offset 88))

;; OS X 10.14
#+darwin
(sys.ffi:defcstruct (stat :size 144)
  (mode    :uint16 :offset  4)
  (uid     :uint32 :offset 16)
  (gid     :uint32 :offset 20)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48)
  (size    :uint64 :offset 96))

(sys.ffi:defcfun (cgstat "stat") :int
  (path :string)
  (buffer :pointer))

(sys.ffi:defcfun (cxstat "__xstat") :int
  (path :string)
  (buffer :pointer))

(sys.ffi:defcfun (cutimes "utimes") :int
  (path :string)
  (times :pointer))

(sys.ffi:defcfun (cchown "chown") :int
  (path :string)
  (owner :uint32)
  (group :uint32))

(sys.ffi:defcfun (cchmod "chmod") :int
  (path :string)
  (mode :uint32))

(defun unix->universal (unix)
  (+ unix (encode-universal-time 0 0 0 1 1 1970 0)))

(defun universal->unix (universal)
  (- universal (encode-universal-time 0 0 0 1 1 1970 0)))

(defun cstat (path buffer)
  (cond ((sys.ffi:foreign-symbol-pointer "stat")
         (cgstat path buffer))
        ((sys.ffi:foreign-symbol-pointer "__xstat")
         (cxstat path buffer))
        (T
         1)))

(defun stat (path)
  (sys.ffi:with-foreign-object (ptr '(:struct stat))
    (if (= 0 (cstat (enpath path) ptr))
        (sys.ffi:mem-ref ptr '(:struct stat))
        (error "Stat failed."))))

(defun utimes (path atime mtime)
  (sys.ffi:with-foreign-object (ptr :long 4)
    (setf (sys.ffi:mem-aref ptr :long 0) (universal->unix atime))
    (setf (sys.ffi:mem-aref ptr :long 2) (universal->unix mtime))
    (unless (= 0 (cutimes (enpath path) ptr))
      (error "Utimes failed."))))

(defun chown (path uid gid)
  (cchown (enpath path) uid gid))

(defun chmod (path mode)
  (cchmod (enpath path) mode))

(define-implementation access-time (file)
  (unix->universal (getf (stat file) 'atime)))

(define-implementation (setf access-time) (value file)
  (utimes file value (modification-time file))
  value)

(define-implementation modification-time (file)
  (unix->universal (getf (stat file) 'mtime)))

(define-implementation (setf modification-time) (value file)
  (utimes file (access-time file) value)
  value)

(define-implementation group (file)
  (getf (stat file) 'gid))

(define-implementation (setf group) (value file)
  (chown file (owner file) value)
  value)

(define-implementation owner (file)
  (getf (stat file) 'uid))

(define-implementation (setf owner) (value file)
  (chown file value (group file))
  value)

(define-implementation attributes (file)
  (getf (stat file) 'mode))

(define-implementation (setf attributes) (value file)
  (chmod file value))
