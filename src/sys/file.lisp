(defpackage #:sys.file
  (:use
   #:cl)
  (:shadow
   #:byte)
  (:local-nicknames
   (#:ffi #:sys.ffi))
  ;; protocol.lisp
  (:export
   #:access-time
   #:modification-time
   #:creation-time
   #:group
   #:owner
   #:attributes
   #:*system*
   #:encode-attributes
   #:decode-attributes))

(in-package #:sys.file)

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
(ffi:defcstruct (stat :size 144)
  (mode    :uint32 :offset 24)
  (uid     :uint32 :offset 28)
  (gid     :uint32 :offset 32)
  (size    :uint64 :offset 48)
  (atime   :uint64 :offset 72)
  (mtime   :uint64 :offset 88))

;; OS X 10.14
#+darwin
(ffi:defcstruct (stat :size 144)
  (mode    :uint16 :offset  4)
  (uid     :uint32 :offset 16)
  (gid     :uint32 :offset 20)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48)
  (size    :uint64 :offset 96))

;; FreeBSD 12.1 AMD64
#+freebsd
(ffi:defcstruct (stat :size 224)
  (mode    :uint16 :offset 24)
  (uid     :uint32 :offset 28)
  (gid     :uint32 :offset 32)
  (size    :uint64 :offset 112)
  (atime   :uint64 :offset 48)
  (mtime   :uint64 :offset 64))

;; OpenBSD 7.1 AMD64
#+openbsd
(ffi:defcstruct (stat :size 128)
  (mode    :uint32 :offset  0)
  (uid     :uint32 :offset 20)
  (gid     :uint32 :offset 24)
  (size    :uint64 :offset 80)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48))

(ffi:defcfun (cgstat "stat") :int
  (path :string)
  (buffer :pointer))

(ffi:defcfun (cxstat "__xstat") :int
  (path :string)
  (buffer :pointer))

(ffi:defcfun (cutimes "utimes") :int
  (path :string)
  (times :pointer))

(ffi:defcfun (cchown "chown") :int
  (path :string)
  (owner :uint32)
  (group :uint32))

(ffi:defcfun (cchmod "chmod") :int
  (path :string)
  (mode :uint32))

(defun unix->universal (unix)
  (+ unix (encode-universal-time 0 0 0 1 1 1970 0)))

(defun universal->unix (universal)
  (- universal (encode-universal-time 0 0 0 1 1 1970 0)))

(defun cstat (path buffer)
  (cond ((ffi:foreign-symbol-pointer "stat")
         (cgstat path buffer))
        ((ffi:foreign-symbol-pointer "__xstat")
         (cxstat path buffer))
        (T
         1)))

(defun stat (path)
  (ffi:with-foreign-object (ptr '(:struct stat))
    (if (= 0 (cstat (enpath path) ptr))
        (ffi:mem-ref ptr '(:struct stat))
        (error "Stat failed."))))

(defun utimes (path atime mtime)
  (ffi:with-foreign-object (ptr :long 4)
    (setf (ffi:mem-aref ptr :long 0) (universal->unix atime))
    (setf (ffi:mem-aref ptr :long 2) (universal->unix mtime))
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
