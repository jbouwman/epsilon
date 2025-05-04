(defpackage #:epsilon.sys.fs
  (:use
   #:cl
   #:epsilon.lib.type)
  (:local-nicknames
   (:ffi :epsilon.sys.ffi)
   (:str :epsilon.lib.string)
   (:uri :epsilon.lib.uri))
  (:shadow
   #:byte)
  (:export
   #:current-dir
   #:dir-p
   #:file-p
   #:home-dir
   #:list-dir
   #:list-dirs
   #:make-dirs
   #:exists-p
   #:runtime-dir
   #:temp-dir
   #:with-current-dir
   #:with-temp-file
   #:replace-extension
   
   #:create-symbolic-link
   #:delete-directory
   #:delete-file*
   #:directory*
   #:ensure-deleted
   #:list-contents
   #:list-directories
   #:list-files
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

(in-package #:epsilon.sys.fs)

(defun parent (file)
  (str:join #\/ (butlast (str:split #\/ file))))

(defun runtime-dir ()
  (parent  (first sb-ext:*posix-argv*)))

(defun temp-dir ()
  "Return a default directory to use for temporary files"
  (epsilon.sys.env:getenv "TMPDIR"))

(defun home-dir ()
  (sb-unix:uid-homedir (sb-unix:unix-getuid)))

(defmacro with-temp-file ((name) &body body)
  `(let ((,name (str:concat
                 (temp-dir)
                 (str:random-string 16)
                 ".tmp")))
     (unwind-protect
          (progn
            ,@body)
       (when (file-p ,name)
         (delete-file* ,name)))))

;; TODO replace other usages of LPs with URLs

(defun current-dir ()
  (uri:make-uri :scheme "file"
                :path (sb-unix:posix-getcwd/)))

(defun (setf current-dir) (dir)
  (sb-posix:chdir dir)
  dir)

(defun file-info (path)
  (sb-posix:stat path))

(defun dir-p (path)
  (handler-case
      (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat path)))
    (sb-posix:syscall-error ()
      nil)))

(defun file-p (path)
  (handler-case
      (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat path)))
    (sb-posix:syscall-error ()
      nil)))

(defun file-info-type (stat)
  (cond ((sb-posix:s-isreg (sb-posix:stat-mode stat)) :file)
        ((sb-posix:s-isdir (sb-posix:stat-mode stat)) :directory)
        ((sb-posix:s-islnk (sb-posix:stat-mode stat)) :link)))

(defun file-info-size (stat)
  (sb-posix:stat-size stat))

(defun file-info-mtime (stat)
  (sb-posix:stat-mtime stat))

(defun file-info-atime (stat)
  (sb-posix:stat-atime stat))

(defun file-info-ctime (stat)
  (sb-posix:stat-ctime stat))

(defun file-info-mode (stat)
  (sb-posix:stat-mode stat))

(defun file-info-uid (stat)
  (sb-posix:stat-uid stat))

(defun file-info-gid (stat)
  (sb-posix:stat-gid stat))

(defun %walk-dir (dirpath f)
  (let (dir)
    (unwind-protect
         (progn
           (setf dir (sb-unix:unix-opendir dirpath))
           (when dir
             (loop :for ent := (sb-unix:unix-readdir dir nil)
                   :while ent
                   :for name := (sb-unix:unix-dirent-name ent)
                   :when (and (not (string= name "."))
                              (not (string= name "..")))
                     :do (funcall f
                                  (uri:make-uri :scheme "file"
                                                :path (format nil "~a/~a" dirpath name))))))
      (when dir
        (sb-unix:unix-closedir dir nil)))))

(defun walk-uri (uri f &key (recursive t) (test (constantly t)))
  (%walk-dir (uri:path uri)
             (lambda (entry)
               (when (funcall test entry)
                 (funcall f entry))
               (when (and recursive (dir-p (uri:path entry)))
                 (walk-uri entry f :recursive t :test test)))))

(defun delete-extension (path-string)
  (subseq path-string 0 (position #\. path-string :from-end t)))

(defun add-extension (path-string extension)
  (str:join #\. (list path-string extension)))

(defun replace-extension (path-string extension)
  (add-extension (delete-extension path-string) extension))

(defun exists-p (uri)
  (not (null (probe-file (uri:path uri)))))

(defun make-dirs (uri)
  (loop :with path := ""
        :for component :in (remove-if #'str:empty-p
                                      (str:split #\/
                                                 (uri:path uri)))
        :do (setf path (format nil "~a/~a" path component))
            (unless (probe-file path)
              (sb-unix:unix-mkdir path #o775))))

#++
(make-dirs (uri:uri "file:///tmp/hello/goodbye"))




(defun list-files (uri extension)
  (let (files)
    (walk-uri uri
              (lambda (entry)
                (push entry files))
              :test (lambda (entry)
                      (and (file-p (uri:path entry))
                           (str:ends-with-p (uri:path entry) extension))))
    (sort files #'string<= :key #'uri:path)))

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

(defun ensure-deleted (pathname)
  (when (file-p pathname)
    (delete-file* pathname)))

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

(defun symbolic-link-p (file)
  (eql :symlink (sb-impl::native-file-kind file)))

(defun create-symbolic-link (link-file destination-file)
  (sb-posix:symlink destination-file link-file))

(defun delete-directory (file)
  (sb-ext:delete-directory file :recursive T))

(defun delete-file* (file)
  (cond ((dir-p file)
         (delete-directory file))
        (t
         (delete-file file))))

(defvar *system*
  #+unix :unix
  #+windows :windows
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
     (decode-bitfield attributes *windows-attributes*))))

(defun encode-attributes (attributes &optional (system *system*))
  (case system
    (:unix
     (encode-bitfield attributes *unix-attributes*))
    (:windows
     (encode-bitfield attributes *windows-attributes*))
    (T
     0)))

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
(ffi:defcstruct (stat :size 144)
  (mode    :uint16 :offset  4)
  (uid     :uint32 :offset 16)
  (gid     :uint32 :offset 20)
  (atime   :uint64 :offset 32)
  (mtime   :uint64 :offset 48)
  (size    :uint64 :offset 96))

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
    (when (= 0 (cstat path ptr))
      (ffi:mem-ref ptr '(:struct stat)))))

(defun utimes (path atime mtime)
  (ffi:with-foreign-object (ptr :long 4)
    (setf (ffi:mem-aref ptr :long 0) (universal->unix atime))
    (setf (ffi:mem-aref ptr :long 2) (universal->unix mtime))
    (unless (= 0 (cutimes path ptr))
      (error "Utimes failed."))))

(defun chown (path uid gid)
  (cchown path uid gid))

(defun chmod (path mode)
  (cchmod path mode))

(defun access-time (file)
  (unix->universal (getf (stat file) 'atime)))

(defun (setf access-time) (value file)
  (utimes file value (modification-time file))
  value)

(defun modification-time (file)
  (unix->universal (getf (stat file) 'mtime)))

(defun (setf modification-time) (value file)
  (utimes file (access-time file) value)
  value)

(defun group (file)
  (getf (stat file) 'gid))

(defun (setf group) (value file)
  (chown file (owner file) value)
  value)

(defun owner (file)
  (getf (stat file) 'uid))

(defun (setf owner) (value file)
  (chown file value (group file))
  value)

(defun attributes (file)
  (let ((s (stat file)))
    (when s
      (getf s 'mode))))

(defun (setf attributes) (value file)
  (chmod file value))
