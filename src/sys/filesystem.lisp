(defpackage #:sys.filesystem
  (:use #:cl)
  (:export
   #:runtime-directory
   #:temporary-directory
   #:make-temporary-file
   #:with-temporary-file
   #:current-directory
   #:with-current-directory
   #:ensure-deleted
   #:truename*
   #:file-exists-p
   #:directory*
   #:list-contents
   #:list-files
   #:list-directories
   #:list-hosts
   #:list-devices
   #:resolve-symbolic-links
   #:directory-p
   #:file-p
   #:symbolic-link-p
   #:create-symbolic-link
   #:rename-file*
   #:copy-file
   #:delete-directory
   #:delete-file*))

(in-package #:sys.filesystem)

;;; Support
#+(and sbcl win32)
(defun string->wstring (string)
  (let* ((count (sb-alien:alien-funcall (sb-alien:extern-alien "MultiByteToWideChar" (function sb-alien:int sb-alien:int (integer 32) sb-alien:c-string sb-alien:int sb-alien:int sb-alien:int))
                                        65001 0 string -1 0 0))
         (ptr (sb-alien:make-alien sb-alien:short count)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "MultiByteToWideChar" (function sb-alien:int sb-alien:int (integer 32) sb-alien:c-string sb-alien:int (* T) sb-alien:int))
                            65001 0 string -1 ptr count)
    ptr))

#+(and sbcl win32)
(defun wstring->string (pointer)
  (let* ((count (sb-alien:alien-funcall (sb-alien:extern-alien "WideCharToMultiByte" (function sb-alien:int sb-alien:int (integer 32) (* T) sb-alien:int sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
                                        65001 0 pointer -1 0 0 0 0))
         (string (sb-alien:make-alien sb-alien:char count)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "WideCharToMultiByte" (function sb-alien:int sb-alien:int (integer 32) (* T) sb-alien:int (* T) sb-alien:int sb-alien:int sb-alien:int))
                            65001 0 pointer -1 string count 0 0)
    (prog1 (sb-alien:cast string sb-alien:c-string)
      (sb-alien:free-alien string))))

(defun runtime-directory ()
  (sys.path:to-directory
   (sys.path:parse-native-namestring
    (first sb-ext:*posix-argv*))))

(defun temporary-directory ()
  (sys.path:parse-native-namestring
   (or #+windows (sys.path::getenv "TEMP")
       #+darwin (sys.path::getenv "TMPDIR")
       #+linux (sys.path::getenv "XDG_RUNTIME_DIR")
       #+windows "~/AppData/Local/Temp"
       #-windows "/tmp")
   :as :directory))

(defun make-temporary-file (&key (name NIL name-p) (type "dat"))
  (if name-p
      (make-pathname :name name :type type :defaults (temporary-directory))
      (loop for path = (make-pathname :name (format NIL "~36r-~36r" (get-universal-time) (random #xFFFFFFFFFFFFFFFF))
                                      :type type :defaults (temporary-directory))
            do (unless (file-exists-p path) (return path)))))

(defun call-with-temporary-file (function &rest args)
  (let ((path (apply #'make-temporary-file args)))
    (ensure-directories-exist path)
    (unwind-protect (funcall function path)
      (ensure-deleted path))))

(defmacro with-temporary-file ((path &rest args) &body body)
  `(call-with-temporary-file (lambda (,path) ,@body) ,@args))

(defun ensure-deleted (pathname)
  (when (file-exists-p pathname)
    (delete-file* pathname)))

(defun truename* (pathname)
  (let ((pathname (sys.path:pathname* pathname)))
    (or (ignore-errors (truename pathname))
        ;; this is here because trying to find the truename of a directory pathname WITHOUT supplying
        ;; a trailing directory separator, causes an error on some lisps.
        #+(or clisp gcl) (ignore-errors (truename (sys.path:force-directory pathname))))))

(defun file-exists-p (pathname)
  (ignore-errors
   (let ((pathname (sys.path:to-physical pathname)))
     (probe-file pathname))))

(defun directory* (directory &rest args &key &allow-other-keys)
  (apply #'directory directory :resolve-symlinks NIL args))

(defun list-contents (directory)
  ;; FIXME: This sucks
  (nconc (list-files directory)
         (list-directories directory)))

(defun list-files (directory)
  (let* ((directory (sys.path:pathname* directory))
         (entries (ignore-errors (directory* (merge-pathnames sys.path:*wild-file* directory)))))
    (remove-if #'directory-p entries)))

(defun list-directories (directory)
  (let* ((directory (sys.path:to-directory directory)))
    (let* (#-(or abcl cormanlisp genera xcl)
           (wild (merge-pathnames
                  #-(or abcl allegro cmucl lispworks sbcl scl xcl)
                  sys.path:*wild-directory*
                  #+(or abcl allegro cmucl lispworks sbcl scl xcl) "*.*"
                  directory))
           (dirs
             #+(or abcl xcl) (system:list-directory directory)
             #+cormanlisp (cl::directory-subdirs directory)
             #+genera (handler-case (loop for (p . k) in (fs:directory-list directory)
                                          when (eql :directory k) collect p)
                        (fs:directory-not-found () nil))
             #+clozure (ignore-errors (directory* wild :directories T :files NIL))
             #+mcl (ignore-errors (directory* wild :directories T))
             #-(or abcl xcl cormanlisp genera clozure mcl) (directory* wild)))
      (loop for path in dirs
            when (directory-p path)
            collect (sys.path:force-directory path)))))

(defun list-hosts ()
  (when (pathname-host *default-pathname-defaults*)
    (list (pathname-host *default-pathname-defaults*))))

(defun list-devices (&optional host)
  (declare (ignore host))
  #+(or windows win32 ms-windows)
  (progn
    #+sbcl (sb-alien:with-alien ((strings (array (integer 16) 1024)))
             (let ((count (sb-alien:alien-funcall (sb-alien:extern-alien "GetLogicalDriveStringsW" (function (integer 32) (integer 32) (array (integer 16) 1024)))
                                                  1024 strings))
                   (base (sb-sys:sap-int (sb-alien:alien-sap strings)))
                   (start 0)
                   (devices ()))
               (dotimes (i count devices)
                 (when (= 0 (sb-alien:deref strings i))
                   (push (string-right-trim ":\\" (wstring->string (sb-sys:int-sap (+ base (* 2 start))))) devices)
                   (setf start (1+ i))))))))

(defun resolve-symbolic-links (pathname)
  #-allegro
  (if (or (typep pathname 'logical-pathname)
          (not (sys.path:absolute-p pathname)))
      pathname
      (or (file-exists-p pathname)
          (sys.path:normalize-pathname pathname)))
  #+allegro
  (if (physical-pathname-p pathname)
      (or (ignore-errors (excl:pathname-resolve-symbolic-links pathname)) pathname)
      pathname))

(defun directory-p (file)
  #+(or abcl xcl) (extensions:probe-directory file)
  #+allegro (excl:probe-directory file)
  #+clozure (ccl:directoryp file)
  #+cmucl (= #o040000 (logand #o170000 (nth-value 3 (unix:unix-stat (namestring file)))))
  #+(or clasp ecl mkcl) (eql :directory (ext:file-kind file NIL))
  #+sbcl (eql :directory (sb-impl::native-file-kind (namestring (truename file))))
  #+lispworks (lw:file-directory-p x)
  #-(or abcl xcl allegro clasp clozure cmucl ecl mkcl sbcl lispworks)
  (sys.path:directory-p file))

(defun file-p (file)
  #+clozure (eql :file (ccl::%file-kind (nth-value 1 (ccl::%stat (namestring file)))))
  #+cmucl (= #o0100000 (logand #o170000 (nth-value 3 (unix:unix-stat (namestring file)))))
  #+(or clasp ecl mkcl) (eql :file (ext:file-kind file NIL))
  #+sbcl (eql :file (sb-impl::native-file-kind (namestring (truename file))))
  #-(or clasp clozure cmucl ecl mkcl sbcl)
  (and (not (directory-p file))
       (not (symbolic-link-p file))))

(defun symbolic-link-p (file)
  #+clozure (ccl::path-is-link file)
  #+cmucl (= #o120000 (logand #o170000 (nth-value 3 (unix:unix-stat (sys.path:native-namestring file)))))
  #+(or clasp ecl mkcl) (eql :link (ext:file-kind file NIL))
  #+sbcl (eql :symlink (sb-impl::native-file-kind (sys.path:native-namestring file)))
  ;; Try to guess by resolving the file and the directory of it separately.
  #-(or clasp clozure ecl cmucl mkcl sbcl)
  (string/= (namestring (resolve-symbolic-links file))
            (namestring (merge-pathnames (resolve-symbolic-links (sys.path:to-directory file)) file))))

(defun create-symbolic-link (link-file destination-file)
  #+(and sbcl unix) (sb-posix:symlink destination-file link-file)
  #+(and sbcl win32) (let ((src (string->wstring (sys.path:native-namestring link-file)))
                           (dst (string->wstring (sys.path:native-namestring destination-file))))
                       (unwind-protect (when (= 0 (sb-alien:alien-funcall (sb-alien:extern-alien "CreateSymbolicLinkW" (function (integer 32) (* (integer 16)) (* (integer 16)) (integer 32)))
                                                                          src dst (if (directory-p destination-file) #x3 #x2)))
                                         (error "Failed to create symlink."))
                         (sb-alien:free-alien src)
                         (sb-alien:free-alien dst)))
  #-sbcl (error "Cannot create symbolic links."))

(defun rename-file* (file to)
  (let ((file (sys.path:to-physical file))
        (to (merge-pathnames (sys.path:to-physical to)
                             (make-pathname :name :unspecific :type :unspecific))))
    (rename-file file to)))

(defun copy-file (file to &key replace skip-root)
  (cond ((directory-p file)
         (let ((to (if skip-root
                       to
                       (sys.path:subdirectory to (sys.path:directory-name file)))))
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
             (with-open-file (out to :element-type '(unsigned-byte 8) :direction :output :if-exists :rename-and-delete)
               (with-open-file (in file :element-type '(unsigned-byte 8) :direction :input :if-does-not-exist :error)
                 (let ((buffer (make-array 8096 :element-type '(unsigned-byte 8))))
                   (declare (dynamic-extent buffer))
                   (loop for read = (read-sequence buffer in)
                         while (< 0 read)
                         do (write-sequence buffer out :end read))))))))))

(defun delete-directory (file)
  (sb-ext:delete-directory file :recursive T))

(defun delete-file* (file)
  (cond ((directory-p file)
         (delete-directory file))
        (T
         (delete-file file))))
