(defpackage :epsilon.sys.fs
  (:use
   :cl
   :epsilon.lib.type)
  (:local-nicknames
   (:env :epsilon.sys.env)
   (:seq :epsilon.lib.sequence)
   (:str :epsilon.lib.string)
   (:uri :epsilon.lib.uri))
  (:shadow
   :byte)
  (:export
   :dir-p
   :file-p
   :home-dir
   :list-dir
   :list-dirs
   :make-dirs
   :exists-p
   :runtime-dir
   :temp-dir
   :with-temp-file
   :replace-extension

   :read-file
 
   :create-symbolic-link
   :delete-directory
   :delete-file*
   :directory*
   :ensure-deleted
   :list-contents
   :list-directories
   :list-files
   :symbolic-link-p

   :access-time
   :modification-time
   :creation-time
   :group
   :owner
   :attributes
   :*system*
   :encode-attributes
   :decode-attributes

   :file=
   :stream=
   :stream-files
   ))

(in-package :epsilon.sys.fs)

(defun parent (file)
  (str:join #\/ (butlast (str:split #\/ file))))

(defun runtime-dir ()
  (parent  (first sb-ext:*posix-argv*)))

(defun temp-dir ()
  "Return a default directory to use for temporary files"
  (env:getenv "TMPDIR"))

(defun home-dir ()
  #+(or linux darwin)
  (sb-unix:uid-homedir (sb-unix:unix-getuid))
  #+(or windows win32)
  (or (env:getenv "USERPROFILE") 
      (str:concat (env:getenv "HOMEDRIVE") (env:getenv "HOMEPATH"))))

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
  #+(or linux darwin)
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
        (sb-unix:unix-closedir dir nil))))
  #+(or windows win32)
  ;; Windows implementation using directory()
  (dolist (entry (directory (pathname (format nil "~a/*.*" dirpath))))
    (let ((name (file-namestring entry)))
      (when (and (not (string= name "."))
                 (not (string= name "..")))
        (funcall f
                 (uri:make-uri :scheme "file"
                               :path (namestring entry)))))))

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
  (str:join #\. (seq:seq (list path-string extension))))

(defun replace-extension (path-string extension)
  (add-extension (delete-extension path-string) extension))

(defun exists-p (uri)
  (not (null (probe-file (uri:path uri)))))

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun make-dirs (uri)
  (loop :with path := ""
        :for component :in (seq:realize
                            (seq:filter (complement #'str:empty-p)
                                        (str:split #\/
                                                   (uri:path uri))))
        :do (setf path (format nil "~a/~a" path component))
            (unless (probe-file path)
              #+(or linux darwin)
              (sb-unix:unix-mkdir path #o775)
              #+(or windows win32)
              (ensure-directories-exist (pathname path)))))
  
(defun list-files (uri extension)
  (let (files)
    (walk-uri uri
              (lambda (entry)
                (push entry files))
              :test (lambda (entry)
                      (and (file-p (uri:path entry))
                           (str:ends-with-p (uri:path entry) extension))))
    (sort files #'string<= :key #'uri:path)))

(defun ensure-deleted (pathname)
  (when (file-p pathname)
    (delete-file* pathname)))

(defun directory* (directory &rest args &key &allow-other-keys)
  (apply #'directory directory :resolve-symlinks NIL args))

(defun list-dir (directory)
  #+(or linux darwin)
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
        (sb-unix:unix-closedir dir nil))))
  #+(or windows win32)
  ;; Windows implementation using directory()
  (mapcar #'file-namestring
          (remove-if (lambda (path)
                       (let ((name (file-namestring path)))
                         (or (string= name ".") (string= name ".."))))
                     (directory (pathname (format nil "~a/*.*" directory))))))

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

(defmacro with-u8-in ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'u8)
     ,@body))

(defmacro with-u8-out ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'u8
                           :direction :output
                           :if-exists :supersede)
     ,@body))

(defun stream-files (f in out)
  (with-u8-in (in in)
    (with-u8-out (out out)
      (funcall f in out))))

(defun stream= (a b)
  (loop :for ab := (read-byte a nil nil)
        :for bb := (read-byte b nil nil)
        :unless (eql ab bb)
          :return nil
        :unless (and ab bb)
          :return t))

(defun file= (a b)
  (with-u8-in (a a)
    (with-u8-in (b b)
      (stream= a b))))

(defun modification-time (path)
  "Return the modification time of the file at PATH as a universal time."
  (handler-case
      (file-info-mtime (sb-posix:stat path))
    (sb-posix:syscall-error ()
      nil)))
