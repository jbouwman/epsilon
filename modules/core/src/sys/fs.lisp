;;;; This module provides low-level filesystem operations using POSIX system calls
;;;; via sb-posix. Handles file I/O, directory operations, permissions, and
;;;; metadata queries.

(defpackage :epsilon.sys.fs
  (:use
   :cl
   :epsilon.type)
  (:local-nicknames
   (:env :epsilon.sys.env)
   (:seq :epsilon.sequence)
   (:str :epsilon.string)
   (:path :epsilon.path))
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
   :copy-file
   :copy-directory
   :change-directory
   :current-directory

   :read-file
   :read-file-string
   :read-file-bytes
   :write-file-string
   :write-file-bytes
 
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

(defun normalize-path-separators (path)
  "Normalize path separators to forward slashes on all platforms"
  (substitute #\/ #\\ path))

(defun parent (file)
  (let ((normalized (normalize-path-separators file)))
    (str:join #\/ (butlast (str:split #\/ normalized)))))

(defun runtime-dir ()
  (parent (normalize-path-separators (first sb-ext:*posix-argv*))))

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
  `(let ((,name (path:string-path-join
                 (temp-dir)
                 (str:concat (str:random-string 16) ".tmp"))))
     (unwind-protect
          (progn
            ,@body)
       (when (file-p ,name)
         (delete-file* ,name)))))

#+win32
(defun directory-pathname-p (pathname)
  "Check if pathname refers to a directory on Windows"
  (let ((namestring (namestring pathname)))
    (or (char= (char namestring (1- (length namestring))) #\\)
        (char= (char namestring (1- (length namestring))) #\/)
        (and (probe-file pathname)
             (not (pathname-name pathname))))))

(defun file-info (path)
  #-win32 (sb-posix:stat path)
  #+win32 (let ((truepath (probe-file path)))
            (when truepath
              (list :path (namestring truepath)
                    :type (if (directory-pathname-p truepath) :directory :file)))))

(defun dir-p (path)
  (handler-case
      #-win32 (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat path)))
      #+win32 (let ((truepath (probe-file path)))
                (and truepath (directory-pathname-p truepath)))
    #-win32 (sb-posix:syscall-error () nil)
    #+win32 (error () nil)))

(defun file-p (path)
  (handler-case
      #-win32 (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat path)))
      #+win32 (let ((truepath (probe-file path)))
                (and truepath (not (directory-pathname-p truepath))))
    #-win32 (sb-posix:syscall-error () nil)
    #+win32 (error () nil)))

(defun file-info-type (stat)
  #-win32 (cond ((sb-posix:s-isreg (sb-posix:stat-mode stat)) :file)
                ((sb-posix:s-isdir (sb-posix:stat-mode stat)) :directory)
                ((sb-posix:s-islnk (sb-posix:stat-mode stat)) :link))
  #+win32 (if (listp stat) 
              (getf stat :type)
              :file))

(defun file-info-size (stat)
  #-win32 (sb-posix:stat-size stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need file size lookup

(defun file-info-mtime (stat)
  #-win32 (sb-posix:stat-mtime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need mtime lookup

(defun file-info-atime (stat)
  #-win32 (sb-posix:stat-atime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need atime lookup

(defun file-info-ctime (stat)
  #-win32 (sb-posix:stat-ctime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need ctime lookup

(defun file-info-mode (stat)
  #-win32 (sb-posix:stat-mode stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need mode lookup

(defun file-info-uid (stat)
  #-win32 (sb-posix:stat-uid stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need uid lookup

(defun file-info-gid (stat)
  #-win32 (sb-posix:stat-gid stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need gid lookup

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
                                  (path:string-path-join dirpath name)))))
      (when dir
        (sb-unix:unix-closedir dir nil))))
  #+(or windows win32)
  ;; Windows implementation using directory()
  (let ((pattern (if (and (> (length dirpath) 0)
                          (char= (char dirpath (1- (length dirpath))) #\\))
                     (concatenate 'string dirpath "*.*")
                     (concatenate 'string dirpath "\\*.*"))))
    (handler-case
        (dolist (entry (directory pattern))
          (let ((name (file-namestring entry)))
            (when (and (not (string= name "."))
                       (not (string= name "..")))
              (funcall f (namestring entry)))))
      (error () nil))))

(defun walk-path (path-string f &key (recursive t) (test (constantly t)))
  (%walk-dir (if (stringp path-string)
                 path-string
                 (path:path-from-uri path-string))
             (lambda (entry-path)
               (when (funcall test entry-path)
                 (funcall f entry-path))
               (when (and recursive (dir-p entry-path))
                 (walk-path entry-path f :recursive t :test test)))))

(defun delete-extension (path-string)
  (subseq path-string 0 (position #\. path-string :from-end t)))

(defun add-extension (path-string extension)
  (str:join #\. (seq:seq (list path-string extension))))

(defun replace-extension (path-string extension)
  (add-extension (delete-extension path-string) extension))

(defun exists-p (path-string)
  (not (null (probe-file (if (stringp path-string)
                             path-string
                             (path:path-from-uri path-string))))))

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun read-file-string (filename)
  "Read file contents as a string (alias for read-file)"
  (read-file filename))

(defun read-file-bytes (filename)
  "Read file contents as a simple byte array compatible with digest functions"
  (with-open-file (stream filename :direction :input :element-type '(unsigned-byte 8))
    (let ((contents (make-array (file-length stream) 
                                :element-type '(unsigned-byte 8) 
                                :fill-pointer nil 
                                :adjustable nil)))
      (read-sequence contents stream)
      contents)))

(defun write-file-string (filename content)
  "Write string content to file"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string content stream)))

(defun write-file-bytes (filename content)
  "Write byte array content to file"
  (with-open-file (stream filename :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence content stream)))

(defun make-dirs (path-string)
  (let ((full-path (normalize-path-separators (if (stringp path-string)
                                                  path-string
                                                  (path:path-from-uri path-string)))))
    (when (and full-path (> (length full-path) 0))
      #+(or linux darwin)
      (let ((is-absolute (char= (char full-path 0) #\/))
            (components (seq:realize
                         (seq:filter (complement #'str:empty-p)
                                     (str:split #\/ full-path)))))
        (loop :with path := (if is-absolute "/" "")
              :for component :in components
              :do (setf path (path:string-path-join path component))
                  (unless (probe-file path)
                    (sb-unix:unix-mkdir path #o775))))
      #+(or windows win32)
      ;; On Windows, use ensure-directories-exist which handles the complexity
      ;; Need to add trailing slash to indicate it's a directory
      (let ((dir-path (if (char= (char full-path (1- (length full-path))) #\/)
                          full-path
                          (str:concat full-path "/"))))
        (ensure-directories-exist (pathname dir-path))))))
  
(defun list-files (path-string extension)
  (let (files)
    (walk-path path-string
               (lambda (entry-path)
                 (push entry-path files))
               :test (lambda (entry-path)
                       (and (file-p entry-path)
                            (str:ends-with-p entry-path extension))))
    (sort files #'string<=)))

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
  (let* ((normalized-dir (normalize-path-separators directory))
         (pattern (if (and (> (length normalized-dir) 0)
                           (char= (char normalized-dir (1- (length normalized-dir))) #\/))
                      (concatenate 'string normalized-dir "*.*")
                      (concatenate 'string normalized-dir "/*.*"))))
    (handler-case
        (mapcar #'file-namestring
                (remove-if (lambda (path)
                             (let ((name (file-namestring path)))
                               (or (string= name ".") (string= name ".."))))
                           (directory pattern)))
      (error () nil))))

(defun symbolic-link-p (file)
  (eql :symlink (sb-impl::native-file-kind file)))

(defun create-symbolic-link (link-file destination-file)
  #+win32 (declare (ignore link-file destination-file))
  #-win32 (sb-posix:symlink destination-file link-file)
  #+win32 (error "Symbolic links not supported on Windows in this implementation"))

(defun delete-directory (file)
  (sb-ext:delete-directory file :recursive T))

(defun delete-file* (file)
  (cond ((dir-p file)
         (delete-directory file))
        (t
         (cl:delete-file file))))


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
      #-win32 (file-info-mtime (sb-posix:stat path))
      #+win32 (let ((truepath (probe-file path)))
                (when truepath
                  (file-write-date truepath)))
    #-win32 (sb-posix:syscall-error () nil)
    #+win32 (error () nil)))

(defun copy-file (source dest)
  "Copy file from source to dest"
  (with-open-file (in source :element-type '(unsigned-byte 8))
    (with-open-file (out dest :element-type '(unsigned-byte 8)
                         :direction :output
                         :if-exists :supersede)
      (loop for byte = (read-byte in nil)
            while byte do (write-byte byte out)))))

(defun current-directory ()
  "Get current working directory"
  #-win32 (pathname (sb-posix:getcwd))
  #+win32 (truename *default-pathname-defaults*))

(defun change-directory (path)
  "Change current working directory"
  #-win32 (sb-posix:chdir (if (stringp path) path (namestring path)))
  #+win32 (let ((new-path (if (stringp path) (pathname path) path)))
            (setf *default-pathname-defaults* (truename new-path))))

(defun copy-directory (source dest)
  "Recursively copy directory contents from source to dest"
  (let ((source-str (if (typep source 'path:path)
                        (path:path-string source)
                        source))
        (dest-str (if (typep dest 'path:path)
                      (path:path-string dest)
                      dest)))
    (make-dirs dest-str)
    (dolist (entry (list-dir source-str))
      (let ((source-path (path:path-string (path:path-join source-str entry)))
            (dest-path (path:path-string (path:path-join dest-str entry))))
        (cond
          ((dir-p source-path)
           (copy-directory source-path dest-path))
          ((file-p source-path)
           (copy-file source-path dest-path)))))))
