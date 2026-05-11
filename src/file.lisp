;;;; Filesystem operations and directory traversal.
;;;;
;;;; Low-level file I/O, directory operations, path manipulation, and
;;;; directory tree walking via POSIX system calls.

(cl:defpackage :epsilon.file
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
   ;; Path operations (modern, no logical pathnames)
   :join-paths
   :split-path
   :basename
   :dirname
   :extension
   :strip-extension
   :with-extension
   :absolute-path
   :relative-path
   :normalize-path
   :clean-path

   ;; File/directory tests
   :dir-p
   :file-p
   :exists-p
   :file-exists-p
   :directory-exists-p
   :is-absolute
   :is-relative

   ;; Directory operations
   :home-dir
   :list-dir
   :list-dirs
   :make-dirs
   :runtime-dir
   :temp-dir
   :with-temp-file
   :with-temp-dir
   :copy-directory
   :walk-path
   :change-directory
   :current-directory

   ;; File operations
   :copy-file
   :rename
   :read-file
   :read-file-string
   :read-file-bytes
   :write-file-string
   :write-file-bytes

   :create-symbolic-link
   :delete-directory
   :delete-file*
   :ensure-deleted
   :list-contents
   :list-directories
   :list-files
   :symbolic-link-p

   :access-time
   :modification-time
   :creation-time
   :file-size
   :file-inode
   :group
   :owner
   :attributes
   :*system*
   :*work-directory*

   ;; Directory traversal (consumed by epsilon/src/project.lisp;
   ;; not re-exported by epsilon.fs)
   :normalize-directory
   :parent-directory
   :at-root-p
   :at-home-p
   :get-cwd
   ))

(in-package :epsilon.file)
;;;; Modern Path Operations (Rust/Go style, no logical pathnames)

(defvar *work-directory* nil
  "When bound, temp-dir/with-temp-dir/with-temp-file allocate here instead of $TMPDIR.")

(defparameter *path-separator*
  #+(or linux darwin) #\/
  #+(or windows win32) #\\
  "Platform-specific path separator")

(defun normalize-separators (path)
  "Convert all path separators to forward slashes"
  (let ((path-str (if (typep path 'path:path)
                      (path:path-string path)
                      path)))
    (substitute #\/ #\\ path-str)))

(defun platform-separator (path)
  "Convert path to use platform-specific separator"
  #+(or linux darwin) (substitute #\/ #\\ path)
  #+(or windows win32) (substitute #\\ #\/ path))

(defun clean-path (path)
  "Clean a path by removing redundant elements and resolving . and ..
   Similar to Go's filepath.Clean or Rust's Path::canonicalize"
  (let* ((path-str (if (typep path 'path:path)
                       (path:path-string path)
                       path))
         (normalized (normalize-separators path-str))
         (absolute-p (and (> (length normalized) 0)
                          (char= (char normalized 0) #\/)))
         (components (remove-if #'str:empty-p
				(seq:realize (str:split #\/ normalized))))
         (cleaned '()))
    ;; Process components
    (dolist (comp components)
      (cond
        ((string= comp ".")  ; Skip current directory
         nil)
        ((string= comp "..") ; Go up one level if possible
         (when (and cleaned
                    (not (string= (car cleaned) "..")))
           (pop cleaned)))
        (t
         (push comp cleaned))))
    ;; Reconstruct path
    (let ((result (str:join #\/ (seq:seq (reverse cleaned)))))
      (cond
        (absolute-p (str:concat "/" result))
        ((string= result "") ".")
        (t result)))))

(defun join-paths (&rest paths)
  "Join path components intelligently, like Go's filepath.Join or Rust's Path::join"
  (when paths
    (let ((result (first paths)))
      (dolist (path (rest paths))
        (when (and path (> (length path) 0))
          (cond
            ;; If path is absolute, replace result
            ((and (> (length path) 0)
                  (char= (char path 0) #\/))
             (setf result path))
            ;; Otherwise append
            (t
             (setf result
                   (if (and (> (length result) 0)
                            (not (char= (char result (1- (length result))) #\/))
                            (not (char= (char result (1- (length result))) #\\)))
                       (str:concat result "/" path)
                       (str:concat result path)))))))
      (clean-path result))))

(defun split-path (path)
  "Split a path into directory and file components
   Returns (values directory filename)"
  (let* ((normalized (normalize-separators path))
         (pos (position #\/ normalized :from-end t)))
    (if pos
        (values (subseq normalized 0 (1+ pos))
                (subseq normalized (1+ pos)))
        (values "" normalized))))

(defun dirname (path)
  "Return the directory part of a path, like Go's filepath.Dir"
  (multiple-value-bind (dir file) (split-path path)
    (declare (ignore file))
    (if (string= dir "")
        "."
        (clean-path dir))))

(defun basename (path)
  "Return the last element of a path, like Go's filepath.Base"
  (multiple-value-bind (dir file) (split-path path)
    (declare (ignore dir))
    file))

(defun extension (path)
  "Return the file extension including the dot, like Go's filepath.Ext"
  (let* ((base (basename path))
         (pos (position #\. base :from-end t)))
    (if pos
        (subseq base pos)
        "")))

(defun strip-extension (path)
  "Remove the file extension from a path"
  (let ((ext (extension path)))
    (if (string= ext "")
        path
        (subseq path 0 (- (length path) (length ext))))))

(defun with-extension (path ext)
  "Replace or add extension to a path"
  (let ((stripped (strip-extension path)))
    (if (and ext (> (length ext) 0)
             (not (char= (char ext 0) #\.)))
        (str:concat stripped "." ext)
        (str:concat stripped ext))))

(defun is-absolute (path)
  "Test if a path is absolute"
  (let ((path-str (if (typep path 'path:path)
                      (path:path-string path)
                      path)))
    #+(or linux darwin)
    (and (> (length path-str) 0) (char= (char path-str 0) #\/))
    #+(or windows win32)
    (or (and (> (length path-str) 0) (char= (char path-str 0) #\/))
        (and (> (length path-str) 0) (char= (char path-str 0) #\\))
        (and (>= (length path-str) 3)
             (alpha-char-p (char path-str 0))
             (char= (char path-str 1) #\:)
             (or (char= (char path-str 2) #\/)
                 (char= (char path-str 2) #\\))))))

(defun is-relative (path)
  "Test if a path is relative"
  (not (is-absolute path)))

(defun absolute-path (path)
  "Convert a path to absolute, resolving from current directory if needed"
  (let ((path-str (if (typep path 'path:path)
                      (path:path-string path)
                      path)))
    (if (is-absolute path-str)
        (clean-path path-str)
        (join-paths (namestring (current-directory)) path-str))))

(defun relative-path (path base)
  "Make path relative to base directory"
  (let ((abs-path (absolute-path path))
        (abs-base (absolute-path base)))
    ;; Simple implementation - just remove base prefix if present
    (if (str:starts-with-p abs-path abs-base)
        (let ((rel (subseq abs-path (length abs-base))))
          (if (and (> (length rel) 0)
                   (or (char= (char rel 0) #\/)
                       (char= (char rel 0) #\\)))
              (subseq rel 1)
              rel))
        abs-path)))

(defun normalize-path (path)
  "Normalize a path to use forward slashes and resolve . and .."
  (clean-path path))

(defun parent (file)
  "Get parent directory (deprecated, use dirname)"
  (dirname file))

(defun runtime-dir ()
  "Return the directory containing the running SBCL binary, as recorded in
   *POSIX-ARGV*.  Used to locate sibling artifacts shipped alongside the
   image."
  (parent (normalize-separators (first sb-ext:*posix-argv*))))

(defun temp-dir ()
  "Return a default directory to use for temporary files.
   When *work-directory* is bound, uses that instead of $TMPDIR."
  (or *work-directory* (env:getenv "TMPDIR")))

(defun home-dir ()
  "Return the current user's home directory as a string, with a trailing
   separator.  On Unix, queries the password database; on Windows, falls
   back to USERPROFILE / HOMEDRIVE+HOMEPATH."
  #+(or linux darwin)
  (sb-unix:uid-homedir (sb-unix:unix-getuid))
  #+(or windows win32)
  (or (env:getenv "USERPROFILE")
      (str:concat (env:getenv "HOMEDRIVE") (env:getenv "HOMEPATH"))))

(defmacro with-temp-file ((name) &body body)
  "Bind NAME to the path of a fresh randomly-named temp file under TEMP-DIR
   and execute BODY.  The file itself is not created up front -- BODY is
   responsible for opening it.  On exit (normal or non-local), the file is
   deleted if it exists."
  `(let ((,name (join-paths
                 (temp-dir)
                 (str:concat "epsilon-" (str:random-string 16) ".tmp"))))
     (unwind-protect
          (progn
            ,@body)
       (when (file-p ,name)
         (delete-file* ,name)))))

(defmacro with-temp-dir ((name) &body body)
  "Create a temporary directory and clean it up after use"
  `(let ((,name (join-paths
                 (temp-dir)
                 (str:concat "epsilon-dir-" (str:random-string 16)))))
     (unwind-protect
          (progn
            (make-dirs ,name)
            ,@body)
       (when (dir-p ,name)
         (delete-directory ,name)))))

#+win32
(defun directory-pathname-p (pathname)
  "Check if pathname refers to a directory on Windows"
  (let ((namestring (namestring pathname)))
    (or (char= (char namestring (1- (length namestring))) #\\)
        (char= (char namestring (1- (length namestring))) #\/)
        (and (probe-file pathname)
             (not (pathname-name pathname))))))

(defun file-info (path)
  "Return a stat-like record for PATH.  On Unix this is the SB-POSIX:STAT
   struct itself; on Windows it's a property list with :path and :type.
   The FILE-INFO-* accessors below normalize across both shapes."
  #-win32 (sb-posix:stat path)
  #+win32 (let ((truepath (probe-file path)))
            (when truepath
              (list :path (namestring truepath)
                    :type (if (directory-pathname-p truepath) :directory :file)))))

(defun dir-p (path)
  "True iff PATH names an existing directory.  Returns NIL (rather than
   signaling) for paths that don't exist or aren't reachable."
  (let ((path-str (if (typep path 'path:path)
                      (path:path-string path)
                      path)))
    (handler-case
        #-win32 (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat path-str)))
        #+win32 (let ((truepath (probe-file path-str)))
                  (and truepath (directory-pathname-p truepath)))
        #-win32 (sb-posix:syscall-error () nil)
        #+win32 (error () nil))))

(defun file-p (path)
  "True iff PATH names an existing regular file (not a directory or other
   special file).  Returns NIL for nonexistent or unreadable paths."
  (let ((path-str (if (typep path 'path:path)
                      (path:path-string path)
                      path)))
    (handler-case
        #-win32 (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:stat path-str)))
        #+win32 (let ((truepath (probe-file path-str)))
                  (and truepath (not (directory-pathname-p truepath))))
        #-win32 (sb-posix:syscall-error () nil)
        #+win32 (error () nil))))

(defun file-info-type (stat)
  "Classify the entry described by STAT (from FILE-INFO) as :FILE,
   :DIRECTORY, or :LINK.  Returns NIL for other special types."
  #-win32 (cond ((sb-posix:s-isreg (sb-posix:stat-mode stat)) :file)
                ((sb-posix:s-isdir (sb-posix:stat-mode stat)) :directory)
                ((sb-posix:s-islnk (sb-posix:stat-mode stat)) :link))
  #+win32 (if (listp stat)
              (getf stat :type)
              :file))

(defun file-info-size (stat)
  "Size in bytes from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-size stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need file size lookup

(defun file-info-mtime (stat)
  "Last-modified time (universal time) from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-mtime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need mtime lookup

(defun file-info-atime (stat)
  "Last-access time (universal time) from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-atime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need atime lookup

(defun file-info-ctime (stat)
  "Status-change time (universal time) from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-ctime stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need ctime lookup

(defun file-info-mode (stat)
  "POSIX mode bits from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-mode stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need mode lookup

(defun file-info-uid (stat)
  "Owner's numeric UID from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-uid stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need uid lookup

(defun file-info-gid (stat)
  "Group's numeric GID from a FILE-INFO record.  Windows stub returns 0."
  #-win32 (sb-posix:stat-gid stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need gid lookup

(defun file-info-ino (stat)
  "Inode number from a FILE-INFO record.  Used to detect file rotation
   (same name across an unlink+create cycle gets a new inode).  Windows
   stub returns 0."
  #-win32 (sb-posix:stat-ino stat)
  #+win32 (if (listp stat) 0 0)) ; Windows implementation would need inode lookup

(defun %walk-dir (dirpath f)
  "Single-level directory iterator: open DIRPATH, call F on the joined path
   of every entry (excluding . and ..), and close the directory.  Used by
   WALK-PATH, which adds recursion."
  #+(or linux darwin)
  (locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let (dir)
      (unwind-protect
           (progn
             (setf dir (sb-posix:opendir dirpath))
             (when dir
               (loop :for ent := (sb-posix:readdir dir)
                     :until (sb-alien:null-alien ent)
                     :for name := (sb-posix:dirent-name ent)
                     :when (and (not (string= name "."))
                                (not (string= name "..")))
                     :do (funcall f
                                  (path:string-path-join dirpath name)))))
        (when dir
          (sb-posix:closedir dir)))))
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
  "Walk the directory tree rooted at PATH-STRING, calling F on each entry
   path that satisfies TEST.  When RECURSIVE (the default), descend into
   subdirectories.  TEST is applied before F, so it can both filter the
   entries F sees and act as a side-effect-free predicate for callers."
  (%walk-dir (if (stringp path-string)
                 path-string
                 (path:path-from-uri path-string))
             (lambda (entry-path)
               (when (funcall test entry-path)
                 (funcall f entry-path))
               (when (and recursive (dir-p entry-path))
                 (walk-path entry-path f :recursive t :test test)))))

(defun exists-p (path-string)
  "Check if file or directory exists"
  (let ((path (if (typep path-string 'path:path)
                  (path:path-string path-string)
                  path-string)))
    (not (null (probe-file path)))))

(defun file-exists-p (path)
  "Check if path exists and is a regular file"
  (and (exists-p path) (file-p path)))

(defun directory-exists-p (path)
  "Check if path exists and is a directory"
  (and (exists-p path) (dir-p path)))

(defun read-file (filename)
  "Read FILENAME's contents into a single string in one shot.  Allocates a
   buffer the size of the file up front; not appropriate for very large
   files where streaming would be a better fit."
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
  "Create directory and all parent directories if needed"
  (let* ((path-str (if (typep path-string 'path:path)
                       (path:path-string path-string)
                       path-string))
         (full-path (absolute-path path-str)))
    (when (and full-path (> (length full-path) 0))
      (let* ((normalized (normalize-separators full-path))
             (is-absolute (char= (char normalized 0) #\/))
             (components (remove-if #'str:empty-p
                                    (seq:realize (str:split #\/ normalized)))))
        (loop :with current := (if is-absolute "/" "")
              :for component :in components
              :do (setf current (if (string= current "")
                                    component
                                    (if (or (char= (char current (1- (length current))) #\/)
                                            (string= current "/"))
					(str:concat current component)
					(str:concat current "/" component))))
              (unless (probe-file current)
                #+(or linux darwin)
                (handler-case
                    (sb-posix:mkdir current #o775)
                  (sb-posix:syscall-error (e)
                    ;; Ignore EEXIST (17) - directory may have been created by parallel process
                    ;; Ignore EROFS (30) - read-only filesystem (e.g. Nix store with pre-compiled FASLs)
                    (unless (member (sb-posix:syscall-errno e) '(17 30))
                      (error e))))
                #+(or windows win32)
                (ensure-directories-exist (pathname current))))))))

(defun list-files (path-string extension)
  "Walk PATH-STRING recursively and return a sorted list of regular-file
   paths whose names end with EXTENSION (a literal suffix string, e.g.
   \".lisp\")."
  (let (files)
    (walk-path path-string
	       (lambda (entry-path)
                 (push entry-path files))
	       :test (lambda (entry-path)
		       (and (file-p entry-path)
                            (str:ends-with-p entry-path extension))))
    (sort files #'string<=)))

(defun ensure-deleted (pathname)
  "Delete PATHNAME if it exists as a regular file.  No-op if it doesn't
   exist or is a directory."
  (when (file-p pathname)
    (delete-file* pathname)))

(defun list-dir (directory)
  "Return a list of entry names (just the basenames, not full paths) in
   DIRECTORY.  Excludes . and ..  Returns NIL on errors."
  #+(or linux darwin)
  (locally
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let (dir)
      (unwind-protect
           (progn
             (setf dir (sb-posix:opendir directory))
             (when dir
               (loop :for ent := (sb-posix:readdir dir)
                     :until (sb-alien:null-alien ent)
                     :for name := (sb-posix:dirent-name ent)
                     :when (and (not (string= name "."))
                                (not (string= name "..")))
                     :collect name)))
        (when dir
          (sb-posix:closedir dir)))))
  #+(or windows win32)
  ;; Windows implementation using directory()
  (let* ((normalized-dir (normalize-separators directory))
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
  "True iff FILE is a symbolic link.  Reaches into SB-IMPL for the kind
   query because SB-POSIX:LSTAT isn't exposed at the level we need."
  (eql :symlink (sb-impl::native-file-kind file)))

(defun create-symbolic-link (link-file destination-file)
  "Create LINK-FILE as a symlink pointing at DESTINATION-FILE.  Signals
   an error on Windows; the SB-POSIX call is the only supported path."
  #-win32 (sb-posix:symlink destination-file link-file)
  #+win32 (error "Symbolic links not supported on Windows in this implementation"))

(defun delete-directory (file)
  "Recursively delete a directory and all its contents."
  (sb-ext:delete-directory file :recursive T))

(defun delete-file* (file)
  "Delete FILE, dispatching on type: regular files go through CL:DELETE-FILE,
   directories through DELETE-DIRECTORY (recursive).  Use this rather than
   CL:DELETE-FILE when callers may pass either kind of path."
  (cond ((dir-p file)
         (delete-directory file))
        (t
         (cl:delete-file file))))


(defmacro with-u8-in ((f in) &body body)
  "Bind F to a binary input stream on path IN (element-type U8) for the
   extent of BODY.  Closes the stream on exit."
  `(with-open-file (,f ,in :element-type 'u8)
     ,@body))

(defmacro with-u8-out ((f in) &body body)
  "Bind F to a binary output stream on path IN (element-type U8,
   :supersede on overwrite) for the extent of BODY.  Closes the stream
   on exit.  The IN parameter is a misnomer kept for symmetry with
   WITH-U8-IN -- it's the output path."
  `(with-open-file (,f ,in :element-type 'u8
                       :direction :output
                       :if-exists :supersede)
     ,@body))

(defun modification-time (path)
  "Return the modification time of the file at PATH as a universal time."
  (handler-case
      #-win32 (file-info-mtime (sb-posix:stat path))
      #+win32 (let ((truepath (probe-file path)))
                (when truepath
                  (file-write-date truepath)))
      #-win32 (sb-posix:syscall-error () nil)
      #+win32 (error () nil)))

(defun file-size (path)
  "Return the size of the file at PATH in bytes, or NIL if the file is
absent or otherwise unreadable."
  (handler-case
      #-win32 (file-info-size (sb-posix:stat path))
      #+win32 (let ((truepath (probe-file path)))
                (when truepath
                  (with-open-file (s truepath :element-type '(unsigned-byte 8))
                    (file-length s))))
      #-win32 (sb-posix:syscall-error () nil)
      #+win32 (error () nil)))

(defun file-inode (path)
  "Return the inode number of the file at PATH, or NIL if unavailable.
Used to detect file rotation: a path can keep the same name across an
unlink+create cycle but the inode number typically changes."
  (handler-case
      #-win32 (file-info-ino (sb-posix:stat path))
      #+win32 (declare (ignore path))
      #+win32 nil
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

(defun rename (old-path new-path)
  "Rename/move a file or directory from old-path to new-path.
   Uses POSIX rename for correct string-based renaming without
   pathname merging issues."
  #-win32 (sb-posix:rename old-path new-path)
  #+win32 (cl:rename-file old-path new-path))

(defun current-directory ()
  "Get current working directory as a string"
  #-win32 (sb-posix:getcwd)
  #+win32 (namestring (truename *default-pathname-defaults*)))

(defun change-directory (path)
  "Change current working directory"
  #-win32 (sb-posix:chdir (if (stringp path) path (namestring path)))
  #+win32 (let ((new-path (if (stringp path) (pathname path) path)))
            (setf *default-pathname-defaults* (truename new-path))))

(defun copy-directory (source dest)
  "Recursively copy directory contents from source to dest"
  (let ((source-str (if (stringp source) source (namestring source)))
        (dest-str (if (stringp dest) dest (namestring dest))))
    (make-dirs dest-str)
    (dolist (entry (list-dir source-str))
      (let ((source-path (join-paths source-str entry))
            (dest-path (join-paths dest-str entry)))
        (cond
          ((dir-p source-path)
           (copy-directory source-path dest-path))
          ((file-p source-path)
           (copy-file source-path dest-path)))))))

;;;; Additional filesystem operations

(defun list-dirs (path)
  "List only directories in path"
  (remove-if-not #'dir-p
                 (mapcar (lambda (name) (join-paths path name))
                         (list-dir path))))

(defun list-contents (path)
  "List all contents of a directory (alias for list-dir)"
  (list-dir path))

(defun list-directories (path)
  "List only directories in path (alias for list-dirs)"
  (list-dirs path))

(defun access-time (path)
  "Return the access time of the file at PATH"
  (handler-case
      #-win32 (file-info-atime (sb-posix:stat path))
      #+win32 (modification-time path)  ; Windows fallback to modification time
      #-win32 (sb-posix:syscall-error () nil)
      #+win32 (error () nil)))

(defun creation-time (path)
  "Return the creation time of the file at PATH"
  (handler-case
      #-win32 (file-info-ctime (sb-posix:stat path))
      #+win32 (modification-time path)  ; Windows fallback to modification time
      #-win32 (sb-posix:syscall-error () nil)
      #+win32 (error () nil)))

(defun group (path)
  "Return the group ID of the file at PATH"
  (handler-case
      #-win32 (file-info-gid (sb-posix:stat path))
      #+win32 0  ; Windows doesn't have group IDs
      #-win32 (sb-posix:syscall-error () nil)))

(defun owner (path)
  "Return the owner ID of the file at PATH"
  (handler-case
      #-win32 (file-info-uid (sb-posix:stat path))
      #+win32 0  ; Windows doesn't have owner IDs
      #-win32 (sb-posix:syscall-error () nil)))

(defun attributes (path)
  "Return file attributes as a property list"
  (when (exists-p path)
    (list :exists t
          :directory (dir-p path)
          :file (file-p path)
          :modification-time (modification-time path)
          :access-time (access-time path)
          :creation-time (creation-time path)
          :owner (owner path)
          :group (group path))))

(defparameter *system*
  #+(or linux darwin) :unix
  #+(or windows win32) :windows
  "The current operating system type")

;;;; Directory Traversal

(defun normalize-directory (path)
  "Ensure path is a directory pathname (ends with /)."
  (let ((namestring (namestring path)))
    (if (and (> (length namestring) 0)
             (char= (char namestring (1- (length namestring))) #\/))
        path
        (pathname (concatenate 'string namestring "/")))))

(defun parent-directory (directory)
  "Get the parent directory of a directory pathname.
   Returns NIL if at root."
  (let* ((dir (normalize-directory directory))
         (dirs (pathname-directory dir)))
    (when (and dirs (> (length dirs) 1))
      (make-pathname :directory (butlast dirs)
                     :defaults dir))))

(defun at-root-p (directory)
  "Check if we're at the root directory."
  (let* ((dirs (pathname-directory (normalize-directory directory))))
    (or (null dirs)
        (<= (length dirs) 1))))

(defun at-home-p (directory)
  "Check if we're at the user's home directory."
  (let ((home (user-homedir-pathname)))
    (when home
      (equalp (truename directory)
              (truename home)))))

(defun get-cwd ()
  "Get the current working directory as a string.
   Uses EPSILON_CWD if set (user's original directory before epsilon chdir)."
  (or (env:getenv "EPSILON_CWD")
      (namestring (sb-posix:getcwd))))
