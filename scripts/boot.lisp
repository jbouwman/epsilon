(defpackage epsilon.tool.boot
  (:use cl)
  (:export boot))

(in-package :epsilon.tool.boot)

#+(or linux darwin)
(require :sb-posix)
#+(or linux darwin)
(require :sb-bsd-sockets)
(require :sb-rotate-byte)

#+win32
(defun %list-dir (dirpath)
  (let ((pattern (if (and (> (length dirpath) 0)
                          (char= (char dirpath (1- (length dirpath))) #\\))
                     (concatenate 'string dirpath "*.*")
                     (concatenate 'string dirpath "\\*.*"))))
    (mapcar #'namestring (directory pattern))))

#-win32
(defun %list-dir (dirpath)
  (let (dir entries)
    (unwind-protect
         (progn
           (setf dir (sb-unix:unix-opendir dirpath))
           (when dir
             (loop for ent = (sb-unix:unix-readdir dir nil)
                   while ent
                   for name = (sb-unix:unix-dirent-name ent)
                   when (and (not (string= name "."))
                          (not (string= name "..")))
                     do (push (format nil "~a/~a" dirpath name) entries))))
      (when dir
        (sb-unix:unix-closedir dir nil)))
    (nreverse entries)))

(defun dir-p (filespec)
  (null (pathname-type (pathname filespec))))

(defun list-dir (dir)
  (let (entries)
    (dolist (entry (%list-dir dir))
      (cond ((dir-p entry)
             (setf entries (append entries (list-dir entry))))
            (t  ; file
             (push entry entries))))
    entries))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun lisp-files (dir)
  (remove-if-not (lambda (filename)
                   (ends-with-p filename ".lisp"))
                 (list-dir dir)))

(defstruct source-info
  path
  defines
  requires)

(defun get-source-info (path)
  (with-open-file (stream path)
    (let ((form (read stream)))
      (when (string-equal 'defpackage (first form))
        (make-source-info :path path
                          :defines (second form)
                          :requires (append (cdr (assoc :use (cddr form)))
                                            (mapcar #'second
                                                    (cdr (assoc :local-nicknames (cddr form))))))))))

(defun sort-dependent (sources)
  (let ((sorted '())
        (visited (make-hash-table :test 'equal))
        (visiting (make-hash-table :test 'equal)))
    (labels ((visit (source)
               (let ((name (string (source-info-defines source))))
                 (when (gethash name visiting)
                   (error "Circular dependency detected: ~A" name))
                 (unless (gethash name visited)
                   (setf (gethash name visiting) t)
                   (dolist (dep (source-info-requires source))
                     (let ((dep-source (find-if (lambda (s)
                                                  (string-equal (string (source-info-defines s))
                                                                (string dep)))
                                                sources)))
                       (when dep-source
                         (visit dep-source))))
                   (remhash name visiting)
                   (setf (gethash name visited) t)
                   (push source sorted)))))
      (dolist (source sources)
        (visit source))
      (nreverse sorted))))

(defun ensure-target-dir (epsilon-dir source-path)
  "Simple core-only target directory creation"
  (let* ((abs-epsilon-dir (if (or (null epsilon-dir) (string= epsilon-dir "."))
                              #+win32
                              (directory-namestring (truename "."))
                              #-win32
                              (directory-namestring (truename "."))
                              (if (ends-with-p epsilon-dir #+win32 "\\" #-win32 "/")
                                  epsilon-dir
                                  (concatenate 'string epsilon-dir #+win32 "\\" #-win32 "/"))))
         (path-sep #+win32 "\\" #-win32 "/")
         (core-src-dir (concatenate 'string abs-epsilon-dir "module" path-sep "core" path-sep "src" path-sep))
         (normalized-source-path #+win32 
                                 (substitute #\\ #\/ source-path)
                                 #-win32
                                 source-path)
         (normalized-core-src-dir #+win32
                                  (substitute #\\ #\/ core-src-dir)
                                  #-win32
                                  core-src-dir)
         (relative-path (if (search normalized-core-src-dir normalized-source-path)
                            (subseq normalized-source-path (length normalized-core-src-dir))
                            (error "Source path ~A not under core module src ~A" normalized-source-path normalized-core-src-dir)))
         (fasl-path (format nil "~atarget~acore~a~a.fasl" 
                            abs-epsilon-dir path-sep path-sep
                            (subseq relative-path 0 (- (length relative-path) 5))))
         (dir-path (directory-namestring fasl-path)))
    (ensure-directories-exist dir-path)
    fasl-path))

(defun concatenate-fasls (fasl-files output-file)
  "Concatenate compiled FASL files into a single boot file"
  (with-open-file (output output-file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (dolist (fasl fasl-files)
      (when (probe-file fasl)
        (with-open-file (input fasl
                               :direction :input
                               :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte input nil)
                while byte
                do (write-byte byte output)))))))

(defun cache-up-to-date-p (cache-file sources)
  "Check if cache file is newer than all source files"
  (let ((cache-time (file-write-date cache-file)))
    (every (lambda (source)
             (let ((source-time (file-write-date (source-info-path source))))
               (and source-time cache-time (>= cache-time source-time))))
           sources)))

(defun boot (&key (epsilon-dir ".") (force nil))
  "Bootstrap core module only - standalone dependency resolution"
  (let* ((abs-epsilon-dir (if (string= epsilon-dir ".")
                              (directory-namestring (truename "."))
                              (if (ends-with-p epsilon-dir #+win32 "\\" #-win32 "/")
                                  epsilon-dir
                                  (concatenate 'string epsilon-dir #+win32 "\\" #-win32 "/"))))
         (path-sep #+win32 "\\" #-win32 "/")
         (core-src-dir (concatenate 'string abs-epsilon-dir "module" path-sep "core" path-sep "src"))
         (fasls '())
         (cache (concatenate 'string abs-epsilon-dir "target" path-sep "epsilon.fasl"))
         (sources (sort-dependent (remove-if #'null
                                            (mapcar #'get-source-info
                                                    (lisp-files core-src-dir))))))
    (when (and (not force)
            (probe-file cache)
            (cache-up-to-date-p cache sources))
      (load cache)
      (return-from boot :cached))
    (dolist (source sources)
      (let* ((source-path (source-info-path source))
             (fasl-path (ensure-target-dir abs-epsilon-dir source-path)))
        (compile-file source-path :output-file fasl-path
                                  :verbose t)
        (load fasl-path :verbose t)
        (push fasl-path fasls)))
    (concatenate-fasls (reverse fasls) cache)
    :built))
