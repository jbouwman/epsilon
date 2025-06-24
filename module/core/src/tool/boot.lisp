(defpackage epsilon.tool.boot
  (:use cl)
  (:export boot))

(in-package :epsilon.tool.boot)

(require :sb-posix)
(require :sb-rotate-byte)

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

(defun list-dir (dir)
  (let (entries)
    (dolist (entry (%list-dir dir))
      (handler-case
          (let ((mode (sb-posix:stat-mode (sb-posix:stat entry))))
            (cond ((sb-posix:s-isdir mode)
                   (setf entries (append entries (list-dir entry))))
                  ((sb-posix:s-isreg mode)
                   (push entry entries))))
        (sb-posix:syscall-error ()
          nil)))
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
  (let* ((core-src-dir (concatenate 'string epsilon-dir "/module/core/src/"))
         (relative-path (if (search core-src-dir source-path)
                            (subseq source-path (length core-src-dir))
                            (error "Source path ~A not under core module src" source-path)))
         (fasl-path (format nil "~a/target/core/~a.fasl" 
                            epsilon-dir
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
  (let* ((core-src-dir (concatenate 'string epsilon-dir "/module/core/src"))
         (fasls '())
         (cache (concatenate 'string epsilon-dir "/target/epsilon.fasl"))
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
             (fasl-path (ensure-target-dir epsilon-dir source-path)))
        (compile-file source-path :output-file fasl-path
                                  :verbose t)
        (load fasl-path :verbose t)
        (push fasl-path fasls)))
    (concatenate-fasls (reverse fasls) cache)
    :built))