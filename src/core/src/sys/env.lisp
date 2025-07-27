;;;; This module provides cross-platform environment variable access and
;;;; feature detection capabilities. Handles system environment queries
;;;; and Common Lisp feature expression evaluation.

(defpackage epsilon.sys.env
  (:use cl)
  (:local-nicknames
   (map epsilon.map))
  (:export featurep
           getenv
           getenvp
           epsilon-home
           system-info
           platform
           version
           read-version-file))

(in-package epsilon.sys.env)

(defun featurep (x &optional (*features* *features*))
  "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
  (cond ((atom x)
         (and (member x *features*) t))
        ((eq :not (car x))
         (assert (null (cddr x))) (not (featurep (cadr x))))
        ((eq :or (car x))
         (some #'featurep (cdr x)))
        ((eq :and (car x))
         (every #'featurep (cdr x)))
        (t
         (error "~S: malformed feature specification ~S" 'featurep x))))

(defun getenv (x)
  "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
  (sb-ext:posix-getenv x))

(defun getenvp (x)
  "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
  (let ((g (getenv x)))
    (and (not (= 0 (length g)))
         g)))

(defparameter *system-info*
  (let ((features *features*))
    (flet ((find-feature (prefix)
             (find-if (lambda (feature)
                        (and (symbolp feature)
                             (let ((name (symbol-name feature)))
                               (and (>= (length name) (length prefix))
                                    (string= name prefix :end1 (length
                                                                prefix))))))
                      features)))
      (map:make-map :os (or (find-feature "DARWIN")
                            (find-feature "LINUX")
                            (find-feature "WIN")
                            (find-feature "FREEBSD")
                            (find-feature "NETBSD")
                            (find-feature "OPENBSD")
                            (find-feature "SUNOS")
                            (find-feature "UNIX")
                            :unknown)
                    :arch (or (find-feature "X86-64")
                              (find-feature "X86")
                              (find-feature "PPC")
                              (find-feature "ARM64")
                              (find-feature "ARM")
                              (find-feature "MIPS")
                              :unknown)))))


(defun system-info ()
  "Returns a map describing the host operating system and CPU
  architecture."
  *system-info*)

(defun platform ()
  (map:get *system-info* :os))

(defun epsilon-home ()
  "Find epsilon home directory by looking for scripts/epsilon.lisp"
  ;; Since epsilon sets the working directory, we can use a relative path
  ;; from the current directory which should be epsilon root when running
  (let ((cwd-scripts (probe-file "scripts/epsilon.lisp")))
    (if cwd-scripts
        (truename ".")
        ;; Fallback: walk up from current directory
        (let ((current-dir (truename *default-pathname-defaults*)))
          (loop for dir = current-dir 
                  then (make-pathname :directory (butlast (pathname-directory dir)))
                for depth from 0 below 10
                when (probe-file (merge-pathnames "scripts/epsilon.lisp" dir))
                  return dir
                when (null (cdr (pathname-directory dir)))
                  do (error "Could not find epsilon home directory"))))))

(defun read-version-file ()
  "Read version from VERSION file"
  (handler-case
      (with-open-file (stream "VERSION")
        (string-trim '(#\Space #\Tab #\Newline) (read-line stream)))
    (error ()
      "unknown")))

(defun version ()
  "Get epsilon version information"
  (let ((version-file-path "target/epsilon-version.edn"))
    (if (probe-file version-file-path)
        ;; Try to read the generated version EDN file
        (handler-case
            (with-open-file (stream version-file-path)
              (let ((version-data (read stream)))
                (getf version-data :version)))
          (error ()
            ;; Fallback to VERSION file
            (read-version-file)))
        ;; No generated version file, use VERSION file
        (read-version-file))))
