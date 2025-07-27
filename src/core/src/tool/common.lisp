(defpackage epsilon.tool.common
  (:use cl)
  (:local-nicknames
   (fs epsilon.sys.fs)
   (path epsilon.path))
  (:export 
   #:event
   #:find-local-package-file
   #:read-local-package-definition
   #:read-local-package-name))

(in-package epsilon.tool.common)

(defgeneric event (reporter event-type event-data)
  )

(defmethod event (reporter event-type event-data)
  )

;;; Local Package Discovery Functions

;;; FIXME duplicate code

(defun find-local-package-file ()
  "Find package.lisp in current working directory"
  (handler-case
      (let* ((user-dir (or (ignore-errors
                            (when (find-package :sb-posix)
                              (funcall (intern "GETENV" :sb-posix) "EPSILON_USER")))
                           (ignore-errors (fs:current-directory))
                           "."))
             (package-path (ignore-errors (path:path-join user-dir "package.lisp"))))
        (when (and package-path (ignore-errors (fs:exists-p package-path)))
          package-path))
    (error ()
      ;; Return nil if package discovery fails
      nil)))

(defun read-local-package-definition ()
  "Read local package definition from package.lisp, return nil if not found or invalid"
  (handler-case
      (let ((package-file (find-local-package-file)))
        (when package-file
          (handler-case
              (with-open-file (stream (path:path-string package-file))
                (let ((form (read stream nil nil)))
                  (when form
                    (let ((package-data (if (and (listp form) (keywordp (first form)))
                                            ;; It's a plist at top level
                                            form
                                            ;; It might be a quoted plist or other form
                                            (when (listp form) form))))
                      (when package-data
                        (let ((result (list :name (getf package-data :name)
                                            :version (getf package-data :version)
                                            :description (getf package-data :description)
                                            :path (ignore-errors (path:path-string (path:path-parent package-file)))
                                            :local t)))
                          result))))))
            (error ()
              nil))))
    (error ()
      nil)))

(defun read-local-package-name ()
  "Read package name from package.lisp in current directory, return nil if not found"
  (let ((package-def (read-local-package-definition)))
    (when package-def
      (getf package-def :name))))

