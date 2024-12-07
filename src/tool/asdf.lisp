;;;; asd-analyzer.lisp
;;;; Usage: sbcl --load asd-analyzer.lisp --eval '(analyze-system-file "my-system.asd")'

(require :asdf)
(require :uiop)
(require :yason) ; For JSON output - you might need to quickload this first

(defun system-dependencies (system)
  "Extract direct dependencies of an ASDF system"
  (when (asdf:component-loaded-p system)
    (loop for dep in (asdf:system-depends-on system)
          when (stringp dep)
            collect dep
          when (listp dep)
            collect (second dep))))

(defun component-dependencies (component)
  "Get dependencies for a component"
  (mapcar #'coerce-to-string 
          (asdf:component-sideway-dependencies component)))

(defun coerce-to-string (item)
  (if (symbolp item)
      (string-downcase (symbol-name item))
      (princ-to-string item)))

(defun analyze-component (component)
  "Analyze a single ASDF component"
  (list :name (coerce-to-string (asdf:component-name component))
        :type (type-of component)
        :pathname (when (slot-boundp component 'asdf:relative-pathname)
                   (namestring (asdf:component-pathname component)))
        :dependencies (component-dependencies component)))

(defun analyze-module (module)
  "Analyze a module and its children"
  (append (analyze-component module)
          (list :components 
                (loop for child in (asdf:component-children module)
                      collect (if (typep child 'asdf:module)
                                (analyze-module child)
                                (analyze-component child))))))

(defun analyze-system (system-name)
  "Analyze an ASDF system and return structured data"
  (handler-case
      (let ((system (asdf:find-system system-name)))
        (list :name (coerce-to-string (asdf:component-name system))
              :version (asdf:component-version system)
              :description (asdf:system-description system)
              :author (asdf:system-author system)
              :license (asdf:system-license system)
              :dependencies (system-dependencies system)
              :components (loop for component in (asdf:component-children system)
                              collect (if (typep component 'asdf:module)
                                        (analyze-module component)
                                        (analyze-component component)))))
    (error (e)
      (format *error-output* "Error analyzing system: ~A~%" e)
      nil)))

(defun analyze-system-file (file-path)
  "Analyze an ASDF file and output JSON to stdout"
  (let* ((truename (truename file-path))
         (system-name (pathname-name truename)))
    (asdf:load-asd truename)
    (let ((system-data (analyze-system system-name)))
      (with-output-to-string (s)
        (yason:encode system-data s)))))

(defun write-system-analysis (file-path output-path)
  "Analyze system and write to file"
  (with-open-file (stream output-path
                         :direction :output
                         :if-exists :supersede)
    (write-string (analyze-system-file file-path) stream)))
