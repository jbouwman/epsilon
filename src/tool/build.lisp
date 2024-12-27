(defpackage :epsilon.tool.build
  (:use :cl)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:map #:epsilon.lib.map)
   (#:string #:epsilon.lib.string)
   (#:uri #:epsilon.lib.uri)
   (#:yaml #:epsilon.lib.yaml))
  (:export #:build
           #:status))

(in-package :epsilon.tool.build)

(defclass locatable ()
  ((uri :initarg :uri :accessor uri)))

(defclass hashable ()
  ((hash :initarg :hash :accessor hash)))

(defclass source-info (locatable hashable)
  ((defines :initarg :defines :accessor source-info-defines)
   (requires :initarg :requires :accessor source-info-requires)))

(defmethod path ((self locatable))
  (uri:path (uri self)))

(defun calculate-hash (uri)
  (let ((digest (epsilon.lib.digest:make-digest :sha-256)))
    (with-open-file (stream (uri:path uri) :element-type 'unsigned-byte)
      (epsilon.lib.digest:digest-stream digest stream))
    (epsilon.lib.codec.hex:u8-to-hex
     (epsilon.lib.digest:get-digest digest))))

(defmethod print-object ((obj source-info) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path obj)
            (subseq (hash obj) 0 10))))

(defclass target-info (locatable hashable)
  ())

(defclass build-step ()
  ((source-info :initarg :source-info :accessor source-info)
   (target-info :initarg :target-info :accessor target-info)))

(defmethod source-uri ((self build-step))
  (uri (source-info self)))

(defmethod target-uri ((self build-step))
  (uri (target-info self)))

(defclass project (locatable)
  ((name :initarg :name :accessor project-name)
   (version :initarg :version :accessor project-version)
   (author :initarg :author :accessor project-author)
   (sources :initarg :sources :accessor project-sources)))

(defun make-source-info (uri)
  (multiple-value-bind (defines requires)
      (interpret-package (read-first-form uri))
    (when (or defines requires)
      (make-instance 'source-info
                     :uri uri
                     :hash (calculate-hash uri)
                     :defines (normalize-package defines)
                     :requires (mapcar #'normalize-package requires)))))

(defun load-project (uri)
  "Parse project definition in directory URI"
  (let* ((path (uri:path (uri:extend uri "package.yaml")))
         (defs (map:from-pairs (yaml:node-value (yaml:parse-file path))))
         (sources (sort-sources (mapcan (lambda (path)
                                          (find-source-info (uri:extend uri path)))
                                        (map:get defs "sources")))))
    (make-instance 'project
                   :uri uri
                   :name (map:get defs "name")
                   :version (map:get defs "version")
                   :author (map:get defs "author")
                   :sources sources)))

(defun find-source-info (uri)
  (remove-if #'null
             (mapcar #'make-source-info
                     (fs:list-files uri ".lisp"))))

(defun %make-build-step (project source-info)
  "Create target information for a source file"
  (let* ((project-path (path project))
         (source-ext (subseq (path source-info) (1+ (length project-path))))
         (target-uri (uri:extend
                      (uri:extend (uri project) "target/lisp")
                      (fs:replace-extension source-ext "fasl"))))
    (make-instance 'build-step
                   :source-info source-info
                   :target-info (%make-target-info target-uri))))

(defun make-build-steps (project)
  (let ((steps '()))
    (dolist (info (project-sources project))
      (push (%make-build-step project info) steps))
    (nreverse steps)))

(defun read-first-form (uri)
  (with-open-file (stream (uri::path uri))
    (read stream)))

(defun normalize-package-component (component)
  (string-downcase component))

(defun normalize-package (sym)
  (when sym
    (string:join #\.
                 (mapcar #'normalize-package-component
                         (string:split #\. (symbol-name sym))))))

(defun interpret-package (form)
  (cond ((string-equal 'defpackage (first form))
         (values (second form)
                 (append (cdr (assoc :use (cddr form)))
                         (mapcar #'second (cdr (assoc :local-nicknames (cddr form)))))))
        ((string-equal 'in-package (first form))
         (values nil
                 (cdr form)))))

(defun sort-sources (sources)
  "Sort source files topologically based on their dependencies.
   Returns two values: sorted list and cyclic dependencies (if any)."
  (let* ((nodes (reduce (lambda (m source-info)
                         (map:assoc m (hash source-info) source-info))
                       (remove-if #'null sources)
                       :initial-value map:+empty+))
         (packages (reduce (lambda (m v)
                           (if (source-info-defines v)
                               (map:assoc m (source-info-defines v) v)
                               m))
                         sources
                         :initial-value map:+empty+))
         (visiting map:+empty+)
         (cycles nil)
         (sorted nil))
    (labels ((dep-hashes (source)
               (loop :for pkg :in (source-info-requires source)
                     :for source := (map:get packages pkg)
                     :when source
                       :collect (hash source)))
             (visit (hash path)
               (when (map:contains-p visiting hash)
                 (let ((cycle (ldiff path (member hash path))))
                   (push cycle cycles))
                 (return-from visit nil))
               (when (member hash sorted :test #'equal)
                 (return-from visit t))
               (let ((node (map:get nodes hash)))
                 (unless node
                   (return-from visit t))
                 (setf visiting (map:assoc visiting hash t))
                 (let ((source (map:get nodes hash)))
                   (dolist (dep (dep-hashes source))
                     (visit dep (cons hash path))))
                 (setf visiting (map:dissoc visiting hash))
                 (push hash sorted)
                 t)))
      (dolist (source-info (map:vals nodes))
        (visit (hash source-info) nil))
      (values
       (mapcar (lambda (hash)
                 (map:get nodes hash))
               (nreverse sorted))
       cycles))))

(defun %make-target-info (uri)
  (make-instance 'target-info
                 :uri uri
                 :hash (when (fs:exists-p uri)
                        (calculate-hash uri))))

(defun build-step-status (step)
  (cond ((not (fs:exists-p (target-uri step)))
         :target-missing)
        ((< (fs:modification-time (uri:path (target-uri step)))
            (fs:modification-time (uri:path (source-uri step))))
         :source-newer)
        (t
         :up-to-date)))

(defun compile-source (step)
  (let ((source-uri (source-uri step))
        (target-uri (target-uri step)))
    (unless (fs:exists-p target-uri)
      (fs:make-dirs (uri:parent-dir target-uri)))
    (handler-case
        (load
         (compile-file (uri:path source-uri)
                       :output-file (uri:path target-uri)))
      (error ()
        ;; todo -- capture compilation error, and probably halt
        ))))

(defun build-order (uri)
  (make-build-steps (load-project uri)))

(defun build (uri)
  (dolist (step (make-build-steps (load-project uri)))
    (case (build-step-status step)
      ((:target-missing
        :source-newer)
       (compile-source step)))))


(build-order (uri:uri "file:///Users/jbouwman/git/epsilon"))
