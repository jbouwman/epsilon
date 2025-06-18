(defpackage :epsilon.tool.build
  (:use :cl)
  (:local-nicknames
   (#:pkg #:epsilon.sys.pkg)
   (#:fs #:epsilon.sys.fs)
   (#:digest #:epsilon.lib.digest)
   (#:fn #:epsilon.lib.function)
   (#:hex #:epsilon.lib.hex)
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string)
   (#:uri #:epsilon.lib.uri)
   (#:yaml #:epsilon.lib.yaml))
  (:export
   #:build
   #:*known-projects*))

(in-package :epsilon.tool.build)

(defvar *known-projects*
  map:+empty+)

(defclass locatable ()
  ((uri :initarg :uri :accessor uri)))

(defclass hashable ()
  ((hash :initarg :hash :accessor hash)))

(defgeneric build-order (node)
  (:documentation "Produce a sequence of steps necessary to build a specific node in the project source tree."))

(defclass source-info (locatable hashable)
  ((defines :initarg :defines :accessor source-info-defines)
   (requires :initarg :requires :accessor source-info-requires)))

(defmethod path ((self locatable))
  (uri:path (uri self)))

(defun calculate-hash (uri)
  (let ((digest (digest:make-digest :sha-256)))
    (with-open-file (stream (uri:path uri) :element-type 'unsigned-byte)
      (digest:digest-stream digest stream))
    (hex:u8-to-hex
     (digest:get-digest digest))))

(defmethod print-object ((obj source-info) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path obj)
            (subseq (hash obj) 0 10))))

(defclass target-info (locatable hashable)
  ())

(defclass build-input ()
  ((project :initarg :project :accessor project)
   (source-info :initarg :source-info :accessor source-info)))

(defclass build-result ()
  ((build-input :initarg :build-input :reader build-input)
   (status :initform :not-compiled :accessor compilation-status)
   (warnings :initform nil :accessor compilation-warnings)
   (errors :initform nil :accessor compilation-errors)
   (stdout-output :initform nil :accessor stdout-output)
   (stderr-output :initform nil :accessor stderr-output)
   (start-time :initform nil :accessor start-time)
   (end-time :initform nil :accessor end-time)))

(defclass project-build ()
  ((project :initarg :project)
   (results :initarg :results)))

(defmethod build-order ((node build-input))
  (let ((root (list node)))
    (dolist (r (source-info-requires (source-info node)))
      (let ((p (find-provider (project node) r)))
        (when p
          (setf root (append (build-order p) root)))))
    (remove-duplicates root
                       :key (lambda (node)
                              (source-info node)))))

(defun provided-package (node)
  (source-info-defines (source-info node)))

(defun find-provider (project package)
  (seq:first
   (seq:filter (lambda (node)
                 (string= (provided-package node)
                          package))
               (build-order project))))

(defmethod print-object ((obj build-input) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path (source-info obj))
            (build-input-status obj))))

(defmethod source-uri ((self build-input))
  (uri (source-info self)))

(defmethod target-uri ((self build-input))
  (uri (target-info self)))

(defclass project (locatable)
  ((name :initarg :name :accessor project-name)
   (version :initarg :version :accessor project-version)
   (author :initarg :author :accessor project-author)
   (sources :initarg :sources :accessor project-sources)
   (tests :initarg :tests :accessor project-tests)))

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
  (let* ((path (uri:path (uri:merge uri "package.yaml")))
         (defs (map:from-pairs (yaml:node-value (yaml:parse-file path))))
         (sources (sort-sources (mapcan (lambda (path)
                                          (find-source-info (uri:merge uri path)))
                                        (map:get defs "sources"))))
         (tests (when (map:get defs "tests")
                  (sort-sources (mapcan (lambda (path)
                                          (find-source-info (uri:merge uri path)))
                                        (map:get defs "tests"))))))
    (let ((project (make-instance 'project
                                  :uri uri
                                  :name (map:get defs "name")
                                  :version (map:get defs "version")
                                  :author (map:get defs "author")
                                  :sources sources
                                  :tests tests)))
      (setf *known-projects* (map:assoc *known-projects* 
                                        (map:get defs "name")
                                        uri))
      project)))

(defun find-source-info (uri)
  (remove-if #'null
             (mapcar #'make-source-info
                     (fs:list-files uri ".lisp"))))

(defun target-info (build-input)
  "Create target information for a source file
   
   Follows URI path best practices using uri:path-join utility:
   - Proper directory/file path distinction
   - Avoids double slashes in path construction  
   - Clean, readable path construction"
  (let* ((project (project build-input))
         (project-path (path project))
         (source-rel-path (subseq (path (source-info build-input))
                                  (length project-path)))
         (target-rel-path (fs:replace-extension source-rel-path "fasl"))
         (target-path (uri:path-join "target" "lisp" target-rel-path))
         (target-uri (uri:merge (uri project) target-path)))
    (make-instance 'target-info
                   :uri target-uri
                   :hash (when (fs:exists-p target-uri)
                           (calculate-hash target-uri)))))

(defun %make-build-input (project source-info)
  "Create target information for a source file"
  (make-instance 'build-input
                 :project project
                 :source-info source-info))

(defmethod build-order ((project project))
  (seq:map (fn:partial #'%make-build-input project)
           (seq:seq (append (project-sources project)
                            (project-tests project)))))

(defun read-first-form (uri)
  (with-open-file (stream (uri::path uri))
    (read stream)))

(defun normalize-package-component (component)
  (string-downcase component))

(defun normalize-package (sym)
  (when sym
    (str:join #\.
              (seq:map #'normalize-package-component
                       (pkg:parse (symbol-name sym))))))

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

(defun build-input-status (build-input)
  (cond ((not (fs:exists-p (target-uri build-input)))
         :target-missing)
        ((< (fs:modification-time (uri:path (target-uri build-input)))
            (fs:modification-time (uri:path (source-uri build-input))))
         :source-newer)
        (t
         :up-to-date)))

(defun watch-operation (build-input fn)
  (let ((result (make-instance 'build-result
                               :build-input build-input))
        (stdout-stream (make-string-output-stream))
        (stderr-stream (make-string-output-stream)))
    (unwind-protect
         (progn
           (setf (start-time result) (get-internal-real-time))
           (let ((*standard-output* stdout-stream)
                 (*error-output* stderr-stream))
             (handler-bind ((warning (lambda (w)
                                       (push w (compilation-warnings result))
                                       (muffle-warning)))
                            (error (lambda (e)
                                     (push e (compilation-errors result)))))
               (funcall fn))))
      (setf (end-time result) (get-internal-real-time)
            (stdout-output result) (get-output-stream-string stdout-stream)
            (stderr-output result) (get-output-stream-string stderr-stream)))
    (report-result result)
    result))

(defun compile-source (build-input)
  (watch-operation build-input
                   (lambda ()
                     (fs:make-dirs (uri:parent (target-uri build-input)))
                     (compile-file (uri:path (source-uri build-input))
                                   :output-file (uri:path (target-uri build-input))))))

(defun load-source (build-input)
  (watch-operation build-input
                   (lambda ()
                     (load (uri:path (target-uri build-input))))))

(defun %build (project &key force)
  "Build the given build-inputs, optionally forcing compilation of all steps"
  (let ((results (seq:seq (seq:realize (seq:map (lambda (build-input)
                                                  (if force
                                                      (compile-source build-input)
                                                      (case (build-input-status build-input)
                                                        ((:target-missing
                                                          :source-newer)
                                                         (compile-source build-input))
                                                        (t
                                                         (load-source build-input)))))
                                                (build-order project))))))
    (make-instance 'project-build
                   :project project
                   :results results)))

(defun build (&key dir force)
  "Build project sources and optionally tests.
  
  DIR - Directory containing project (defaults to current directory)
  FORCE - Force compilation of all build steps regardless of timestamps"
  (let* ((project-dir (uri:file-uri (or dir (namestring *default-pathname-defaults*))))
         (project (load-project project-dir)))
    (%build project :force force)))

(defun operation-wall-time (result)
  (if (and (start-time result) (end-time result))
      (/ (- (end-time result) (start-time result))
         internal-time-units-per-second)
      0))

(defun report-result (result)
  (let* ((build-input (build-input result))
         (source-path (path (source-info build-input)))
         (action (if (compilation-errors result) "FAILED" 
                     (if (compilation-warnings result) "COMPILED" "LOADED")))
         (warning-count (length (compilation-warnings result)))
         (error-count (length (compilation-errors result))))
    (format t "  ~a ~a" action source-path)
    (when (or (> warning-count 0) (> error-count 0))
      (format t " (")
      (when (> warning-count 0)
        (format t "~d warning~:p" warning-count))
      (when (> error-count 0)
        (when (> warning-count 0)
          (format t " "))
        (format t "~d error~:p" error-count))
      (format t ")"))
    (format t "~%")
    (when (compilation-errors result)
      (when (stdout-output result)
        (format t "    stdout: ~a~%" (stdout-output result)))
      (when (stderr-output result)
        (format t "    stderr: ~a~%" (stderr-output result))))))

(defun report (project-build)
  (with-slots (project results) project-build
    (let ((total-time (seq:reduce #'+ (seq:map #'operation-wall-time results)
                                  :initial-value 0)))
      (format t "~&Project: ~a (build time: ~,2fs)~%"
              (uri:path (uri project))
              total-time)
      (seq:each #'report-result results))))
