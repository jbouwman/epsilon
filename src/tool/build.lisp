;;;;  Key Reusable Components from Test Framework
;;;;
;;;;  1. Output Capture Mechanism (test.lisp:196-214): The test framework captures stdout/stderr using make-string-output-stream and make-broadcast-stream
;;;;  2. Timing and Metrics Collection (test.lisp:164-190): Wall-time and CPU-time measurement infrastructure
;;;;  3. Result Storage (test.lisp:101-121): Structured result objects with status, conditions, and output fields
;;;;  4. Exception Handling (test.lisp:455-473): Error handling with condition classification
;;;;
;;;;  Proposed Integration Strategy
;;;;
;;;;  1. Create a Build Result Framework
;;;;
;;;;  Extend the build system with similar result tracking:
;;;;
;;;;  (defclass compilation-result ()
;;;;    ((source-file :initarg :source-file :reader source-file)
;;;;     (target-file :initarg :target-file :reader target-file)
;;;;     (status :initform :not-compiled :accessor compilation-status)
;;;;     (warnings :initform nil :accessor compilation-warnings)
;;;;     (errors :initform nil :accessor compilation-errors)
;;;;     (stdout-output :initform nil :accessor stdout-output)
;;;;     (stderr-output :initform nil :accessor stderr-output)
;;;;     (start-time :initform nil :accessor start-time)
;;;;     (end-time :initform nil :accessor end-time)))
;;;;
;;;;  2. Enhanced Compilation Function
;;;;
;;;;  Modify compile-source in build.lisp:206-217 to capture warnings:
;;;;
;;;;  (defun compile-source-with-capture (step)
;;;;    (let ((result (make-instance 'compilation-result
;;;;                                :source-file (source-uri step)
;;;;                                :target-file (target-uri step)))
;;;;          (stdout-stream (make-string-output-stream))
;;;;          (stderr-stream (make-string-output-stream)))
;;;;      (unwind-protect
;;;;           (progn
;;;;             (setf (start-time result) (get-internal-real-time))
;;;;             (let ((*standard-output* (make-broadcast-stream *standard-output* stdout-stream))
;;;;                   (*error-output* (make-broadcast-stream *error-output* stderr-stream)))
;;;;               (handler-bind ((warning (lambda (w)
;;;;                                        (push w (compilation-warnings result))
;;;;                                        (muffle-warning)))
;;;;                             (error (lambda (e)
;;;;                                      (push e (compilation-errors result)))))
;;;;                 (compile-file (uri:path (source-uri step))
;;;;                             :output-file (uri:path (target-uri step))))))
;;;;        (setf (end-time result) (get-internal-real-time)
;;;;              (stdout-output result) (get-output-stream-string stdout-stream)
;;;;              (stderr-output result) (get-output-stream-string stderr-stream)))
;;;;      result))
;;;;
;;;;  3. Output File Location Control
;;;;
;;;;  For controlling output file locations under $REPO/target/fasl/$SRCDIR/, modify %make-build-step in build.lisp:106-115:
;;;;
;;;;  (defun %make-build-step (project source-info)
;;;;    "Create target information for a source file"
;;;;    (let* ((project-path (path project))
;;;;           (source-rel-path (subseq (path source-info) (1+ (length project-path))))
;;;;           (source-dir (directory-namestring source-rel-path))
;;;;           (source-name (file-namestring source-rel-path))
;;;;           (target-uri (uri:merge
;;;;                        (uri:merge (uri project) "target/fasl/")
;;;;                        (uri:merge source-dir
;;;;                                  (fs:replace-extension source-name "fasl")))))
;;;;      (make-instance 'build-step
;;;;                     :source-info source-info
;;;;                     :target-info (%make-target-info target-uri))))
;;;;
;;;;  4. Reporting Infrastructure
;;;;
;;;;  Reuse the test framework's formatting functions (format-test-entry, log-test-result) for compilation results:
;;;;
;;;;  (defun format-compilation-entry (source-file result)
;;;;    (format-test-entry (file-namestring source-file) 78
;;;;                       (compilation-result-to-test-result result)))
;;;;
;;;;  (defun log-compilation-result (result stream)
;;;;    "Log compilation warnings and errors to stream"
;;;;    (format stream "~&=== COMPILATION: ~A ===~%" (source-file result))
;;;;    (when (compilation-warnings result)
;;;;      (format stream "WARNINGS (~D):~%" (length (compilation-warnings result)))
;;;;      (dolist (warning (compilation-warnings result))
;;;;        (format stream "  ~A~%" warning)))
;;;;    (when (compilation-errors result)
;;;;      (format stream "ERRORS (~D):~%" (length (compilation-errors result)))
;;;;      (dolist (error (compilation-errors result))
;;;;        (format stream "  ~A~%" error))))
;;;;
;;;;  This approach reuses the output capture, timing, and reporting
;;;;  infrastructure from the test framework and extends the build
;;;;  system to provide compilation feedback and flexible output file
;;;;  placement.

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
  (:export #:build
           #:status
           #:build-for-test
           #:list-tests))

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

(defclass build-step ()
  ((source-info :initarg :source-info :accessor source-info)
   (target-info :initarg :target-info :accessor target-info)))

(defmethod print-object ((obj build-step) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a)"
            (path (source-info obj))
            (build-step-status obj))))

(defmethod source-uri ((self build-step))
  (uri (source-info self)))

(defmethod target-uri ((self build-step))
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
    (make-instance 'project
                   :uri uri
                   :name (map:get defs "name")
                   :version (map:get defs "version")
                   :author (map:get defs "author")
                   :sources sources
                   :tests tests)))

(defun find-source-info (uri)
  (remove-if #'null
             (mapcar #'make-source-info
                     (fs:list-files uri ".lisp"))))

(defun %make-build-step (project source-info)
  "Create target information for a source file"
  (let* ((project-path (path project))
         (source-ext (subseq (path source-info) (1+ (length project-path))))
         (target-uri (uri:merge
                      (uri:merge (uri project) "target/lisp")
                      (fs:replace-extension source-ext "fasl"))))
    (make-instance 'build-step
                   :source-info source-info
                   :target-info (%make-target-info target-uri))))

;;;; TODO something like the following should work
;;;;
;;;; (seq:map (lambda (source)
;;;;            (%make-build-step project source))
;;;;          (project-sources project))
;;;;
;;;; Better yet, build the 'steps' at load time

(defun build-order (project)
  (seq:map (fn:partial #'%make-build-step project)
           (seq:seq (project-sources project))))

(defun test-order (project)
  (seq:map (fn:partial #'%make-build-step project)
           (seq:seq (project-tests project))))

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
      (fs:make-dirs (uri:parent target-uri)))
    (handler-case
        (load
         (compile-file (uri:path source-uri)
                       :output-file (uri:path target-uri)))
      (error ()
        ;; todo -- capture compilation error, and probably halt
        ))))

(defun load-source (step)
  (let ((target-uri (target-uri step)))
    (unless (fs:exists-p target-uri)
      (error "compile first"))
    (handler-case
        (load (uri:path target-uri))
      (error ()
        ;; todo -- capture compilation error, and probably halt
        ))))

(defun build-steps (steps &key force)
  "Build the given steps, optionally forcing compilation of all steps"
  (dolist (step (seq:realize steps))
    (if force
        (compile-source step)
        (case (build-step-status step)
          ((:target-missing
            :source-newer)
           (compile-source step))
          (t
           (load-source step))))))

(defun build (&key dir force (test t))
  "Build project sources and optionally tests.
  
  DIR - Directory containing project (defaults to current directory)
  FORCE - Force compilation of all build steps regardless of timestamps
  TEST - Build test sources in addition to main sources (default t)"
  (let* ((project-dir (cond (dir (uri:file-uri dir))
                           (t (uri:file-uri (namestring *default-pathname-defaults*)))))
         (project (load-project project-dir)))
    (build-steps (build-order project) :force force)
    (when test
      (build-steps (test-order project) :force force))))
