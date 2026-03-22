;;;; Test CLI Command Handler
;;;;
;;;; Provides the `epsilon test` command implementation.
;;;; Registered via :commands in module.plist and loaded lazily
;;;; when the test command is invoked.

(defpackage epsilon.test.cli
  (:use :cl)
  (:local-nicknames
   (str epsilon.string)
   (loader epsilon.loader)
   (graph epsilon.graph)
   (log epsilon.log)
   (commands epsilon.commands))
  (:export #:run)
  (:enter t))

;;; ---------------------------------------------------------------------------
;;; Platform utilities
;;; ---------------------------------------------------------------------------

(defun cpu-count ()
  "Return the number of available CPU cores."
  (max 1 (handler-case
              (sb-alien:alien-funcall
               (sb-alien:extern-alien
                "sysconf"
                (function sb-alien:long sb-alien:long))
               ;; _SC_NPROCESSORS_ONLN = 58 on Darwin, 84 on Linux
               #+darwin 58
               #+linux 84)
            (error () 4))))

(defun default-worker-count ()
  "Return the default number of parallel test workers."
  1)

;;; ---------------------------------------------------------------------------
;;; Module dependency utilities
;;; ---------------------------------------------------------------------------

(defun module-requires (module-name environment)
  "Return the list of dependency names for MODULE-NAME."
  (let* ((mod (loader:get-module environment module-name))
         (location (when mod (loader:module-location mod)))
         (project (when location
                    (handler-case
                        (loader:load-module-project
                         (commands:ensure-location-string location))
                      (error () nil)))))
    (when project
      (loader:module-project-requires project))))

(defun all-module-names (environment)
  "Return all module names for the current platform."
  (let ((platform (string-downcase (symbol-name (epsilon.sys.env:platform)))))
    (loop for mod in (loader:query-modules environment)
          for name = (loader:module-name mod)
          for metadata = (loader:module-metadata mod)
          for mod-platform = (getf metadata :platform)
          when (or (not mod-platform)
                   (string-equal mod-platform platform))
            collect name)))

(defun topological-module-order (module-names environment)
  "Return MODULE-NAMES sorted in dependency order (dependencies first).
   Modules not in MODULE-NAMES are excluded from the result."
  (let ((name-set (make-hash-table :test 'equal)))
    (dolist (n module-names)
      (setf (gethash n name-set) t))
    (let ((sorted (graph:topological-sort
                   module-names
                   (lambda (name)
                     ;; Only return deps that are in our set
                     (remove-if-not (lambda (d) (gethash d name-set))
                                    (module-requires name environment)))
                   :on-cycle :collect
                   :test 'equal)))
      sorted)))

(defun expand-with-dependencies (module-names environment)
  "Expand MODULE-NAMES to include all transitive dependencies.
   Returns the expanded set (not yet sorted)."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (labels ((walk (name)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (dolist (dep (module-requires name environment))
                   (walk dep))
                 (push name result))))
      (dolist (name module-names)
        (walk name)))
    (nreverse result)))

;;; ---------------------------------------------------------------------------
;;; Single-module test runner
;;; ---------------------------------------------------------------------------

(defun run-module-tests (environment module-name
                         &key verbose noclean update-snapshots
                              test-mode)
  "Run tests for a single module.
   Returns (VALUES SUCCESS-P FAILED-MODULES)."
  (loader:load-module environment "epsilon.test")
  (when update-snapshots
    (let ((set-update-fn (find-symbol "SET-UPDATE-SNAPSHOTS"
                                      (find-package "EPSILON.TEST"))))
      (when set-update-fn
        (funcall set-update-fn t))))
  (let ((run-tests-fn (find-symbol "RUN-TESTS" (find-package "EPSILON.TEST"))))
    (unless run-tests-fn
      (format *error-output* "Error: EPSILON.TEST:RUN-TESTS not found~%")
      (return-from run-module-tests
        (values nil (list (cons module-name :internal-error)))))
    (handler-case
        (funcall run-tests-fn environment (list module-name)
                 :verbose verbose
                 :noclean noclean
                 :test-mode test-mode)
      (error (e)
        (format *error-output* "Error testing ~A: ~A~%" module-name e)
        (values nil (list (cons module-name :error)))))))

;;; ---------------------------------------------------------------------------
;;; Parallel module runner
;;; ---------------------------------------------------------------------------

(defun preload-modules (environment modules)
  "Pre-load MODULES sequentially to avoid loader reentrancy in parallel mode.
   Returns (VALUES loaded-modules load-failures) where load-failures is
   a list of (module-name . :load-error) pairs."
  (let ((loaded '())
        (failures '()))
    (dolist (mod modules)
      (handler-case
          (progn
            (handler-bind ((sb-ext:compiler-note #'muffle-warning))
              (loader:load-module environment mod))
            (push mod loaded))
        (error (e)
          (format *error-output* "~&  FAIL ~A  (load error: ~A)~%" mod e)
          (push (cons mod :load-error) failures))))
    (values (nreverse loaded) (nreverse failures))))

(defvar *module-timeout* 120
  "Maximum seconds to allow a single module's tests to run before timing out.
   Set to NIL to disable module-level timeouts.")

(defun run-modules-parallel (environment modules n-workers
                             &key verbose noclean update-snapshots
                                  test-mode
                                  fail-fast)
  "Run module test suites across N-WORKERS threads.
   Pre-loads modules sequentially, then dispatches tests in parallel.
   Returns list of (module-name . success-p) pairs."
  ;; Pre-load all modules sequentially to avoid loader reentrancy
  (multiple-value-bind (loaded-modules load-failures)
      (preload-modules environment modules)
    (let ((results (mapcar (lambda (f) (cons (car f) nil)) load-failures))
          (queue (copy-list loaded-modules))
          (lock (sb-thread:make-mutex :name "test-parallel-lock"))
          (abort-flag nil)
          (threads '())
          (fresh-state-fn (find-symbol "CALL-WITH-FRESH-TEST-STATE"
                                       (find-package "EPSILON.TEST"))))
      (dotimes (i n-workers)
        (push
         (sb-thread:make-thread
          (lambda ()
            (loop
              (let ((module nil))
                (sb-thread:with-mutex (lock)
                  (when (or (null queue) abort-flag)
                    (return))
                  (setf module (pop queue)))
                (flet ((do-test ()
                         (multiple-value-bind (success-p)
                             (run-module-tests
                              environment module
                              :verbose verbose
                              :noclean noclean
                              :update-snapshots update-snapshots
                              :test-mode test-mode)
                           success-p)))
                  (let ((ok (if *module-timeout*
                                (handler-case
                                    (sb-ext:with-timeout *module-timeout*
                                      (if fresh-state-fn
                                          (funcall fresh-state-fn #'do-test)
                                          (do-test)))
                                  (sb-ext:timeout ()
                                    (format *error-output*
                                            "~&  TIMEOUT ~A  (exceeded ~Ds)~%"
                                            module *module-timeout*)
                                    nil))
                                (if fresh-state-fn
                                    (funcall fresh-state-fn #'do-test)
                                    (do-test)))))
                    (sb-thread:with-mutex (lock)
                      (push (cons module ok) results)
                      (when (and fail-fast (not ok))
                        (setf abort-flag t))))))))
          :name (format nil "test-worker-~D" i))
         threads))
      (dolist (th threads)
        (sb-thread:join-thread th))
      (nreverse results))))

;;; ---------------------------------------------------------------------------
;;; Argument parsing
;;; ---------------------------------------------------------------------------

(defun parse-test-args (args)
  "Parse command-line ARGS for 'epsilon test'.
   Returns a plist with keys :modules, :verbose, :noclean, :update-snapshots,
   :test-mode, :changed, :force, :all, :fail-fast, :dependencies, :parallel,
   :sequential, :log-spec."
  (let ((modules '())
        (verbose nil) (noclean nil) (update-snapshots nil)
        (test-mode :unit) (changed nil) (force nil) (all-modules nil)
        (fail-fast nil) (dependencies nil) (parallel nil) (sequential nil)
        (log-spec nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
               ((string= arg "--verbose")       (setf verbose t) (incf i))
               ((string= arg "--noclean")        (setf noclean t) (incf i))
               ((string= arg "--update-snapshots") (setf update-snapshots t) (incf i))
               ((string= arg "--integration")    (setf test-mode :integration) (incf i))
               ((string= arg "--all")            (setf all-modules t) (incf i))
               ((string= arg "--fail-fast")      (setf fail-fast t) (incf i))
               ((string= arg "--dependencies")   (setf dependencies t) (incf i))
               ((string= arg "--changed")        (setf changed t) (incf i))
               ((string= arg "--force")          (setf force t) (incf i))
               ((string= arg "--sequential")     (setf sequential t) (incf i))
               ((and (string= arg "--log") (< (1+ i) (length args)))
                (setf log-spec (nth (1+ i) args)) (incf i 2))
               ((and (string= arg "--parallel") (< (1+ i) (length args)))
                (let ((n (handler-case (parse-integer (nth (1+ i) args))
                           (error () nil))))
                  (if (and n (plusp n))
                      (setf parallel n)
                      (progn
                        (format *error-output*
                                "Error: --parallel requires a positive integer~%")
                        (sb-ext:exit :code 1))))
                (incf i 2))
               ((string= arg "integration") (setf test-mode :integration) (incf i))
               ((string= arg "unit")        (setf test-mode :unit) (incf i))
               ((not (str:starts-with-p arg "-")) (push arg modules) (incf i))
               (t (incf i))))
    (list :modules (nreverse modules) :verbose verbose :noclean noclean
          :update-snapshots update-snapshots :test-mode test-mode
          :changed changed :force force :all all-modules
          :fail-fast fail-fast :dependencies dependencies
          :parallel parallel :sequential sequential :log-spec log-spec)))

;;; ---------------------------------------------------------------------------
;;; Module resolution
;;; ---------------------------------------------------------------------------

(defun resolve-modules (environment modules-to-test
                        &key all changed dependencies)
  "Resolve module names from user input, expanding --all, --changed, --dependencies.
   Returns the sorted list of resolved module names, or calls sb-ext:exit on error."
  (let ((modules modules-to-test))
    (when all
      (setf modules (all-module-names environment)))
    (when changed
      (let ((changed-modules (commands:detect-changed-modules environment)))
        (if changed-modules
            (progn
              (format t "Changed modules: ~{~A~^, ~}~%" changed-modules)
              (setf modules (append modules changed-modules)))
            (when (null modules)
              (format t "No changed modules detected.~%")
              (sb-ext:exit :code 0)))))
    (when (null modules)
      (format *error-output* "Usage: epsilon test [modules...] [options]~%")
      (format *error-output* "~%Run 'epsilon test --help' for options.~%")
      (sb-ext:exit :code 1))
    ;; Resolve names
    (let ((resolved (mapcar (lambda (mod)
                              (or (commands:resolve-module-name mod environment)
                                  (progn
                                    (format *error-output*
                                            "Warning: Module '~A' not found~%" mod)
                                    nil)))
                            modules)))
      (setf resolved (remove-duplicates (remove nil resolved) :test #'string=))
      (when dependencies
        (setf resolved (expand-with-dependencies resolved environment)))
      (topological-module-order resolved environment))))

;;; ---------------------------------------------------------------------------
;;; Test framework setup
;;; ---------------------------------------------------------------------------

(defun configure-log-spec (log-spec resolved)
  "Apply --log LOG-SPEC for modules in RESOLVED."
  (if (or (position #\= log-spec) (position #\+ log-spec)
          (position #\@ log-spec) (position #\, log-spec))
      (log:configure-from-spec log-spec)
      (let ((level-kw (or (log::preset-level log-spec)
                          (log::valid-level-p log-spec))))
        (when level-kw
          (log:configure-from-spec "warn,+simple")
          (dolist (mod resolved)
            (log:set-level mod level-kw))))))

(defun prepare-test-framework (environment &key force update-snapshots)
  "Load the test framework and apply --force / --update-snapshots settings."
  (loader:load-module environment "epsilon.test")
  (when force
    (let ((cache-var (find-symbol "*TEST-CACHE-ENABLED*"
                                  (find-package "EPSILON.TEST"))))
      (when cache-var
        (setf (symbol-value cache-var) nil))))
  (when update-snapshots
    (let ((set-update-fn (find-symbol "SET-UPDATE-SNAPSHOTS"
                                      (find-package "EPSILON.TEST"))))
      (when set-update-fn
        (funcall set-update-fn t)))))

;;; ---------------------------------------------------------------------------
;;; Sequential execution
;;; ---------------------------------------------------------------------------

(defun run-sequential (environment resolved &key verbose noclean test-mode
                                                 fail-fast)
  "Run RESOLVED modules sequentially.  Exits with code 1 on failure."
  (let ((run-tests-fn (find-symbol "RUN-TESTS" (find-package "EPSILON.TEST"))))
    (unless run-tests-fn
      (format *error-output* "Error: EPSILON.TEST:RUN-TESTS not found~%")
      (sb-ext:exit :code 1))
    (if fail-fast
        (dolist (module resolved)
          (multiple-value-bind (success-p)
              (handler-case
                  (funcall run-tests-fn environment (list module)
                           :verbose verbose :noclean noclean
                           :test-mode test-mode)
                (error (e)
                  (format *error-output* "~A~%" e)
                  (values nil)))
            (unless success-p
              (sb-ext:exit :code 1))))
        ;; Run-to-completion: pass all modules at once
        (multiple-value-bind (success-p failed-modules)
            (funcall run-tests-fn environment resolved
                     :verbose verbose :noclean noclean
                     :test-mode test-mode)
          (format t "~%~D module~:P, ~D passed, ~D failed~%"
                  (length resolved)
                  (- (length resolved) (length failed-modules))
                  (length failed-modules))
          (sb-ext:exit :code (if success-p 0 1))))))

;;; ---------------------------------------------------------------------------
;;; Parallel execution (summary + exit)
;;; ---------------------------------------------------------------------------

(defun run-parallel (environment resolved parallel
                     &key verbose noclean update-snapshots test-mode fail-fast)
  "Run RESOLVED modules in PARALLEL workers.  Exits when done."
  (let* ((results (run-modules-parallel
                   environment resolved parallel
                   :verbose verbose :noclean noclean
                   :update-snapshots update-snapshots
                   :test-mode test-mode :fail-fast fail-fast))
         (failures (remove-if #'cdr results)))
    (format t "~%~D module~:P, ~D passed, ~D failed~%"
            (length results)
            (- (length results) (length failures))
            (length failures))
    (when failures
      (format *error-output* "~%Failed modules:~%")
      (dolist (f failures)
        (format *error-output* "  ~A~%" (car f))))
    ;; Force-exit to avoid hanging on orphan threads from test servers
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code (if failures 1 0) :abort t)))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun run (environment args passthrough-args)
  "Handler for 'epsilon test' command."
  (declare (ignore passthrough-args))
  (let* ((opts (parse-test-args args))
         (modules-to-test (getf opts :modules))
         (parallel        (getf opts :parallel))
         (sequential      (getf opts :sequential))
         (verbose         (getf opts :verbose))
         (noclean         (getf opts :noclean))
         (update-snapshots (getf opts :update-snapshots))
         (test-mode       (getf opts :test-mode))
         (fail-fast       (getf opts :fail-fast))
         (log-spec        (getf opts :log-spec)))
    ;; Resolve modules (handles --all, --changed, --dependencies, exit on empty)
    (let ((resolved (resolve-modules environment modules-to-test
                                     :all (getf opts :all)
                                     :changed (getf opts :changed)
                                     :dependencies (getf opts :dependencies))))
      ;; Default to parallel when running multiple modules
      (when (and (null parallel) (not sequential) (> (length resolved) 1))
        (setf parallel (default-worker-count)))

      (when log-spec
        (configure-log-spec log-spec resolved))

      (format t "Testing ~D module~:P~@[ (parallel: ~D)~]~%"
              (length resolved) parallel)

      (prepare-test-framework environment
                              :force (getf opts :force)
                              :update-snapshots update-snapshots)

      (if parallel
          (run-parallel environment resolved parallel
                        :verbose verbose :noclean noclean
                        :update-snapshots update-snapshots
                        :test-mode test-mode :fail-fast fail-fast)
          (run-sequential environment resolved
                          :verbose verbose :noclean noclean
                          :test-mode test-mode :fail-fast fail-fast)))))
