;;;; Test CLI Command Handler
;;;;
;;;; Provides the `epsilon test` command implementation.
;;;; Registered via :commands in module.sexp and loaded lazily
;;;; when the test command is invoked.
(defpackage epsilon.test.cli
  (:use :cl)
  (:import (epsilon.string str)
           (epsilon.loader loader)
           (epsilon.graph graph)
           (epsilon.log log)
           (epsilon.commands commands)
           (epsilon.test test)
           (epsilon.test.suite suite)
           (epsilon.build.pool pool)
           (epsilon.sys.thread thread)
           (epsilon.sys.lock lock))
  (:export #:run))

;;; ---------------------------------------------------------------------------
;;; Platform utilities
;;; ---------------------------------------------------------------------------
(defun cpu-count ()
  "Return the number of available CPU cores."
  (max 1
       (handler-case (sb-alien:alien-funcall
         (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:long))
         ;; _SC_NPROCESSORS_ONLN = 58 on Darwin, 84 on Linux
         #+darwin 58
         #+linux 84)
         (error
          ()
          4))))

(defun default-worker-count ()
  "Return the default number of parallel test workers."
  1)

;;; ---------------------------------------------------------------------------
;;; Module dependency utilities
;;; ---------------------------------------------------------------------------
(defun %resolve-dep-name (dep-name)
  "Resolve a dependency name to a concrete module name.
DEP-NAME may be a module name (e.g. \"epsilon.http\") or a virtual name
from :provides (e.g. \"epsilon.net\" provided by \"epsilon.linux\").
Returns the concrete module name, or DEP-NAME if no resolution needed."
  (let ((direct (loader:get-module dep-name)))
    (if direct
        dep-name
        ;; Not a direct module name -- search :provides
        (let ((providers (loader:query-modules :provides dep-name)))
          (if providers
              (loader:module-name (first providers))
              dep-name)))))

(defun module-requires (module-name)
  "Return the list of dependency names for MODULE-NAME.
Resolves virtual :provides names to concrete module names."
  (let* ((mod (loader:get-module module-name))
         (location (when mod
                     (loader:module-location mod)))
         (project (when location
                    (handler-case (loader:load-module-project (commands:ensure-location-string location))
                      (error
                       ()
                       nil)))))
    (when project
      (mapcar #'%resolve-dep-name (loader:module-project-requires project)))))

(defun all-module-names ()
  "Return all module names for the current platform."
  (let ((platform (string-downcase (symbol-name (epsilon.sys.env:platform)))))
    (loop for mod in (loader:query-modules)
          for name = (loader:module-name mod)
          for metadata = (loader:module-metadata mod)
          for mod-platform = (getf metadata :platform)
          when (or (not mod-platform) (string-equal mod-platform platform))
          collect name)))

(defun topological-module-order (module-names)
  "Return MODULE-NAMES sorted in dependency order (dependencies first).
   Modules not in MODULE-NAMES are excluded from the result."
  (let ((name-set (make-hash-table :test 'equal)))
    (dolist (n module-names) (setf (gethash n name-set) t))
    (let ((sorted (graph:topological-sort module-names
                                          (lambda (name)
                                            ;; Only return deps that are in our set
                                            (remove-if-not (lambda (d)
                                                             (gethash d name-set))
                                                           (module-requires name)))
                                          :on-cycle
                                          :collect
                                          :test
                                          'equal)))
      sorted)))

;;; ---------------------------------------------------------------------------
;;; Single-module test runner
;;; ---------------------------------------------------------------------------
(defun run-module-tests (module-name &key verbose noclean update-snapshots test-mode tags exclude-tags)
  "Run tests for a single module.
   Returns (VALUES SUCCESS-P FAILED-MODULES)."
  (loader:load-module "epsilon.test")
  (when update-snapshots
    (test:set-update-snapshots t))
  (let ((run-tests-fn #'test:run-tests))
    (handler-case (funcall run-tests-fn
                           (list module-name)
                           :verbose
                           verbose
                           :noclean
                           noclean
                           :test-mode
                           test-mode
                           :tags
                           tags
                           :exclude-tags
                           exclude-tags)
      (error
       (e)
       (format *error-output* "Error testing ~A: ~A~%" module-name e)
       (values nil (list (cons module-name :error)))))))

;;; ---------------------------------------------------------------------------
;;; Parallel module runner
;;; ---------------------------------------------------------------------------
(defun preload-modules (modules)
  "Pre-load MODULES sequentially to avoid loader reentrancy in parallel mode.
   Returns (VALUES loaded-modules load-failures) where load-failures is
   a list of (module-name . :load-error) pairs."
  (let ((loaded '())
        (failures '()))
    (dolist (mod modules)
      (handler-case (progn
        (handler-bind ((sb-ext:compiler-note #'muffle-warning))
          (loader:load-module mod))
        (push mod loaded))
        (error
         (e)
         (format *error-output* "~&  FAIL ~A  (load error: ~A)~%" mod e)
         (push (cons mod :load-error) failures))))
    (values (nreverse loaded) (nreverse failures))))

(defun parallel-precompile (modules n-workers &key verbose force)
  "Compile MODULES + transitive deps across N-WORKERS subprocess workers.
   Returns the list of MODULES whose subgraph compiled successfully (i.e.
   safe to load in this image). Modules that failed to build (or were
   skipped because a dep failed) are dropped and their failure is printed.
   When the pool fails to start at all, falls back to sequential preload
   semantics by returning MODULES unchanged."
  (let ((summary (pool:build-modules-parallel modules
                                              :workers n-workers
                                              :verbose verbose
                                              :force force)))
    (let ((failed-or-skipped (mapcar #'car (pool:pool-summary-failures summary))))
      (when failed-or-skipped
        (format *error-output*
                "~&  ~D module~:P failed/skipped during parallel build:~%"
                (length failed-or-skipped))
        (dolist (n failed-or-skipped)
          (format *error-output* "    - ~A~%" n)))
      (remove-if (lambda (m) (member m failed-or-skipped :test #'string=))
                 modules))))

(defvar *module-timeout*
  120
  "Maximum seconds to allow a single module's tests to run before timing out.
   Set to NIL to disable module-level timeouts.")

(defun run-modules-parallel (modules n-workers
                                     &key
                                     verbose
                                     noclean
                                     update-snapshots
                                     test-mode
                                     fail-fast
                                     tags
                                     exclude-tags)
  "Run module test suites across N-WORKERS threads.
   Compiles modules across N subprocess workers (when feasible), then
   loads them sequentially in the parent image, then dispatches tests
   across N threads. Returns list of (module-name . success-p) pairs."
  ;; Subprocess-pool precompile when we have more than one worker; the
  ;; worker boot cost is not worth amortising for a single-slot run.
  (let ((to-load (if (> n-workers 1)
                     (parallel-precompile modules n-workers
                                          :verbose verbose)
                     modules)))
    (multiple-value-bind (loaded-modules load-failures) (preload-modules to-load)
    (let ((results (mapcar (lambda (f)
                             (cons (car f) nil))
                           load-failures))
          (queue (copy-list loaded-modules))
          (lock (lock:make-lock "test-parallel-lock"))
          (abort-flag nil)
          (threads '())
          (fresh-state-fn #'test:call-with-fresh-test-state))
      (dotimes (i n-workers)
        (push (thread:make-thread (lambda ()
                                       (loop (let ((module nil))
                                               (lock:with-lock (lock)
                                                 (when (or (null queue) abort-flag)
                                                   (return))
                                                 (setf module (pop queue)))
                                               (flet ((do-test ()
                                                               (multiple-value-bind (success-p) (run-module-tests module
                                                                                                                  :verbose
                                                                                                                  verbose
                                                                                                                  :noclean
                                                                                                                  noclean
                                                                                                                  :update-snapshots
                                                                                                                  update-snapshots
                                                                                                                  :test-mode
                                                                                                                  test-mode
                                                                                                                  :tags
                                                                                                                  tags
                                                                                                                  :exclude-tags
                                                                                                                  exclude-tags)
                                                                 success-p)))
                                                 (let ((ok (if *module-timeout*
                                                             (handler-case (sb-ext:with-timeout *module-timeout*
                                                               (if fresh-state-fn
                                                                 (funcall fresh-state-fn #'do-test)
                                                                 (do-test)))
                                                               (sb-ext:timeout
                                                                ()
                                                                (format *error-output*
                                                                        "~&  TIMEOUT ~A  (exceeded ~Ds)~%"
                                                                        module
                                                                        *module-timeout*)
                                                                nil))
                                                             (if fresh-state-fn
                                                               (funcall fresh-state-fn #'do-test)
                                                               (do-test)))))
                                                   (lock:with-lock (lock)
                                                     (push (cons module ok) results)
                                                     (when (and fail-fast (not ok))
                                                       (setf abort-flag t))))))))
                                     :name
                                     (format nil "test-worker-~D" i))
              threads))
      (dolist (th threads) (thread:join-thread th))
      (nreverse results)))))

;;; ---------------------------------------------------------------------------
;;; Argument parsing
;;; ---------------------------------------------------------------------------
(defun parse-test-args (args)
  "Parse command-line ARGS for 'epsilon test'.
   Returns a plist with keys :modules, :verbose, :noclean, :update-snapshots,
   :test-mode, :force, :all, :fail-fast, :parallel, :sequential, :log-spec,
   :tags, :exclude-tags."
  (let ((modules '())
        (verbose nil)
        (noclean nil)
        (update-snapshots nil)
        (test-mode :unit)
        (force nil)
        (all-modules nil)
        (fail-fast nil)
        (parallel nil)
        (sequential nil)
        (log-spec nil)
        (tags '())
        (exclude-tags '()))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
            ((string= arg "--verbose")
             (setf verbose t)
             (incf i))
            ((string= arg "--noclean")
             (setf noclean t)
             (incf i))
            ((string= arg "--update-snapshots")
             (setf update-snapshots t)
             (incf i))
            ((string= arg "--integration")
             (setf test-mode :integration)
             (incf i))
            ((string= arg "--all")
             (setf all-modules t)
             (incf i))
            ((string= arg "--fail-fast")
             (setf fail-fast t)
             (incf i))
            ((string= arg "--force")
             (setf force t)
             (incf i))
            ((string= arg "--sequential")
             (setf sequential t)
             (incf i))
            ((string= arg "--profile-heap")
             (setf suite::*profile-heap* t)
             (incf i))
            ((and (string= arg "--log") (< (1+ i) (length args)))
             (setf log-spec (nth (1+ i) args))
             (incf i 2))
            ((and (string= arg "--parallel") (< (1+ i) (length args)))
             (let ((n (handler-case (parse-integer (nth (1+ i) args))
                        (error
                         ()
                         nil))))
               (if (and n (plusp n))
                 (setf parallel n)
                 (progn
                   (format *error-output* "Error: --parallel requires a positive integer~%")
                   (sb-ext:exit :code 1))))
             (incf i 2))
            ((string= arg "integration")
             (setf test-mode :integration)
             (incf i))
            ((string= arg "unit")
             (setf test-mode :unit)
             (incf i))
            ((and (string= arg "--tag") (< (1+ i) (length args)))
             (push (intern (string-upcase (nth (1+ i) args)) :keyword) tags)
             (incf i 2))
            ((and (string= arg "--no-tag") (< (1+ i) (length args)))
             (push (intern (string-upcase (nth (1+ i) args)) :keyword) exclude-tags)
             (incf i 2))
            ((not (str:starts-with-p arg "-"))
             (push arg modules)
             (incf i))
            (t
             (incf i))))
    (list :modules
          (nreverse modules)
          :verbose
          verbose
          :noclean
          noclean
          :update-snapshots
          update-snapshots
          :test-mode
          test-mode
          :force
          force
          :all
          all-modules
          :fail-fast
          fail-fast
          :parallel
          parallel
          :sequential
          sequential
          :log-spec
          log-spec
          :tags
          (nreverse tags)
          :exclude-tags
          (nreverse exclude-tags))))

;;; ---------------------------------------------------------------------------
;;; Module resolution
;;;
;;; Selection semantic:
;;;
;;;   * No module args, no --all: derive the input set from the git diff
;;;     against main and walk transitive *dependents* (consumers).  This
;;;     is "dep-based testing": when a leaf module changes, every module
;;;     whose ABI/source contracts depend on it is exercised.  No
;;;     changes means nothing to test -- exit 0 with a status line.
;;;
;;;   * Explicit module names: test exactly those modules.  We do NOT
;;;     auto-expand to dependents -- a user (or a CI harness like
;;;     `nixos-integration-test`'s `epsilon test kreisler.hemidemi`)
;;;     who names a module wants exactly that scope, not the full
;;;     consumer fanout.
;;;
;;;   * --all: every known module.
;;;
;;; In every case the test-result cache (epsilon.test::*test-cache-
;;; enabled*) short-circuits any module whose content hash matches a
;;; prior successful run -- the per-symbol ABI hashing in epsilon.loader
;;; means that hash already incorporates transitive dep ABIs, so cache
;;; hits are sound across dep changes.  --force bypasses the cache.
;;; ---------------------------------------------------------------------------
(defun resolve-modules (modules-to-test &key all)
  "Resolve module names for the test run.  See file header for the
selection semantic.  Returns the topologically-sorted list of module
names, or calls sb-ext:exit on error / on the no-changes shortcut."
  (let ((modules modules-to-test))
    (cond
      (all
       (setf modules (all-module-names)))
      ((null modules)
       ;; Dep-based default: changed modules + their transitive
       ;; dependents (things that import the changed bits).  This is
       ;; the input that needs re-testing when an upstream changes; the
       ;; cache then skips any of those whose content hash is unchanged.
       (let ((changed-modules
              (commands:detect-changed-modules :include-deps t)))
         (cond
           (changed-modules
            (format t "Changed modules: ~{~A~^, ~}~%" changed-modules)
            (setf modules changed-modules))
           (t
            (format t "No changed modules detected.~%")
            (sb-ext:exit :code 0))))))
    (when (null modules)
      (format *error-output* "Usage: epsilon test [modules...] [options]~%")
      (format *error-output* "~%Run 'epsilon test --help' for options.~%")
      (sb-ext:exit :code 1))
    ;; Resolve names; explicit modules go through unchanged.
    (let ((resolved (mapcar (lambda (mod)
                              (or (commands:resolve-module-name mod)
                                  (progn
                                    (format *error-output* "Warning: Module '~A' not found~%" mod)
                                    nil)))
                            modules)))
      (setf resolved (remove-duplicates (remove nil resolved) :test #'string=))
      (topological-module-order resolved))))

;;; ---------------------------------------------------------------------------
;;; Test framework setup
;;; ---------------------------------------------------------------------------
(defun configure-log-spec (log-spec resolved)
  "Apply --log LOG-SPEC for modules in RESOLVED."
  (if (or (position #\= log-spec)
          (position #\+ log-spec)
          (position #\@ log-spec)
          (position #\, log-spec))
    (log:configure-from-spec log-spec)
    (let ((level-kw (or (log::preset-level log-spec) (log::valid-level-p log-spec))))
      (when level-kw
        (log:configure-from-spec "warn,+simple")
        (dolist (mod resolved) (log:set-level mod level-kw))))))

(defun prepare-test-framework (&key force update-snapshots)
  "Load the test framework and apply --force / --update-snapshots settings."
  (loader:load-module "epsilon.test")
  (when force
    (setf test:*test-cache-enabled* nil))
  (when update-snapshots
    (test:set-update-snapshots t)))

;;; ---------------------------------------------------------------------------
;;; Sequential execution
;;; ---------------------------------------------------------------------------
(defun run-sequential (resolved &key verbose noclean test-mode fail-fast tags exclude-tags)
  "Run RESOLVED modules sequentially.  Exits with code 1 on failure."
  (let ((run-tests-fn #'test:run-tests))
    (if fail-fast
      (dolist (module resolved)
        (multiple-value-bind (success-p) (handler-case (funcall run-tests-fn
                                                                (list module)
                                                                :verbose
                                                                verbose
                                                                :noclean
                                                                noclean
                                                                :test-mode
                                                                test-mode
                                                                :tags
                                                                tags
                                                                :exclude-tags
                                                                exclude-tags)
          (error
           (e)
           (format *error-output* "~A~%" e)
           (values nil)))
          (unless success-p
            (sb-ext:exit :code 1))))
      ;; Run-to-completion: pass all modules at once
      (multiple-value-bind (success-p failed-modules) (funcall run-tests-fn
                                                               resolved
                                                               :verbose
                                                               verbose
                                                               :noclean
                                                               noclean
                                                               :test-mode
                                                               test-mode
                                                               :tags
                                                               tags
                                                               :exclude-tags
                                                               exclude-tags)
        (format t
                "~%~D module~:P, ~D passed, ~D failed~%"
                (length resolved)
                (- (length resolved) (length failed-modules))
                (length failed-modules))
        (sb-ext:exit :code
                     (if success-p
                       0
                       1))))))

;;; ---------------------------------------------------------------------------
;;; Parallel execution (summary + exit)
;;; ---------------------------------------------------------------------------
(defun run-parallel (resolved parallel &key verbose noclean update-snapshots test-mode fail-fast tags exclude-tags)
  "Run RESOLVED modules in PARALLEL workers.  Exits when done."
  (let* ((results (run-modules-parallel resolved
                                        parallel
                                        :verbose
                                        verbose
                                        :noclean
                                        noclean
                                        :update-snapshots
                                        update-snapshots
                                        :test-mode
                                        test-mode
                                        :fail-fast
                                        fail-fast
                                        :tags
                                        tags
                                        :exclude-tags
                                        exclude-tags))
         (failures (remove-if #'cdr results)))
    (format t
            "~%~D module~:P, ~D passed, ~D failed~%"
            (length results)
            (- (length results) (length failures))
            (length failures))
    (when failures
      (format *error-output* "~%Failed modules:~%")
      (dolist (f failures) (format *error-output* "  ~A~%" (car f))))
    ;; Force-exit to avoid hanging on orphan threads from test servers
    (finish-output)
    (finish-output *error-output*)
    (sb-ext:exit :code
                 (if failures
                   1
                   0)
                 :abort
                 t)))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------
(defun run (args passthrough-args)
  "Handler for 'epsilon test' command.
   Dispatches to subcommands: run (default), list."
  (declare (ignore passthrough-args))
  (cond
    ((and args (string= (first args) "list"))
     (run-list (rest args)))
    ((and args (string= (first args) "run"))
     (run-tests-command (rest args)))
    (t
     (run-tests-command args))))

(defun run-tests-command (args)
  "Execute the 'epsilon test [run]' subcommand."
  (let* ((opts (parse-test-args args))
         (modules-to-test (getf opts :modules))
         (parallel (getf opts :parallel))
         (sequential (getf opts :sequential))
         (verbose (getf opts :verbose))
         (noclean (getf opts :noclean))
         (update-snapshots (getf opts :update-snapshots))
         (test-mode (getf opts :test-mode))
         (fail-fast (getf opts :fail-fast))
         (log-spec (getf opts :log-spec))
         (tags (getf opts :tags))
         (exclude-tags (getf opts :exclude-tags)))
    ;; Resolve modules.  Default: changed + transitive deps.  --all opts
    ;; into the full module list.  Explicit module names go through
    ;; transitive-dependent expansion too.  Cache hits short-circuit
    ;; per-module unless --force was passed.
    (let ((resolved (resolve-modules modules-to-test
                                     :all
                                     (getf opts :all))))
      ;; Default to parallel when running multiple modules
      (when (and (null parallel) (not sequential) (> (length resolved) 1))
        (setf parallel (default-worker-count)))
      (when log-spec
        (configure-log-spec log-spec resolved))
      (format t "Testing ~D module~:P~@[ (parallel: ~D)~]~%" (length resolved) parallel)
      (prepare-test-framework :force (getf opts :force) :update-snapshots update-snapshots)
      (if parallel
        (run-parallel resolved
                      parallel
                      :verbose
                      verbose
                      :noclean
                      noclean
                      :update-snapshots
                      update-snapshots
                      :test-mode
                      test-mode
                      :fail-fast
                      fail-fast
                      :tags
                      tags
                      :exclude-tags
                      exclude-tags)
        (run-sequential resolved
                        :verbose
                        verbose
                        :noclean
                        noclean
                        :test-mode
                        test-mode
                        :fail-fast
                        fail-fast
                        :tags
                        tags
                        :exclude-tags
                        exclude-tags)))))

;;; ---------------------------------------------------------------------------
;;; List subcommand
;;; ---------------------------------------------------------------------------
(defun parse-list-args (args)
  "Parse command-line ARGS for 'epsilon test list'.
   Returns a plist with keys :modules, :tags-only, :name."
  (let ((modules '())
        (tags-only nil)
        (name-pattern nil))
    (loop with i = 0
          while (< i (length args))
          for arg = (nth i args)
          do (cond
            ((string= arg "--tags")
             (setf tags-only t)
             (incf i))
            ((and (string= arg "--name") (< (1+ i) (length args)))
             (setf name-pattern (nth (1+ i) args))
             (incf i 2))
            ((not (str:starts-with-p arg "-"))
             (push arg modules)
             (incf i))
            (t
             (incf i))))
    (list :modules (nreverse modules)
          :tags-only tags-only
          :name name-pattern)))

(defun run-list (args)
  "Execute the 'epsilon test list' subcommand."
  (let* ((opts (parse-list-args args))
         (tags-only (getf opts :tags-only))
         (name-pattern (getf opts :name))
         (modules-to-list (getf opts :modules)))
    (loader:load-module "epsilon.test")
    ;; Default to all modules when none specified
    (let ((resolved (if modules-to-list
                      (remove nil
                              (mapcar (lambda (mod)
                                        (or (commands:resolve-module-name mod)
                                            (progn
                                              (format *error-output* "Warning: Module '~A' not found~%" mod)
                                              nil)))
                                      modules-to-list))
                      (all-module-names))))
      ;; Load test files for each module
      (dolist (module resolved)
        (handler-case
            (handler-bind ((sb-ext:compiler-note #'muffle-warning))
              (loader:load-module module)
              (loader:load-module-resources module :tests))
          (error (e)
            (format *error-output* "Warning: Could not load ~A: ~A~%" module e))))
      ;; Dispatch to tag listing or test listing
      (progn
        (if tags-only
          ;; List all tags
          (let ((known-tags (suite:list-tags)))
            (if known-tags
              (progn
                (format t "Known tags:~%")
                (dolist (tag known-tags)
                  (format t "  ~(~A~)~%" tag)))
              (format t "No tags found.~%")))
          ;; List tests as table
          (let* ((all-entries (suite:list-all-tests-with-hashes))
                 ;; Filter by --name substring if given
                 (entries (if name-pattern
                            (remove-if-not
                             (lambda (entry)
                               (search name-pattern
                                       (symbol-name (second entry))
                                       :test #'char-equal))
                             all-entries)
                            all-entries)))
            (if entries
              (let* (;; Build rows: (hash qualified-name tag description)
                     (rows (mapcar
                            (lambda (entry)
                              (let* ((hash (first entry))
                                     (sym (second entry))
                                     (test-id (third entry))
                                     (tag (suite:test-tag sym))
                                     (doc (or (documentation sym 'function) "")))
                                (list hash test-id
                                      (if tag (string-downcase (symbol-name tag)) "")
                                      doc)))
                            entries))
                     ;; Compute column widths
                     (hash-w (max 4 (reduce #'max rows :key (lambda (r) (length (first r))))))
                     (name-w (max 4 (reduce #'max rows :key (lambda (r) (length (second r))))))
                     (tag-w (max 3 (reduce #'max rows :key (lambda (r) (length (third r))))))
                     (desc-w 40))
                ;; Header
                (format t "~VA  ~VA  ~VA  ~A~%"
                        hash-w "HASH" name-w "NAME" tag-w "TAG" "DESCRIPTION")
                (format t "~VA  ~VA  ~VA  ~A~%"
                        hash-w (make-string hash-w :initial-element #\-)
                        name-w (make-string name-w :initial-element #\-)
                        tag-w (make-string tag-w :initial-element #\-)
                        (make-string desc-w :initial-element #\-))
                ;; Rows
                (dolist (row rows)
                  (let ((desc (fourth row)))
                    (format t "~VA  ~VA  ~VA  ~A~%"
                            hash-w (first row)
                            name-w (second row)
                            tag-w (third row)
                            (if (> (length desc) desc-w)
                              (concatenate 'string (subseq desc 0 (- desc-w 3)) "...")
                              desc))))
                (format t "~%~D test~:P~%" (length rows)))
              (format t "No tests found.~%"))))))))
