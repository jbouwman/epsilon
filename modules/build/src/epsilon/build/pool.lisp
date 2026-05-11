;;;; epsilon.build.pool - Subprocess pool that compiles modules
;;;; in dependency order across N SBCL workers.
;;;;
;;;; Each worker is a long-lived child process running
;;;; `epsilon build-worker`. The pool drives a manager thread per slot.
;;;; The manager pops a ready module from the work queue, sends a
;;;; (:compile :module NAME) frame to its worker, reads the reply, and
;;;; updates the shared graph state. When the worker retires or dies
;;;; the manager respawns. The pool exits when every module is either
;;;; built or skipped (because a dependency failed).

(defpackage epsilon.build.pool
  (:use :cl)
  (:import (epsilon.build.protocol proto)
           (epsilon.process process)
           (epsilon.process.spawn spawn)
           (epsilon.sys.lock lock)
           (epsilon.sys.variable cv)
           (epsilon.sys.thread thread)
           (epsilon.graph graph)
           (epsilon.loader loader)
           (epsilon.sys.env env)
           (epsilon.log log)
           (epsilon.file fs)
           (epsilon.commands commands))
  (:export #:build-modules-parallel
           #:print-build-plan
           #:pool-summary
           #:pool-summary-total
           #:pool-summary-succeeded
           #:pool-summary-failed
           #:pool-summary-skipped
           #:pool-summary-elapsed-ms
           #:pool-summary-workers-spawned
           #:pool-summary-failures))

(in-package :epsilon.build.pool)

;;; ---------------------------------------------------------------------------
;;; Module dependency helpers (parallels epsilon.test.cli's local helpers)
;;; ---------------------------------------------------------------------------

(defun resolve-dep-name (dep-name)
  "Resolve DEP-NAME to a concrete module name, walking :provides if needed."
  (if (loader:get-module dep-name)
      dep-name
      (let ((providers (loader:query-modules :provides dep-name)))
        (if providers
            (loader:module-name (first providers))
            dep-name))))

(defun module-requires (module-name)
  "Return the dependency module names for MODULE-NAME, with :provides
   names resolved to concrete modules."
  (let* ((mod (loader:get-module module-name))
         (location (when mod (loader:module-location mod)))
         (project (when location
                    (handler-case
                        (loader:load-module-project
                         (commands:ensure-location-string location))
                      (error () nil)))))
    (when project
      (mapcar #'resolve-dep-name
              (loader:module-project-requires project)))))

(defun expand-with-dependencies (module-names)
  "Return MODULE-NAMES + all transitive requires, preserving deepest-first
   order suitable for a fresh topological sort."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (labels ((walk (name)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (dolist (dep (module-requires name))
                   (walk dep))
                 (push name out))))
      (dolist (n module-names) (walk n)))
    (nreverse out)))

(defun topo-order (module-names)
  (let ((set (make-hash-table :test 'equal)))
    (dolist (n module-names) (setf (gethash n set) t))
    (graph:topological-sort
     module-names
     (lambda (name)
       (remove-if-not (lambda (d) (gethash d set))
                      (module-requires name)))
     :on-cycle :collect
     :test 'equal)))

;;; ---------------------------------------------------------------------------
;;; Pool data structures
;;; ---------------------------------------------------------------------------

(defstruct mjob
  (name "" :type string)
  (state :pending :type keyword)         ; :pending :ready :running :done :failed :skipped
  (deps nil :type list)                  ; module names this depends on (within build set)
  (deps-pending 0 :type fixnum)
  (dependents nil :type list)            ; module names that depend on this
  (critical-height 1 :type fixnum)       ; 1 + max height of any dependent (Hu's algo)
  module-hash
  error-reason
  worker-id
  (queued-at 0 :type integer)
  (started-at 0 :type integer)
  (finished-at 0 :type integer))

(defstruct slot
  (id 0 :type fixnum)
  (handle nil)                           ; spawn:process-handle (or nil while spawning)
  (in-stream nil)                        ; parent writes here
  (out-stream nil)                       ; parent reads here
  (log-path "" :type string)
  (current-job nil)                      ; mjob or nil
  (jobs-done 0 :type fixnum)             ; jobs completed by the current worker
  ;; Hash of module names this worker has already loaded into its image.
  ;; Used by sticky scheduling so a slot prefers ready jobs whose deps
  ;; it already has resident.
  (loaded-set (make-hash-table :test 'equal) :type hash-table)
  (state :idle :type keyword))           ; :idle :busy :dead

(defstruct pool
  jobs                                   ; hash: name -> mjob
  ready                                  ; list of names with deps-pending = 0 and state :pending
  total
  remaining                              ; not :done and not :failed and not :skipped
  succeeded
  failed
  skipped
  fail-fast
  aborted
  force
  verbose
  workers-spawned                        ; total worker process spawns (incl. respawns)
  start-time
  lock
  cv
  slots                                  ; list of slot
  epsilon-binary
  sbcl-binary                            ; or nil if pre-warmed core path is unavailable
  worker-core                            ; or nil
  heap-mb                                ; per-worker heap MB
  log-dir
  max-jobs-per-worker
  ;; Verbose-output column widths.  Computed once per build from the
  ;; jobs hash so each progress line lines up regardless of slot ID
  ;; or module-name length.
  (slot-id-width 1 :type fixnum)         ; digits in the highest slot id (1 .. ceil(log10 N))
  (name-width 0 :type fixnum))           ; max module-name length in this build set

(defstruct pool-summary
  (total 0 :type fixnum)
  (succeeded 0 :type fixnum)
  (failed 0 :type fixnum)
  (skipped 0 :type fixnum)
  (elapsed-ms 0 :type fixnum)
  (workers-spawned 0 :type fixnum)
  (failures nil :type list))             ; list of (name . reason)

;;; ---------------------------------------------------------------------------
;;; Graph initialisation
;;; ---------------------------------------------------------------------------

(defun build-graph (target-modules)
  "Resolve TARGET-MODULES + transitive deps, topologically sort, and
   return (VALUES JOBS READY-LIST), where JOBS is a name->mjob hash.
   Each mjob is annotated with its critical-path height -- the length
   of the longest dependents chain rooted at it -- so the scheduler can
   pop high-height jobs first (Hu's longest-path-first heuristic)."
  (let* ((expanded (expand-with-dependencies
                    (mapcar #'resolve-dep-name target-modules))))
    (multiple-value-bind (sorted cycles) (topo-order expanded)
      (when cycles
        (error "Cyclic module dependencies detected: ~A" cycles))
      (let ((jobs (make-hash-table :test 'equal))
            (ready '()))
        (dolist (name sorted)
          (setf (gethash name jobs)
                (make-mjob :name name)))
        ;; Wire deps and dependents (intersected with our set).
        (dolist (name sorted)
          (let* ((mj (gethash name jobs))
                 (deps (remove-if-not (lambda (d) (gethash d jobs))
                                      (module-requires name))))
            (setf (mjob-deps mj) deps
                  (mjob-deps-pending mj) (length deps))
            (dolist (d deps)
              (push name (mjob-dependents (gethash d jobs))))))
        ;; Compute critical-path height. Walk modules in reverse
        ;; topological order so each module's dependents are already
        ;; assigned before we compute its own height.
        (dolist (name (reverse sorted))
          (let ((mj (gethash name jobs))
                (h 0))
            (dolist (dep-name (mjob-dependents mj))
              (let ((dh (mjob-critical-height (gethash dep-name jobs))))
                (when (> dh h) (setf h dh))))
            (setf (mjob-critical-height mj) (1+ h))))
        ;; Initial ready set.
        (maphash (lambda (name mj)
                   (when (zerop (mjob-deps-pending mj))
                     (setf (mjob-state mj) :ready)
                     (push name ready)))
                 jobs)
        (values jobs (nreverse ready))))))

;;; ---------------------------------------------------------------------------
;;; Build plan printer (used by --print-plan and verbose start-of-build)
;;; ---------------------------------------------------------------------------

(defun %sort-plan (jobs)
  "Return JOBS as a list of mjobs sorted by (height descending, name asc).
   That's the order most useful to a human reader: high-criticality
   roots first, ties broken alphabetically."
  (let ((mjs '()))
    (maphash (lambda (k mj) (declare (ignore k)) (push mj mjs)) jobs)
    (sort mjs (lambda (a b)
                (cond
                  ((/= (mjob-critical-height a) (mjob-critical-height b))
                   (> (mjob-critical-height a) (mjob-critical-height b)))
                  (t (string< (mjob-name a) (mjob-name b))))))))

(defun print-build-plan (target-modules &key (stream *standard-output*))
  "Print the topo-sorted build plan for TARGET-MODULES.  Resolves
   transitive deps, runs the same dependency walk as the actual build,
   and prints one line per module annotated with critical-path height
   and (truncated) dep list.  Returns the (jobs ready) values from
   BUILD-GRAPH so callers can chain into the actual build if they want."
  (multiple-value-bind (jobs ready) (build-graph target-modules)
    (declare (ignore ready))
    (let* ((mjs (%sort-plan jobs))
           (max-height (if mjs
                           (reduce #'max mjs :key #'mjob-critical-height)
                           0))
           (max-name (reduce #'max mjs :key (lambda (mj) (length (mjob-name mj)))
                             :initial-value 0))
           (frontier-counts (make-hash-table)))
      (dolist (mj mjs)
        (incf (gethash (mjob-critical-height mj) frontier-counts 0)))
      (format stream "~%Build plan: ~D module~:P, longest dependency chain = ~D level~:P~%"
              (hash-table-count jobs) max-height)
      (format stream "Frontier widths (level: count):")
      (loop for h from max-height downto 1
            for n = (gethash h frontier-counts 0)
            when (plusp n)
              do (format stream " ~D:~D" h n))
      (format stream "~%~%")
      (dolist (mj mjs)
        (let ((deps (mjob-deps mj)))
          (format stream "  [~D] ~vA  ~A~%"
                  (mjob-critical-height mj)
                  max-name
                  (mjob-name mj)
                  (cond
                    ((null deps) "(no deps in this build set)")
                    ((<= (length deps) 4)
                     (format nil "<- ~{~A~^, ~}" deps))
                    (t
                     (format nil "<- ~{~A~^, ~}, +~D more"
                             (subseq deps 0 4) (- (length deps) 4)))))))
      (format stream "~%")
      (finish-output stream)
      (values jobs nil))))

;;; ---------------------------------------------------------------------------
;;; Worker subprocess lifecycle
;;; ---------------------------------------------------------------------------

(defun epsilon-binary-path ()
  (let ((home (env:getenv "EPSILON_HOME")))
    (unless home
      (error "EPSILON_HOME not set; cannot locate epsilon binary"))
    (let ((binary (format nil "~A/epsilon"
                          (string-right-trim "/" home))))
      (unless (probe-file binary)
        (error "epsilon binary not found at ~A" binary))
      binary)))

(defun sbcl-binary-path ()
  "Locate the SBCL executable that the worker subprocess should run.
   Mirrors what epsilon's shell wrapper does (EPSILON_SBCL > vendored
   build) so workers spawned with --core stay consistent with the
   parent's interpreter."
  (let ((env-sbcl (env:getenv "EPSILON_SBCL")))
    (when (and env-sbcl (probe-file env-sbcl))
      (return-from sbcl-binary-path env-sbcl)))
  (let ((home (env:getenv "EPSILON_HOME")))
    (when home
      (let ((vendor-dir (format nil "~A/vendor/sbcl/"
                                (string-right-trim "/" home))))
        (when (probe-file vendor-dir)
          (dolist (sub (directory (merge-pathnames "*/" vendor-dir)))
            (let ((sbcl (merge-pathnames "sbcl" sub)))
              (when (probe-file sbcl)
                (return-from sbcl-binary-path (namestring sbcl)))))))))
  nil)

(defun ensure-log-dir ()
  (let* ((root (or loader:*build-root* "_build"))
         (dir (format nil "~A/.parallel" (string-right-trim "/" root))))
    (fs:make-dirs dir)
    dir))

;;; ---------------------------------------------------------------------------
;;; Pre-warmed worker core (opportunity #1)
;;; ---------------------------------------------------------------------------
;;;
;;; A worker's biggest fixed cost is booting epsilon and loading
;;; epsilon.build. We can serialise that work once into a saved
;;; SBCL core, then invoke each worker as `sbcl --core worker.core ...`,
;;; which lands in the worker's main loop within ~100 ms instead of
;;; 1-2 s. The saved core is keyed on epsilon.build's module
;;; content hash; any source change to the worker, the protocol or the
;;; pool code itself rolls the hash and forces regeneration.

(defun worker-core-path (log-dir)
  (format nil "~A/worker.core" (string-right-trim "/" log-dir)))

(defun worker-core-key-path (log-dir)
  (format nil "~A/worker.core.key" (string-right-trim "/" log-dir)))

(defun current-worker-core-key ()
  "Return a stable key for the current epsilon.build image.
   We use the module's content hash; any edit to the package's source
   files rotates the key."
  (let* ((mod (loader:get-module "epsilon.build"))
         (hash (and mod (loader:module-content-hash mod))))
    (cond
      ((stringp hash) hash)
      (hash (princ-to-string hash))
      (t "no-content-hash"))))

(defun read-existing-core-key (log-dir)
  (let ((p (worker-core-key-path log-dir)))
    (when (probe-file p)
      (handler-case
          (with-open-file (s p :direction :input)
            (read-line s nil nil))
        (error () nil)))))

(defun write-core-key (log-dir key)
  (with-open-file (s (worker-core-key-path log-dir)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string key s)
    (terpri s)))

(defun build-worker-core (log-dir)
  "Spawn a one-shot helper subprocess that loads epsilon.build
   and saves an SBCL core image to LOG-DIR/worker.core via
   epsilon.build.core:save-and-exit. Returns T on success."
  (let* ((epsilon-bin (epsilon-binary-path))
         (core-path (worker-core-path log-dir))
         (form (format nil "(epsilon.build.core:save-and-exit ~S)"
                       core-path))
         (cwd (project-cwd-for-worker))
         (log-path (format nil "~A/core-build.log"
                           (string-right-trim "/" log-dir))))
    (when (probe-file core-path)
      (ignore-errors (delete-file core-path)))
    (let* ((spec (spawn:make-spawn-spec
                  epsilon-bin
                  :args (list epsilon-bin
                              "eval"
                              "--module" "epsilon.build"
                              form)
                  :stdin :null
                  :stdout :merge-stderr
                  :stderr log-path
                  :environment :inherit
                  :working-directory cwd
                  :process-group :new-group
                  :search-path nil))
           (handle (spawn:spawn spec)))
      (handler-case (spawn:wait-for-handle handle) (error () nil))
      (probe-file core-path))))

(defun ensure-worker-core (log-dir &key verbose)
  "Return the path to an up-to-date worker core, regenerating if
   needed. Returns NIL when generation fails (caller should fall back
   to the shell-wrapper invocation path)."
  (let* ((core-path (worker-core-path log-dir))
         (key (current-worker-core-key))
         (existing-key (read-existing-core-key log-dir)))
    (cond
      ((and (probe-file core-path)
            existing-key
            (string= existing-key key))
       (when verbose
         (format t "Reusing pre-warmed worker core: ~A~%" core-path)
         (finish-output))
       core-path)
      (t
       (when verbose
         (format t "Building pre-warmed worker core (this is one-time per build of epsilon.build)...~%")
         (finish-output))
       (let ((start (get-internal-real-time)))
         (cond
           ((build-worker-core log-dir)
            (write-core-key log-dir key)
            (when verbose
              (format t "Worker core built in ~,2Fs (~A)~%"
                      (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second)
                      core-path)
              (finish-output))
            core-path)
           (t
            (when verbose
              (format *error-output*
                      "Worker core build failed; falling back to shell-wrapper invocation~%")
              (finish-output *error-output*))
            nil)))))))

(defun project-cwd-for-worker ()
  "Return a directory where the worker will discover the same project.sexp
   as the parent. Falls back to EPSILON_HOME's parent when EPSILON_CWD
   is unset (e.g. when called from a non-shell-wrapped invocation)."
  (or (env:getenv "EPSILON_CWD")
      (let ((home (env:getenv "EPSILON_HOME")))
        (when home
          (or (handler-case
                  (namestring (truename
                               (format nil "~A/.."
                                       (string-right-trim "/" home))))
                (error () nil))
              "/")))))

(defun %spawn-spec-from-core (sbcl-binary worker-core heap-mb cwd log-path)
  "Spawn-spec invoking SBCL directly with --core, bypassing the shell
   wrapper. Saves the boot phase since the saved core has epsilon and
   epsilon.build already loaded."
  (spawn:make-spawn-spec
   sbcl-binary
   :args (list sbcl-binary
               "--core" worker-core
               "--noinform"
               "--dynamic-space-size" (princ-to-string heap-mb)
               "--no-sysinit"
               "--no-userinit"
               "--disable-debugger")
   :stdin :pipe
   :stdout :pipe
   :stderr log-path
   :environment :inherit
   :working-directory cwd
   :process-group :new-group
   :search-path nil))

(defun %spawn-spec-from-script (epsilon-binary max-jobs-per-worker cwd log-path)
  "Fallback spawn-spec that goes through the epsilon shell wrapper.
   Used when the pre-warmed core is unavailable or cannot be built."
  (spawn:make-spawn-spec
   epsilon-binary
   :args (list epsilon-binary
               "build-worker"
               "--max-jobs" (princ-to-string max-jobs-per-worker))
   :stdin :pipe
   :stdout :pipe
   :stderr log-path
   :environment :inherit
   :working-directory cwd
   :process-group :new-group
   :search-path nil))

(defun spawn-worker (slot epsilon-binary max-jobs-per-worker log-dir
                          &key worker-core sbcl-binary heap-mb)
  "Spawn a fresh worker subprocess for SLOT. When WORKER-CORE and
   SBCL-BINARY are provided, invoke SBCL directly with --core to skip
   epsilon's boot phase; otherwise fall back to the shell wrapper.
   Updates SLOT in place. Returns SLOT."
  (let* ((log-path (format nil "~A/worker-~D.log" log-dir (slot-id slot)))
         (cwd (project-cwd-for-worker))
         (spec (if (and worker-core sbcl-binary)
                   (%spawn-spec-from-core sbcl-binary worker-core
                                          (or heap-mb 2048)
                                          cwd log-path)
                   (%spawn-spec-from-script epsilon-binary
                                            max-jobs-per-worker
                                            cwd log-path)))
         (handle (spawn:spawn spec)))
    (setf (slot-handle slot) handle
          (slot-log-path slot) log-path
          (slot-in-stream slot)
          (sb-sys:make-fd-stream (spawn:process-handle-stdin-fd handle)
                                 :output t :external-format :utf-8
                                 :buffering :line)
          (slot-out-stream slot)
          (sb-sys:make-fd-stream (spawn:process-handle-stdout-fd handle)
                                 :input t :external-format :utf-8
                                 :buffering :full)
          (slot-state slot) :idle
          (slot-current-job slot) nil)
    slot))

(defun reap-worker (slot)
  "Close streams and reap the worker process for SLOT."
  (ignore-errors (close (slot-in-stream slot)))
  (ignore-errors (close (slot-out-stream slot)))
  (let ((handle (slot-handle slot)))
    (when handle
      (handler-case
          (spawn:handle-terminate-gracefully handle :timeout 3)
        (error () nil))
      (handler-case (spawn:wait-for-handle handle 1) (error () nil))))
  (setf (slot-handle slot) nil
        (slot-in-stream slot) nil
        (slot-out-stream slot) nil
        (slot-state slot) :dead
        (slot-current-job slot) nil)
  slot)

(defun await-hello (slot)
  "Wait for the worker's :hello frame. Returns T on success; on EOF or
   protocol error returns NIL and the slot is left dead."
  (handler-case
      (let ((frame (proto:read-frame (slot-out-stream slot))))
        (cond
          ((null frame) nil)
          ((and (consp frame) (eq (first frame) :hello))
           t)
          (t
           (log:warn "worker ~D: unexpected first frame ~S"
                     (slot-id slot) frame)
           nil)))
    (error (e)
      (log:warn "worker ~D: error awaiting hello: ~A" (slot-id slot) e)
      nil)))

;;; ---------------------------------------------------------------------------
;;; Graph mutation
;;; ---------------------------------------------------------------------------

(defun mark-success (pool name hash)
  "Caller holds POOL-LOCK."
  (let ((mj (gethash name (pool-jobs pool))))
    (setf (mjob-state mj) :done
          (mjob-module-hash mj) hash
          (mjob-finished-at mj) (get-internal-real-time))
    (incf (pool-succeeded pool))
    (decf (pool-remaining pool))
    (dolist (dep-name (mjob-dependents mj))
      (let ((dmj (gethash dep-name (pool-jobs pool))))
        (when (eq (mjob-state dmj) :pending)
          (decf (mjob-deps-pending dmj))
          (when (zerop (mjob-deps-pending dmj))
            (setf (mjob-state dmj) :ready)
            (push dep-name (pool-ready pool))))))
    (cv:condition-broadcast (pool-cv pool))))

(defun mark-failure (pool name reason)
  "Caller holds POOL-LOCK. Marks NAME failed and recursively marks
   dependents as :skipped. Aborts the whole pool when fail-fast."
  (let ((mj (gethash name (pool-jobs pool))))
    (when (eq (mjob-state mj) :running)
      (setf (mjob-state mj) :failed
            (mjob-error-reason mj) reason
            (mjob-finished-at mj) (get-internal-real-time))
      (incf (pool-failed pool))
      (decf (pool-remaining pool)))
    (labels ((propagate (n)
               (dolist (dep-name (mjob-dependents (gethash n (pool-jobs pool))))
                 (let ((dmj (gethash dep-name (pool-jobs pool))))
                   (when (member (mjob-state dmj) '(:pending :ready))
                     (setf (mjob-state dmj) :skipped
                           (mjob-error-reason dmj)
                           (format nil "blocked by failed dep: ~A" n))
                     (incf (pool-skipped pool))
                     (decf (pool-remaining pool))
                     (propagate dep-name))))))
      (propagate name))
    (when (pool-fail-fast pool)
      (setf (pool-aborted pool) t))
    (cv:condition-broadcast (pool-cv pool))))

;;; ---------------------------------------------------------------------------
;;; Job dispatch (called from a manager thread)
;;; ---------------------------------------------------------------------------

(defun %pick-best-ready (pool slot)
  "Pick the best ready job for SLOT and remove it from the ready list.
   Selection lexicographic priority:
     1. Maximum overlap with SLOT's already-loaded module set (sticky
        scheduling -- avoids reloading deps the worker doesn't have).
     2. Maximum critical-path height (Hu's longest-path-first).
   Falls back to FIFO order on full ties.
   Caller must hold POOL-LOCK. Returns the popped module name or NIL."
  (declare (ignorable slot))
  (let ((ready (pool-ready pool)))
    (unless ready (return-from %pick-best-ready nil))
    (let* ((loaded (and slot (slot-loaded-set slot)))
           (best nil)
           (best-overlap -1)
           (best-height -1))
      (dolist (name ready)
        (let* ((mj (gethash name (pool-jobs pool)))
               (overlap (if loaded
                            (count-if (lambda (d) (gethash d loaded))
                                      (mjob-deps mj))
                            0))
               (height (mjob-critical-height mj)))
          (when (or (> overlap best-overlap)
                    (and (= overlap best-overlap) (> height best-height)))
            (setf best name
                  best-overlap overlap
                  best-height height))))
      (setf (pool-ready pool) (delete best ready :test #'equal :count 1))
      best)))

(defun take-ready-job (pool slot)
  "Pop the best ready name for SLOT, or NIL if the pool should drain.
   Blocks until something becomes available."
  (lock:with-lock ((pool-lock pool))
    (loop
      (when (pool-aborted pool) (return nil))
      (when (zerop (pool-remaining pool)) (return nil))
      (let ((name (%pick-best-ready pool slot)))
        (when name
          (let ((mj (gethash name (pool-jobs pool))))
            (setf (mjob-state mj) :running
                  (mjob-started-at mj) (get-internal-real-time))
            (return name))))
      (cv:condition-wait (pool-cv pool) (pool-lock pool)))))

(defun %record-loaded (pool slot root)
  "Mark ROOT and its transitive deps (within the build set) as loaded
   in SLOT's image. Called after a worker reports :ok so subsequent
   take-ready-job calls can prefer jobs whose deps already overlap."
  (let ((loaded (slot-loaded-set slot))
        (jobs (pool-jobs pool)))
    (labels ((visit (n)
               (unless (gethash n loaded)
                 (setf (gethash n loaded) t)
                 (let ((mj (gethash n jobs)))
                   (when mj
                     (dolist (d (mjob-deps mj))
                       (visit d)))))))
      (visit root))))

(defun complete-job (pool name response slot)
  "Apply a worker's :ok / :error response under the pool lock."
  (lock:with-lock ((pool-lock pool))
    (let ((op (and (consp response) (first response))))
      (case op
        (:ok
         (mark-success pool name (getf (rest response) :module-hash))
         (%record-loaded pool slot name)
         (when (pool-verbose pool)
           ;; Columns: "  [SS] OK    NAME....  Tms"
           ;; SS  = slot id, right-justified in pool-slot-id-width digits
           ;; NAME = padded to pool-name-width so the elapsed column lines up
           ;; T   = right-justified ms in 6 cols (handles 0..999999ms cleanly)
           (format t "  [~vD] OK    ~v,A  ~6Dms~%"
                   (pool-slot-id-width pool)
                   (slot-id slot)
                   (pool-name-width pool)
                   name
                   (or (getf (rest response) :elapsed-ms) -1)))
         (finish-output))
        (:error
         (let ((reason (or (getf (rest response) :reason) "unknown error")))
           (mark-failure pool name reason)
           (format *error-output* "  [~vD] FAIL  ~v,A  ~A~%"
                   (pool-slot-id-width pool) (slot-id slot)
                   (pool-name-width pool) name
                   reason)
           (finish-output *error-output*)))
        (t
         (let ((reason (format nil "unexpected response: ~S" response)))
           (mark-failure pool name reason)
           (format *error-output* "  [~vD] PROTO ~v,A  ~A~%"
                   (pool-slot-id-width pool) (slot-id slot)
                   (pool-name-width pool) name
                   reason)
           (finish-output *error-output*)))))))

(defun dispatch-and-wait (pool slot name)
  "Send :compile to SLOT's worker and block on its reply. Returns
   :ok, :error, or :died."
  (let ((mj (gethash name (pool-jobs pool))))
    (setf (slot-current-job slot) mj))
  (handler-case
      (proto:write-frame (slot-in-stream slot)
                         (list :compile :module name :force (pool-force pool)))
    (error (e)
      (log:warn "worker ~D: write failed: ~A" (slot-id slot) e)
      (lock:with-lock ((pool-lock pool))
        (mark-failure pool name (format nil "worker write failed: ~A" e)))
      (return-from dispatch-and-wait :died)))
  (let ((response (handler-case (proto:read-frame (slot-out-stream slot))
                    (error (e)
                      (log:warn "worker ~D: read failed: ~A" (slot-id slot) e)
                      nil))))
    (cond
      ((null response)
       (lock:with-lock ((pool-lock pool))
         (mark-failure pool name "worker died before responding"))
       :died)
      (t
       (complete-job pool name response slot)
       (incf (slot-jobs-done slot))
       (if (eq (first response) :error) :error :ok)))))

(defun shutdown-worker (slot)
  "Send :shutdown to SLOT's worker, drain the :goodbye, and reap it.
   Used when SLOT has hit its job-count cap."
  (handler-case
      (progn
        (proto:write-frame (slot-in-stream slot) '(:shutdown))
        ;; Worker should reply with :goodbye then exit. Drain it,
        ;; ignoring any error.
        (handler-case (proto:read-frame (slot-out-stream slot))
          (error () nil)))
    (error () nil))
  (reap-worker slot))

;;; ---------------------------------------------------------------------------
;;; Manager thread (one per slot)
;;; ---------------------------------------------------------------------------

(defun manager-loop (pool slot)
  (loop
    (when (and (slot-handle slot) (eq (slot-state slot) :dead))
      (reap-worker slot))
    ;; Spawn a fresh worker for this slot if needed.
    (unless (slot-handle slot)
      (setf (slot-jobs-done slot) 0)
      (clrhash (slot-loaded-set slot))
      (incf (pool-workers-spawned pool))
      (spawn-worker slot
                    (pool-epsilon-binary pool)
                    (pool-max-jobs-per-worker pool)
                    (pool-log-dir pool)
                    :worker-core (pool-worker-core pool)
                    :sbcl-binary (pool-sbcl-binary pool)
                    :heap-mb (pool-heap-mb pool))
      (unless (await-hello slot)
        (log:warn "worker ~D never said hello; reaping" (slot-id slot))
        (reap-worker slot)
        ;; Bail on this slot if a fresh worker can't even start.
        (lock:with-lock ((pool-lock pool))
          (cv:condition-broadcast (pool-cv pool)))
        (return)))
    ;; Grab a job and run it.
    (let ((name (take-ready-job pool slot)))
      (unless name (return))
      (setf (slot-state slot) :busy)
      (let ((result (dispatch-and-wait pool slot name)))
        (setf (slot-current-job slot) nil
              (slot-state slot) :idle)
        (case result
          (:died
           (reap-worker slot)))    ; loop will respawn at top
        ;; Recycle this worker if it has hit its job cap. Spawning a
        ;; replacement happens at the top of the next iteration.
        (when (and (eq (slot-state slot) :idle)
                   (>= (slot-jobs-done slot) (pool-max-jobs-per-worker pool)))
          (shutdown-worker slot)))))
  (when (slot-handle slot)
    (shutdown-worker slot)))

;;; ---------------------------------------------------------------------------
;;; Public entry point
;;; ---------------------------------------------------------------------------

(defun default-worker-count ()
  "Default to the CPU count, but cap at 16 because most workspaces
   compile <200 modules and worker boot cost dominates beyond that. The
   --workers flag overrides for explicit large-host runs."
  (handler-case
      (let ((n (sb-alien:alien-funcall
                (sb-alien:extern-alien "sysconf"
                                       (function sb-alien:long sb-alien:long))
                #+linux 84 #+darwin 58)))
        (max 1 (min 16 n)))
    (error () 4)))

(defun build-modules-parallel (target-modules
                               &key
                                 workers
                                 (max-jobs-per-worker 20)
                                 force
                                 fail-fast
                                 verbose
                                 (use-core t)
                                 (heap-mb (parse-integer
                                           (or (env:getenv "EPSILON_HEAP_MB") "2048")
                                           :junk-allowed t)))
  "Compile TARGET-MODULES and their transitive dependencies across a
   pool of SBCL subprocess workers. Returns a POOL-SUMMARY.

   USE-CORE: when T (default), build/reuse a pre-warmed SBCL core
   image at _build/.parallel/worker.core so workers skip the
   epsilon-boot phase. Set to NIL to force the slower script-based
   invocation for debugging."
  (when (null target-modules)
    (return-from build-modules-parallel (make-pool-summary)))
  (let* ((log-dir (ensure-log-dir))
         (sbcl-binary (sbcl-binary-path))
         (worker-core (and use-core sbcl-binary
                           (ensure-worker-core log-dir :verbose verbose)))
         (n-workers (or workers (default-worker-count))))
    (multiple-value-bind (jobs ready) (build-graph target-modules)
      (let* ((total (hash-table-count jobs))
             (effective-workers (max 1 (min n-workers total)))
             (slot-id-width (max 1 (length (princ-to-string
                                            (max 0 (1- effective-workers))))))
             (name-width (let ((m 0))
                           (maphash (lambda (n mj)
                                      (declare (ignore mj))
                                      (setf m (max m (length n))))
                                    jobs)
                           m))
             (pool (make-pool
                    :jobs jobs
                    :ready ready
                    :total total
                    :remaining total
                    :succeeded 0
                    :failed 0
                    :skipped 0
                    :fail-fast fail-fast
                    :aborted nil
                    :force force
                    :verbose verbose
                    :workers-spawned 0
                    :start-time (get-internal-real-time)
                    :lock (lock:make-lock "epsilon-build-pool")
                    :cv (cv:make-condition-variable :name "epsilon-build-pool")
                    :slots nil
                    :epsilon-binary (epsilon-binary-path)
                    :sbcl-binary sbcl-binary
                    :worker-core worker-core
                    :heap-mb (or heap-mb 2048)
                    :log-dir log-dir
                    :max-jobs-per-worker max-jobs-per-worker
                    :slot-id-width slot-id-width
                    :name-width name-width)))
        (format t "Parallel build: ~D module~:P, ~D worker~:P (max-jobs/worker=~D)~A~%"
                total effective-workers max-jobs-per-worker
                (if worker-core " [pre-warmed core]" ""))
        (when verbose
          ;; Re-print the full plan so users can see what's queued and
          ;; what level of the dependency frontier each module sits on.
          ;; Re-uses the same machinery as `epsilon build --print-plan`.
          (print-build-plan target-modules))
        (finish-output)
        ;; Allocate slots and start manager threads.
        (let ((slots (loop for i from 0 below effective-workers
                           collect (make-slot :id i)))
              (threads '()))
          (setf (pool-slots pool) slots)
          (dolist (s slots)
            (push (thread:make-thread
                   (let ((s s)) (lambda () (manager-loop pool s)))
                   :name (format nil "pbuild-mgr-~D" (slot-id s)))
                  threads))
          (dolist (th threads)
            (handler-case (thread:join-thread th) (error () nil))))
        (let* ((elapsed-ms (round (* 1000 (/ (- (get-internal-real-time)
                                                (pool-start-time pool))
                                             internal-time-units-per-second))))
               (sum-ms 0)
               (failures (let (acc)
                           (maphash (lambda (n mj)
                                      (when (member (mjob-state mj)
                                                    '(:failed :skipped))
                                        (push (cons n (mjob-error-reason mj))
                                              acc)))
                                    jobs)
                           (nreverse acc))))
          ;; Sum per-module elapsed (started-at to finished-at) over all
          ;; succeeded jobs.  Ratio against wall-clock elapsed-ms gives
          ;; the realised parallelism factor -- the speedup the user
          ;; got from dependency-aware scheduling vs strictly serial.
          (maphash (lambda (n mj)
                     (declare (ignore n))
                     (when (and (eq (mjob-state mj) :done)
                                (plusp (mjob-started-at mj))
                                (plusp (mjob-finished-at mj)))
                       (incf sum-ms
                             (round (* 1000 (/ (- (mjob-finished-at mj)
                                                  (mjob-started-at mj))
                                               internal-time-units-per-second))))))
                   jobs)
          (when (and verbose (plusp elapsed-ms) (plusp (pool-succeeded pool)))
            (format t "Parallelism: wall=~,2Fs  sum=~,2Fs  speedup=~,2Fx (vs strictly serial)~%"
                    (/ elapsed-ms 1000.0)
                    (/ sum-ms 1000.0)
                    (/ sum-ms (float elapsed-ms 1.0))))
          (make-pool-summary
           :total total
           :succeeded (pool-succeeded pool)
           :failed (pool-failed pool)
           :skipped (pool-skipped pool)
           :elapsed-ms elapsed-ms
           :workers-spawned (pool-workers-spawned pool)
           :failures failures))))))
