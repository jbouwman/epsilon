;;;; compiler.lisp - Modern compiler interface
;;;;
;;;; Provides compile-source as a replacement for compile-file with
;;;; structured diagnostics, progress events, and FASL control.
(defpackage epsilon.compiler
  (:use cl)
  (:import (epsilon.compiler.config config)
           (epsilon.diagnostic diag)
           (epsilon.fs fs)
           (epsilon.source-location loc)
           (epsilon.compiler.progress progress)
           (epsilon.compiler.fasl fasl)
           (epsilon.reader reader)
           (epsilon.mutable-map mmap)))

;;; Compilation artifacts structure
(defstruct (compilation-artifacts (:constructor %make-compilation-artifacts) (:copier nil))
  "Output artifacts from compilation."
  (fasl-file nil :type (or pathname null))
  (source-map nil :type (or pathname null))
  (coverage-data nil :type t)
  (profile-data nil :type t)
  (metadata nil :type list))

(defun make-compilation-artifacts (&key fasl-file source-map coverage-data profile-data metadata)
  "Create compilation artifacts."
  (%make-compilation-artifacts :fasl-file fasl-file
                               :source-map source-map
                               :coverage-data coverage-data
                               :profile-data profile-data
                               :metadata metadata))

;;; Compilation result structure
(defstruct (compilation-result (:constructor %make-compilation-result) (:copier nil))
  "Complete result of a compilation operation."
  ;; Status
  (success nil :type boolean)
  (warnings-p nil :type boolean)
  (errors-p nil :type boolean)
  ;; Diagnostics
  (diagnostics nil :type list)
  (error-count 0 :type (integer 0))
  (warning-count 0 :type (integer 0))
  ;; Artifacts
  (artifacts nil :type (or compilation-artifacts null))
  ;; Timing
  (start-time 0 :type integer)
  (end-time 0 :type integer)
  (phases nil :type list)
  ;; Statistics
  (forms-processed 0 :type (integer 0))
  (functions-compiled 0 :type (integer 0))
  (macros-expanded 0 :type (integer 0))
  (bytes-consed 0 :type integer))

(defun make-compilation-result (&key success
                                     warnings-p
                                     errors-p
                                     diagnostics
                                     error-count
                                     warning-count
                                     artifacts
                                     start-time
                                     end-time
                                     phases
                                     forms-processed
                                     functions-compiled
                                     macros-expanded
                                     bytes-consed)
  "Create a compilation result."
  (%make-compilation-result :success success
                            :warnings-p warnings-p
                            :errors-p errors-p
                            :diagnostics diagnostics
                            :error-count (or error-count 0)
                            :warning-count (or warning-count 0)
                            :artifacts artifacts
                            :start-time (or start-time 0)
                            :end-time (or end-time 0)
                            :phases phases
                            :forms-processed (or forms-processed 0)
                            :functions-compiled (or functions-compiled 0)
                            :macros-expanded (or macros-expanded 0)
                            :bytes-consed (or bytes-consed 0)))

;;; Helper functions (must be defined before use)
(defun make-fasl-pathname (input-file config)
  "Generate output FASL pathname from input file and config."
  (let ((output-dir (config:compiler-config-output-directory config))
        (extension (config:compiler-config-fasl-extension config))
        (name (let ((s (namestring input-file)))
                (fs:strip-extension (fs:basename s)))))
    (if output-dir
      (make-pathname :defaults output-dir :name name :type extension)
      (make-pathname :defaults input-file :type extension))))

(defun extract-condition-location (condition)
  "Extract source location from SBCL condition.

   Resolution order:
   1. CST source-path resolution via epsilon.compile-integration's
      RESOLVE-SOURCE-PATH-LOCATION when *current-file-info* is set
      (compile-file path; gives byte-accurate sub-expression range).
   2. The location set by the real-time tracking hooks (top-level form
      granularity).
   3. SBCL's compiler-error-context for a bare file location."
  (declare (ignore condition))
  (let ((tracker (find-package :epsilon.compile-integration)))
    (when tracker
      (let* ((file-info-var (find-symbol "*CURRENT-FILE-INFO*" tracker))
             (resolver (find-symbol "RESOLVE-SOURCE-PATH-LOCATION" tracker))
             (file-info (when (and file-info-var (boundp file-info-var))
                          (symbol-value file-info-var)))
             (ctx (when (boundp 'sb-c::*compiler-error-context*)
                    sb-c::*compiler-error-context*))
             (path (when (and ctx (typep ctx 'sb-c::compiler-error-context))
                     (sb-c::compiler-error-context-original-source-path ctx))))
        (when (and file-info path resolver (fboundp resolver))
          (let ((loc (funcall resolver file-info path)))
            (when loc (return-from extract-condition-location loc)))))
      (let ((getter (find-symbol "GET-REAL-TIME-SOURCE-LOCATION" tracker)))
        (when (and getter (fboundp getter))
          (let ((loc (funcall getter)))
            (when loc (return-from extract-condition-location loc)))))))
  (when (and (boundp 'sb-c::*compiler-error-context*) sb-c::*compiler-error-context*)
    (let ((context sb-c::*compiler-error-context*))
      (when (typep context 'sb-c::compiler-error-context)
        (let ((file (sb-c::compiler-error-context-file-name context)))
          (when (and file (not (eq file :lisp)))
            (loc:make-source-location :file (if (pathnamep file)
                                                (namestring file)
                                                file))))))))

(defun format-source-context (file line &key (context-lines 2))
  "Extract source context around LINE from FILE as a formatted string."
  (when (and file (probe-file file))
    (handler-case (let ((ctx (loc:get-source-context-lines file
                                                           line
                                                           :before context-lines
                                                           :after context-lines)))
      (when ctx
        (format nil
                "~{~A~^~%~}"
                (mapcar (lambda (entry)
                          (format nil "~4D | ~A" (getf entry :number) (getf entry :text)))
                        ctx))))
      (error
       ()
       nil))))

(defun handle-sbcl-condition (condition severity)
  "Convert SBCL condition to epsilon diagnostic and emit it."
  (let* ((location (extract-condition-location condition))
         (message (princ-to-string condition))
         (diagnostic (diag:make-diagnostic :severity severity
                                           :message message
                                           :location location
                                           :source-context (when location
                                             (format-source-context (loc:source-location-file location)
                                                                    (loc:source-location-line location))))))
    (diag:emit-diagnostic diagnostic)))

;;; Macros for compilation control
(defmacro with-optimization-settings (qualities &body body)
  "Execute BODY with specified optimization qualities."
  (declare (ignore qualities))
  ;; Note: Can't actually set optimization at runtime with locally
  ;; Just execute the body
  `(progn ,@body))

(defmacro with-sbcl-compilation-hooks ((config) &body body)
  "Execute BODY with SBCL compiler hooks for capturing conditions."
  (declare (ignore config))
  `(handler-bind ((sb-c::compiler-error (lambda (c) (handle-sbcl-condition c :error) nil)) (warning (lambda (c)
                                                                                                            (handle-sbcl-condition c
                                                                                                                                   :warning)
                                                                                                            (muffle-warning c)))
                                                                                           (style-warning (lambda (c)
                                                                                                                  (handle-sbcl-condition c
                                                                                                                                         :style-warning)
                                                                                                                  (muffle-warning c))))
                 ,@body))

;;; Internal compilation function
(defun compile-with-config (input-file output-file config)
  "Compile INPUT-FILE to OUTPUT-FILE using CONFIG.
   Returns (values output-truename warnings-p failure-p).

   Routes through epsilon.compile:compile-file-safely so the CST-aware
   *compile-file-around* hook fires (installing the source-tracking
   hooks that populate *current-compilation-location*), and wraps the
   call in WITH-SBCL-COMPILATION-HOOKS so warnings flow through
   *diagnostic-handler* rather than escaping to stderr."
  (declare (ignorable config))
  ;; TODO: Use config optimization settings when we implement proper
  ;; optimization quality handling.
  (with-sbcl-compilation-hooks (config)
    (epsilon.compile:compile-file-safely input-file
                                         :output-file output-file
                                         :verbose nil
                                         :print nil)))

;;; Core compilation interface
(defun compile-source (input-file &key output-file (config config:*compiler-config*))
  "Compile a source file with full control and diagnostics.

   Returns a COMPILATION-RESULT with structured diagnostics,
   artifacts, and statistics.

   Unlike COMPILE-FILE, this function:
   - Returns structured diagnostics with source locations
   - Supports relocatable FASL generation
   - Provides real-time progress events
   - Supports instrumentation for coverage/profiling
   - Produces deterministic output for reproducible builds

   Example:
   (compile-source \"my-file.lisp\"
     :output-file \"my-file.fasl\"
     :config (make-compiler-config
               :optimize-qualities '((speed 3) (safety 0))
               :fasl-options (make-fasl-options
                               :path-mode :relative
                               :source-root #p\"/project/src/\")))"
  (let* ((input (pathname input-file))
         (output (or output-file (make-fasl-pathname input config)))
         (result (make-compilation-result :start-time (get-internal-real-time)))
         (diagnostics '())
         (error-count 0)
         (warning-count 0))
    ;; Set up diagnostic capture
    (flet ((collect-diagnostic (diag)
                               (push diag diagnostics)
                               (ecase (diag:diagnostic-severity diag)
                                 (:error
                                  (incf error-count))
                                 ((:warning :style-warning)
                                  (incf warning-count))
                                 ((:note :info :hint)))))
      ;; Bind our diagnostic handler
      (let ((diag:*diagnostic-handler* #'collect-diagnostic))
        (progress:emit-progress :compilation-started :file input)
        ;; Perform compilation with hooks
        (multiple-value-bind (output-truename sbcl-warnings-p sbcl-failure-p) (compile-with-config input
                                                                                                   output
                                                                                                   config)
          (setf (compilation-result-success result) (not sbcl-failure-p))
          (setf (compilation-result-warnings-p result) sbcl-warnings-p)
          (setf (compilation-result-errors-p result) sbcl-failure-p)
          (setf (compilation-result-error-count result) error-count)
          (setf (compilation-result-warning-count result) warning-count)
          ;; Build artifacts
          (when (and (not sbcl-failure-p) output-truename (probe-file output-truename))
            (setf (compilation-result-artifacts result)
                  (make-compilation-artifacts :fasl-file output-truename
                                              :metadata (list (cons :source-file (namestring input))
                                                              (cons :compile-time (get-universal-time))))))
          (progress:emit-progress :compilation-finished
                                  :file input
                                  :data (list :success (not sbcl-failure-p))))))
    ;; Finalize result
    (setf (compilation-result-end-time result) (get-internal-real-time))
    (setf (compilation-result-diagnostics result) (nreverse diagnostics))
    result))

;;; Compile forms (for REPL/eval usage)
(defun compile-forms (forms &key (name "anonymous"))
  "Compile a list of forms, returning a compilation result.

   NAME is for identification purposes (currently unused).
   Useful for compiling code that isn't in a file."
  (declare (ignore name))
  (let ((result (make-compilation-result :start-time (get-internal-real-time)))
        (diagnostics '()))
    (flet ((collect-diagnostic (diag) (push diag diagnostics)))
      (let ((diag:*diagnostic-handler* #'collect-diagnostic))
        (handler-bind ((warning (lambda (c)
                                  (handle-sbcl-condition c :warning)
                                  (muffle-warning c))))
          (dolist (form forms) (compile nil `(lambda () ,form))))))
    (setf (compilation-result-end-time result) (get-internal-real-time))
    (setf (compilation-result-diagnostics result) (nreverse diagnostics))
    (setf (compilation-result-success result) t)
    (setf (compilation-result-forms-processed result) (length forms))
    result))

;;; CST-aware form compilation
;;;
;;; SBCL stores per-warning navigation as an ORIGINAL-SOURCE-PATH on the
;;; compiler-error-context. The encoding is innermost-index-first, ending
;;; in the top-level form index, e.g. for (LAMBDA () (PROGN (+ 1 2)
;;; (- 3 UNDEF))) a warning inside (- 3 UNDEF) carries path (2 2 0) =
;;; "form 0, subindex 2, subindex 2." We walk the wrapper LAMBDA by these
;;; indices to find the offending cons cell, then look it up in the
;;; CST-derived position map.

(defun navigate-by-source-path (form-tree path)
  "Navigate FORM-TREE by an SBCL ORIGINAL-SOURCE-PATH. PATH is innermost-
   first and ends with the form index; non-integer markers (e.g. ORIGINAL-
   SOURCE-START structures) are skipped. Returns the cons cell at the path
   or NIL if the indices walk out of bounds."
  (let ((indices (rest (reverse (remove-if-not #'integerp path))))
        (current form-tree))
    (dolist (idx indices current)
      (unless (consp current)
        (return-from navigate-by-source-path nil))
      (when (or (minusp idx) (>= idx (length current)))
        (return-from navigate-by-source-path nil))
      (setf current (nth idx current)))))

(defun range-source-location (entry line-offsets file)
  "Build a source-location with start and (when available) end coordinates
   from a POSITIONS map ENTRY. ENTRY is the (START . END) pair that
   WALK-CST-DATUM stores, or NIL. Returns NIL when ENTRY is NIL."
  (when entry
    (let* ((start (if (consp entry) (car entry) entry))
           (end (when (consp entry) (cdr entry)))
           (start-lc (loc:offset-to-line-column line-offsets start))
           (end-lc (when end (loc:offset-to-line-column line-offsets end))))
      (loc:make-source-location :file file
                                :offset start
                                :end-offset end
                                :line (car start-lc)
                                :column (cdr start-lc)
                                :end-line (when end-lc (car end-lc))
                                :end-column (when end-lc (cdr end-lc))))))

(defun cst-warning-location (lambda-form positions line-offsets file)
  "Resolve the current SBCL warning to a byte-accurate source-location by
   walking sb-c::*compiler-error-context*'s ORIGINAL-SOURCE-PATH through
   LAMBDA-FORM and looking the resulting cons cell up in POSITIONS.
   Returns NIL when no path is available or it doesn't resolve to a cell
   tracked in POSITIONS."
  (let ((ctx (when (boundp 'sb-c::*compiler-error-context*)
               sb-c::*compiler-error-context*)))
    (when (and ctx (typep ctx 'sb-c::compiler-error-context))
      (let* ((path (sb-c::compiler-error-context-original-source-path ctx))
             (cell (navigate-by-source-path lambda-form path))
             (entry (when (consp cell) (mmap:get positions cell))))
        (range-source-location entry line-offsets file)))))

(defun build-cst-top-pairs (source)
  "Read SOURCE (a string) with the position-aware reader and return
   (top-pairs positions line-offsets). TOP-PAIRS is a list of
   (syntax-node . datum) for each non-trivia top-level node, both
   sharing the same cons cells that POSITIONS is keyed by.

   Reads incrementally and switches *package* on each top-level
   (in-package ...) directive (via epsilon.compile-integration's
   read-all-syntax-tracking-in-package when available) so symbols
   intern in the same packages SBCL would have used.  Package state
   is scoped to this call."
  (let* ((integration (find-package :epsilon.compile-integration))
         (read-fn (or (when integration
                        (find-symbol "READ-ALL-SYNTAX-TRACKING-IN-PACKAGE"
                                     integration))
                      'reader:read-all-syntax))
         (*package* *package*)
         (nodes (funcall read-fn source))
         (positions (mmap:make-map :test 'eq))
         (line-offsets (loc:build-line-offsets source))
         (top-pairs (let ((acc nil))
                      (dolist (node nodes (nreverse acc))
                        (let ((kind (reader:syntax-node-kind node)))
                          (unless (member kind '(:whitespace :comment))
                            (let ((datum (reader:syntax-node-to-datum node)))
                              (unless (eq datum reader:*cst-elided*)
                                (push (cons node datum) acc))))))))
         (walk-fn (when integration
                    (find-symbol "WALK-CST-DATUM" integration))))
    (when walk-fn
      (dolist (pair top-pairs)
        (funcall walk-fn (car pair) (cdr pair) positions)))
    (values top-pairs positions line-offsets)))

(defun %compile-cst-forms (top-pairs positions line-offsets name)
  "Compile each top-level CST datum with handlers that route warnings
   through *diagnostic-handler*, attaching CST-derived source locations
   (sub-expression-accurate via SBCL's compiler-error-context source
   path; falls back to the top-level form's location). Returns a
   compilation-result."
  (let ((result (make-compilation-result :start-time (get-internal-real-time)))
        (diagnostics '()))
    (flet ((collect-diagnostic (d)
             (push d diagnostics))
           (form-location (form)
             (range-source-location (mmap:get positions form)
                                    line-offsets name)))
      (let ((diag:*diagnostic-handler* #'collect-diagnostic))
        (dolist (pair top-pairs)
          (let* ((form (cdr pair))
                 (form-loc (form-location form))
                 (lambda-form `(lambda () ,form)))
            (handler-bind
                ((warning
                   (lambda (c)
                     (diag:emit-diagnostic
                      (diag:make-diagnostic
                       :severity :warning
                       :message (princ-to-string c)
                       :location (or (cst-warning-location
                                      lambda-form positions
                                      line-offsets name)
                                     form-loc)))
                     (muffle-warning c))))
              (compile nil lambda-form))))))
    (setf (compilation-result-end-time result) (get-internal-real-time))
    (setf (compilation-result-diagnostics result) (nreverse diagnostics))
    (setf (compilation-result-success result) t)
    (setf (compilation-result-warnings-p result) (not (null diagnostics)))
    (setf (compilation-result-warning-count result) (length diagnostics))
    (setf (compilation-result-forms-processed result) (length top-pairs))
    result))

(defun compile-forms-from-cst (code &key (name "anonymous"))
  "Compile CODE string using the Epsilon reader CST for source tracking.
   Reads CODE once with the position-aware reader, builds a position map
   keyed by EQ on the cons cells produced by SYNTAX-NODE-TO-DATUM, then
   compiles those same cons cells. Diagnostics emitted during compile are
   resolved to the offending sub-expression's byte offset and line/column
   when SBCL's compiler-error-context provides a source path; otherwise
   they fall back to the originating top-level form's location.
   NAME is recorded as the location's file (defaults to \"anonymous\").
   Returns a compilation-result."
  (multiple-value-bind (top-pairs positions line-offsets)
      (build-cst-top-pairs code)
    (%compile-cst-forms top-pairs positions line-offsets name)))

(defun compile-source-from-cst (input-file)
  "Compile INPUT-FILE using only the Epsilon CST reader (Stage 2 of
   IMPL-320). The file is read once into the position-aware CST, every
   top-level form is compiled in-memory via COMPILE, and diagnostics
   carry byte-accurate sub-expression locations against the file's
   absolute path.

   Unlike COMPILE-SOURCE this does not produce a FASL — the goal is
   diagnostics-only flows: LSP analysis, lint, pre-flight error
   surfacing. Read-time evaluation that compile-file honors (#.expr,
   feature expressions, mid-file in-package) is *not* honored here:
   the CST reader parses structurally and does not execute forms
   between reads. Use COMPILE-SOURCE for files that depend on those.

   Returns a compilation-result."
  (let* ((path (pathname input-file))
         (source (with-open-file (in path :direction :input
                                          :external-format :utf-8)
                   (let ((s (make-string (file-length in))))
                     (read-sequence s in)
                     (if (find #\Nul s :from-end t)
                         (string-right-trim '(#\Nul) s)
                         s)))))
    (multiple-value-bind (top-pairs positions line-offsets)
        (build-cst-top-pairs source)
      (%compile-cst-forms top-pairs positions line-offsets
                          (namestring path)))))

;;; Module/project compilation (stubs for future implementation)
(defun compile-module (module-name &key force config)
  "Compile an epsilon module with dependency handling.

   Returns (values success-p results)."
  (declare (ignore module-name force config))
  ;; Placeholder - full implementation requires loader integration
  (values nil nil))

(defun compile-project (project-root &key config output-dir)
  "Compile an entire project.

   Returns (values success-p results)."
  (declare (ignore project-root config output-dir))
  ;; Placeholder - full implementation requires project discovery
  (values nil nil))
