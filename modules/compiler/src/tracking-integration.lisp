;;;; This module hooks into SBCL's compilation pipeline to provide
;;;; real-time source location information during compilation, including
;;;; accurate line numbers and form offsets.
(defpackage epsilon.compile-integration
  (:use cl)
  (:import (epsilon.compile-api api)
           (epsilon.source-location loc)
           (epsilon.compile-location location)
           (epsilon.log log)
           (epsilon.mutable-map mmap)
           (epsilon.reader reader))
  (:export
    #:*real-time-source-tracking*
    #:*current-compilation-location*
    #:*form-position-map*
    #:*cst-replace-reader*
    #:install-compiler-hooks
    #:uninstall-compiler-hooks
    #:with-source-tracking
    #:get-real-time-source-location
    #:track-form-processing
    #:enhanced-process-toplevel-form
    #:sbcl-source-path-to-location
    #:extract-line-from-form-number
    #:build-file-info-from-cst
    #:walk-cst-datum
    #:resolve-source-path-location
    #:read-all-syntax-tracking-in-package
    #:in-package-form-package
    ;; File info structure and functions
    #:compilation-file-info
    #:compilation-file-info-p
    #:compilation-file-info-pathname
    #:compilation-file-info-line-positions
    #:compilation-file-info-form-positions
    #:compilation-file-info-top-level-datums
    #:position-to-line-column
    ;; Integration functions
    #:enhance-logging-with-compilation-context
    ;; SBCL source path analysis
    #:extract-line-from-sbcl-source-path
    #:file-position-to-line-number))

(in-package epsilon.compile-integration)

;;; Global state for real-time tracking
(defvar *real-time-source-tracking*
  nil
  "When true, real-time source location tracking is active.")

(defvar *current-compilation-location*
  nil
  "The current source location being compiled.")

(defvar *form-position-map*
  nil
  "Hash table mapping forms to file positions during compilation.")

(defvar *original-process-toplevel-form*
  nil
  "Original SBCL process-toplevel-form function.")

(defvar *original-sub-find-source-paths*
  nil
  "Original SBCL sub-find-source-paths function.")

(defvar *original-do-forms-from-info*
  nil
  "Original SBCL %do-forms-from-info function (used by the optional CST
   reader replacement).")

(defvar *cst-replace-reader*
  nil
  "When true, CST-COMPILE-FILE-AROUND additionally hooks
   sb-c::%do-forms-from-info so that compile-file pulls its top-level
   forms from the pre-built CST datums instead of running cl:read on the
   file. The CST reader does not honor read-time evaluation (#.expr,
   feature expressions) or mid-file in-package directives, so this is
   off by default and safest for files without those features.")

;;; File position tracking utilities
(defstruct compilation-file-info
  "Information about a file being compiled.
   TOP-LEVEL-DATUMS is the ordered vector of top-level CST datums (one
   per non-trivia syntax-node), used together with FORM-POSITIONS to
   resolve sub-expression locations from SBCL source paths."
  (pathname nil :type (or pathname null))
  (stream nil :type (or stream null))
  (line-positions nil :type (or simple-vector null))
  (form-positions nil :type hash-table)
  (top-level-datums #() :type simple-vector))

(defvar *current-file-info*
  nil
  "Current file compilation information.")

;;; CST-based position tracking

(defun in-package-form-package (datum)
  "If DATUM is a top-level (in-package ...) form, return a package
   designator (string) for its argument; otherwise NIL.  Strings,
   keywords, and symbols are all accepted as package designators."
  (when (and (consp datum)
             (consp (cdr datum))
             (symbolp (first datum))
             (string-equal (symbol-name (first datum)) "IN-PACKAGE"))
    (let ((arg (second datum)))
      (cond ((stringp arg) arg)
            ((keywordp arg) (symbol-name arg))
            ((symbolp arg) (symbol-name arg))))))

(defun ensure-defpackage-defined (datum)
  "If DATUM is a top-level form whose head is named DEFPACKAGE (under
   any package -- cl:defpackage or extension macros like the one in
   epsilon.main that shadow it), and the named package does not yet
   exist, evaluate it so subsequent (in-package ...) directives in the
   same file can resolve.  Errors during evaluation are swallowed on
   the assumption that the real compile pass will surface them."
  (when (and (consp datum)
             (consp (cdr datum))
             (symbolp (first datum))
             (string-equal (symbol-name (first datum)) "DEFPACKAGE"))
    (let* ((name (second datum))
           (pkg-name (cond ((stringp name) name)
                           ((symbolp name) (symbol-name name)))))
      (when (and pkg-name (null (find-package pkg-name)))
        (handler-case (eval datum)
          (error () nil))))))

(defun read-all-syntax-tracking-in-package (source)
  "Like reader:read-all-syntax, but reads incrementally and switches
   *package* whenever a top-level (in-package ...) form is encountered.
   Top-level CL:DEFPACKAGE forms are also evaluated eagerly so the
   package exists in time for a following (in-package ...).  Subsequent
   symbols are interned in the new package so the resulting datums match
   what SBCL's reader would have produced.  Returns the list of syntax
   nodes (whitespace and comments included), in the same shape as
   reader:read-all-syntax."
  (let ((cur (reader:make-cursor :source source))
        (nodes '()))
    (loop for node = (reader:read-syntax cur)
          while node
          do (push node nodes)
             (when (eq (reader:syntax-node-kind node) :list)
               (let ((datum (reader:syntax-node-to-datum node)))
                 (ensure-defpackage-defined datum)
                 (let ((pkg-name (in-package-form-package datum)))
                   (when pkg-name
                     (let ((pkg (find-package pkg-name)))
                       (when pkg (setf *package* pkg))))))))
    (nreverse nodes)))

(defun read-file-to-string (pathname)
  "Read file contents as a string."
  (with-open-file (in pathname :direction :input :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (read-sequence s in)
      (if (find #\Nul s :from-end t)
          (string-right-trim '(#\Nul) s)
          s))))

(defun build-line-starts (source)
  "Build vector of line-start byte offsets from source string."
  (let ((starts (make-array 256 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 starts)
    (loop for i from 0 below (length source)
          when (char= (char source i) #\Newline)
          do (vector-push-extend (1+ i) starts))
    starts))

(defun walk-cst-datum (node datum positions)
  "Walk CST node and datum tree in parallel, recording byte ranges.
   For each cons cell in DATUM, records a (START . END) byte-offset
   pair from the corresponding CST node into POSITIONS (a mutable EQ
   hash table). This gives byte-accurate ranges for every sub-expression,
   not just top-level forms; readers that only care about the start
   should take CAR of the entry."
  (let ((cst-loc (reader:syntax-node-location node)))
    (when cst-loc
      (let ((offset (loc:source-location-offset cst-loc))
            (end (loc:source-location-end-offset cst-loc)))
        (when (consp datum)
          (mmap:put! positions datum (cons offset end)))
        (when (and (consp datum)
                   (reader:syntax-node-children node))
          (let ((non-trivia (remove-if
                             (lambda (c) (member (reader:syntax-node-kind c)
                                                 '(:whitespace :comment)))
                             (reader:syntax-node-children node))))
            (case (reader:syntax-node-kind node)
              ;; Sugar nodes: datum = (QUOTE x), CST has one meaningful child
              ((:quote :backquote :function :unquote :splice)
               (when (and non-trivia (cdr datum))
                 (walk-cst-datum (first non-trivia) (second datum) positions)))
              ;; List nodes: walk elements paired with non-trivia children
              (:list
               (loop for child in non-trivia
                     for cell on datum
                     do (walk-cst-datum child (car cell) positions)))
              ;; Other compound nodes (vectors, maps, sets)
              (otherwise
               (when (listp datum)
                 (loop for child in non-trivia
                       for cell on datum
                       do (walk-cst-datum child (car cell) positions)))))))))))

(defun build-file-info-from-cst (pathname)
  "Build compilation-file-info from the Epsilon reader CST.
   Single pass: reads file once with the position-aware reader,
   walks the CST to record byte ranges for every cons cell, and
   captures the ordered list of top-level datums so SBCL source
   paths can be resolved to specific cells.

   Reads incrementally and switches *package* on each top-level
   (in-package ...) directive so symbols intern in the same package
   SBCL would have used.  Package changes are scoped to this call.
   Forms elided by #+/#- reader conditionals are dropped from the
   datum vector so they don't reach the downstream compile pass."
  (let ((source (read-file-to-string pathname))
        (positions (mmap:make-map :test 'eq))
        (datums '()))
    (let* ((*package* *package*)
           (nodes (read-all-syntax-tracking-in-package source))
           (line-starts (build-line-starts source)))
      (dolist (node nodes)
        (unless (member (reader:syntax-node-kind node) '(:whitespace :comment))
          (let ((datum (reader:syntax-node-to-datum node)))
            (unless (eq datum reader:*cst-elided*)
              (walk-cst-datum node datum positions)
              (push datum datums)))))
      (make-compilation-file-info
       :pathname (pathname pathname)
       :line-positions (coerce line-starts 'simple-vector)
       :form-positions positions
       :top-level-datums (coerce (nreverse datums) 'simple-vector)))))

(defun resolve-source-path-location (file-info source-path)
  "Walk FILE-INFO's top-level CST datums by SBCL's SOURCE-PATH (innermost
   indices first, ending in form-index) to find the offending cons cell,
   then look up its byte range in FORM-POSITIONS. Returns a source-location
   covering [start, end) or NIL when the path doesn't resolve to a tracked
   cell. SBCL's reader and the CST reader produce structurally identical
   list shapes for the same source, so navigation by index matches even
   when individual symbols (interned in different packages) wouldn't
   compare EQUAL."
  (when (and file-info source-path)
    (let* ((cleaned (remove-if-not #'integerp source-path))
           (reversed (reverse cleaned)))
      (when reversed
        (let* ((form-idx (first reversed))
               (path (rest reversed))
               (datums (compilation-file-info-top-level-datums file-info)))
          (when (and (integerp form-idx)
                     (>= form-idx 0)
                     (< form-idx (length datums)))
            (let ((current (aref datums form-idx)))
              (dolist (idx path)
                (cond ((not (consp current)) (return))
                      ((or (minusp idx) (>= idx (length current)))
                       (setf current nil)
                       (return))
                      (t (setf current (nth idx current)))))
              (when (consp current)
                (let ((entry (mmap:get
                              (compilation-file-info-form-positions file-info)
                              current)))
                  (when entry
                    (let* ((start (if (consp entry) (car entry) entry))
                           (end (when (consp entry) (cdr entry)))
                           (line-offsets
                            (compilation-file-info-line-positions file-info))
                           (start-lc (loc:offset-to-line-column
                                      line-offsets start))
                           (end-lc (when end
                                     (loc:offset-to-line-column
                                      line-offsets end))))
                      (loc:make-source-location
                       :file (namestring
                              (compilation-file-info-pathname file-info))
                       :offset start
                       :end-offset end
                       :line (car start-lc)
                       :column (cdr start-lc)
                       :end-line (when end-lc (car end-lc))
                       :end-column (when end-lc (cdr end-lc))))))))))))))

(defun position-to-line-column (file-info char-position)
  "Convert character position to line and column."
  (when (and file-info char-position)
    (let ((line-positions (compilation-file-info-line-positions file-info)))
      (when line-positions
        (let ((line-num (position-if (lambda (pos)
                                       (> pos char-position))
                                     line-positions)))
          (if line-num
            (values (1+ line-num) (1+ (- char-position (aref line-positions (1- line-num)))))
            (values (length line-positions)
                    (1+ (- char-position (aref line-positions (1- (length line-positions))))))))))))

;;; SBCL integration functions

(defun patch-file-info-positions (form-index)
  "Replace SBCL's recorded position for top-level form FORM-INDEX with the
   byte offset of the corresponding CST datum. SBCL records the position
   it was AT before reading each form, which lands on whatever leading
   trivia (comments / whitespace / a previous form's trailing newline)
   sits ahead of the form. Our CST start offset points at the form's
   opening character. The positions array is what eventually populates
   debug-source-start-positions in the FASL, so debuggers and stack
   traces inherit the byte-accurate per-form positions."
  (when (and *current-file-info* (integerp form-index))
    (let* ((datums (compilation-file-info-top-level-datums *current-file-info*))
           (positions (compilation-file-info-form-positions *current-file-info*)))
      (when (and (< form-index (length datums))
                 (boundp 'sb-c::*source-info*)
                 sb-c::*source-info*)
        (let* ((datum (aref datums form-index))
               (entry (when (consp datum) (mmap:get positions datum)))
               (start (cond ((consp entry) (car entry))
                            ((numberp entry) entry))))
          (when start
            (let* ((file-info (sb-c::source-info-file-info sb-c::*source-info*))
                   (sbcl-positions (sb-c::file-info-positions file-info)))
              (when (and sbcl-positions
                         (< form-index (length sbcl-positions)))
                (setf (aref sbcl-positions form-index) start)))))))))

(defun enhanced-process-toplevel-form (form path compile-time-too)
  "Enhanced version of SBCL's process-toplevel-form with source tracking."
  (declare (list path))
  ;; Track current form processing and extract line numbers
  (when *real-time-source-tracking*
    (track-form-processing form path)
    ;; NEW: Extract and set current compilation location
    (let ((location (extract-location-from-sbcl-state form path)))
      (when location
        (setf *current-compilation-location* location)))
    ;; Patch file-info-positions for this form so the FASL's debug
    ;; source carries CST-derived byte offsets. PATH at top-level entry
    ;; looks like (ORIGINAL-SOURCE-START depth form-idx).
    (when (and (consp path)
               (eq (first path) 'sb-c::original-source-start))
      (patch-file-info-positions (third path))))
  ;; Call original function
  (funcall *original-process-toplevel-form* form path compile-time-too))

(defun enhanced-sub-find-source-paths (form path depth)
  "Enhanced version of sub-find-source-paths that tracks positions.
   Note: The epsilon SBCL fork adds a DEPTH argument to the standard signature."
  (when *real-time-source-tracking*
    ;; Extract position information if we have current file info
    (when (and *current-file-info* form)
      (let* ((entry (mmap:get (compilation-file-info-form-positions *current-file-info*) form))
             (start (cond ((consp entry) (car entry))
                          ((numberp entry) entry)))
             (end (when (consp entry) (cdr entry))))
        (when start
          (multiple-value-bind (line column) (position-to-line-column *current-file-info* start)
            (multiple-value-bind (end-line end-column)
                (when end (position-to-line-column *current-file-info* end))
              (setf *current-compilation-location*
                    (loc:make-source-location
                     :file (namestring (compilation-file-info-pathname *current-file-info*))
                     :offset start
                     :end-offset end
                     :line line
                     :column column
                     :end-line end-line
                     :end-column end-column))))))))
  ;; Call original function
  (funcall *original-sub-find-source-paths* form path depth))

(defun track-form-processing (form path)
  "Track the processing of a form during compilation."
  (when *real-time-source-tracking*
    ;; Update current location based on SBCL's internal state
    (let ((location (extract-location-from-sbcl-state form path)))
      (when location
        (setf *current-compilation-location* location)
        ;; Location is now available for logging system to use
        nil))))

(defun extract-location-from-sbcl-state (form path)
  "Extract source location from SBCL's current compilation state."
  (let ((file nil)
        (line nil)
        (column nil))
    ;; Get file from compiler state
    (setf file
          (cond
            ((and (boundp 'sb-c::*compile-file-pathname*) sb-c::*compile-file-pathname*)
             (namestring sb-c::*compile-file-pathname*))
            ((and *current-file-info* (compilation-file-info-pathname *current-file-info*))
             (namestring (compilation-file-info-pathname *current-file-info*)))
            (t
             nil)))
    ;; NEW: Extract line numbers from SBCL source path system
    (multiple-value-bind (extracted-line extracted-column) (extract-line-from-sbcl-source-path path)
      (when extracted-line
        (setf line extracted-line column extracted-column)))
    ;; Fallback: Extract line/column from our enhanced tracking
    (when (and (not line) *current-file-info* form)
      (let* ((entry (mmap:get (compilation-file-info-form-positions *current-file-info*) form))
             (start (cond ((consp entry) (car entry))
                          ((numberp entry) entry))))
        (when start
          (multiple-value-setq (line column) (position-to-line-column *current-file-info* start)))))
    (when file
      (loc:make-source-location :file file :line line :column column))))

;;; Hook installation and management
(defun source-info-matches-cst-p (source-info)
  "True when SOURCE-INFO refers to the same file as *current-file-info*.
   We compare truenames so that compile-file recursive invocations on
   different files leave the recursive ones untouched."
  (when (and *current-file-info*
             source-info
             (fboundp 'sb-c::source-info-file-info))
    (let* ((file-info (sb-c::source-info-file-info source-info))
           (sbcl-truename (when (and file-info
                                     (fboundp 'sb-c::file-info-truename))
                            (sb-c::file-info-truename file-info)))
           (cst-pathname (compilation-file-info-pathname *current-file-info*)))
      (and sbcl-truename cst-pathname
           (equal (truename sbcl-truename) (truename cst-pathname))))))

(defun enhanced-%do-forms-from-info (function info condition-name)
  "Replacement for sb-c::%do-forms-from-info. When *cst-replace-reader*
   is true and INFO refers to the same file as *current-file-info*, walk
   the pre-built CST top-level datums instead of running cl:read on the
   source. SBCL's compile-file pipeline then sees our cons cells, so the
   stored top-level form (and form-numbers in code-locations) carry
   CST identity and our positions map covers them directly.

   FUNCTION's calling convention matches SBCL's: (form &key current-index).
   We pass each datum with the index of its non-trivia top-level node."
  (cond
    ((and *cst-replace-reader* (source-info-matches-cst-p info))
     (let ((datums (compilation-file-info-top-level-datums *current-file-info*)))
       (loop for datum across datums
             for idx from 0
             do (funcall function datum :current-index idx))))
    (t
     (funcall *original-do-forms-from-info* function info condition-name))))

(defun install-compiler-hooks ()
  "Install hooks into SBCL's compilation system."
  (unless *original-process-toplevel-form*
    ;; Unlock SBCL packages to allow modification of internal functions
    (sb-ext:unlock-package :sb-c)
    (sb-ext:unlock-package :sb-int)
    (sb-ext:unlock-package :sb-kernel)
    ;; Save original functions
    (setf *original-process-toplevel-form* (symbol-function 'sb-c::process-toplevel-form))
    (setf *original-sub-find-source-paths* (symbol-function 'sb-c::sub-find-source-paths))
    (setf *original-do-forms-from-info* (symbol-function 'sb-c::%do-forms-from-info))
    ;; Replace with enhanced versions
    (setf (symbol-function 'sb-c::process-toplevel-form) #'enhanced-process-toplevel-form)
    (setf (symbol-function 'sb-c::sub-find-source-paths) #'enhanced-sub-find-source-paths)
    (setf (symbol-function 'sb-c::%do-forms-from-info) #'enhanced-%do-forms-from-info)
    ;; Enable real-time source tracking
    (setf *real-time-source-tracking* t)
    (log:debug "SBCL compiler hooks installed")))

(defun uninstall-compiler-hooks ()
  "Uninstall compiler hooks and restore original functions."
  (when *original-process-toplevel-form*
    ;; Unlock packages again in case they were re-locked
    (sb-ext:unlock-package :sb-c)
    (sb-ext:unlock-package :sb-int)
    (sb-ext:unlock-package :sb-kernel)
    ;; Restore original functions
    (setf (symbol-function 'sb-c::process-toplevel-form) *original-process-toplevel-form*)
    (setf (symbol-function 'sb-c::sub-find-source-paths) *original-sub-find-source-paths*)
    (when *original-do-forms-from-info*
      (setf (symbol-function 'sb-c::%do-forms-from-info) *original-do-forms-from-info*))
    (setf *original-process-toplevel-form* nil)
    (setf *original-sub-find-source-paths* nil)
    (setf *original-do-forms-from-info* nil)
    ;; Re-lock packages for safety (optional)
    (handler-case (progn
      (sb-ext:lock-package :sb-c)
      (sb-ext:lock-package :sb-int)
      (sb-ext:lock-package :sb-kernel))
      (error
       ()
       nil)) ; Ignore errors if already locked
    (log:debug "SBCL compiler hooks uninstalled")))

;;; High-level interface
(defmacro with-source-tracking ((&key (enable t) file) &body body)
  "Execute body with source location tracking enabled."
  `(let ((*real-time-source-tracking* ,enable)
         (*current-compilation-location* nil)
         (*current-file-info* ,(when file
                                 `(build-file-info-from-cst ,file))))
     (unwind-protect (progn (when ,enable (install-compiler-hooks)) ,@body)
       (when ,enable (uninstall-compiler-hooks)))))

(defun get-real-time-source-location ()
  "Get the current real-time source location during compilation."
  (or
    *current-compilation-location*
    ;; Fallback to SBCL state extraction
    (when (and (boundp 'sb-c::*current-path*) sb-c::*current-path*)
      (extract-location-from-sbcl-state (car sb-c::*current-path*) sb-c::*current-path*))))

;;; SBCL source path utilities
(defun sbcl-source-path-to-location (source-path)
  "Convert SBCL source path to our location structure."
  (when source-path
    (let ((file nil))
      ;; Get file from compilation state
      (when (and (boundp 'sb-c::*compile-file-pathname*) sb-c::*compile-file-pathname*)
        (setf file (namestring sb-c::*compile-file-pathname*)))
      (loc:make-source-location :file file))))

(defun extract-line-from-sbcl-source-path (path)
  "Extract line number from SBCL source path using ORIGINAL-SOURCE-START."
  (when (and path (listp path))
    ;; Look for ORIGINAL-SOURCE-START in the path
    (let ((source-start-node (find-if (lambda (node)
                                        (and (listp node)
                                             (eq (first node) 'sb-c::original-source-start)))
                                      (if (listp (first path))
                                        path
                                        (list path)))))
      (when source-start-node
        (let ((form-index (third source-start-node))) ; (ORIGINAL-SOURCE-START 0 form-index)
          (when (and form-index
                     (numberp form-index)
                     (boundp 'sb-c::*source-info*)
                     sb-c::*source-info*)
            ;; Get FILE-INFO from current compilation
            (let ((file-info (when (fboundp 'sb-c::source-info-file-info)
                               (sb-c::source-info-file-info sb-c::*source-info*))))
              (when file-info
                ;; Get form positions array
                (let ((positions (when (fboundp 'sb-c::file-info-positions)
                                   (sb-c::file-info-positions file-info))))
                  (when (and positions (< form-index (length positions)))
                    ;; Get character position of this form
                    (let ((char-pos (aref positions form-index)))
                      ;; Convert character position to line number
                      (values (file-position-to-line-number file-info char-pos) nil))))))))))))

(defun file-position-to-line-number (file-info char-position)
  "Convert file character position to line number.
   Uses *current-file-info*'s CST-derived line offsets when they are
   available (O(log n) binary search); otherwise falls back to a
   char-by-char scan of the file. FILE-INFO is the SBCL file-info
   passed by callers but only its truename is used."
  (declare (ignore file-info))
  (when (and *current-file-info* char-position
             (compilation-file-info-line-positions *current-file-info*))
    (return-from file-position-to-line-number
      (car (loc:offset-to-line-column
            (compilation-file-info-line-positions *current-file-info*)
            char-position))))
  ;; Fallback: scan the file. Reached only when *current-file-info* is
  ;; unset (e.g. compile-file invoked outside cst-compile-file-around).
  (when (and (boundp 'sb-c::*source-info*) sb-c::*source-info* char-position)
    (let* ((sbcl-fi (when (fboundp 'sb-c::source-info-file-info)
                      (sb-c::source-info-file-info sb-c::*source-info*)))
           (pathname (when (and sbcl-fi (fboundp 'sb-c::file-info-truename))
                       (sb-c::file-info-truename sbcl-fi))))
      (when (and pathname (probe-file pathname))
        (with-open-file (stream pathname :direction :input)
          (let ((line 1)
                (pos 0))
            (loop while (< pos char-position)
                  do (let ((char (read-char stream nil nil)))
                       (unless char (return))
                       (incf pos)
                       (when (char= char #\Newline) (incf line))))
            line))))))

(defun extract-line-from-form-number (file form-number)
  "Extract line number from form number using cached file info."
  (when (and *current-file-info* file form-number)
    ;; This is a simplified approximation
    ;; In practice, we'd need to correlate form numbers with actual positions
    (+ form-number 1)))

;;; Install the compile-file enhancement hook.
;;; After this module loads, all compile-file-safely calls automatically
;;; get CST-based source tracking with byte-accurate sub-expression positions.

(defun cst-compile-file-around (input-file compile-thunk)
  "Wrap a compile-file call with CST source tracking.
   Reads the source file with the Epsilon reader, builds a position map
   for every sub-expression, installs the compiler hooks, then delegates
   to COMPILE-THUNK for the actual compilation."
  (let ((file-info (handler-case (build-file-info-from-cst input-file)
                     (error () nil))))
    (if file-info
        (let ((*current-file-info* file-info))
          (unwind-protect
              (progn (install-compiler-hooks)
                     (funcall compile-thunk))
            (uninstall-compiler-hooks)))
        (funcall compile-thunk))))

(setf epsilon.compile:*compile-file-around* #'cst-compile-file-around)
