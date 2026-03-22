;;;; Snapshot Testing for epsilon.test
;;;;
;;;; Provides "golden master" testing where expected output is stored
;;;; in snapshot files and compared against actual output.
;;;;
;;;; Inspired by Jest (JavaScript) and Insta (Rust).

(defpackage epsilon.test.snapshot
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.path path)
            (epsilon.file fs)
            (epsilon.string str)
            (epsilon.sequence seq)
            (epsilon.log log))
  (:enter t))

;;; Configuration

(defvar *update-snapshots* nil
  "When T, update snapshot files instead of comparing.
   Set via --update-snapshots flag.")

(defvar *snapshot-dir* "tests/snapshots"
  "Directory relative to module root where snapshots are stored.")

(defvar *current-module* nil
  "The current module being tested, used to locate snapshot directory.")

(defvar *snapshot-mismatches* nil
  "List of snapshot mismatches found during test run.")

;;; Snapshot File Operations

(defun snapshot-path (module-name snapshot-name)
  "Return the full path to a snapshot file.
   SNAPSHOT-NAME can include subdirectories like \"compiler/parse-defun\"."
  (let* ((module (epsilon.loader:get-module (epsilon.loader:environment) module-name :error-p t))
         (module-dir (path::path-from-uri (epsilon.loader:module-uri module)))
         (snapshot-file (format nil "~A/~A.snap" *snapshot-dir* snapshot-name)))
    (path:string-path-join module-dir snapshot-file)))

(defun ensure-snapshot-directory (snapshot-path)
  "Ensure the directory for SNAPSHOT-PATH exists."
  (let ((dir (directory-namestring snapshot-path)))
    (ensure-directories-exist dir)))

(defun read-snapshot (path)
  "Read and parse a snapshot file. Returns the snapshot value or NIL if not found."
  (when (probe-file path)
    (with-open-file (stream path :direction :input)
      ;; Skip header comments
      (loop for line = (read-line stream nil nil)
            while (and line (str:starts-with-p line ";;"))
            finally (when line
                      ;; Put back the non-comment line by reading from start
                      (file-position stream 0)))
      ;; Skip header lines again and read the actual content
      (loop for line = (read-line stream nil nil)
            while (and line (str:starts-with-p line ";;"))
            finally (when line
                      ;; Unread doesn't work well, so re-read the file
                      nil))
      ;; Re-open and skip headers properly
      (file-position stream 0)
      (loop for line = (read-line stream nil nil)
            while (and line (str:starts-with-p line ";;"))
            finally (when line
                      ;; Position is now at the data
                      (file-position stream (- (file-position stream) (1+ (length line))))))
      ;; Read the actual Lisp form
      (handler-case
          (read stream nil nil)
        (error (e)
          (log:warn "Failed to parse snapshot ~A: ~A" path e)
          nil)))))

(defun read-snapshot-raw (path)
  "Read a snapshot file and return the raw content after headers."
  (when (probe-file path)
    (with-open-file (stream path :direction :input)
      (let ((lines nil)
            (in-header t))
        (loop for line = (read-line stream nil nil)
              while line
              do (if (and in-header (str:starts-with-p line ";;"))
                     nil  ; Skip header
                     (progn
                       (setf in-header nil)
                       (push line lines))))
        (str:join (string #\Newline) (reverse lines))))))

(defun write-snapshot (path value &key (format :lisp) test-name)
  "Write a snapshot file with proper headers.
   FORMAT can be :lisp, :json, or :text."
  (ensure-snapshot-directory path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    ;; Write header
    (format stream "~&;; Snapshot file - do not edit manually~%")
    (format stream ";; Generated: ~A~%" (format-timestamp))
    (when test-name
      (format stream ";; Test: ~A~%" test-name))
    (format stream ";; Format: ~A~%" format)
    (format stream "~%")
    ;; Write content based on format
    (case format
      (:lisp
       (write value :stream stream :pretty t :readably t)
       (terpri stream))
      (:text
       (princ value stream)
       (terpri stream))
      (:json
       ;; For JSON, store as string
       (write value :stream stream :pretty t)
       (terpri stream))
      (otherwise
       (write value :stream stream :pretty t :readably t)
       (terpri stream)))))

(defun format-timestamp ()
  "Return current timestamp in ISO 8601 format."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))

;;; Value Comparison and Diff

(defun values-equal-p (expected actual &key (format :lisp))
  "Compare two values for equality based on format."
  (case format
    (:text (string= (princ-to-string expected) (princ-to-string actual)))
    (otherwise (equal expected actual))))

(defun compute-snapshot-diff (expected actual &key (format :lisp))
  "Compute a human-readable diff between expected and actual values."
  (case format
    (:text
     (let ((exp-str (princ-to-string expected))
           (act-str (princ-to-string actual)))
       (if (string= exp-str act-str)
           nil
           (list (format nil "Text differs:~%Expected: ~A~%Actual:   ~A"
                         (truncate-string exp-str 100)
                         (truncate-string act-str 100))))))
    (otherwise
     ;; For Lisp values, use structural diff
     (compute-lisp-diff expected actual))))

(defun compute-lisp-diff (expected actual &optional (path "root"))
  "Compute structural diff between two Lisp values."
  (cond
    ((equal expected actual) nil)
    ((and (listp expected) (listp actual))
     (let ((diffs nil))
       ;; Check length difference
       (when (/= (length expected) (length actual))
         (push (format nil "~A: length differs (expected ~D, got ~D)"
                       path (length expected) (length actual))
               diffs))
       ;; Compare elements
       (loop for i from 0
             for exp in expected
             for act in actual
             for elem-diffs = (compute-lisp-diff exp act (format nil "~A[~D]" path i))
             when elem-diffs
             do (setf diffs (append diffs elem-diffs)))
       diffs))
    ((and (stringp expected) (stringp actual))
     (if (string= expected actual)
         nil
         (list (format nil "~A: strings differ~%  expected: ~S~%  actual:   ~S"
                       path expected actual))))
    (t
     (list (format nil "~A: expected ~S, got ~S" path expected actual)))))

(defun truncate-string (str max-len)
  "Truncate string to MAX-LEN characters, adding ... if truncated."
  (if (<= (length str) max-len)
      str
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")))

;;; Redaction Support

(defvar *redaction-patterns* nil
  "List of (pattern replacement) pairs for redacting non-deterministic values.")

(defun add-redaction (pattern replacement)
  "Add a redaction pattern. PATTERN is a regex, REPLACEMENT is the substitute text."
  (push (list pattern replacement) *redaction-patterns*))

(defun clear-redactions ()
  "Clear all redaction patterns."
  (setf *redaction-patterns* nil))

(defun redact-value (value)
  "Apply redaction patterns to VALUE.
   Currently only works on string values."
  (if (stringp value)
      (reduce (lambda (str pattern-replacement)
                (let ((pattern (first pattern-replacement))
                      (replacement (second pattern-replacement)))
                  ;; Simple string replacement - regex would need epsilon.regex
                  (str:replace-all str pattern replacement)))
              *redaction-patterns*
              :initial-value value)
      value))

;;; Main API

(defun snapshot-matches-impl (module-name snapshot-name actual &key (format :lisp) redact test-name)
  "Check if ACTUAL matches the snapshot stored at SNAPSHOT-NAME.
   If snapshot doesn't exist, creates it (with warning).
   If *update-snapshots* is T, updates the snapshot.
   Returns T if match, signals test failure if mismatch."
  (let* ((path (snapshot-path module-name snapshot-name))
         (processed-actual (if redact (redact-value actual) actual))
         (exists (probe-file path)))
    (cond
      ;; Update mode - always write
      (*update-snapshots*
       (write-snapshot path processed-actual :format format :test-name test-name)
       (log:info "Updated snapshot: ~A" snapshot-name)
       t)

      ;; Snapshot doesn't exist - create it
      ((not exists)
       (write-snapshot path processed-actual :format format :test-name test-name)
       (log:warn "Created new snapshot: ~A" snapshot-name)
       ;; Return T but warn - new snapshots should be reviewed
       t)

      ;; Compare with existing snapshot
      (t
       (let ((expected (if (eq format :text)
                           (read-snapshot-raw path)
                           (read-snapshot path))))
         (if (values-equal-p expected processed-actual :format format)
             t
             (let ((diff (compute-snapshot-diff expected processed-actual :format format)))
               (push (list snapshot-name path diff) *snapshot-mismatches*)
               ;; Signal failure through test framework
               (error "Snapshot mismatch for ~A:~%~{  ~A~%~}"
                      snapshot-name diff))))))))

(defun get-snapshot-mismatches ()
  "Return list of snapshot mismatches from current test run."
  *snapshot-mismatches*)

(defun clear-snapshot-mismatches ()
  "Clear the list of snapshot mismatches."
  (setf *snapshot-mismatches* nil))

;;; Convenience Macros (exported through epsilon.test)

;; Note: The main macro snapshot-matches is defined in epsilon.test
;; to have access to the current test context.
