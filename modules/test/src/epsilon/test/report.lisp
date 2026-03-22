(defpackage epsilon.test.report
  (:use :cl)
  (:require (epsilon.test.suite suite)
            (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.string str)
            (epsilon.stacktrace strace))
  (:enter t))

;;; Import helper from suite package
(defun symbol-package-name (symbol)
  "Get the package name of a symbol."
  (suite::symbol-package-name symbol))

;;;
;;; 'shell' formatter
;;;

(defclass shell-report ()
  ((failure-count :initform 0)
   (max-failures :initform 10)))

(defmethod suite:event ((formatter shell-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (format t "~&Running tests:~%~%"))

(defmethod suite:event ((formatter shell-report) (event-type (eql :start-group)) group)
  (format-package-header (first group)))

(defun format-package-header (package-name)
  "Format a package header with standard indentation"
  (format t "~&;; ~A~%" (string-downcase package-name)))

(defun format-status-field (result)
  "Format the timing and status field, returning its string representation"
  (when result
    (format nil "~,2fs~@[ ~A~]"
            (suite::elapsed-time result)
            (case (suite:status result)
              (:failure "FAILURE")
              (:error "ERROR")
              (:skip "SKIP")
              (otherwise nil)))))

(defun format-test-entry (test-name &optional (total-width 78) result)
  "Format a test entry with properly aligned timing and status.
TOTAL-WIDTH specifies the desired total line width (default 78 characters)."
  (let* ((status-field (format-status-field result))
         (status-width (if status-field (length status-field) 0))
         (assertion-count (length (suite:assertions result)))
         (name-field (format nil "~a (~d)" test-name assertion-count))
         (name-width (length (string name-field)))
         (dots-width (+ 10 (- total-width
                              name-width
                              status-width))))
    (format t ";;     ~A ~V,,,'.A ~A~%"
            name-field
            dots-width
            "."
            (or status-field ""))))

(defun format-condition-details (result)
  "Print a message summarizing a test failure, error, or skip"
  (when (suite:condition result)
    (case (suite:status result)
      (:failure
       (format nil "~&~%~%~A~%~%"
               (suite::failure-message (suite:condition result))))
      (:error
       (let ((trace (suite::stack-trace result)))
         (if (strace:stack-trace-p trace)
             ;; New structured stack trace
             (format nil "~&~%~%~A~%"
                     (strace:format-stack-trace trace nil
                       :show-source t
                       :show-locals nil
                       :max-frames 10
                       :format :default))
             ;; Legacy string stack trace (backwards compatibility)
             (format nil "~&~%~%~A~%~%Stack:~%~%~A~%~%"
                     (suite::original-error (suite:condition result))
                     trace))))
      (:skip
       (format nil "~&;;       Skipped: ~A~%"
               (suite::skip-message (suite:condition result)))))))

(defmethod suite:event ((formatter shell-report) (event-type (eql :end-test)) result)
  (with-slots (failure-count max-failures) formatter
    (let ((test (suite:test result)))
      (format-test-entry (string-downcase (symbol-name test)) 60 result)
      (when (member (suite:status result) '(:failure :error))
        (when (< failure-count max-failures)
          (format t "~A" (format-condition-details result))
          (incf failure-count))))))

(defmethod suite:event ((formatter shell-report) (event-type (eql :end)) run)
  (let ((total-failures (+ (length (suite:failures run)) (length (suite:errors run)))))
    (with-slots (failure-count max-failures) formatter
      (format t "~&~%Test Run Complete:~%")
      (format t ";;   Tests: ~D~%" (map:size (suite::tests run)))
      (format t ";;   Failures: ~D~%" (length (suite:failures run)))
      (format t ";;   Errors: ~D~%" (length (suite:errors run)))
      (format t ";;   Skipped: ~D~%" (length (suite:skipped run)))
      (when (> total-failures max-failures)
        (format t ";;   (Only ~D of ~D failures/errors shown)~%"
                (min failure-count max-failures) total-failures))
      (format t ";;   Time: ~,2F seconds~%"
              (/ (- (suite::end-time run) (suite::start-time run))
                 internal-time-units-per-second)))))

;;;
;;; 'silent' formatter (for aggregation)
;;;

(defclass silent-report () ())

(defmethod suite:event ((formatter silent-report) event-type event-data)
  ;; Silent reporter does nothing - just collects results
  (declare (ignore formatter event-type event-data))
  nil)

;;;
;;; 'minimal' formatter
;;;

(defclass minimal-report ()
  ((failure-count :initform 0 :accessor failure-count)
   (error-count :initform 0 :accessor error-count)
   (test-count :initform 0 :accessor test-count)
   (start-time :initform nil :accessor start-time)))

(defmethod suite:event ((formatter minimal-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (setf (start-time formatter) (get-internal-real-time))
  (format t "Running tests...~%"))

(defmethod suite:event ((formatter minimal-report) (event-type (eql :end-test)) result)
  (incf (test-count formatter))
  (case (suite:status result)
    (:failure
     (incf (failure-count formatter))
     (format t "X ~A~%" (suite:test result))
     (when (suite:condition result)
       (format t "  ~A~%" (suite::failure-message (suite:condition result)))))
    (:error
     (incf (error-count formatter))
     (format t "X ~A (ERROR)~%" (suite:test result))
     (when (suite:condition result)
       (format t "  ~A~%" (suite::original-error (suite:condition result)))))
    (:skip
     ;; Don't output anything for skipped tests in minimal mode
     nil)
    (otherwise
     ;; Don't output anything for passing tests in minimal mode
     nil)))

(defmethod suite:event ((formatter minimal-report) (event-type (eql :end)) run)
  (let* ((total-tests (test-count formatter))
         (failures (failure-count formatter))
         (errors (error-count formatter))
         (passed (- total-tests failures errors (length (suite:skipped run))))
         (elapsed-time (/ (- (get-internal-real-time) (start-time formatter))
                         internal-time-units-per-second)))
    (format t "~%")
    (if (and (zerop failures) (zerop errors))
        (format t "OK All ~D tests passed (~,2fs)~%" total-tests elapsed-time)
        (format t "Tests: ~D passed, ~D failed~@[, ~D errors~] (~,2fs)~%"
                passed failures (when (> errors 0) errors) elapsed-time))))

(defmethod suite:event ((formatter minimal-report) event-type event-data)
  ;; Ignore other events
  (declare (ignore formatter event-type event-data))
  nil)

;;;
;;; 'dot' formatter - Python unittest style progress dots
;;;
;;; Shows compact progress during test execution:
;;;   . = pass
;;;   F = failure
;;;   E = error
;;;   s = skip
;;;
;;; Example output:
;;;   ....F...s..E....
;;;
;;;   ======================================================================
;;;   FAIL: test-something (my.package)
;;;   ----------------------------------------------------------------------
;;;   AssertionError: expected 42, got 41
;;;
;;;   ----------------------------------------------------------------------
;;;   Ran 17 tests in 0.54s
;;;
;;;   FAILED (failures=1, errors=1, skipped=1)

(defclass dot-report ()
  ((test-count :initform 0 :accessor test-count)
   (failure-count :initform 0 :accessor failure-count)
   (error-count :initform 0 :accessor error-count)
   (skip-count :initform 0 :accessor skip-count)
   (start-time :initform nil :accessor start-time)
   (failures :initform nil :accessor dot-failures)
   (errors :initform nil :accessor dot-errors)
   (column :initform 0 :accessor column)
   (max-columns :initform 70 :initarg :max-columns :accessor max-columns)
   (max-failure-details :initform 1 :initarg :max-failure-details
                        :accessor max-failure-details)))

(defmethod suite:event ((formatter dot-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (setf (start-time formatter) (get-internal-real-time))
  (setf (column formatter) 0))

(defmethod suite:event ((formatter dot-report) (event-type (eql :start-group)) group)
  (declare (ignore formatter group))
  ;; No output for groups in dot format
  nil)

(defmethod suite:event ((formatter dot-report) (event-type (eql :start-test)) test)
  (declare (ignore formatter test))
  ;; No output when test starts
  nil)

(defun dot-newline-if-needed (formatter)
  "Print newline if we've reached max columns"
  (when (>= (column formatter) (max-columns formatter))
    (format t "~%")
    (setf (column formatter) 0)))

(defmethod suite:event ((formatter dot-report) (event-type (eql :end-test)) result)
  (incf (test-count formatter))
  (dot-newline-if-needed formatter)
  (incf (column formatter))
  (case (suite:status result)
    (:failure
     (incf (failure-count formatter))
     (push result (dot-failures formatter))
     (format t "F")
     (force-output))
    (:error
     (incf (error-count formatter))
     (push result (dot-errors formatter))
     (format t "E")
     (force-output))
    (:skip
     (incf (skip-count formatter))
     (format t "s")
     (force-output))
    (otherwise
     (format t ".")
     (force-output))))

(defun format-dot-separator ()
  "Print the standard separator line"
  (format t "~&~V,,,'-A~%" 70 ""))

(defun format-dot-double-separator ()
  "Print the double separator line"
  (format t "~&~V,,,'=A~%" 70 ""))

(defun format-dot-failure-detail (result kind)
  "Format a single failure or error detail"
  (let* ((test (suite:test result))
         (pkg-name (symbol-package-name test)))
    (format-dot-double-separator)
    (format t "~A: ~A (~A)~%"
            kind
            (string-downcase (symbol-name test))
            (string-downcase pkg-name))
    (format-dot-separator)
    (case (suite:status result)
      (:failure
       (when (suite:condition result)
         (format t "~A~%" (suite::failure-message (suite:condition result)))))
      (:error
       (when (suite:condition result)
         (let ((trace (suite::stack-trace result)))
           (if (strace:stack-trace-p trace)
               ;; Use structured stack trace - compact with limited frames
               (format t "~A~%"
                       (strace:format-stack-trace trace nil
                         :show-source t
                         :show-locals nil
                         :max-frames 5
                         :source-context 1
                         :format :compact))
               ;; Legacy fallback: print error and raw trace
               (progn
                 (format t "~A~%" (suite::original-error (suite:condition result)))
                 (when trace
                   (format t "~%Traceback:~%~A~%" trace))))))))
    (format t "~%")))

(defun format-captured-output (result)
  "Print the last 20 lines of captured stdout/stderr for a test result."
  (flet ((print-tail (label output)
           (when (and output (not (string= output "")))
             (let* ((lines (seq:realize (str:split #\Newline output)))
                    (lines (remove-if (lambda (l) (string= l "")) lines))
                    (total (length lines))
                    (tail (if (> total 20) (last lines 20) lines)))
               (when tail
                 (format t "~%  Captured ~A:~%" label)
                 (when (> total 20)
                   (format t "    ... (~D lines omitted)~%" (- total 20)))
                 (dolist (line tail)
                   (format t "    ~A~%" line)))))))
    (print-tail "stdout" (suite::stdout-output result))
    (print-tail "stderr" (suite::stderr-output result))))

(defmethod suite:event ((formatter dot-report) (event-type (eql :end)) run)
  (declare (ignore run))
  ;; End the progress line
  (format t "~%")

  ;; Print failure/error details up to max-failure-details
  (let ((all-problems (append
                       (mapcar (lambda (f) (cons "FAIL" f)) (reverse (dot-failures formatter)))
                       (mapcar (lambda (e) (cons "ERROR" e)) (reverse (dot-errors formatter)))))
        (shown 0)
        (max-details (max-failure-details formatter)))
    (when all-problems
      (format t "~%")
      (dolist (problem all-problems)
        (when (< shown max-details)
          (format-dot-failure-detail (cdr problem) (car problem))
          (format-captured-output (cdr problem))
          (incf shown)))
      (let ((remaining (- (length all-problems) shown)))
        (when (> remaining 0)
          (format t "~%... and ~D more failure~:P (use --verbose to see all)~%"
                  remaining)))))

  ;; Print summary
  (format-dot-separator)
  (let* ((total-tests (test-count formatter))
         (failures (failure-count formatter))
         (errors (error-count formatter))
         (skipped (skip-count formatter))
         (elapsed-time (/ (- (get-internal-real-time) (start-time formatter))
                         internal-time-units-per-second)))

    (format t "Ran ~D test~:P in ~,2Fs~%"
            total-tests elapsed-time)
    (format t "~%")

    (cond
      ((and (zerop failures) (zerop errors))
       (if (> skipped 0)
           (format t "OK (skipped=~D)~%" skipped)
           (format t "OK~%")))
      (t
       (format t "FAILED (")
       (let ((parts nil))
         (when (> failures 0)
           (push (format nil "failures=~D" failures) parts))
         (when (> errors 0)
           (push (format nil "errors=~D" errors) parts))
         (when (> skipped 0)
           (push (format nil "skipped=~D" skipped) parts))
         (format t "~{~A~^, ~}" (reverse parts)))
       (format t ")~%")))))

(defmethod suite:event ((formatter dot-report) event-type event-data)
  ;; Ignore other events
  (declare (ignore formatter event-type event-data))
  nil)

;;; TAP (Test Anything Protocol) formatter

(defclass tap-report ()
  ((test-count :initform 0 :accessor test-count)))

(defmethod suite:event ((formatter tap-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  ;; TAP version declaration
  (format t "TAP version 13~%"))

(defmethod suite:event ((formatter tap-report) (event-type (eql :start-group)) group)
  (declare (ignore group))
  ;; Groups are not explicitly represented in TAP
  )

(defmethod suite:event ((formatter tap-report) (event-type (eql :end-test)) result)
  (let ((test (suite:test result))
        (status (suite:status result)))
    (incf (test-count formatter))
    (case status
      (:failure
       (format t "not ok ~D ~A~%"
               (test-count formatter)
               (format-tap-test-name test))
       (when (suite:condition result)
         (format t "  ---~%")
         (format t "  message: ~S~%"
                 (suite::failure-message (suite:condition result)))
         (format t "  severity: fail~%")
         (format t "  ...~%")))
      (:error
       (format t "not ok ~D ~A~%"
               (test-count formatter)
               (format-tap-test-name test))
       (when (suite:condition result)
         (format t "  ---~%")
         (let ((trace (suite::stack-trace result)))
           ;; Get error message from stack trace or original error
           (format t "  message: ~S~%"
                   (if (strace:stack-trace-p trace)
                       (or (strace:stack-trace-condition-message trace)
                           (format nil "~A" (suite::original-error (suite:condition result))))
                       (format nil "~A" (suite::original-error (suite:condition result)))))
           (format t "  severity: fail~%")
           (when trace
             (format t "  data:~%")
             (format t "    stack: |~%")
             ;; Format stack trace to string, then split
             (let ((trace-str (if (strace:stack-trace-p trace)
                                  (strace:format-stack-trace trace nil
                                    :show-source nil
                                    :show-locals nil
                                    :max-frames 10
                                    :format :compact)
                                  (format nil "~A" trace))))
               (dolist (line (str:split #\Newline trace-str))
                 (format t "      ~A~%" line)))))
         (format t "  ...~%")))
      (:skip
       (format t "ok ~D ~A # SKIP~@[ ~A~]~%"
               (test-count formatter)
               (format-tap-test-name test)
               (when (suite:condition result)
                 (suite::skip-message (suite:condition result)))))
      (otherwise
       (format t "ok ~D ~A~%"
               (test-count formatter)
               (format-tap-test-name test))))))

(defmethod suite:event ((formatter tap-report) (event-type (eql :end)) run)
  (format t "1..~D~%" (test-count formatter))
  ;; Optional plan can also be at the beginning, but at end is more reliable
  (let ((failures (+ (length (suite:failures run)) (length (suite:errors run)))))
    (when (> failures 0)
      (format t "# Failed ~D test~P out of ~D~%"
              failures failures (test-count formatter)))))

(defun format-tap-test-name (test-symbol)
  "Format test name for TAP output"
  (format nil "~A::~A"
          (string-downcase (symbol-package-name test-symbol))
          (string-downcase (symbol-name test-symbol))))

(defclass null-report ()
  ())

;;;
;;; 'verbose' formatter - detailed output for debugging
;;;

(defclass verbose-report ()
  ((current-package :initform nil :accessor current-package)
   (assertion-count :initform 0 :accessor assertion-count)))

(defmethod suite:event ((formatter verbose-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (format t "~&== EPSILON TEST RUNNER ==~%")
  (format t "   Verbose Mode Active~%~%"))

(defmethod suite:event ((formatter verbose-report) (event-type (eql :start-group)) group)
  (setf (current-package formatter) (first group))
  (format t "~%-- Package: ~A~%" (string-downcase (first group))))

(defmethod suite:event ((formatter verbose-report) (event-type (eql :start-test)) test)
  (setf (assertion-count formatter) 0)
  (format t "   > Running test: ~A~%" (string-downcase (symbol-name test))))

(defmethod suite:event ((formatter verbose-report) (event-type (eql :end-test)) result)
  (let* ((test (suite:test result))
         (status (suite:status result))
         (elapsed (suite::elapsed-time result))
         (assertions (suite:assertions result))
         (status-symbol (case status
                              (:failure "X")
                              (:error "!")
                              (:skip "-")
                              (otherwise ".")))
         (status-text (case status
                            (:failure "FAILED")
                            (:error "ERROR")
                            (:skip "SKIPPED")
                            (otherwise "PASSED"))))
    (declare (ignore test))

    ;; Show test completion with timing
    (format t "   ~A Test ~A in ~,3fs (~D assertion~:P)~%"
            status-symbol status-text elapsed (length assertions))

    ;; Show assertion details if there are any
    (when assertions
      (format t "      Assertions:~%")
      (dolist (assertion (reverse assertions))
        (destructuring-bind (result report-fn) assertion
          (declare (ignore report-fn))
          (cond
           ((and (listp result) (eq (first result) :label-start))
            (format t "        - ~A~%" (second result)))
           ((and (listp result) (eq (first result) :label-end))
            nil) ; Skip end labels
           (result
            (format t "          . Passed~%"))
           (t
            (format t "          X Failed~%"))))))

    ;; Show stdout/stderr if present
    (when (and (suite::stdout-output result)
               (not (string= (suite::stdout-output result) "")))
      (format t "      Standard Output:~%")
      (dolist (line (seq:realize (str:split #\Newline (suite::stdout-output result))))
        (unless (string= line "")
          (format t "        > ~A~%" line))))

    (when (and (suite::stderr-output result)
               (not (string= (suite::stderr-output result) "")))
      (format t "      Error Output:~%")
      (dolist (line (seq:realize (str:split #\Newline (suite::stderr-output result))))
        (unless (string= line "")
          (format t "        ! ~A~%" line))))

    ;; Show detailed error information for failures/errors
    (when (member status '(:failure :error))
      (format t "~%      === FAILURE DETAILS ===~%")
      (case status
        (:failure
         (let ((msg (suite::failure-message (suite:condition result))))
           (format t "      Message: ~A~%" msg)))
        (:error
         (let ((error (suite::original-error (suite:condition result)))
               (trace (suite::stack-trace result)))
           (format t "      Error: ~A~%" error)
           (when trace
             (format t "      Stack Trace:~%")
             ;; Format stack trace properly
             (let ((trace-str (if (strace:stack-trace-p trace)
                                  (strace:format-stack-trace trace nil
                                    :show-source t
                                    :show-locals nil
                                    :max-frames 15
                                    :format :default)
                                  (format nil "~A" trace))))
               (dolist (line (seq:realize (str:split #\Newline trace-str)))
                 (unless (string= line "")
                   (format t "        ~A~%" line))))))))

      (format t "~%"))))

(defmethod suite:event ((formatter verbose-report) (event-type (eql :end)) run)
  (format t "~%== TEST SUMMARY ==~%")
  (let* ((total-tests (map:size (suite::tests run)))
         (failures (length (suite:failures run)))
         (errors (length (suite:errors run)))
         (skipped (length (suite:skipped run)))
         (passed (- total-tests failures errors skipped))
         (total-time (/ (- (suite::end-time run) (suite::start-time run))
                        internal-time-units-per-second)))

    (format t "Total Tests: ~D  Passed: ~D  Failed: ~D  Errors: ~D  Skipped: ~D~%"
            total-tests passed failures errors skipped)
    (format t "Total Time: ~,3F seconds~%" total-time)

    (if (zerop (+ failures errors))
        (format t "~%ALL TESTS PASSED~%")
      (format t "~%TESTS FAILED~%"))))

(defparameter *report-formats*
  {:none null-report
   :shell shell-report
   :verbose verbose-report
   :minimal minimal-report
   :dot dot-report
   :tap tap-report})

(defun to-keyword (value)
  (etypecase value
    (symbol (intern (string value) :keyword))
    (string (intern (string-upcase value) :keyword))))

(defun make (&key format file &allow-other-keys)
  (declare (ignore file))
  (let ((report-class (or (map:get *report-formats* (to-keyword format))
                          (error "unknown test report type ~s: want one of ~A" format
                                 (map:keys *report-formats*)))))
    (make-instance report-class)))
