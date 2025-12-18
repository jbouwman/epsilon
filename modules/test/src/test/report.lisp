(defpackage epsilon.test.report
  (:use
   cl
   epsilon.tool.common)
  (:local-nicknames
   (suite epsilon.test.suite)
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string))
  (:export
   make))

(in-package epsilon.test.report)

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

(defmethod event ((formatter shell-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (format t "~&Running tests:~%~%"))

(defmethod event ((formatter shell-report) (event-type (eql :start-group)) group)
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
	   (format nil "~&~%~%~A~%~%Stack:~%~%~A~%~%"
		   (suite::original-error (suite:condition result))
		   (suite::stack-trace result)))
	  (:skip
	   (format nil "~&;;       Skipped: ~A~%"
		   (suite::skip-message (suite:condition result)))))))

(defmethod event ((formatter shell-report) (event-type (eql :end-test)) result)
  (with-slots (failure-count max-failures) formatter
    (let ((test (suite:test result)))
      (format-test-entry (string-downcase (symbol-name test)) 60 result)
      (when (member (suite:status result) '(:failure :error))
        (when (< failure-count max-failures)
          (format t "~A" (format-condition-details result))
          (incf failure-count))))))

(defmethod event ((formatter shell-report) (event-type (eql :end)) run)
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

(defmethod event ((formatter silent-report) event-type event-data)
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

(defmethod event ((formatter minimal-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (setf (start-time formatter) (get-internal-real-time))
  (format t "Running tests...~%"))

(defmethod event ((formatter minimal-report) (event-type (eql :end-test)) result)
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

(defmethod event ((formatter minimal-report) (event-type (eql :end)) run)
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

(defmethod event ((formatter minimal-report) event-type event-data)
  ;; Ignore other events
  (declare (ignore formatter event-type event-data))
  nil)

;;; TAP (Test Anything Protocol) formatter

(defclass tap-report ()
  ((test-count :initform 0 :accessor test-count)))

(defmethod event ((formatter tap-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  ;; TAP version declaration
  (format t "TAP version 13~%"))

(defmethod event ((formatter tap-report) (event-type (eql :start-group)) group)
  (declare (ignore group))
  ;; Groups are not explicitly represented in TAP
  )

(defmethod event ((formatter tap-report) (event-type (eql :end-test)) result)
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
             (format t "  message: ~S~%"
                     (format nil "~A" (suite::original-error (suite:condition result))))
             (format t "  severity: fail~%")
             (when (suite::stack-trace result)
               (format t "  data:~%")
               (format t "    stack: |~%")
               (dolist (line (str:split (suite::stack-trace result) #\Newline))
		 (format t "      ~A~%" line)))
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

(defmethod event ((formatter tap-report) (event-type (eql :end)) run)
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

(defmethod event ((formatter verbose-report) (event-type (eql :start)) event-data)
  (declare (ignore event-data))
  (format t "~&== EPSILON TEST RUNNER ==~%")
  (format t "   Verbose Mode Active~%~%"))

(defmethod event ((formatter verbose-report) (event-type (eql :start-group)) group)
  (setf (current-package formatter) (first group))
  (format t "~%-- Package: ~A~%" (string-downcase (first group))))

(defmethod event ((formatter verbose-report) (event-type (eql :start-test)) test)
  (setf (assertion-count formatter) 0)
  (format t "   > Running test: ~A~%" (string-downcase (symbol-name test))))

(defmethod event ((formatter verbose-report) (event-type (eql :end-test)) result)
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
             (let ((error (suite::original-error (suite:condition result))))
               (format t "      Error: ~A~%" error)
               (when (suite::stack-trace result)
		 (format t "      Stack Trace:~%")
		 (dolist (line (seq:realize (str:split #\Newline (suite::stack-trace result))))
		   (unless (string= line "")
                     (format t "        ~A~%" line)))))))

      (format t "~%"))))

(defmethod event ((formatter verbose-report) (event-type (eql :end)) run)
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
  (map:make-map :none 'null-report
                :shell 'shell-report
                :verbose 'verbose-report
                :minimal 'minimal-report
                :tap 'tap-report))

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
