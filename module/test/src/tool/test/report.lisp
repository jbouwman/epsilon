(defpackage epsilon.test.report
  (:use
   cl
   epsilon.tool.common)
  (:local-nicknames
   (suite epsilon.test.suite)
   (map epsilon.lib.map)
   (str epsilon.lib.string)
   (xml epsilon.lib.xml))
  (:export
   make))

(in-package epsilon.test.report)

;;;
;;; 'shell' formatter
;;;

(defclass shell-report ()
  ((failure-count :initform 0)
   (max-failures :initform 10)))

(defmethod event ((formatter shell-report) (event-type (eql :start)) event-data)
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
;;; 'junit' XML formatter
;;;

(defclass junit-report ()
  ((output-file :initform "target/TEST-epsilon.xml"
                :reader output-file)))

(defmethod event ((formatter junit-report) (event-type (eql :end)) run)
  (emit-junit-xml formatter run))

(defun emit-junit-xml (report run)
  "Emit JUnit XML format test results to file"
  (with-open-file (stream (output-file report)
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    ;; This XML declaration should be handled by the xml package.
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (xml:emit (make-junit-testsuites run) stream)))

(defun make-junit-testsuites (run)
  "Create JUnit XML testsuites element"
  (let* ((all-tests (map:vals (suite::tests run)))
         (total-tests (length all-tests))
         (total-failures (length (suite:failures run)))
         (total-errors (length (suite:errors run)))
         (total-skipped (length (suite:skipped run)))
         (total-time (/ (- (suite::end-time run) (suite::start-time run)) 
                        internal-time-units-per-second)))
    (xml:element "testsuites"
                 :attributes (list "tests" total-tests
                                   "failures" total-failures
                                   "errors" total-errors
                                   "skipped" total-skipped
                                   "time" (format nil "~,3F" total-time))
                 :children (sort-into-suites run))))

(defun sort-into-suites (run)
  "Create JUnit testsuite elements for each package, sorted by package name"
  (mapcar (lambda (suite-name)
            (make-junit-testsuite suite-name (suite:suite-tests run suite-name)))
          (suite:list-suites run)))

(defun make-junit-testsuite (suite-name tests)
  "Create JUnit XML testsuite element for a package"
  (let* ((test-count (length tests))
         (failures (count-if (lambda (r) (eq :failure (suite:status r))) tests))
         (errors (count-if (lambda (r) (eq :error (suite:status r))) tests))
         (skipped (count-if (lambda (r) (eq :skip (suite:status r))) tests))
         (suite-time (reduce #'+ tests :key #'suite::elapsed-time :initial-value 0))
         (testcases (mapcar #'make-junit-testcase (reverse tests))))
    (xml:element "testsuite"
                 :attributes (list "name" suite-name
                                   "tests" test-count
                                   "failures" failures
                                   "errors" errors
                                   "skipped" skipped
                                   "time" (format nil "~,3F" suite-time))
                 :children testcases)))

(defun make-junit-testcase (result)
  "Create JUnit XML testcase element for a test result"
  (let* ((test-symbol (suite:test result))
         (test-name (symbol-name test-symbol))
         (class-name (package-name (symbol-package test-symbol)))
         (test-time (suite::elapsed-time result))
         (children '()))
    
    (case (suite:status result)
      (:failure
       (push (xml:element "failure"
                          :attributes (list "message" (suite::failure-message (suite:condition result)))
                          :children (list (xml:text (suite::failure-message (suite:condition result)))))
             children))
      (:error
       (push (xml:element "error"
                          :attributes (list "message" (format nil "~A" (suite::original-error (suite:condition result))))
                          :children (list (xml:text (format nil "~A~%~%~A" 
                                                            (suite::original-error (suite:condition result))
                                                            (or (suite::stack-trace result) "")))))
             children))
      (:skip
       (push (xml:element "skipped"
                          :attributes (list "message" (suite::skip-message (suite:condition result))))
             children)))
    
    (when (and (suite::stdout-output result) (not (string= (suite::stdout-output result) "")))
      (push (xml:element "system-out" 
                         :children (list (xml:text (suite::stdout-output result))))
            children))
    
    (when (and (suite::stderr-output result) (not (string= (suite::stderr-output result) "")))
      (push (xml:element "system-err"
                         :children (list (xml:text (suite::stderr-output result))))
            children))
    
    (xml:element "testcase"
                 :attributes (list "name" test-name
                                   "classname" class-name
                                   "time" (format nil "~,3F" test-time))
                 :children (reverse children))))

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
           (dolist (line (str:split #\Newline (suite::stack-trace result)))
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
          (string-downcase (package-name (symbol-package test-symbol)))
          (string-downcase (symbol-name test-symbol))))

(defclass null-report ()
  ())

(defparameter *report-formats*
  (map:make-map :none 'null-report
                :shell 'shell-report
                :junit 'junit-report
                :tap 'tap-report))

(defun to-keyword (value)
  (etypecase value
    (symbol (intern (string value) :keyword))
    (string (intern (string-upcase value) :keyword))))

(defun make (&key format &allow-other-keys)
  (make-instance (or (map:get *report-formats* (to-keyword format))
                     (error "unknown test report type ~s: want one of ~A" format
                            (map:keys *report-formats*)))))