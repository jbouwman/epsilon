(defpackage #:tool.test
  (:use
   #:cl
   #:sb-gray
   #:lib.list
   #:lib.symbol)
  (:export
   #:define-test-package
   #:deftest
   #:describe-failed-tests
   #:is
   #:run-all-tests
   #:run-package-tests
   #:run-suite-tests
   #:run-tests
   #:signals
   #:skip
   #:test-file))

(defpackage #:tool.test.suites
  (:documentation
   "Namespace for test suites defined via DEFINE-TEST-PACKAGE."))

(in-package #:tool.test)

(defun test-file (name)
  (merge-pathnames (format nil "tests/data/~A" name)
                   (asdf:system-source-directory "epsilon")))

(defvar *suite*)

(defvar *root-suite*)

(defvar *package-bound-suites* (make-hash-table))

(defvar *print-test-run-progress* t)

(defvar *test-progress-print-right-margin* 80)

(defvar *debug-on-unexpected-error* t)

(defvar *debug-on-assertion-failure* t)

(defvar *test-result-history* '())

(defvar *last-test-result* nil)

(defvar *failures-and-errors-are-expected* nil)

(defvar *always-show-failed-sexp* nil)

(defvar *warn-about-test-redefinitions* nil)

(defvar *test-run-standard-output* '*standard-output*
  "*STANDARD-OUTPUT* is bound to (eval *test-run-standard-output*) at
the toplevel entry point to any test.")

(defvar *tests* (make-hash-table :test 'eql))

(defvar *ignore-package-suite-mismatch* nil)

(defvar *test-run* nil
  "Status and progress info for a particular test run.")

(defvar *current-test* nil
  "Current singleton instance of TEST executing its associated DEFTEST lambda.")

(defvar *progress-char-count* 0)

(defvar *run-test-function* 'run-test-body-in-handlers)

(defvar *pretty-log-stream* nil)

(defvar *pretty-log-verbose-p* nil)

(defvar *within-non-suite-test* nil
  "True within the scope of a non-suite test. Used to suppress printing test
  status for recursive test calls.")

;; Stream

(defclass column-counting-output-stream (fundamental-character-output-stream)
  ((column :initform 0 :accessor output-column)
   (understream :initarg :understream :initform (error "required!"))))

(defmethod stream-write-sequence ((s column-counting-output-stream) seq &optional start end)
  "Write SEQ to stream S."
  (let ((newline-pos (position #\Newline seq :from-end t)))
    (when newline-pos
      (setf (output-column s) (- (length seq) newline-pos 1))))
  (write-sequence seq (slot-value s 'understream) :start start :end end))

(defmethod stream-line-column ((s column-counting-output-stream))
  "Tell column number that stream S is currently at."
  (output-column s))

(defmethod stream-start-line-p ((s column-counting-output-stream))
  "Tell if stream S is already at start of fresh new line."
  (zerop (output-column s)))

(defmethod stream-write-char ((s column-counting-output-stream) char)
  "Write CHAR to stream S."
  (if (char= char #\Newline)
      (setf (output-column s) 0)
      (incf (output-column s)))
  (write-char char (slot-value s 'understream)))

(defmacro without-debugging (&body body)
  `(let* ((*debug-on-unexpected-error* nil)
          (*debug-on-assertion-failure* nil))
     ,@body))

;;; Testable class

(defclass testable ()
  ((name :accessor name-of
         :initarg :name
         :type symbol)
   (parent :initform nil
           :accessor parent-of
           :type (or null testable))
   (children :initform (make-hash-table)
             :accessor children-of
             :initarg :children
             :documentation "A mapping from testable names to testables")
   (auto-call :initform t
              :accessor auto-call?
              :initarg :auto-call
              :type boolean
              :documentation "Controls whether to automatically call
this test when its parent suite is invoked. Enabled by default.")))

(defmethod print-object ((self testable) s)
  (print-unreadable-object (self s :type nil :identity nil)
    (format s "test ~s" (name-of self))
    (let* ((children (count-tests self)))
      (unless (zerop children) (format s " :tests ~s" children))))
  self)

(defmethod shared-initialize :after ((self testable) slot-names
                                     &key (in (or (parent-of self)
                                                  (find-suite-for-package *package*)
                                                  (and (boundp '*suite*)
                                                       *suite*))
                                              in-supplied-p))
  (declare (ignore slot-names))
  (assert (name-of self))
  (setf (find-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (let ((*ignore-package-suite-mismatch* in-supplied-p))
    (setf (parent-of self) in)))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (when (and new-parent
             (symbol-package (name-of self)) ; leave alone tests named
                                        ; by uninterned symbols
             (not (eq new-parent *root-suite*))
             (not (eq (symbol-package (name-of new-parent))
                      (symbol-package (name-of self))))
             (not *ignore-package-suite-mismatch*)
             (not (gethash (package-of self) *package-bound-suites*)))
    (warn 'test-style-warning
          :test self
          :format-control "Adding test under parent ~S which is in a different package (parent: ~A, child: ~A). Maybe a missing (in-root-suite)?"
          :format-arguments (list new-parent (symbol-package
                                              (name-of new-parent))
                                  (symbol-package (name-of self)))))
  (let* ((old-parent (parent-of self)))
    (when old-parent
      (remhash (name-of self) (children-of old-parent)))
    (prog1
        (call-next-method)
      (when new-parent
        (setf (gethash (name-of self) (children-of new-parent)) self)))))

(defgeneric count-tests (testable)
  (:method ((self testable))
    (+ (hash-table-count (children-of self))
       (loop
         :for child :being :the :hash-values :of (children-of self)
         :summing (count-tests child)))))

;;; The object that represents a particular test run.

(defmacro check-required (sym) `(error "Must provide ~a" ,sym))

(defclass test-run ()
  ((test :accessor test-of :initarg :test)
   (internal-realtime-spent-with-test
    :initform nil
    :accessor internal-realtime-spent-with-test-of
    :initarg :internal-realtime-spent-with-test)
   (actual-test-arguments :accessor actual-test-arguments-of
                          :initarg :actual-test-arguments
                          :initform (check-required 'actual-test-arguments))
   ;; recording
   ;;
   (self-failures :initform nil
                  :reader self-failures)
   (self-assertions :initform nil
                    :reader self-assertions)
   (self-skipped :initform nil :accessor skipped-p)
   ;; tree structure
   ;;
   (parent-test-run
    :initarg :parent-test-run :initform nil :accessor parent-test-run-of)
   (children-test-runs
    :initform nil :accessor children-test-runs-of)))

(defgeneric failures-of (test-run)
  (:method ((test-run test-run))
    (reduce #'append (mapcar #'self-failures
                             (all-test-runs-of test-run)))))

(defgeneric assertions-of (test-run)
  (:method ((test-run test-run))
    (reduce #'append (mapcar #'self-assertions
                             (all-test-runs-of test-run)))))

(defgeneric skips-of (test-run)
  (:method ((test-run test-run))
    (count t (mapcar #'skipped-p (all-test-runs-of test-run)))))

(defmethod initialize-instance :after ((obj test-run) &key parent-test-run &allow-other-keys)
  (setf (parent-test-run-of obj) parent-test-run))

(defmethod (setf parent-test-run-of) :before (new-parent (obj test-run))
  (declare (ignore new-parent))
  (let ((ex-parent (parent-test-run-of obj)))
    (when ex-parent
      (setf (children-test-runs-of ex-parent)
            (remove obj (children-test-runs-of ex-parent))))))

(defmethod (setf parent-test-run-of) :after (new-parent (obj test-run))
  (when new-parent
    (push obj (children-test-runs-of new-parent))))

(defmethod shared-initialize ((obj test-run) slots &rest args)
  (declare (ignore slots args))
  (call-next-method)
  (with-slots (self-failures self-assertions children-test-runs) obj
    (setq self-failures nil self-assertions nil children-test-runs nil)))

(defgeneric real-time-spent-in-seconds (test-run)
  (:method ((self test-run))
    (let* ((time-spent (internal-realtime-spent-with-test-of self)))
      (when time-spent
        (coerce (/ time-spent
                   internal-time-units-per-second)
                'float)))))

;; Conditions

(define-condition test-assertion (warning)
  ()
  (:documentation "Signalled when an assertion such as IS is encountered"))

(define-condition is-assertion (test-assertion)
  ((form :initarg :form
         :initform (check-required 'form)
         :accessor form-of)
   (message :initarg :message
            :initform (check-required 'message)
            :accessor message-of)
   (message-args :initarg :message-args
                 :initform (check-required 'message-args)
                 :accessor message-args-of)))

(define-condition signals-assertion (test-assertion)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)))

(define-condition not-signals-assertion (test-assertion)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)))

(define-condition finishes-assertion (test-assertion) ())

(define-condition test-related-condition ()
  ((test :initform (check-required 'test) :accessor test-of :initarg :test)))

(define-condition test-started (test-related-condition) ())

(define-condition test-style-warning (style-warning test-related-condition
                                      simple-warning)
  ())

(define-condition failure (simple-condition)
  ((test-run :initform *test-run* :accessor test-run-of
             :documentation "Might perfectly well be NIL") 
   (progress-char :initform #\X :accessor progress-char-of
                  :initarg :progress-char :allocation :class)
   (expected :initarg :expected :initform *failures-and-errors-are-expected*
             :accessor expected-p)))

(define-condition failed-assertion (failure)
  ((form :accessor form-of :initarg :form))
  (:report (lambda (c stream)
             (if (test-run-of c)
                 (format stream "Test assertion failed when running ~a:~%~%"
                         (name-of (test-of (test-run-of c))))
                 (format stream "Test assertion failed:~%~%"))
             (describe c stream))))

(defmethod describe-object ((self failed-assertion) stream)
  (let ((*print-circle* nil))
    (handler-case (apply #'format stream (simple-condition-format-control self)
                         (simple-condition-format-arguments self))
      (error ()
        (format stream "Can't format custom message for ~a form ~a"
                'failed-assertion (form-of self))))))

(define-condition missing-condition (failure)
  ((expected-condition-type :initarg :expected-condition-type
                            :accessor expected-condition-type-of)
   (form :accessor form-of :initarg :form)))

(defmethod describe-object ((self missing-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S failed to signal a condition of type ~S" (form-of self)
            (expected-condition-type-of self))))

(define-condition unwanted-condition (failure)
  ((expected-condition-type :initarg :expected-condition-type :accessor expected-condition-type-of)
   (observed-condition :initarg :observed-condition :accessor observed-condition-of)
   (form :accessor form-of :initarg :form)))

(defmethod describe-object ((self unwanted-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S signaled an unwanted condition ~S"
            (form-of self) (observed-condition-of self))))

(define-condition unexpected-error (failure)
  ((error :accessor error-of :initform (error "Must provide ~S" 'error)
          :initarg :error)
   (progress-char :initform #\E :accessor progress-char-of
                  :initarg :progress-char :allocation :class))
  (:report (lambda (c stream)
             (if (test-run-of c)
                 (format stream "Unexpected error when running ~a:~%~%"
                         (name-of (test-of (test-run-of c))))
                 (format stream "Unexpected error:~%~%"))
             (describe c stream))))

(defmethod describe-object ((self unexpected-error) stream)
  (format stream "~a" (error-of self)))

(defmethod print-object ((self unexpected-error) stream)
  (print-unreadable-object (self stream :identity nil :type nil)
    (format stream "error ~{~A~^,~}: ~S"
            (mapcar (lambda (x)
                      (name-of (test-of x)))
                    (loop for test-run = (test-run-of self) then (parent-test-run-of test-run)
                          while test-run collect test-run))
            (error-of self))))

(define-condition test-skipped (warning)
  ()
  (:documentation "Signalled when test is skipped"))

(defun find-test (name &key (otherwise :error))
  "Find and return test associated with NAME.

If no such thing is found, OTHERWISE says what do to: if :ERROR,
signal error; if a function, call it; else return OTHERWISE."
  (multiple-value-bind (test found-p)
      (if (typep name 'testable)
          (values name t)
          (gethash name *tests*))
    (when (and (not found-p)
               otherwise)
      (typecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf find-test) (new-value key)
  (if new-value
      (progn
        (when (and *warn-about-test-redefinitions*
                   (gethash key *tests*))
          (warn 'test-style-warning
                :format-control "redefining test ~A"
                :format-arguments (list
                                   (let ((*package* #.(find-package "KEYWORD")))
                                     (format nil "~S" key)))))
        (setf (gethash key *tests*) new-value))
      (delete-test key)))

(defun delete-test (name &rest args)
  (let* ((test (apply #'find-test name args))
         (name (name-of test))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite~
and has no parent")
      (remhash name *tests*)
      (setf (parent-of test) nil)
      (fmakunbound (name-of test))
      (loop
        :for subtest :being :the :hash-values :of (children-of test)
        :do (delete-test (name-of subtest)))
      (when (eq *suite* test)
        (setf *suite* parent)))
    test))

(defun all-test-runs-of (test-run)
  (cons test-run
        (loop for test-run in (children-test-runs-of test-run)
              append (all-test-runs-of test-run))))

(defun failed-assertion-p (failure)
  (or (typep failure 'failed-assertion)
      (typep failure 'missing-condition)
      (typep failure 'unwanted-condition)))

(defun unexpected-p (failure)
  (typep failure 'unexpected-error))

(defun test-run-statistics (test-run)
  (let* ((failures (failures-of test-run))
         (failed-assertion-count (count-if #'failed-assertion-p
                                           failures))
         (unexpected-error-count (count-if #'unexpected-p
                                           failures))
         (expected-count (count-if 'expected-p failures))
         (skips-count (skips-of test-run)))
    (list :number-of-tests-run (length (all-test-runs-of test-run))
          :number-of-assertions (length (assertions-of test-run))
          :number-of-failures (length failures)
          :number-of-expected-failures expected-count
          :number-of-failed-assertions failed-assertion-count
          :number-of-unexpected-errors unexpected-error-count
          :number-of-skips skips-count)))

(defmethod print-object ((self test-run) stream)
  (print-unreadable-object (self stream :identity nil :type nil)
    (destructuring-bind (&key number-of-tests-run
                           number-of-assertions
                           number-of-failures
                           number-of-failed-assertions
                           number-of-unexpected-errors
                           number-of-expected-failures
                         &allow-other-keys)
        (test-run-statistics self)
      (format stream "test-run of ~a: ~A test~:P, ~A assertion~:P, ~A failure~:P in ~
~A sec~[~:; (~A failed assertion~:P, ~A error~:P, ~A expected)~]"
              (name-of (test-of self))
              number-of-tests-run
              number-of-assertions
              number-of-failures
              (real-time-spent-in-seconds self)
              number-of-failures ; index in the ~[] conditional
              number-of-failed-assertions
              number-of-unexpected-errors
              (cond ((= number-of-expected-failures number-of-failures)
                     "all")
                    ((zerop number-of-expected-failures)
                     "none")
                    (t number-of-expected-failures))))))

(defmacro without-test-progress-printing (&body body)
  (with-gensyms (old-state)
    `(let ((,old-state *print-test-run-progress*))
       (unwind-protect
            (progn
              (setf *print-test-run-progress* nil)
              ,@body)
         (setf *print-test-run-progress* ,old-state)))))

(defmacro with-toplevel-restarts (&body body)
  `(block restart-wrapper
     (restart-bind
         ((continue-without-debugging
            (lambda ()
              (setf *debug-on-unexpected-error* nil
                    *debug-on-assertion-failure* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Turn off debugging for this test session and invoke the first ~
CONTINUE restart~@:>")))
          (continue-without-debugging-errors
            (lambda ()
              (setf *debug-on-unexpected-error* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Do not stop at unexpected errors for the rest of this test session ~
and continue by invoking the first CONTINUE restart~@:>")))
          (continue-without-debugging-assertions
            (lambda ()
              (setf *debug-on-assertion-failure* nil)
              (continue))
            :report-function (lambda (stream)
                               (format stream "~
~@<Do not stop at failed assertions for the rest of this test session ~
and continue by invoking the first CONTINUE restart~@:>")))
          (abort-testing
            (lambda ()
              (return-from restart-wrapper))
            :report-function (lambda (stream)
                               (format stream "~@<Abort the entire ~
test session~@:>"))))
       ,@body)))

(defun run-failed-tests (&optional (test-run *last-test-result*))
  (warn "Re-running failed tests without considering their dynamic
environment, which may affect their behaviour!")
  (with-toplevel-restarts
    (loop
      :for failure in (failures-of test-run)
      :do (apply (name-of (test-of (test-run-of failure)))
                 (actual-test-arguments-of (test-run-of failure))))
    (when *print-test-run-progress*
      (terpri *debug-io*))))

(defmacro with-expected-failures* (&whole whole condition &body body)
  "Run BODY and registering failure conditions as expected failure iff
CONDITION."
  (with-gensyms (with-expected-failures-block starting-failure-count)
    `(let* ((*failures-and-errors-are-expected* ,condition)
            (,starting-failure-count
              (length (failures-of *test-run*))))
       (block ,with-expected-failures-block
         (restart-case
             (handler-bind ((serious-condition
                              (lambda (error)
                                (record-failure 'unexpected-error :error error)
                                (return-from ,with-expected-failures-block
                                  (values)))))
               (multiple-value-prog1
                   (progn ,@body)
                 (unless (< ,starting-failure-count
                            (length (failures-of *test-run*)))
                   (warn "The following ~S block ran without any failures: ~S"
                         'with-expected-failures* ',whole))))
           (continue ()
             :report (lambda (stream)
                       (format stream "~
~@<Skip the rest of the innermost WITH-EXPECTED-FAILURES body and ~
continue by returning (values)~@:>"))
             (values)))))))

(defmacro with-expected-failures (&body body)
  "Run BODY registering failured conditions as expected failures."
  `(with-expected-failures* t ,@body))

(defun funcall-test-with-feedback-message (test-function &rest args)
  "Run TEST non-interactively and print results to *STANDARD-OUTPUT*.
This function is ideal for ASDF:TEST-OP's."
  (let* ((*test-run-standard-output* (make-broadcast-stream))
         (result (without-debugging (apply test-function args)))
         (*package* (find-package :common-lisp)))
    (format *standard-output*
            "The result of ~S is:

  ~A

For more details run it from the REPL."
            test-function result)
    result))

(defun assert-expression-and-message (input-form)
  (let* ((negatedp nil)
         (predicate)
         (arguments '()))
    (labels ((process (form)
               (if (consp form)
                   (case (first form)
                     ((not)
                      (assert (= (length form) 2))
                      (setf negatedp (not negatedp))
                      (process (second form)))
                     (t (setf predicate (first form))
                      (setf arguments (rest form))))
                   (setf predicate form))))
      (process input-form)
      (cond ((ignore-errors
              (macro-function predicate))
             (values '() input-form "Macro expression ~S evaluated to false."
                     (list `(quote ,input-form))))
            ((and (ignore-errors
                   (fdefinition predicate))
                  ;; let's just skip CL:IF and don't change its evaluation
                  ;; semantics while trying to be more informative...
                  (not (eq predicate 'if)))
             (cond ((= (length arguments) 0)
                    (values '()
                            input-form
                            "Expression ~A evaluated to false."
                            (list `(quote ,input-form))))
                   ((= (length arguments) 2)
                    (with-gensyms (x y)
                      (values `((,x ,(first arguments))
                                (,y ,(second arguments)))
                              (if negatedp
                                  `(not (,predicate ,x ,y))
                                  `(,predicate ,x ,y))
                              "Binary predicate ~A failed.~%~
                               x: ~S => ~S~%~
                               y: ~S => ~S"
                              (list (if negatedp
                                        `(quote (not (,predicate x y)))
                                        `(quote (,predicate x y)))
                                    `(quote ,(first arguments)) x
                                    `(quote ,(second arguments)) y))))
                   (t (let* ((arg-values (mapcar (lambda (el)
                                                   (unless (keywordp el)
                                                     (gensym)))
                                                 arguments))
                             (bindings (loop
                                         :for arg :in arguments
                                         :for arg-value :in arg-values
                                         :when arg-value
                                           :collect `(,arg-value ,arg)))
                             (expression-values
                               (mapcar (lambda (arg-value argument)
                                         (or arg-value argument))
                                       arg-values
                                       arguments))
                             (expression
                               (if negatedp
                                   `(not (,predicate ,@expression-values))
                                   `(,predicate ,@expression-values))))
                        (loop
                          :with message = "Expression ~A evaluated to ~A"
                          :for arg :in arguments
                          :for idx :upfrom 0
                          :for arg-value :in arg-values
                          :when arg-value
                            :do (setf message (concatenate
                                               'string message
                                               "~%~D: ~A => ~S"))
                            :and :append `(,idx (quote ,arg) ,arg-value)
                                   :into message-args
                          :finally
                             (return
                               (values bindings
                                       expression
                                       message
                                       (nconc
                                        (list `(quote (,predicate ,@arguments))
                                              (if negatedp "true" "false"))
                                        message-args))))))))
            (t
             (values '() input-form "Expression ~A evaluated to false."
                     (list `(quote ,input-form))))))))

(defun write-progress-char (char)
  (when *print-test-run-progress*
    (when (and (not (zerop *progress-char-count*))
               (zerop (mod *progress-char-count*
                           *test-progress-print-right-margin*)))
      (terpri *debug-io*))
    (incf *progress-char-count*)
    (write-char char *debug-io*)))

(defun register-assertion-was-successful ()
  (write-progress-char #\.))

(defun record-failure (condition-type &rest args)
  (assert (subtypep condition-type 'failure))
  (let ((failure (apply #'make-condition condition-type args)))
    ;; Remember that IS might be called in any test-run
    ;; and so *TEST-RUN* might be nil.
    ;;
    (when *test-run*
      (push failure (slot-value *test-run* 'self-failures)))
    (write-progress-char (progress-char-of failure))
    (unless (eq condition-type 'unexpected-error)
      (restart-case
          (error failure)
        (continue ()
          :report (lambda (stream)
                    (if *test-run*
                        (format stream "~@<Roger, go on testing...~@:>")
                        (format stream "~@<Ignore the failure and continue~@:>"))))))))

(defmacro is (&whole whole form
              &optional (message nil message-p) &rest message-args)
  (multiple-value-bind (bindings expression expression-message expression-message-args)
      (assert-expression-and-message form)
    (with-gensyms (result format-control format-arguments)
      `(progn
         (warn 'is-assertion :form ',form :message ,message :message-args ',message-args)
         (let* (,@bindings
                (,result (multiple-value-list ,expression)))
           (multiple-value-bind (,format-control ,format-arguments)
               ,(if message-p
		    `(if *always-show-failed-sexp*
			 (values (format nil "~A~%~%~A" ,message ,expression-message)
				 (list ,@message-args ,@expression-message-args))
			 (values ,message (list ,@message-args)))
		    `(values ,expression-message
			     (list ,@expression-message-args)))

             (if (first ,result)
                 (register-assertion-was-successful)
                 (record-failure 'failed-assertion
                                 :form ',whole
                                 :format-control ,format-control
                                 :format-arguments ,format-arguments)))
           (values-list ,result))))))

(defmacro signals (&whole whole what &body body)
  (declare (ignore whole))
  (let* ((condition-type what))
    (unless (symbolp condition-type)
      (error "SIGNALS expects a symbol as condition-type! (Is ~
there a superfulous quote at ~S?)" condition-type))
    `(progn
       (warn 'signals-assertion :expected-condition-type ',what)
       (block test-block
         (handler-bind ((,condition-type
                          (lambda (c)
                            (register-assertion-was-successful)
                            (return-from test-block c))))
           ,@body)
         (record-failure 'missing-condition
                         :expected-condition-type ',what
                         :form ',body)
         (values)))))

(defmacro not-signals (&whole whole what &body body)
  (declare (ignore whole))
  (let* ((condition-type what))
    (unless (symbolp condition-type)
      (error "SIGNALS expects a symbol as condition-type! (Is ~
there a superfulous quote at ~S?)" condition-type))
    `(progn
       (warn 'not-signals-assertion :expected-condition-type ',what)
       (block test-block
         (multiple-value-prog1
             (handler-bind ((,condition-type
                              (lambda (c)
                                (record-failure 'unwanted-condition
                                                :expected-condition-type ',what
                                                :observed-condition c
                                                :form ',body)
                                (return-from test-block c))))
               ,@body)
           (register-assertion-was-successful))))))

(defmacro finishes (&whole whole_ &body body)
  ;; could be `(not-signals t ,@body), but that would register a
  ;; confusing failed-assertion
  (with-gensyms (success? whole ;; test-run
                               )
    `(let* ((,success? nil)
            (,whole ',whole_)
            ;; (,test-run *test-run*)
            )
       (warn 'finishes-assertion)
       (unwind-protect
            (multiple-value-prog1
                (progn
                  ,@body)
              (setf ,success? t)
              (register-assertion-was-successful))
         (unless ,success?
           ;; TODO painfully broken: when we don't finish due to a restart, then
           ;; we don't want this to be triggered
           ;;
           (record-failure 'failed-assertion
                           :form ,whole
                           :format-control "FINISHES block did not finish: ~S"
                           :format-arguments ,whole))))))

(defun skip ()
  (signal 'test-skipped))

(defmacro skip-unless (condition)
  `(unless ,condition (skip)))

(defclass test (testable)
  ((package :initform nil :accessor package-of :initarg :package)
   (lambda-list :initform nil :accessor lambda-list-of :initarg :lambda-list)
   (compile-before-run :initform t :accessor compile-before-run-p
                       :initarg :compile-before-run :type boolean)
   (declarations :initform nil :accessor declarations-of
                 :initarg :declarations)
   (documentation :initform nil :accessor documentation-of
                  :initarg :documentation)
   (body :initform nil :accessor body-of
         :initarg :body)))

(defun ensure-test (name &rest args &key &allow-other-keys)
  (let ((test (find-test name :otherwise nil)))
    (if test
        (apply #'reinitialize-instance test args)
        (apply #'make-instance 'test :name name args))))

(defun call-with-test-handlers (function)
  ;; NOTE: the order of the bindings in this handler-bind is important
  (handler-bind
      ((failure
         (lambda (c)
           (declare (ignore c))
           (unless *debug-on-assertion-failure*
             (continue))))
       (serious-condition
         (lambda (c)
           (record-failure 'unexpected-error :error c)
           (unless *debug-on-unexpected-error*
             (return-from call-with-test-handlers)))))
    (funcall function)))

(defun run-test-body-in-handlers (test function)
  (declare (type test test)
           (type function function))
  (signal 'test-started :test test)
  (labels ((run-test-body ()
             (restart-case
                 (let* ((*package* (package-of test))
                        (*readtable* (copy-readtable))
                        (start-time (get-internal-run-time)))
                   (multiple-value-prog1
                       (funcall function)
                     (setf (internal-realtime-spent-with-test-of *test-run*)
                           (- (get-internal-run-time) start-time))))
               (continue ()
                 :report (lambda (stream)
                           (format stream "~
~@<Skip the rest of the test ~S and continue by ~
returning (values)~@:>" (name-of test)))
                 (values))
               (retest ()
                 :report (lambda (stream)
                           (format stream "~@<Rerun the test ~S~@:>"
                                   (name-of test)))
                 (reinitialize-instance *test-run*)
                 (return-from run-test-body (run-test-body))))))
    (call-with-test-handlers
     (lambda ()
       (run-test-body)))))

(defmacro deftest (&whole whole name args &body body)
  (multiple-value-bind (remaining-forms declarations documentation)
      (parse-body body :documentation t :whole whole)
    (destructuring-bind (name &rest test-args &key (in nil in-provided?)
                                                timeout &allow-other-keys)
        (ensure-list name)
      (remove-from-plistf test-args :in)
      (with-gensyms (body-sym)
        `(progn
           (eval-when (:load-toplevel :execute)
             (ensure-test ',name
                          :package ,*package*
                          :lambda-list ',args
                          :declarations ',declarations
                          :documentation ',documentation
                          :body ',remaining-forms
                          ,@(when in-provided?
                              `(:in (find-test ',in)))
                          ,@test-args))
           (defun ,name ,args
             ,@(when documentation (list documentation))
             ,@declarations
             (let* ((*current-test* (find-test ',name))
                    (parent-test-run *test-run*)
                    (*test-run* nil))
               (labels ((,name () ,@remaining-forms) ; for clarity in debugger
                        (,body-sym ()
                          (setq *test-run*
                                (progn
                                  (make-instance
                                   'test-run
                                   :test *current-test*
                                   :actual-test-arguments ,args
                                   :parent-test-run parent-test-run)))
                          (handler-bind
                              ((test-skipped
                                 (lambda (condition)
                                   (setf (skipped-p *test-run*) t)
                                   (continue condition)))
                               (test-assertion
                                 (lambda (a)
                                   (push a (slot-value *test-run* 'self-assertions))
                                   (muffle-warning)))
                               (test-started
                                 (lambda (c) (declare (ignore c)))))
                            (when ,timeout
                              (error "timeouts are not implemented"))
                            (funcall *run-test-function* *current-test* #',name))))
                 (if parent-test-run
                     (,body-sym)
                     (with-toplevel-restarts
                       (let ((*standard-output* (eval *test-run-standard-output*))
                             (*debug-on-assertion-failure* *debug-on-assertion-failure*)
                             (*debug-on-unexpected-error*  *debug-on-unexpected-error*)
                             (*print-test-run-progress*    *print-test-run-progress*)
                             (*progress-char-count*        *progress-char-count*))
                         (unwind-protect
                              (let ((results (multiple-value-list (,body-sym))))
                                (multiple-value-prog1
                                    (values-list
                                     (append results
                                             (list *test-run*)))
                                  (when *print-test-run-progress*
                                    (terpri *debug-io*))))
                           (push *test-run* *test-result-history*)
                           (setq *last-test-result* *test-run*)))))))))))))

(defun find-suite-for-package (package)
  (gethash package *package-bound-suites*))

(defun make-suite (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))


(defmacro defsuite (name-or-name-with-args &optional args)
  (destructuring-bind (name &rest deftest-args)
      (ensure-list name-or-name-with-args)
    (let ((bind-to-package (getf deftest-args :bind-to-package)))
      (setq bind-to-package
            (if (eq t bind-to-package)
                *package*
                (find-package bind-to-package)))
      (remf deftest-args :bind-to-package)
      (with-gensyms (test)
        `(progn
           (deftest (,name ,@deftest-args) ,args
             (let* ((,test (find-test ',name)))
               (loop
                 :for subtest :being :the :hash-values
                   :of (children-of ,test)
                 :when (and (auto-call? subtest)
                            (or (zerop (length
                                        (lambda-list-of subtest)))
                                (member (first
                                         (lambda-list-of subtest))
                                        '(&rest &key &optional))))
                   :do (funcall (name-of subtest))))
             (values))
           (let ((suite (find-test ',name)))
             ,(when bind-to-package
                `(setf (gethash ,bind-to-package *package-bound-suites*) suite))
             (values suite)))))))

(setf *root-suite* (make-suite 'root-suite :documentation "Root Suite" :in nil))
(setf *suite* *root-suite*)

(defsuite (tool.test.suites::all-tests :in root-suite))

(defun run-all-tests ()
  "Run all currently defined tests."
  (run-tests 'tool.test.suites::all-tests))

(defmacro define-test-package (name-or-name-with-args &body package-options)
  "Defines a new package and binds to it a new test suite.

The binding between package and suite means that tests defined while
inside this package are automatically added to the associated
suite. Inside the new package, the function RUN-PACKAGE-TESTS is the
preferred way to execute the suite. To run the tests from outside, use
RUN-TESTS.

NAME-OR-NAME-WITH-ARGS names the package and suite to create. It is
either a single symbol NAME, or a list (NAME :IN PARENT-SUITE) where
PARENT-SUITE designated the suite previously created with
DEFSUITE that should parent the newly created suite.

Package NAME is defined via normal `defpackage', and in addition to
processing PACKAGE-OPTIONS, automatically USES the :TOOL.TEST and :CL
packages."
  (destructuring-bind (name &key (in 'tool.test.suites::all-tests))
      (ensure-list name-or-name-with-args)
    (unless (find-package name)
      (make-package name :use nil))
    (let ((suite-sym (intern (string name) :tool.test.suites)))
      `(progn
	 (defpackage ,name
	   ,@(append `((:use :tool.test :cl))
		     package-options))
	 (defsuite (,suite-sym :bind-to-package ,name
			       :in ,in))))))

(defun run-tests (testable &key
                             (describe-failures t)
                             verbose
                             (stream *standard-output*)
                             interactive)
  "Execute tests designated by TESTABLE.

Returns two values:

1. A boolean indicating whether all tests were successful, and
2. A list of objects containing test results for each executed suite.

TESTABLE can be a test or suite designator as accepted by
FIND-TEST, or a package designator for a package associated with a
test suite, or a list composed of any combination of the above.

With optional INTERACTIVE, run tests interactively, i.e. break on
errors and unexpected assertion failures.

With optional DESCRIBE-FAILURES, T by default, describe failures to
optional STREAM, which defaults to *STANDARD-OUTPUT*.

With optional VERBOSE print more information about each test run, like
its docstring."
  (loop for thing in (ensure-list testable)
        ;; `suite' is used though it needn't be a test suite, might be
        ;; just a single TESTABLE.
        ;;
        for suite = (etypecase thing
                      (testable thing)
                      (package (find-suite-for-package thing))
                      (symbol (or (find-test thing :otherwise nil)
                                  (find-suite-for-package
                                   (find-package thing)))))
        for result = (progn
                       (assert suite
                               nil
                               "Can't find anything testable designated by ~a"
                               thing)
                       (run-suite-tests suite
                                        :verbose verbose
                                        :stream stream
                                        :interactive interactive)
                       *last-test-result*)
        collect result into results
        do (unless (or interactive
                       (not describe-failures)
                       (zerop (length (failures-of result))))
             (describe-failed-tests :result result :stream stream))

        finally
           (return (values (every #'zerop
                                  (mapcar #'length
                                          (mapcar #'failures-of results)))
                           results))))

(defun run-package-tests (&key (package *package* package-supplied-p)
                            (packages (list *package*) packages-supplied-p)
                            (describe-failures t)
                            verbose
                            (stream *standard-output*)
                            interactive)
  "Execute test suite(s) associated with PACKAGE or PACKAGES.

PACKAGE defaults to the current package. Don't supply both both
PACKAGE and PACKAGES.

See RUN-TESTS for the meaning of the remaining keyword arguments."
  (assert (not (and packages-supplied-p package-supplied-p))
          nil
          "Supply either :PACKAGE or :PACKAGES, not both")
  (run-tests (if packages-supplied-p
                 packages
                 package)
             :describe-failures describe-failures
             :verbose verbose
             :stream stream
             :interactive interactive))

(defun run-suite-tests (suite-designator &key verbose (stream t) interactive)
  (let ((*debug-on-unexpected-error* interactive)
        (*debug-on-assertion-failure* interactive)
        (*print-test-run-progress* nil)
        (*pretty-log-stream*
          (make-instance 'column-counting-output-stream  :understream stream))
        (*pretty-log-verbose-p* verbose)
        (*run-test-function* #'pretty-run-test)
        (*test-run* nil))
    (funcall (etypecase suite-designator
               (symbol suite-designator)
               (test (name-of suite-designator))))
    (terpri stream)
    (values)))

(defun pretty-run-test (test function)
  ;; HACK: until printing of recursive tests is implemented nicely we avoid
  ;; reporting non-toplevel tests altogether.
  (when *within-non-suite-test*
    (return-from pretty-run-test (run-test-body-in-handlers test function)))
  (labels
      ((depth-of (test-run)
         (let ((depth 0))
           (loop while (setf test-run (parent-test-run-of test-run))
                 do (incf depth))
           depth))
       (pp (format-control &rest format-args)
         ;; format magic courtesy of Robert Smith (github #24)
         (format *pretty-log-stream* "~&~v@{~C~:*~}"
                 (* (depth-of *test-run*) 2) #\Space)
         (apply #'format *pretty-log-stream* format-control format-args))
       (suite-p ()
         (not (zerop (hash-table-count (children-of test))))))
    (if (suite-p)
        (pp "~A (Suite)" (name-of test))
        (pp "~A" (name-of test)))
    (let* ((*error-output* *pretty-log-stream*)
           (*standard-output* *pretty-log-stream*)
           (*within-non-suite-test* (not (suite-p)))
           (retval-v-list (multiple-value-list
                           (run-test-body-in-handlers test function)))
           (failures (failures-of *test-run*))
           (skipped (skipped-p *test-run*)))
      (unless (suite-p)
        (format *pretty-log-stream* "~v@{~C~:*~}"
                (max 1 (- *test-progress-print-right-margin*
                          (output-column *pretty-log-stream*)
                          (length "[FAIL]")))
                #\.)
        (format *pretty-log-stream* "[~A]~%"
                (cond
                  (skipped  "SKIP")
                  (failures "FAIL")
                  (t        " OK ")))
        (when (and *pretty-log-verbose-p* (not skipped))
          (pp "    (~A)"
              (or (documentation (name-of test) 'function)
                  "no docstring for this test"))
          (pp "    (~A assertions, ~A failed, ~A errors, ~A expected)~%"
              (length (assertions-of *test-run*))
              (count-if (lambda (x) (typep x 'failed-assertion)) failures)
              (count-if (lambda (x) (typep x 'unexpected-error)) failures)
              (count-if 'expected-p failures))))
      (values-list retval-v-list))))

(defun indented-format (level stream format-control &rest format-arguments)
  (let ((line-prefix (make-string level :initial-element #\Space)))
    (let ((output (format nil "~?~%" format-control format-arguments)))
      (with-input-from-string (s output)
        (loop for line = (read-line s nil nil) until (null line)
              do (format stream "~A~A~%" line-prefix line))))))

(defun describe-failed-tests (&key (result *last-test-result* result-provided-p)
                                (stream t))
  "Prints out a report for RESULT in STREAM.

RESULT defaults to `*last-test-result*' and STREAM defaults to t"
  (check-type result (or null test-run))
  ;; Check if there was a last run.
  (when (null result)
    (unless result-provided-p
      (format stream "~&~%No tests have been run yet.~%"))
    (return-from describe-failed-tests))

  ;; Guaranteed that RESULT is an object of type TEST-RUN.
  (let* ((failures (failures-of result))
         (nfailures (length failures)))
    (cond ((zerop nfailures)
           (format stream "~&~%Test run had no failures.~%"))
          (t
           (format stream "~&~%Test run had ~D failure~:P:~%" nfailures)
           (loop for failure in failures
                 for test-num from 1
                 do (format stream "~%  Failure ~A: ~A when running ~S~%"
                            test-num
                            (type-of failure)
                            (name-of (test-of (test-run-of failure))))
                    (indented-format 4 stream "~a" (describe-object failure nil)))))))
