;;;; Mocking and Stubbing for epsilon.test
;;;;
;;;; Provides test doubles for isolating code under test.
;;;;
;;;; Inspired by mockall (Rust), testify (Go), unittest.mock (Python).

(defpackage epsilon.test.mock
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.log log))
  (:enter t))

;;; Call Recording

(defclass call-record ()
  ((fn-name :initarg :fn-name :reader call-fn-name)
   (args :initarg :args :reader call-args)
   (result :initarg :result :initform nil :accessor call-result)
   (timestamp :initarg :timestamp :reader call-timestamp))
  (:documentation "Record of a function call."))

(defun make-call-record (fn-name args &optional result)
  "Create a call record."
  (make-instance 'call-record
                 :fn-name fn-name
                 :args args
                 :result result
                 :timestamp (get-internal-real-time)))

;;; Stub Management

(defvar *stubs* nil
  "Alist of (symbol . original-fn) for active stubs.")

(defvar *call-log* nil
  "List of call-record for spied functions.")

(defun stub-function (symbol replacement-fn)
  "Replace SYMBOL's function with REPLACEMENT-FN, saving the original."
  (let ((original (and (fboundp symbol) (symbol-function symbol))))
    (push (cons symbol original) *stubs*)
    (setf (symbol-function symbol) replacement-fn)
    original))

(defun unstub-function (symbol)
  "Restore SYMBOL's original function."
  (let ((entry (assoc symbol *stubs*)))
    (when entry
      (if (cdr entry)
          (setf (symbol-function symbol) (cdr entry))
          (fmakunbound symbol))
      (setf *stubs* (remove entry *stubs*)))))

(defun unstub-all ()
  "Restore all stubbed functions."
  (dolist (entry *stubs*)
    (let ((symbol (car entry))
          (original (cdr entry)))
      (if original
          (setf (symbol-function symbol) original)
          (fmakunbound symbol))))
  (setf *stubs* nil))

(defun clear-call-log ()
  "Clear the call log."
  (setf *call-log* nil))

(defun get-calls (fn-name)
  "Get all calls to FN-NAME from the call log."
  (remove-if-not (lambda (r) (eq (call-fn-name r) fn-name))
                 *call-log*))

(defun call-count (fn-name)
  "Count how many times FN-NAME was called."
  (length (get-calls fn-name)))

(defun called-p (fn-name)
  "Return T if FN-NAME was called at least once."
  (> (call-count fn-name) 0))

(defun called-with-p (fn-name &rest expected-args)
  "Return T if FN-NAME was called with EXPECTED-ARGS."
  (some (lambda (record)
          (args-match-p expected-args (call-args record)))
        (get-calls fn-name)))

(defun args-match-p (expected actual)
  "Check if EXPECTED args match ACTUAL args.
   :any matches any single argument."
  (and (= (length expected) (length actual))
       (every (lambda (e a)
                (or (eq e :any)
                    (equal e a)))
              expected actual)))

;;; Stub Builders

(defun returns (value)
  "Create a stub that always returns VALUE."
  (lambda (&rest args)
    (declare (ignore args))
    value))

(defun returns-sequence (values)
  "Create a stub that returns VALUES in sequence, then repeats the last."
  (let ((remaining values))
    (lambda (&rest args)
      (declare (ignore args))
      (if (cdr remaining)
          (pop remaining)
          (car remaining)))))

(defun calls (fn)
  "Create a stub that calls FN with the arguments."
  fn)

(defun raises (condition &rest args)
  "Create a stub that raises CONDITION."
  (lambda (&rest call-args)
    (declare (ignore call-args))
    (apply #'error condition args)))

(defun spy-on (symbol)
  "Create a spy that records calls but invokes the original function."
  (let ((original (symbol-function symbol)))
    (stub-function symbol
                   (lambda (&rest args)
                     (let ((result (apply original args)))
                       (push (make-call-record symbol args result) *call-log*)
                       result)))))

;;; Macros for Scoped Stubbing

(defmacro with-stub ((fn-spec replacement) &body body)
  "Execute BODY with FN-SPEC stubbed to REPLACEMENT.

   FN-SPEC is a function symbol.
   REPLACEMENT is called instead of the original function.

   Example:
   (with-stub (http:get (returns '(:status 200 :body \"{}\")))
     (fetch-user-data \"alice\"))"
  `(progn
     (stub-function ',fn-spec ,replacement)
     (unwind-protect
         (progn ,@body)
       (unstub-function ',fn-spec))))

(defmacro with-stubs (stubs &body body)
  "Execute BODY with multiple stubs.

   STUBS is a list of (fn-spec replacement) pairs.

   Example:
   (with-stubs ((http:get (returns '(:status 200)))
                (db:query (returns '())))
     (process-request))"
  (if (null stubs)
      `(progn ,@body)
      `(with-stub ,(first stubs)
         (with-stubs ,(rest stubs) ,@body))))

(defmacro with-mock ((fn-spec &key returns times with-args) &body body)
  "Execute BODY with FN-SPEC mocked and verify expectations.

   RETURNS - Value to return
   TIMES - Expected call count (nil = any)
   WITH-ARGS - Expected arguments (can use :any)

   Example:
   (with-mock (send-email :times 1
                          :with-args (\"alice@test.com\" :any :any)
                          :returns t)
     (register-user \"alice\" \"alice@test.com\"))"
  (let ((fn-sym (gensym "FN"))
        (expected-times times)
        (expected-args with-args))
    `(let ((,fn-sym ',fn-spec))
       (clear-call-log)
       (stub-function ,fn-sym
                      (lambda (&rest args)
                        (push (make-call-record ,fn-sym args ,returns)
                              *call-log*)
                        ,returns))
       (unwind-protect
           (progn
             ,@body
             ;; Verify expectations
             ,@(when expected-times
                 `((let ((actual-count (call-count ,fn-sym)))
                     (unless (= actual-count ,expected-times)
                       (error "Expected ~A to be called ~D times, but was called ~D times"
                              ,fn-sym ,expected-times actual-count)))))
             ,@(when expected-args
                 `((unless (called-with-p ,fn-sym ,@expected-args)
                     (error "Expected ~A to be called with ~S"
                            ,fn-sym ',expected-args)))))
         (unstub-function ,fn-sym)))))

(defmacro with-spy ((fn-spec calls-var) &body body)
  "Execute BODY with FN-SPEC spied on, binding calls to CALLS-VAR.

   Example:
   (with-spy (log:info calls)
     (process-request)
     (assert-= 2 (length calls)))"
  `(progn
     (clear-call-log)
     (spy-on ',fn-spec)
     (unwind-protect
         (let ((,calls-var nil))
           (prog1
               (progn ,@body)
             (setf ,calls-var (mapcar #'call-args (get-calls ',fn-spec)))))
       (unstub-function ',fn-spec))))

;;; Test Doubles Factory

(defun make-stub (fn-symbol behavior)
  "Create a stub for FN-SYMBOL with given BEHAVIOR.
   BEHAVIOR can be:
   - A value: always return this value
   - A function: call it with the arguments
   - A list of values: return them in sequence"
  (declare (ignore fn-symbol))
  (let ((stub-fn (cond
                   ((functionp behavior) behavior)
                   ((listp behavior) (returns-sequence behavior))
                   (t (returns behavior)))))
    (lambda (&rest args)
      (apply stub-fn args))))

(defun make-mock (fn-symbol &key returns on-call)
  "Create a mock that tracks calls and optionally returns values.
   ON-CALL is called with each invocation's args for side effects."
  (lambda (&rest args)
    (when on-call
      (apply on-call args))
    (push (make-call-record fn-symbol args returns) *call-log*)
    returns))

;;; Verification Utilities

(defun verify-call-count (fn-name expected)
  "Verify FN-NAME was called EXPECTED times."
  (let ((actual (call-count fn-name)))
    (unless (= actual expected)
      (error "Expected ~A to be called ~D times, but was called ~D times"
             fn-name expected actual))))

(defun verify-called (fn-name)
  "Verify FN-NAME was called at least once."
  (unless (called-p fn-name)
    (error "Expected ~A to be called" fn-name)))

(defun verify-not-called (fn-name)
  "Verify FN-NAME was never called."
  (when (called-p fn-name)
    (error "Expected ~A not to be called, but it was called ~D times"
           fn-name (call-count fn-name))))

(defun verify-called-with (fn-name &rest expected-args)
  "Verify FN-NAME was called with EXPECTED-ARGS."
  (unless (apply #'called-with-p fn-name expected-args)
    (let ((actual-calls (mapcar #'call-args (get-calls fn-name))))
      (error "Expected ~A to be called with ~S~%Actual calls: ~S"
             fn-name expected-args actual-calls))))

(defun verify-call-order (&rest fn-names)
  "Verify functions were called in the specified order."
  (let* ((all-calls (sort (copy-list *call-log*) #'<
                          :key #'call-timestamp))
         (relevant-calls (remove-if-not
                          (lambda (r) (member (call-fn-name r) fn-names))
                          all-calls))
         (call-order (mapcar #'call-fn-name relevant-calls)))
    (unless (equal call-order fn-names)
      (error "Expected call order ~S but got ~S" fn-names call-order))))
