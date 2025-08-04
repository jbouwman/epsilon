;;;; Test fixtures provide setup and teardown functionality for tests

(defpackage epsilon.test.fixture
  (:use cl)
  (:local-nicknames
   (#:map #:epsilon.map))
  (:export
   #:fixture
   #:with-fixture
   #:fixture
   #:fixture-p
   #:fixture-name
   #:fixture-setup
   #:fixture-teardown
   #:*fixtures*))

(in-package :epsilon.test.fixture)

(defstruct fixture
  "A test fixture with setup and teardown functions"
  name
  setup-fn
  teardown-fn)

(defparameter *fixtures* (make-hash-table :test 'eq)
  "Registry of defined fixtures")

(defmacro fixture (name (&rest args) &body clauses)
  "Define a test fixture with setup and teardown.
   
   Example:
   (fixture test-server ()
     (:setup
      (let ((server (start-server :port 0)))
        server))
     (:teardown
      (stop-server server)))
   
   The setup clause should return the fixture value.
   The teardown clause can access that value by the fixture name."
  (let ((setup-body nil)
        (teardown-body nil))
    ;; Parse clauses
    (dolist (clause clauses)
      (destructuring-bind (keyword &rest body) clause
        (ecase keyword
          (:setup (setf setup-body body))
          (:teardown (setf teardown-body body)))))
    
    `(progn
       (setf (gethash ',name *fixtures*)
             (make-fixture
              :name ',name
              :setup-fn (lambda ,args
                          ,@setup-body)
              :teardown-fn (lambda (,name)
                            ,@teardown-body)))
       ',name)))

(defmacro with-fixture ((var fixture-name &rest args) &body body)
  "Execute body with fixture setup/teardown.
   
   Example:
   (with-fixture (server test-server)
     (test-server-function server))"
  (let ((fixture-sym (gensym "FIXTURE"))
        (result-sym (gensym "RESULT")))
    `(let ((,fixture-sym (gethash ',fixture-name *fixtures*)))
       (unless ,fixture-sym
         (error "Unknown fixture: ~S" ',fixture-name))
       (let ((,var (funcall (fixture-setup-fn ,fixture-sym) ,@args)))
         (unwind-protect
              (let ((,result-sym (progn ,@body)))
                ,result-sym)
           (when (fixture-teardown-fn ,fixture-sym)
             (funcall (fixture-teardown-fn ,fixture-sym) ,var)))))))

(defun clear-fixtures ()
  "Clear all registered fixtures"
  (clrhash *fixtures*))

(defun list-fixtures ()
  "Return a list of all registered fixture names"
  (loop for name being the hash-keys of *fixtures*
        collect name))