;;;; callback-test.lisp - Tests for JIT callback support
;;;;
;;;; Tests the alien-lambda2 + %define-alien-callable callback mechanism.
;;;; Previously skipped due to alien-lambda memory corruption (IMPL-224),
;;;; now re-enabled after migrating to GC-safe static trampolines.

(defpackage :epsilon.foreign.jit.callback.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import
   (epsilon.foreign.jit.callback cb)))

(deftest create-simple-callback
  "can create a simple callback"
  (let ((callback (cb:make-jit-callback (lambda (x) (* x 2)) :int '(:int))))
    (assert-true (cb:jit-callback-p callback))
    (cb:free-jit-callback callback)))

(deftest callback-pointer-is-valid
  "callback pointer is a valid SAP"
  (let ((callback (cb:make-jit-callback (lambda (x) x) :int '(:int))))
    (assert-true (sb-sys:system-area-pointer-p (cb:jit-callback-pointer callback)))
    (assert-true (not (sb-sys:sap= (cb:jit-callback-pointer callback)
                                    (sb-sys:int-sap 0))))
    (cb:free-jit-callback callback)))

(deftest callback-stores-signature
  "callback stores signature information"
  (let ((callback (cb:make-jit-callback (lambda (x) x) :double '(:double))))
    (assert-eq :double (cb:jit-callback-return-type callback))
    (assert-equal '(:double) (cb:jit-callback-arg-types callback))
    (cb:free-jit-callback callback)))

(deftest call-int-callback
  "can call callback returning int"
  (let ((callback (cb:make-jit-callback (lambda (x) (* x 3)) :int '(:int))))
    (assert-= 15 (cb:call-callback callback 5))
    (cb:free-jit-callback callback)))

(deftest call-double-callback
  "can call callback returning double"
  (let ((callback (cb:make-jit-callback (lambda (x) (* x 2.5d0)) :double '(:double))))
    (assert-= 5.0d0 (cb:call-callback callback 2.0d0))
    (cb:free-jit-callback callback)))

(deftest call-void-callback
  "can call callback returning void"
  (let ((called nil))
    (let ((callback (cb:make-jit-callback (lambda () (setf called t)) :void '())))
      (cb:call-callback callback)
      (assert-true called)
      (cb:free-jit-callback callback))))

(deftest call-multi-arg-callback
  "can call callback with multiple arguments"
  (let ((callback (cb:make-jit-callback (lambda (a b) (+ a b)) :int '(:int :int))))
    (assert-= 7 (cb:call-callback callback 3 4))
    (cb:free-jit-callback callback)))

(deftest register-named-callback
  "can register a named callback"
  (let ((id (cb:register-callback 'test-named-cb (lambda (x) x) :int '(:int))))
    (assert-true (integerp id))
    (assert-true (cb:jit-callback-p (cb:get-callback 'test-named-cb)))
    (cb:unregister-callback 'test-named-cb)))

(deftest get-callback-by-id
  "can get callback by ID"
  (let* ((callback (cb:make-jit-callback (lambda () 42) :int '()))
         (id (cb:jit-callback-id callback)))
    (assert-true (cb:jit-callback-p (cb:get-callback id)))
    (cb:free-jit-callback callback)))

(deftest list-callbacks-returns-all
  "list-callbacks returns all registered callbacks"
  (cb:clear-callbacks)
  (let ((cb1 (cb:make-jit-callback (lambda () 1) :int '()))
        (cb2 (cb:make-jit-callback (lambda () 2) :int '())))
    (let ((all (cb:list-callbacks)))
      (assert-true (>= (length all) 2)))
    (cb:free-jit-callback cb1)
    (cb:free-jit-callback cb2)))

(deftest clear-callbacks-removes-all
  "clear-callbacks removes all callbacks"
  (cb:make-jit-callback (lambda () 1) :int '())
  (cb:make-jit-callback (lambda () 2) :int '())
  (cb:clear-callbacks)
  (assert-= 0 (length (cb:list-callbacks))))

(deftest defcallback-creates-function
  "defcallback creates a Lisp function"
  (cb:defcallback test-defcb-fn :int ((x :int))
    (* x 10))
  (assert-= 50 (test-defcb-fn 5))
  (cb:unregister-callback 'test-defcb-fn))

(deftest defcallback-registers-callback
  "defcallback registers the callback"
  (cb:defcallback test-defcb-reg :int ((x :int))
    (+ x 1))
  (assert-true (cb:jit-callback-p (cb:get-callback 'test-defcb-reg)))
  (cb:unregister-callback 'test-defcb-reg))

(deftest defcallback-provides-pointer
  "defcallback provides a callable pointer"
  (cb:defcallback test-defcb-ptr :int ((x :int))
    x)
  (let ((ptr (cb:callback-pointer 'test-defcb-ptr)))
    (assert-true (sb-sys:system-area-pointer-p ptr)))
  (cb:unregister-callback 'test-defcb-ptr))

(deftest with-jit-callback-provides-pointer
  "with-jit-callback provides temporary pointer"
  (cb:with-jit-callback (ptr (lambda (x) (* x 2)) :int '(:int))
    (assert-true (sb-sys:system-area-pointer-p ptr))))

(deftest callback-handles-error
  "callback handles errors gracefully"
  (let ((callback (cb:make-jit-callback (lambda () (error "test error")) :int '())))
    ;; call-callback has error handling -- should return default value, not signal
    (let ((result (cb:call-callback callback)))
      (assert-= 0 result))
    (cb:free-jit-callback callback)))

(deftest callback-float-types
  "callbacks handle float/double types"
  (let ((callback (cb:make-jit-callback (lambda (x) (coerce (* x 2) 'single-float))
                                        :float '(:float))))
    (assert-true (cb:jit-callback-p callback))
    (cb:free-jit-callback callback)))

(deftest callback-unsigned-types
  "callbacks handle unsigned types"
  (let ((callback (cb:make-jit-callback (lambda (x) (+ x 1)) :uint '(:uint))))
    (assert-true (cb:jit-callback-p callback))
    (assert-= 6 (cb:call-callback callback 5))
    (cb:free-jit-callback callback)))
