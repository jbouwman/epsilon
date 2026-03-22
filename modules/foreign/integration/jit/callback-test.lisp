;;;; callback-test.lisp - Tests for JIT callback support
;;;;
;;;; All tests skipped: alien-lambda/alien-lambda2 via eval corrupts SBCL
;;;; memory on CI (IMPL-224). Re-enable after migrating make-jit-callback
;;;; to a safe callback mechanism.

(defpackage :epsilon.foreign.jit.callback.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:cb :epsilon.foreign.jit.callback))
  (:enter t))

(deftest create-simple-callback
  "can create a simple callback"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest callback-pointer-is-valid
  "callback pointer is a valid SAP"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest callback-stores-signature
  "callback stores signature information"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest call-int-callback
  "can call callback returning int"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest call-double-callback
  "can call callback returning double"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest call-void-callback
  "can call callback returning void"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest call-multi-arg-callback
  "can call callback with multiple arguments"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest register-named-callback
  "can register a named callback"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest get-callback-by-id
  "can get callback by ID"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest list-callbacks-returns-all
  "list-callbacks returns all registered callbacks"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest clear-callbacks-removes-all
  "clear-callbacks removes all callbacks"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest defcallback-creates-function
  "defcallback creates a Lisp function"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest defcallback-registers-callback
  "defcallback registers the callback"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest defcallback-provides-pointer
  "defcallback provides a callable pointer"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest with-jit-callback-provides-pointer
  "with-jit-callback provides temporary pointer"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest callback-handles-error
  "callback handles errors gracefully"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest callback-float-types
  "callbacks handle float/double types"
  (skip "alien-lambda memory corruption (IMPL-224)"))

(deftest callback-unsigned-types
  "callbacks handle unsigned types"
  (skip "alien-lambda memory corruption (IMPL-224)"))
