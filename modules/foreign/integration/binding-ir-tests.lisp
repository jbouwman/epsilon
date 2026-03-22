;;;; binding-ir-tests.lisp - Tests for Binding Intermediate Representation

(defpackage :epsilon.foreign.binding-ir.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:bir :epsilon.foreign.binding-ir)
   (:lc :epsilon.foreign.libclang))
   (:enter t))

;;; ============================================================================
;;; BIR Structure Tests
;;; ============================================================================

(deftest create-binding-ir
  "can create a binding-ir structure"
  (let ((ir (bir:make-binding-ir)))
    (assert-true (bir:binding-ir-p ir))
    (assert-equal bir:+bir-version+ (bir:binding-ir-version ir))
    (assert-true (bir:binding-ir-platform ir))))

(deftest create-bir-function
  "can create a bir-function structure"
  (let ((fn (bir:make-bir-function
             :name "test_func"
             :return-type :int
             :params nil
             :variadic-p nil)))
    (assert-true (bir:bir-function-p fn))
    (assert-equal "test_func" (bir:bir-function-name fn))
    (assert-equal :int (bir:bir-function-return-type fn))))

(deftest create-bir-struct
  "can create a bir-struct structure"
  (let ((st (bir:make-bir-struct
             :name "test_struct"
             :size 16
             :alignment 8)))
    (assert-true (bir:bir-struct-p st))
    (assert-equal "test_struct" (bir:bir-struct-name st))
    (assert-equal 16 (bir:bir-struct-size st))
    (assert-equal 8 (bir:bir-struct-alignment st))))

(deftest create-bir-enum
  "can create a bir-enum structure"
  (let ((en (bir:make-bir-enum
             :name "test_enum"
             :values (list (bir:make-bir-enum-value :name "A" :value 0)
                           (bir:make-bir-enum-value :name "B" :value 1)))))
    (assert-true (bir:bir-enum-p en))
    (assert-equal "test_enum" (bir:bir-enum-name en))
    (assert-equal 2 (length (bir:bir-enum-values en)))))

;;; ============================================================================
;;; Serialization Round-Trip Tests
;;; ============================================================================

(deftest write-and-read-binding-ir
  "can write and read back a binding-ir"
  (let* ((fn1 (bir:make-bir-function
               :name "foo"
               :return-type :int
               :params (list (bir:make-bir-param :name "x" :type :int))
               :variadic-p nil
               :header "test.h"
               :line 42))
         (st1 (bir:make-bir-struct
               :name "point"
               :size 8
               :alignment 4
               :fields (list (bir:make-bir-field :name "x" :type :int :offset 0 :size 4)
                             (bir:make-bir-field :name "y" :type :int :offset 4 :size 4))))
         (en1 (bir:make-bir-enum
               :name "color"
               :values (list (bir:make-bir-enum-value :name "RED" :value 0)
                             (bir:make-bir-enum-value :name "GREEN" :value 1))))
         (ir (bir:make-binding-ir
              :source-headers '("test.h")
              :functions (list fn1)
              :structs (list st1)
              :enums (list en1))))

    ;; Write to string
    (let ((output (with-output-to-string (s)
                    (bir:write-binding-ir ir s))))
      (assert-true (search ":BINDING-IR" output))
      (assert-true (search "foo" output))
      (assert-true (search "point" output))
      (assert-true (search "color" output))

      ;; Read back
      (let ((ir2 (with-input-from-string (s output)
                   (bir:read-binding-ir s))))
        (assert-true (bir:binding-ir-p ir2))
        (assert-equal 1 (length (bir:binding-ir-functions ir2)))
        (assert-equal 1 (length (bir:binding-ir-structs ir2)))
        (assert-equal 1 (length (bir:binding-ir-enums ir2)))

        ;; Check function
        (let ((fn2 (first (bir:binding-ir-functions ir2))))
          (assert-equal "foo" (bir:bir-function-name fn2))
          (assert-equal :int (bir:bir-function-return-type fn2))
          (assert-equal 42 (bir:bir-function-line fn2)))

        ;; Check struct
        (let ((st2 (first (bir:binding-ir-structs ir2))))
          (assert-equal "point" (bir:bir-struct-name st2))
          (assert-equal 8 (bir:bir-struct-size st2))
          (assert-equal 2 (length (bir:bir-struct-fields st2))))

        ;; Check enum
        (let ((en2 (first (bir:binding-ir-enums ir2))))
          (assert-equal "color" (bir:bir-enum-name en2))
          (assert-equal 2 (length (bir:bir-enum-values en2))))))))

;;; ============================================================================
;;; Validation Tests
;;; ============================================================================

(deftest validate-binding-ir-success
  "validate-binding-ir succeeds on valid BIR"
  (let ((ir (bir:make-binding-ir)))
    (assert-true (bir:validate-binding-ir ir))))

(deftest bir-compatible-current-platform
  "bir-compatible-p returns true for current platform"
  (let ((ir (bir:make-binding-ir)))
    (assert-true (bir:bir-compatible-p ir))))

;;; ============================================================================
;;; Code Generation Tests
;;; ============================================================================

(deftest generate-defshared-form
  "can generate defshared form from bir-function"
  (let ((fn (bir:make-bir-function
             :name "my_func"
             :return-type :int
             :params (list (bir:make-bir-param :name "x" :type :int)
                           (bir:make-bir-param :name "y" :type :double)))))
    (let ((form (bir::generate-defshared-form fn :mylib)))
      (assert-true (listp form))
      (assert-eq 'epsilon.foreign:defshared (first form))
      ;; Compare symbol name since package may differ
      (assert-equal "MY-FUNC" (symbol-name (second form)))
      (assert-equal "my_func" (third form))
      (assert-eq :mylib (fourth form))
      (assert-eq :int (fifth form)))))

(deftest c-name-to-lisp-name-conversion
  "c-name-to-lisp-name converts correctly"
  (assert-equal "pq-connectdb" (bir::c-name-to-lisp-name "PQconnectdb"))
  (assert-equal "ssl-ctx-new" (bir::c-name-to-lisp-name "SSL_CTX_new"))
  (assert-equal "simple-name" (bir::c-name-to-lisp-name "simple_name"))
  (assert-equal "foo" (bir::c-name-to-lisp-name "foo")))

;;; ============================================================================
;;; Header Groveling Tests (require libclang)
;;; ============================================================================

(defun get-test-headers-dir ()
  "Get the path to the test-headers directory."
  (let* ((env (epsilon.loader:environment))
         (module (epsilon.loader:get-module env "epsilon.foreign")))
    (when module
      (let ((location (epsilon.path:path-string (epsilon.loader:module-location module))))
        (concatenate 'string location "/tests/jit/test-headers/")))))

(defparameter *test-headers-dir* (get-test-headers-dir))

(defparameter *test-math-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "math.h")))

(deftest grovel-to-ir-math-header
  "can grovel math.h to IR"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (unless *test-math-header*
    (skip "Test headers not found"))
  (let ((ir (bir:grovel-to-ir *test-math-header*)))
    (assert-true (bir:binding-ir-p ir))
    (assert-true (> (length (bir:binding-ir-functions ir)) 0))
    ;; Should find sin, cos, etc.
    (let ((fn-names (mapcar #'bir:bir-function-name (bir:binding-ir-functions ir))))
      (assert-true (member "sin" fn-names :test #'string=))
      (assert-true (member "cos" fn-names :test #'string=)))))

(deftest grovel-to-ir-with-prefix
  "can grovel headers with prefix filter"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (unless *test-math-header*
    (skip "Test headers not found"))
  ;; This test uses a prefix that won't match anything in math.h
  (let ((ir (bir:grovel-to-ir *test-math-header* :prefix "ZZZZZ")))
    (assert-true (bir:binding-ir-p ir))
    (assert-equal 0 (length (bir:binding-ir-functions ir)))))
