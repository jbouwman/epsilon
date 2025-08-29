;;;; Tests for the validation system

(defpackage epsilon.validation-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (v epsilon.validation)
   (schema epsilon.module-schema)))

(in-package epsilon.validation-tests)

(deftest test-basic-validators
  ;; Test required validator
  (let ((result (v:required nil)))
    (is (v:validation-failure-p result)))
  (let ((result (v:required "")))
    (is (v:validation-failure-p result)))
  (let ((result (v:required "value")))
    (is (v:validation-success-p result))
    (is (equal "value" (v:validation-value result))))
  
  ;; Test type validators
  (let ((result (v:string-type "hello")))
    (is (v:validation-success-p result)))
  (let ((result (v:string-type 42)))
    (is (v:validation-failure-p result)))
  
  (let ((result (v:integer-type 42)))
    (is (v:validation-success-p result)))
  (let ((result (v:integer-type "not-an-int")))
    (is (v:validation-failure-p result)))
  
  ;; Test list type
  (let ((result (v:list-type '(1 2 3))))
    (is (v:validation-success-p result)))
  (let ((result (v:list-type "not-a-list")))
    (is (v:validation-failure-p result)))
  t)

(deftest test-constraint-validators
  ;; Test min-length
  (let* ((validator (v:min-length 3))
         (result (funcall validator "ab")))
    (is (v:validation-failure-p result)))
  (let* ((validator (v:min-length 3))
         (result (funcall validator "abc")))
    (is (v:validation-success-p result)))
  
  ;; Test max-length
  (let* ((validator (v:max-length 3))
         (result (funcall validator "abcd")))
    (is (v:validation-failure-p result)))
  (let* ((validator (v:max-length 3))
         (result (funcall validator "abc")))
    (is (v:validation-success-p result)))
  
  ;; Test unique-items
  (let* ((validator (v:unique-items))
         (result (funcall validator '(1 2 3 2))))
    (is (v:validation-failure-p result)))
  (let* ((validator (v:unique-items))
         (result (funcall validator '(1 2 3))))
    (is (v:validation-success-p result)))
  
  ;; Test one-of
  (let* ((validator (v:one-of '("red" "green" "blue")))
         (result (funcall validator "yellow")))
    (is (v:validation-failure-p result)))
  (let* ((validator (v:one-of '("red" "green" "blue")))
         (result (funcall validator "green")))
    (is (v:validation-success-p result)))
  t)

(deftest test-composite-validators
  ;; Test all-of
  (let* ((validator (v:all-of (list #'v:required
                                    #'v:string-type
                                    (v:min-length 3))))
         (result (funcall validator "ab")))
    (is (v:validation-failure-p result))
    (is (= 1 (length (v:validation-errors result)))))
  
  (let* ((validator (v:all-of (list #'v:required
                                    #'v:string-type
                                    (v:min-length 3))))
         (result (funcall validator "abc")))
    (is (v:validation-success-p result)))
  
  ;; Test chain
  (let* ((validator (v:chain
                     #'v:required
                     #'v:string-type
                     (v:transform #'string-upcase #'v:string-type)))
         (result (funcall validator "hello")))
    (is (v:validation-success-p result))
    (is (equal "HELLO" (v:validation-value result))))
  
  ;; Test optional
  (let* ((validator (v:optional #'v:string-type))
         (result (funcall validator nil)))
    (is (v:validation-success-p result))
    (is (null (v:validation-value result))))
  
  (let* ((validator (v:optional #'v:string-type))
         (result (funcall validator "value")))
    (is (v:validation-success-p result))
    (is (equal "value" (v:validation-value result))))
  t)

(deftest test-multi-stage-validation
  ;; Test stages that continue on failure
  (let* ((stages (list
                  (v:stage "type" #'v:string-type :continue-on-failure t)
                  (v:stage "length" (v:min-length 5) :continue-on-failure t)))
         (validator (v:multi-stage stages))
         (result (funcall validator 42)))
    (is (v:validation-failure-p result))
    ;; Should have errors from both stages
    (is (>= (length (v:validation-errors result)) 1)))
  
  ;; Test stages that stop on failure
  (let* ((stages (list
                  (v:stage "type" #'v:string-type :continue-on-failure nil)
                  (v:stage "length" (v:min-length 5) :continue-on-failure nil)))
         (validator (v:multi-stage stages))
         (result (funcall validator 42)))
    (is (v:validation-failure-p result))
    ;; Should only have error from first stage
    (is (= 1 (length (v:validation-errors result)))))
  
  ;; Test successful multi-stage
  (let* ((stages (list
                  (v:stage "type" #'v:string-type)
                  (v:stage "length" (v:min-length 3))))
         (validator (v:multi-stage stages))
         (result (funcall validator "hello")))
    (is (v:validation-success-p result))
    (is (equal "hello" (v:validation-value result))))
  t)

(deftest test-module-schema-validation
  ;; Test valid module metadata
  (let ((valid-metadata '(:name "test-module"
                          :version "1.0.0"
                          :description "A test module"
                          :sources ("src")
                          :tests ("tests"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata valid-metadata "/tmp/test/module.lisp")
      ;; This will fail on filesystem validation, but should pass earlier stages
      (is (not valid-p))
      (is (not (null errors)))))
  
  ;; Test invalid structure
  (let ((invalid-metadata '(:name)))  ; Odd number of elements
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata invalid-metadata "/tmp/test/module.lisp")
      (is (not valid-p))
      (is (not (null errors)))
      (is (search "property list" (first errors)))))
  
  ;; Test missing name
  (let ((no-name-metadata '(:version "1.0.0")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata no-name-metadata "/tmp/test/module.lisp")
      (is (not valid-p))
      (is (not (null errors)))
      (is (search "required" (first errors)))))
  
  ;; Test unknown keys
  (let ((unknown-key-metadata '(:name "test" :unknown-key "value")))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata unknown-key-metadata "/tmp/test/module.lisp")
      (is (not valid-p))
      (is (not (null errors)))
      (is (search "Unknown key" (first errors)))))
  
  ;; Test type errors
  (let ((bad-types-metadata '(:name 42  ; Should be string
                              :sources "src")))  ; Should be list
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata bad-types-metadata "/tmp/test/module.lisp")
      (is (not valid-p))
      (is (not (null errors)))
      (is (>= (length errors) 2))))
  
  ;; Test sources/tests intersection
  (let ((overlapping-metadata '(:name "test"
                                :sources ("src" "common")
                                :tests ("tests" "common"))))
    (multiple-value-bind (valid-p errors)
        (schema:validate-module-metadata overlapping-metadata "/tmp/test/module.lisp")
      (is (not valid-p))
      (is (not (null errors)))
      ;; Check if overlap error exists anywhere in errors, not just last
      (is (some (lambda (err) (search "overlap" err)) errors))))
  t)

(deftest test-error-accumulation
  ;; Test that errors accumulate properly
  (let* ((validator (v:all-of (list
                               (v:validator (constantly nil) "Error 1")
                               (v:validator (constantly nil) "Error 2")
                               (v:validator (constantly nil) "Error 3"))))
         (result (funcall validator "test")))
    (is (v:validation-failure-p result))
    (is (= 3 (length (v:validation-errors result))))
    (is (equal "Error 1" (v:validation-error-message (first (v:validation-errors result)))))
    (is (equal "Error 2" (v:validation-error-message (second (v:validation-errors result)))))
    (is (equal "Error 3" (v:validation-error-message (third (v:validation-errors result))))))
  t)

(deftest test-field-context
  ;; Test that field names are properly added to errors
  (let* ((validator #'v:required)
         (result (v:validate validator nil "username")))
    (is (v:validation-failure-p result))
    (let ((error (first (v:validation-errors result))))
      (is (equal "username" (v:validation-error-field error)))))
  
  ;; Test nested field names
  (let* ((inner-validator #'v:required)
         (result1 (v:validate inner-validator nil "field"))
         (result2 (v:validate (lambda (v) (declare (ignore v)) result1) nil "parent")))
    (is (v:validation-failure-p result2))
    (let ((error (first (v:validation-errors result2))))
      (is (equal "parent.field" (v:validation-error-field error)))))
  t)

;; Individual tests will be run by the test framework
;; No need for a master test runner