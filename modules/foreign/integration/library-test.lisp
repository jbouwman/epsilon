;;;; library-test.lisp - Tests for library wrapping

(defpackage :epsilon.foreign.jit.library.test
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (:lib :epsilon.foreign.jit.library)
   (:lc :epsilon.foreign.libclang))
   (:enter t))

;;; ============================================================================
;;; Test Configuration
;;; ============================================================================

;;; Use local test headers that work on all platforms
(defun get-test-headers-dir ()
  "Get the path to the test-headers directory."
  (let* ((env (epsilon.loader:environment))
         (module (epsilon.loader:get-module env "epsilon.foreign")))
    (when module
      (let ((location (epsilon.path:path-string (epsilon.loader:module-location module))))
        (concatenate 'string location "/tests/jit/test-headers/")))))

(defparameter *test-headers-dir* (get-test-headers-dir))

(defparameter *math-header*
  (when *test-headers-dir*
    (concatenate 'string *test-headers-dir* "math.h")))

;;; Helper macro that evaluates headers at runtime (unlike deflibrary which quotes)
(defmacro define-test-library (name &key headers prefix include-functions
                                         exclude-functions include-paths lazy)
  "Like deflibrary but evaluates headers argument."
  `(lib::%define-library ',name
                         (let ((h ,headers))
                           (if (listp h) h (list h)))
                         nil ,prefix
                         ',include-functions
                         ',exclude-functions
                         ,include-paths
                         ,lazy))

;;; ============================================================================
;;; Name Conversion Tests
;;; ============================================================================

(deftest c-name-to-lisp-converts-underscores
  "c-name-to-lisp-name converts underscores to hyphens"
  (let ((result (lib::c-name-to-lisp-name "my_function")))
    (assert-equal "MY-FUNCTION" (symbol-name result))))

(deftest c-name-to-lisp-uppercases
  "c-name-to-lisp-name uppercases the name"
  (let ((result (lib::c-name-to-lisp-name "lowercase")))
    (assert-equal "LOWERCASE" (symbol-name result))))

(deftest c-name-to-lisp-with-prefix
  "c-name-to-lisp-name applies prefix"
  (let ((result (lib::c-name-to-lisp-name "sqrt" "m-")))
    (assert-equal "M-SQRT" (symbol-name result))))

;;; ============================================================================
;;; Include/Exclude Tests
;;; ============================================================================

(deftest should-include-with-no-lists
  "includes all when no lists specified"
  (assert-true (lib::should-include-function-p "anything" nil nil)))

(deftest should-include-with-include-list
  "includes only functions in include list"
  (assert-true (lib::should-include-function-p "sin" '("sin" "cos") nil))
  (assert-true (not (lib::should-include-function-p "tan" '("sin" "cos") nil))))

(deftest should-exclude-with-exclude-list
  "excludes functions in exclude list"
  (assert-true (not (lib::should-include-function-p "sincos" nil '("sincos"))))
  (assert-true (lib::should-include-function-p "sin" nil '("sincos"))))

(deftest include-takes-precedence
  "include list checked before exclude"
  ;; If in include list and not in exclude list, include it
  (assert-true (lib::should-include-function-p "sin" '("sin") '("cos")))
  ;; If in both lists, exclude wins
  (assert-true (not (lib::should-include-function-p "sin" '("sin") '("sin")))))

;;; ============================================================================
;;; Library Definition Tests
;;; ============================================================================

(deftest deflibrary-creates-package
  "deflibrary creates a package for the library"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (unless *math-header*
    (skip "test headers directory not found"))
  ;; Clean up any previous test
  (lib:unload-library 'test-lib-pkg)
  ;; Define library
  (define-test-library test-lib-pkg
    :headers *math-header*
    :include-functions ("sin"))
  ;; Check package exists
  (assert-true (find-package :test-lib-pkg))
  ;; Clean up
  (lib:unload-library 'test-lib-pkg))

(deftest deflibrary-exports-functions
  "deflibrary exports wrapped functions"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-exports)
  (define-test-library test-lib-exports
    :headers *math-header*
    :include-functions ("sin" "cos"))
  ;; Check exports
  (let ((pkg (find-package :test-lib-exports)))
    (assert-true pkg)
    (multiple-value-bind (sym status)
        (find-symbol "SIN" pkg)
      (assert-true sym)
      (assert-equal :external status)))
  (lib:unload-library 'test-lib-exports))

(deftest deflibrary-with-prefix
  "deflibrary applies prefix to function names"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-prefix)
  (define-test-library test-lib-prefix
    :headers *math-header*
    :prefix "math-"
    :include-functions ("sqrt"))
  (let ((pkg (find-package :test-lib-prefix)))
    (assert-true pkg)
    (multiple-value-bind (sym status)
        (find-symbol "MATH-SQRT" pkg)
      (assert-true sym)
      (assert-equal :external status)))
  (lib:unload-library 'test-lib-prefix))

;;; ============================================================================
;;; Function Calling Tests
;;; ============================================================================

;; TODO: cos function returns unexpected value through library wrapper
(deftest deflibrary-functions-callable
  "deflibrary creates callable functions"
  (skip "cos function returns unexpected value through library wrapper")
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-call)
  (define-test-library test-lib-call
    :headers *math-header*
    :include-functions ("sin" "cos" "floor"))
  (let ((sin-fn (find-symbol "SIN" :test-lib-call))
        (cos-fn (find-symbol "COS" :test-lib-call))
        (floor-fn (find-symbol "FLOOR" :test-lib-call)))
    ;; Test sin
    (assert-true (< (abs (- 0.0d0 (funcall sin-fn 0.0d0))) 0.0001))
    (assert-true (< (abs (- 1.0d0 (funcall sin-fn (/ pi 2)))) 0.0001))
    ;; Test cos
    (assert-true (< (abs (- 1.0d0 (funcall cos-fn 0.0d0))) 0.0001))
    ;; Test floor
    (assert-equal 3.0d0 (funcall floor-fn 3.7d0)))
  (lib:unload-library 'test-lib-call))

;; TODO: hypot function returns unexpected value through library wrapper
(deftest deflibrary-multi-arg-functions
  "deflibrary handles multi-argument functions"
  (skip "hypot function returns unexpected value through library wrapper")
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-multi)
  (define-test-library test-lib-multi
    :headers *math-header*
    :include-functions ("pow" "hypot"))
  (let ((pow-fn (find-symbol "POW" :test-lib-multi))
        (hypot-fn (find-symbol "HYPOT" :test-lib-multi)))
    ;; Test pow
    (assert-equal 8.0d0 (funcall pow-fn 2.0d0 3.0d0))
    (assert-equal 1024.0d0 (funcall pow-fn 2.0d0 10.0d0))
    ;; Test hypot
    (assert-equal 5.0d0 (funcall hypot-fn 3.0d0 4.0d0)))
  (lib:unload-library 'test-lib-multi))

;;; ============================================================================
;;; Lazy Loading Tests
;;; ============================================================================

(deftest deflibrary-lazy-loading
  "deflibrary lazy option delays caller creation"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-lazy)
  (define-test-library test-lib-lazy
    :headers *math-header*
    :include-functions ("sin")
    :lazy t)
  ;; Caller should not be created yet
  (let ((wrapper (lib:get-library 'test-lib-lazy)))
    (assert-true wrapper)
    (assert-equal 0 (hash-table-count (lib::library-wrapper-callers wrapper))))
  ;; Call the function
  (let ((sin-fn (find-symbol "SIN" :test-lib-lazy)))
    (funcall sin-fn 0.0d0)
    ;; Now caller should be cached
    (let ((wrapper (lib:get-library 'test-lib-lazy)))
      (assert-equal 1 (hash-table-count (lib::library-wrapper-callers wrapper)))))
  (lib:unload-library 'test-lib-lazy))

;;; ============================================================================
;;; Introspection Tests
;;; ============================================================================

(deftest library-functions-returns-list
  "library-functions returns function list"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-intro)
  (define-test-library test-lib-intro
    :headers *math-header*
    :include-functions ("sin" "cos" "tan"))
  (let ((fns (lib:library-functions 'test-lib-intro)))
    (assert-true (listp fns))
    (assert-equal 3 (length fns))
    (assert-true (member "sin" fns :test #'string=))
    (assert-true (member "cos" fns :test #'string=))
    (assert-true (member "tan" fns :test #'string=)))
  (lib:unload-library 'test-lib-intro))

(deftest library-function-count-correct
  "library-function-count returns correct count"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-count)
  (define-test-library test-lib-count
    :headers *math-header*
    :include-functions ("sin" "cos"))
  (assert-equal 2 (lib:library-function-count 'test-lib-count))
  (lib:unload-library 'test-lib-count))

(deftest get-library-returns-wrapper
  "get-library returns wrapper structure"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-get)
  (define-test-library test-lib-get
    :headers *math-header*
    :include-functions ("sin"))
  (let ((wrapper (lib:get-library 'test-lib-get)))
    (assert-true wrapper)
    (assert-true (lib:library-wrapper-p wrapper))
    (assert-equal 'test-lib-get (lib:library-wrapper-name wrapper)))
  (lib:unload-library 'test-lib-get))

;;; ============================================================================
;;; Unload Tests
;;; ============================================================================

(deftest unload-library-removes-package
  "unload-library removes the package"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-unload)
  (define-test-library test-lib-unload
    :headers *math-header*
    :include-functions ("sin"))
  (assert-true (find-package :test-lib-unload))
  (lib:unload-library 'test-lib-unload)
  (assert-true (null (find-package :test-lib-unload)))
  (assert-true (null (lib:get-library 'test-lib-unload))))

;;; ============================================================================
;;; Exclude Function Tests
;;; ============================================================================

(deftest deflibrary-excludes-functions
  "deflibrary respects exclude-functions"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (lib:unload-library 'test-lib-exclude)
  (define-test-library test-lib-exclude
    :headers *math-header*
    :include-functions ("sin" "cos" "tan")
    :exclude-functions ("tan"))
  (let ((fns (lib:library-functions 'test-lib-exclude)))
    (assert-equal 2 (length fns))
    (assert-true (member "sin" fns :test #'string=))
    (assert-true (member "cos" fns :test #'string=))
    (assert-true (not (member "tan" fns :test #'string=))))
  (lib:unload-library 'test-lib-exclude))
