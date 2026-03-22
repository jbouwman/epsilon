(defpackage epsilon.result-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.result res)
            (epsilon.option opt))
  (:enter t))

;;; Constructor tests

(deftest test-ok-basic
  (let ((r (res:ok 42)))
    (assert-true (res:ok-p r))
    (assert-true (not (res:err-p r)))
    (assert-true (res:result-p r))))

(deftest test-ok-nil
  ;; Unlike Option, Ok can hold nil
  (let ((r (res:ok nil)))
    (assert-true (res:ok-p r))
    (assert-true (null (res:unwrap r)))))

(deftest test-err-basic
  (let ((r (res:err "error message")))
    (assert-true (res:err-p r))
    (assert-true (not (res:ok-p r)))
    (assert-true (res:result-p r))))

;;; Predicate tests

(deftest test-predicates
  (assert-true (res:ok-p (res:ok 1)))
  (assert-true (not (res:ok-p (res:err "e"))))
  (assert-true (not (res:ok-p 42)))
  (assert-true (res:err-p (res:err "e")))
  (assert-true (not (res:err-p (res:ok 1))))
  (assert-true (not (res:err-p nil)))
  (assert-true (res:result-p (res:ok 1)))
  (assert-true (res:result-p (res:err "e")))
  (assert-true (not (res:result-p 42)))
  (assert-true (not (res:result-p nil))))

;;; Accessor tests

(deftest test-unwrap-ok
  (assert-true (= 42 (res:unwrap (res:ok 42)))))

(deftest test-unwrap-err-errors
  (assert-condition (error) (res:unwrap (res:err "fail"))))

(deftest test-expect-ok
  (assert-true (= 42 (res:expect (res:ok 42) "should not fail"))))

(deftest test-expect-err-errors-with-message
  (assert-condition (error)
    (res:expect (res:err "original") "operation failed")))

(deftest test-unwrap-err-on-err
  (assert-true (equal "fail" (res:unwrap-err (res:err "fail")))))

(deftest test-unwrap-err-on-ok-errors
  (assert-condition (error) (res:unwrap-err (res:ok 42))))

(deftest test-expect-err-on-err
  (assert-true (equal "fail" (res:expect-err (res:err "fail") "should fail"))))

(deftest test-expect-err-on-ok-errors
  (assert-condition (error) (res:expect-err (res:ok 42) "should fail")))

(deftest test-unwrap-or
  (assert-true (= 42 (res:unwrap-or (res:ok 42) 0)))
  (assert-true (= 0 (res:unwrap-or (res:err "fail") 0))))

(deftest test-unwrap-or-else
  (assert-true (= 42 (res:unwrap-or-else (res:ok 42)
                                (lambda (e)
                                  (declare (ignore e))
                                  (error "should not be called")))))
  ;; Default-fn receives the error
  (let ((received-error nil))
    (assert-true (= 99 (res:unwrap-or-else (res:err "my-error")
                                  (lambda (e)
                                    (setf received-error e)
                                    99))))
    (assert-true (equal "my-error" received-error))))

;;; Transformation tests

(deftest test-map-ok
  (let ((result (res:map #'1+ (res:ok 1))))
    (assert-true (res:ok-p result))
    (assert-true (= 2 (res:unwrap result)))))

(deftest test-map-err
  (let ((result (res:map #'1+ (res:err "fail"))))
    (assert-true (res:err-p result))
    (assert-true (equal "fail" (res:unwrap-err result)))))

(deftest test-map-err-transformation
  (let ((result (res:map-err #'string-upcase (res:err "fail"))))
    (assert-true (res:err-p result))
    (assert-true (equal "FAIL" (res:unwrap-err result)))))

(deftest test-map-err-on-ok
  (let ((result (res:map-err #'string-upcase (res:ok 42))))
    (assert-true (res:ok-p result))
    (assert-true (= 42 (res:unwrap result)))))

(deftest test-flatmap-ok-returns-ok
  (let ((result (res:flatmap (lambda (x) (res:ok (* x 2)))
                             (res:ok 5))))
    (assert-true (res:ok-p result))
    (assert-true (= 10 (res:unwrap result)))))

(deftest test-flatmap-ok-returns-err
  (let ((result (res:flatmap (lambda (x)
                               (declare (ignore x))
                               (res:err "inner error"))
                             (res:ok 5))))
    (assert-true (res:err-p result))
    (assert-true (equal "inner error" (res:unwrap-err result)))))

(deftest test-flatmap-err
  (let ((called nil))
    (let ((result (res:flatmap (lambda (x)
                                 (setf called t)
                                 (res:ok x))
                               (res:err "fail"))))
      (assert-true (res:err-p result))
      (assert-true (not called)))))

(deftest test-flatmap-requires-result-return
  (assert-condition (error) (res:flatmap (lambda (x) x) (res:ok 5))))

(deftest test-flatten-ok-ok
  (let ((result (res:flatten (res:ok (res:ok 42)))))
    (assert-true (res:ok-p result))
    (assert-true (= 42 (res:unwrap result)))))

(deftest test-flatten-ok-err
  (let ((result (res:flatten (res:ok (res:err "inner")))))
    (assert-true (res:err-p result))
    (assert-true (equal "inner" (res:unwrap-err result)))))

(deftest test-flatten-err
  (let ((result (res:flatten (res:err "outer"))))
    (assert-true (res:err-p result))
    (assert-true (equal "outer" (res:unwrap-err result)))))

(deftest test-flatten-requires-nested-result
  (assert-condition (error) (res:flatten (res:ok 42))))

;;; Combinator tests

(deftest test-and-then
  ;; Same as flatmap
  (let ((result (res:and-then (res:ok 5)
                              (lambda (x) (res:ok (* x 2))))))
    (assert-true (= 10 (res:unwrap result)))))

(deftest test-or-else-ok
  (let ((called nil))
    (let ((result (res:or-else (res:ok 42)
                               (lambda (e)
                                 (declare (ignore e))
                                 (setf called t)
                                 (res:ok 0)))))
      (assert-true (= 42 (res:unwrap result)))
      (assert-true (not called)))))

(deftest test-or-else-err-recovers
  (let ((result (res:or-else (res:err "fail")
                             (lambda (e)
                               (declare (ignore e))
                               (res:ok 99)))))
    (assert-true (res:ok-p result))
    (assert-true (= 99 (res:unwrap result)))))

(deftest test-or-else-err-returns-new-err
  (let ((result (res:or-else (res:err "first")
                             (lambda (e)
                               (res:err (format nil "wrapped: ~A" e))))))
    (assert-true (res:err-p result))
    (assert-true (equal "wrapped: first" (res:unwrap-err result)))))

(deftest test-or-else-requires-result-return
  (assert-condition (error) (res:or-else (res:err "fail")
                                  (lambda (e)
                                    (declare (ignore e))
                                    42))))

(deftest test-transpose-ok-some
  (let ((result (res:transpose (res:ok (opt:some 42)))))
    (assert-true (opt:some-p result))
    (let ((inner (opt:unwrap result)))
      (assert-true (res:ok-p inner))
      (assert-true (= 42 (res:unwrap inner))))))

(deftest test-transpose-ok-none
  (assert-true (opt:none-p (res:transpose (res:ok (opt:none))))))

(deftest test-transpose-err
  (let ((result (res:transpose (res:err "fail"))))
    (assert-true (opt:some-p result))
    (let ((inner (opt:unwrap result)))
      (assert-true (res:err-p inner))
      (assert-true (equal "fail" (res:unwrap-err inner))))))

;;; Conversion tests

(deftest test-ok-option-on-ok
  (let ((result (res:ok-option (res:ok 42))))
    (assert-true (opt:some-p result))
    (assert-true (= 42 (opt:unwrap result)))))

(deftest test-ok-option-on-err
  (assert-true (opt:none-p (res:ok-option (res:err "fail")))))

(deftest test-err-option-on-err
  (let ((result (res:err-option (res:err "fail"))))
    (assert-true (opt:some-p result))
    (assert-true (equal "fail" (opt:unwrap result)))))

(deftest test-err-option-on-ok
  (assert-true (opt:none-p (res:err-option (res:ok 42)))))

(deftest test-from-condition-success
  (let ((result (res:from-condition (+ 1 2))))
    (assert-true (res:ok-p result))
    (assert-true (= 3 (res:unwrap result)))))

(deftest test-from-condition-error
  (let ((result (res:from-condition (error "boom"))))
    (assert-true (res:err-p result))
    (assert-true (typep (res:unwrap-err result) 'error))))

(deftest test-try-call-success
  (let ((result (res:try-call #'+ 1 2)))
    (assert-true (res:ok-p result))
    (assert-true (= 3 (res:unwrap result)))))

(deftest test-try-call-error
  (let ((result (res:try-call #'parse-integer "not-a-number")))
    (assert-true (res:err-p result))))

;;; Print representation tests

(deftest test-print-ok
  (let ((str (format nil "~A" (res:ok 42))))
    (assert-true (search "OK" str))
    (assert-true (search "42" str))))

(deftest test-print-err
  (let ((str (format nil "~A" (res:err "fail"))))
    (assert-true (search "ERR" str))
    (assert-true (search "fail" str))))

;;; Integration tests

(deftest test-result-chain
  ;; Simulate parsing and validating
  (flet ((parse-int (s)
           (handler-case
               (res:ok (parse-integer s))
             (error () (res:err "parse error"))))
         (validate-positive (n)
           (if (> n 0)
               (res:ok n)
               (res:err "must be positive"))))
    ;; Successful chain
    (let ((result (res:flatmap #'validate-positive
                               (parse-int "42"))))
      (assert-true (res:ok-p result))
      (assert-true (= 42 (res:unwrap result))))
    ;; Parse fails
    (let ((result (res:flatmap #'validate-positive
                               (parse-int "bad"))))
      (assert-true (res:err-p result))
      (assert-true (equal "parse error" (res:unwrap-err result))))
    ;; Validation fails
    (let ((result (res:flatmap #'validate-positive
                               (parse-int "-5"))))
      (assert-true (res:err-p result))
      (assert-true (equal "must be positive" (res:unwrap-err result))))))

(deftest test-result-recovery
  ;; Try primary, fall back to secondary
  (flet ((primary () (res:err "primary failed"))
         (secondary () (res:ok "from secondary")))
    (let ((result (res:or-else (primary)
                               (lambda (e)
                                 (declare (ignore e))
                                 (secondary)))))
      (assert-true (res:ok-p result))
      (assert-true (equal "from secondary" (res:unwrap result))))))
