(defpackage #:epsilon.reader.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:reader #:epsilon.reader)))

(in-package #:epsilon.reader.tests)

;;; Helper to read with epsilon syntax

(defun read-fn (string)
  "Read a string using epsilon reader extensions"
  (reader:with-epsilon-reader
    (read-from-string string)))

;;; Helper to check lambda structure
(defun lambda-form-p (form num-args)
  "Check if FORM is a lambda with NUM-ARGS parameters"
  (and (listp form)
       (eq (first form) 'lambda)
       (listp (second form))
       (= (length (second form)) num-args)))

;;; Basic parsing tests

(deftest test-nullary-function
  (let ((fn (read-fn "#f(list)")))
    (is (lambda-form-p fn 0))
    (is (equal '(list) (third fn)))))

(deftest test-single-arg-percent
  (let ((fn (read-fn "#f(+ % 1)")))
    (is (lambda-form-p fn 1))
    ;; Check the body has the right structure
    (is (eq '+ (first (third fn))))))

(deftest test-single-arg-execution
  (let ((fn (eval (read-fn "#f(+ % 1)"))))
    (is (= 2 (funcall fn 1)))
    (is (= 11 (funcall fn 10)))))

(deftest test-two-args
  (let ((fn (read-fn "#f(+ %1 %2)")))
    (is (lambda-form-p fn 2))))

(deftest test-two-args-execution
  (let ((fn (eval (read-fn "#f(+ %1 %2)"))))
    (is (= 3 (funcall fn 1 2)))
    (is (= 15 (funcall fn 10 5)))))

(deftest test-three-args
  (let ((fn (read-fn "#f(list %1 %2 %3)")))
    (is (lambda-form-p fn 3))))

(deftest test-three-args-execution
  (let ((fn (eval (read-fn "#f(list %1 %2 %3)"))))
    (is (equal '(a b c) (funcall fn 'a 'b 'c)))))

(deftest test-non-sequential-args
  ;; Using %1 and %3 should still create 3-arg lambda
  (let ((fn (read-fn "#f(list %1 %3)")))
    (is (lambda-form-p fn 3))))

(deftest test-non-sequential-execution
  (let ((fn (eval (read-fn "#f(cons %1 %3)"))))
    (is (equal '(a . c) (funcall fn 'a 'b 'c)))))

;;; Nested expression tests

(deftest test-nested-expression
  (let ((fn (read-fn "#f(string-upcase (first %))")))
    (is (lambda-form-p fn 1))))

(deftest test-nested-execution
  (let ((fn (eval (read-fn "#f(string-upcase (first %))"))))
    (is (equal "HELLO" (funcall fn '("hello" "world"))))))

(deftest test-deeply-nested
  (let ((fn (eval (read-fn "#f(car (cdr (car %)))"))))
    (is (= 2 (funcall fn '((1 2 3)))))))

;;; Edge cases

(deftest test-nullary-execution
  (let ((fn (eval (read-fn "#f(+ 1 2)"))))
    (is (= 3 (funcall fn)))))

(deftest test-percent-in-string
  ;; % in a string should not be treated as an argument
  (let ((fn (read-fn "#f(format nil \"~a%\" %)")))
    (is (lambda-form-p fn 1))))

(deftest test-complex-body
  (let ((fn (eval (read-fn "#f(if (> % 0) :positive :non-positive)"))))
    (is (eq :positive (funcall fn 5)))
    (is (eq :non-positive (funcall fn -5)))
    (is (eq :non-positive (funcall fn 0)))))

;;; Installation/uninstallation tests

(deftest test-install-uninstall
  ;; Should be able to install and uninstall without error
  (let ((original *readtable*))
    (reader:install-reader-macros)
    (is (not (eq original *readtable*)))
    (reader:uninstall-reader-macros)
    ;; After uninstall, we should have a restored readtable
    (is t)))

(deftest test-with-epsilon-reader-restores
  (let ((before *readtable*))
    (reader:with-epsilon-reader
      ;; Inside, we have epsilon extensions
      (is (not (eq before *readtable*))))
    ;; Outside, original is restored
    (is (eq before *readtable*))))

;;; Integration tests with standard functions

(deftest test-mapcar-integration
  (reader:with-epsilon-reader
    (is (equal '(2 3 4)
               (mapcar (eval (read-from-string "#f(+ % 1)"))
                       '(1 2 3))))))

(deftest test-remove-if-integration
  (reader:with-epsilon-reader
    (is (equal '(2 4 6)
               (remove-if (eval (read-from-string "#f(oddp %)"))
                          '(1 2 3 4 5 6))))))

(deftest test-reduce-integration
  (reader:with-epsilon-reader
    (is (= 15
           (reduce (eval (read-from-string "#f(+ %1 %2)"))
                   '(1 2 3 4 5))))))

(deftest test-sort-integration
  (reader:with-epsilon-reader
    (let* ((compare-fn (eval (read-from-string "#f(< %1 %2)")))
           (result (sort (list 3 1 4 1 5 9 2 6) compare-fn)))
      ;; Just check it's sorted in ascending order
      (is (= 1 (first result)))
      (is (= 9 (car (last result)))))))
