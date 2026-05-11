;;;; Tests for epsilon.doc.emit -- documentation output formatting

(defpackage epsilon.doc.emit-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.emit emit)
           (epsilon.doc.parse parse)
           (epsilon.json json)
           (epsilon.map map)))

;;; S-expression emission

(deftest test-emit-sexp-symbol-doc
  "emit :sexp produces a readable S-expression string."
  (let* ((data (list :name "TRANSPOSE"
                     :package "EPSILON.MUSIC"
                     :type :function
                     :summary "Transpose a pitch."
                     :params (list (list :name "pitch" :doc "A pitch struct."))
                     :since "1.0.0"))
         (result (emit:emit :sexp data)))
    (assert-true (stringp result))
    (assert-true (search "TRANSPOSE" result))
    (assert-true (search "EPSILON.MUSIC" result))))

(deftest test-emit-sexp-roundtrip
  "emit :sexp output can be read back into equivalent data."
  (let* ((data (list :name "FOO" :type :function :summary "Does foo."))
         (result (emit:emit :sexp data))
         (read-back (read-from-string result)))
    (assert-equal "FOO" (getf read-back :name))
    (assert-equal :function (getf read-back :type))))

;;; JSON emission

(deftest test-emit-json-basic
  "emit :json produces valid JSON string."
  (let* ((data (list :name "TRANSPOSE"
                     :package "EPSILON.MUSIC"
                     :type :function
                     :summary "Transpose a pitch."))
         (result (emit:emit :json data)))
    (assert-true (stringp result))
    ;; Should be parseable JSON
    (let ((parsed (json:parse result)))
      (assert-not-null parsed))))

(deftest test-emit-json-has-fields
  "emit :json includes all plist fields."
  (let* ((data (list :name "TRANSPOSE"
                     :package "EPSILON.MUSIC"
                     :type :function
                     :summary "Transpose a pitch."
                     :since "1.0.0"))
         (result (emit:emit :json data))
         (parsed (json:parse result)))
    (assert-equal "TRANSPOSE" (map:get parsed "name"))
    (assert-equal "EPSILON.MUSIC" (map:get parsed "package"))
    (assert-equal "function" (map:get parsed "type"))
    (assert-equal "1.0.0" (map:get parsed "since"))))

(deftest test-emit-json-params
  "emit :json correctly serializes nested param lists."
  (let* ((data (list :name "FN"
                     :type :function
                     :params (list (list :name "x" :doc "First arg.")
                                   (list :name "y" :doc "Second arg."))))
         (result (emit:emit :json data))
         (parsed (json:parse result))
         (params (map:get parsed "params")))
    ;; JSON arrays come back as vectors
    (assert-equal 2 (length params))
    (assert-equal "x" (map:get (aref params 0) "name"))
    (assert-equal "First arg." (map:get (aref params 0) "doc"))))

(deftest test-emit-json-nil-values-omitted
  "emit :json omits keys with NIL values."
  (let* ((data (list :name "FN" :type :function :summary "Does stuff."
                     :params nil :returns nil))
         (result (emit:emit :json data))
         (parsed (json:parse result)))
    ;; nil values should not appear as keys
    (assert-nil (map:get parsed "params"))
    (assert-nil (map:get parsed "returns"))))

;;; Lambda-list emission

(deftest test-emit-json-lambda-list
  "emit :json correctly serializes lambda-list with symbols."
  (let* ((data (list :name "FN"
                     :type :function
                     :lambda-list '(x y &key direction)))
         (result (emit:emit :json data))
         (parsed (json:parse result))
         (ll (map:get parsed "lambda-list")))
    ;; Lambda list elements become strings
    (assert-not-null ll)
    (assert-true (>= (length ll) 3))))

;;; Error cases

(deftest test-emit-unknown-format
  "emit signals error for unknown format."
  (assert-condition (error)
    (emit:emit :xml (list :name "FOO"))))
