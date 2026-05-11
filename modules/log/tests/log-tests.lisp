(cl:defpackage epsilon.log-tests
  (:use :cl :epsilon.test)
  (:local-nicknames (log :epsilon.log)))

(cl:in-package :epsilon.log-tests)

(deftest test-logger-enabled-p-by-level
  "logger-enabled-p returns t for >= the configured level, nil below"
  (let ((logger (log:get-logger "epsilon.log-tests.lvl")))
    (setf (log:logger-level logger) :warn)
    (assert-true (log:logger-enabled-p logger :warn))
    (assert-true (log:logger-enabled-p logger :error))
    (assert-true (log:logger-enabled-p logger :fatal))
    (assert-false (log:logger-enabled-p logger :info))
    (assert-false (log:logger-enabled-p logger :debug))
    (assert-false (log:logger-enabled-p logger :trace))))

(deftest test-get-logger-is-idempotent
  "get-logger returns the same logger object for the same name"
  (let ((a (log:get-logger "epsilon.log-tests.idem"))
        (b (log:get-logger "epsilon.log-tests.idem")))
    (assert-eq a b)))

(deftest test-logger-effective-level-inherits-from-parent
  "logger-effective-level walks up the parent chain when own level is nil"
  (let ((parent (log:get-logger "epsilon.log-tests.parent"))
        (child  (log:get-logger "epsilon.log-tests.parent.child")))
    (setf (log:logger-level parent) :warn)
    (setf (log:logger-level child)  nil)
    (assert-eq :warn (log:logger-effective-level child))
    (setf (log:logger-level child) :debug)
    (assert-eq :debug (log:logger-effective-level child))))

(deftest test-format-timestamp-iso8601-shape
  "format-timestamp produces a 19-character ISO 8601 string"
  (let ((ts (log:format-timestamp 0)))
    (assert-equal 19 (length ts))
    (assert-eql #\T (char ts 10))
    (assert-eql #\- (char ts 4))
    (assert-eql #\: (char ts 13))))

(deftest test-escape-json-string-basics
  "escape-json-string handles backslash, quote, newline, tab"
  (assert-equal "" (log:escape-json-string ""))
  (assert-equal "abc" (log:escape-json-string "abc"))
  (assert-equal "a\\\\b" (log:escape-json-string "a\\b"))
  (assert-equal "a\\\"b" (log:escape-json-string "a\"b"))
  (assert-equal "a\\nb" (log:escape-json-string (format nil "a~%b")))
  (assert-equal "a\\tb" (log:escape-json-string (format nil "a~Cb" #\Tab))))

(deftest test-escape-json-string-control-char
  "Control chars below 0x20 are emitted as \\uXXXX escapes"
  (assert-equal "\\u0001" (log:escape-json-string (string (code-char 1))))
  (assert-equal "\\u001F" (log:escape-json-string (string (code-char #x1F)))))

(deftest test-with-context-binds-and-restores
  "with-context binds the log context dynamically; outer binding restored after"
  (let ((before (symbol-value (find-symbol "*LOG-CONTEXT*" :epsilon.log))))
    (log:with-context (:user "alice" :rid 7)
      (let ((during (symbol-value (find-symbol "*LOG-CONTEXT*" :epsilon.log))))
        (assert-true (member "alice" during :test #'equal))
        (assert-true (member 7 during))))
    (assert-equal before
                  (symbol-value (find-symbol "*LOG-CONTEXT*" :epsilon.log)))))

(deftest test-fields-builds-alist
  "fields combines key/value pairs into an alist of (key . value) cons cells"
  (let ((result (log:fields :a 1 :b "two")))
    (assert-equal 1 (cdr (assoc :a result)))
    (assert-equal "two" (cdr (assoc :b result)))))

(deftest test-fields-rejects-odd-arity
  "fields signals an error when called with an odd number of arguments"
  (assert-condition (error) (log:fields :a 1 :b)))
