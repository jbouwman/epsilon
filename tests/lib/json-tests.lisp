(defpackage :epsilon.lib.json.tests
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames
   (#:json #:epsilon.lib.json)
   (#:test #:epsilon.tool.test)))

(in-package :epsilon.lib.json.tests)

;; Helper function to load test JSON files
(defun load-test-json (filename)
  "Load a JSON test file from tests/lib/json/"
  (let ((path (test:project-file "epsilon" (format nil "tests/lib/json/~a" filename))))
    (with-open-file (stream path :direction :input)
      (let ((content (make-string (file-length stream))))
        (read-sequence content stream)
        content))))

;; Basic value tests

(deftest parse-simple-string ()
  "Test parsing a simple JSON string"
  (let ((content (load-test-json "simple-string.json")))
    (is (string= (json:parse-string content) "hello world"))))

(deftest parse-simple-number ()
  "Test parsing a simple JSON number"
  (let ((content (load-test-json "simple-number.json")))
    (is (= (json:parse-string content) 42))))

(deftest parse-boolean-true ()
  "Test parsing JSON true literal"
  (is (eq (json:parse-string "true") t)))

(deftest parse-boolean-false ()
  "Test parsing JSON false literal"
  (is (eq (json:parse-string "false") nil)))

(deftest parse-null ()
  "Test parsing JSON null literal"
  (is (eq (json:parse-string "null") nil)))

;; Array tests

(deftest parse-simple-array ()
  "Test parsing a simple JSON array"
  (let ((content (load-test-json "simple-array.json")))
    (let ((result (json:parse-string content)))
      (is (listp result))
      (is (= (length result) 4))
      (is (= (first result) 1))
      (is (= (second result) 2))
      (is (= (third result) 3))
      (is (string= (fourth result) "hello")))))

(deftest parse-empty-array ()
  "Test parsing an empty JSON array"
  (let ((result (json:parse-string "[]")))
    (is (listp result))
    (is (null result))))

;; Object tests

(deftest parse-simple-object ()
  "Test parsing a simple JSON object"
  (let ((content (load-test-json "simple-object.json")))
    (let ((result (json:parse-string content)))
      (is (listp result))
      (is (string= (cdr (assoc "name" result :test #'string=)) "John"))
      (is (= (cdr (assoc "age" result :test #'string=)) 30))
      (is (eq (cdr (assoc "active" result :test #'string=)) t)))))

(deftest parse-empty-object ()
  "Test parsing an empty JSON object"
  (let ((result (json:parse-string "{}")))
    (is (listp result))
    (is (null result))))

(deftest parse-nested-object ()
  "Test parsing a nested JSON object"
  (let ((content (load-test-json "nested-object.json")))
    (let ((result (json:parse-string content)))
      (is (listp result))
      ;; Check user object
      (let ((user (cdr (assoc "user" result :test #'string=))))
        (is (listp user))
        (is (string= (cdr (assoc "name" user :test #'string=)) "Alice"))
        ;; Check nested details
        (let ((details (cdr (assoc "details" user :test #'string=))))
          (is (listp details))
          (is (= (cdr (assoc "age" details :test #'string=)) 25))
          (let ((hobbies (cdr (assoc "hobbies" details :test #'string=))))
            (is (listp hobbies))
            (is (= (length hobbies) 2))
            (is (string= (first hobbies) "reading"))
            (is (string= (second hobbies) "coding")))))
      ;; Check settings object
      (let ((settings (cdr (assoc "settings" result :test #'string=))))
        (is (listp settings))
        (is (string= (cdr (assoc "theme" settings :test #'string=)) "dark"))
        (is (eq (cdr (assoc "notifications" settings :test #'string=)) nil))))))

(deftest parse-empty-structures ()
  "Test parsing JSON with empty objects and arrays"
  (let ((content (load-test-json "empty-structures.json")))
    (let ((result (json:parse-string content)))
      (is (listp result))
      ;; Check empty object
      (let ((empty-obj (cdr (assoc "empty_object" result :test #'string=))))
        (is (listp empty-obj))
        (is (null empty-obj)))
      ;; Check empty array
      (let ((empty-arr (cdr (assoc "empty_array" result :test #'string=))))
        (is (listp empty-arr))
        (is (null empty-arr)))
      ;; Check null value
      (let ((null-val (cdr (assoc "null_value" result :test #'string=))))
        (is (eq null-val nil))))))

;; String escape tests

(deftest parse-string-escapes ()
  "Test parsing JSON strings with escape sequences"
  (is (string= (json:parse-string "\"\\\"quoted\\\"\"") "\"quoted\""))
  (is (string= (json:parse-string "\"line1\\nline2\"") (format nil "line1~%line2")))
  (is (string= (json:parse-string "\"tab\\there\"") (format nil "tab~There")))
  (is (string= (json:parse-string "\"back\\\\slash\"") "back\\slash")))

;; Number tests

(deftest parse-negative-number ()
  "Test parsing negative numbers"
  (is (= (json:parse-string "-42") -42)))

(deftest parse-decimal-number ()
  "Test parsing decimal numbers"
  (is (= (json:parse-string "3.14") 3.14)))

(deftest parse-exponential-number ()
  "Test parsing exponential notation"
  (is (= (json:parse-string "1e3") 1000.0))
  (is (= (json:parse-string "2.5e-1") 0.25)))

;; Whitespace tests

(deftest parse-with-whitespace ()
  "Test parsing JSON with various whitespace"
  (let ((result (json:parse-string "  {  \"key\"  :  \"value\"  }  ")))
    (is (listp result))
    (is (string= (cdr (assoc "key" result :test #'string=)) "value"))))

;; Error handling tests

(deftest parse-invalid-json-fails ()
  "Test that invalid JSON throws appropriate errors"
  (is-thrown-p (error) (json:parse-string "{"))
  (is-thrown-p (error) (json:parse-string "invalid"))
  (is-thrown-p (error) (json:parse-string "{key: value}"))  ; unquoted key
  (is-thrown-p (error) (json:parse-string "{'key': 'value'}")))  ; single quotes
