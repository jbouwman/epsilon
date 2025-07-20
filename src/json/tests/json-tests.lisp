(defpackage :epsilon.json.tests
  (:use
   :cl
   :epsilon.syntax
   :epsilon.test)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:json #:epsilon.json)
   (#:p #:epsilon.parser)
   (#:path #:epsilon.path)))

(in-package :epsilon.json.tests)

;; Helper function to load test JSON files
;;
;; TODO convert to URLs

(defun load-test-json (filename)
  "Load a JSON test file from tests/lib/json/"
  (->> filename
       (path:string-path-join "tests/json")
       (project-file "epsilon.json")
       fs:read-file))

;;;; Basic value tests

(deftest parse-string
  "Test parsing JSON strings"
  (let ((content (load-test-json "simple-string.json")))
    (is-equal (json:parse content) "hello world"))
  (is-equal (json:parse "\"hello\"") "hello")
  (is-equal (json:parse "\"\"") ""))

(deftest parse-string-escapes
  "Test parsing JSON strings with escape sequences"
  (is-equal (json:parse "\"\\\"quoted\\\"\"") "\"quoted\"")
  (is-equal (json:parse "\"line1\\nline2\"") (format nil "line1~%line2"))
  (is-equal (json:parse "\"tab\\there\"") (format nil "tab	here"))
  (is-equal (json:parse "\"back\\\\slash\"") "back\\slash"))

(deftest parse-number
  "Test parsing JSON numbers"
  (let ((content (load-test-json "simple-number.json")))
    (is-= (json:parse content) 42))
  (is-equal (json:parse "42") 42)
  (is-equal (json:parse "-42") -42)
  (is-equal (json:parse "3.14") 3.14)
  (is-equal (json:parse "1.23e4") 1.23e4)
  (is-equal (json:parse "1e3") 1000.0)
  (is-equal (json:parse "2.5e-1") 0.25)
  (is-equal (json:parse "0") 0))

(deftest parse-literals
  "Test parsing JSON literal values"
  (is-eq (json:parse "true") t)
  (is-eq (json:parse "false") nil)
  (is-eq (json:parse "null") nil))

;;;; Array tests

(deftest parse-array
  "Test parsing JSON arrays"
  (let ((content (load-test-json "simple-array.json")))
    (let ((result (json:parse content)))
      (is (listp result))
      (is (= (length result) 4))
      (is (= (first result) 1))
      (is (= (second result) 2))
      (is (= (third result) 3))
      (is (string= (fourth result) "hello"))))
  ;; Empty array
  (is-equal (json:parse "[]") '())
  ;; Simple arrays
  (is-equal (json:parse "[1, 2, 3]") '(1 2 3))
  (is-equal (json:parse "[\"a\", \"b\", \"c\"]") '("a" "b" "c"))
  (is-equal (json:parse "[1, \"two\", true, null]") '(1 "two" t nil)))

;;;; Object tests

(deftest parse-object
  "Test parsing JSON objects"
  (let ((content (load-test-json "simple-object.json")))
    (let ((result (json:parse content)))
      (is (listp result))
      (is (string= (cdr (assoc "name" result :test #'string=)) "John"))
      (is (= (cdr (assoc "age" result :test #'string=)) 30))
      (is (eq (cdr (assoc "active" result :test #'string=)) t))))
  ;; Empty object
  (is-equal (json:parse "{}") '())
  ;; Simple objects
  (is-equal (json:parse "{\"name\": \"John\", \"age\": 30}")
            '(("name" . "John") ("age" . 30)))
  (is-equal (json:parse "{\"nested\": {\"key\": \"value\"}}")
            '(("nested" . (("key" . "value"))))))

(deftest parse-nested-object
  "Test parsing nested JSON objects"
  (let ((content (load-test-json "nested-object.json")))
    (let ((result (json:parse content)))
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
            (is-= (length hobbies) 2)
            (is (string= (first hobbies) "reading"))
            (is (string= (second hobbies) "coding")))))
      ;; Check settings object
      (let ((settings (cdr (assoc "settings" result :test #'string=))))
        (is (listp settings))
        (is (string= (cdr (assoc "theme" settings :test #'string=)) "dark"))
        (is (eq (cdr (assoc "notifications" settings :test #'string=)) nil))))))

(deftest parse-empty-structures
  "Test parsing JSON with empty objects and arrays"
  (let ((content (load-test-json "empty-structures.json")))
    (let ((result (json:parse content)))
      (is (typep result 'list))
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
        (is-eq null-val nil)))))

;;;; Whitespace tests

(deftest parse-whitespace
  "Test parsing JSON with various whitespace"
  (let ((result (json:parse "  {  \"key\"  :  \"value\"  }  ")))
    (is (typep result 'list))
    (is-equal (cdr (assoc "key" result :test #'string=)) "value"))
  (is-equal (json:parse "[ 1 , 2 , 3 ]") '(1 2 3)))

;;;; Complex structure tests

(deftest parse-complex
  "Test parsing complex JSON structures"
  (let ((json-str "{
    \"name\": \"Alice\",
    \"age\": 25,
    \"hobbies\": [\"reading\", \"coding\"],
    \"address\": {
      \"street\": \"123 Main St\",
      \"city\": \"Anytown\"
    },
    \"active\": true
  }"))
    (let ((result (json:parse json-str)))
      (is (listp result) "Should be an object (alist)")
      (is-equal (cdr (assoc "name" result :test #'string=)) "Alice")
      (is-equal (cdr (assoc "age" result :test #'string=)) 25)
      (is-equal (cdr (assoc "active" result :test #'string=)) t))))

;;;; Error handling tests

(deftest parse-invalid-json
  "Test that invalid JSON throws appropriate errors"
  (is-thrown (error) (json:parse "{"))
  (is-thrown (error) (json:parse "invalid"))
  (is-thrown (error) (json:parse "{key: value}"))  ; unquoted key
  (is-thrown (error) (json:parse "{'key': 'value'}")))  ; single quotes
