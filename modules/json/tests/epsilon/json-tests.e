(package epsilon.json-tests
  (use epsilon.syntax epsilon.test)
  (import (epsilon.sys.fs fs)
          (epsilon.json json)
          (epsilon.map map)
          (epsilon.parser p)
          (epsilon.path path)))

;; Helper function to load test JSON files

(defun load-test-json (filename)
  "Load a JSON test file from tests/lib/json/"
  (->> filename
       (path:string-path-join "tests/json")
       (module-file "epsilon.json")
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
  "Test parsing JSON arrays - arrays decode to vectors"
  (let ((content (load-test-json "simple-array.json")))
    (let ((result (json:parse content)))
      (is (vectorp result))
      (is (= (length result) 4))
      (is (= (aref result 0) 1))
      (is (= (aref result 1) 2))
      (is (= (aref result 2) 3))
      (is (string= (aref result 3) "hello"))))
  ;; Empty array
  (is (equalp (json:parse "[]") #()))
  ;; Simple arrays
  (is (equalp (json:parse "[1, 2, 3]") #(1 2 3)))
  (is (equalp (json:parse "[\"a\", \"b\", \"c\"]") #("a" "b" "c")))
  (is (equalp (json:parse "[1, \"two\", true, null]") #(1 "two" t nil))))

;;;; Object tests

(deftest parse-object
  "Test parsing JSON objects - objects decode to epsilon.maps"
  (let ((content (load-test-json "simple-object.json")))
    (let ((result (json:parse content)))
      (is (map:map-p result))
      (is (string= (map:get result "name") "John"))
      (is (= (map:get result "age") 30))
      (is (eq (map:get result "active") t))))
  ;; Empty object
  (is (map:map-p (json:parse "{}")))
  (is (zerop (map:size (json:parse "{}"))))
  ;; Simple objects
  (let ((obj (json:parse "{\"name\": \"John\", \"age\": 30}")))
    (is (map:map-p obj))
    (is (equal (map:get obj "name") "John"))
    (is (= (map:get obj "age") 30)))
  ;; Nested object
  (let ((obj (json:parse "{\"nested\": {\"key\": \"value\"}}")))
    (is (map:map-p obj))
    (let ((nested (map:get obj "nested")))
      (is (map:map-p nested))
      (is (equal (map:get nested "key") "value")))))

(deftest parse-nested-object
  "Test parsing nested JSON objects"
  (let ((content (load-test-json "nested-object.json")))
    (let ((result (json:parse content)))
      (is (map:map-p result))
      ;; Check user object
      (let ((user (map:get result "user")))
        (is (map:map-p user))
        (is (string= (map:get user "name") "Alice"))
        ;; Check nested details
        (let ((details (map:get user "details")))
          (is (map:map-p details))
          (is (= (map:get details "age") 25))
          (let ((hobbies (map:get details "hobbies")))
            (is (vectorp hobbies))
            (is-= (length hobbies) 2)
            (is (string= (aref hobbies 0) "reading"))
            (is (string= (aref hobbies 1) "coding")))))
      ;; Check settings object
      (let ((settings (map:get result "settings")))
        (is (map:map-p settings))
        (is (string= (map:get settings "theme") "dark"))
        (is (eq (map:get settings "notifications") nil))))))

(deftest parse-empty-structures
  "Test parsing JSON with empty objects and arrays"
  (let ((content (load-test-json "empty-structures.json")))
    (let ((result (json:parse content)))
      (is (map:map-p result))
      ;; Check empty object
      (let ((empty-obj (map:get result "empty_object")))
        (is (map:map-p empty-obj))
        (is (zerop (map:size empty-obj))))
      ;; Check empty array - now a vector
      (let ((empty-arr (map:get result "empty_array")))
        (is (vectorp empty-arr))
        (is (zerop (length empty-arr))))
      ;; Check null value
      (let ((null-val (map:get result "null_value")))
        (is-eq null-val nil)))))

;;;; Whitespace tests

(deftest parse-whitespace
  "Test parsing JSON with various whitespace"
  (let ((result (json:parse "  {  \"key\"  :  \"value\"  }  ")))
    (is (map:map-p result))
    (is-equal (map:get result "key") "value"))
  (is (equalp (json:parse "[ 1 , 2 , 3 ]") #(1 2 3))))

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
      (is (map:map-p result) "Should be an epsilon.map")
      (is-equal (map:get result "name") "Alice")
      (is-equal (map:get result "age") 25)
      (is-equal (map:get result "active") t))))

;;;; Error handling tests

(deftest parse-invalid-json
  "Test that invalid JSON throws appropriate errors"
  (is-thrown (error) (json:parse "{"))
  (is-thrown (error) (json:parse "invalid"))
  (is-thrown (error) (json:parse "{key: value}"))  ; unquoted key
  (is-thrown (error) (json:parse "{'key': 'value'}")))  ; single quotes
