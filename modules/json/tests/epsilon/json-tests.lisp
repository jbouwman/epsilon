(defpackage epsilon.json-tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:require (epsilon.file fs)
            (epsilon.json json)
            (epsilon.map map)
            (epsilon.parser p)
            (epsilon.path path))
  (:enter t))

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
    (assert-equal (json:parse content) "hello world"))
  (assert-equal (json:parse "\"hello\"") "hello")
  (assert-equal (json:parse "\"\"") ""))

(deftest parse-string-escapes
  "Test parsing JSON strings with escape sequences"
  (assert-equal (json:parse "\"\\\"quoted\\\"\"") "\"quoted\"")
  (assert-equal (json:parse "\"line1\\nline2\"") (format nil "line1~%line2"))
  (assert-equal (json:parse "\"tab\\there\"") (format nil "tab	here"))
  (assert-equal (json:parse "\"back\\\\slash\"") "back\\slash"))

(deftest parse-number
  "Test parsing JSON numbers"
  (let ((content (load-test-json "simple-number.json")))
    (assert-= (json:parse content) 42))
  (assert-equal (json:parse "42") 42)
  (assert-equal (json:parse "-42") -42)
  (assert-equal (json:parse "3.14") 3.14)
  (assert-equal (json:parse "1.23e4") 1.23e4)
  (assert-equal (json:parse "1e3") 1000.0)
  (assert-equal (json:parse "2.5e-1") 0.25)
  (assert-equal (json:parse "0") 0))

(deftest parse-literals
  "Test parsing JSON literal values"
  (assert-eq (json:parse "true") t)
  (assert-eq (json:parse "false") nil)
  (assert-eq (json:parse "null") nil))

;;;; Array tests

(deftest parse-array
  "Test parsing JSON arrays - arrays decode to vectors"
  (let ((content (load-test-json "simple-array.json")))
    (let ((result (json:parse content)))
      (assert-true (vectorp result))
      (assert-true (= (length result) 4))
      (assert-true (= (aref result 0) 1))
      (assert-true (= (aref result 1) 2))
      (assert-true (= (aref result 2) 3))
      (assert-true (string= (aref result 3) "hello"))))
  ;; Empty array
  (assert-true (equalp (json:parse "[]") #()))
  ;; Simple arrays
  (assert-true (equalp (json:parse "[1, 2, 3]") #(1 2 3)))
  (assert-true (equalp (json:parse "[\"a\", \"b\", \"c\"]") #("a" "b" "c")))
  (assert-true (equalp (json:parse "[1, \"two\", true, null]") #(1 "two" t nil))))

;;;; Object tests

(deftest parse-object
  "Test parsing JSON objects - objects decode to epsilon.maps"
  (let ((content (load-test-json "simple-object.json")))
    (let ((result (json:parse content)))
      (assert-true (map:map-p result))
      (assert-true (string= (map:get result "name") "John"))
      (assert-true (= (map:get result "age") 30))
      (assert-true (eq (map:get result "active") t))))
  ;; Empty object
  (assert-true (map:map-p (json:parse "{}")))
  (assert-true (zerop (map:size (json:parse "{}"))))
  ;; Simple objects
  (let ((obj (json:parse "{\"name\": \"John\", \"age\": 30}")))
    (assert-true (map:map-p obj))
    (assert-true (equal (map:get obj "name") "John"))
    (assert-true (= (map:get obj "age") 30)))
  ;; Nested object
  (let ((obj (json:parse "{\"nested\": {\"key\": \"value\"}}")))
    (assert-true (map:map-p obj))
    (let ((nested (map:get obj "nested")))
      (assert-true (map:map-p nested))
      (assert-true (equal (map:get nested "key") "value")))))

(deftest parse-nested-object
  "Test parsing nested JSON objects"
  (let ((content (load-test-json "nested-object.json")))
    (let ((result (json:parse content)))
      (assert-true (map:map-p result))
      ;; Check user object
      (let ((user (map:get result "user")))
        (assert-true (map:map-p user))
        (assert-true (string= (map:get user "name") "Alice"))
        ;; Check nested details
        (let ((details (map:get user "details")))
          (assert-true (map:map-p details))
          (assert-true (= (map:get details "age") 25))
          (let ((hobbies (map:get details "hobbies")))
            (assert-true (vectorp hobbies))
            (assert-= (length hobbies) 2)
            (assert-true (string= (aref hobbies 0) "reading"))
            (assert-true (string= (aref hobbies 1) "coding")))))
      ;; Check settings object
      (let ((settings (map:get result "settings")))
        (assert-true (map:map-p settings))
        (assert-true (string= (map:get settings "theme") "dark"))
        (assert-true (eq (map:get settings "notifications") nil))))))

(deftest parse-empty-structures
  "Test parsing JSON with empty objects and arrays"
  (let ((content (load-test-json "empty-structures.json")))
    (let ((result (json:parse content)))
      (assert-true (map:map-p result))
      ;; Check empty object
      (let ((empty-obj (map:get result "empty_object")))
        (assert-true (map:map-p empty-obj))
        (assert-true (zerop (map:size empty-obj))))
      ;; Check empty array - now a vector
      (let ((empty-arr (map:get result "empty_array")))
        (assert-true (vectorp empty-arr))
        (assert-true (zerop (length empty-arr))))
      ;; Check null value
      (let ((null-val (map:get result "null_value")))
        (assert-eq null-val nil)))))

;;;; Whitespace tests

(deftest parse-whitespace
  "Test parsing JSON with various whitespace"
  (let ((result (json:parse "  {  \"key\"  :  \"value\"  }  ")))
    (assert-true (map:map-p result))
    (assert-equal (map:get result "key") "value"))
  (assert-true (equalp (json:parse "[ 1 , 2 , 3 ]") #(1 2 3))))

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
      (assert-true (map:map-p result) "Should be an epsilon.map")
      (assert-equal (map:get result "name") "Alice")
      (assert-equal (map:get result "age") 25)
      (assert-equal (map:get result "active") t))))

;;;; Error handling tests

(deftest parse-invalid-json
  "Test that invalid JSON throws appropriate errors"
  (assert-condition (error) (json:parse "{"))
  (assert-condition (error) (json:parse "invalid"))
  (assert-condition (error) (json:parse "{key: value}"))  ; unquoted key
  (assert-condition (error) (json:parse "{'key': 'value'}")))  ; single quotes

;;;; Object hook tests

(deftest parse-with-object-hook
  "Test parsing with object-hook for custom deserialization"
  ;; Simple hook that adds a marker
  (let ((result (json:parse "{\"name\": \"Alice\"}"
                            :object-hook (lambda (obj)
                                           (list :converted (map:get obj "name"))))))
    (assert-true (listp result))
    (assert-eq (first result) :converted)
    (assert-equal (second result) "Alice"))
  ;; Nested objects should all get transformed
  (let ((result (json:parse "{\"outer\": {\"inner\": \"value\"}}"
                            :object-hook (lambda (obj)
                                           (if (map:get obj "inner")
                                               :inner-found
                                               obj)))))
    (assert-true (map:map-p result))
    (assert-eq (map:get result "outer") :inner-found)))

;;;; JSON Lines tests

(deftest parse-jsonl
  "Test parsing JSON Lines format"
  (let ((lines (epsilon.sequence:realize
                (json:parse-lines "{\"a\":1}
{\"b\":2}
{\"c\":3}"))))
    (assert-= (length lines) 3)
    (assert-true (map:map-p (first lines)))
    (assert-= (map:get (first lines) "a") 1)
    (assert-= (map:get (second lines) "b") 2)
    (assert-= (map:get (third lines) "c") 3))
  ;; Empty lines should be skipped
  (let ((lines (epsilon.sequence:realize
                (json:parse-lines "{\"a\":1}

{\"b\":2}"))))
    (assert-= (length lines) 2)))

(deftest encode-jsonl
  "Test encoding to JSON Lines format"
  (let ((result (json:encode-lines-to-string
                 (list (map:assoc map:+empty+ "a" 1)
                       (map:assoc map:+empty+ "b" 2)))))
    (assert-true (stringp result))
    ;; Should contain two lines
    (assert-true (search "{\"a\":1}" result))
    (assert-true (search "{\"b\":2}" result))))

;;;; Boolean encoding tests

(deftest encode-false-keyword
  "The :false keyword encodes as JSON false."
  (assert-equal "false" (json:encode-to-string :false))
  ;; Inside an object
  (let ((obj (map:assoc map:+empty+ "enabled" :false)))
    (assert-equal "{\"enabled\":false}" (json:encode-to-string obj)))
  ;; Inside an array
  (assert-equal "[true,false,null]"
                (json:encode-to-string (vector t :false nil))))

;;;; Default encoder tests

(deftest encode-with-default
  "Test encoding with custom default encoder"
  ;; Custom type should use default encoder
  (let ((result (json:encode-to-string
                 (list :keyword "value")
                 :default (lambda (v)
                            (if (keywordp v)
                                (symbol-name v)
                                v)))))
    (assert-true (stringp result))
    (assert-true (search "KEYWORD" result)))
  ;; Nested values should also use default
  (let ((result (json:encode-to-string
                 (map:assoc map:+empty+ "key" :value)
                 :default (lambda (v)
                            (if (keywordp v)
                                (symbol-name v)
                                v)))))
    (assert-true (search "VALUE" result))))

;;;; JSON Pointer tests (RFC 6901)

(deftest json-pointer-get
  "Test JSON Pointer retrieval"
  (let ((doc (json:parse "{\"users\": [{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]}")))
    ;; Root pointer
    (assert-true (map:map-p (json:get-pointer doc "")))
    ;; Object key
    (assert-true (vectorp (json:get-pointer doc "/users")))
    ;; Array index
    (assert-true (map:map-p (json:get-pointer doc "/users/0")))
    ;; Nested path
    (assert-equal (json:get-pointer doc "/users/0/name") "Alice")
    (assert-equal (json:get-pointer doc "/users/1/name") "Bob")))

(deftest json-pointer-set
  "Test JSON Pointer setting"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (updated (json:set-pointer doc "/name" "Bob")))
    (assert-equal (json:get-pointer updated "/name") "Bob")
    ;; Original unchanged (immutable)
    (assert-equal (json:get-pointer doc "/name") "Alice"))
  ;; Set nested value
  (let* ((doc (json:parse "{\"user\": {\"name\": \"Alice\"}}"))
         (updated (json:set-pointer doc "/user/name" "Bob")))
    (assert-equal (json:get-pointer updated "/user/name") "Bob")))

(deftest json-pointer-exists
  "Test JSON Pointer existence check"
  (let ((doc (json:parse "{\"a\": {\"b\": 1}}")))
    (assert-true (json:pointer-exists-p doc "/a"))
    (assert-true (json:pointer-exists-p doc "/a/b"))
    (assert-true (not (json:pointer-exists-p doc "/c")))
    (assert-true (not (json:pointer-exists-p doc "/a/c")))))

(deftest json-pointer-escape
  "Test JSON Pointer escape/unescape"
  ;; Tilde and slash must be escaped
  (let ((doc (json:parse "{\"a/b\": 1, \"c~d\": 2}")))
    (assert-= (json:get-pointer doc "/a~1b") 1)
    (assert-= (json:get-pointer doc "/c~0d") 2)))

;;;; JSON Patch tests (RFC 6902)

(deftest json-patch-add
  "Test JSON Patch add operation"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (patch (vector (json:parse "{\"op\": \"add\", \"path\": \"/age\", \"value\": 30}")))
         (result (json:apply-patch doc patch)))
    (assert-= (json:get-pointer result "/age") 30)
    (assert-equal (json:get-pointer result "/name") "Alice")))

(deftest json-patch-remove
  "Test JSON Patch remove operation"
  (let* ((doc (json:parse "{\"name\": \"Alice\", \"age\": 30}"))
         (patch (vector (json:parse "{\"op\": \"remove\", \"path\": \"/age\"}")))
         (result (json:apply-patch doc patch)))
    (assert-true (not (json:pointer-exists-p result "/age")))
    (assert-equal (json:get-pointer result "/name") "Alice")))

(deftest json-patch-replace
  "Test JSON Patch replace operation"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (patch (vector (json:parse "{\"op\": \"replace\", \"path\": \"/name\", \"value\": \"Bob\"}")))
         (result (json:apply-patch doc patch)))
    (assert-equal (json:get-pointer result "/name") "Bob")))

(deftest json-patch-move
  "Test JSON Patch move operation"
  (let* ((doc (json:parse "{\"first\": \"Alice\", \"last\": \"Smith\"}"))
         (patch (vector (json:parse "{\"op\": \"move\", \"from\": \"/first\", \"path\": \"/name\"}")))
         (result (json:apply-patch doc patch)))
    (assert-equal (json:get-pointer result "/name") "Alice")
    (assert-true (not (json:pointer-exists-p result "/first")))))

(deftest json-patch-copy
  "Test JSON Patch copy operation"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (patch (vector (json:parse "{\"op\": \"copy\", \"from\": \"/name\", \"path\": \"/alias\"}")))
         (result (json:apply-patch doc patch)))
    (assert-equal (json:get-pointer result "/name") "Alice")
    (assert-equal (json:get-pointer result "/alias") "Alice")))

(deftest json-patch-test
  "Test JSON Patch test operation"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (patch-pass (vector (json:parse "{\"op\": \"test\", \"path\": \"/name\", \"value\": \"Alice\"}")))
         (patch-fail (vector (json:parse "{\"op\": \"test\", \"path\": \"/name\", \"value\": \"Bob\"}"))))
    ;; Should pass without error
    (assert-true (map:map-p (json:apply-patch doc patch-pass)))
    ;; Should fail with error
    (assert-condition (error) (json:apply-patch doc patch-fail))))

(deftest json-patch-multiple
  "Test applying multiple patch operations"
  (let* ((doc (json:parse "{\"name\": \"Alice\"}"))
         (patch (vector
                 (json:parse "{\"op\": \"add\", \"path\": \"/age\", \"value\": 30}")
                 (json:parse "{\"op\": \"add\", \"path\": \"/city\", \"value\": \"NYC\"}")
                 (json:parse "{\"op\": \"replace\", \"path\": \"/name\", \"value\": \"Bob\"}")))
         (result (json:apply-patch doc patch)))
    (assert-equal (json:get-pointer result "/name") "Bob")
    (assert-= (json:get-pointer result "/age") 30)
    (assert-equal (json:get-pointer result "/city") "NYC")))
