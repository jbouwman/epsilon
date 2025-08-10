(defpackage :epsilon.tool.format.tests
  (:use
   :cl
   :epsilon.test)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:fmt #:epsilon.tool.format)
   (#:map #:epsilon.map)
   (#:seq #:epsilon.sequence)
   (#:str #:epsilon.string)
   (#:test #:epsilon.test)
   (#:path #:epsilon.path)))

(in-package :epsilon.tool.format.tests)

;; Extend format.lisp to support JSON output for epsilon maps and sequences

(defmethod fmt:to-ir ((object map::hamt))
  "Convert epsilon map to JSON object IR"
  (let ((pairs '()))
    (map:reduce (lambda (acc key value)
                  (declare (ignore acc))
                  (push (fmt:make-ir-pair
                         (fmt:to-ir (if (stringp key) key (format nil "~a" key)))
                         (fmt:to-ir value))
                        pairs)
                  nil)
                object)
    (fmt:make-ir-object (nreverse pairs))))

;; Add to-ir methods for sequences and lists

(defmethod fmt:to-ir ((object list))
  "Convert list to JSON array IR"
  (fmt:make-ir-array (mapcar #'fmt:to-ir object)))

(defmethod fmt:to-ir ((object vector))
  "Convert vector to JSON array IR"
  (fmt:make-ir-array (map 'list #'fmt:to-ir object)))

;; Add to-ir methods for epsilon.sequence

(defmethod fmt:to-ir ((object seq:cons))
  "Convert epsilon sequence to JSON array IR"
  (fmt:make-ir-array (mapcar #'fmt:to-ir (seq:realize object))))

;; Primitive type methods

(defmethod fmt:to-ir ((object string))
  "Convert string to JSON string IR"
  (fmt:make-ir-primitive object))

(defmethod fmt:to-ir ((object number))
  "Convert number to JSON number IR"
  (fmt:make-ir-primitive object))

(defmethod fmt:to-ir ((object symbol))
  "Convert symbol to JSON based on type"
  (cond
    ((eq object t) (fmt:make-ir-primitive t))
    ((eq object nil) (fmt:make-ir-primitive nil))
    ((eq object :null) (fmt:make-ir-primitive nil))  ; JSON null
    (t (fmt:make-ir-primitive (string-downcase (symbol-name object))))))


;; Test helper functions

(defun format-to-string (object &key (indent-width 2) (line-limit 80) (format-style :expanded))
  "Format object to string using the pretty printer"
  (with-output-to-string (stream)
    (fmt:format-object stream object
                       :indent-width indent-width 
                       :line-limit line-limit
                       :format-style format-style)))

(defun tokenize (object &key (indent-width 2) (format-style :expanded))
  "Tokenize an object using the pretty printer"
  (fmt:tokenize object
                :indent-width indent-width 
                :format-style format-style))

(defun load-test-resource (filename)
  "Load the contents of a test resource file"
  (let ((path (module-file "epsilon.format" (path:string-path-join "tests/format" filename))))
    (fs:read-file path)))

(defun find-string-difference (str1 str2)
  "Find the first character position where two strings differ. 
   Returns NIL if strings are equal, or (values position char1 char2) for first difference."
  (let ((len1 (length str1))
        (len2 (length str2))
        (min-len (min (length str1) (length str2))))
    (loop for i from 0 below min-len
          when (char/= (char str1 i) (char str2 i))
            do (return-from find-string-difference 
                 (values i (char str1 i) (char str2 i))))
    ;; If one string is a prefix of the other
    (when (/= len1 len2)
      (values min-len 
              (if (> len1 len2) (char str1 min-len) nil)
              (if (> len2 len1) (char str2 min-len) nil)))))

(defun assert-strings-equal (actual expected &optional message)
  "Assert that two strings are equal, showing character difference offset if they differ"
  (multiple-value-bind (pos char1 char2) (find-string-difference actual expected)
    (when pos
      (let ((context-start (max 0 (- pos 20)))
            (context-end (min (length expected) (+ pos 20))))
        (error "String mismatch at position ~d~@[: ~a~]~%Expected: ~a~%Actual:   ~a~%Context around position ~d:~%Expected: ~s~%Actual:   ~s"
               pos message
               (or char2 "EOF") (or char1 "EOF")
               pos
               (subseq expected context-start context-end)
               (subseq actual context-start (min (length actual) context-end)))))))

(defun make-test-data ()
  "Create test data structure with epsilon maps and sequences"
  (let ((users-map (map:assoc (map:assoc map:+empty+ 
                                         "users" 
                                         (list (map:make-map "id" 1
                                                             "name" "John Doe"
                                                             "email" "john@example.com"
                                                             "active" t
                                                             "tags" #("admin" "developer"))
                                               (map:make-map "id" 2
                                                             "name" "Jane Smith"
                                                             "email" "jane@example.com"
                                                             "active" nil
                                                             "tags" #("user"))))
                              "metadata"
                              (map:make-map "total" 2
                                            "page" 1
                                            "limit" 10))))
    users-map))

;; Test suite
(deftest test-simple-map
  "Test formatting a simple epsilon map as JSON"
  (let ((simple-map (map:make-map "name" "test" "value" 42))
        (expected (load-test-resource "simple-map.json")))
    (let ((result (format-to-string simple-map :line-limit 40)))
      (assert-strings-equal result expected "Simple map formatting"))))

(deftest test-simple-list
  "Test formatting a simple list as JSON array"
  (let ((simple-list '(1 2 3 "hello" t nil))
        (expected (load-test-resource "simple-list.json")))
    (let ((result (format-to-string simple-list :line-limit 40)))
      (assert-strings-equal result expected "Simple list formatting"))))

(deftest test-nested-structures
  "Test formatting nested epsilon maps and lists"
  (let ((data (make-test-data))
        (expected (load-test-resource "nested-structures.json")))
    (let ((result (format-to-string data :line-limit 80)))
      (assert-strings-equal result expected "Nested structures formatting"))))

(deftest test-compact-vs-expanded
  "Test explicit compact vs expanded formatting modes"
  (let ((data (map:make-map "name" "test" "value" 42)))
    (let ((compact-result (format-to-string data :format-style :compact))
          (expanded-result (format-to-string data :format-style :expanded)))
      (is (= (count #\Newline compact-result) 0)
          "Compact should have no newlines")
      (is (> (count #\Newline expanded-result) 0)
          "Expanded should have newlines"))))

(deftest test-vector-formatting
"Test that vectors format as JSON arrays"
  (let* ((vec #(1 "two" 3.14 t nil))
         (expected (load-test-resource "vector-formatting.json"))
         (result (format-to-string vec)))
    (assert-strings-equal result expected "Vector formatting")))

(deftest test-indentation
  "Test proper indentation in expanded layout"
  (let ((data (make-test-data)))
    (let ((result (format-to-string data :indent-width 4 :line-limit 40)))
      ;; Check that indentation increases inside objects/arrays
      (let ((lines (loop for line in (seq:realize (str:split #\Newline result))
                         collect line)))
        (is (some (lambda (line) 
                    (and (> (length line) 4)
                         (every (lambda (c) (char= c #\Space)) 
                                (subseq line 0 4))))
                  lines)
            "Should have 4-space indented lines")))))

(deftest test-epsilon-sequence
  "Test formatting epsilon sequences as JSON arrays"
  (let ((seq-data (seq:cons 1 (seq:cons "hello" (seq:cons t seq:*empty*)))))
    (let ((result (format-to-string seq-data)))
      (is (search "[" result) "Should contain opening bracket")
      (is (search "]" result) "Should contain closing bracket")
      (is (search "1" result) "Should contain number")
      (is (search "\"hello\"" result) "Should contain quoted string")
      (is (search "true" result) "Should convert T to true"))))
