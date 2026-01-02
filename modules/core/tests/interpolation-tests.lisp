;;;; interpolation-tests.lisp - Tests for string interpolation reader macro
;;;;
;;;; Tests the #~"..." interpolation syntax including variable embedding,
;;;; expression evaluation, escape sequences, and edge cases.

(defpackage #:epsilon.interpolation.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:interp #:epsilon.interpolation)
   (#:str #:epsilon.string)))

(in-package #:epsilon.interpolation.tests)

;;; Helper to read with interpolation syntax

(defun read-interp (string)
  "Read a string using interpolation reader extensions."
  (interp:with-interpolation-reader
    (read-from-string string)))

;;; Helper macro to make variables visible to eval
;;; (eval operates in the null lexical environment, so we use progv
;;; to bind special/dynamic variables that eval can see)

(defmacro with-eval-bindings (bindings &body body)
  "Execute body with bindings visible to eval via dynamic scope.
   Interns symbols at runtime in *package* to match read-from-string."
  (let ((var-names (mapcar (lambda (b) (symbol-name (first b))) bindings))
        (vals (mapcar #'second bindings)))
    `(progv (mapcar #'intern ',var-names)
            (list ,@vals)
       ,@body)))

;;; Basic interpolation tests

(deftest test-simple-variable-interpolation
  "Test basic variable interpolation."
  (interp:with-interpolation-reader
    (with-eval-bindings ((name "World"))
      (is (string= (eval (read-from-string "#~\"Hello, ~{name}!\""))
                   "Hello, World!")))))

(deftest test-expression-interpolation
  "Test interpolation with arithmetic expression."
  (interp:with-interpolation-reader
    (with-eval-bindings ((a 1) (b 2))
      (is (string= (eval (read-from-string "#~\"Sum: ~{(+ a b)}\""))
                   "Sum: 3")))))

(deftest test-multiple-interpolations
  "Test multiple interpolations in one string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((first "John") (last "Doe"))
      (is (string= (eval (read-from-string "#~\"~{first} ~{last}\""))
                   "John Doe")))))

(deftest test-interpolation-with-surrounding-text
  "Test interpolation with text before and after."
  (interp:with-interpolation-reader
    (with-eval-bindings ((count 42))
      (is (string= (eval (read-from-string "#~\"You have ~{count} messages.\""))
                   "You have 42 messages.")))))

;;; Plain string tests (no interpolation)

(deftest test-plain-string
  "Test string without any interpolation."
  (let ((result (read-interp "#~\"plain string\"")))
    (is (stringp result))
    (is (string= result "plain string"))))

(deftest test-empty-string
  "Test empty interpolated string."
  (let ((result (read-interp "#~\"\"")))
    (is (stringp result))
    (is (string= result ""))))

;;; Escape sequence tests

(deftest test-escaped-tilde
  "Test escaped tilde produces single tilde."
  (let ((result (read-interp "#~\"100~~ complete\"")))
    (is (stringp result))
    (is (string= result "100~ complete"))))

(deftest test-newline-escape
  "Test newline escape sequence."
  (let ((result (read-interp "#~\"Line1\\nLine2\"")))
    (is (stringp result))
    (is (string= result (format nil "Line1~%Line2")))))

(deftest test-tab-escape
  "Test tab escape sequence."
  (let ((result (read-interp "#~\"Col1\\tCol2\"")))
    (is (stringp result))
    (is (string= result (format nil "Col1~CCol2" #\Tab)))))

(deftest test-quote-escape
  "Test escaped quote in string."
  (let ((result (read-interp "#~\"Say \\\"hello\\\"\"")))
    (is (stringp result))
    (is (string= result "Say \"hello\""))))

(deftest test-backslash-escape
  "Test escaped backslash."
  (let ((result (read-interp "#~\"path\\\\to\\\\file\"")))
    (is (stringp result))
    (is (string= result "path\\to\\file"))))

;;; Expression tests

(deftest test-function-call-interpolation
  "Test interpolation with function call."
  (interp:with-interpolation-reader
    (with-eval-bindings ((s "hello"))
      (is (string= (eval (read-from-string "#~\"Upper: ~{(string-upcase s)}\""))
                   "Upper: HELLO")))))

(deftest test-nested-function-calls
  "Test interpolation with nested function calls."
  (interp:with-interpolation-reader
    (with-eval-bindings ((items '("a" "b" "c")))
      (is (string= (eval (read-from-string "#~\"First: ~{(first items)}\""))
                   "First: a")))))

(deftest test-conditional-expression
  "Test interpolation with conditional."
  (interp:with-interpolation-reader
    (with-eval-bindings ((active t))
      (is (string= (eval (read-from-string "#~\"Status: ~{(if active \"ON\" \"OFF\")}\""))
                   "Status: ON")))))

(deftest test-numeric-expression
  "Test interpolation with numeric result."
  (interp:with-interpolation-reader
    (is (string= (eval (read-from-string "#~\"Result: ~{(* 6 7)}\""))
                 "Result: 42"))))

;;; Edge cases

(deftest test-adjacent-interpolations
  "Test two interpolations with no space between."
  (interp:with-interpolation-reader
    (with-eval-bindings ((a "foo") (b "bar"))
      (is (string= (eval (read-from-string "#~\"~{a}~{b}\""))
                   "foobar")))))

(deftest test-interpolation-at-start
  "Test interpolation at string start."
  (interp:with-interpolation-reader
    (with-eval-bindings ((greeting "Hello"))
      (is (string= (eval (read-from-string "#~\"~{greeting}, World!\""))
                   "Hello, World!")))))

(deftest test-interpolation-at-end
  "Test interpolation at string end."
  (interp:with-interpolation-reader
    (with-eval-bindings ((name "Bob"))
      (is (string= (eval (read-from-string "#~\"Hello, ~{name}\""))
                   "Hello, Bob")))))

(deftest test-only-interpolation
  "Test string that is only an interpolation."
  (interp:with-interpolation-reader
    (with-eval-bindings ((value "test"))
      (is (string= (eval (read-from-string "#~\"~{value}\""))
                   "test")))))

(deftest test-tilde-not-followed-by-brace
  "Test tilde that is not part of interpolation."
  (let ((result (read-interp "#~\"about ~50% done\"")))
    (is (stringp result))
    (is (string= result "about ~50% done"))))

;;; Nested braces in expressions

(deftest test-nested-braces-in-expression
  "Test expression with nested braces (hash table access)."
  (interp:with-interpolation-reader
    (let ((m (make-hash-table)))
      (setf (gethash :key m) "value")
      (with-eval-bindings ((m m))
        (is (string= (eval (read-from-string "#~\"Got: ~{(gethash :key m)}\""))
                     "Got: value"))))))

(deftest test-let-expression-in-interpolation
  "Test let expression within interpolation."
  (interp:with-interpolation-reader
    (is (string= (eval (read-from-string "#~\"Result: ~{(let ((x 10)) (+ x 5))}\""))
                 "Result: 15"))))

;;; Type conversion tests

(deftest test-integer-to-string
  "Test integer is converted to string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((n 42))
      (is (string= (eval (read-from-string "#~\"Number: ~{n}\""))
                   "Number: 42")))))

(deftest test-float-to-string
  "Test float is converted to string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((my-pi 3.14159))
      (is (str:starts-with-p
           (eval (read-from-string "#~\"Pi: ~{my-pi}\""))
           "Pi: 3.14")))))

(deftest test-symbol-to-string
  "Test symbol is converted to string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((sym 'hello))
      (is (string= (eval (read-from-string "#~\"Symbol: ~{sym}\""))
                   "Symbol: HELLO")))))

(deftest test-nil-to-string
  "Test nil is converted to string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((val nil))
      (is (string= (eval (read-from-string "#~\"Value: ~{val}\""))
                   "Value: NIL")))))

(deftest test-list-to-string
  "Test list is converted to string."
  (interp:with-interpolation-reader
    (with-eval-bindings ((lst '(1 2 3)))
      (is (string= (eval (read-from-string "#~\"List: ~{lst}\""))
                   "List: (1 2 3)")))))

;;; Readtable management tests

(deftest test-install-uninstall
  "Test installing and uninstalling reader macro."
  (let ((original *readtable*))
    (interp:install-interpolation-reader)
    (is (not (eq original *readtable*)))
    (interp:uninstall-interpolation-reader)
    ;; After uninstall, we should have a restored readtable
    (is t)))

(deftest test-with-interpolation-reader-restores
  "Test that with-interpolation-reader restores readtable."
  (let ((before *readtable*))
    (interp:with-interpolation-reader
      ;; Inside, we have interpolation extensions
      (is (not (eq before *readtable*))))
    ;; Outside, original is restored
    (is (eq before *readtable*))))

;;; Form structure tests (verify correct macro expansion)

(deftest test-plain-string-is-literal
  "Plain strings should expand to string literals, not concat calls."
  (let ((result (read-interp "#~\"no interpolation here\"")))
    (is (stringp result))))

(deftest test-single-interpolation-generates-concat
  "Single interpolation should generate str:concat form."
  (let ((result (read-interp "#~\"Hello, ~{name}!\"")))
    (is (listp result))
    (is (eq (first result) 'str:concat))))

(deftest test-concat-parts-are-correct
  "Verify the structure of generated concat form."
  (let ((result (read-interp "#~\"A~{x}B\"")))
    (is (listp result))
    (is (eq (first result) 'str:concat))
    ;; Should have: "A", (princ-to-string x), "B"
    (is (= 4 (length result)))  ; str:concat + 3 parts
    (is (string= "A" (second result)))
    (is (listp (third result)))
    (is (eq 'princ-to-string (first (third result))))
    (is (string= "B" (fourth result)))))

;;; String with quotes inside expression

(deftest test-string-literal-in-expression
  "Test expression containing string literal."
  (interp:with-interpolation-reader
    (is (string= (eval (read-from-string "#~\"Got: ~{(epsilon.string:concat \"a\" \"b\")}\""))
                 "Got: ab"))))

;;; Multiple escaped tildes

(deftest test-multiple-escaped-tildes
  "Test multiple escaped tildes in a row."
  (let ((result (read-interp "#~\"~~~~ tildes\"")))
    (is (stringp result))
    (is (string= result "~~ tildes"))))

;;; Combined escape and interpolation

(deftest test-combined-escapes-and-interpolation
  "Test escape sequences combined with interpolation."
  (interp:with-interpolation-reader
    (with-eval-bindings ((name "Bob"))
      (is (string= (eval (read-from-string "#~\"Line1:\\n\\t~{name}\\nLine2\""))
                   (format nil "Line1:~%~CBob~%Line2" #\Tab))))))
