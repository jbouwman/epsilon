;;;; reader-syntax-tests.lisp - Tests for position-aware Lisp reader
;;;;
;;;; Comprehensive tests covering atoms, compounds, Epsilon extensions,
;;;; trivia, span accuracy, roundtrip fidelity, error recovery, and
;;;; position conversion utilities.

(defpackage :epsilon.reader.tests
  (:use :cl :epsilon.reader)
  (:import (epsilon.test test)
           (epsilon.source-location loc)))

;;;; ======================================================================
;;;; Helpers
;;;; ======================================================================

(defun first-meaningful-node (nodes)
  "Return the first non-whitespace, non-comment node."
  (find-if (lambda (n)
             (not (member (syntax-node-kind n) '(:whitespace :comment))))
           nodes))

(defun meaningful-nodes (nodes)
  "Return all non-whitespace, non-comment nodes."
  (remove-if (lambda (n)
               (member (syntax-node-kind n) '(:whitespace :comment)))
             nodes))

(defun parse1 (source)
  "Parse SOURCE and return the first meaningful node."
  (first-meaningful-node (read-all-syntax source)))

(defun node-source-text (source node)
  "Extract the source text that NODE spans."
  (node-text source node))

;;;; ======================================================================
;;;; Integer Tests
;;;; ======================================================================

(test:deftest test-integer-positive ()
  "Parse a positive integer."
  (let* ((src "42")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-equal 42 (syntax-node-datum node))
    (test:assert-equal 0 (loc:source-location-offset (syntax-node-location node)))
    (test:assert-equal 2 (loc:source-location-end-offset (syntax-node-location node)))
    (test:assert-equal "42" (node-source-text src node))))

(test:deftest test-integer-negative ()
  "Parse a negative integer."
  (let* ((src "-7")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-equal -7 (syntax-node-datum node))
    (test:assert-equal "-7" (node-source-text src node))))

(test:deftest test-integer-zero ()
  "Parse zero."
  (let* ((src "0")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-equal 0 (syntax-node-datum node))))

;;;; ======================================================================
;;;; Float Tests
;;;; ======================================================================

(test:deftest test-float-simple ()
  "Parse a simple float."
  (let* ((src "3.14")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-true (floatp (syntax-node-datum node)))
    (test:assert-equal "3.14" (node-source-text src node))))

(test:deftest test-float-negative ()
  "Parse a negative float."
  (let* ((src "-0.5")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-true (floatp (syntax-node-datum node)))))

;;;; ======================================================================
;;;; Ratio Tests
;;;; ======================================================================

(test:deftest test-ratio ()
  "Parse a ratio."
  (let* ((src "1/3")
         (node (parse1 src)))
    (test:assert-equal :number (syntax-node-kind node))
    (test:assert-equal 1/3 (syntax-node-datum node))
    (test:assert-equal "1/3" (node-source-text src node))))

;;;; ======================================================================
;;;; String Tests
;;;; ======================================================================

(test:deftest test-string-simple ()
  "Parse a simple string."
  (let* ((src "\"hello\"")
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-equal "hello" (syntax-node-datum node))
    (test:assert-equal "\"hello\"" (node-source-text src node))))

(test:deftest test-string-empty ()
  "Parse an empty string."
  (let* ((src "\"\"")
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-equal "" (syntax-node-datum node))))

(test:deftest test-string-escapes ()
  "Parse a string with escape sequences."
  (let* ((src "\"a\\nb\\tc\"")
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-true (find #\Newline (syntax-node-datum node)))
    (test:assert-true (find #\Tab (syntax-node-datum node)))))

(test:deftest test-string-escaped-quote ()
  "Parse a string with escaped quotes."
  (let* ((src "\"say \\\"hi\\\"\"")
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-true (find #\" (syntax-node-datum node)))))

(test:deftest test-triple-quoted-string ()
  "A triple-quoted heredoc preserves its body verbatim, including
characters (commas, backslashes, single quotes) that would otherwise
need escaping or break a regular CL reader."
  (let* ((src "\"\"\"hello, world!\"\"\"")
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-equal "hello, world!" (syntax-node-datum node))))

(test:deftest test-triple-quoted-multiline ()
  "Triple-quoted strings span newlines and preserve embedded `\"`
characters as long as they're not three in a row."
  (let* ((src (format nil "\"\"\"line one~%line two with a \"quote\" inside~%line three\"\"\""))
         (node (parse1 src)))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-true (find #\Newline (syntax-node-datum node)))
    (test:assert-true (find #\" (syntax-node-datum node)))))

(test:deftest test-triple-quoted-empty ()
  "Six quotes in a row yields an empty triple-quoted string."
  (let* ((node (parse1 "\"\"\"\"\"\"")))
    (test:assert-equal :string (syntax-node-kind node))
    (test:assert-equal "" (syntax-node-datum node))))

;;;; ======================================================================
;;;; Symbol Tests
;;;; ======================================================================

(test:deftest test-symbol-bare ()
  "Parse a bare symbol."
  (let* ((src "foo")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal "FOO" (symbol-name (syntax-node-datum node)))
    (test:assert-equal "foo" (node-source-text src node))))

(test:deftest test-symbol-plus ()
  "Parse the + symbol."
  (let* ((src "+")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal '+ (syntax-node-datum node))))

(test:deftest test-symbol-hyphenated ()
  "Parse a hyphenated symbol."
  (let* ((src "my-func")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal "MY-FUNC" (symbol-name (syntax-node-datum node)))))

(test:deftest test-symbol-external-qualified ()
  "Parse pkg:name as the external symbol NAME in package PKG, not as a
   symbol literally named PKG:NAME in the current package."
  (let* ((src "common-lisp:format")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal 'cl:format (syntax-node-datum node))
    (test:assert-equal "FORMAT" (symbol-name (syntax-node-datum node)))
    (test:assert-equal "COMMON-LISP"
                       (package-name (symbol-package (syntax-node-datum node))))))

(test:deftest test-symbol-internal-qualified ()
  "Parse pkg::name as the (possibly internal) symbol NAME in package PKG."
  (let* ((src "epsilon.reader::syntax-node-kind")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal 'syntax-node-kind (syntax-node-datum node))
    (test:assert-equal "EPSILON.READER"
                       (package-name (symbol-package (syntax-node-datum node))))))

(test:deftest test-symbol-pipe-delimited ()
  "Parse |With Spaces And CASE| as a case-preserving symbol."
  (let* ((src "|Hello World|")
         (node (parse1 src)))
    (test:assert-equal :symbol (syntax-node-kind node))
    (test:assert-equal "Hello World"
                       (symbol-name (syntax-node-datum node)))))

;;;; ======================================================================
;;;; Keyword Tests
;;;; ======================================================================

(test:deftest test-keyword-simple ()
  "Parse a keyword."
  (let* ((src ":name")
         (node (parse1 src)))
    (test:assert-equal :keyword (syntax-node-kind node))
    (test:assert-equal :name (syntax-node-datum node))
    (test:assert-equal ":name" (node-source-text src node))))

;;;; ======================================================================
;;;; Character Tests
;;;; ======================================================================

(test:deftest test-character-simple ()
  "Parse a simple character literal."
  (let* ((src "#\\x")
         (node (parse1 src)))
    (test:assert-equal :character (syntax-node-kind node))
    (test:assert-equal #\x (syntax-node-datum node))
    (test:assert-equal "#\\x" (node-source-text src node))))

(test:deftest test-character-space ()
  "Parse #\\Space."
  (let* ((src "#\\Space")
         (node (parse1 src)))
    (test:assert-equal :character (syntax-node-kind node))
    (test:assert-equal #\Space (syntax-node-datum node))))

(test:deftest test-character-newline ()
  "Parse #\\Newline."
  (let* ((src "#\\Newline")
         (node (parse1 src)))
    (test:assert-equal :character (syntax-node-kind node))
    (test:assert-equal #\Newline (syntax-node-datum node))))

;;;; ======================================================================
;;;; List Tests
;;;; ======================================================================

(test:deftest test-list-empty ()
  "Parse an empty list."
  (let* ((src "()")
         (node (parse1 src)))
    (test:assert-equal :list (syntax-node-kind node))
    (test:assert-equal 0 (length (meaningful-nodes (syntax-node-children node))))
    (test:assert-equal "()" (node-source-text src node))))

(test:deftest test-list-simple ()
  "Parse a simple list."
  (let* ((src "(+ 1 2)")
         (node (parse1 src)))
    (test:assert-equal :list (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 3 (length kids))
      (test:assert-equal :symbol (syntax-node-kind (first kids)))
      (test:assert-equal :number (syntax-node-kind (second kids)))
      (test:assert-equal :number (syntax-node-kind (third kids))))
    (test:assert-equal "(+ 1 2)" (node-source-text src node))))

(test:deftest test-list-nested ()
  "Parse nested lists."
  (let* ((src "(a (b c))")
         (node (parse1 src)))
    (test:assert-equal :list (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 2 (length kids))
      (test:assert-equal :list (syntax-node-kind (second kids))))))

(test:deftest test-list-mixed-types ()
  "Parse a list with mixed element types."
  (let* ((src "(1 \"two\" :three)")
         (node (parse1 src)))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 3 (length kids))
      (test:assert-equal :number (syntax-node-kind (first kids)))
      (test:assert-equal :string (syntax-node-kind (second kids)))
      (test:assert-equal :keyword (syntax-node-kind (third kids))))))

;;;; ======================================================================
;;;; Quote / Backquote / Unquote Tests
;;;; ======================================================================

(test:deftest test-quote ()
  "Parse a quoted expression."
  (let* ((src "'foo")
         (node (parse1 src)))
    (test:assert-equal :quote (syntax-node-kind node))
    (test:assert-equal "'foo" (node-source-text src node))
    (let ((inner (first (meaningful-nodes (syntax-node-children node)))))
      (test:assert-equal :symbol (syntax-node-kind inner)))))

(test:deftest test-backquote ()
  "Parse a backquoted expression."
  (let* ((src "`(a b)")
         (node (parse1 src)))
    (test:assert-equal :backquote (syntax-node-kind node))
    (test:assert-equal "`(a b)" (node-source-text src node))))

(test:deftest test-unquote ()
  "Parse an unquoted expression."
  (let* ((src ",x")
         (node (parse1 src)))
    (test:assert-equal :unquote (syntax-node-kind node))
    (test:assert-equal ",x" (node-source-text src node))))

(test:deftest test-splice ()
  "Parse a splice expression."
  (let* ((src ",@xs")
         (node (parse1 src)))
    (test:assert-equal :splice (syntax-node-kind node))
    (test:assert-equal ",@xs" (node-source-text src node))))

;;;; ======================================================================
;;;; Epsilon Extensions: Vectors
;;;; ======================================================================

(test:deftest test-vector-literal ()
  "Parse an Epsilon vector literal."
  (let* ((src "[1 2 3]")
         (node (parse1 src)))
    (test:assert-equal :vector (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 3 (length kids)))
    (test:assert-equal "[1 2 3]" (node-source-text src node))))

(test:deftest test-vector-empty ()
  "Parse an empty vector."
  (let* ((src "[]")
         (node (parse1 src)))
    (test:assert-equal :vector (syntax-node-kind node))
    (test:assert-equal 0 (length (meaningful-nodes (syntax-node-children node))))))

(test:deftest test-vector-nested ()
  "Parse nested vectors."
  (let* ((src "[[1 2] [3 4]]")
         (node (parse1 src)))
    (test:assert-equal :vector (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 2 (length kids))
      (test:assert-equal :vector (syntax-node-kind (first kids)))
      (test:assert-equal :vector (syntax-node-kind (second kids))))))

;;;; ======================================================================
;;;; Epsilon Extensions: Maps
;;;; ======================================================================

(test:deftest test-map-literal ()
  "Parse an Epsilon map literal."
  (let* ((src "{:a 1 :b 2}")
         (node (parse1 src)))
    (test:assert-equal :map (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 4 (length kids)))
    (test:assert-equal "{:a 1 :b 2}" (node-source-text src node))))

(test:deftest test-map-empty ()
  "Parse an empty map."
  (let* ((src "{}")
         (node (parse1 src)))
    (test:assert-equal :map (syntax-node-kind node))
    (test:assert-equal 0 (length (meaningful-nodes (syntax-node-children node))))))

;;;; ======================================================================
;;;; Epsilon Extensions: Sets
;;;; ======================================================================

(test:deftest test-set-literal ()
  "Parse an Epsilon set literal."
  (let* ((src "#{1 2 3}")
         (node (parse1 src)))
    (test:assert-equal :set (syntax-node-kind node))
    (let ((kids (meaningful-nodes (syntax-node-children node))))
      (test:assert-equal 3 (length kids)))
    (test:assert-equal "#{1 2 3}" (node-source-text src node))))

(test:deftest test-set-empty ()
  "Parse an empty set."
  (let* ((src "#{}")
         (node (parse1 src)))
    (test:assert-equal :set (syntax-node-kind node))
    (test:assert-equal 0 (length (meaningful-nodes (syntax-node-children node))))))

;;;; ======================================================================
;;;; Epsilon Extensions: Anonymous Functions
;;;; ======================================================================

(test:deftest test-anon-fn ()
  "Parse an anonymous function literal."
  (let* ((src "#f(+ % 1)")
         (node (parse1 src)))
    (test:assert-equal :anon-fn (syntax-node-kind node))
    (test:assert-equal "#f(+ % 1)" (node-source-text src node))))

(test:deftest test-anon-fn-multi-arg ()
  "Parse an anonymous function with multiple args."
  (let* ((src "#f(* %1 %2)")
         (node (parse1 src)))
    (test:assert-equal :anon-fn (syntax-node-kind node))
    (test:assert-equal "#f(* %1 %2)" (node-source-text src node))))

;;;; ======================================================================
;;;; Epsilon Extensions: String Interpolation
;;;; ======================================================================

(test:deftest test-interpolated-string ()
  "Parse an interpolated string."
  (let* ((src "#~\"hello ~{name}\"")
         (node (parse1 src)))
    (test:assert-equal :interpolated-string (syntax-node-kind node))
    (test:assert-equal src (node-source-text src node))))

(test:deftest test-interpolated-string-no-interp ()
  "Parse an interpolated string with no actual interpolations."
  (let* ((src "#~\"plain text\"")
         (node (parse1 src)))
    (test:assert-equal :interpolated-string (syntax-node-kind node))
    (test:assert-equal src (node-source-text src node))))

;;;; ======================================================================
;;;; Function Quote
;;;; ======================================================================

(test:deftest test-function-quote ()
  "Parse #'symbol."
  (let* ((src "#'car")
         (node (parse1 src)))
    (test:assert-equal :function (syntax-node-kind node))
    (test:assert-equal "#'car" (node-source-text src node))
    (let ((inner (first (meaningful-nodes (syntax-node-children node)))))
      (test:assert-equal :symbol (syntax-node-kind inner)))))

;;;; ======================================================================
;;;; Trivia Tests
;;;; ======================================================================

(test:deftest test-whitespace-preserved ()
  "Whitespace nodes are preserved in the output."
  (let* ((src "  42  ")
         (nodes (read-all-syntax src)))
    (test:assert-equal 3 (length nodes))
    (test:assert-equal :whitespace (syntax-node-kind (first nodes)))
    (test:assert-equal :number (syntax-node-kind (second nodes)))
    (test:assert-equal :whitespace (syntax-node-kind (third nodes)))))

(test:deftest test-line-comment-preserved ()
  "Line comments are preserved as nodes."
  (let* ((src (format nil "; this is a comment~%42"))
         (nodes (read-all-syntax src)))
    (let ((comment (find :comment nodes :key #'syntax-node-kind))
          (number (find :number nodes :key #'syntax-node-kind)))
      (test:assert-true (not (null comment)))
      (test:assert-true (not (null number))))))

(test:deftest test-block-comment-preserved ()
  "Block comments are preserved as nodes."
  (let* ((src "#| block comment |# 42")
         (nodes (read-all-syntax src)))
    (let ((comment (find :comment nodes :key #'syntax-node-kind)))
      (test:assert-true (not (null comment)))
      (test:assert-equal "#| block comment |#" (node-source-text src comment)))))

(test:deftest test-nested-block-comment ()
  "Nested block comments are handled correctly."
  (let* ((src "#| outer #| inner |# still outer |# 1")
         (nodes (read-all-syntax src)))
    (let ((comment (find :comment nodes :key #'syntax-node-kind)))
      (test:assert-true (not (null comment)))
      (test:assert-equal "#| outer #| inner |# still outer |#"
                         (node-source-text src comment)))))

;;;; ======================================================================
;;;; Span Accuracy Tests
;;;; ======================================================================

(test:deftest test-span-accuracy-atoms ()
  "Every atom node's span reproduces exact source text."
  (let* ((src "42 \"hello\" foo :bar #\\x")
         (nodes (read-all-syntax src)))
    (dolist (node nodes)
      (test:assert-equal (node-source-text src node)
                         (subseq src
                                 (loc:source-location-offset (syntax-node-location node))
                                 (loc:source-location-end-offset (syntax-node-location node)))))))

(test:deftest test-span-accuracy-compound ()
  "Compound node spans include delimiters."
  (let* ((src "(a b)")
         (node (parse1 src)))
    (test:assert-equal 0 (loc:source-location-offset (syntax-node-location node)))
    (test:assert-equal 5 (loc:source-location-end-offset (syntax-node-location node)))
    (test:assert-equal "(a b)" (node-source-text src node))))

;;;; ======================================================================
;;;; Roundtrip Fidelity Tests
;;;; ======================================================================

(test:deftest test-roundtrip-simple ()
  "Concatenating all top-level node texts reproduces the source."
  (let* ((src "(defun foo (x) (+ x 1))")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

(test:deftest test-roundtrip-multiline ()
  "Roundtrip works with multiline source."
  (let* ((src "(defun bar (x y)
  ;; Add them
  (+ x y))")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

(test:deftest test-roundtrip-mixed-syntax ()
  "Roundtrip with mixed Epsilon syntax."
  (let* ((src "'foo `(a ,b ,@c) [1 2] {:k v} #{x}")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

(test:deftest test-roundtrip-comments-and-whitespace ()
  "Roundtrip preserves comments and whitespace exactly."
  (let* ((src "  ; comment
(a  b   c)  ; trailing
")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

(test:deftest test-roundtrip-strings-with-escapes ()
  "Roundtrip preserves string escape sequences in source text."
  (let* ((src "(list \"a\\nb\" \"c\\td\")")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

;;;; ======================================================================
;;;; Error Recovery Tests
;;;; ======================================================================

(test:deftest test-error-unterminated-string ()
  "Unterminated string produces an error node."
  (let* ((src "\"oops")
         (nodes (read-all-syntax src))
         (node (first nodes)))
    (test:assert-equal :reader-error (syntax-node-kind node))))

(test:deftest test-error-unexpected-close-paren ()
  "Unexpected close paren produces an error node."
  (let* ((src ")")
         (nodes (read-all-syntax src))
         (node (first nodes)))
    (test:assert-equal :reader-error (syntax-node-kind node))))

(test:deftest test-error-unterminated-list ()
  "Unterminated list produces an error node."
  (let* ((src "(a b")
         (nodes (read-all-syntax src))
         (node (first-meaningful-node nodes)))
    (test:assert-equal :reader-error (syntax-node-kind node))))

(test:deftest test-error-recovery-continues ()
  "After an error, the reader continues to produce nodes."
  (let* ((src ") 42")
         (nodes (read-all-syntax src))
         (meaningful (meaningful-nodes nodes)))
    (test:assert-true (>= (length meaningful) 2))
    (test:assert-equal :reader-error (syntax-node-kind (first meaningful)))
    (test:assert-equal :number (syntax-node-kind (second meaningful)))))

;;;; ======================================================================
;;;; Position Conversion Tests (now via epsilon.source-location)
;;;; ======================================================================

(test:deftest test-line-index-single-line ()
  "Build line offsets for single-line source."
  (let ((idx (loc:build-line-offsets "hello")))
    (test:assert-equal 1 (length idx))
    (test:assert-equal 0 (aref idx 0))))

(test:deftest test-line-index-multi-line ()
  "Build line offsets for multi-line source."
  (let ((idx (loc:build-line-offsets (format nil "ab~%cd~%ef"))))
    (test:assert-equal 3 (length idx))
    (test:assert-equal 0 (aref idx 0))
    (test:assert-equal 3 (aref idx 1))
    (test:assert-equal 6 (aref idx 2))))

(test:deftest test-offset-to-line-column ()
  "Convert offset to line/column."
  (let ((idx (loc:build-line-offsets (format nil "ab~%cd~%ef"))))
    ;; 'a' is at offset 0 = line 1, col 0
    (let ((lc (loc:offset-to-line-column idx 0)))
      (test:assert-equal 1 (car lc))
      (test:assert-equal 0 (cdr lc)))
    ;; 'b' is at offset 1 = line 1, col 1
    (let ((lc (loc:offset-to-line-column idx 1)))
      (test:assert-equal 1 (car lc))
      (test:assert-equal 1 (cdr lc)))
    ;; 'c' is at offset 3 = line 2, col 0
    (let ((lc (loc:offset-to-line-column idx 3)))
      (test:assert-equal 2 (car lc))
      (test:assert-equal 0 (cdr lc)))
    ;; 'e' is at offset 6 = line 3, col 0
    (let ((lc (loc:offset-to-line-column idx 6)))
      (test:assert-equal 3 (car lc))
      (test:assert-equal 0 (cdr lc)))))

(test:deftest test-line-column-to-offset ()
  "Convert line/column to offset."
  (let ((idx (loc:build-line-offsets (format nil "ab~%cd~%ef"))))
    (test:assert-equal 0 (loc:line-column-to-offset idx 1 0))
    (test:assert-equal 1 (loc:line-column-to-offset idx 1 1))
    (test:assert-equal 3 (loc:line-column-to-offset idx 2 0))
    (test:assert-equal 6 (loc:line-column-to-offset idx 3 0))))

(test:deftest test-position-roundtrip ()
  "offset -> line/col -> offset roundtrips."
  (let* ((src (format nil "first~%second~%third"))
         (idx (loc:build-line-offsets src)))
    (dotimes (i (length src))
      (let* ((lc (loc:offset-to-line-column idx i))
             (back (loc:line-column-to-offset idx (car lc) (cdr lc))))
        (test:assert-equal i back)))))

;;;; ======================================================================
;;;; find-node-at-offset Tests
;;;; ======================================================================

(test:deftest test-find-node-at-offset-atom ()
  "Find node at offset returns the atom."
  (let* ((src "(+ 1 2)")
         (nodes (read-all-syntax src))
         ;; offset 3 is in the '1'
         (found (find-node-at-offset nodes 3)))
    (test:assert-true (not (null found)))
    (test:assert-equal :number (syntax-node-kind found))
    (test:assert-equal 1 (syntax-node-datum found))))

(test:deftest test-find-node-at-offset-nested ()
  "Find deepest node in nested structure."
  (let* ((src "(a (b c))")
         (nodes (read-all-syntax src))
         ;; offset 4 is 'b' inside the inner list
         (found (find-node-at-offset nodes 4)))
    (test:assert-true (not (null found)))
    (test:assert-equal :symbol (syntax-node-kind found))))

(test:deftest test-find-node-at-offset-paren ()
  "Find node at opening paren returns the list node."
  (let* ((src "(a b)")
         (nodes (read-all-syntax src))
         (found (find-node-at-offset nodes 0)))
    (test:assert-true (not (null found)))
    ;; At offset 0 (the paren itself), deepest is the list
    ;; unless 'a' starts at 1
    (test:assert-true (member (syntax-node-kind found) '(:list)))))

;;;; ======================================================================
;;;; find-nodes-in-range Tests
;;;; ======================================================================

(test:deftest test-find-nodes-in-range ()
  "Find all nodes overlapping a range."
  (let* ((src "a b c")
         (nodes (read-all-syntax src))
         ;; Range covering 'b'
         (found (find-nodes-in-range nodes 2 3)))
    (test:assert-true (>= (length found) 1))
    (test:assert-true (find :symbol found :key #'syntax-node-kind))))

;;;; ======================================================================
;;;; syntax-node-to-datum Tests
;;;; ======================================================================

(test:deftest test-datum-extraction-number ()
  "Extract number datum."
  (let ((node (parse1 "42")))
    (test:assert-equal 42 (syntax-node-to-datum node))))

(test:deftest test-datum-extraction-string ()
  "Extract string datum."
  (let ((node (parse1 "\"hello\"")))
    (test:assert-equal "hello" (syntax-node-to-datum node))))

(test:deftest test-datum-extraction-list ()
  "Extract list datum."
  (let ((node (parse1 "(1 2 3)")))
    (let ((data (syntax-node-to-datum node)))
      (test:assert-equal 3 (length data))
      (test:assert-equal 1 (first data))
      (test:assert-equal 2 (second data))
      (test:assert-equal 3 (third data)))))

(test:deftest test-nodes-to-data-skips-trivia ()
  "syntax-nodes-to-data skips whitespace and comments."
  (let* ((src (format nil "1 ; comment~% 2 3"))
         (nodes (read-all-syntax src))
         (data (syntax-nodes-to-data nodes)))
    (test:assert-equal 3 (length data))
    (test:assert-equal 1 (first data))
    (test:assert-equal 2 (second data))
    (test:assert-equal 3 (third data))))

;;;; ======================================================================
;;;; Dispatch Reader Tests
;;;; ======================================================================

(test:deftest test-reader-conditional-plus ()
  "Parse #+feature form."
  (let* ((src "#+sbcl t")
         (node (parse1 src)))
    (test:assert-equal :reader-conditional-plus (syntax-node-kind node))
    (test:assert-equal "#+sbcl t" (node-source-text src node))))

(test:deftest test-reader-conditional-minus ()
  "Parse #-feature form."
  (let* ((src "#-sbcl nil")
         (node (parse1 src)))
    (test:assert-equal :reader-conditional-minus (syntax-node-kind node))
    (test:assert-equal "#-sbcl nil" (node-source-text src node))))

(test:deftest test-uninterned-symbol ()
  "Parse #:foo uninterned symbol."
  (let* ((src "#:foo")
         (node (parse1 src)))
    (test:assert-equal :uninterned-symbol (syntax-node-kind node))
    (test:assert-equal "#:foo" (node-source-text src node))))

;;;; ======================================================================
;;;; Multi-form Source Tests
;;;; ======================================================================

(test:deftest test-multiple-top-level-forms ()
  "Parse multiple top-level forms."
  (let* ((src (format nil "(defun f () 1)~%~%(defun g () 2)"))
         (nodes (read-all-syntax src))
         (forms (meaningful-nodes nodes)))
    (test:assert-equal 2 (length forms))
    (test:assert-equal :list (syntax-node-kind (first forms)))
    (test:assert-equal :list (syntax-node-kind (second forms)))))

(test:deftest test-empty-source ()
  "Parse empty source returns empty list."
  (let ((nodes (read-all-syntax "")))
    (test:assert-equal 0 (length nodes))))

(test:deftest test-whitespace-only-source ()
  "Parse whitespace-only source returns whitespace nodes."
  (let ((nodes (read-all-syntax (format nil "   ~%~%  "))))
    (test:assert-true (> (length nodes) 0))
    (dolist (n nodes)
      (test:assert-equal :whitespace (syntax-node-kind n)))))

;;;; ======================================================================
;;;; Real-world Roundtrip Test
;;;; ======================================================================

(test:deftest test-roundtrip-defun ()
  "Roundtrip a realistic defun form."
  (let* ((src "(defun factorial (n)
  \"Compute N factorial.\"
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

(test:deftest test-roundtrip-complex-form ()
  "Roundtrip a complex form with mixed syntax."
  (let* ((src "(let ((v [1 2 3])
      (m {:a 1 :b 2})
      (s #{:x :y}))
  ;; Process data
  #f(+ % 1)
  #'car
  `(list ,v ,@items))")
         (nodes (read-all-syntax src))
         (reconstructed (apply #'concatenate 'string
                               (mapcar (lambda (n) (node-source-text src n))
                                       nodes))))
    (test:assert-equal src reconstructed)))

;;;; ======================================================================
;;;; Reader Conditionals (#+ / #-) and Read-Time Eval (#.)
;;;; ======================================================================

(test:deftest test-feature-expression-symbol ()
  "Bare feature names match against *features* by symbol-name."
  (test:assert-true (feature-expression-true-p :sbcl))
  ;; Match-by-name means 'sbcl in cl-user resolves the same way.
  (let ((*package* (find-package :cl-user)))
    (test:assert-true (feature-expression-true-p (intern "SBCL")))))

(test:deftest test-feature-expression-and-or-not ()
  "AND/OR/NOT compose feature names."
  (test:assert-true (feature-expression-true-p '(or :sbcl :ccl)))
  (test:assert-true (feature-expression-true-p '(or :ccl :sbcl)))
  (test:assert-true (not (feature-expression-true-p '(and :sbcl :ccl))))
  (test:assert-true (feature-expression-true-p '(not :ccl)))
  (test:assert-true (not (feature-expression-true-p '(not :sbcl)))))

(test:deftest test-reader-conditional-plus-includes ()
  "#+sbcl form keeps the form when sbcl is in *features*."
  (let* ((*package* (find-package :cl-user))
         (data (syntax-nodes-to-data
                (read-all-syntax "(a) #+sbcl (b) (c)"))))
    (test:assert-equal 3 (length data))
    (test:assert-equal "B" (symbol-name (first (second data))))))

(test:deftest test-reader-conditional-plus-excludes ()
  "#+nonexistent-feature form drops the form."
  (let* ((*package* (find-package :cl-user))
         (data (syntax-nodes-to-data
                (read-all-syntax "(a) #+definitely-not-a-feature (b) (c)"))))
    (test:assert-equal 2 (length data))
    (test:assert-equal "A" (symbol-name (first (first data))))
    (test:assert-equal "C" (symbol-name (first (second data))))))

(test:deftest test-reader-conditional-minus-excludes ()
  "#-sbcl form drops the form when sbcl is in *features*."
  (let* ((*package* (find-package :cl-user))
         (data (syntax-nodes-to-data
                (read-all-syntax "(a) #-sbcl (b) (c)"))))
    (test:assert-equal 2 (length data))
    (test:assert-equal "A" (symbol-name (first (first data))))
    (test:assert-equal "C" (symbol-name (first (second data))))))

(test:deftest test-reader-conditional-minus-includes ()
  "#-nonexistent-feature form keeps the form."
  (let* ((*package* (find-package :cl-user))
         (data (syntax-nodes-to-data
                (read-all-syntax
                 "(a) #-definitely-not-a-feature (b) (c)"))))
    (test:assert-equal 3 (length data))))

(test:deftest test-reader-conditional-complex-expression ()
  "Compound feature expressions evaluate against *features*."
  (let* ((*package* (find-package :cl-user))
         (data (syntax-nodes-to-data
                (read-all-syntax
                 "#+(or sbcl ccl) :present #+(and sbcl no-such) :missing"))))
    (test:assert-equal '(:present) data)))

(test:deftest test-read-time-eval-evaluates ()
  "#.expr evaluates expr at extraction time when *read-eval* is true."
  (let* ((*package* (find-package :cl-user))
         (*read-eval* t)
         (data (syntax-nodes-to-data
                (read-all-syntax "(x #.(+ 2 3) y)"))))
    ;; Outer is a single list of three elements: x, 5, y.
    (test:assert-equal 1 (length data))
    (test:assert-equal 3 (length (first data)))
    (test:assert-equal 5 (second (first data)))))

(test:deftest test-read-time-eval-disabled ()
  "When *read-eval* is NIL, #.expr returns the inner datum unchanged."
  (let* ((*package* (find-package :cl-user))
         (*read-eval* nil)
         (data (syntax-nodes-to-data
                (read-all-syntax "(x #.(+ 2 3) y)"))))
    ;; Inner second element is (+ 2 3) -- the unevaluated form.
    (test:assert-equal 1 (length data))
    (let ((middle (second (first data))))
      (test:assert-true (consp middle))
      (test:assert-equal "+" (symbol-name (first middle)))
      (test:assert-equal 2 (second middle))
      (test:assert-equal 3 (third middle)))))

(test:deftest test-reader-conditional-preserved-in-cst ()
  "Reader conditionals are still represented in the CST itself even when
   their datum is elided -- they just don't show up in the data view."
  (let* ((nodes (read-all-syntax "(a) #+sbcl (b)"))
         (kinds (mapcar #'syntax-node-kind (meaningful-nodes nodes))))
    (test:assert-equal '(:list :reader-conditional-plus) kinds)))
