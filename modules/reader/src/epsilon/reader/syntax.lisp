;;;; syntax.lisp - Position-aware Lisp reader
;;;;
;;;; A from-scratch reader that produces a concrete syntax tree with
;;;; byte-accurate [start, end) spans. Handles standard Common Lisp syntax
;;;; plus Epsilon extensions (vectors, maps, sets, anon fns, string
;;;; interpolation). Whitespace and comments are first-class nodes for
;;;; roundtrip fidelity.
;;;;
;;;; Key invariant: sibling node spans tile the source without gaps or
;;;; overlaps, guaranteeing that concatenating node texts reproduces the
;;;; original source byte-for-byte.

(defpackage :epsilon.reader
  (:use :cl)
  (:import
   (:epsilon.source-location :loc))
  (:export
   ;; Data types
   #:syntax-node
   #:make-syntax-node
   #:syntax-node-kind
   #:syntax-node-location
   #:syntax-node-datum
   #:syntax-node-children
   #:cursor
   #:make-cursor
   #:cursor-source
   #:cursor-pos
   #:cursor-line
   #:cursor-col
   #:cursor-file
   ;; Public API
   #:read-syntax
   #:read-all-syntax
   #:read-file-syntax
   #:syntax-node-to-datum
   #:syntax-nodes-to-data
   #:*cst-elided*
   #:feature-expression-true-p
   #:node-text
   #:find-node-at-offset
   #:find-ancestor-chain
   #:find-nodes-in-range
   #:build-line-index
   #:offset-to-line-column
   #:line-column-to-offset))

;;;; ======================================================================
;;;; Data Structures
;;;; ======================================================================

(defstruct syntax-node
  "A node in the concrete syntax tree.
   KIND is a keyword identifying the node type.
   LOCATION is a source-location with byte offset range.
   DATUM is the parsed Lisp value (for atoms) or nil.
   CHILDREN is a list of child syntax-nodes (for compound forms)."
  (kind nil :type keyword)
  (location nil :type (or null loc:source-location))
  (datum nil)
  (children nil :type list))

(defstruct cursor
  "Tracks position while scanning a source string."
  (source "" :type string)
  (pos 0 :type fixnum)
  (line 1 :type fixnum)
  (col 0 :type fixnum)
  (file nil :type (or pathname string null)))

;;;; ======================================================================
;;;; Location Helper
;;;; ======================================================================

(declaim (inline make-loc))

(defun make-loc (cur start end)
  "Create a source-location from cursor's file and byte offsets."
  (loc:make-source-location :file (cursor-file cur) :offset start :end-offset end))

;;;; ======================================================================
;;;; Cursor Operations
;;;; ======================================================================

(declaim (inline cursor-eof-p cursor-peek cursor-read-char))

(defun cursor-eof-p (cur)
  "Return T if the cursor is at end of source."
  (>= (cursor-pos cur) (length (cursor-source cur))))

(defun cursor-peek (cur)
  "Return the character at the current position without advancing, or NIL at EOF."
  (if (cursor-eof-p cur)
      nil
      (char (cursor-source cur) (cursor-pos cur))))

(defun cursor-peek-next (cur)
  "Return the character one past current position without advancing, or NIL."
  (let ((next-pos (1+ (cursor-pos cur))))
    (if (>= next-pos (length (cursor-source cur)))
        nil
        (char (cursor-source cur) next-pos))))

(defun cursor-read-char (cur)
  "Read one character and advance the cursor. Returns NIL at EOF."
  (when (cursor-eof-p cur)
    (return-from cursor-read-char nil))
  (let ((ch (char (cursor-source cur) (cursor-pos cur))))
    (incf (cursor-pos cur))
    (if (char= ch #\Newline)
        (progn
          (incf (cursor-line cur))
          (setf (cursor-col cur) 0))
        (incf (cursor-col cur)))
    ch))

(defun cursor-advance (cur n)
  "Advance cursor by N characters, updating line/col tracking."
  (dotimes (_ n)
    (cursor-read-char cur)))

;;;; ======================================================================
;;;; Character Classification
;;;; ======================================================================

(declaim (inline whitespacep terminatingp))

(defun whitespacep (ch)
  "Return T if CH is whitespace."
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun terminatingp (ch)
  "Return T if CH terminates a token (atom boundary)."
  (or (whitespacep ch)
      (member ch '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\`) :test #'char=)))

(defun digit-char-10-p (ch)
  "Return T if CH is a decimal digit."
  (and (char>= ch #\0) (char<= ch #\9)))

;;;; ======================================================================
;;;; Error Recovery
;;;; ======================================================================

(defun make-error-node (cur start message)
  "Create a :reader-error node spanning from START to current position."
  (make-syntax-node :kind :reader-error
                    :location (make-loc cur start (cursor-pos cur))
                    :datum message))

(defun synchronize (cur)
  "Advance cursor to the next recovery point: a newline at depth 0 or
   the position after a matching close delimiter. Skips over strings."
  (let ((depth 0))
    (loop
      (when (cursor-eof-p cur)
        (return))
      (let ((ch (cursor-peek cur)))
        (cond
          ((member ch '(#\( #\[ #\{) :test #'char=)
           (cursor-read-char cur)
           (incf depth))
          ((member ch '(#\) #\] #\}) :test #'char=)
           (cursor-read-char cur)
           (if (> depth 0)
               (decf depth)
               (when (zerop depth)
                 (return))))
          ((char= ch #\")
           ;; Skip over string
           (cursor-read-char cur)
           (loop
             (when (cursor-eof-p cur) (return))
             (let ((sc (cursor-read-char cur)))
               (cond
                 ((char= sc #\\) (cursor-read-char cur))
                 ((char= sc #\") (return))))))
          ((and (char= ch #\Newline) (zerop depth))
           (return))
          (t
           (cursor-read-char cur)))))))

;;;; ======================================================================
;;;; Whitespace and Comments
;;;; ======================================================================

(defun read-whitespace-node (cur)
  "Read contiguous whitespace, return a :whitespace node."
  (let ((start (cursor-pos cur)))
    (loop while (and (not (cursor-eof-p cur))
                     (whitespacep (cursor-peek cur)))
          do (cursor-read-char cur))
    (make-syntax-node :kind :whitespace
                      :location (make-loc cur start (cursor-pos cur)))))

(defun read-line-comment-node (cur)
  "Read a ; line comment through end of line, return a :comment node."
  (let ((start (cursor-pos cur)))
    ;; Consume the leading semicolons and everything through newline
    (loop while (and (not (cursor-eof-p cur))
                     (not (char= (cursor-peek cur) #\Newline)))
          do (cursor-read-char cur))
    (make-syntax-node :kind :comment
                      :location (make-loc cur start (cursor-pos cur)))))

(defun read-block-comment-node (cur)
  "Read a #|...|# block comment (nested), return a :comment node.
   Assumes #| has already been consumed."
  (let ((start (- (cursor-pos cur) 2))  ; include the #|
        (depth 1))
    (loop while (and (> depth 0) (not (cursor-eof-p cur)))
          do (let ((ch (cursor-read-char cur)))
               (cond
                 ;; Nested #|
                 ((and (char= ch #\#)
                       (not (cursor-eof-p cur))
                       (char= (cursor-peek cur) #\|))
                  (cursor-read-char cur)
                  (incf depth))
                 ;; Closing |#
                 ((and (char= ch #\|)
                       (not (cursor-eof-p cur))
                       (char= (cursor-peek cur) #\#))
                  (cursor-read-char cur)
                  (decf depth)))))
    (make-syntax-node :kind :comment
                      :location (make-loc cur start (cursor-pos cur)))))

(defun skip-whitespace-and-comments (cur)
  "Consume whitespace and comments, returning a list of trivia nodes."
  (let ((trivia '()))
    (loop
      (cond
        ((cursor-eof-p cur)
         (return (nreverse trivia)))
        ((whitespacep (cursor-peek cur))
         (push (read-whitespace-node cur) trivia))
        ((char= (cursor-peek cur) #\;)
         (push (read-line-comment-node cur) trivia))
        (t
         (return (nreverse trivia)))))))

;;;; ======================================================================
;;;; Atom Readers
;;;; ======================================================================

(defun cursor-peek-at (cur offset)
  "Peek the character at (cursor-pos cur)+OFFSET, or NIL beyond EOF."
  (let ((pos (+ (cursor-pos cur) offset)))
    (when (< pos (length (cursor-source cur)))
      (char (cursor-source cur) pos))))

(defun cursor-three-quotes-p (cur)
  "T iff the cursor is sitting on a literal `\"\"\"` triple-quote."
  (and (eql (cursor-peek cur)         #\")
       (eql (cursor-peek-next cur)    #\")
       (eql (cursor-peek-at cur 2)    #\")))

(defun read-triple-quoted-string-body (cur start)
  "Read the body of a `\"\"\"...\"\"\"` heredoc. The cursor is positioned
just AFTER the opening triple quote.  No escape processing inside
(raw text); reads up to and through the next `\"\"\"`."
  (let ((chars '()))
    (loop
      (when (cursor-eof-p cur)
        (return-from read-triple-quoted-string-body
          (make-error-node cur start "Unterminated triple-quoted string")))
      (cond
        ((cursor-three-quotes-p cur)
         (cursor-read-char cur)
         (cursor-read-char cur)
         (cursor-read-char cur)
         (return (make-syntax-node
                  :kind :string
                  :location (make-loc cur start (cursor-pos cur))
                  :datum (coerce (nreverse chars) 'string))))
        (t
         (push (cursor-read-char cur) chars))))))

(defun read-string-node (cur)
  "Read a string literal.  Supports the standard `\"...\"` form and the
`\"\"\"...\"\"\"` triple-quoted heredoc used by CIN.  Cursor is on the
opening quote."
  (let ((start (cursor-pos cur)))
    (cursor-read-char cur)  ; consume opening "
    ;; Triple-quoted heredoc: another two quotes immediately follow.
    (when (and (eql (cursor-peek cur) #\")
               (eql (cursor-peek-next cur) #\"))
      (cursor-read-char cur)  ; 2nd "
      (cursor-read-char cur)  ; 3rd "
      (return-from read-string-node
        (read-triple-quoted-string-body cur start)))
    ;; Standard double-quoted string.
    (let ((chars '()))
      (loop
        (when (cursor-eof-p cur)
          (return-from read-string-node
            (make-error-node cur start "Unterminated string literal")))
        (let ((ch (cursor-read-char cur)))
          (cond
            ((char= ch #\")
             (return (make-syntax-node
                      :kind :string
                      :location (make-loc cur start (cursor-pos cur))
                      :datum (coerce (nreverse chars) 'string))))
            ((char= ch #\\)
             (when (cursor-eof-p cur)
               (return-from read-string-node
                 (make-error-node cur start "Unterminated escape in string")))
             (let ((esc (cursor-read-char cur)))
               (push (case esc
                       (#\n #\Newline)
                       (#\t #\Tab)
                       (#\r #\Return)
                       (#\\ #\\)
                       (#\" #\")
                       (#\0 #\Nul)
                       (#\a #\Bel)
                       (#\b #\Backspace)
                       (#\f #\Page)
                       (t esc))
                     chars)))
            (t (push ch chars))))))))

(defun read-token (cur)
  "Read a token (non-whitespace, non-delimiter) string from cursor.
   Returns the token string and does not interpret it."
  (let ((start (cursor-pos cur)))
    (loop while (and (not (cursor-eof-p cur))
                     (not (terminatingp (cursor-peek cur))))
          do (let ((ch (cursor-peek cur)))
               (cond
                 ;; Escaped character in symbol: |...|
                 ((char= ch #\|)
                  (cursor-read-char cur)
                  (loop while (and (not (cursor-eof-p cur))
                                   (not (char= (cursor-peek cur) #\|)))
                        do (cursor-read-char cur))
                  (when (not (cursor-eof-p cur))
                    (cursor-read-char cur)))  ; consume closing |
                 ;; Backslash escape in symbol
                 ((char= ch #\\)
                  (cursor-read-char cur)
                  (when (not (cursor-eof-p cur))
                    (cursor-read-char cur)))
                 (t
                  (cursor-read-char cur)))))
    (subseq (cursor-source cur) start (cursor-pos cur))))

(defun try-parse-number (token)
  "Attempt to parse TOKEN as a number. Returns the number or NIL.
   Handles integers, floats, ratios, and hex (#x) literals."
  (let ((len (length token)))
    (when (zerop len) (return-from try-parse-number nil))
    ;; Try standard CL number parsing
    (handler-case
        (let ((val (let ((*read-eval* nil)
                         (*read-base* 10))
                     (read-from-string token))))
          (when (numberp val) val))
      (error () nil))))

(defun parse-token-with-cl-reader (token)
  "Classify TOKEN by handing it to cl:read-from-string with *read-eval*
   disabled.  Returns (values KIND DATUM) where KIND is :number, :symbol,
   or :keyword.  Delegating to CL's reader gives correct handling of
   package-qualified symbols (pkg:name and pkg::name), pipe-delimited
   names, backslash escapes, and the full numeric format grammar.

   When CL's reader signals a syntactic error (e.g. a lone dot, which
   is meaningful only inside a list context), fall back to interning
   the upcased token verbatim.  This preserves the previously lenient
   CST behavior so downstream refactor/rewrite flows that synthesize
   such tokens continue to round-trip.

   *package* is consulted for unqualified symbols so callers must
   already have it set to the package the form would be read in."
  (handler-case
      (let* ((*read-eval* nil)
             (parsed (read-from-string token)))
        (cond ((numberp parsed) (values :number parsed))
              ((keywordp parsed) (values :keyword parsed))
              ((symbolp parsed) (values :symbol parsed))
              (t (values :symbol (intern (string-upcase token))))))
    (error ()
      (values :symbol (intern (string-upcase token))))))

(defun read-number-or-symbol-node (cur)
  "Read a token and classify it as number, symbol, or keyword."
  (let ((start (cursor-pos cur))
        (token (read-token cur)))
    (when (zerop (length token))
      (return-from read-number-or-symbol-node
        (make-error-node cur start "Empty token")))
    (let ((loc (make-loc cur start (cursor-pos cur))))
      (multiple-value-bind (kind datum) (parse-token-with-cl-reader token)
        (make-syntax-node :kind kind :location loc :datum datum)))))

(defun read-character-node (cur start)
  "Read a character literal after #\\. START is the position of #.
   Cursor is positioned after the backslash."
  (when (cursor-eof-p cur)
    (return-from read-character-node
      (make-error-node cur start "Unterminated character literal")))
  ;; Read the character name
  (let ((first-char (cursor-read-char cur)))
    ;; Check for named characters (e.g., #\Space, #\Newline)
    (if (and (alpha-char-p first-char)
             (not (cursor-eof-p cur))
             (alpha-char-p (cursor-peek cur)))
        ;; Multi-character name
        (let ((name (make-string-output-stream)))
          (write-char first-char name)
          (loop while (and (not (cursor-eof-p cur))
                           (not (terminatingp (cursor-peek cur))))
                do (write-char (cursor-read-char cur) name))
          (let* ((name-str (get-output-stream-string name))
                 (ch (name-char name-str)))
            (if ch
                (make-syntax-node :kind :character
                                  :location (make-loc cur start (cursor-pos cur))
                                  :datum ch)
                (make-error-node cur start
                                 (format nil "Unknown character name: ~A" name-str)))))
        ;; Single character
        (make-syntax-node :kind :character
                          :location (make-loc cur start (cursor-pos cur))
                          :datum first-char))))

;;;; ======================================================================
;;;; Compound Readers
;;;; ======================================================================

(defun close-delimiter-p (ch)
  "Return T if CH is a closing delimiter character."
  (member ch '(#\) #\] #\})))

(defun read-delimited-nodes (cur close-char kind)
  "Read nodes until CLOSE-CHAR, collecting children with trivia.
   Returns a syntax-node of the given KIND. Cursor starts after the
   opening delimiter."
  (let ((start (1- (cursor-pos cur)))  ; include opening delimiter
        (children '()))
    (loop
      ;; Collect trivia
      (let ((trivia (skip-whitespace-and-comments cur)))
        (setf children (nconc children trivia)))
      ;; Check for close or EOF
      (cond
        ((cursor-eof-p cur)
         (return (make-syntax-node
                  :kind :reader-error
                  :location (make-loc cur start (cursor-pos cur))
                  :datum (format nil "Unterminated ~A, expected ~A"
                                 kind close-char)
                  :children (nreverse children))))
        ((char= (cursor-peek cur) close-char)
         (cursor-read-char cur)  ; consume closing delimiter
         (return (make-syntax-node
                  :kind kind
                  :location (make-loc cur start (cursor-pos cur))
                  :children children)))
        ;; Mismatched close delimiter: e.g., ] when expecting )
        ((and (not (char= (cursor-peek cur) close-char))
              (close-delimiter-p (cursor-peek cur)))
         (let* ((mismatch-start (cursor-pos cur))
                (actual-char (cursor-read-char cur)))
           (setf children (nconc children
                                 (list (make-syntax-node
                                        :kind :reader-error
                                        :location (make-loc cur mismatch-start (cursor-pos cur))
                                        :datum (format nil "Mismatched delimiter: expected ~A but found ~A"
                                                       close-char actual-char)))))
           (return (make-syntax-node
                    :kind kind
                    :location (make-loc cur start (cursor-pos cur))
                    :children children))))
        (t
         (let ((child (read-syntax-node cur)))
           (when child
             (setf children (nconc children (list child))))))))))

(defun read-list-node (cur)
  "Read a list form (...). Cursor is on the opening paren."
  (cursor-read-char cur)  ; consume (
  (read-delimited-nodes cur #\) :list))

(defun read-vector-node (cur)
  "Read an Epsilon vector literal [...]. Cursor is on the opening bracket."
  (cursor-read-char cur)  ; consume [
  (read-delimited-nodes cur #\] :vector))

(defun read-map-node (cur)
  "Read an Epsilon map literal {...}. Cursor is on the opening brace."
  (cursor-read-char cur)  ; consume {
  (read-delimited-nodes cur #\} :map))

;;;; ======================================================================
;;;; Prefix / Sugar Readers
;;;; ======================================================================

(defun read-prefixed-node (cur kind)
  "Read a prefix form (quote, backquote, etc.). Cursor is on the prefix char.
   Consumes the prefix and reads one child node."
  (let ((start (cursor-pos cur)))
    (cursor-read-char cur)  ; consume prefix character
    ;; Collect any trivia between prefix and expression
    (let* ((trivia (skip-whitespace-and-comments cur))
           (child (read-syntax-node cur)))
      (if child
          (make-syntax-node :kind kind
                            :location (make-loc cur start (cursor-pos cur))
                            :children (append trivia (list child)))
          (make-error-node cur start
                           (format nil "Expected expression after ~A" kind))))))

(defun read-quote-node (cur)
  "Read 'expr."
  (read-prefixed-node cur :quote))

(defun read-backquote-node (cur)
  "Read `expr."
  (read-prefixed-node cur :backquote))

(defun read-unquote-node (cur)
  "Read ,expr or ,@expr."
  (let ((start (cursor-pos cur)))
    (cursor-read-char cur)  ; consume ,
    (cond
      ((and (not (cursor-eof-p cur)) (char= (cursor-peek cur) #\@))
       (cursor-read-char cur)  ; consume @
       (let* ((trivia (skip-whitespace-and-comments cur))
              (child (read-syntax-node cur)))
         (if child
             (make-syntax-node :kind :splice
                               :location (make-loc cur start (cursor-pos cur))
                               :children (append trivia (list child)))
             (make-error-node cur start "Expected expression after ,@"))))
      (t
       (let* ((trivia (skip-whitespace-and-comments cur))
              (child (read-syntax-node cur)))
         (if child
             (make-syntax-node :kind :unquote
                               :location (make-loc cur start (cursor-pos cur))
                               :children (append trivia (list child)))
             (make-error-node cur start "Expected expression after ,")))))))

;;;; ======================================================================
;;;; Dispatch (#) Reader
;;;; ======================================================================

(defun read-dispatch-node (cur)
  "Read a # dispatch form. Cursor is on the #."
  (let ((start (cursor-pos cur)))
    (cursor-read-char cur)  ; consume #
    (when (cursor-eof-p cur)
      (return-from read-dispatch-node
        (make-error-node cur start "Unexpected end of input after #")))
    (let ((sub-char (cursor-peek cur)))
      (case sub-char
        ;; #' function quote
        (#\'
         (cursor-read-char cur)
         (let* ((trivia (skip-whitespace-and-comments cur))
                (child (read-syntax-node cur)))
           (if child
               (make-syntax-node :kind :function
                                 :location (make-loc cur start (cursor-pos cur))
                                 :children (append trivia (list child)))
               (make-error-node cur start "Expected symbol after #'"))))

        ;; #( vector literal (CL standard)
        (#\(
         (cursor-read-char cur)
         (let ((node (read-delimited-nodes cur #\) :cl-vector)))
           (setf (syntax-node-location node)
                 (loc:copy-source-location (syntax-node-location node) :offset start))
           node))

        ;; #\ character literal
        (#\\
         (cursor-read-char cur)
         (read-character-node cur start))

        ;; #| block comment
        (#\|
         (cursor-read-char cur)
         (read-block-comment-node cur))

        ;; #{ set literal
        (#\{
         (cursor-read-char cur)
         (let ((node (read-delimited-nodes cur #\} :set)))
           (setf (syntax-node-location node)
                 (loc:copy-source-location (syntax-node-location node) :offset start))
           node))

        ;; #+ reader conditional (include).
        ;; CLHS specifies the feature expression is read with *package*
        ;; bound to :keyword so bare names resolve to keywords.
        (#\+
         (cursor-read-char cur)
         (let* ((trivia1 (skip-whitespace-and-comments cur))
                (feature (let ((*package* (find-package :keyword)))
                           (read-syntax-node cur)))
                (trivia2 (skip-whitespace-and-comments cur))
                (form (read-syntax-node cur)))
           (make-syntax-node :kind :reader-conditional-plus
                             :location (make-loc cur start (cursor-pos cur))
                             :children (append trivia1
                                              (when feature (list feature))
                                              trivia2
                                              (when form (list form))))))

        ;; #- reader conditional (exclude). Same keyword-package binding
        ;; as #+ for the feature expression.
        (#\-
         ;; Could be a number like #-1 ... but usually reader conditional
         ;; Check if followed by digit -- if so, treat as unknown dispatch
         (if (and (cursor-peek-next cur)
                  (digit-char-10-p (cursor-peek-next cur)))
             ;; Unknown numeric dispatch
             (progn
               (let ((token (read-token cur)))
                 (make-syntax-node :kind :reader-macro
                                   :location (make-loc cur start (cursor-pos cur))
                                   :datum (format nil "#~A" token))))
             ;; Reader conditional (exclude)
             (progn
               (cursor-read-char cur)
               (let* ((trivia1 (skip-whitespace-and-comments cur))
                      (feature (let ((*package* (find-package :keyword)))
                                 (read-syntax-node cur)))
                      (trivia2 (skip-whitespace-and-comments cur))
                      (form (read-syntax-node cur)))
                 (make-syntax-node :kind :reader-conditional-minus
                                   :location (make-loc cur start (cursor-pos cur))
                                   :children (append trivia1
                                                     (when feature (list feature))
                                                     trivia2
                                                     (when form (list form))))))))

        ;; #f anonymous function
        (#\f
         ;; Check it's actually #f( pattern
         (if (and (cursor-peek-next cur)
                  (char= (cursor-peek-next cur) #\())
             (progn
               (cursor-read-char cur)  ; consume f
               (cursor-read-char cur)  ; consume (
               (let ((node (read-delimited-nodes cur #\) :anon-fn)))
                 (setf (syntax-node-location node)
                 (loc:copy-source-location (syntax-node-location node) :offset start))
                 node))
             ;; Just a symbol starting with #f...
             (let ((token (read-token cur)))
               (make-syntax-node :kind :reader-macro
                                 :location (make-loc cur start (cursor-pos cur))
                                 :datum (format nil "#~A" token)))))

        ;; #~ string interpolation
        (#\~
         (cursor-read-char cur)  ; consume ~
         (if (and (not (cursor-eof-p cur)) (char= (cursor-peek cur) #\"))
             (read-interpolated-string-node cur start)
             (make-error-node cur start "Expected \" after #~")))

        ;; #. read-time eval
        (#\.
         (cursor-read-char cur)
         (let* ((trivia (skip-whitespace-and-comments cur))
                (child (read-syntax-node cur)))
           (make-syntax-node :kind :read-eval
                             :location (make-loc cur start (cursor-pos cur))
                             :children (append trivia (when child (list child))))))

        ;; #: uninterned symbol
        (#\:
         (cursor-read-char cur)
         (let ((token (read-token cur)))
           (make-syntax-node :kind :uninterned-symbol
                             :location (make-loc cur start (cursor-pos cur))
                             :datum (make-symbol (string-upcase token)))))

        ;; #n= #n# label/reference
        (otherwise
         (cond
           ;; Numeric prefix: #1= #1# etc
           ((and sub-char (digit-char-10-p sub-char))
            (let ((digits '()))
              (loop while (and (not (cursor-eof-p cur))
                               (digit-char-10-p (cursor-peek cur)))
                    do (push (cursor-read-char cur) digits))
              (let ((suffix (if (cursor-eof-p cur) nil (cursor-peek cur))))
                (cond
                  ((and suffix (char= suffix #\=))
                   (cursor-read-char cur)
                   (let ((child (read-syntax-node cur)))
                     (make-syntax-node :kind :label-def
                                       :location (make-loc cur start (cursor-pos cur))
                                       :datum (parse-integer (coerce (nreverse digits) 'string))
                                       :children (when child (list child)))))
                  ((and suffix (char= suffix #\#))
                   (cursor-read-char cur)
                   (make-syntax-node :kind :label-ref
                                     :location (make-loc cur start (cursor-pos cur))
                                     :datum (parse-integer (coerce (nreverse digits) 'string))))
                  (t
                   ;; #nR radix or #nA array, etc
                   (when suffix (cursor-read-char cur))
                   (let ((token (read-token cur)))
                     (make-syntax-node :kind :reader-macro
                                       :location (make-loc cur start (cursor-pos cur))
                                       :datum (format nil "#~A~@[~A~]~A"
                                                      (coerce (nreverse digits) 'string)
                                                      suffix token))))))))
           ;; Other dispatch macros (generic fallback)
           (t
            (cursor-read-char cur)
            (let ((child (read-syntax-node cur)))
              (make-syntax-node :kind :reader-macro
                                :location (make-loc cur start (cursor-pos cur))
                                :datum sub-char
                                :children (when child (list child)))))))))))

;;;; ======================================================================
;;;; String Interpolation Reader
;;;; ======================================================================

(defun read-interpolated-string-node (cur start)
  "Read a #~\"...\" interpolated string. Cursor is on the opening quote.
   START is the position of the # character."
  (cursor-read-char cur)  ; consume opening "
  (let ((children '())
        (literal-start (cursor-pos cur)))
    (loop
      (when (cursor-eof-p cur)
        (return-from read-interpolated-string-node
          (make-error-node cur start "Unterminated interpolated string")))
      (let ((ch (cursor-peek cur)))
        (cond
          ;; End of string
          ((char= ch #\")
           ;; Flush any accumulated literal
           (when (> (cursor-pos cur) literal-start)
             (push (make-syntax-node
                    :kind :string-fragment
                    :location (make-loc cur literal-start (cursor-pos cur)))
                   children))
           (cursor-read-char cur)  ; consume closing "
           (return (make-syntax-node
                    :kind :interpolated-string
                    :location (make-loc cur start (cursor-pos cur))
                    :children (nreverse children))))

          ;; Tilde - interpolation or escape
          ((char= ch #\~)
           (cursor-read-char cur)  ; consume ~
           (when (cursor-eof-p cur)
             (return-from read-interpolated-string-node
               (make-error-node cur start "Unterminated interpolated string after ~")))
           (let ((next (cursor-peek cur)))
             (cond
               ;; ~~ escaped tilde
               ((char= next #\~)
                (cursor-read-char cur))
               ;; ~{expr} interpolation
               ((char= next #\{)
                ;; Flush literal before interpolation
                (let ((tilde-pos (1- (cursor-pos cur))))
                  (when (> tilde-pos literal-start)
                    (push (make-syntax-node
                           :kind :string-fragment
                           :location (make-loc cur literal-start tilde-pos))
                          children)))
                (cursor-read-char cur)  ; consume {
                ;; Read expression inside { ... }
                (let ((expr-start (cursor-pos cur))
                      (brace-depth 1))
                  (loop while (and (> brace-depth 0) (not (cursor-eof-p cur)))
                        do (let ((bc (cursor-peek cur)))
                             (cond
                               ((char= bc #\{) (incf brace-depth) (cursor-read-char cur))
                               ((char= bc #\}) (decf brace-depth)
                                (when (> brace-depth 0) (cursor-read-char cur)))
                               ((char= bc #\")
                                ;; Skip string inside interpolation
                                (cursor-read-char cur)
                                (loop while (not (cursor-eof-p cur))
                                      do (let ((sc (cursor-read-char cur)))
                                           (cond ((char= sc #\\) (cursor-read-char cur))
                                                 ((char= sc #\") (return))))))
                               (t (cursor-read-char cur)))))
                  (push (make-syntax-node
                         :kind :interpolation
                         :location (make-loc cur (- expr-start 2)  ; include ~{
                                             (1+ (cursor-pos cur))))  ; include }
                        children)
                  (cursor-read-char cur)  ; consume closing }
                  (setf literal-start (cursor-pos cur))))
               ;; Literal tilde
               (t nil))))

          ;; Backslash escape
          ((char= ch #\\)
           (cursor-read-char cur)
           (when (not (cursor-eof-p cur))
             (cursor-read-char cur)))

          ;; Regular character
          (t (cursor-read-char cur)))))))

;;;; ======================================================================
;;;; Main Dispatch
;;;; ======================================================================

(defun read-syntax-node (cur)
  "Read one syntax node from the cursor. Returns a syntax-node or NIL at EOF."
  (when (cursor-eof-p cur)
    (return-from read-syntax-node nil))
  (let ((ch (cursor-peek cur)))
    (handler-case
        (cond
          ;; Whitespace
          ((whitespacep ch)
           (read-whitespace-node cur))

          ;; Line comment
          ((char= ch #\;)
           (read-line-comment-node cur))

          ;; String
          ((char= ch #\")
           (read-string-node cur))

          ;; List
          ((char= ch #\()
           (read-list-node cur))

          ;; Close delimiters at top level
          ((char= ch #\))
           (let ((start (cursor-pos cur)))
             (cursor-read-char cur)
             (make-error-node cur start "Unexpected )")))
          ((char= ch #\])
           (let ((start (cursor-pos cur)))
             (cursor-read-char cur)
             (make-error-node cur start "Unexpected ]")))
          ((char= ch #\})
           (let ((start (cursor-pos cur)))
             (cursor-read-char cur)
             (make-error-node cur start "Unexpected }")))

          ;; Vector [...]
          ((char= ch #\[)
           (read-vector-node cur))

          ;; Map {...}
          ((char= ch #\{)
           (read-map-node cur))

          ;; Quote
          ((char= ch #\')
           (read-quote-node cur))

          ;; Backquote
          ((char= ch #\`)
           (read-backquote-node cur))

          ;; Unquote
          ((char= ch #\,)
           (read-unquote-node cur))

          ;; Dispatch
          ((char= ch #\#)
           (read-dispatch-node cur))

          ;; Number or symbol
          (t
           (read-number-or-symbol-node cur)))
      (error (e)
        (let ((start (cursor-pos cur)))
          (synchronize cur)
          (make-syntax-node :kind :reader-error
                            :location (make-loc cur start (cursor-pos cur))
                            :datum (format nil "~A" e)))))))

;;;; ======================================================================
;;;; Public API
;;;; ======================================================================

(defun read-syntax (cursor)
  "Read one syntax node from CURSOR. Returns a syntax-node or NIL at EOF."
  (read-syntax-node cursor))

(defun read-all-syntax (source)
  "Read all syntax nodes from SOURCE string.
   Returns a list of syntax-nodes including whitespace and comments."
  (let ((cur (make-cursor :source source))
        (nodes '()))
    (loop
      (when (cursor-eof-p cur)
        (return (nreverse nodes)))
      (let ((node (read-syntax-node cur)))
        (when node
          (push node nodes))))))

(defun read-file-syntax (path)
  "Read all syntax nodes from file at PATH.
   Returns a list of syntax-nodes with file-aware source-locations."
  (let ((source (with-open-file (in path :direction :input
                                         :external-format :utf-8)
                  (let ((s (make-string (file-length in))))
                    (read-sequence s in)
                    ;; Trim to actual length (file-length may overcount for UTF-8)
                    (if (find #\Nul s :from-end t)
                        (string-right-trim '(#\Nul) s)
                        s)))))
    (let ((cur (make-cursor :source source :file path))
          (nodes '()))
      (loop
        (when (cursor-eof-p cur)
          (return (nreverse nodes)))
        (let ((node (read-syntax-node cur)))
          (when node
            (push node nodes)))))))

(defun node-text (source node)
  "Extract the original source text for NODE."
  (let ((loc (syntax-node-location node)))
    (subseq source (loc:source-location-offset loc) (loc:source-location-end-offset loc))))

(defparameter *cst-elided*
  (list :cst-elided)
  "Sentinel returned by SYNTAX-NODE-TO-DATUM for forms that the reader
   should drop -- the form on the unselected side of a #+/#- reader
   conditional.  SYNTAX-NODES-TO-DATA filters this sentinel out by EQ
   identity so it never appears in the resulting list.  A unique cons is
   used so it cannot collide with any plausible source datum.")

(defun feature-expression-true-p (feature)
  "Evaluate a CL feature expression against *features*.  FEATURE is a
   symbol (matched by symbol-name so packaging differences don't matter)
   or a list whose head is AND, OR, or NOT (matched by name).  Returns T
   when the feature should be considered present."
  (cond
    ((symbolp feature)
     (let ((name (symbol-name feature)))
       (loop for f in *features*
             thereis (and (symbolp f) (string= (symbol-name f) name)))))
    ((and (consp feature) (symbolp (first feature)))
     (let ((op (symbol-name (first feature)))
           (rest (rest feature)))
       (cond ((string= op "AND") (every #'feature-expression-true-p rest))
             ((string= op "OR")  (some  #'feature-expression-true-p rest))
             ((string= op "NOT") (not   (feature-expression-true-p (first rest))))
             (t nil))))
    (t nil)))

(defun reader-conditional-form-node (node)
  "Return the inner form node for a :reader-conditional-plus or -minus
   NODE -- the second non-trivia child (the first is the feature
   expression).  Returns NIL when the conditional has no form."
  (let ((non-trivia
          (loop for c in (syntax-node-children node)
                unless (member (syntax-node-kind c)
                               '(:whitespace :comment))
                  collect c)))
    (when (cdr non-trivia)
      (second non-trivia))))

(defun reader-conditional-feature-datum (node)
  "Return the feature-expression datum for a reader-conditional NODE,
   or NIL if absent."
  (let ((non-trivia
          (loop for c in (syntax-node-children node)
                unless (member (syntax-node-kind c)
                               '(:whitespace :comment))
                  collect c)))
    (when non-trivia
      (syntax-node-to-datum (first non-trivia)))))

(defun syntax-node-to-datum (node)
  "Extract a plain Lisp value from NODE, recursively processing children.
   Strips whitespace and comment nodes.  For reader conditionals the
   feature expression is evaluated against *features* and the unselected
   form is replaced with *CST-ELIDED* (which SYNTAX-NODES-TO-DATA
   filters).  For #. read-time eval, the inner form's datum is evaluated
   when CL:*READ-EVAL* is true; errors during eval are swallowed and the
   raw inner datum returned."
  (case (syntax-node-kind node)
    ;; Atoms - return datum directly
    ((:number :string :symbol :keyword :character :uninterned-symbol)
     (syntax-node-datum node))
    ;; Compound - process children, filter trivia
    (:list
     (let ((data (syntax-nodes-to-data (syntax-node-children node))))
       data))
    (:vector
     (apply #'vector (syntax-nodes-to-data (syntax-node-children node))))
    (:cl-vector
     (apply #'vector (syntax-nodes-to-data (syntax-node-children node))))
    (:map
     (syntax-nodes-to-data (syntax-node-children node)))
    (:set
     (syntax-nodes-to-data (syntax-node-children node)))
    ;; Sugar
    (:quote
     (let ((inner (syntax-nodes-to-data (syntax-node-children node))))
       (list 'quote (first inner))))
    (:backquote
     (let ((inner (syntax-nodes-to-data (syntax-node-children node))))
       (list 'backquote (first inner))))
    (:unquote
     (let ((inner (syntax-nodes-to-data (syntax-node-children node))))
       (list 'unquote (first inner))))
    (:splice
     (let ((inner (syntax-nodes-to-data (syntax-node-children node))))
       (list 'splice (first inner))))
    (:function
     (let ((inner (syntax-nodes-to-data (syntax-node-children node))))
       (list 'function (first inner))))
    ;; Reader conditionals
    (:reader-conditional-plus
     (let ((feature (reader-conditional-feature-datum node))
           (form (reader-conditional-form-node node)))
       (if (and form (feature-expression-true-p feature))
           (syntax-node-to-datum form)
           *cst-elided*)))
    (:reader-conditional-minus
     (let ((feature (reader-conditional-feature-datum node))
           (form (reader-conditional-form-node node)))
       (if (and form (not (feature-expression-true-p feature)))
           (syntax-node-to-datum form)
           *cst-elided*)))
    ;; #. read-time eval
    (:read-eval
     (let* ((non-trivia (loop for c in (syntax-node-children node)
                              unless (member (syntax-node-kind c)
                                             '(:whitespace :comment))
                                collect c))
            (inner (when non-trivia
                     (syntax-node-to-datum (first non-trivia)))))
       (cond ((null non-trivia) nil)
             ((not *read-eval*) inner)
             (t (handler-case (eval inner)
                  (error () inner))))))
    ;; Trivia returns nothing meaningful
    ((:whitespace :comment) nil)
    ;; Error nodes
    (:reader-error (syntax-node-datum node))
    ;; Default
    (otherwise (syntax-node-datum node))))

(defun syntax-nodes-to-data (nodes)
  "Extract plain Lisp values from NODES, skipping whitespace, comments,
   and forms that the reader conditionals elided."
  (let ((acc nil))
    (dolist (node nodes (nreverse acc))
      (let ((kind (syntax-node-kind node)))
        (unless (member kind '(:whitespace :comment))
          (let ((datum (syntax-node-to-datum node)))
            (unless (eq datum *cst-elided*)
              (push datum acc))))))))

;;;; ======================================================================
;;;; Node Search
;;;; ======================================================================

(defun find-node-at-offset (nodes offset)
  "Find the deepest syntax-node containing byte OFFSET.
   Searches through NODES (a flat list of top-level nodes).
   Returns the node, or NIL if no node contains the offset."
  (labels ((search-node (node)
             (let ((loc (syntax-node-location node)))
               (when (and (<= (loc:source-location-offset loc) offset)
                          (< offset (loc:source-location-end-offset loc)))
                 ;; This node contains the offset; check children
                 (dolist (child (syntax-node-children node))
                   (let ((found (search-node child)))
                     (when found
                       (return-from search-node found))))
                 node))))
    (dolist (node nodes)
      (let ((found (search-node node)))
        (when found
          (return found))))))

(defun find-ancestor-chain (nodes offset)
  "Find the chain of nodes from outermost to innermost containing OFFSET.
   Returns a list where the first element is the top-level node and the last
   is the deepest node containing OFFSET.  Returns NIL if no node contains it.
   Whitespace nodes are excluded from the chain."
  (labels ((search-node (node path)
             (let ((loc (syntax-node-location node)))
               (when (and (<= (loc:source-location-offset loc) offset)
                          (< offset (loc:source-location-end-offset loc)))
                 (let ((new-path (if (member (syntax-node-kind node)
                                            '(:whitespace))
                                     path
                                     (append path (list node)))))
                   ;; Try to descend into children
                   (dolist (child (syntax-node-children node))
                     (let ((found (search-node child new-path)))
                       (when found
                         (return-from search-node found))))
                   ;; No deeper child matched -- return current path
                   new-path)))))
    (dolist (node nodes)
      (let ((found (search-node node nil)))
        (when found
          (return found))))))

(defun find-nodes-in-range (nodes start end)
  "Find all syntax-nodes overlapping the range [START, END).
   Returns a list of nodes."
  (let ((result '()))
    (labels ((search-node (node)
               (let ((loc (syntax-node-location node)))
                 (when (and (< (loc:source-location-offset loc) end)
                            (> (loc:source-location-end-offset loc) start))
                   (push node result)
                   (dolist (child (syntax-node-children node))
                     (search-node child))))))
      (dolist (node nodes)
        (search-node node)))
    (nreverse result)))
