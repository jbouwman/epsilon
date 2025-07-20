;;;;  TODO
;;;;
;;;;  String Processing Utilities
;;;;  - maybe-coerce-to-simple-string - string coercion
;;;;  - quote-meta-chars - string escaping
;;;;
;;;;  The lexer struct and parsing predicates (end-of-string-p,
;;;;  looking-at-p) could become epsilon.lib.lexer.

(defpackage #:epsilon.lib.regex
  (:use #:cl
        #:epsilon.lib.syntax
        #:epsilon.lib.symbol
        #:epsilon.lib.type)
  (:local-nicknames
   (:str :epsilon.lib.string)
   (:charset :epsilon.lib.charset))
  (:import-from :epsilon.lib.string
                #:word-char-p
                #:whitespacep
                #:nsubseq
                #:string-list-to-simple-string)
  (:shadow
   #:compile
   #:search)
  (:export
   #:scan
   #:scan-to-strings
   #:search
   #:match
   #:findall
   #:finditer
   #:sub
   #:subn
   #:compile))

(in-package #:epsilon.lib.regex)

(defvar *extended-mode-p* nil
  "Whether the parser will start in extended mode.")

(declaim (boolean *extended-mode-p*))


(defvar *string* (make-sequence 'simple-string 0)
  "The string which is currently scanned by SCAN.
Will always be coerced to a SIMPLE-STRING.")

(declaim (simple-string *string*))

(defvar *start-pos* 0
  "Where to start scanning within *STRING*.")

(declaim (fixnum *start-pos*))

(defvar *real-start-pos* nil
  "The real start of *STRING*. This is for repeated scans and is only used internally.")

(declaim (type (or null fixnum) *real-start-pos*))

(defvar *end-pos* 0
  "Where to stop scanning within *STRING*.")

(declaim (fixnum *end-pos*))

(defvar *reg-starts* (make-array 0)
  "An array which holds the start positions
of the current register candidates.")

(declaim (simple-vector *reg-starts*))

(defvar *regs-maybe-start* (make-array 0)
  "An array which holds the next start positions
of the current register candidates.")

(declaim (simple-vector *regs-maybe-start*))

(defvar *reg-ends* (make-array 0)
  "An array which holds the end positions
of the current register candidates.")

(declaim (simple-vector *reg-ends*))

(defvar *end-string-pos* nil
  "Start of the next possible end-string candidate.")

(defvar *rep-num* 0
  "Counts the number of \"complicated\" repetitions while the matchers
are built.")

(declaim (fixnum *rep-num*))

(defvar *zero-length-num* 0
  "Counts the number of repetitions the inner regexes of which may
have zero-length while the matchers are built.")

(declaim (fixnum *zero-length-num*))

(defvar *repeat-counters* (make-array 0
                                      :initial-element 0
                                      :element-type 'fixnum)
  "An array to keep track of how often
repetitive patterns have been tested already.")

(declaim (type (array fixnum (*)) *repeat-counters*))

(defvar *last-pos-stores* (make-array 0)
  "An array to keep track of the last positions
where we saw repetitive patterns.
Only used for patterns which might have zero length.")

(declaim (simple-vector *last-pos-stores*))

(defvar *use-bmh-matchers* nil
  "Whether the scanners created by COMPILE should use the \(fast
but large) Boyer-Moore-Horspool matchers.")

(defvar *optimize-char-classes* nil
  "Whether character classes should be compiled into look-ups into
O\(1) data structures.  This is usually fast but will be costly in
terms of scanner creation time and might be costly in terms of size if
*REGEX-CHAR-CODE-LIMIT* is high.  This value will be used as the :KIND
keyword argument to CREATE-OPTIMIZED-TEST-FUNCTION - see there for the
possible non-NIL values.")

(defvar *property-resolver* nil
  "Should be NIL or a designator for a function which accepts strings
and returns unary character test functions or NIL.  This 'resolver' is
intended to handle `character properties' like \\p{IsAlpha}.  If
*PROPERTY-RESOLVER* is NIL, then the parser will simply treat \\p and
\\P as #\\p and #\\P as in older versions of CL-PPCRE.")

(defvar *allow-quoting* nil
  "Whether the parser should support Perl's \\Q and \\E.")

(defmacro maybe-coerce-to-simple-string (string)
  "Coerces STRING to a simple STRING unless it already is one."
  (with-unique-names (=string=)
    `(let ((,=string= ,string))
       (cond ((simple-string-p ,=string=)
              ,=string=)
             (t
              (coerce ,=string=
                      'simple-string))))))


(defvar *syntax-error-string* nil
  "The string which caused the syntax error.")

(define-condition regex-syntax-error (simple-error)
  ((string :initarg :string
           :reader regex-syntax-error-string)
   (pos :initarg :pos
        :reader regex-syntax-error-pos))
  (:default-initargs
   :pos nil
   :string *syntax-error-string*)
  (:report (lambda (condition stream)
             (format stream "~?~@[ at position ~A~]~@[ in string ~S~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (regex-syntax-error-pos condition)
                     (regex-syntax-error-string condition))))
  (:documentation "Signaled if the parser encounters an error
when trying to parse a regex string or to convert a parse tree into
its internal representation."))

(setf (documentation 'regex-syntax-error-string 'function)
      "Returns the string the parser was parsing when the error was
encountered \(or NIL if the error happened while trying to convert a
parse tree).")

(setf (documentation 'regex-syntax-error-pos 'function)
      "Returns the position within the string where the error occurred
\(or NIL if the error happened while trying to convert a parse tree")

(define-condition regex-invocation-error (simple-error)
  ()
  (:documentation "Signaled when functions are
invoked with wrong arguments."))

(defmacro signal-syntax-error* (pos format-control &rest format-arguments)
  `(error 'regex-syntax-error
          :pos ,pos
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-syntax-error (format-control &rest format-arguments)
  `(signal-syntax-error* nil ,format-control ,@format-arguments))

(defmacro signal-invocation-error (format-control &rest format-arguments)
  `(error 'regex-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))




(declaim (inline map-char-to-special-class))

(defun map-char-to-special-char-class (chr)
  "Maps escaped characters like \"\\d\" to the tokens which represent
their associated character classes."
  (case chr
    ((#\d)
     :digit-class)
    ((#\D)
     :non-digit-class)
    ((#\w)
     :word-char-class)
    ((#\W)
     :non-word-char-class)
    ((#\s)
     :whitespace-char-class)
    ((#\S)
     :non-whitespace-char-class)))

(declaim (inline make-lexer-internal))

(defstruct (lexer (:constructor make-lexer-internal))
  "LEXER structures are used to hold the regex string which is
currently lexed and to keep track of the lexer's state."
  (str "" :type string :read-only t)
  (len 0 :type fixnum :read-only t)
  (reg 0 :type fixnum)
  (pos 0 :type fixnum)
  (last-pos nil :type list))

(defun make-lexer (string)
  (declare (string string))
  (make-lexer-internal :str (maybe-coerce-to-simple-string string)
                       :len (length string)))

(declaim (inline end-of-string-p))

(defun end-of-string-p (lexer)
  "Tests whether we're at the end of the regex string."
  (<= (lexer-len lexer)
      (lexer-pos lexer)))

(declaim (inline looking-at-p))

(defun looking-at-p (lexer chr)
  "Tests whether the next character the lexer would see is CHR.
Does not respect extended mode."
  (and (not (end-of-string-p lexer))
       (char= (schar (lexer-str lexer) (lexer-pos lexer))
              chr)))

(declaim (inline next-char-non-extended))

(defun next-char-non-extended (lexer)
  "Returns the next character which is to be examined and updates the
POS slot. Does not respect extended mode."
  (cond ((end-of-string-p lexer) nil)
        (t (prog1
               (schar (lexer-str lexer) (lexer-pos lexer))
             (incf (lexer-pos lexer))))))

(defun next-char (lexer)
  "Returns the next character which is to be examined and updates the
POS slot. Respects extended mode, i.e.  whitespace, comments, and also
nested comments are skipped if applicable."
  (let ((next-char (next-char-non-extended lexer))
        last-loop-pos)
    (loop
      (setq last-loop-pos (lexer-pos lexer))
      (when (and next-char
                 (char= next-char #\()
                 (looking-at-p lexer #\?))
        (incf (lexer-pos lexer))
        (cond ((looking-at-p lexer #\#)
               (let ((error-pos (- (lexer-pos lexer) 2)))
                 (unless
                     (loop for skip-char = next-char
                             then (next-char-non-extended lexer)
                           while (and skip-char
                                      (char/= skip-char #\)))
                           finally (return skip-char))
                   (signal-syntax-error* error-pos "Comment group not closed.")))
               (setq next-char (next-char-non-extended lexer)))
              (t
               (decf (lexer-pos lexer)))))
      (when *extended-mode-p*
        (loop while (and next-char
                         (or (char= next-char #\#)
                             (whitespacep next-char)))
              do (setq next-char
                       (if (char= next-char #\#)
                           (loop for skip-char = next-char
                                   then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (char/= skip-char #\Newline))
                                 finally (return (next-char-non-extended lexer)))
                           (loop for skip-char = next-char
                                   then (next-char-non-extended lexer)
                                 while (and skip-char
                                            (whitespacep skip-char))
                                 finally (return skip-char))))))
      (unless (> (lexer-pos lexer) last-loop-pos)
        (return next-char)))))

(declaim (inline fail))

(defun fail (lexer)
  "Moves (LEXER-POS LEXER) back to the last position stored in
\(LEXER-LAST-POS LEXER) and pops the LAST-POS stack."
  (unless (lexer-last-pos lexer)
    (signal-syntax-error "LAST-POS stack of LEXER ~A is empty." lexer))
  (setf (lexer-pos lexer) (pop (lexer-last-pos lexer)))
  nil)

(defun get-number (lexer &key (radix 10) max-length no-whitespace-p)
  "Read and consume the number the lexer is currently looking at and
return it. Returns NIL if no number could be identified.
RADIX is used as in PARSE-INTEGER. If MAX-LENGTH is not NIL we'll read
at most the next MAX-LENGTH characters. If NO-WHITESPACE-P is not NIL
we don't tolerate whitespace in front of the number."
  (when (or (end-of-string-p lexer)
            (and no-whitespace-p
                 (whitespacep (schar (lexer-str lexer) (lexer-pos lexer)))))
    (return-from get-number nil))
  (multiple-value-bind (integer new-pos)
      (parse-integer (lexer-str lexer)
                     :start (lexer-pos lexer)
                     :end (if max-length
                              (let ((end-pos (+ (lexer-pos lexer)
                                                (the fixnum max-length)))
                                    (lexer-len (lexer-len lexer)))
                                (if (< end-pos lexer-len)
                                    end-pos
                                    lexer-len))
                              (lexer-len lexer))
                     :radix radix
                     :junk-allowed t)
    (cond ((and integer (>= (the fixnum integer) 0))
           (setf (lexer-pos lexer) new-pos)
           integer)
          (t nil))))

(declaim (inline try-number))

(defun try-number (lexer &key (radix 10) max-length no-whitespace-p)
  "Like GET-NUMBER but won't consume anything if no number is seen."
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((number (get-number lexer
                            :radix radix
                            :max-length max-length
                            :no-whitespace-p no-whitespace-p)))
    (or number (fail lexer))))

(declaim (inline make-char-from-code))

(defun make-char-from-code (number error-pos)
  "Create character from char-code NUMBER. NUMBER can be NIL
which is interpreted as 0. ERROR-POS is the position where
the corresponding number started within the regex string."
  (let ((code (logand #o377 (the fixnum (or number 0)))))
    (or (and (< code char-code-limit)
             (code-char code))
        (signal-syntax-error* error-pos "No character for hex-code ~X." number))))

(defun unescape-char (lexer)
  "Convert the characters\(s) following a backslash into a token
which is returned. This function is to be called when the backslash
has already been consumed. Special character classes like \\W are
handled elsewhere."
  (when (end-of-string-p lexer)
    (signal-syntax-error "String ends with backslash."))
  (let ((chr (next-char-non-extended lexer)))
    (case chr
      ((#\E)
       (if *allow-quoting*
           :void
           #\E))
      ((#\c)
       (let ((next-char (next-char-non-extended lexer)))
         (unless next-char
           (signal-syntax-error* (lexer-pos lexer) "Character missing after '\\c'"))
         (code-char (logxor #x40 (char-code (char-upcase next-char))))))
      ((#\x)
       (let* ((error-pos (lexer-pos lexer))
              (number (get-number lexer :radix 16 :max-length 2 :no-whitespace-p t)))
         (make-char-from-code number error-pos)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (let* ((error-pos (decf (lexer-pos lexer)))
              (number (get-number lexer :radix 8 :max-length 3)))
         (make-char-from-code number error-pos)))
      ((#\t)
       #\Tab)
      ((#\n)
       #\Newline)
      ((#\r)
       #\Return)
      ((#\f)
       #\Page)
      ((#\b)
       #\Backspace)
      ((#\a)
       (code-char 7))                  ; ASCII bell
      ((#\e)
       (code-char 27))                 ; ASCII escape
      (otherwise
       chr))))

(defun read-char-property (lexer first-char)
  (unless (eql (next-char-non-extended lexer) #\{)
    (signal-syntax-error* (lexer-pos lexer) "Expected left brace after \\~A." first-char))
  (let ((name (with-output-to-string (out nil :element-type
                                          'character)
                (loop
                  (let ((char (or (next-char-non-extended lexer)
                                  (signal-syntax-error "Unexpected EOF after \\~A{." first-char))))
                    (when (char= char #\})
                      (return))
                    (write-char char out))))))
    (list (if (char= first-char #\p) :property :inverted-property)
          name)))

(defun collect-char-class (lexer)
  "Reads and consumes characters from regex string until a right
bracket is seen.  Assembles them into a list \(which is returned) of
characters, character ranges, like \(:RANGE #\\A #\\E) for a-e, and
tokens representing special character classes."
  (let ((start-pos (lexer-pos lexer))         ; remember start for error message
        hyphen-seen
        last-char
        list)
    (flet ((handle-char (c)
             "Do the right thing with character C depending on whether
we're inside a range or not."
             (cond ((and hyphen-seen last-char)
                    (setf (car list) (list :range last-char c)
                          last-char nil))
                   (t
                    (push c list)
                    (setq last-char c)))
             (setq hyphen-seen nil)))
      (loop for first = t then nil
            for c = (next-char-non-extended lexer)
            while c
            do (cond
                 ((char= c #\\)
                  (let ((next-char (next-char-non-extended lexer)))
                    (case next-char
                      ((#\d #\D #\w #\W #\s #\S)
                       (push (map-char-to-special-char-class next-char) list)
                       (when hyphen-seen
                         (push #\- list))
                       (when (looking-at-p lexer #\-)
                         (push #\- list)
                         (incf (lexer-pos lexer)))
                       (setq hyphen-seen nil))
                      ((#\P #\p)
                       (cond ((null *property-resolver*)
                              (handle-char next-char))
                             (t
                              (push (read-char-property lexer next-char) list)
                              (when hyphen-seen
                                (push #\- list))
                              (when (looking-at-p lexer #\-)
                                (push #\- list)
                                (incf (lexer-pos lexer)))
                              (setq hyphen-seen nil))))
                      ((#\E)
                       (unless *allow-quoting*
                         (handle-char #\E)))
                      (otherwise
                       (decf (lexer-pos lexer))
                       (handle-char (unescape-char lexer))))))
                 (first
                  (handle-char c))
                 ((char= c #\])
                  (when hyphen-seen
                    (setq hyphen-seen nil)
                    (handle-char #\-))
                  (return-from collect-char-class (nreverse list)))
                 ((and (char= c #\-)
                       last-char
                       (not hyphen-seen))
                  (setq hyphen-seen t))
                 ((char= c #\-)
                  (handle-char #\-))
                 (t
                  (handle-char c))))
      (signal-syntax-error* start-pos "Missing right bracket to close character class."))))

(defun maybe-parse-flags (lexer)
  "Reads a sequence of modifiers \(including #\\- to reverse their
meaning) and returns a corresponding list of \"flag\" tokens.  The
\"x\" modifier is treated specially in that it dynamically modifies
the behaviour of the lexer itself via the special variable
*EXTENDED-MODE-P*."
  (prog1
      (loop with set = t
            for chr = (next-char-non-extended lexer)
            unless chr
              do (signal-syntax-error "Unexpected end of string.")
            while (find chr "-imsx" :test #'char=)
            if (char= chr #\-)
              do (setq set nil)
            else if (char= chr #\x)
                   do (setq *extended-mode-p* set)
            else collect (if set
                             (case chr
                               ((#\i)
                                :case-insensitive-p)
                               ((#\m)
                                :multi-line-mode-p)
                               ((#\s)
                                :single-line-mode-p))
                             (case chr
                               ((#\i)
                                :case-sensitive-p)
                               ((#\m)
                                :not-multi-line-mode-p)
                               ((#\s)
                                :not-single-line-mode-p))))
    (decf (lexer-pos lexer))))

(defun get-quantifier (lexer)
  "Returns a list of two values (min max) if what the lexer is looking
at can be interpreted as a quantifier. Otherwise returns NIL and
resets the lexer to its old position."
  (push (lexer-pos lexer) (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (case next-char
      ((#\*)
       '(0 nil))
      ((#\+)
       '(1 nil))
      ((#\?)
       '(0 1))
      ((#\{)
       (let ((num1 (get-number lexer :no-whitespace-p t)))
         (if num1
             (let ((next-char (next-char-non-extended lexer)))
               (case next-char
                 ((#\,)
                  (let* ((num2 (get-number lexer :no-whitespace-p t))
                         (next-char (next-char-non-extended lexer)))
                    (case next-char
                      ((#\})
                       (list num1 num2))
                      (otherwise
                       (fail lexer)))))
                 ((#\})
                  (list num1 num1))
                 (otherwise
                  (fail lexer))))
             (fail lexer))))
      (otherwise
       (fail lexer)))))

(defun parse-register-name-aux (lexer)
  "Reads and returns the name in a named register group.  It is
assumed that the starting #\< character has already been read.  The
closing #\> will also be consumed."
  (let ((end-name (position #\>
                            (lexer-str lexer)
                            :start (lexer-pos lexer)
                            :test #'char=)))
    (unless end-name
      (signal-syntax-error* (1- (lexer-pos lexer)) "Opening #\< in named group has no closing #\>."))
    (let ((name (subseq (lexer-str lexer)
                        (lexer-pos lexer)
                        end-name)))
      (unless (every #'(lambda (char)
                         (or (alphanumericp char)
                             (char= #\- char)))
                     name)
        (signal-syntax-error* (lexer-pos lexer) "Invalid character in named register group."))
      (setf (lexer-pos lexer) (1+ end-name))
      name)))

(declaim (inline unget-token))

(defun unget-token (lexer)
  "Moves the lexer back to the last position stored in the LAST-POS stack."
  (if (lexer-last-pos lexer)
      (setf (lexer-pos lexer)
            (pop (lexer-last-pos lexer)))
      (error "No token to unget \(this should not happen)")))

(defun get-token (lexer)
  "Returns and consumes the next token from the regex string \(or NIL)."
  (push (lexer-pos lexer)
        (lexer-last-pos lexer))
  (let ((next-char (next-char lexer)))
    (cond (next-char
           (case next-char
             ((#\))
              :close-paren)
             ((#\|)
              :vertical-bar)
             ((#\?)
              :question-mark)
             ((#\.)
              :everything)
             ((#\^)
              :start-anchor)
             ((#\$)
              :end-anchor)
             ((#\+ #\*)
              (signal-syntax-error* (1- (lexer-pos lexer)) "Quantifier '~A' not allowed." next-char))
             ((#\{)
              (let ((this-pos (lexer-pos lexer))
                    (this-last-pos (lexer-last-pos lexer)))
                (unget-token lexer)
                (when (get-quantifier lexer)
                  (signal-syntax-error* (car this-last-pos)
                                        "Quantifier '~A' not allowed."
                                        (subseq (lexer-str lexer)
                                                (car this-last-pos)
                                                (lexer-pos lexer))))
                (setf (lexer-pos lexer) this-pos
                      (lexer-last-pos lexer) this-last-pos)
                next-char))
             ((#\[)
              (cons  (cond ((looking-at-p lexer #\^)
                            (incf (lexer-pos lexer))
                            :inverted-char-class)
                           (t
                            :char-class))
                     (collect-char-class lexer)))
             ((#\\)
              (let ((next-char (next-char-non-extended lexer)))
                (case next-char
                  ((#\A)
                   :modeless-start-anchor)
                  ((#\Z)
                   :modeless-end-anchor)
                  ((#\z)
                   :modeless-end-anchor-no-newline)
                  ((#\b)
                   :word-boundary)
                  ((#\B)
                   :non-word-boundary)
                  ((#\k)
                   #\k)
                  ((#\d #\D #\w #\W #\s #\S)
                   (map-char-to-special-char-class next-char))
                  ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (let* ((old-pos (decf (lexer-pos lexer)))
                          (backref-number (get-number lexer)))
                     (declare (fixnum backref-number))
                     (cond ((and (> backref-number (lexer-reg lexer))
                                 (<= 10 backref-number))
                            (setf (lexer-pos lexer) old-pos)
                            (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                                 old-pos))
                           (t
                            (list :back-reference backref-number)))))
                  ((#\0)
                   (let ((old-pos (decf (lexer-pos lexer))))
                     (make-char-from-code (get-number lexer :radix 8 :max-length 3)
                                          old-pos)))
                  ((#\P #\p)
                   (cond (*property-resolver* (read-char-property lexer next-char))
                         (t next-char)))
                  (otherwise
                   (decf (lexer-pos lexer))
                   (unescape-char lexer)))))
             ((#\()
              (cond ((looking-at-p lexer #\?)
                     (incf (lexer-pos lexer))
                     (let* ((flags (maybe-parse-flags lexer))
                            (next-char (next-char-non-extended lexer)))
                       (when (and flags
                                  (not (find next-char ":)" :test #'char=)))
                         (signal-syntax-error* (car (lexer-last-pos lexer))
                                               "Sequence '~A' not recognized."
                                               (subseq (lexer-str lexer)
                                                       (car (lexer-last-pos lexer))
                                                       (lexer-pos lexer))))
                       (case next-char
                         ((nil)
                          (signal-syntax-error "End of string following '(?'."))
                         ((#\))
                          (or (and flags
                                   (cons :flags flags))
                              :void))
                         ((#\()
                          :open-paren-paren)
                         ((#\>)
                          :open-paren-greater)
                         ((#\=)
                          :open-paren-equal)
                         ((#\!)
                          :open-paren-exclamation)
                         ((#\:)
                          (values :open-paren-colon flags))
                         ((#\<)
                          (let ((next-char (next-char-non-extended lexer)))
                            (cond ((and next-char
                                        (alpha-char-p next-char))
                                   (signal-syntax-error* (1- (lexer-pos lexer))
                                                         "Character '~A' may not follow '(?<'"
                                                         next-char)
                                   (decf (lexer-pos lexer))
                                   :open-paren-less-letter)
                                  (t
                                   (case next-char
                                     ((#\=)
                                      :open-paren-less-equal)
                                     ((#\!)
                                      :open-paren-less-exclamation)
                                     ((#\))
                                      :void)
                                     ((nil)
                                      (signal-syntax-error "End of string following '(?<'."))
                                     (t
                                      (signal-syntax-error* (1- (lexer-pos lexer))
                                                            "Character '~A' may not follow '(?<'."
                                                            next-char )))))))
                         (otherwise
                          (signal-syntax-error* (1- (lexer-pos lexer))
                                                "Character '~A' may not follow '(?'."
                                                next-char)))))
                    (t
                     :open-paren)))
             (otherwise
              next-char)))
          (t
           (pop (lexer-last-pos lexer))
           nil))))

(declaim (inline start-of-subexpr-p))

(defun start-of-subexpr-p (lexer)
  "Tests whether the next token can start a valid sub-expression, i.e.
a stand-alone regex."
  (let* ((pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (not (or (null next-char)
             (prog1
                 (member (the character next-char)
                         '(#\) #\|)
                         :test #'char=)
               (setf (lexer-pos lexer) pos))))))

(defun group (lexer)
  "Parses and consumes a <group>.
The productions are: <group> -> \"\(\"<regex>\")\"
                                \"\(?:\"<regex>\")\"
                                \"\(?>\"<regex>\")\"
                                \"\(?<flags>:\"<regex>\")\"
                                \"\(?=\"<regex>\")\"
                                \"\(?!\"<regex>\")\"
                                \"\(?<=\"<regex>\")\"
                                \"\(?<!\"<regex>\")\"
                                \"\(?\(\"<num>\")\"<regex>\")\"
                                \"\(?\(\"<regex>\")\"<regex>\")\"
                                <legal-token>
where <flags> is parsed by the lexer function MAYBE-PARSE-FLAGS.
Will return <parse-tree> or \(<grouping-type> <parse-tree>) where
<grouping-type> is one of six keywords - see source for details."
  (multiple-value-bind (open-token flags)
      (get-token lexer)
    (cond ((eq open-token :open-paren-paren)
           (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                  (number (try-number lexer :no-whitespace-p t))
                  (*extended-mode-p* *extended-mode-p*))
             (declare (fixnum open-paren-pos))
             (cond (number
                    (let* ((inner-close-token (get-token lexer))
                           (reg-expr (reg-expr lexer))
                           (close-token (get-token lexer)))
                      (unless (eq inner-close-token :close-paren)
                        (signal-syntax-error* (+ open-paren-pos 2)
                                              "Opening paren has no matching closing paren."))
                      (unless (eq close-token :close-paren)
                        (signal-syntax-error* open-paren-pos
                                              "Opening paren has no matching closing paren."))
                      (list :branch number reg-expr)))
                   (t
                    (decf (lexer-pos lexer))
                    (let* ((inner-reg-expr (group lexer))
                           (reg-expr (reg-expr lexer))
                           (close-token (get-token lexer)))
                      (unless (eq close-token :close-paren)
                        (signal-syntax-error* open-paren-pos
                                              "Opening paren has no matching closing paren."))
                      (list :branch inner-reg-expr reg-expr))))))
          ((member open-token '(:open-paren
                                :open-paren-colon
                                :open-paren-greater
                                :open-paren-equal
                                :open-paren-exclamation
                                :open-paren-less-equal
                                :open-paren-less-exclamation
                                :open-paren-less-letter)
                   :test #'eq)
           (let ((*extended-mode-p* *extended-mode-p*))
             (let* ((open-paren-pos (car (lexer-last-pos lexer)))
                    (register-name (when (eq open-token :open-paren-less-letter)
                                     (parse-register-name-aux lexer)))
                    (reg-expr (reg-expr lexer))
                    (close-token (get-token lexer)))
               (when (or (eq open-token :open-paren)
                         (eq open-token :open-paren-less-letter))
                 (incf (lexer-reg lexer)))
               (unless (eq close-token :close-paren)
                 (signal-syntax-error* open-paren-pos
                                       "Opening paren has no matching closing paren."))
               (if flags
                   (cons :group (nconc flags (list reg-expr)))
                   (if (eq open-token :open-paren-less-letter)
                       (list :named-register register-name
                             reg-expr)
                       (list (case open-token
                               ((:open-paren)
                                :register)
                               ((:open-paren-colon)
                                :group)
                               ((:open-paren-greater)
                                :standalone)
                               ((:open-paren-equal)
                                :positive-lookahead)
                               ((:open-paren-exclamation)
                                :negative-lookahead)
                               ((:open-paren-less-equal)
                                :positive-lookbehind)
                               ((:open-paren-less-exclamation)
                                :negative-lookbehind))
                             reg-expr))))))
          (t
           open-token))))

(defun greedy-quant (lexer)
  "Parses and consumes a <greedy-quant>.
The productions are: <greedy-quant> -> <group> | <group><quantifier>
where <quantifier> is parsed by the lexer function GET-QUANTIFIER.
Will return <parse-tree> or (:GREEDY-REPETITION <min> <max> <parse-tree>)."
  (let* ((group (group lexer))
         (token (get-quantifier lexer)))
    (if token
        (list :greedy-repetition (first token) (second token) group)
        group)))

(defun quant (lexer)
  "Parses and consumes a <quant>.
The productions are: <quant> -> <greedy-quant> | <greedy-quant>\"?\".
Will return the <parse-tree> returned by GREEDY-QUANT and optionally
change :GREEDY-REPETITION to :NON-GREEDY-REPETITION."
  (let* ((greedy-quant (greedy-quant lexer))
         (pos (lexer-pos lexer))
         (next-char (next-char lexer)))
    (when next-char
      (if (char= next-char #\?)
          (setf (car greedy-quant) :non-greedy-repetition)
          (setf (lexer-pos lexer) pos)))
    greedy-quant))

(defun seq (lexer)
  "Parses and consumes a <seq>.
The productions are: <seq> -> <quant> | <quant><seq>.
Will return <parse-tree> or (:SEQUENCE <parse-tree> <parse-tree>)."
  (flet ((make-array-from-two-chars (char1 char2)
           (let ((string (make-array 2
                                     :element-type 'character
                                     :fill-pointer t
                                     :adjustable t)))
             (setf (aref string 0) char1)
             (setf (aref string 1) char2)
             string)))
    (if (start-of-subexpr-p lexer)
        (loop with seq-is-sequence-p = nil
              with last-cdr
              for quant = (quant lexer)
              for quant-is-char-p = (characterp quant)
              for seq = quant
                then
                (cond ((and quant-is-char-p (characterp seq))
                       (make-array-from-two-chars seq quant))
                      ((and quant-is-char-p (stringp seq))
                       (vector-push-extend quant seq)
                       seq)
                      ((not seq-is-sequence-p)
                       (setf last-cdr (list quant)
                             seq-is-sequence-p t)
                       (list* :sequence seq last-cdr))
                      ((and quant-is-char-p
                            (characterp (car last-cdr)))
                       (setf (car last-cdr)
                             (make-array-from-two-chars (car last-cdr)
                                                        quant))
                       seq)
                      ((and quant-is-char-p
                            (stringp (car last-cdr)))
                       (vector-push-extend quant (car last-cdr))
                       seq)
                      (t
                       (let ((cons (list quant)))
                         (psetf last-cdr cons
                                (cdr last-cdr) cons))
                       seq))
              while (start-of-subexpr-p lexer)
              finally (return seq))
        :void)))

(defun reg-expr (lexer)
  "Parses and consumes a <regex>, a complete regular expression.
The productions are: <regex> -> <seq> | <seq>\"|\"<regex>.
Will return <parse-tree> or (:ALTERNATION <parse-tree> <parse-tree>)."
  (let ((pos (lexer-pos lexer)))
    (case (next-char lexer)
      ((nil)
       :void)
      ((#\|)
       (list :alternation :void (reg-expr lexer)))
      (otherwise
       (setf (lexer-pos lexer) pos)
       (let* ((seq (seq lexer))
              (pos (lexer-pos lexer)))
         (case (next-char lexer)
           ((nil)
            seq)
           ((#\|)
            (let ((reg-expr (reg-expr lexer)))
              (cond ((and (consp reg-expr)
                          (eq (first reg-expr) :alternation))
                     (setf (cdr reg-expr)
                           (cons seq (cdr reg-expr)))
                     reg-expr)
                    (t (list :alternation seq reg-expr)))))
           (otherwise
            (setf (lexer-pos lexer) pos)
            seq)))))))

(defun parse-string (string)
  "Translate the regex string STRING into a parse tree."
  (let* ((lexer (make-lexer string))
         (parse-tree (reg-expr lexer)))
    (if (end-of-string-p lexer)
        parse-tree
        (signal-syntax-error* (lexer-pos lexer) "Expected end of string."))))

(defclass regex ()
  ()
  (:documentation "The REGEX base class.  All other classes inherit
from this one."))

(defclass seq (regex)
  ((elements :initarg :elements
             :accessor elements
             :type cons
             :documentation "A list of REGEX objects."))
  (:documentation "SEQ objects represents sequences of regexes.
\(Like \"ab\" is the sequence of \"a\" and \"b\".)"))

(defclass alternation (regex)
  ((choices :initarg :choices
            :accessor choices
            :type cons
            :documentation "A list of REGEX objects"))
  (:documentation "ALTERNATION objects represent alternations of
regexes.  \(Like \"a|b\" ist the alternation of \"a\" or \"b\".)"))

(defclass lookahead (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive."))
  (:documentation "LOOKAHEAD objects represent look-ahead assertions."))

(defclass lookbehind (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX object we're checking.")
   (positivep :initarg :positivep
              :reader positivep
              :documentation "Whether this assertion is positive.")
   (len :initarg :len
        :accessor len
        :type fixnum
        :documentation "The \(fixed) length of the enclosed regex."))
  (:documentation "LOOKBEHIND objects represent look-behind assertions."))

(defclass repetition (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The REGEX that's repeated.")
   (greedyp :initarg :greedyp
            :reader greedyp
            :documentation "Whether the repetition is greedy.")
   (minimum :initarg :minimum
            :accessor minimum
            :type fixnum
            :documentation "The minimal number of repetitions.")
   (maximum :initarg :maximum
            :accessor maximum
            :documentation "The maximal number of repetitions.
Can be NIL for unbounded.")
   (min-len :initarg :min-len
            :reader min-len
            :documentation "The minimal length of the enclosed regex.")
   (len :initarg :len
        :reader len
        :documentation "The length of the enclosed regex.  NIL if
unknown.")
   (min-rest :initform 0
             :accessor min-rest
             :type fixnum
             :documentation "The minimal number of characters which
must appear after this repetition.")
   (contains-register-p :initarg :contains-register-p
                        :reader contains-register-p
                        :documentation "Whether the regex contains a
register."))
  (:documentation "REPETITION objects represent repetitions of regexes."))

(defmethod print-object ((repetition repetition) stream)
  (print-unreadable-object (repetition stream :type t :identity t)
    (princ (regex repetition) stream)))

(defclass register (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex.")
   (num :initarg :num
        :reader num
        :type fixnum
        :documentation "The number of this register, starting from 0.
This is the index into *REGS-START* and *REGS-END*.")
   (name :initarg :name
         :reader name
         :documentation "Name of this register or NIL."))
  (:documentation "REGISTER objects represent register groups."))

(defmethod print-object ((register register) stream)
  (print-unreadable-object (register stream :type t :identity t)
    (princ (regex register) stream)))

(defclass standalone (regex)
  ((regex :initarg :regex
          :accessor regex
          :documentation "The inner regex."))
  (:documentation "A standalone regular expression."))

(defclass back-reference (regex)
  ((num :initarg :num
        :accessor num
        :type fixnum
        :documentation "The number of the register this
reference refers to.")
   (name :initarg :name
         :accessor name
         :documentation "The name of the register this
reference refers to or NIL.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "Whether we check
case-insensitively."))
  (:documentation "BACK-REFERENCE objects represent backreferences."))

(defclass char-class (regex)
  ((test-function :initarg :test-function
                  :reader test-function
                  :type (or function symbol nil)
                  :documentation "A unary function \(accepting a
character) which stands in for the character class and does the work
of checking whether a character belongs to the class."))
  (:documentation "CHAR-CLASS objects represent character classes."))

(defclass str (regex)
  ((str :initarg :str
        :accessor str
        :type string
        :documentation "The actual string.")
   (len :initform 0
        :accessor len
        :type fixnum
        :documentation "The length of the string.")
   (case-insensitive-p :initarg :case-insensitive-p
                       :reader case-insensitive-p
                       :documentation "If we match case-insensitively.")
   (offset :initform nil
           :accessor offset
           :documentation "Offset from the left of the whole
parse tree. The first regex has offset 0. NIL if unknown, i.e. behind
a variable-length regex.")
   (skip :initform nil
         :initarg :skip
         :accessor skip
         :documentation "If we can avoid testing for this
string because the SCAN function has done this already.")
   (start-of-end-string-p :initform nil
                          :accessor start-of-end-string-p
                          :documentation "If this is the unique
STR which starts END-STRING (a slot of MATCHER)."))
  (:documentation "STR objects represent string."))

(defmethod print-object ((str str) stream)
  (print-unreadable-object (str stream :type t :identity t)
    (princ (str str) stream)))

(defclass anchor (regex)
  ((startp :initarg :startp
           :reader startp
           :documentation "Whether this is a \"start anchor\".")
   (multi-line-p :initarg :multi-line-p
                 :initform nil
                 :reader multi-line-p
                 :documentation "Whether we're in multi-line mode,
i.e. whether each #\\Newline is surrounded by anchors.")
   (no-newline-p :initarg :no-newline-p
                 :initform nil
                 :reader no-newline-p
                 :documentation "Whether we ignore #\\Newline at the end."))
  (:documentation "ANCHOR objects represent anchors like \"^\" or \"$\"."))

(defclass everything (regex)
  ((single-line-p :initarg :single-line-p
                  :reader single-line-p
                  :documentation "Whether we're in single-line mode,
i.e. whether we also match #\\Newline."))
  (:documentation "EVERYTHING objects represent regexes matching
\"everything\", i.e. dots."))

(defclass word-boundary (regex)
  ((negatedp :initarg :negatedp
             :reader negatedp
             :documentation "Whether we mean the opposite,
i.e. no word-boundary."))
  (:documentation "WORD-BOUNDARY objects represent word-boundary assertions."))

(defclass branch (regex)
  ((test :initarg :test
         :accessor test
         :documentation "The test of this branch, one of
LOOKAHEAD, LOOKBEHIND, or a number.")
   (then-regex :initarg :then-regex
               :accessor then-regex
               :documentation "The regex that's to be matched if the
test succeeds.")
   (else-regex :initarg :else-regex
               :initform (make-instance 'void)
               :accessor else-regex
               :documentation "The regex that's to be matched if the
test fails."))
  (:documentation "BRANCH objects represent Perl's conditional regular
expressions."))

(defclass filter (regex)
  ((fn :initarg :fn
       :accessor fn
       :type (or function symbol)
       :documentation "The user-defined function.")
   (len :initarg :len
        :reader len
        :documentation "The fixed length of this filter or NIL."))
  (:documentation "FILTER objects represent arbitrary functions
defined by the user."))

(defclass void (regex)
  ()
  (:documentation "VOID objects represent empty regular expressions."))

(defmethod initialize-instance :after ((str str) &rest init-args)
  (declare (ignore init-args))
  "Automatically computes the length of a STR after initialization."
  (let ((str-slot (slot-value str 'str)))
    (unless (typep str-slot
                   'simple-string)
      (setf (slot-value str 'str)
            (coerce str-slot
                    'simple-string))))
  (setf (len str) (length (str str))))

(defmethod len ((void void))
  0)

(defmethod str ((void void))
  "")

(defmethod skip ((void void))
  nil)

(defmethod start-of-end-string-p ((void void))
  nil)

(defgeneric case-mode (regex old-case-mode)
  (:documentation "Utility function used by the optimizer (see GATHER-STRINGS).
Returns a keyword denoting the case-(in)sensitivity of a STR or its
second argument if the STR has length 0. Returns NIL for REGEX objects
which are not of type STR."))

(defmethod case-mode ((str str) old-case-mode)
  (cond ((zerop (len str))
         old-case-mode)
        ((case-insensitive-p str)
         :case-insensitive)
        (t
         :case-sensitive)))

(defmethod case-mode ((regex regex) old-case-mode)
  (declare (ignore old-case-mode))
  nil)

(defgeneric copy-regex (regex)
  (:documentation "Implements a deep copy of a REGEX object."))

(defmethod copy-regex ((anchor anchor))
  (make-instance 'anchor
                 :startp (startp anchor)
                 :multi-line-p (multi-line-p anchor)
                 :no-newline-p (no-newline-p anchor)))

(defmethod copy-regex ((everything everything))
  (make-instance 'everything
                 :single-line-p (single-line-p everything)))

(defmethod copy-regex ((word-boundary word-boundary))
  (make-instance 'word-boundary
                 :negatedp (negatedp word-boundary)))

(defmethod copy-regex ((void void))
  (make-instance 'void))

(defmethod copy-regex ((lookahead lookahead))
  (make-instance 'lookahead
                 :regex (copy-regex (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod copy-regex ((seq seq))
  (make-instance 'seq
                 :elements (mapcar #'copy-regex (elements seq))))

(defmethod copy-regex ((alternation alternation))
  (make-instance 'alternation
                 :choices (mapcar #'copy-regex (choices alternation))))

(defmethod copy-regex ((branch branch))
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                             (copy-regex test)
                             test)
                   :then-regex (copy-regex (then-regex branch))
                   :else-regex (copy-regex (else-regex branch)))))

(defmethod copy-regex ((lookbehind lookbehind))
  (make-instance 'lookbehind
                 :regex (copy-regex (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod copy-regex ((repetition repetition))
  (make-instance 'repetition
                 :regex (copy-regex (regex repetition))
                 :greedyp (greedyp repetition)
                 :minimum (minimum repetition)
                 :maximum (maximum repetition)
                 :min-len (min-len repetition)
                 :len (len repetition)
                 :contains-register-p (contains-register-p repetition)))

(defmethod copy-regex ((register register))
  (make-instance 'register
                 :regex (copy-regex (regex register))
                 :num (num register)
                 :name (name register)))

(defmethod copy-regex ((standalone standalone))
  (make-instance 'standalone
                 :regex (copy-regex (regex standalone))))

(defmethod copy-regex ((back-reference back-reference))
  (make-instance 'back-reference
                 :num (num back-reference)
                 :case-insensitive-p (case-insensitive-p back-reference)))

(defmethod copy-regex ((char-class char-class))
  (make-instance 'char-class
                 :test-function (test-function char-class)))

(defmethod copy-regex ((str str))
  (make-instance 'str
                 :str (str str)
                 :case-insensitive-p (case-insensitive-p str)))

(defmethod copy-regex ((filter filter))
  (make-instance 'filter
                 :fn (fn filter)
                 :len (len filter)))

(defgeneric remove-registers (regex)
  (:documentation "Returns a deep copy of a REGEX (see COPY-REGEX) and
optionally removes embedded REGISTER objects if possible and if the
special variable REMOVE-REGISTERS-P is true."))

(defmethod remove-registers ((register register))
  (declare (special remove-registers-p reg-seen))
  (cond (remove-registers-p
         (remove-registers (regex register)))
        (t
         (setq reg-seen t)
         (copy-regex register))))

(defmethod remove-registers ((repetition repetition))
  (let* (reg-seen
         (inner-regex (remove-registers (regex repetition))))
    (declare (special reg-seen))
    (make-instance 'repetition
                   :regex inner-regex
                   :greedyp (greedyp repetition)
                   :minimum (minimum repetition)
                   :maximum (maximum repetition)
                   :min-len (min-len repetition)
                   :len (len repetition)
                   :contains-register-p reg-seen)))

(defmethod remove-registers ((standalone standalone))
  (make-instance 'standalone
                 :regex (remove-registers (regex standalone))))

(defmethod remove-registers ((lookahead lookahead))
  (make-instance 'lookahead
                 :regex (remove-registers (regex lookahead))
                 :positivep (positivep lookahead)))

(defmethod remove-registers ((lookbehind lookbehind))
  (make-instance 'lookbehind
                 :regex (remove-registers (regex lookbehind))
                 :positivep (positivep lookbehind)
                 :len (len lookbehind)))

(defmethod remove-registers ((branch branch))
  (with-slots (test)
      branch
    (make-instance 'branch
                   :test (if (typep test 'regex)
                             (remove-registers test)
                             test)
                   :then-regex (remove-registers (then-regex branch))
                   :else-regex (remove-registers (else-regex branch)))))

(defmethod remove-registers ((alternation alternation))
  (declare (special remove-registers-p))
  (setq remove-registers-p nil)
  (copy-regex alternation))

(defmethod remove-registers ((regex regex))
  (copy-regex regex))

(defmethod remove-registers ((seq seq))
  (make-instance 'seq
                 :elements (mapcar #'remove-registers (elements seq))))

(defgeneric everythingp (regex)
  (:documentation "Returns an EVERYTHING object if REGEX is equivalent
to this object, otherwise NIL.  So, \"(.){1}\" would return true
\(i.e. the object corresponding to \".\", for example."))

(defmethod everythingp ((seq seq))
  (let ((cleaned-elements (remove-if #'(lambda (element)
                                         (typep element 'void))
                                     (elements seq))))
    (and (= 1 (length cleaned-elements))
         (everythingp (first cleaned-elements)))))

(defmethod everythingp ((alternation alternation))
  (with-slots (choices)
      alternation
    (and (= 1 (length choices))
         (everythingp (first choices)))))

(defmethod everythingp ((repetition repetition))
  (with-slots (maximum minimum regex)
      repetition
    (and maximum
         (= 1 minimum maximum)
         (everythingp regex))))

(defmethod everythingp ((register register))
  (everythingp (regex register)))

(defmethod everythingp ((standalone standalone))
  (everythingp (regex standalone)))

(defmethod everythingp ((everything everything))
  everything)

(defmethod everythingp ((regex regex))
  nil)

(defgeneric regex-length (regex)
  (:documentation "Return the length of REGEX if it is fixed, NIL otherwise."))

(defmethod regex-length ((seq seq))
  (loop for sub-regex in (elements seq)
        for len = (regex-length sub-regex)
        if (not len) do (return nil)
          sum len))

(defmethod regex-length ((alternation alternation))
  (loop for sub-regex in (choices alternation)
        for old-len = nil then len
        for len = (regex-length sub-regex)
        if (or (not len)
               (and old-len (/= len old-len))) do (return nil)
        finally (return len)))

(defmethod regex-length ((branch branch))
  (let ((then-length (regex-length (then-regex branch))))
    (and then-length
         (eql then-length (regex-length (else-regex branch)))
         then-length)))

(defmethod regex-length ((repetition repetition))
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eql minimum maximum))
        (* minimum len)
        nil)))

(defmethod regex-length ((register register))
  (regex-length (regex register)))

(defmethod regex-length ((standalone standalone))
  (regex-length (regex standalone)))

(defmethod regex-length ((back-reference back-reference))
  nil)

(defmethod regex-length ((char-class char-class))
  1)

(defmethod regex-length ((everything everything))
  1)

(defmethod regex-length ((str str))
  (len str))

(defmethod regex-length ((filter filter))
  (len filter))

(defmethod regex-length ((regex regex))
  0)

(defgeneric regex-min-length (regex)
  (:documentation "Returns the minimal length of REGEX."))

(defmethod regex-min-length ((seq seq))
  (loop for sub-regex in (elements seq)
        for len = (regex-min-length sub-regex)
        sum len))

(defmethod regex-min-length ((alternation alternation))
  (loop for sub-regex in (choices alternation)
        for len = (regex-min-length sub-regex)
        minimize len))

(defmethod regex-min-length ((branch branch))
  (min (regex-min-length (then-regex branch))
       (regex-min-length (else-regex branch))))

(defmethod regex-min-length ((repetition repetition))
  (* (minimum repetition) (min-len repetition)))

(defmethod regex-min-length ((register register))
  (regex-min-length (regex register)))

(defmethod regex-min-length ((standalone standalone))
  (regex-min-length (regex standalone)))

(defmethod regex-min-length ((char-class char-class))
  1)

(defmethod regex-min-length ((everything everything))
  1)

(defmethod regex-min-length ((str str))
  (len str))

(defmethod regex-min-length ((filter filter))
  (or (len filter)
      0))

(defmethod regex-min-length ((regex regex))
  0)

(defgeneric compute-offsets (regex start-pos)
  (:documentation "Returns the offset the following regex would have
relative to START-POS or NIL if we can't compute it. Sets the OFFSET
slot of REGEX to START-POS if REGEX is a STR. May also affect OFFSET
slots of STR objects further down the tree."))

(defmethod compute-offsets ((seq seq) start-pos)
  (loop for element in (elements seq)
        for pos = start-pos then curr-offset
        for curr-offset = (compute-offsets element pos)
        while curr-offset
        finally (return curr-offset)))

(defmethod compute-offsets ((alternation alternation) start-pos)
  (loop for choice in (choices alternation)
        for old-offset = nil then curr-offset
        for curr-offset = (compute-offsets choice start-pos)
        if (or (not curr-offset)
               (and old-offset (/= curr-offset old-offset)))
          do (return nil)
        finally (return curr-offset)))

(defmethod compute-offsets ((branch branch) start-pos)
  (let ((then-offset (compute-offsets (then-regex branch) start-pos)))
    (and then-offset
         (eql then-offset (compute-offsets (else-regex branch) start-pos))
         then-offset)))

(defmethod compute-offsets ((repetition repetition) start-pos)
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eq minimum maximum))
        (+ start-pos (* minimum len))
        nil)))

(defmethod compute-offsets ((register register) start-pos)
  (compute-offsets (regex register) start-pos))

(defmethod compute-offsets ((standalone standalone) start-pos)
  (compute-offsets (regex standalone) start-pos))

(defmethod compute-offsets ((char-class char-class) start-pos)
  (1+ start-pos))

(defmethod compute-offsets ((everything everything) start-pos)
  (1+ start-pos))

(defmethod compute-offsets ((str str) start-pos)
  (setf (offset str) start-pos)
  (+ start-pos (len str)))

(defmethod compute-offsets ((back-reference back-reference) start-pos)
  (declare (ignore start-pos))
  nil)

(defmethod compute-offsets ((filter filter) start-pos)
  (let ((len (len filter)))
    (if len
        (+ start-pos len)
        nil)))

(defmethod compute-offsets ((regex regex) start-pos)
  start-pos)

(defmacro case-insensitive-mode-p (flags)
  "Accessor macro to extract the first flag out of a three-element flag list."
  `(first ,flags))

(defmacro multi-line-mode-p (flags)
  "Accessor macro to extract the second flag out of a three-element flag list."
  `(second ,flags))

(defmacro single-line-mode-p (flags)
  "Accessor macro to extract the third flag out of a three-element flag list."
  `(third ,flags))

(defun set-flag (token)
  "Reads a flag token and sets or unsets the corresponding entry in
the special FLAGS list."
  (declare (special flags))
  (case token
    ((:case-insensitive-p)
     (setf (case-insensitive-mode-p flags) t))
    ((:case-sensitive-p)
     (setf (case-insensitive-mode-p flags) nil))
    ((:multi-line-mode-p)
     (setf (multi-line-mode-p flags) t))
    ((:not-multi-line-mode-p)
     (setf (multi-line-mode-p flags) nil))
    ((:single-line-mode-p)
     (setf (single-line-mode-p flags) t))
    ((:not-single-line-mode-p)
     (setf (single-line-mode-p flags) nil))
    (otherwise
     (signal-syntax-error "Unknown flag token ~A." token))))

(defgeneric resolve-property (property)
  (:documentation "Resolves PROPERTY to a unary character test
function.  PROPERTY can either be a function designator or it can be a
string which is resolved using *PROPERTY-RESOLVER*.")
  (:method ((property-name string))
    (funcall *property-resolver* property-name))
  (:method ((function-name symbol))
    function-name)
  (:method ((test-function function))
    test-function))

(defun convert-char-class-to-test-function (list invertedp case-insensitive-p)
  "Combines all items in LIST into test function and returns a
logical-OR combination of these functions.  Items can be single
characters, character ranges like \(:RANGE #\\A #\\E), or special
character classes like :DIGIT-CLASS.  Does the right thing with
respect to case-\(in)sensitivity as specified by the special variable
FLAGS."
  (declare (special flags))
  (let ((test-functions
          (loop for item in list
                collect (cond ((characterp item)
                               (let ((this-char item))
                                 (lambda (char)
                                   (declare (character char this-char))
                                   (char= char this-char))))
                              ((symbolp item)
                               (case item
                                 ((:digit-class) #'str:digit-char-p)
                                 ((:non-digit-class) (charset:complement* #'str:digit-char-p))
                                 ((:whitespace-char-class) #'whitespacep)
                                 ((:non-whitespace-char-class) (charset:complement* #'whitespacep))
                                 ((:word-char-class) #'word-char-p)
                                 ((:non-word-char-class) (charset:complement* #'word-char-p))
                                 (otherwise
                                  (signal-syntax-error "Unknown symbol ~A in character class." item))))
                              ((and (consp item)
                                    (eq (first item) :property))
                               (resolve-property (second item)))
                              ((and (consp item)
                                    (eq (first item) :inverted-property))
                               (charset:complement* (resolve-property (second item))))
                              ((and (consp item)
                                    (eq (first item) :range))
                               (let ((from (second item))
                                     (to (third item)))
                                 (when (char> from to)
                                   (signal-syntax-error "Invalid range from ~S to ~S in char-class." from to))
                                 (lambda (char)
                                   (declare (character char from to))
                                   (char<= from char to))))
                              (t (signal-syntax-error "Unknown item ~A in char-class list." item))))))
    (unless test-functions
      (signal-syntax-error "Empty character class."))
    (cond ((cdr test-functions)           
           (cond ((and invertedp case-insensitive-p)
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                          never (or (funcall test-function char-down)
                                    (and char-up (funcall test-function char-up))))))
                 (case-insensitive-p
                  (lambda (char)
                    (declare (character char))
                    (loop with both-case-p = (both-case-p char)
                          with char-down = (if both-case-p (char-downcase char) char)
                          with char-up = (if both-case-p (char-upcase char) nil)
                          for test-function in test-functions
                            thereis (or (funcall test-function char-down)
                                        (and char-up (funcall test-function char-up))))))
                 (invertedp
                  (lambda (char)
                    (loop for test-function in test-functions
                          never (funcall test-function char))))
                 (t
                  (lambda (char)
                    (loop for test-function in test-functions
                            thereis (funcall test-function char))))))
          (t (let ((test-function (first test-functions)))
               (cond ((and invertedp case-insensitive-p)
                      (lambda (char)
                        (declare (character char))
                        (not (or (funcall test-function (char-downcase char))
                                 (and (both-case-p char)
                                      (funcall test-function (char-upcase char)))))))
                     (case-insensitive-p
                      (lambda (char)
                        (declare (character char))
                        (or (funcall test-function (char-downcase char))
                            (and (both-case-p char)
                                 (funcall test-function (char-upcase char))))))
                     (invertedp (charset:complement* test-function))
                     (t test-function)))))))

(defun maybe-split-repetition (regex
                               greedyp
                               minimum
                               maximum
                               min-len
                               length
                               reg-seen)
  "Splits a REPETITION object into a constant and a varying part if
applicable, i.e. something like
  a{3,} -> a{3}a*
The arguments to this function correspond to the REPETITION slots of
the same name."
  (declare (fixnum minimum)
           (type (or fixnum null) maximum))
  (when maximum
    (when (zerop maximum)
      (return-from maybe-split-repetition
        (make-instance 'void)))
    (when (= 1 minimum maximum)
      (return-from maybe-split-repetition
        regex)))
  (let ((constant-repetition (if (plusp minimum)
                                 (make-instance 'repetition
                                                :regex (copy-regex regex)
                                                :greedyp greedyp
                                                :minimum minimum
                                                :maximum minimum
                                                :min-len min-len
                                                :len length
                                                :contains-register-p reg-seen)
                                 nil)))
    (when (and maximum
               (= maximum minimum))
      (return-from maybe-split-repetition
        constant-repetition))
    (let ((varying-repetition
            (make-instance 'repetition
                           :regex regex
                           :greedyp greedyp
                           :minimum 0
                           :maximum (if maximum (- maximum minimum) nil)
                           :min-len min-len
                           :len length
                           :contains-register-p reg-seen)))
      (cond ((zerop minimum)
             varying-repetition)
            ((= 1 minimum)
             (make-instance 'seq
                            :elements (list (copy-regex regex)
                                            varying-repetition)))
            (t
             (make-instance 'seq
                            :elements (list constant-repetition
                                            varying-repetition)))))))

(defun maybe-accumulate (str)
  "Accumulate STR into the special variable STARTS-WITH if
ACCUMULATE-START-P (also special) is true and STARTS-WITH is either
NIL or a STR object of the same case mode. Always returns NIL."
  (declare (special accumulate-start-p starts-with))
  (declare (ftype (function (t) fixnum) len))
  (when accumulate-start-p
    (etypecase starts-with
      (str
       (cond ((eq (case-insensitive-p starts-with)
                  (case-insensitive-p str))
              (setf (len starts-with)
                    (+ (len starts-with) (len str)))
              (adjust-array (slot-value starts-with 'str)
                            (len starts-with)
                            :fill-pointer t)
              (setf (subseq (slot-value starts-with 'str)
                            (- (len starts-with) (len str)))
                    (str str)
                    (skip str) t))
             (t (setq accumulate-start-p nil))))
      (null
       (setf starts-with
             (make-instance 'str
                            :str ""
                            :case-insensitive-p (case-insensitive-p str))
             (slot-value starts-with 'str)
             (make-array (len str)
                         :initial-contents (str str)
                         :element-type 'character
                         :fill-pointer t
                         :adjustable t)
             (len starts-with)
             (len str)
             (skip str) t))
      (everything
       (setq accumulate-start-p nil))))
  nil)

(declaim (inline convert-aux))

(defun convert-aux (parse-tree)
  "Converts the parse tree PARSE-TREE into a REGEX object and returns
it.  Will also
  - split and optimize repetitions,
  - accumulate strings or EVERYTHING objects into the special variable
    STARTS-WITH,
  - keep track of all registers seen in the special variable REG-NUM,
  - keep track of all named registers seen in the special variable REG-NAMES
  - keep track of the highest backreference seen in the special
    variable MAX-BACK-REF,
  - maintain and adher to the currently applicable modifiers in the special
    variable FLAGS, and
  - maybe even wash your car..."
  (if (consp parse-tree)
      (convert-compound-parse-tree (first parse-tree) parse-tree)
      (convert-simple-parse-tree parse-tree)))

(defgeneric convert-compound-parse-tree (token parse-tree &key)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are conses and dispatches on TOKEN which is the
first element of the parse tree.")
  (:method ((token t) (parse-tree t) &key)
    (signal-syntax-error "Unknown token ~A in parse-tree." token)))

(defmethod convert-compound-parse-tree ((token (eql :sequence)) parse-tree &key)
  "The case for parse trees like \(:SEQUENCE {<regex>}*)."
  (cond ((cddr parse-tree)
         (loop for parse-tree-rest on (rest parse-tree)
               while parse-tree-rest
               do (setf (car parse-tree-rest)
                        (convert-aux (car parse-tree-rest))))
         (make-instance 'seq :elements (rest parse-tree)))
        (t (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :group)) parse-tree &key)
  "The case for parse trees like \(:GROUP {<regex>}*).
This is a syntactical construct equivalent to :SEQUENCE intended to
keep the effect of modifiers local."
  (declare (special flags))
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (cond ((cddr parse-tree)
           (loop for parse-tree-rest on (rest parse-tree)
                 while parse-tree-rest
                 do (setf (car parse-tree-rest)
                          (convert-aux (car parse-tree-rest))))
           (make-instance 'seq :elements (rest parse-tree)))
          (t (convert-aux (second parse-tree))))))

(defmethod convert-compound-parse-tree ((token (eql :alternation)) parse-tree &key)
  "The case for \(:ALTERNATION {<regex>}*)."
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (loop for parse-tree-rest on (rest parse-tree)
        while parse-tree-rest
        do (setf (car parse-tree-rest)
                 (convert-aux (car parse-tree-rest))))
  (make-instance 'alternation :choices (rest parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :branch)) parse-tree &key)
  "The case for \(:BRANCH <test> <regex>).
Here, <test> must be look-ahead, look-behind or number; if <regex> is
an alternation it must have one or two choices."
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (let* ((test-candidate (second parse-tree))
         (test (cond ((numberp test-candidate)
                      (when (zerop (the fixnum test-candidate))
                        (signal-syntax-error "Register 0 doesn't exist: ~S." parse-tree))
                      (1- (the fixnum test-candidate)))
                     (t (convert-aux test-candidate))))
         (alternations (convert-aux (third parse-tree))))
    (when (and (not (numberp test))
               (not (typep test 'lookahead))
               (not (typep test 'lookbehind)))
      (signal-syntax-error "Branch test must be look-ahead, look-behind or number: ~S." parse-tree))
    (typecase alternations
      (alternation
       (case (length (choices alternations))
         ((0)
          (signal-syntax-error "No choices in branch: ~S." parse-tree))
         ((1)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))))
         ((2)
          (make-instance 'branch
                         :test test
                         :then-regex (first
                                      (choices alternations))
                         :else-regex (second
                                      (choices alternations))))
         (otherwise
          (signal-syntax-error "Too much choices in branch: ~S." parse-tree))))
      (t
       (make-instance 'branch
                      :test test
                      :then-regex alternations)))))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookahead)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKAHEAD <regex>)."
  (declare (special flags accumulate-start-p))
  (setq accumulate-start-p nil)
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'lookahead
                   :regex (convert-aux (second parse-tree))
                   :positivep t)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookahead)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKAHEAD <regex>)."
  (let ((regex (convert-compound-parse-tree :positive-lookahead parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :positive-lookbehind)) parse-tree &key)
  "The case for \(:POSITIVE-LOOKBEHIND <regex>)."
  (declare (special flags accumulate-start-p))
  (setq accumulate-start-p nil)
  (let* ((flags (copy-list flags))
         (regex (convert-aux (second parse-tree)))
         (len (regex-length regex)))
    (declare (special flags))
    (unless len
      (signal-syntax-error "Variable length look-behind not implemented \(yet): ~S." parse-tree))
    (make-instance 'lookbehind
                   :regex regex
                   :positivep t
                   :len len)))

(defmethod convert-compound-parse-tree ((token (eql :negative-lookbehind)) parse-tree &key)
  "The case for \(:NEGATIVE-LOOKBEHIND <regex>)."
  (let ((regex (convert-compound-parse-tree :positive-lookbehind parse-tree)))
    (setf (slot-value regex 'positivep) nil)
    regex))

(defmethod convert-compound-parse-tree ((token (eql :greedy-repetition)) parse-tree &key (greedyp t))
  "The case for \(:GREEDY-REPETITION|:NON-GREEDY-REPETITION <min> <max> <regex>).
This function is also used for the non-greedy case in which case it is
called with GREEDYP set to NIL as you would expect."
  (declare (special accumulate-start-p starts-with))
  (let ((local-accumulate-start-p accumulate-start-p))
    (let ((minimum (second parse-tree))
          (maximum (third parse-tree)))
      (declare (fixnum minimum))
      (declare (type (or null fixnum) maximum))
      (unless (and maximum
                   (= 1 minimum maximum))
        (setq accumulate-start-p nil))
      (let* (reg-seen
             (regex (convert-aux (fourth parse-tree)))
             (min-len (regex-min-length regex))
             (length (regex-length regex)))
        (declare (special reg-seen))
        (when (and local-accumulate-start-p
                   (not starts-with)
                   (zerop minimum)
                   (not maximum))
          (setq starts-with (everythingp regex)))
        (if (or (not reg-seen)
                (not greedyp)
                (not length)
                (zerop length)
                (and maximum (= minimum maximum)))
            (maybe-split-repetition regex
                                    greedyp
                                    minimum
                                    maximum
                                    min-len
                                    length
                                    reg-seen)
            (let* (reg-seen ; new instance for REMOVE-REGISTERS
                   (remove-registers-p t)
                   (inner-regex (remove-registers regex))
                   (inner-repetition
                     (maybe-split-repetition inner-regex
                                             t
                                             (if (zerop minimum)
                                                 0
                                                 (1- minimum))
                                             (and maximum
                                                  (1- maximum))
                                             min-len
                                             length
                                             reg-seen))
                   (inner-seq
                     (make-instance 'seq
                                    :elements (list inner-repetition
                                                    regex))))
              (declare (special remove-registers-p reg-seen))
              (if (plusp minimum)
                  inner-seq
                  (maybe-split-repetition inner-seq
                                          t
                                          0
                                          1
                                          min-len
                                          nil
                                          t))))))))

(defmethod convert-compound-parse-tree ((token (eql :non-greedy-repetition)) parse-tree &key)
  "The case for \(:NON-GREEDY-REPETITION <min> <max> <regex>)."
  (convert-compound-parse-tree :greedy-repetition parse-tree :greedyp nil))

(defmethod convert-compound-parse-tree ((token (eql :register)) parse-tree &key name)
  "The case for \(:REGISTER <regex>).  Also used for named registers
when NAME is not NIL."
  (declare (special flags reg-num reg-names))
  (let ((flags (copy-list flags))
        (stored-reg-num reg-num))
    (declare (special flags reg-seen named-reg-seen))
    (setq reg-seen t)
    (when name (setq named-reg-seen t))
    (incf (the fixnum reg-num))
    (push name reg-names)
    (make-instance 'register
                   :regex (convert-aux (if name (third parse-tree) (second parse-tree)))
                   :num stored-reg-num
                   :name name)))

(defmethod convert-compound-parse-tree ((token (eql :named-register)) parse-tree &key)
  "The case for \(:NAMED-REGISTER <regex>)."
  (convert-compound-parse-tree :register parse-tree :name (copy-seq (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :filter)) parse-tree &key)
  "The case for \(:FILTER <function> &optional <length>)."
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'filter
                 :fn (second parse-tree)
                 :len (third parse-tree)))

(defmethod convert-compound-parse-tree ((token (eql :standalone)) parse-tree &key)
  "The case for \(:STANDALONE <regex>)."
  (declare (special flags accumulate-start-p))
  (setq accumulate-start-p nil)
  (let ((flags (copy-list flags)))
    (declare (special flags))
    (make-instance 'standalone :regex (convert-aux (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :back-reference)) parse-tree &key)
  "The case for \(:BACK-REFERENCE <number>|<name>)."
  (declare (special flags accumulate-start-p reg-num reg-names max-back-ref))
  (let* ((backref-name (and (stringp (second parse-tree))
                            (second parse-tree)))
         (referred-regs
           (when backref-name
             (loop for name in reg-names
                   for reg-index from 0
                   when (string= name backref-name)
                     collect (- reg-num reg-index))))
         (backref-number (or (first referred-regs) (second parse-tree))))
    (declare (type (or fixnum null) backref-number))
    (when (or (not (typep backref-number 'fixnum))
              (<= backref-number 0))
      (signal-syntax-error "Illegal back-reference: ~S." parse-tree))
    (setq accumulate-start-p nil
          max-back-ref (max (the fixnum max-back-ref)
                            backref-number))
    (flet ((make-back-ref (backref-number)
             (make-instance 'back-reference
                            :num (1- backref-number)
                            :case-insensitive-p (case-insensitive-mode-p flags)
                            :name (copy-seq backref-name))))
      (cond
        ((cdr referred-regs)
         (make-instance 'alternation
                        :choices (loop
                                   for reg-index in referred-regs
                                   collect (make-back-ref reg-index))))
        (t
         (make-back-ref backref-number))))))

(defmethod convert-compound-parse-tree ((token (eql :regex)) parse-tree &key)
  "The case for \(:REGEX <string>)."
  (convert-aux (parse-string (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :char-class)) parse-tree &key invertedp)
  "The case for \(:CHAR-CLASS {<item>}*) where item is one of
- a character,
- a character range: \(:RANGE <char1> <char2>), or
- a special char class symbol like :DIGIT-CHAR-CLASS.
Also used for inverted char classes when INVERTEDP is true."
  (declare (special flags accumulate-start-p))
  (let ((test-function
          (charset:create-optimized-test-function
           (convert-char-class-to-test-function (rest parse-tree)
                                                invertedp
                                                (case-insensitive-mode-p flags)))))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function test-function)))

(defmethod convert-compound-parse-tree ((token (eql :inverted-char-class)) parse-tree &key)
  "The case for \(:INVERTED-CHAR-CLASS {<item>}*)."
  (convert-compound-parse-tree :char-class parse-tree :invertedp t))

(defmethod convert-compound-parse-tree ((token (eql :property)) parse-tree &key)
  "The case for \(:PROPERTY <name>) where <name> is a string."
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (resolve-property (second parse-tree))))

(defmethod convert-compound-parse-tree ((token (eql :inverted-property)) parse-tree &key)
  "The case for \(:INVERTED-PROPERTY <name>) where <name> is a string."
  (declare (special accumulate-start-p))
  (setq accumulate-start-p nil)
  (make-instance 'char-class :test-function (charset:complement* (resolve-property (second parse-tree)))))

(defmethod convert-compound-parse-tree ((token (eql :flags)) parse-tree &key)
  "The case for \(:FLAGS {<flag>}*) where flag is a modifier symbol
like :CASE-INSENSITIVE-P."
  (mapc #'set-flag (rest parse-tree))
  (make-instance 'void))

(defgeneric convert-simple-parse-tree (parse-tree)
  (:documentation "Helper function for CONVERT-AUX which converts
parse trees which are atoms.")
  (:method ((parse-tree (eql :void)))
    (make-instance 'void))
  (:method ((parse-tree (eql :word-boundary)))
    (make-instance 'word-boundary :negatedp nil))
  (:method ((parse-tree (eql :non-word-boundary)))
    (make-instance 'word-boundary :negatedp t))
  (:method ((parse-tree (eql :everything)))
    (declare (special flags accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'everything :single-line-p (single-line-mode-p flags)))
  (:method ((parse-tree (eql :digit-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function #'str:digit-char-p))
  (:method ((parse-tree (eql :word-char-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function #'word-char-p))
  (:method ((parse-tree (eql :whitespace-char-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function #'whitespacep))
  (:method ((parse-tree (eql :non-digit-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function (charset:complement* #'str:digit-char-p)))
  (:method ((parse-tree (eql :non-word-char-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function (charset:complement* #'word-char-p)))
  (:method ((parse-tree (eql :non-whitespace-char-class)))
    (declare (special accumulate-start-p))
    (setq accumulate-start-p nil)
    (make-instance 'char-class :test-function (charset:complement* #'whitespacep)))
  (:method ((parse-tree (eql :start-anchor)))
    (declare (special flags))
    (make-instance 'anchor :startp t :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :end-anchor)))
    (declare (special flags))
    (make-instance 'anchor :startp nil :multi-line-p (multi-line-mode-p flags)))
  (:method ((parse-tree (eql :modeless-start-anchor)))
    (make-instance 'anchor :startp t))
  (:method ((parse-tree (eql :modeless-end-anchor)))
    (make-instance 'anchor :startp nil))
  (:method ((parse-tree (eql :modeless-end-anchor-no-newline)))
    (make-instance 'anchor :startp nil :no-newline-p t))
  (:method ((parse-tree (eql :case-insensitive-p)))
    (set-flag parse-tree)
    (make-instance 'void))
  (:method ((parse-tree (eql :case-sensitive-p)))
    (set-flag parse-tree)
    (make-instance 'void))
  (:method ((parse-tree (eql :multi-line-mode-p)))
    (set-flag parse-tree)
    (make-instance 'void))
  (:method ((parse-tree (eql :not-multi-line-mode-p)))
    (set-flag parse-tree)
    (make-instance 'void))
  (:method ((parse-tree (eql :single-line-mode-p)))
    (set-flag parse-tree)
    (make-instance 'void))
  (:method ((parse-tree (eql :not-single-line-mode-p)))
    (set-flag parse-tree)
    (make-instance 'void)))

(defmethod convert-simple-parse-tree ((parse-tree string))
  (declare (special flags))
  (let ((str (make-instance 'str
                            :str parse-tree
                            :case-insensitive-p (case-insensitive-mode-p flags))))
    (maybe-accumulate str)
    str))

(defmethod convert-simple-parse-tree ((parse-tree character))
  (convert-simple-parse-tree (string parse-tree)))

(defmethod convert-simple-parse-tree (parse-tree)
  "The default method - check if there's a translation."
  (let ((translation (and (symbolp parse-tree) (parse-tree-synonym parse-tree))))
    (if translation
        (convert-aux (copy-tree translation))
        (signal-syntax-error "Unknown token ~A in parse tree." parse-tree))))

(defun convert (parse-tree)
  "Converts the parse tree PARSE-TREE into an equivalent REGEX object
and returns three values: the REGEX object, the number of registers
seen and an object the regex starts with which is either a STR object
or an EVERYTHING object \(if the regex starts with something like
\".*\") or NIL."
  (let* ((flags (list nil nil nil))
         (reg-num 0)
         reg-names
         named-reg-seen
         (accumulate-start-p t)
         starts-with
         (max-back-ref 0)
         (converted-parse-tree (convert-aux parse-tree)))
    (declare (special flags reg-num reg-names named-reg-seen
                      accumulate-start-p starts-with max-back-ref))
    (when (> (the fixnum max-back-ref)
             (the fixnum reg-num))
      (signal-syntax-error "Backreference to register ~A which has not been defined." max-back-ref))
    (when (typep starts-with 'str)
      (setf (slot-value starts-with 'str)
            (coerce (slot-value starts-with 'str)
                    'simple-string)))
    (values converted-parse-tree reg-num starts-with
            (when named-reg-seen
              (nreverse reg-names)))))

(defgeneric flatten (regex)
  (:documentation "Merges adjacent sequences and alternations, i.e. it
transforms #<SEQ #<STR \"a\"> #<SEQ #<STR \"b\"> #<STR \"c\">>> to
#<SEQ #<STR \"a\"> #<STR \"b\"> #<STR \"c\">>. This is a destructive
operation on REGEX."))

(defmethod flatten ((seq seq))
  (let ((elements-rest (elements seq)))
    (loop
      (unless elements-rest
        (return))
      (let ((flattened-element (flatten (car elements-rest)))
            (next-elements-rest (cdr elements-rest)))
        (cond ((typep flattened-element 'seq)
               (let ((flattened-element-elements
                       (elements flattened-element)))
                 (setf (car elements-rest)
                       (car flattened-element-elements)
                       (cdr elements-rest)
                       (nconc (cdr flattened-element-elements)
                              (cdr elements-rest)))))
              (t
               (setf (car elements-rest) flattened-element)))
        (setq elements-rest next-elements-rest))))
  (let ((elements (elements seq)))
    (cond ((cadr elements)
           seq)
          ((cdr elements)
           (first elements))
          (t (make-instance 'void)))))

(defmethod flatten ((alternation alternation))
  (let ((choices-rest (choices alternation)))
    (loop
      (unless choices-rest
        (return))
      (let ((flattened-choice (flatten (car choices-rest)))
            (next-choices-rest (cdr choices-rest)))
        (cond ((typep flattened-choice 'alternation)
               (let ((flattened-choice-choices
                       (choices flattened-choice)))
                 (setf (car choices-rest)
                       (car flattened-choice-choices)
                       (cdr choices-rest)
                       (nconc (cdr flattened-choice-choices)
                              (cdr choices-rest)))))
              (t
               (setf (car choices-rest) flattened-choice)))
        (setq choices-rest next-choices-rest))))
  (let ((choices (choices alternation)))
    (cond ((cadr choices)
           alternation)
          ((cdr choices)
           (first choices))
          (t (signal-syntax-error "Encountered alternation without choices.")))))

(defmethod flatten ((branch branch))
  (with-slots (test then-regex else-regex)
      branch
    (setq test
          (if (numberp test)
              test
              (flatten test))
          then-regex (flatten then-regex)
          else-regex (flatten else-regex))
    branch))

(defmethod flatten ((regex regex))
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
     (setf (regex regex)
           (flatten (regex regex)))
     regex)
    (t
     regex)))

(defgeneric gather-strings (regex)
  (:documentation "Collects adjacent strings or characters into one
string provided they have the same case mode. This is a destructive
operation on REGEX."))

(defmethod gather-strings ((seq seq))
  (let* ((start-point (cons nil (elements seq)))
         (curr-point start-point)
         old-case-mode
         collector
         collector-start
         (collector-length 0)
         skip)
    (declare (fixnum collector-length))
    (loop
      (let ((elements-rest (cdr curr-point)))
        (unless elements-rest
          (return))
        (let* ((element (car elements-rest))
               (case-mode (case-mode element old-case-mode)))
          (cond ((and case-mode
                      (eq case-mode old-case-mode))
                 (let ((old-collector-length collector-length))
                   (unless (and (adjustable-array-p collector)
                                (array-has-fill-pointer-p collector))
                     (setq collector
                           (make-array collector-length
                                       :initial-contents collector
                                       :element-type 'character
                                       :fill-pointer t
                                       :adjustable t)
                           collector-start nil))
                   (adjust-array collector
                                 (incf collector-length (len element))
                                 :fill-pointer t)
                   (setf (subseq collector
                                 old-collector-length)
                         (str element)
                         skip (skip element)))
                 (setf (cdr curr-point) (cdr elements-rest)))
                (t
                 (let ((collected-string
                         (cond (collector-start
                                collector-start)
                               (collector
                                (make-instance 'str
                                               :skip skip
                                               :str collector
                                               :case-insensitive-p
                                               (eq old-case-mode
                                                   :case-insensitive)))
                               (t nil))))
                   (cond (case-mode
                          (setq collector (str element)
                                collector-start element
                                skip (skip element)
                                collector-length (len element))
                          (cond (collected-string
                                 (setf (car elements-rest)
                                       collected-string
                                       curr-point
                                       (cdr curr-point)))
                                (t
                                 (setf (cdr curr-point)
                                       (cdr elements-rest)))))
                         (t
                          (cond (collected-string
                                 (setf (car elements-rest)
                                       collected-string
                                       curr-point
                                       (cdr curr-point)
                                       (cdr curr-point)
                                       (cons (gather-strings element)
                                             (cdr curr-point))
                                       curr-point
                                       (cdr curr-point)))
                                (t
                                 (setf (car elements-rest)
                                       (gather-strings element)
                                       curr-point
                                       (cdr curr-point))))
                          (setq collector nil
                                collector-start nil))))))
          (setq old-case-mode case-mode))))
    (when collector
      (setf (cdr curr-point)
            (cons
             (make-instance 'str
                            :skip skip
                            :str collector
                            :case-insensitive-p
                            (eq old-case-mode
                                :case-insensitive))
             nil)))
    (setf (elements seq) (cdr start-point))
    seq))

(defmethod gather-strings ((alternation alternation))
  (loop for choices-rest on (choices alternation)
        while choices-rest
        do (setf (car choices-rest)
                 (gather-strings (car choices-rest))))
  alternation)

(defmethod gather-strings ((branch branch))
  (with-slots (test then-regex else-regex)
      branch
    (setq test
          (if (numberp test)
              test
              (gather-strings test))
          then-regex (gather-strings then-regex)
          else-regex (gather-strings else-regex))
    branch))

(defmethod gather-strings ((regex regex))
  (typecase regex
    ((or repetition register lookahead lookbehind standalone)
     (setf (regex regex)
           (gather-strings (regex regex)))
     regex)
    (t
     regex)))

(defgeneric start-anchored-p (regex &optional in-seq-p)
  (:documentation "Returns T if REGEX starts with a \"real\" start
anchor, i.e. one that's not in multi-line mode, NIL otherwise. If
IN-SEQ-P is true the function will return :ZERO-LENGTH if REGEX is a
zero-length assertion."))

(defmethod start-anchored-p ((seq seq) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (loop for element in (elements seq)
        for anchored-p = (start-anchored-p element t)
        while (eq anchored-p :zero-length)
        finally (return (and anchored-p (not (eq anchored-p :zero-length))))))

(defmethod start-anchored-p ((alternation alternation) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (loop for choice in (choices alternation)
        always (start-anchored-p choice)))

(defmethod start-anchored-p ((branch branch) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (and (start-anchored-p (then-regex branch))
       (start-anchored-p (else-regex branch))))

(defmethod start-anchored-p ((repetition repetition) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (and (plusp (minimum repetition))
       (start-anchored-p (regex repetition))))

(defmethod start-anchored-p ((register register) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex register)))

(defmethod start-anchored-p ((standalone standalone) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (start-anchored-p (regex standalone)))

(defmethod start-anchored-p ((anchor anchor) &optional in-seq-p)
  (declare (ignore in-seq-p))
  (and (startp anchor)
       (not (multi-line-p anchor))))

(defmethod start-anchored-p ((regex regex) &optional in-seq-p)
  (typecase regex
    ((or lookahead lookbehind word-boundary void)
     (if in-seq-p
         :zero-length
         nil))
    (filter
     (if (and in-seq-p
              (len regex)
              (zerop (len regex)))
         :zero-length
         nil))
    (t
     nil)))

(defgeneric end-string-aux (regex &optional old-case-insensitive-p)
  (:documentation "Returns the constant string (if it exists) REGEX
ends with wrapped into a STR object, otherwise NIL.
OLD-CASE-INSENSITIVE-P is the CASE-INSENSITIVE-P slot of the last STR
collected or :VOID if no STR has been collected yet. (This is a helper
function called by END-STRING.)"))

(defmethod end-string-aux ((str str)
                           &optional (old-case-insensitive-p :void))
  (declare (special last-str))
  (cond ((and (not (skip str))          ; avoid constituents of STARTS-WITH
              (or (eq old-case-insensitive-p :void)
                  (eq (case-insensitive-p str) old-case-insensitive-p)))
         (setf last-str str
               (skip str) t)
         str)
        (t nil)))

(defmethod end-string-aux ((seq seq)
                           &optional (old-case-insensitive-p :void))
  (declare (special continuep))
  (let (case-insensitive-p
        concatenated-string
        concatenated-start
        (concatenated-length 0))
    (declare (fixnum concatenated-length))
    (loop for element in (reverse (elements seq))
          for loop-old-case-insensitive-p = old-case-insensitive-p
            then (if skip
                     loop-old-case-insensitive-p
                     (case-insensitive-p element-end))
          for element-end = (end-string-aux element
                                            loop-old-case-insensitive-p)
          for skip = (if element-end
                         (zerop (len element-end))
                         nil)
          unless element-end
            do (setq continuep nil)
          while element-end
          unless skip
            do (cond (concatenated-string
                      (when concatenated-start
                        (setf concatenated-string
                              (make-array concatenated-length
                                          :initial-contents (reverse (str concatenated-start))
                                          :element-type 'character
                                          :fill-pointer t
                                          :adjustable t)
                              concatenated-start nil))
                      (let ((len (len element-end))
                            (str (str element-end)))
                        (declare (fixnum len))
                        (incf concatenated-length len)
                        (loop for i of-type fixnum downfrom (1- len) to 0
                              do (vector-push-extend (char str i)
                                                     concatenated-string))))
                     (t
                      (setf concatenated-string
                            t
                            concatenated-start
                            element-end
                            concatenated-length
                            (len element-end)
                            case-insensitive-p
                            (case-insensitive-p element-end))))
          while continuep)
    (cond ((zerop concatenated-length)
           nil)
          (concatenated-start
           concatenated-start)
          (t
           (make-instance 'str
                          :str (nreverse concatenated-string)
                          :case-insensitive-p case-insensitive-p)))))

(defmethod end-string-aux ((register register)
                           &optional (old-case-insensitive-p :void))
  (end-string-aux (regex register) old-case-insensitive-p))

(defmethod end-string-aux ((standalone standalone)
                           &optional (old-case-insensitive-p :void))
  (end-string-aux (regex standalone) old-case-insensitive-p))

(defmethod end-string-aux ((regex regex)
                           &optional (old-case-insensitive-p :void))
  (declare (special last-str end-anchored-p continuep))
  (typecase regex
    ((or anchor lookahead lookbehind word-boundary void)
     (when (and (typep regex 'anchor)
                (not (startp regex))
                (or (no-newline-p regex)
                    (not (multi-line-p regex)))
                (eq old-case-insensitive-p :void))
       (setq end-anchored-p (if (no-newline-p regex) 0 1)))
     (make-instance 'str
                    :str ""
                    :case-insensitive-p :void))
    (t
     nil)))

(defun end-string (regex)
  (declare (special end-string-offset))
  "Returns the constant string (if it exists) REGEX ends with wrapped
into a STR object, otherwise NIL."
  (let ((continuep t)
        last-str)
    (declare (special continuep last-str))
    (prog1
        (end-string-aux regex)
      (when last-str
        (setf (start-of-end-string-p last-str) t
              end-string-offset (offset last-str))))))

(defgeneric compute-min-rest (regex current-min-rest)
  (:documentation "Returns the minimal length of REGEX plus
CURRENT-MIN-REST. This is similar to REGEX-MIN-LENGTH except that it
recurses down into REGEX and sets the MIN-REST slots of REPETITION
objects."))

(defmethod compute-min-rest ((seq seq) current-min-rest)
  (loop for element in (reverse (elements seq))
        for last-min-rest = current-min-rest then this-min-rest
        for this-min-rest = (compute-min-rest element last-min-rest)
        finally (return this-min-rest)))

(defmethod compute-min-rest ((alternation alternation) current-min-rest)
  (loop for choice in (choices alternation)
        minimize (compute-min-rest choice current-min-rest)))

(defmethod compute-min-rest ((branch branch) current-min-rest)
  (min (compute-min-rest (then-regex branch) current-min-rest)
       (compute-min-rest (else-regex branch) current-min-rest)))

(defmethod compute-min-rest ((str str) current-min-rest)
  (+ current-min-rest (len str)))

(defmethod compute-min-rest ((filter filter) current-min-rest)
  (+ current-min-rest (or (len filter) 0)))

(defmethod compute-min-rest ((repetition repetition) current-min-rest)
  (setf (min-rest repetition) current-min-rest)
  (compute-min-rest (regex repetition) current-min-rest)
  (+ current-min-rest (* (minimum repetition) (min-len repetition))))

(defmethod compute-min-rest ((register register) current-min-rest)
  (compute-min-rest (regex register) current-min-rest))

(defmethod compute-min-rest ((standalone standalone) current-min-rest)
  (declare (ignore current-min-rest))
  (compute-min-rest (regex standalone) 0))

(defmethod compute-min-rest ((lookahead lookahead) current-min-rest)
  (compute-min-rest (regex lookahead) 0)
  current-min-rest)

(defmethod compute-min-rest ((lookbehind lookbehind) current-min-rest)
  (compute-min-rest (regex lookbehind) (+ current-min-rest (len lookbehind)))
  current-min-rest)

(defmethod compute-min-rest ((regex regex) current-min-rest)
  (typecase regex
    ((or char-class everything)
     (1+ current-min-rest))
    (t
     current-min-rest)))

(declaim (inline *string*= *string*-equal))

(defun *string*= (string2 start1 end1 start2 end2)
  "Like STRING=, i.e. compares the special string *STRING* from START1
to END1 with STRING2 from START2 to END2. Note that there's no
boundary check - this has to be implemented by the caller."
  (declare (fixnum start1 end1 start2 end2))
  (loop for string1-idx of-type fixnum from start1 below end1
        for string2-idx of-type fixnum from start2 below end2
        always (char= (schar *string* string1-idx)
                      (schar string2 string2-idx))))

(defun *string*-equal (string2 start1 end1 start2 end2)
  "Like STRING-EQUAL, i.e. compares the special string *STRING* from
START1 to END1 with STRING2 from START2 to END2. Note that there's no
boundary check - this has to be implemented by the caller."
  (declare (fixnum start1 end1 start2 end2))
  (loop for string1-idx of-type fixnum from start1 below end1
        for string2-idx of-type fixnum from start2 below end2
        always (char-equal (schar *string* string1-idx)
                           (schar string2 string2-idx))))

(defgeneric create-matcher-aux (regex next-fn)
  (:documentation "Creates a closure which takes one parameter,
START-POS, and tests whether REGEX can match *STRING* at START-POS
such that the call to NEXT-FN after the match would succeed."))

(defmethod create-matcher-aux ((seq seq) next-fn)
  (loop for element in (reverse (elements seq))
        for curr-matcher = next-fn then next-matcher
        for next-matcher = (create-matcher-aux element curr-matcher)
        finally (return next-matcher)))

(defmethod create-matcher-aux ((alternation alternation) next-fn)
  (let ((all-matchers (mapcar #'(lambda (choice)
                                  (create-matcher-aux choice next-fn))
                              (choices alternation))))
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (loop for matcher in all-matchers
              thereis (funcall (the function matcher) start-pos)))))

(defmethod create-matcher-aux ((register register) next-fn)
  (let ((num (num register)))
    (declare (fixnum num))
    (flet ((store-end-of-reg (start-pos)
             (declare (fixnum start-pos)
                      (function next-fn))
             (setf (svref *reg-starts* num) (svref *regs-maybe-start* num)
                   (svref *reg-ends* num) start-pos)
             (funcall next-fn start-pos)))
      (let ((inner-matcher (create-matcher-aux (regex register)
                                               #'store-end-of-reg)))
        (declare (function inner-matcher))
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (let ((old-*reg-starts* (svref *reg-starts* num))
                (old-*regs-maybe-start* (svref *regs-maybe-start* num))
                (old-*reg-ends* (svref *reg-ends* num)))
            (setf (svref *regs-maybe-start* num) start-pos)
            (let ((next-pos (funcall inner-matcher start-pos)))
              (unless next-pos
                (setf (svref *reg-starts* num) old-*reg-starts*
                      (svref *regs-maybe-start* num) old-*regs-maybe-start*
                      (svref *reg-ends* num) old-*reg-ends*))
              next-pos)))))))

(defmethod create-matcher-aux ((lookahead lookahead) next-fn)
  (let ((test-matcher (create-matcher-aux (regex lookahead) #'identity)))
    (declare (function next-fn test-matcher))
    (if (positivep lookahead)
        (lambda (start-pos)
          (and (funcall test-matcher start-pos)
               (funcall next-fn start-pos)))
        (lambda (start-pos)
          (and (not (funcall test-matcher start-pos))
               (funcall next-fn start-pos))))))

(defmethod create-matcher-aux ((lookbehind lookbehind) next-fn)
  (let ((len (len lookbehind))
        (test-matcher (create-matcher-aux (regex lookbehind) #'identity)))
    (declare (function next-fn test-matcher)
             (fixnum len))
    (if (positivep lookbehind)
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (>= (- start-pos (or *real-start-pos* *start-pos*)) len)
               (funcall test-matcher (- start-pos len))
               (funcall next-fn start-pos)))
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (or (< (- start-pos (or *real-start-pos* *start-pos*)) len)
                   (not (funcall test-matcher (- start-pos len))))
               (funcall next-fn start-pos))))))

(defmacro insert-char-class-tester ((char-class chr-expr) &body body)
  "Utility macro to replace each occurence of '\(CHAR-CLASS-TEST)
within BODY with the correct test (corresponding to CHAR-CLASS)
against CHR-EXPR."
  (with-rebinding (char-class)
    (with-unique-names (test-function)
      (flet ((substitute-char-class-tester (new)
               (subst new '(char-class-test) body
                      :test #'equalp)))
        `(let ((,test-function (test-function ,char-class)))
           ,@(substitute-char-class-tester
              `(funcall ,test-function ,chr-expr)))))))

(defmethod create-matcher-aux ((char-class char-class) next-fn)
  (declare (function next-fn))
  (insert-char-class-tester (char-class (schar *string* start-pos))
    (lambda (start-pos)
      (declare (fixnum start-pos))
      (and (< start-pos *end-pos*)
           (char-class-test)
           (funcall next-fn (1+ start-pos))))))

(defmethod create-matcher-aux ((str str) next-fn)
  (declare (fixnum *end-string-pos*)
           (function next-fn)
           (special end-string))
  (let* ((len (len str))
         (case-insensitive-p (case-insensitive-p str))
         (start-of-end-string-p (start-of-end-string-p str))
         (skip (skip str))
         (str (str str))
         (chr (schar str 0))
         (end-string (and end-string (str end-string)))
         (end-string-len (if end-string
                             (length end-string)
                             nil)))
    (declare (fixnum len))
    (cond ((and start-of-end-string-p case-insensitive-p)
           (lambda (start-pos)
             (declare (fixnum start-pos end-string-len))
             (let ((test-end-pos (+ start-pos end-string-len)))
               (declare (fixnum test-end-pos))
               (and (or (= start-pos *end-string-pos*)
                        (and (<= test-end-pos *end-pos*)
                             (*string*-equal end-string start-pos test-end-pos
                                             0 end-string-len)))
                    (funcall next-fn (+ start-pos len))))))
          (start-of-end-string-p
           (lambda (start-pos)
             (declare (fixnum start-pos end-string-len))
             (let ((test-end-pos (+ start-pos end-string-len)))
               (declare (fixnum test-end-pos))
               (and (or (= start-pos *end-string-pos*)
                        (and (<= test-end-pos *end-pos*)
                             (*string*= end-string start-pos test-end-pos
                                        0 end-string-len)))
                    (funcall next-fn (+ start-pos len))))))
          (skip
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (funcall next-fn (+ start-pos len))))
          ((and (= len 1) case-insensitive-p)
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (< start-pos *end-pos*)
                  (char-equal (schar *string* start-pos) chr)
                  (funcall next-fn (1+ start-pos)))))
          ((= len 1)
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (< start-pos *end-pos*)
                  (char= (schar *string* start-pos) chr)
                  (funcall next-fn (1+ start-pos)))))
          (case-insensitive-p
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (let ((next-pos (+ start-pos len)))
               (declare (fixnum next-pos))
               (and (<= next-pos *end-pos*)
                    (*string*-equal str start-pos next-pos 0 len)
                    (funcall next-fn next-pos)))))
          (t
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (let ((next-pos (+ start-pos len)))
               (declare (fixnum next-pos))
               (and (<= next-pos *end-pos*)
                    (*string*= str start-pos next-pos 0 len)
                    (funcall next-fn next-pos))))))))

(declaim (inline word-boundary-p))

(defun word-boundary-p (start-pos)
  "Check whether START-POS is a word-boundary within *STRING*."
  (declare (fixnum start-pos))
  (let ((1-start-pos (1- start-pos))
        (*start-pos* (or *real-start-pos* *start-pos*)))
    (or (and (or (= start-pos *end-pos*)
                 (and (< start-pos *end-pos*)
                      (not (word-char-p (schar *string* start-pos)))))
             (and (< 1-start-pos *end-pos*)
                  (<= *start-pos* 1-start-pos)
                  (word-char-p (schar *string* 1-start-pos))))
        (and (or (= start-pos *start-pos*)
                 (and (< 1-start-pos *end-pos*)
                      (<= *start-pos* 1-start-pos)
                      (not (word-char-p (schar *string* 1-start-pos)))))
             (and (< start-pos *end-pos*)
                  (word-char-p (schar *string* start-pos)))))))

(defmethod create-matcher-aux ((word-boundary word-boundary) next-fn)
  (declare (function next-fn))
  (if (negatedp word-boundary)
      (lambda (start-pos)
        (and (not (word-boundary-p start-pos))
             (funcall next-fn start-pos)))
      (lambda (start-pos)
        (and (word-boundary-p start-pos)
             (funcall next-fn start-pos)))))

(defmethod create-matcher-aux ((everything everything) next-fn)
  (declare (function next-fn))
  (if (single-line-p everything)
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (and (< start-pos *end-pos*)
             (funcall next-fn (1+ start-pos))))
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (and (< start-pos *end-pos*)
             (char/= (schar *string* start-pos) #\Newline)
             (funcall next-fn (1+ start-pos))))))

(defmethod create-matcher-aux ((anchor anchor) next-fn)
  (declare (function next-fn))
  (let ((startp (startp anchor))
        (multi-line-p (multi-line-p anchor)))
    (cond ((no-newline-p anchor)
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (= start-pos *end-pos*)
                  (funcall next-fn start-pos))))
          ((and startp multi-line-p)
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (let ((*start-pos* (or *real-start-pos* *start-pos*)))
               (and (or (= start-pos *start-pos*)
                        (and (<= start-pos *end-pos*)
                             (> start-pos *start-pos*)
                             (char= #\Newline
                                    (schar *string* (1- start-pos)))))
                    (funcall next-fn start-pos)))))
          (startp
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (= start-pos (or *real-start-pos* *start-pos*))
                  (funcall next-fn start-pos))))
          (multi-line-p
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (or (= start-pos *end-pos*)
                      (and (< start-pos *end-pos*)
                           (char= #\Newline
                                  (schar *string* start-pos))))
                  (funcall next-fn start-pos))))
          (t
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (and (or (= start-pos *end-pos*)
                      (and (= start-pos (1- *end-pos*))
                           (char= #\Newline
                                  (schar *string* start-pos))))
                  (funcall next-fn start-pos)))))))

(defmethod create-matcher-aux ((back-reference back-reference) next-fn)
  (declare (function next-fn))
  (let ((num (num back-reference)))
    (if (case-insensitive-p back-reference)
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (let ((reg-start (svref *reg-starts* num))
                (reg-end (svref *reg-ends* num)))
            (and reg-start
                 (let ((next-pos (+ start-pos (- (the fixnum reg-end)
                                                 (the fixnum reg-start)))))
                   (declare (fixnum next-pos))
                   (and
                    (<= next-pos *end-pos*)
                    (*string*-equal *string* start-pos next-pos
                                    reg-start reg-end)
                    (funcall next-fn next-pos))))))
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (let ((reg-start (svref *reg-starts* num))
                (reg-end (svref *reg-ends* num)))
            (and reg-start
                 (let ((next-pos (+ start-pos (- (the fixnum reg-end)
                                                 (the fixnum reg-start)))))
                   (declare (fixnum next-pos))
                   (and
                    (<= next-pos *end-pos*)
                    (*string*= *string* start-pos next-pos
                               reg-start reg-end)
                    (funcall next-fn next-pos)))))))))

(defmethod create-matcher-aux ((branch branch) next-fn)
  (let* ((test (test branch))
         (then-matcher (create-matcher-aux (then-regex branch) next-fn))
         (else-matcher (create-matcher-aux (else-regex branch) next-fn)))
    (declare (function then-matcher else-matcher))
    (cond ((numberp test)
           (lambda (start-pos)
             (declare (fixnum test))
             (if (and (< test (length *reg-starts*))
                      (svref *reg-starts* test))
                 (funcall then-matcher start-pos)
                 (funcall else-matcher start-pos))))
          (t
           (let ((test-matcher (create-matcher-aux test #'identity)))
             (declare (function test-matcher))
             (lambda (start-pos)
               (if (funcall test-matcher start-pos)
                   (funcall then-matcher start-pos)
                   (funcall else-matcher start-pos))))))))

(defmethod create-matcher-aux ((standalone standalone) next-fn)
  (let ((inner-matcher (create-matcher-aux (regex standalone) #'identity)))
    (declare (function next-fn inner-matcher))
    (lambda (start-pos)
      (let ((next-pos (funcall inner-matcher start-pos)))
        (and next-pos
             (funcall next-fn next-pos))))))

(defmethod create-matcher-aux ((filter filter) next-fn)
  (let ((fn (fn filter)))
    (lambda (start-pos)
      (let ((next-pos (funcall fn start-pos)))
        (and next-pos
             (funcall next-fn next-pos))))))

(defmethod create-matcher-aux ((void void) next-fn)
  next-fn)

(defmacro incf-after (place &optional (delta 1) &environment env)
  "Utility macro inspired by C's \"place++\", i.e. first return the
value of PLACE and afterwards increment it by DELTA."
  (with-unique-names (%temp)
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
        (get-setf-expansion place env)
      `(let* (,@(mapcar #'list vars vals)
              (,%temp ,reader-form)
              (,(car store-vars) (+ ,%temp ,delta)))
         ,writer-form
         ,%temp))))

(defmacro greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple greedy repetitions (where simple
means that the minimum number of repetitions is zero, that the inner
regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
       (lambda (start-pos)
         (declare (fixnum start-pos maximum))
         (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                    (+ start-pos
                                       (the fixnum (* len maximum)))))
               (curr-pos start-pos))
           (declare (fixnum target-end-pos curr-pos))
           (block greedy-constant-length-matcher
             (tagbody
              forward-loop
                (when (>= curr-pos target-end-pos)
                  (go backward-loop))
                (when ,check-curr-pos
                  (incf curr-pos len)
                  (go forward-loop))
              backward-loop
                (when (< curr-pos start-pos)
                  (return-from greedy-constant-length-matcher nil))
                (let ((result (funcall next-fn curr-pos)))
                  (when result
                    (return-from greedy-constant-length-matcher result)))
                (decf curr-pos len)
                (go backward-loop)))))
       (lambda (start-pos)
         (declare (fixnum start-pos))
         (let ((target-end-pos (1+ (- *end-pos* len min-rest)))
               (curr-pos start-pos))
           (declare (fixnum target-end-pos curr-pos))
           (block greedy-constant-length-matcher
             (tagbody
              forward-loop
                (when (>= curr-pos target-end-pos)
                  (go backward-loop))
                (when ,check-curr-pos
                  (incf curr-pos len)
                  (go forward-loop))
              backward-loop
                (when (< curr-pos start-pos)
                  (return-from greedy-constant-length-matcher nil))
                (let ((result (funcall next-fn curr-pos)))
                  (when result
                    (return-from greedy-constant-length-matcher result)))
                (decf curr-pos len)
                (go backward-loop)))))))

(defun create-greedy-everything-matcher (maximum min-rest next-fn)
  "Creates a closure which just matches as far ahead as possible,
i.e. a closure for a dot in single-line mode."
  (declare (fixnum min-rest) (function next-fn))
  (if maximum
      (lambda (start-pos)
        (declare (fixnum start-pos maximum))
        (let ((target-end-pos (min (+ start-pos maximum)
                                   (- *end-pos* min-rest))))
          (declare (fixnum target-end-pos))
          (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
                  thereis (funcall next-fn curr-pos))))
      (lambda (start-pos)
        (declare (fixnum start-pos))
        (let ((target-end-pos (- *end-pos* min-rest)))
          (declare (fixnum target-end-pos))
          (loop for curr-pos of-type fixnum from target-end-pos downto start-pos
                  thereis (funcall next-fn curr-pos))))))

(defgeneric create-greedy-constant-length-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-greedy-constant-length-matcher ((repetition repetition)
                                                  next-fn)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
           next-fn)
          (t
           (typecase regex
             (str
              (let ((str (str regex)))
                (if (= 1 len)
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                          (greedy-constant-length-closure
                           (char-equal chr (schar *string* curr-pos)))
                          (greedy-constant-length-closure
                           (char= chr (schar *string* curr-pos)))))
                    (if (case-insensitive-p regex)
                        (greedy-constant-length-closure
                         (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                        (greedy-constant-length-closure
                         (*string*= str curr-pos (+ curr-pos len) 0 len))))))
             (char-class
              (insert-char-class-tester (regex (schar *string* curr-pos))
                (greedy-constant-length-closure
                 (char-class-test))))
             (everything
              (if (single-line-p regex)
                  (create-greedy-everything-matcher maximum min-rest next-fn)
                  (greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
             (t
              (let ((inner-matcher (create-matcher-aux regex #'identity)))
                (declare (function inner-matcher))
                (greedy-constant-length-closure
                 (funcall inner-matcher curr-pos)))))))))

(defgeneric create-greedy-no-zero-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (let ((maximum (maximum repetition))
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
       (setq repeat-matcher
             (create-matcher-aux (regex repetition) next-fn))
       (lambda (start-pos)
         (declare (function repeat-matcher))
         (or (funcall repeat-matcher start-pos)
             (funcall next-fn start-pos))))
      (maximum
       (let ((rep-num (incf-after *rep-num*)))
         (flet ((greedy-aux (start-pos)
                  (declare (fixnum start-pos maximum rep-num)
                           (function repeat-matcher))
                  (or (and (< (aref *repeat-counters* rep-num) maximum)
                           (incf (aref *repeat-counters* rep-num))
                           (prog1
                               (funcall repeat-matcher start-pos)
                             (decf (aref *repeat-counters* rep-num))))
                      (funcall next-fn start-pos))))
           (setq repeat-matcher
                 (create-matcher-aux (regex repetition) #'greedy-aux))
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (setf (aref *repeat-counters* rep-num) 0)
             (greedy-aux start-pos)))))
      (t
       (flet ((greedy-aux (start-pos)
                (declare (fixnum start-pos)
                         (function repeat-matcher))
                (or (funcall repeat-matcher start-pos)
                    (funcall next-fn start-pos))))
         (setq repeat-matcher
               (create-matcher-aux (regex repetition) #'greedy-aux))
         #'greedy-aux)))))

(defgeneric create-greedy-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is greedy and the minimal number of
repetitions is zero."))

(defmethod create-greedy-matcher ((repetition repetition) next-fn)
  (let ((maximum (maximum repetition))
        (zero-length-num (incf-after *zero-length-num*))
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
       (let ((rep-num (incf-after *rep-num*)))
         (flet ((greedy-aux (start-pos)
                  (declare (fixnum start-pos maximum rep-num)
                           (function repeat-matcher))
                  (let ((old-last-pos
                          (svref *last-pos-stores* zero-length-num)))
                    (when (and old-last-pos
                               (= (the fixnum old-last-pos) start-pos))
                      (return-from greedy-aux (funcall next-fn start-pos)))
                    (setf (svref *last-pos-stores* zero-length-num) start-pos)
                    (or (and (< (aref *repeat-counters* rep-num) maximum)
                             (incf (aref *repeat-counters* rep-num))
                             (prog1
                                 (funcall repeat-matcher start-pos)
                               (decf (aref *repeat-counters* rep-num))
                               (setf (svref *last-pos-stores* zero-length-num)
                                     old-last-pos)))
                        (funcall next-fn start-pos)))))
           (setq repeat-matcher
                 (create-matcher-aux (regex repetition) #'greedy-aux))
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (setf (aref *repeat-counters* rep-num) 0
                   (svref *last-pos-stores* zero-length-num) nil)
             (greedy-aux start-pos)))))
      (t
       (flet ((greedy-aux (start-pos)
                (declare (fixnum start-pos)
                         (function repeat-matcher))
                (let ((old-last-pos
                        (svref *last-pos-stores* zero-length-num)))
                  (when (and old-last-pos
                             (= (the fixnum old-last-pos) start-pos))
                    (return-from greedy-aux (funcall next-fn start-pos)))
                  (setf (svref *last-pos-stores* zero-length-num) start-pos)
                  (or (prog1
                          (funcall repeat-matcher start-pos)
                        (setf (svref *last-pos-stores* zero-length-num) old-last-pos))
                      (funcall next-fn start-pos)))))
         (setq repeat-matcher
               (create-matcher-aux (regex repetition) #'greedy-aux))
         (lambda (start-pos)
           (declare (fixnum start-pos))
           (setf (svref *last-pos-stores* zero-length-num) nil)
           (greedy-aux start-pos)))))))

(defmacro non-greedy-constant-length-closure (check-curr-pos)
  "This is the template for simple non-greedy repetitions \(where
simple means that the minimum number of repetitions is zero, that the
inner regex to be checked is of fixed length LEN, and that it doesn't
contain registers, i.e. there's no need for backtracking).
CHECK-CURR-POS is a form which checks whether the inner regex of the
repetition matches at CURR-POS."
  `(if maximum
       (lambda (start-pos)
         (declare (fixnum start-pos maximum))
         (let ((target-end-pos (min (1+ (- *end-pos* len min-rest))
                                    (+ start-pos
                                       (the fixnum (* len maximum))))))
           (loop for curr-pos of-type fixnum from start-pos
                   below target-end-pos
                 by len
                   thereis (funcall next-fn curr-pos)
                 while ,check-curr-pos
                 finally (return (funcall next-fn curr-pos)))))
       (lambda (start-pos)
         (declare (fixnum start-pos))
         (let ((target-end-pos (1+ (- *end-pos* len min-rest))))
           (loop for curr-pos of-type fixnum from start-pos
                   below target-end-pos
                 by len
                   thereis (funcall next-fn curr-pos)
                 while ,check-curr-pos
                 finally (return (funcall next-fn curr-pos)))))))

(defgeneric create-non-greedy-constant-length-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION is of fixed length and doesn't contain registers."))

(defmethod create-non-greedy-constant-length-matcher ((repetition repetition) next-fn)
  (let ((len (len repetition))
        (maximum (maximum repetition))
        (regex (regex repetition))
        (min-rest (min-rest repetition)))
    (declare (fixnum len min-rest)
             (function next-fn))
    (cond ((zerop len)
           next-fn)
          (t
           (typecase regex
             (str
              (let ((str (str regex)))
                (if (= 1 len)
                    (let ((chr (schar str 0)))
                      (if (case-insensitive-p regex)
                          (non-greedy-constant-length-closure
                           (char-equal chr (schar *string* curr-pos)))
                          (non-greedy-constant-length-closure
                           (char= chr (schar *string* curr-pos)))))
                    (if (case-insensitive-p regex)
                        (non-greedy-constant-length-closure
                         (*string*-equal str curr-pos (+ curr-pos len) 0 len))
                        (non-greedy-constant-length-closure
                         (*string*= str curr-pos (+ curr-pos len) 0 len))))))
             (char-class
              (insert-char-class-tester (regex (schar *string* curr-pos))
                (non-greedy-constant-length-closure
                 (char-class-test))))
             (everything
              (if (single-line-p regex)
                  (non-greedy-constant-length-closure
                   t)
                  (non-greedy-constant-length-closure
                   (char/= #\Newline (schar *string* curr-pos)))))
             (t
              (let ((inner-matcher (create-matcher-aux regex #'identity)))
                (declare (function inner-matcher))
                (non-greedy-constant-length-closure
                 (funcall inner-matcher curr-pos)))))))))

(defgeneric create-non-greedy-no-zero-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero.  It is furthermore assumed that the inner regex
of REPETITION can never match a zero-length string \(or instead the
maximal number of repetitions is 1)."))

(defmethod create-non-greedy-no-zero-matcher ((repetition repetition) next-fn)
  (let ((maximum (maximum repetition))
        repeat-matcher)
    (declare (function next-fn))
    (cond
      ((eql maximum 1)
       (setq repeat-matcher
             (create-matcher-aux (regex repetition) next-fn))
       (lambda (start-pos)
         (declare (function repeat-matcher))
         (or (funcall next-fn start-pos)
             (funcall repeat-matcher start-pos))))
      (maximum
       (let ((rep-num (incf-after *rep-num*)))
         (flet ((non-greedy-aux (start-pos)
                  (declare (fixnum start-pos maximum rep-num)
                           (function repeat-matcher))
                  (or (funcall next-fn start-pos)
                      (and (< (aref *repeat-counters* rep-num) maximum)
                           (incf (aref *repeat-counters* rep-num))
                           (prog1
                               (funcall repeat-matcher start-pos)
                             (decf (aref *repeat-counters* rep-num)))))))
           (setq repeat-matcher
                 (create-matcher-aux (regex repetition) #'non-greedy-aux))
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (setf (aref *repeat-counters* rep-num) 0)
             (non-greedy-aux start-pos)))))
      (t
       (flet ((non-greedy-aux (start-pos)
                (declare (fixnum start-pos)
                         (function repeat-matcher))
                (or (funcall next-fn start-pos)
                    (funcall repeat-matcher start-pos))))
         (setq repeat-matcher
               (create-matcher-aux (regex repetition) #'non-greedy-aux))
         #'non-greedy-aux)))))

(defgeneric create-non-greedy-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION is non-greedy and the minimal number of
repetitions is zero."))

(defmethod create-non-greedy-matcher ((repetition repetition) next-fn)
  (let ((zero-length-num (incf-after *zero-length-num*))
        (maximum (maximum repetition))
        repeat-matcher)
    (declare (fixnum zero-length-num)
             (function next-fn))
    (cond
      (maximum
       (let ((rep-num (incf-after *rep-num*)))
         (flet ((non-greedy-aux (start-pos)
                  (declare (fixnum start-pos maximum rep-num)
                           (function repeat-matcher))
                  (let ((old-last-pos
                          (svref *last-pos-stores* zero-length-num)))
                    (when (and old-last-pos
                               (= (the fixnum old-last-pos) start-pos))
                      (return-from non-greedy-aux (funcall next-fn start-pos)))
                    (setf (svref *last-pos-stores* zero-length-num) start-pos)
                    (or (funcall next-fn start-pos)
                        (and (< (aref *repeat-counters* rep-num) maximum)
                             (incf (aref *repeat-counters* rep-num))
                             (prog1
                                 (funcall repeat-matcher start-pos)
                               (decf (aref *repeat-counters* rep-num))
                               (setf (svref *last-pos-stores* zero-length-num)
                                     old-last-pos)))))))
           (setq repeat-matcher
                 (create-matcher-aux (regex repetition) #'non-greedy-aux))
           (lambda (start-pos)
             (declare (fixnum start-pos))
             (setf (aref *repeat-counters* rep-num) 0
                   (svref *last-pos-stores* zero-length-num) nil)
             (non-greedy-aux start-pos)))))
      (t
       (flet ((non-greedy-aux (start-pos)
                (declare (fixnum start-pos)
                         (function repeat-matcher))
                (let ((old-last-pos
                        (svref *last-pos-stores* zero-length-num)))
                  (when (and old-last-pos
                             (= (the fixnum old-last-pos) start-pos))
                    (return-from non-greedy-aux (funcall next-fn start-pos)))
                  (setf (svref *last-pos-stores* zero-length-num) start-pos)
                  (or (funcall next-fn start-pos)
                      (prog1
                          (funcall repeat-matcher start-pos)
                        (setf (svref *last-pos-stores* zero-length-num)
                              old-last-pos))))))
         (setq repeat-matcher
               (create-matcher-aux (regex repetition) #'non-greedy-aux))
         (lambda (start-pos)
           (declare (fixnum start-pos))
           (setf (svref *last-pos-stores* zero-length-num) nil)
           (non-greedy-aux start-pos)))))))

(defmacro constant-repetition-constant-length-closure (check-curr-pos)
  "This is the template for simple constant repetitions (where simple
means that the inner regex to be checked is of fixed length LEN, and
that it doesn't contain registers, i.e. there's no need for
backtracking) and where constant means that MINIMUM is equal to
MAXIMUM.  CHECK-CURR-POS is a form which checks whether the inner
regex of the repetition matches at CURR-POS."
  `(lambda (start-pos)
     (declare (fixnum start-pos))
     (let ((target-end-pos (+ start-pos
                              (the fixnum (* len repetitions)))))
       (declare (fixnum target-end-pos))
       (and (>= *end-pos* target-end-pos)
            (loop for curr-pos of-type fixnum from start-pos
                    below target-end-pos
                  by len
                  always ,check-curr-pos)
            (funcall next-fn target-end-pos)))))

(defgeneric create-constant-repetition-constant-length-matcher
    (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions.
It is furthermore assumed that the inner regex of REPETITION is of
fixed length and doesn't contain registers."))

(defmethod create-constant-repetition-constant-length-matcher
    ((repetition repetition) next-fn)
  (let ((len (len repetition))
        (repetitions (minimum repetition))
        (regex (regex repetition)))
    (declare (fixnum len repetitions)
             (function next-fn))
    (if (zerop len)
        (create-matcher-aux regex next-fn)
        (typecase regex
          (str
           (let ((str (str regex)))
             (if (= 1 len)
                 (let ((chr (schar str 0)))
                   (if (case-insensitive-p regex)
                       (constant-repetition-constant-length-closure
                        (and (char-equal chr (schar *string* curr-pos))
                             (1+ curr-pos)))
                       (constant-repetition-constant-length-closure
                        (and (char= chr (schar *string* curr-pos))
                             (1+ curr-pos)))))
                 (if (case-insensitive-p regex)
                     (constant-repetition-constant-length-closure
                      (let ((next-pos (+ curr-pos len)))
                        (declare (fixnum next-pos))
                        (and (*string*-equal str curr-pos next-pos 0 len)
                             next-pos)))
                     (constant-repetition-constant-length-closure
                      (let ((next-pos (+ curr-pos len)))
                        (declare (fixnum next-pos))
                        (and (*string*= str curr-pos next-pos 0 len)
                             next-pos)))))))
          (char-class
           (insert-char-class-tester (regex (schar *string* curr-pos))
             (constant-repetition-constant-length-closure
              (and (char-class-test)
                   (1+ curr-pos)))))
          (everything
           (if (single-line-p regex)
               (lambda (start-pos)
                 (declare (fixnum start-pos))
                 (let ((next-pos (+ start-pos repetitions)))
                   (declare (fixnum next-pos))
                   (and (<= next-pos *end-pos*)
                        (funcall next-fn next-pos))))
               (constant-repetition-constant-length-closure
                (and (char/= #\Newline (schar *string* curr-pos))
                     (1+ curr-pos)))))
          (t
           (let ((inner-matcher (create-matcher-aux regex #'identity)))
             (declare (function inner-matcher))
             (constant-repetition-constant-length-closure
              (funcall inner-matcher curr-pos))))))))

(defgeneric create-constant-repetition-matcher (repetition next-fn)
  (:documentation "Creates a closure which tries to match REPETITION.
It is assumed that REPETITION has a constant number of repetitions."))

(defmethod create-constant-repetition-matcher ((repetition repetition) next-fn)
  (let ((repetitions (minimum repetition))
        (rep-num (incf-after *rep-num*))
        repeat-matcher)
    (declare (fixnum repetitions rep-num)
             (function next-fn))
    (if (zerop (min-len repetition))
        (let ((zero-length-num (incf-after *zero-length-num*)))
          (declare (fixnum zero-length-num))
          (flet ((constant-aux (start-pos)
                   (declare (fixnum start-pos)
                            (function repeat-matcher))
                   (let ((old-last-pos
                           (svref *last-pos-stores* zero-length-num)))
                     (when (and old-last-pos
                                (= (the fixnum old-last-pos) start-pos))
                       (return-from constant-aux (funcall next-fn start-pos)))
                     (setf (svref *last-pos-stores* zero-length-num) start-pos)
                     (cond ((< (aref *repeat-counters* rep-num) repetitions)
                            (incf (aref *repeat-counters* rep-num))
                            (prog1
                                (funcall repeat-matcher start-pos)
                              (decf (aref *repeat-counters* rep-num))
                              (setf (svref *last-pos-stores* zero-length-num)
                                    old-last-pos)))
                           (t
                            (funcall next-fn start-pos))))))
            (setq repeat-matcher
                  (create-matcher-aux (regex repetition) #'constant-aux))
            (lambda (start-pos)
              (declare (fixnum start-pos))
              (setf (aref *repeat-counters* rep-num) 0
                    (aref *last-pos-stores* zero-length-num) nil)
              (constant-aux start-pos))))
        (flet ((constant-aux (start-pos)
                 (declare (fixnum start-pos)
                          (function repeat-matcher))
                 (cond ((< (aref *repeat-counters* rep-num) repetitions)
                        (incf (aref *repeat-counters* rep-num))
                        (prog1
                            (funcall repeat-matcher start-pos)
                          (decf (aref *repeat-counters* rep-num))))
                       (t (funcall next-fn start-pos)))))
          (setq repeat-matcher
                (create-matcher-aux (regex repetition) #'constant-aux))
          (lambda (start-pos)
            (declare (fixnum start-pos))
            (setf (aref *repeat-counters* rep-num) 0)
            (constant-aux start-pos))))))

(defmethod create-matcher-aux ((repetition repetition) next-fn)
  (with-slots (minimum maximum len min-len greedyp contains-register-p)
      repetition
    (cond ((and maximum
                (zerop maximum))
           (error "Got REPETITION with MAXIMUM 0 \(should not happen)"))
          ((and maximum
                (= minimum maximum 1))
           (error "Got REPETITION with MAXIMUM 1 and MINIMUM 1 \(should not happen)"))
          ((and (eql minimum maximum)
                len
                (not contains-register-p))
           (create-constant-repetition-constant-length-matcher repetition next-fn))
          ((eql minimum maximum)
           (create-constant-repetition-matcher repetition next-fn))
          ((and greedyp
                len
                (not contains-register-p))
           (create-greedy-constant-length-matcher repetition next-fn))
          ((and greedyp
                (or (plusp min-len)
                    (eql maximum 1)))
           (create-greedy-no-zero-matcher repetition next-fn))
          (greedyp
           (create-greedy-matcher repetition next-fn))
          ((and len
                (plusp len)
                (not contains-register-p))
           (create-non-greedy-constant-length-matcher repetition next-fn))
          ((or (plusp min-len)
               (eql maximum 1))
           (create-non-greedy-no-zero-matcher repetition next-fn))
          (t
           (create-non-greedy-matcher repetition next-fn)))))

(defmacro bmh-matcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-BMH-MATCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
       (declare (fixnum start-pos))
       (if (or (minusp start-pos)
               (> (the fixnum (+ start-pos m)) *end-pos*))
           nil
           (loop named bmh-matcher
                 for k of-type fixnum = (+ start-pos m -1)
                   then (+ k (max 1 (aref skip (char-code (schar *string* k)))))
                 while (< k *end-pos*)
                 do (loop for j of-type fixnum downfrom (1- m)
                          for i of-type fixnum downfrom k
                          while (and (>= j 0)
                                     (,char-compare (schar *string* i)
                                                    (schar pattern j)))
                          finally (if (minusp j)
                                      (return-from bmh-matcher (1+ i)))))))))

(defun create-bmh-matcher (pattern case-insensitive-p)
  "Returns a Boyer-Moore-Horspool matcher which searches the (special)
simple-string *STRING* for the first occurence of the substring
PATTERN.  The search starts at the position START-POS within *STRING*
and stops before *END-POS* is reached.  Depending on the second
argument the search is case-insensitive or not.  If the special
variable *USE-BMH-MATCHERS* is NIL, use the standard SEARCH function
instead.  \(BMH matchers are faster but need much more space.)"
  (unless *use-bmh-matchers*
    (let ((test (if case-insensitive-p #'char-equal #'char=)))
      (return-from create-bmh-matcher
        (lambda (start-pos)
          (declare (fixnum start-pos))
          (and (not (minusp start-pos))
               (cl:search pattern
                          *string*
                          :start2 start-pos
                          :end2 *end-pos*
                          :test test))))))
  (let* ((m (length pattern))
	 (skip (make-array charset:*char-code-limit*
                           :element-type 'fixnum
                           :initial-element m)))
    (declare (fixnum m))
    (loop for k of-type fixnum below m
          if case-insensitive-p
            do (setf (aref skip (char-code (char-upcase (schar pattern k)))) (- m k 1)
                     (aref skip (char-code (char-downcase (schar pattern k)))) (- m k 1))
	  else
            do (setf (aref skip (char-code (schar pattern k))) (- m k 1)))
    (if case-insensitive-p
        (bmh-matcher-aux :case-insensitive-p t)
        (bmh-matcher-aux))))

(defmacro char-searcher-aux (&key case-insensitive-p)
  "Auxiliary macro used by CREATE-CHAR-SEARCHER."
  (let ((char-compare (if case-insensitive-p 'char-equal 'char=)))
    `(lambda (start-pos)
       (declare (fixnum start-pos))
       (and (not (minusp start-pos))
            (loop for i of-type fixnum from start-pos below *end-pos*
                    thereis (and (,char-compare (schar *string* i) chr) i))))))

(defun create-char-searcher (chr case-insensitive-p)
  "Returns a function which searches the (special) simple-string
*STRING* for the first occurence of the character CHR. The search
starts at the position START-POS within *STRING* and stops before
*END-POS* is reached.  Depending on the second argument the search is
case-insensitive or not."
  (if case-insensitive-p
      (char-searcher-aux :case-insensitive-p t)
      (char-searcher-aux)))

(declaim (inline newline-skipper))

(defun newline-skipper (start-pos)
  "Finds the next occurence of a character in *STRING* which is behind
a #\Newline."
  (declare (fixnum start-pos))
  (loop for i of-type fixnum from (1- start-pos) below *end-pos*
          thereis (and (char= (schar *string* i)
                              #\Newline)
                       (1+ i))))

(defmacro insert-advance-fn (advance-fn)
  "Creates the actual closure returned by COMPILE-AUX by
replacing '(ADVANCE-FN-DEFINITION) with a suitable definition for
ADVANCE-FN.  This is a utility macro used by COMPILE-AUX."
  (subst
   advance-fn '(advance-fn-definition)
   '(lambda (string start end)
     (block scan
       (let* ((*string* string)
              (*start-pos* start)
              (*end-pos* end)
              (*end-string-pos* (1- *start-pos*))
              (*repeat-counters* *repeat-counters*)
              (*last-pos-stores* *last-pos-stores*)
              (*reg-starts* *reg-starts*)
              (*regs-maybe-start* *regs-maybe-start*)
              (*reg-ends* *reg-ends*)
              (scan-start-pos *start-pos*)
              (starts-with-str (if start-string-test
                                   (str starts-with)
                                   nil))
              (max-end-pos (- *end-pos* min-len)))
         (declare (fixnum scan-start-pos)
                  (function match-fn))
         (labels ((advance-fn-definition))
           (declare (inline advance-fn))
           (when (plusp rep-num)
             (setq *repeat-counters* (make-array rep-num
                                                 :initial-element 0
                                                 :element-type 'fixnum)))
           (when (plusp zero-length-num)
             (setq *last-pos-stores* (make-array zero-length-num
                                                 :initial-element nil)))
           (when (plusp reg-num)
             (setq *reg-starts* (make-array reg-num :initial-element nil)
                   *regs-maybe-start* (make-array reg-num :initial-element nil)
                   *reg-ends* (make-array reg-num :initial-element nil)))
           (when end-anchored-p
             (let ((end-test-pos (- *end-pos* (the fixnum end-string-len))))
               (declare (fixnum end-test-pos)
                        (function end-string-test))
               (unless (setq *end-string-pos* (funcall end-string-test
                                                       end-test-pos))
                 (when (and (= 1 (the fixnum end-anchored-p))
                            (> *end-pos* scan-start-pos)
                            (char= #\Newline (schar *string* (1- *end-pos*))))
                   (setq *end-string-pos* (funcall end-string-test
                                                   (1- end-test-pos))))))
             (unless (and *end-string-pos*
                          (<= *start-pos* *end-string-pos*))
               (return-from scan nil))
             (when end-string-offset
               (setq scan-start-pos (max scan-start-pos
                                         (- (the fixnum *end-string-pos*)
                                            (the fixnum end-string-offset))))))
           (cond
             (start-anchored-p
              (when (or (/= *start-pos* scan-start-pos)
                        (< max-end-pos *start-pos*))
                (return-from scan nil))
              (when starts-with-str
                (locally
                    (declare (fixnum starts-with-len))
                  (cond ((and (case-insensitive-p starts-with)
                              (not (*string*-equal starts-with-str
                                                   *start-pos*
                                                   (+ *start-pos*
                                                      starts-with-len)
                                                   0 starts-with-len)))
                         (return-from scan nil))
                        ((and (not (case-insensitive-p starts-with))
                              (not (*string*= starts-with-str
                                              *start-pos*
                                              (+ *start-pos* starts-with-len)
                                              0 starts-with-len)))
                         (return-from scan nil))
                        (t nil))))
              (when (and end-string-test
                         (not end-anchored-p))
                (block end-string-loop
                  (setq *end-string-pos* *start-pos*)
                  (loop
                    (unless (setq *end-string-pos*
                                  (funcall (the function end-string-test)
                                           *end-string-pos*))
                      (return-from scan nil))
                    (unless end-string-offset
                      (return-from end-string-loop))
                    (let ((maybe-start-pos (- (the fixnum *end-string-pos*)
                                              (the fixnum end-string-offset))))
                      (cond ((= maybe-start-pos *start-pos*)
                             (return-from end-string-loop))
                            ((and (< maybe-start-pos *start-pos*)
                                  (< (+ *end-string-pos* end-string-len) *end-pos*))
                             (incf *end-string-pos*))
                            (t
                             (return-from scan nil)))))))
              (let ((next-pos (funcall match-fn *start-pos*)))
                (when next-pos
                  (values (if next-pos *start-pos* nil)
                          next-pos
                          *reg-starts*
                          *reg-ends*))))
             (t
              (loop for pos = (if starts-with-everything
                                  scan-start-pos
                                  (advance-fn scan-start-pos))
                      then (advance-fn pos)
                    while (and pos
                               (<= (the fixnum pos) max-end-pos))
                    do (let ((next-pos (funcall match-fn pos)))
                         (when next-pos
                           (return-from scan (values pos
                                                     next-pos
                                                     *reg-starts*
                                                     *reg-ends*)))
                         (incf (the fixnum pos))))))))))
   :test #'equalp))

(defun compile-aux (match-fn
                    min-len
                    start-anchored-p
                    starts-with
                    start-string-test
                    end-anchored-p
                    end-string-test
                    end-string-len
                    end-string-offset
                    rep-num
                    zero-length-num
                    reg-num)
  "Auxiliary function to create and return a scanner \(which is
actually a closure).  Used by COMPILE."
  (declare (fixnum min-len zero-length-num rep-num reg-num))
  (let ((starts-with-len (if (typep starts-with 'str)
                             (len starts-with)))
        (starts-with-everything (typep starts-with 'everything)))
    (cond
      ((and start-string-test end-string-test end-string-offset)
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (fixnum end-string-offset starts-with-len)
                             (function start-string-test end-string-test))
                    (loop
                      (unless (setq pos (funcall start-string-test pos))
                        (return-from scan nil))
                      (locally
                          (declare (fixnum pos))
                        (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                          (return-from advance-fn pos))
                        (let ((try-pos (+ pos starts-with-len)))
                          (loop
                            (unless (setq *end-string-pos*
                                          (funcall end-string-test try-pos))
                              (return-from scan nil))
                            (let ((new-pos (- (the fixnum *end-string-pos*)
                                              end-string-offset)))
                              (declare (fixnum new-pos *end-string-pos*))
                              (cond ((= new-pos pos)
                                     (return-from advance-fn pos))
                                    ((> new-pos pos)
                                     (setq pos new-pos)
                                     (return))
                                    (t
                                     (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and starts-with-everything end-string-test end-string-offset)
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (fixnum end-string-offset)
                             (function end-string-test))
                    (loop
                      (unless (setq pos (newline-skipper pos))
                        (return-from scan nil))
                      (locally
                          (declare (fixnum pos))
                        (when (= pos (- (the fixnum *end-string-pos*) end-string-offset))
                          (return-from advance-fn pos))
                        (let ((try-pos pos))
                          (loop
                            (unless (setq *end-string-pos*
                                          (funcall end-string-test try-pos))
                              (return-from scan nil))
                            (let ((new-pos (- (the fixnum *end-string-pos*)
                                              end-string-offset)))
                              (declare (fixnum new-pos *end-string-pos*))
                              (cond ((= new-pos pos)
                                     (return-from advance-fn pos))
                                    ((> new-pos pos)
                                     (setq pos new-pos)
                                     (return))
                                    (t
                                     (setq try-pos (1+ *end-string-pos*))))))))))))
      ((and start-string-test end-string-test)
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (function start-string-test end-string-test))
                    (unless (setq pos (funcall start-string-test pos))
                      (return-from scan nil))
                    (if (<= (the fixnum pos)
                            (the fixnum *end-string-pos*))
                        (return-from advance-fn pos))
                    (unless (setq *end-string-pos* (funcall end-string-test pos))
                      (return-from scan nil))
                    pos)))
      ((and starts-with-everything end-string-test)
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (function end-string-test))
                    (unless (setq pos (newline-skipper pos))
                      (return-from scan nil))
                    (if (<= (the fixnum pos)
                            (the fixnum *end-string-pos*))
                        (return-from advance-fn pos))
                    (unless (setq *end-string-pos* (funcall end-string-test pos))
                      (return-from scan nil))
                    pos)))
      (start-string-test
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (function start-string-test))
                    (unless (setq pos (funcall start-string-test pos))
                      (return-from scan nil))
                    pos)))
      (starts-with-everything
       (insert-advance-fn
        (advance-fn (pos)
                    (unless (setq pos (newline-skipper pos))
                      (return-from scan nil))
                    pos)))
      (end-string-test
       (insert-advance-fn
        (advance-fn (pos)
                    (declare (function end-string-test))
                    (if (<= (the fixnum pos)
                            (the fixnum *end-string-pos*))
                        (return-from advance-fn pos))
                    (unless (setq *end-string-pos* (funcall end-string-test pos))
                      (return-from scan nil))
                    pos)))
      (t
       (insert-advance-fn
        (advance-fn (pos)
                    pos))))))

(defvar *look-ahead-for-suffix* t
  "Controls whether scanners will optimistically look ahead for a
  constant suffix of a regular expression, if there is one.")

(defgeneric compile (regex &key case-insensitive-mode
                             multi-line-mode
                             single-line-mode
                             extended-mode
                             destructive)
  (:documentation "Accepts a regular expression - either as a
parse-tree or as a string - and returns a scan closure which will scan
strings for this regular expression and a list mapping registers to
their names \(NIL stands for unnamed ones).  The \"mode\" keyword
arguments are equivalent to the imsx modifiers in Perl.  If
DESTRUCTIVE is not NIL, the function is allowed to destructively
modify its first argument \(but only if it's a parse tree)."))

(defmethod compile ((regex-string string) &key case-insensitive-mode
                                            multi-line-mode
                                            single-line-mode
                                            extended-mode
                                            destructive)
  (declare (ignore destructive))
  (let* ((*extended-mode-p* extended-mode)
         (quoted-regex-string (if *allow-quoting*
                                  (quote-sections (clean-comments regex-string extended-mode))
                                  regex-string))
         (*syntax-error-string* (copy-seq quoted-regex-string)))
    (compile (cons :group (list (parse-string quoted-regex-string)))
             :case-insensitive-mode case-insensitive-mode
             :multi-line-mode multi-line-mode
             :single-line-mode single-line-mode
             :destructive t)))

(defmethod compile ((scanner function) &key case-insensitive-mode
                                         multi-line-mode
                                         single-line-mode
                                         extended-mode
                                         destructive)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

(defmethod compile ((parse-tree t) &key case-insensitive-mode
                                     multi-line-mode
                                     single-line-mode
                                     extended-mode
                                     destructive)
  (when extended-mode
    (signal-invocation-error "Extended mode doesn't make sense in parse trees."))
  (unless destructive
    (setq parse-tree (copy-tree parse-tree)))
  (let (flags)
    (if single-line-mode
        (push :single-line-mode-p flags))
    (if multi-line-mode
        (push :multi-line-mode-p flags))
    (if case-insensitive-mode
        (push :case-insensitive-p flags))
    (when flags
      (setq parse-tree (list :group (cons :flags flags) parse-tree))))
  (let ((*syntax-error-string* nil))
    (multiple-value-bind (regex reg-num starts-with reg-names)
        (convert parse-tree)
      (let ((regex (gather-strings (flatten regex))))
        (compute-min-rest regex 0)
        (compute-offsets regex 0)
        (let* (end-string-offset
               end-anchored-p
               (end-string (end-string regex))
               (end-string-test (and *look-ahead-for-suffix*
                                     end-string
                                     (plusp (len end-string))
                                     (if (= 1 (len end-string))
                                         (create-char-searcher
                                          (schar (str end-string) 0)
                                          (case-insensitive-p end-string))
                                         (create-bmh-matcher
                                          (str end-string)
                                          (case-insensitive-p end-string)))))
               (*rep-num* 0)
               (*zero-length-num* 0)
               (match-fn (create-matcher-aux regex #'identity))
               (start-string-test (and (typep starts-with 'str)
                                       (plusp (len starts-with))
                                       (if (= 1 (len starts-with))
                                           (create-char-searcher
                                            (schar (str starts-with) 0)
                                            (case-insensitive-p starts-with))
                                           (create-bmh-matcher
                                            (str starts-with)
                                            (case-insensitive-p starts-with))))))
          (declare (special end-string-offset end-anchored-p end-string))
          (values (compile-aux match-fn
                               (regex-min-length regex)
                               (or (start-anchored-p regex)
                                   (and (typep starts-with 'everything)
                                        (single-line-p starts-with)))
                               starts-with
                               start-string-test
                               (and end-string-test end-anchored-p)
                               end-string-test
                               (if end-string-test
                                   (len end-string)
                                   nil)
                               end-string-offset
                               *rep-num*
                               *zero-length-num*
                               reg-num)
                  reg-names))))))

(defgeneric scan (regex target-string &key start end real-start-pos)
  (:documentation "Searches TARGET-STRING from START to END and tries
to match REGEX.  On success returns four values - the start of the
match, the end of the match, and two arrays denoting the beginnings
and ends of register matches.  On failure returns NIL.  REGEX can be a
string which will be parsed according to Perl syntax, a parse tree, or
a pre-compiled scanner created by COMPILE.  TARGET-STRING will
be coerced to a simple string if it isn't one already.  The
REAL-START-POS parameter should be ignored - it exists only for
internal purposes."))

(defmethod scan ((regex-string string) target-string
                 &key (start 0)
                   (end (length target-string))
                   ((:real-start-pos *real-start-pos*) nil))
  (funcall (compile regex-string)
           (maybe-coerce-to-simple-string target-string)
           start end))

(defmethod scan ((scanner function) target-string
                 &key (start 0)
                   (end (length target-string))
                   ((:real-start-pos *real-start-pos*) nil))
  (funcall scanner
           (maybe-coerce-to-simple-string target-string)
           start end))

(defmethod scan ((parse-tree t) target-string
                 &key (start 0)
                   (end (length target-string))
                   ((:real-start-pos *real-start-pos*) nil))
  (funcall (compile parse-tree)
           (maybe-coerce-to-simple-string target-string)
           start end))

(define-compiler-macro scan (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(scan (load-time-value (compile ,regex))
                ,target-string ,@rest))
        (t form)))

(defun scan-to-strings (regex target-string &key (start 0)
                                              (end (length target-string))
                                              sharedp)
  "Like SCAN but returns substrings of TARGET-STRING instead of
positions, i.e. this function returns two values on success: the whole
match as a string plus an array of substrings (or NILs) corresponding
to the matched registers.  If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (unless match-start
      (return-from scan-to-strings nil))
    (let ((substr-fn (if sharedp #'nsubseq #'subseq)))
      (values (funcall substr-fn
                       target-string match-start match-end)
              (map 'vector
                   (lambda (reg-start reg-end)
                     (if reg-start
                         (funcall substr-fn
                                  target-string reg-start reg-end)
                         nil))
                   reg-starts
                   reg-ends)))))

(define-compiler-macro scan-to-strings
    (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(scan-to-strings (load-time-value (compile ,regex))
                           ,target-string ,@rest))
        (t form)))

(defmacro do-scans ((match-start match-end reg-starts reg-ends regex
                     target-string
                     result-form
                     &key start end)
                    &body body
                    &environment env)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START, MATCH-END, REG-STARTS, and
REG-ENDS bound to the four return values of each match in turn.  After
the last match, returns RESULT-FORM if provided or NIL otherwise. An
implicit block named NIL surrounds DO-SCANS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match. BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (%start %end %regex scanner)
      (declare (ignorable %regex scanner))
      `(block nil
         (let* ((,%start (or ,start 0))
                (,%end (or ,end (length ,target-string)))
                ,@(unless (constantp regex env)
                    `((,%regex ,regex)
                      (,scanner (typecase ,%regex
                                  (function ,%regex)
                                  (t (compile ,%regex)))))))
           (setq ,target-string
                 (maybe-coerce-to-simple-string ,target-string))
           (loop
             (multiple-value-bind
                   (,match-start ,match-end ,reg-starts ,reg-ends)
                 (scan ,(cond ((constantp regex env) regex)
                              (t scanner))
                       ,target-string :start ,%start :end ,%end
                       :real-start-pos (or ,start 0))
               (declare
                (ignorable ,match-start ,match-end ,reg-starts ,reg-ends))
               (unless ,match-start
                 (return ,result-form))
               (locally
                   ,@body)
               (setq ,%start (if (= ,match-start ,match-end)
                                 (1+ ,match-end)
                                 ,match-end)))))))))

(defmacro do-matches ((match-start match-end regex target-string result-form
                       &key start end)
                      &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-START and MATCH-END bound to the
start/end positions of each match in turn.  After the last match,
returns RESULT-FORM if provided or NIL otherwise.  An implicit block
named NIL surrounds DO-MATCHES; RETURN may be used to terminate the
loop immediately.  If REGEX matches an empty string the scan is
continued one position behind this match.  BODY may start with
declarations."
  (with-unique-names (reg-starts reg-ends)
    `(do-scans (,match-start ,match-end
                ,reg-starts ,reg-ends
                ,regex ,target-string
                ,result-form
                :start ,start :end ,end)
       ,@body)))

(defmacro do-matches-as-strings ((match-var regex
                                  target-string
                                  result-form
                                  &key start end sharedp)
                                 &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-VAR bound to the substring of
TARGET-STRING corresponding to each match in turn.  After the last
match, returns RESULT-FORM if provided or NIL otherwise.  An implicit
block named NIL surrounds DO-MATCHES-AS-STRINGS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match.  If SHAREDP is true,
the substrings may share structure with TARGET-STRING.  BODY may start
with declarations."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end substr-fn)
      `(let ((,substr-fn (if ,sharedp #'nsubseq #'subseq)))
         (do-matches (,match-start ,match-end ,regex ,target-string
                      ,result-form :start ,start :end ,end)
           (let ((,match-var
                   (funcall ,substr-fn
                            ,target-string ,match-start ,match-end)))
             ,@body))))))

(defun count-matches (regex target-string
                      &key (start 0)
                        (end (length target-string)))
  "Returns a count of all substrings of TARGET-STRING which match REGEX."
  (let ((count 0))
    (do-matches (s e regex target-string count
                 :start start :end end)
      (incf count))))

(define-compiler-macro count-matches (&whole form regex &rest rest)
  "Make sure that constant forms are compiled into scanners at
compile time."
  (cond ((constantp regex)
         `(count-matches (load-time-value (compile ,regex))
                         ,@rest))
        (t form)))

(defun all-matches (regex target-string
                    &key (start 0)
                      (end (length target-string)))
  "Returns a list containing the start and end positions of all
matches of REGEX against TARGET-STRING, i.e. if there are N matches
the list contains (* 2 N) elements.  If REGEX matches an empty string
the scan is continued one position behind this match."
  (let (result-list)
    (do-matches (match-start match-end
                 regex target-string
                 (nreverse result-list)
                 :start start :end end)
      (push match-start result-list)
      (push match-end result-list))))

(define-compiler-macro all-matches (&whole form regex &rest rest)
  "Make sure that constant forms are compiled into scanners at
compile time."
  (cond ((constantp regex)
         `(all-matches (load-time-value (compile ,regex))
                       ,@rest))
        (t form)))

(defun all-matches-as-strings (regex target-string
                               &key (start 0)
                                 (end (length target-string))
                                 sharedp)
  "Returns a list containing all substrings of TARGET-STRING which
match REGEX. If REGEX matches an empty string the scan is continued
one position behind this match. If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (let (result-list)
    (do-matches-as-strings (match regex target-string (nreverse result-list)
                             :start start :end end :sharedp sharedp)
      (push match result-list))))

(define-compiler-macro all-matches-as-strings (&whole form regex &rest rest)
  "Make sure that constant forms are compiled into scanners at
compile time."
  (cond ((constantp regex)
         `(all-matches-as-strings
           (load-time-value (compile ,regex))
           ,@rest))
        (t form)))

(defun split (regex target-string
              &key (start 0)
                (end (length target-string))
                limit
                with-registers-p
                omit-unmatched-p
                sharedp)
  "Matches REGEX against TARGET-STRING as often as possible and
returns a list of the substrings between the matches.  If
WITH-REGISTERS-P is true, substrings corresponding to matched
registers are inserted into the list as well.  If OMIT-UNMATCHED-P is
true, unmatched registers will simply be left out, otherwise they will
show up as NIL.  LIMIT limits the number of elements returned -
registers aren't counted.  If LIMIT is NIL \(or 0 which is
equivalent), trailing empty strings are removed from the result list.
If REGEX matches an empty string the scan is continued one position
behind this match.  If SHAREDP is true, the substrings may share
structure with TARGET-STRING."
  (let ((pos-list (list start))
        (counter 0))
    (when (eql limit 0)
      (setq limit nil))
    (do-scans (match-start match-end
               reg-starts reg-ends
               regex target-string nil
               :start start :end end)
      (unless (and (= match-start match-end)
                   (= match-start (car pos-list)))
        (when (and limit
                   (plusp limit)
                   (>= (incf counter) limit))
          (return))
        (push match-start pos-list)
        (when with-registers-p
          (loop for reg-start across reg-starts
                for reg-end across reg-ends
                if reg-start
                  do (push reg-start pos-list)
                     (push reg-end pos-list)
                else unless omit-unmatched-p
                       do (push nil pos-list)
                          (push nil pos-list)))
        (push match-end pos-list)))
    (push end pos-list)
    (nreverse
     (loop with substr-fn = (if sharedp #'nsubseq #'subseq)
           with string-seen = nil
           for (this-end this-start) on pos-list by #'cddr
           if (or limit
                  (setq string-seen
                        (or string-seen
                            (and this-start
                                 (> this-end this-start)))))
             collect (if this-start
                         (funcall substr-fn
                                  target-string this-start this-end)
                         nil)))))

(define-compiler-macro split (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(split (load-time-value (compile ,regex))
                 ,target-string ,@rest))
        (t form)))

(defun string-case-modifier (str from to start end)
  (declare (fixnum from to start end))
  "Checks whether all words in STR between FROM and TO are upcased,
downcased or capitalized and returns a function which applies a
corresponding case modification to strings.  Returns #'IDENTITY
otherwise, especially if words in the target area extend beyond FROM
or TO.  STR is supposed to be bounded by START and END.  It is assumed
that \(<= START FROM TO END)."
  (case
      (if (or (<= to from)
              (and (< start from)
                   (alphanumericp (char str (1- from)))
                   (alphanumericp (char str from)))
              (and (< to end)
                   (alphanumericp (char str to))
                   (alphanumericp (char str (1- to)))))
          nil
          (loop with last-char-both-case
                with current-result
                for index of-type fixnum from from below to
                for chr = (char str index)
                do (cond ((not (both-case-p chr))
                          (setq last-char-both-case nil))
                         ((upper-case-p chr)
                          (setq current-result
                                (if last-char-both-case
                                    (case current-result
                                      ((:undecided) :upcase)
                                      ((:downcase :capitalize) (return nil))
                                      ((:upcase) current-result))
                                    (case current-result
                                      ((nil) :undecided)
                                      ((:downcase) (return nil))
                                      ((:capitalize :upcase) current-result)))
                                last-char-both-case t))
                         (t
                          (setq current-result
                                (case current-result
                                  ((nil) :downcase)
                                  ((:undecided) :capitalize)
                                  ((:downcase) current-result)
                                  ((:capitalize) (if last-char-both-case
                                                     current-result
                                                     (return nil)))
                                  ((:upcase) (return nil)))
                                last-char-both-case t)))
                finally (return current-result)))
    ((nil) #'identity)
    ((:undecided :upcase) #'string-upcase)
    ((:downcase) #'string-downcase)
    ((:capitalize) #'string-capitalize)))

(defgeneric build-replacement-template (replacement-string)
  (:documentation "Converts a replacement string for REGEX-REPLACE or
REGEX-REPLACE-ALL into a replacement template which is an
S-expression."))

(let* ((*use-bmh-matchers* nil)
       (reg-scanner (compile "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defmethod build-replacement-template ((replacement-string string))
    (let ((from 0)
          (collector '()))
      (do-matches (match-start match-end reg-scanner replacement-string nil)
        (when (< from match-start)
          (push (subseq replacement-string from match-start) collector))
        (let* ((parse-start (position-if #'str:digit-char-p
                                         replacement-string
                                         :start match-start
                                         :end match-end))
               (token (if parse-start
                          (1- (parse-integer replacement-string
                                             :start parse-start
                                             :junk-allowed t))
                          (case (char replacement-string (1+ match-start))
                            ((#\&) :match)
                            ((#\`) :before-match)
                            ((#\') :after-match)
                            ((#\\) :backslash)))))
          (when (and (numberp token) (< token 0))
            (signal-invocation-error "Illegal substring ~S in replacement string."
                                     (subseq replacement-string match-start match-end)))
          (push token collector))
        (setq from match-end))
      (when (< from (length replacement-string))
        (push (subseq replacement-string from) collector))
      (nreverse collector))))

(defmethod build-replacement-template ((replacement-function function))
  (list replacement-function))

(defmethod build-replacement-template ((replacement-function-symbol symbol))
  (list replacement-function-symbol))

(defmethod build-replacement-template ((replacement-list list))
  replacement-list)

(defun build-replacement (replacement-template
                          target-string
                          start end
                          match-start match-end
                          reg-starts reg-ends
                          simple-calls
                          element-type)
  "Accepts a replacement template and the current values from the
matching process in REGEX-REPLACE or REGEX-REPLACE-ALL and returns the
corresponding string."
  (let ((reg-bound (if reg-starts
                       (array-dimension reg-starts 0)
                       0)))
    (with-output-to-string (s nil :element-type element-type)
      (loop for token in replacement-template
            do (typecase token
                 (string
                  (write-string token s))
                 (integer
                  (when (>= token reg-bound)
                    (signal-invocation-error "Reference to non-existent register ~A in replacement string."
                                             (1+ token)))
                  (when (svref reg-starts token)
                    (write-string target-string s
                                  :start (svref reg-starts token)
                                  :end (svref reg-ends token))))
                 (function
                  (write-string 
                   (cond (simple-calls
                          (apply token
                                 (nsubseq target-string match-start match-end)
                                 (map 'list
                                      (lambda (reg-start reg-end)
                                        (and reg-start
                                             (nsubseq target-string reg-start reg-end)))
                                      reg-starts reg-ends)))
                         (t
                          (funcall token
                                   target-string
                                   start end
                                   match-start match-end
                                   reg-starts reg-ends)))
                   s))
                 (symbol
                  (case token
                    ((:backslash)
                     (write-char #\\ s))
                    ((:match)
                     (write-string target-string s
                                   :start match-start
                                   :end match-end))
                    ((:before-match)
                     (write-string target-string s
                                   :start start
                                   :end match-start))
                    ((:after-match)
                     (write-string target-string s
                                   :start match-end
                                   :end end))
                    (otherwise
                     (write-string
                      (cond (simple-calls
                             (apply token
                                    (nsubseq target-string match-start match-end)
                                    (map 'list
                                         (lambda (reg-start reg-end)
                                           (and reg-start
                                                (nsubseq target-string reg-start reg-end)))
                                         reg-starts reg-ends)))
                            (t
                             (funcall token
                                      target-string
                                      start end
                                      match-start match-end
                                      reg-starts reg-ends)))
                      s)))))))))

(defun replace-aux (target-string replacement pos-list reg-list start end
                    preserve-case simple-calls element-type)
  "Auxiliary function used by REGEX-REPLACE and REGEX-REPLACE-ALL.
POS-LIST contains a list with the start and end positions of all
matches while REG-LIST contains a list of arrays representing the
corresponding register start and end positions."
  (let ((replacement-template (build-replacement-template replacement)))
    (with-output-to-string (s nil :element-type element-type)
      (loop for (from to) on (append (list start) pos-list (list end))
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                       (build-replacement replacement-template
                                                          target-string
                                                          start end
                                                          from to
                                                          reg-starts reg-ends
                                                          simple-calls
                                                          element-type)
                                       nil)
            while to
            if replace
              do (write-string (if preserve-case
                                   (funcall (string-case-modifier target-string
                                                                  from to
                                                                  start end)
                                            curr-replacement)
                                   curr-replacement)
                               s)
            else
              do (write-string target-string s :start from :end to)))))

(defun regex-replace (regex target-string replacement &key
                                                        (start 0)
                                                        (end (length target-string))
                                                        preserve-case
                                                        simple-calls
                                                        (element-type 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace the first match with REPLACEMENT.  Two values are returned;
the modified string, and T if REGEX matched or NIL otherwise.
  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.
  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)
  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.
  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.
  ELEMENT-TYPE is the element type of the resulting string."
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (if match-start
        (values (replace-aux target-string replacement
                             (list match-start match-end)
                             (list reg-starts reg-ends)
                             start end preserve-case
                             simple-calls element-type)
                t)
        (values (subseq target-string start end)
                nil))))

(define-compiler-macro regex-replace
    (&whole form regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(regex-replace (load-time-value (compile ,regex))
                         ,target-string ,replacement ,@rest))
        (t form)))

(defun regex-replace-all (regex target-string replacement &key
                                                            (start 0)
                                                            (end (length target-string))
                                                            preserve-case
                                                            simple-calls
                                                            (element-type 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace all matches with REPLACEMENT.  Two values are returned; the
modified string, and T if REGEX matched or NIL otherwise.
  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.
  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)
  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.
  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.
  ELEMENT-TYPE is the element type of the resulting string."
  (let ((pos-list '())
        (reg-list '()))
    (do-scans (match-start match-end reg-starts reg-ends regex target-string
               nil
               :start start :end end)
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if pos-list
        (values (replace-aux target-string replacement
                             (nreverse pos-list)
                             (nreverse reg-list)
                             start end preserve-case
                             simple-calls element-type)
                t)
        (values (subseq target-string start end)
                nil))))

(define-compiler-macro regex-replace-all
    (&whole form regex target-string replacement &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  (cond ((constantp regex)
         `(regex-replace-all (load-time-value (compile ,regex))
                             ,target-string ,replacement ,@rest))
        (t form)))

(let* ((*use-bmh-matchers* nil)
       (non-word-char-scanner (compile "[^a-zA-Z_0-9]")))
  (defun quote-meta-chars (string &key (start 0) (end (length string)))
    "Quote, i.e. prefix with #\\\\, all non-word characters in STRING."
    (regex-replace-all non-word-char-scanner string "\\\\\\&"
                       :start start :end end)))

(let* ((*use-bmh-matchers* nil)
       (*allow-quoting* nil)
       (quote-char-scanner (compile "\\\\Q"))
       (section-scanner (compile "\\\\Q((?:[^\\\\]|\\\\(?!Q))*?)(?:\\\\E|$)")))
  (defun quote-sections (string)
    "Replace sections inside of STRING which are enclosed by \\Q and
\\E with the quoted equivalent of these sections \(see
QUOTE-META-CHARS). Repeat this as long as there are such
sections. These sections may nest."
    (flet ((quote-substring (target-string start end match-start
                             match-end reg-starts reg-ends)
             (declare (ignore start end match-start match-end))
             (quote-meta-chars target-string
                               :start (svref reg-starts 0)
                               :end (svref reg-ends 0))))
      (loop for result = string then (regex-replace-all section-scanner
                                                        result
                                                        #'quote-substring)
            while (scan quote-char-scanner result)
            finally (return result)))))

(let* ((*use-bmh-matchers* nil)
       (comment-scanner (compile "(?s)\\(\\?#.*?\\)"))
       (extended-comment-scanner (compile "(?m:#.*?$)|(?s:\\(\\?#.*?\\))"))
       (quote-token-scanner (compile "\\\\[QE]"))
       (quote-token-replace-scanner (compile "\\\\([QE])")))
  (defun clean-comments (string &optional extended-mode)
    "Clean \(?#...) comments within STRING for quoting, i.e. convert
\\Q to Q and \\E to E.  If EXTENDED-MODE is true, also clean
end-of-line comments, i.e. those starting with #\\# and ending with
#\\Newline."
    (flet ((remove-tokens (target-string start end match-start
                           match-end reg-starts reg-ends)
             (declare (ignore start end reg-starts reg-ends))
             (loop for result = (nsubseq target-string match-start match-end)
                     then (regex-replace-all quote-token-replace-scanner result "\\1")
                   while (scan quote-token-scanner result)
                   finally (return result))))
      (regex-replace-all (if extended-mode
                             extended-comment-scanner
                             comment-scanner)
                         string
                         #'remove-tokens))))

(defun parse-tree-synonym (symbol)
  "Returns the parse tree the SYMBOL symbol is a synonym for.  Returns
NIL is SYMBOL wasn't yet defined to be a synonym."
  (get symbol 'parse-tree-synonym))

(defun (setf parse-tree-synonym) (new-parse-tree symbol)
  "Defines SYMBOL to be a synonm for the parse tree NEW-PARSE-TREE."
  (setf (get symbol 'parse-tree-synonym) new-parse-tree))

(defmacro define-parse-tree-synonym (name parse-tree)
  "Defines the symbol NAME to be a synonym for the parse tree
PARSE-TREE.  Both arguments are quoted."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (parse-tree-synonym ',name) ',parse-tree)))

(defun search (pattern string &key (start 0) (end (length string)))
  "Search for the first occurrence of PATTERN in STRING.
Returns the first match as a string and an array of captured groups,
or NIL if no match is found."
  (scan-to-strings pattern string :start start :end end))

(defun match (pattern string &key (start 0) (end (length string)))
  "Match PATTERN at the beginning of STRING.
Returns the match as a string and an array of captured groups,
or NIL if no match is found at the start."
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan pattern string :start start :end end)
    (when (and match-start (= match-start start))
      (let ((match-string (subseq string match-start match-end))
            (groups (when reg-starts
                      (map 'vector
                           (lambda (reg-start reg-end)
                             (when (and reg-start reg-end)
                               (subseq string reg-start reg-end)))
                           reg-starts reg-ends))))
        (values match-string groups)))))

(defun findall (pattern string &key (start 0) (end (length string)))
  "Find all non-overlapping matches of PATTERN in STRING.
Returns a list of matched strings."
  (all-matches-as-strings pattern string :start start :end end))

(defun finditer (pattern string &key (start 0) (end (length string)))
  "Find all non-overlapping matches of PATTERN in STRING.
Returns a list of (start . end) position pairs."
  (let ((positions (all-matches pattern string :start start :end end)))
    (loop for i from 0 below (length positions) by 2
          collect (cons (elt positions i) (elt positions (1+ i))))))

(defun sub (pattern replacement string &key (count 0) (start 0) (end (length string)))
  "Replace occurrences of PATTERN in STRING with REPLACEMENT.
If COUNT is 0 (default), replace all occurrences.
If COUNT is positive, replace at most COUNT occurrences.
Returns the modified string."
  (let* ((substring (subseq string start end))
         (prefix (subseq string 0 start))
         (suffix (subseq string end))
         (result (if (zerop count)
                     (regex-replace-all pattern substring replacement)
                     (let ((temp substring)
                           (replacements 0))
                       (loop while (and (< replacements count)
                                        (scan pattern temp))
                             do (setf temp (regex-replace pattern temp replacement))
                                (incf replacements))
                       temp))))
    (concatenate 'string prefix result suffix)))

(defun subn (pattern replacement string &key (count 0) (start 0) (end (length string)))
  "Replace occurrences of PATTERN in STRING with REPLACEMENT.
Returns two values: the modified string and the number of replacements made.
If COUNT is 0 (default), replace all occurrences.
If COUNT is positive, replace at most COUNT occurrences."
  (let* ((substring (subseq string start end))
         (prefix (subseq string 0 start))
         (suffix (subseq string end)))
    (if (zerop count)
        (multiple-value-bind (result matched-p)
            (regex-replace-all pattern substring replacement)
          (let ((replacement-count (if matched-p
                                       (count-matches pattern substring)
                                       0)))
            (values (concatenate 'string prefix result suffix) replacement-count)))
        (let ((result substring)
              (replacements 0))
          (loop while (and (< replacements count)
                           (scan pattern result))
                do (setf result (regex-replace pattern result replacement))
                   (incf replacements))
          (values (concatenate 'string prefix result suffix) replacements)))))
