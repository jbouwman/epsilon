(in-package #:lib.regex)

;;; special variables used by the lexer/parser combo

(defvar *extended-mode-p* nil
  "Whether the parser will start in extended mode.")
(declaim (boolean *extended-mode-p*))

;;; special variables used by the SCAN function and the matchers

(defvar *regex-char-code-limit* char-code-limit
  "The upper exclusive bound on the char-codes of characters which can
occur in character classes.  Change this value BEFORE creating
scanners if you don't need full Unicode support.")
(declaim (fixnum *regex-char-code-limit*))
  
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
  "Whether the scanners created by CREATE-SCANNER should use the \(fast
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

(defvar *allow-named-registers* nil
  "Whether the parser should support AllegroCL's named registers
\(?<name>\"<regex>\") and back-reference \\k<name> syntax.")

(pushnew :lib.regex *features*)
