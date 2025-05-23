(in-package #:epsilon.lib.regex)

;; TODO merge all behavior dynamics into a single flag set
;; TODO Make create-scanner operate on parse tree, configuration block

(defvar *look-ahead-for-suffix* t
  "Controls whether scanners will optimistically look ahead for a
  constant suffix of a regular expression, if there is one.")

(defgeneric create-scanner (regex &key case-insensitive-mode
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

(defmethod create-scanner ((regex-string string) &key case-insensitive-mode
                                                      multi-line-mode
                                                      single-line-mode
                                                      extended-mode
                                                      destructive)
  (declare (ignore destructive))
  ;; parse the string into a parse-tree and then call CREATE-SCANNER
  ;; again
  (let* ((*extended-mode-p* extended-mode)
         (quoted-regex-string (if *allow-quoting*
                                (quote-sections (clean-comments regex-string extended-mode))
                                regex-string))
         (*syntax-error-string* (copy-seq quoted-regex-string)))
    ;; wrap the result with :GROUP to avoid infinite loops for
    ;; constant strings
    (create-scanner (cons :group (list (parse-string quoted-regex-string)))
                    :case-insensitive-mode case-insensitive-mode
                    :multi-line-mode multi-line-mode
                    :single-line-mode single-line-mode
                    :destructive t)))

(defmethod create-scanner ((scanner function) &key case-insensitive-mode
                                                   multi-line-mode
                                                   single-line-mode
                                                   extended-mode
                                                   destructive)
  (declare (ignore destructive))
  (when (or case-insensitive-mode multi-line-mode single-line-mode extended-mode)
    (signal-invocation-error "You can't use the keyword arguments to modify an existing scanner."))
  scanner)

(defmethod create-scanner ((parse-tree t) &key case-insensitive-mode
                                               multi-line-mode
                                               single-line-mode
                                               extended-mode
                                               destructive)
  (when extended-mode
    (signal-invocation-error "Extended mode doesn't make sense in parse trees."))
  ;; convert parse-tree into internal representation REGEX and at the
  ;; same time compute the number of registers and the constant string
  ;; (or anchor) the regex starts with (if any)
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
      ;; simplify REGEX by flattening nested SEQ and ALTERNATION
      ;; constructs and gathering STR objects
      (let ((regex (gather-strings (flatten regex))))
        ;; set the MIN-REST slots of the REPETITION objects
        (compute-min-rest regex 0)
        ;; set the OFFSET slots of the STR objects
        (compute-offsets regex 0)
        (let* (end-string-offset
               end-anchored-p
               ;; compute the constant string the regex ends with (if
               ;; any) and at the same time set the special variables
               ;; END-STRING-OFFSET and END-ANCHORED-P
               (end-string (end-string regex))
               ;; if we found a non-zero-length end-string we create an
               ;; efficient search function for it
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
               ;; initialize the counters for CREATE-MATCHER-AUX
               (*rep-num* 0)
               (*zero-length-num* 0)
               ;; create the actual matcher function (which does all the
               ;; work of matching the regular expression) corresponding
               ;; to REGEX and at the same time set the special
               ;; variables *REP-NUM* and *ZERO-LENGTH-NUM*
               (match-fn (create-matcher-aux regex #'identity))
               ;; if the regex starts with a string we create an
               ;; efficient search function for it
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
          ;; now create the scanner and return it
          (values (create-scanner-aux match-fn
                                      (regex-min-length regex)
                                      (or (start-anchored-p regex)
                                          ;; a dot in single-line-mode also
                                          ;; implicitly anchors the regex at
                                          ;; the start, i.e. if we can't match
                                          ;; from the first position we won't
                                          ;; match at all
                                          (and (typep starts-with 'everything)
                                               (single-line-p starts-with)))
                                      starts-with
                                      start-string-test
                                      ;; only mark regex as end-anchored if we
                                      ;; found a non-zero-length string before
                                      ;; the anchor
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
a pre-compiled scanner created by CREATE-SCANNER.  TARGET-STRING will
be coerced to a simple string if it isn't one already.  The
REAL-START-POS parameter should be ignored - it exists only for
internal purposes."))

(defmethod scan ((regex-string string) target-string
                                       &key (start 0)
                                            (end (length target-string))
                                            ((:real-start-pos *real-start-pos*) nil))
  ;; note that the scanners are optimized for simple strings so we
  ;; have to coerce TARGET-STRING into one if it isn't already
  (funcall (create-scanner regex-string)
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
  (funcall (create-scanner parse-tree)
           (maybe-coerce-to-simple-string target-string)
           start end))

(define-compiler-macro scan (&whole form regex target-string &rest rest)
  "Make sure that constant forms are compiled into scanners at compile time."
  ;; Don't pass &environment to CONSTANTP, it may not be digestable by
  ;; LOAD-TIME-VALUE, e.g., MACROLETs.
  (cond ((constantp regex)
         `(scan (load-time-value (create-scanner ,regex))
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
         `(scan-to-strings (load-time-value (create-scanner ,regex))
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
      ;; the NIL BLOCK to enable exits via (RETURN ...)
      `(block nil
         (let* ((,%start (or ,start 0))
                (,%end (or ,end (length ,target-string)))
                ,@(unless (constantp regex env)
                    ;; leave constant regular expressions as they are -
                    ;; SCAN's compiler macro will take care of them;
                    ;; otherwise create a scanner unless the regex is
                    ;; already a function (otherwise SCAN will do this
                    ;; on each iteration)
                    `((,%regex ,regex)
                      (,scanner (typecase ,%regex
                                  (function ,%regex)
                                  (t (create-scanner ,%regex)))))))
           ;; coerce TARGET-STRING to a simple string unless it is one
           ;; already (otherwise SCAN will do this on each iteration)
           (setq ,target-string
                 (maybe-coerce-to-simple-string ,target-string))
           (loop
            ;; invoke SCAN and bind the returned values to the
            ;; provided variables
            (multiple-value-bind
                (,match-start ,match-end ,reg-starts ,reg-ends)
                (scan ,(cond ((constantp regex env) regex)
                             (t scanner))
                      ,target-string :start ,%start :end ,%end
                      :real-start-pos (or ,start 0))
              ;; declare the variables to be IGNORABLE to prevent the
              ;; compiler from issuing warnings
              (declare
               (ignorable ,match-start ,match-end ,reg-starts ,reg-ends))
              (unless ,match-start
                ;; stop iteration on first failure
                (return ,result-form))
              ;; execute BODY (wrapped in LOCALLY so it can start with
              ;; declarations)
              (locally
                ,@body)
              ;; advance by one position if we had a zero-length match
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
  ;; this is a simplified form of DO-SCANS - we just provide two dummy
  ;; vars and ignore them
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
        ;; simple use DO-MATCHES to extract the substrings
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
         `(count-matches (load-time-value (create-scanner ,regex))
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
          `(all-matches (load-time-value (create-scanner ,regex))
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
            (load-time-value (create-scanner ,regex))
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
  ;; initialize list of positions POS-LIST to extract substrings with
  ;; START so that the start of the next match will mark the end of
  ;; the first substring
  (let ((pos-list (list start))
        (counter 0))
    ;; how would Larry Wall do it?
    (when (eql limit 0)
      (setq limit nil))
    (do-scans (match-start match-end
               reg-starts reg-ends
               regex target-string nil
               :start start :end end)
      (unless (and (= match-start match-end)
                   (= match-start (car pos-list)))
        ;; push start of match on list unless this would be an empty
        ;; string adjacent to the last element pushed onto the list
        (when (and limit
                   ;; perlfunc(1) says
                   ;;   If LIMIT is negative, it is treated as if
                   ;;   it were instead arbitrarily large;
                   ;;   as many fields as possible are produced.
                   (plusp limit)
                   (>= (incf counter) limit))
          (return))
        (push match-start pos-list)
        (when with-registers-p
          ;; optionally insert matched registers
          (loop for reg-start across reg-starts
                for reg-end across reg-ends
                if reg-start
                  ;; but only if they've matched
                  do (push reg-start pos-list)
                     (push reg-end pos-list)
                else unless omit-unmatched-p
                  ;; or if we're allowed to insert NIL instead
                  do (push nil pos-list)
                     (push nil pos-list)))
        ;; now end of match
        (push match-end pos-list)))
    ;; end of whole string
    (push end pos-list)
    ;; now collect substrings
    (nreverse
     (loop with substr-fn = (if sharedp #'nsubseq #'subseq)
           with string-seen = nil
           for (this-end this-start) on pos-list by #'cddr
           ;; skip empty strings from end of list
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
         `(split (load-time-value (create-scanner ,regex))
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
        ;; if it's a zero-length string or if words extend beyond FROM
        ;; or TO we return NIL, i.e. #'IDENTITY
        nil
        ;; otherwise we loop through STR from FROM to TO
        (loop with last-char-both-case
              with current-result
              for index of-type fixnum from from below to
              for chr = (char str index)
              do (cond ((not (both-case-p chr))
                         ;; this character doesn't have a case so we
                         ;; consider it as a word boundary (note that
                         ;; this differs from how \b works in Perl)
                         (setq last-char-both-case nil))
                       ((upper-case-p chr)
                         ;; an uppercase character
                         (setq current-result
                                 (if last-char-both-case
                                   ;; not the first character in a 
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
                         ;; a lowercase character
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

;; first create a scanner to identify the special parts of the
;; replacement string (eat your own dog food...)

(defgeneric build-replacement-template (replacement-string)
  (:documentation "Converts a replacement string for REGEX-REPLACE or
REGEX-REPLACE-ALL into a replacement template which is an
S-expression."))

(let* ((*use-bmh-matchers* nil)
       (reg-scanner (create-scanner "\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')")))
  (defmethod build-replacement-template ((replacement-string string))
    (let ((from 0)
          ;; COLLECTOR will hold the (reversed) template
          (collector '()))
      ;; scan through all special parts of the replacement string
      (do-matches (match-start match-end reg-scanner replacement-string nil)
        (when (< from match-start)
          ;; strings between matches are copied verbatim
          (push (subseq replacement-string from match-start) collector))
        ;; PARSE-START is true if the pattern matched a number which
        ;; refers to a register
        (let* ((parse-start (position-if #'digit-char-p
                                         replacement-string
                                         :start match-start
                                         :end match-end))
               (token (if parse-start
                        (1- (parse-integer replacement-string
                                           :start parse-start
                                           :junk-allowed t))
                        ;; if we didn't match a number we convert the
                        ;; character to a symbol
                        (case (char replacement-string (1+ match-start))
                          ((#\&) :match)
                          ((#\`) :before-match)
                          ((#\') :after-match)
                          ((#\\) :backslash)))))
          (when (and (numberp token) (< token 0))
            ;; make sure we don't accept something like "\\0"
            (signal-invocation-error "Illegal substring ~S in replacement string."
                                     (subseq replacement-string match-start match-end)))
          (push token collector))
        ;; remember where the match ended
        (setq from match-end))
      (when (< from (length replacement-string))
        ;; push the rest of the replacement string onto the list
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
  ;; the upper exclusive bound of the register numbers in the regular
  ;; expression
  (let ((reg-bound (if reg-starts
                     (array-dimension reg-starts 0)
                     0)))
    (with-output-to-string (s nil :element-type element-type)
      (loop for token in replacement-template
            do (typecase token
                 (string
                   ;; transfer string parts verbatim
                   (write-string token s))
                 (integer
                   ;; replace numbers with the corresponding registers
                   (when (>= token reg-bound)
                     ;; but only if the register was referenced in the
                     ;; regular expression
                     (signal-invocation-error "Reference to non-existent register ~A in replacement string."
                                              (1+ token)))
                   (when (svref reg-starts token)
                     ;; and only if it matched, i.e. no match results
                     ;; in an empty string
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
                       ;; just a backslash
                       (write-char #\\ s))
                     ((:match)
                       ;; the whole match
                       (write-string target-string s
                                     :start match-start
                                     :end match-end))
                     ((:before-match)
                       ;; the part of the target string before the match
                       (write-string target-string s
                                     :start start
                                     :end match-start))
                     ((:after-match)
                       ;; the part of the target string after the match
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
  ;; build the template once before we start the loop
  (let ((replacement-template (build-replacement-template replacement)))
    (with-output-to-string (s nil :element-type element-type)
      ;; loop through all matches and take the start and end of the
      ;; whole string into account
      (loop for (from to) on (append (list start) pos-list (list end))
            ;; alternate between replacement and no replacement
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                     ;; build the replacement string
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
                                 ;; modify the case of the replacement
                                 ;; string if necessary
                                 (funcall (string-case-modifier target-string
                                                                from to
                                                                start end)
                                          curr-replacement)
                                 curr-replacement)
                               s)
            else
              ;; no replacement
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
         `(regex-replace (load-time-value (create-scanner ,regex))
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
         `(regex-replace-all (load-time-value (create-scanner ,regex))
                             ,target-string ,replacement ,@rest))
        (t form)))

(let* ((*use-bmh-matchers* nil)
       (non-word-char-scanner (create-scanner "[^a-zA-Z_0-9]")))
  (defun quote-meta-chars (string &key (start 0) (end (length string)))
    "Quote, i.e. prefix with #\\\\, all non-word characters in STRING."
    (regex-replace-all non-word-char-scanner string "\\\\\\&"
                       :start start :end end)))

(let* ((*use-bmh-matchers* nil)
       (*allow-quoting* nil)
       (quote-char-scanner (create-scanner "\\\\Q"))
       (section-scanner (create-scanner "\\\\Q((?:[^\\\\]|\\\\(?!Q))*?)(?:\\\\E|$)")))
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
       (comment-scanner (create-scanner "(?s)\\(\\?#.*?\\)"))
       (extended-comment-scanner (create-scanner "(?m:#.*?$)|(?s:\\(\\?#.*?\\))"))
       (quote-token-scanner (create-scanner "\\\\[QE]"))
       (quote-token-replace-scanner (create-scanner "\\\\([QE])")))
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
                   ;; we must probably repeat this because the comment
                   ;; can contain substrings like \\Q
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
