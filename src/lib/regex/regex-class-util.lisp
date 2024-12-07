(in-package #:epsilon.lib.regex)

;;; The following four methods allow a VOID object to behave like a
;;; zero-length STR object (only readers needed)

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

;;; Note that COPY-REGEX and REMOVE-REGISTERS could have easily been
;;; wrapped into one function. Maybe in the next release...

;;; Further note that this function is used by CONVERT to factor out
;;; complicated repetitions, i.e. cases like
;;;   (a)* -> (?:a*(a))?
;;; This won't work for, say,
;;;   ((a)|(b))* -> (?:(?:a|b)*((a)|(b)))?
;;; and therefore we stop REGISTER removal once we see an ALTERNATION.

(defgeneric remove-registers (regex)
  (:documentation "Returns a deep copy of a REGEX (see COPY-REGEX) and
optionally removes embedded REGISTER objects if possible and if the
special variable REMOVE-REGISTERS-P is true."))

(defmethod remove-registers ((register register))
  (declare (special remove-registers-p reg-seen))
  (cond (remove-registers-p
          (remove-registers (regex register)))
        (t
          ;; mark REG-SEEN as true so enclosing REPETITION objects
          ;; (see method below) know if they contain a register or not
          (setq reg-seen t)
          (copy-regex register))))

(defmethod remove-registers ((repetition repetition))
  (let* (reg-seen
         (inner-regex (remove-registers (regex repetition))))
    ;; REMOVE-REGISTERS will set REG-SEEN (see method above) if
    ;; (REGEX REPETITION) contains a REGISTER
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
  ;; an ALTERNATION, so we can't remove REGISTER objects further down
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
  ;; we might have degenerate cases like (:SEQUENCE :VOID ...)
  ;; due to the parsing process
  (let ((cleaned-elements (remove-if #'(lambda (element)
                                         (typep element 'void))
                                     (elements seq))))
    (and (= 1 (length cleaned-elements))
         (everythingp (first cleaned-elements)))))

(defmethod everythingp ((alternation alternation))
  (with-slots (choices)
      alternation
    (and (= 1 (length choices))
         ;; this is unlikely to happen for human-generated regexes,
         ;; but machine-generated ones might look like this
         (everythingp (first choices)))))

(defmethod everythingp ((repetition repetition))
  (with-slots (maximum minimum regex)
      repetition
    (and maximum
         (= 1 minimum maximum)
         ;; treat "<regex>{1,1}" like "<regex>"
         (everythingp regex))))

(defmethod everythingp ((register register))
  (everythingp (regex register)))

(defmethod everythingp ((standalone standalone))
  (everythingp (regex standalone)))

(defmethod everythingp ((everything everything))
  everything)

(defmethod everythingp ((regex regex))
  ;; the general case for ANCHOR, BACK-REFERENCE, BRANCH, CHAR-CLASS,
  ;; LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY
  nil)

(defgeneric regex-length (regex)
  (:documentation "Return the length of REGEX if it is fixed, NIL otherwise."))

(defmethod regex-length ((seq seq))
  ;; simply add all inner lengths unless one of them is NIL
  (loop for sub-regex in (elements seq)
        for len = (regex-length sub-regex)
        if (not len) do (return nil)
        sum len))

(defmethod regex-length ((alternation alternation))
  ;; only return a true value if all inner lengths are non-NIL and
  ;; mutually equal
  (loop for sub-regex in (choices alternation)
        for old-len = nil then len
        for len = (regex-length sub-regex)
        if (or (not len)
               (and old-len (/= len old-len))) do (return nil)
        finally (return len)))

(defmethod regex-length ((branch branch))
  ;; only return a true value if both alternations have a length and
  ;; if they're equal
  (let ((then-length (regex-length (then-regex branch))))
    (and then-length
         (eql then-length (regex-length (else-regex branch)))
         then-length)))

(defmethod regex-length ((repetition repetition))
  ;; we can only compute the length of a REPETITION object if the
  ;; number of repetitions is fixed; note that we don't call
  ;; REGEX-LENGTH for the inner regex, we assume that the LEN slot is
  ;; always set correctly
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
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
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
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  0)

(defgeneric regex-min-length (regex)
  (:documentation "Returns the minimal length of REGEX."))

(defmethod regex-min-length ((seq seq))
  ;; simply add all inner minimal lengths
  (loop for sub-regex in (elements seq)
        for len = (regex-min-length sub-regex)
        sum len))

(defmethod regex-min-length ((alternation alternation))
  ;; minimal length of an alternation is the minimal length of the
  ;; "shortest" element
  (loop for sub-regex in (choices alternation)
        for len = (regex-min-length sub-regex)
        minimize len))

(defmethod regex-min-length ((branch branch))
  ;; minimal length of both alternations
  (min (regex-min-length (then-regex branch))
       (regex-min-length (else-regex branch))))

(defmethod regex-min-length ((repetition repetition))
  ;; obviously the product of the inner minimal length and the minimal
  ;; number of repetitions
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
  ;; the general case for ANCHOR, BACK-REFERENCE, LOOKAHEAD,
  ;; LOOKBEHIND, VOID, and WORD-BOUNDARY
  0)

(defgeneric compute-offsets (regex start-pos)
  (:documentation "Returns the offset the following regex would have
relative to START-POS or NIL if we can't compute it. Sets the OFFSET
slot of REGEX to START-POS if REGEX is a STR. May also affect OFFSET
slots of STR objects further down the tree."))

;; note that we're actually only interested in the offset of
;; "top-level" STR objects (see ADVANCE-FN in the SCAN function) so we
;; can stop at variable-length alternations and don't need to descend
;; into repetitions

(defmethod compute-offsets ((seq seq) start-pos)
  (loop for element in (elements seq)
        ;; advance offset argument for next call while looping through
        ;; the elements
        for pos = start-pos then curr-offset
        for curr-offset = (compute-offsets element pos)
        while curr-offset
        finally (return curr-offset)))

(defmethod compute-offsets ((alternation alternation) start-pos)
  (loop for choice in (choices alternation)
        for old-offset = nil then curr-offset
        for curr-offset = (compute-offsets choice start-pos)
        ;; we stop immediately if two alternations don't result in the
        ;; same offset
        if (or (not curr-offset)
               (and old-offset (/= curr-offset old-offset)))
          do (return nil)
        finally (return curr-offset)))

(defmethod compute-offsets ((branch branch) start-pos)
  ;; only return offset if both alternations have equal value
  (let ((then-offset (compute-offsets (then-regex branch) start-pos)))
    (and then-offset
         (eql then-offset (compute-offsets (else-regex branch) start-pos))
         then-offset)))

(defmethod compute-offsets ((repetition repetition) start-pos)
  ;; no need to descend into the inner regex
  (with-slots (len minimum maximum)
      repetition
    (if (and len
             (eq minimum maximum))
      ;; fixed number of repetitions, so we know how to proceed
      (+ start-pos (* minimum len))
      ;; otherwise return NIL
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
  ;; with enough effort we could possibly do better here, but
  ;; currently we just give up and return NIL
  (declare (ignore start-pos))
  nil)

(defmethod compute-offsets ((filter filter) start-pos)
  (let ((len (len filter)))
    (if len
      (+ start-pos len)
      nil)))

(defmethod compute-offsets ((regex regex) start-pos)
  ;; the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  ;; WORD-BOUNDARY (which all have zero-length)
  start-pos)
