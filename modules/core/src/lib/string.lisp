;;;; This module provides string processing functions including
;;;; splitting, joining, trimming, case conversion, and Unicode-aware
;;;; operations.

(defpackage :epsilon.string
  (:use
   :cl
   :epsilon.syntax)
  (:shadow
   :digit-char-p
   :replace
   :reverse)
  (:local-nicknames
   (:seq :epsilon.sequence))
  (:export
   :concat
   :empty-p
   :first-char
   :last-char
   :join
   :split
   :random-string
   :ends-with-p
   :starts-with-p
   :strip
   :strip-left
   :strip-right
   :contains-p
   :trim
   ;; String replacement
   :replace-all
   :replace-first
   :replace
   ;; Case conversion
   :upcase
   :downcase
   :capitalize
   :titlecase
   ;; Substring operations
   :substring
   :substring-before
   :substring-after
   :substring-between
   ;; Search operations
   :index-of
   :last-index-of
   :count-substring
   ;; Character classification
   :digit-char-p
   :word-char-p
   :whitespacep
   :blank-p
   ;; String comparison
   :equals-ignore-case
   :compare
   :compare-ignore-case
   ;; String transformation
   :reverse
   :repeat
   :pad-left
   :pad-right
   :center
   ;; String utilities
   :nsubseq
   :string-list-to-simple-string
   ;; Byte conversion
   :string-to-octets
   :octets-to-string))

(in-package :epsilon.string)

(defun split (delimiter string)
  "Returns a lazy sequence of substrings of string, split by delimiter."
  (cond
    ((zerop (length string))
     (seq:cons "" seq:*empty*))
    ((and (>= (length string) 1) 
          (char= (char string 0) delimiter))
     (seq:cons "" (split delimiter (subseq string 1))))
    (t
     (let ((pos (position delimiter string)))
       (if pos
           (seq:cons (subseq string 0 pos)
                     (split delimiter (subseq string (1+ pos))))
           (seq:cons string seq:*empty*))))))

(defun join (delimiter strings)
  "Join STRINGS with DELIMITER between each element.
   Example: (join \", \" '(\"a\" \"b\" \"c\")) => \"a, b, c\""
  (when (seq:empty-p strings)
    (return-from join ""))
  (let* ((strings (seq:realize strings))
         (s (make-string (+ (apply #'+ (mapcar #'length strings))
                           (1- (length strings)))
                        :initial-element delimiter))
        (offset 0))
    (dolist (c strings)
      (dotimes (i (length c))
        (setf (aref s (+ i offset))
              (aref c i)))
      (incf offset (1+ (length c))))
    s))

(defun concat (&rest strings)
  "Concatenate all STRINGS into a single string.
   Example: (concat \"hello\" \" \" \"world\") => \"hello world\""
  (let ((output (make-string (apply #'+ (mapcar #'length strings))))
        (offset 0))
    (dolist (string strings output)
      (dotimes (i (length string))
        (setf (aref output (+ offset i))
              (aref string i)))
      (incf offset (length string)))))

(defun random-string (&optional (length 12))
  "Generate a random alphanumeric string of given LENGTH.
   Example: (random-string 5) => \"aB3xK\""
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))

(defun empty-p (s)
  "Return t if S is a string and non-zero-length"
  (not (and (stringp s) (plusp (length s)))))

(defun first-char (s)
  "Return the first character of a non-empty string S, or NIL"
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun last-char (s)
  "Return the last character of a non-empty string S, or NIL"
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun first-index (string char-fn)
  "Find index of first character in STRING satisfying CHAR-FN predicate.
   Example: (first-index \"hello123\" #'digit-char-p) => 5"
  (loop :for i :from 0 :to (1- (length string))
        :when (funcall char-fn (aref string i))
          :return i))

(defun last-index (string char-fn)
  "Find index after last character in STRING satisfying CHAR-FN predicate.
   Example: (last-index \"abc123\" #'alpha-char-p) => 3"
  (loop :for i :from (length string) :downto 1
        :when (funcall char-fn (aref string (1- i)))
          :return i))

(defun strip-right (string c)
  "Remove trailing occurrences of character C from STRING.
   Example: (strip-right \"hello...\" #\\.) => \"hello\""
  (let ((index (last-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string 0 index)  ; index is already 1-based, includes the non-matching character
        "")))  ; Return empty string when all characters match

(defun strip-left (string c)
  "Remove leading occurrences of character C from STRING.
   Example: (strip-left \"...hello\" #\\.) => \"hello\""
  (let ((index (first-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string index)
        "")))

(defun strip (string c)
  "Remove leading and trailing occurrences of character C from STRING.
   Example: (strip \"***text***\" #\\*) => \"text\""
  (strip-right (strip-left string c) c))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun starts-with-p (seq prefix &key (test #'char-equal))
  "Returns true if the sequence SEQ starts with the sequence
PREFIX whereby the elements are compared using TEST."
  (let ((mismatch (mismatch seq prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))

;;;; Character Classification Functions

(define-constant +whitespace-char-string+
  (coerce '(#\Space #\Tab #\Linefeed #\Return #\Page) 'string)
  "A string of all characters which are considered to be whitespace.
Same as Perl's [\\s].")

(declaim (inline digit-char-p))  
(defun digit-char-p (chr)
  "Tests whether a character is a decimal digit, i.e. the same as
Perl's [\\d].  Note that this function shadows the standard Common
Lisp function CL:DIGIT-CHAR-P."
  (char<= #\0 chr #\9))

(declaim (inline word-char-p))  
(defun word-char-p (chr)
  "Tests whether a character is a \"word\" character.  In the ASCII
charset this is equivalent to a-z, A-Z, 0-9, or _, i.e. the same as
Perl's [\\w]."
  (or (alphanumericp chr)
      (char= chr #\_)))

(defun whitespacep (chr)
  "Tests whether a character is whitespace, i.e. whether it would
match [\\s] in Perl."
  (find chr +whitespace-char-string+ :test #'char=))

;;;; String Utilities

(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Returns a subsequence by pointing to location in original sequence."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun contains-p (haystack needle &key (test #'char=))
  "Returns true if HAYSTACK contains NEEDLE."
  (and (search needle haystack :test test) t))

(defun trim (string &optional (char-bag +whitespace-char-string+))
  "Remove whitespace (or CHAR-BAG characters) from both ends of STRING."
  (string-trim char-bag string))

;;;; String Replacement Functions

(defun replace-all (string old new)
  "Replace all occurrences of OLD with NEW in STRING."
  (if (or (null string) (zerop (length old)))
      string
      (with-output-to-string (out)
        (loop with old-len = (length old)
              with start = 0
              for pos = (search old string :start2 start)
              while pos do
                (write-string string out :start start :end pos)
                (write-string new out)
                (setf start (+ pos old-len))
              finally (write-string string out :start start)))))

(defun replace-first (string old new)
  "Replace the first occurrence of OLD with NEW in STRING."
  (if (null string)
      string
      (let ((pos (search old string)))
        (if pos
            (concatenate 'string
                         (subseq string 0 pos)
                         new
                         (subseq string (+ pos (length old))))
            string))))

(defun replace (string old new &key (all nil) (count 1))
  "Replace occurrences of OLD with NEW in STRING.
   If :ALL is true, replace all occurrences.
   Otherwise, replace up to :COUNT occurrences (default 1)."
  (cond
    ((null string) string)
    (all (replace-all string old new))
    ((zerop count) string)
    (t
     (with-output-to-string (out)
       (loop with old-len = (length old)
             with start = 0
             with replacements = 0
             for pos = (search old string :start2 start)
             while (and pos (< replacements count)) do
               (write-string string out :start start :end pos)
               (write-string new out)
               (setf start (+ pos old-len))
               (incf replacements)
             finally (write-string string out :start start))))))

;;;; Case Conversion Functions

(defun upcase (string)
  "Convert STRING to uppercase."
  (when string
    (string-upcase string)))

(defun downcase (string)
  "Convert STRING to lowercase."
  (when string
    (string-downcase string)))

(defun capitalize (string)
  "Capitalize the first letter of each word in STRING."
  (when string
    (with-output-to-string (out)
      (let ((in-word nil))
        (loop for char across string do
          (cond
            ((alpha-char-p char)
             (write-char (if in-word
                             (char-downcase char)
                             (char-upcase char))
                         out)
             (setf in-word t))
            ((digit-char-p char)
             (write-char char out)
             ;; Don't set in-word for digits - next alpha char should be capitalized
             )
            (t
             (write-char char out)
             (setf in-word nil))))))))

(defun titlecase (string)
  "Convert STRING to title case (same as capitalize for now)."
  (capitalize string))

;;;; Substring Operations

(defun substring (string start &optional end)
  "Extract substring from STRING starting at START to END (or end of string)."
  (when string
    (let ((len (length string)))
      (cond
        ((>= start len) "")  ; If start is beyond string, return empty string
        ((< start 0) nil)    ; If start is negative, return nil
        (t
         (if end
             (subseq string start (min end len))
             (subseq string start)))))))

(defun substring-before (string delimiter)
  "Return substring before first occurrence of DELIMITER, or NIL if not found."
  (when string
    (let ((pos (search delimiter string)))
      (when pos
        (subseq string 0 pos)))))

(defun substring-after (string delimiter)
  "Return substring after first occurrence of DELIMITER, or NIL if not found."
  (when string
    (let ((pos (search delimiter string)))
      (when pos
        (subseq string (+ pos (length delimiter)))))))

(defun substring-between (string start-delim end-delim)
  "Return substring between START-DELIM and END-DELIM."
  (when string
    (let ((start-pos (search start-delim string)))
      (when start-pos
        (let* ((content-start (+ start-pos (length start-delim)))
               (end-pos (search end-delim string :start2 content-start)))
          (when end-pos
            (subseq string content-start end-pos)))))))

;;;; Search Operations

(defun index-of (string substring &key (start 0))
  "Find index of first occurrence of SUBSTRING in STRING, starting from START."
  (when (and string substring)
    (search substring string :start2 start)))

(defun last-index-of (string substring)
  "Find index of last occurrence of SUBSTRING in STRING."
  (when (and string substring)
    (let ((last-pos nil))
      (loop with start = 0
            for pos = (search substring string :start2 start)
            while pos do
              (setf last-pos pos)
              (setf start (1+ pos)))
      last-pos)))

(defun count-substring (string substring)
  "Count non-overlapping occurrences of SUBSTRING in STRING."
  (if (or (null string) (null substring) (zerop (length substring)))
      0
      (loop with count = 0
            with start = 0
            for pos = (search substring string :start2 start)
            while pos do
              (incf count)
              (setf start (+ pos (length substring)))
            finally (return count))))

;;;; String Analysis

(defun blank-p (string)
  "Return T if STRING is NIL, empty, or contains only whitespace."
  (or (null string)
      (zerop (length string))
      (every #'whitespacep string)))

;;;; String Comparison

(defun equals-ignore-case (string1 string2)
  "Compare STRING1 and STRING2 case-insensitively."
  (and string1 string2
       (string-equal string1 string2)))

(defun compare (string1 string2)
  "Compare STRING1 and STRING2 lexicographically.
   Returns -1 if STRING1 < STRING2, 0 if equal, 1 if STRING1 > STRING2."
  (when (and string1 string2)
    (cond
      ((string< string1 string2) -1)
      ((string> string1 string2) 1)
      (t 0))))

(defun compare-ignore-case (string1 string2)
  "Compare STRING1 and STRING2 lexicographically, ignoring case."
  (when (and string1 string2)
    (compare (downcase string1) (downcase string2))))

;;;; String Transformation

(defun reverse (string)
  "Reverse the characters in STRING."
  (when string
    (cl:reverse string)))

(defun repeat (string n)
  "Repeat STRING N times."
  (when string
    (apply #'concatenate 'string
           (make-list n :initial-element string))))

(defun pad-left (string width &optional (padding-char #\Space))
  "Pad STRING on the left with PADDING-CHAR to reach WIDTH."
  (when string
    (let ((len (length string)))
      (if (>= len width)
          string
          (concatenate 'string
                       (make-string (- width len) :initial-element padding-char)
                       string)))))

(defun pad-right (string width &optional (padding-char #\Space))
  "Pad STRING on the right with PADDING-CHAR to reach WIDTH."
  (when string
    (let ((len (length string)))
      (if (>= len width)
          string
          (concatenate 'string
                       string
                       (make-string (- width len) :initial-element padding-char))))))

(defun center (string width &optional (padding-char #\Space))
  "Center STRING within WIDTH using PADDING-CHAR."
  (when string
    (let* ((len (length string))
           (total-padding (- width len)))
      (if (<= total-padding 0)
          string
          (let* ((left-padding (floor total-padding 2))
                 (right-padding (- total-padding left-padding)))
            (concatenate 'string
                         (make-string left-padding :initial-element padding-char)
                         string
                         (make-string right-padding :initial-element padding-char)))))))

;;;; Byte conversion functions

(defun string-to-octets (string &key (encoding :utf-8))
  "Convert a string to a byte array using the specified encoding."
  (declare (ignore encoding)) ; For now, assume UTF-8
  (map 'vector #'char-code string))

(defun octets-to-string (octets &key (encoding :utf-8))
  "Convert a byte array to a string using the specified encoding."
  (declare (ignore encoding)) ; For now, assume UTF-8
  (map 'string #'code-char octets))
