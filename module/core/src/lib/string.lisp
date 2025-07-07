(defpackage :epsilon.lib.string
  (:use
   :cl
   :epsilon.lib.syntax)
  (:shadow
   :digit-char-p)
  (:local-nicknames
   (:seq :epsilon.lib.sequence))
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
   ;; Character classification
   :digit-char-p
   :word-char-p
   :whitespacep
   ;; String utilities
   :nsubseq
   :string-list-to-simple-string))

(in-package :epsilon.lib.string)

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
  (let ((output (make-string (apply #'+ (mapcar #'length strings))))
        (offset 0))
    (dolist (string strings output)
      (dotimes (i (length string))
        (setf (aref output (+ offset i))
              (aref string i)))
      (incf offset (length string)))))

(defun random-string (&optional (length 12))
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
  (loop :for i :from 0 :to (1- (length string))
        :when (funcall char-fn (aref string i))
          :return (when (< 0 i)
                    i)))

(defun last-index (string char-fn)
  (loop :for i :from (length string) :downto 0
        :when (funcall char-fn (aref string (1- i)))
          :return (when (< i (length string))
                    i)))

(defun strip-right (string c)
  (let ((index (last-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string 0 index)
        string)))

;; todo when-let

(defun strip-left (string c)
  (let ((index (first-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string index)
        string)))

;; todo ->

(defun strip (string c)
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

(defun string-list-to-simple-string (string-list)
  "Concatenates a list of strings to one simple-string."
  ;; this function provided by JP Massar; note that we can't use APPLY
  ;; with CONCATENATE here because of CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (declare (fixnum total-size))
    (dolist (string string-list)
      (declare (string string))
      (incf total-size (length string)))
    (let ((result-string (make-sequence 'simple-string
                                        total-size))
          (curr-pos 0))
      (declare (fixnum curr-pos))
      (dolist (string string-list)
        (declare (string string))
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun contains-p (haystack needle &key (test #'char=))
  "Returns true if HAYSTACK contains NEEDLE."
  (and (search needle haystack :test test) t))

(defun trim (string &optional (char-bag +whitespace-char-string+))
  "Remove whitespace (or CHAR-BAG characters) from both ends of STRING."
  (string-trim char-bag string))
