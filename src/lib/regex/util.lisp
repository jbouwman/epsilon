(in-package #:lib.regex)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

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

(defconstant +whitespace-char-string+
  (coerce '(#\Space #\Tab #\Linefeed #\Return #\Page) 'string)
  "A string of all characters which are considered to be whitespace.
Same as Perl's [\\s].")

(defun whitespacep (chr)
  "Tests whether a character is whitespace, i.e. whether it would
match [\\s] in Perl."
  (find chr +whitespace-char-string+ :test #'char=))

(defmacro maybe-coerce-to-simple-string (string)
  "Coerces STRING to a simple STRING unless it already is one."
  (with-unique-names (=string=)
    `(let ((,=string= ,string))
      (cond ((simple-string-p ,=string=)
              ,=string=)
            (t
             (coerce ,=string=
                     'simple-string))))))

(declaim (inline nsubseq))
(defun nsubseq (sequence start &optional (end (length sequence)))
  "Returns a subsequence by pointing to location in original sequence."
  (make-array (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun normalize-var-list (var-list)
  "Utility function for REGISTER-GROUPS-BIND and DO-REGISTER-GROUPS.
Creates the long form \(a list of \(FUNCTION VAR) entries) out of the
short form of VAR-LIST."
  (loop for element in var-list
        if (consp element)
          nconc (loop for var in (rest element)
                      collect (list (first element) var))
        else
          collect (list '(function identity) element)))

(defun string-list-to-simple-string (string-list)
  "Concatenates a list of strings to one simple-string."
  ;; this function provided by JP Massar; note that we can't use APPLY
  ;; with CONCATENATE here because of CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (declare (fixnum total-size))
    (dolist (string string-list)
      #-:genera (declare (string string))
      (incf total-size (length string)))
    (let ((result-string (make-sequence 'simple-string
                                        total-size))
          (curr-pos 0))
      (declare (fixnum curr-pos))
      (dolist (string string-list)
        #-:genera (declare (string string))
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun complement* (test-function)
  "Like COMPLEMENT but optimized for unary functions."
  (typecase test-function
    (function
     (lambda (char)
       (declare (character char))
       (not (funcall (the function test-function) char))))
    (otherwise
     (lambda (char)
       (declare (character char))
       (not (funcall test-function char))))))
