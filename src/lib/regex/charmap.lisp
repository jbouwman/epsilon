(in-package #:epsilon.lib.regex)

(defstruct (charmap  (:constructor make-charmap%))
  ;; a bit vector mapping char codes to "booleans" (1 for set members,
  ;; 0 for others)
  (vector #*0 :type simple-bit-vector)
  ;; the smallest character code of all characters in the set
  (start 0 :type fixnum)
  ;; the upper (exclusive) bound of all character codes in the set
  (end 0 :type fixnum)
  ;; the number of characters in the set, or NIL if this is unknown
  (count nil :type (or fixnum null))
  ;; whether the charmap actually represents the complement of the set  
  (complementp nil :type boolean))

(declaim (inline in-charmap-p))
(defun in-charmap-p (char charmap)
  "Tests whether the character CHAR belongs to the set represented by CHARMAP."
  (declare (character char) (charmap charmap))
  (let* ((char-code (char-code char))
         (char-in-vector-p
          (let ((charmap-start (charmap-start charmap)))
            (declare (fixnum charmap-start))
            (and (<= charmap-start char-code)
                 (< char-code (the fixnum (charmap-end charmap)))
                 (= 1 (sbit (the simple-bit-vector (charmap-vector charmap))
                            (- char-code charmap-start)))))))
    (cond ((charmap-complementp charmap) (not char-in-vector-p))
          (t char-in-vector-p))))

(defun charmap-contents (charmap)
  "Returns a list of all characters belonging to a character map.
Only works for non-complement charmaps."
  (declare (charmap charmap))
  (and (not (charmap-complementp charmap))
       (loop for code of-type fixnum from (charmap-start charmap) to (charmap-end charmap)
             for i across (the simple-bit-vector (charmap-vector charmap))
             when (= i 1)
             collect (code-char code))))

(defun make-charmap (start end test-function &optional complementp)
  "Creates and returns a charmap representing all characters with
character codes in the interval [start end) that satisfy
TEST-FUNCTION.  The COMPLEMENTP slot of the charmap is set to the
value of the optional argument, but this argument doesn't have an
effect on how TEST-FUNCTION is used."
  (declare (fixnum start end))
  (let ((vector (make-array (- end start) :element-type 'bit))
        (count 0))
    (declare (fixnum count))
    (loop for code from start below end
          for char = (code-char code)
          for index from 0
          when char do
          (incf count)
          (setf (sbit vector index) (if (funcall test-function char) 1 0)))
    (make-charmap% :vector vector
                   :start start
                   :end end
                   ;; we don't know for sure if COMPLEMENTP is true as
                   ;; there isn't a necessary a character for each
                   ;; integer below *REGEX-CHAR-CODE-LIMIT*
                   :count (and (not complementp) count)
                   ;; make sure it's boolean
                   :complementp (not (not complementp)))))

(defun create-charmap-from-test-function (test-function start end)
  "Creates and returns a charmap representing all characters with
character codes between START and END which satisfy TEST-FUNCTION.
Tries to find the smallest interval which is necessary to represent
the character set and uses the complement representation if that
helps."
  (let (start-in end-in start-out end-out)
    ;; determine the smallest intervals containing the set and its
    ;; complement, [start-in, end-in) and [start-out, end-out) - first
    ;; the lower bound
    (loop for code from start below end
          for char = (code-char code)
          until (and start-in start-out)
          when (and char
                    (not start-in)
                    (funcall test-function char))
          do (setq start-in code)
          when (and char
                    (not start-out)
                    (not (funcall test-function char)))
          do (setq start-out code))
    (unless start-in
      ;; no character satisfied the test, so return a "pseudo" charmap
      ;; where IN-CHARMAP-P is always false
      (return-from create-charmap-from-test-function
        (make-charmap% :count 0)))
    (unless start-out
      ;; no character failed the test, so return a "pseudo" charmap
      ;; where IN-CHARMAP-P is always true
      (return-from create-charmap-from-test-function
        (make-charmap% :complementp t)))
    ;; now determine upper bound
    (loop for code from (1- end) downto start
          for char = (code-char code)
          until (and end-in end-out)
          when (and char
                    (not end-in)
                    (funcall test-function char))
          do (setq end-in (1+ code))
          when (and char
                    (not end-out)
                    (not (funcall test-function char)))
          do (setq end-out (1+ code)))
    ;; use the smaller interval
    (cond ((<= (- end-in start-in) (- end-out start-out))
           (make-charmap start-in end-in test-function))
          (t (make-charmap start-out end-out (complement* test-function) t)))))
