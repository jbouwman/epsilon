;;;; properties.lisp - Property-based testing
;;;;
;;;; Provides QuickCheck-style property-based testing with generators,
;;;; shrinking, and property combinators.

(defpackage :epsilon.properties
  (:use :cl)
  (:local-nicknames
   (:opt :epsilon.option)
   (:result :epsilon.result))
  (:export
   ;; Generators
   #:gen
   #:gen-integer
   #:gen-float
   #:gen-string
   #:gen-symbol
   #:gen-list
   #:gen-vector
   #:gen-one-of
   #:gen-elements
   #:gen-frequency
   #:gen-such-that
   #:gen-fmap
   #:gen-bind
   #:gen-return
   #:gen-tuple
   #:gen-char
   #:gen-boolean
   #:sample

   ;; Properties
   #:defprop
   #:for-all
   #:prop
   #:==>
   #:check
   #:quickcheck

   ;; Shrinking
   #:shrink
   #:shrink-integer
   #:shrink-list
   #:shrink-string

   ;; Test integration
   #:defproptest
   #:*num-tests*
   #:*max-shrinks*

   ;; Property result accessors
   #:property-result
   #:property-result-passed-p
   #:property-result-num-tests
   #:property-result-failing-input
   #:property-result-shrunk-input
   #:property-result-exception))

(in-package :epsilon.properties)

;;; Configuration

(defvar *num-tests* 100
  "Default number of tests to run for each property")

(defvar *max-shrinks* 100
  "Maximum number of shrink attempts")

(defvar *current-size* 0
  "Current size parameter for generation")

;;; Generator type

(defstruct gen
  "A generator produces random values given a size parameter"
  (generate nil :type function)  ; (size rng) -> value
  (shrink nil :type (or null function)))  ; value -> list of smaller values

(defun sample (generator &key (count 10) (size 30))
  "Generate sample values from a generator.

   Example:
   (sample (gen-integer :min 0 :max 100) :count 5)
   => (42 17 91 3 68)"
  (loop repeat count
        collect (funcall (gen-generate generator) size (random most-positive-fixnum))))

;;; Basic generators

(defun gen-return (value)
  "Generator that always returns VALUE.

   Example:
   (sample (gen-return 42)) => (42 42 42 ...)"
  (make-gen
   :generate (lambda (size rng)
               (declare (ignore size rng))
               value)
   :shrink (constantly nil)))

(defun gen-integer (&key (min most-negative-fixnum) (max most-positive-fixnum))
  "Generator for integers in range [MIN, MAX].

   Example:
   (sample (gen-integer :min 0 :max 100))"
  (make-gen
   :generate (lambda (size rng)
               (declare (ignore size))
               (let ((range (- max min)))
                 (if (zerop range)
                     min
                     (+ min (mod rng (1+ range))))))
   :shrink (lambda (n)
             (shrink-integer n :min min))))

(defun gen-float (&key (min -1.0e10) (max 1.0e10))
  "Generator for floating-point numbers in range [MIN, MAX].

   Example:
   (sample (gen-float :min 0.0 :max 1.0))"
  (make-gen
   :generate (lambda (size rng)
               (declare (ignore size))
               (+ min (* (/ (mod rng 1000000) 1000000.0) (- max min))))
   :shrink (constantly nil)))

(defun gen-boolean ()
  "Generator for boolean values.

   Example:
   (sample (gen-boolean)) => (T NIL T T NIL ...)"
  (make-gen
   :generate (lambda (size rng)
               (declare (ignore size))
               (evenp rng))
   :shrink (lambda (b)
             (if b (list nil) nil))))

(defun gen-char (&key (min #\Space) (max #\~))
  "Generator for characters in ASCII range.

   Example:
   (sample (gen-char))"
  (make-gen
   :generate (lambda (size rng)
               (declare (ignore size))
               (code-char (+ (char-code min)
                             (mod rng (1+ (- (char-code max) (char-code min)))))))
   :shrink (constantly nil)))

(defun gen-string (&key (max-length 50) (char-gen (gen-char)))
  "Generator for strings.

   Example:
   (sample (gen-string :max-length 10))"
  (make-gen
   :generate (lambda (size rng)
               (let ((length (mod rng (1+ (min size max-length)))))
                 (coerce
                  (loop repeat length
                        for i from 0
                        collect (funcall (gen-generate char-gen)
                                        size
                                        (mod (* rng (1+ i)) most-positive-fixnum)))
                  'string)))
   :shrink #'shrink-string))

(defun gen-symbol (&key (max-length 20))
  "Generator for symbols.

   Example:
   (sample (gen-symbol))"
  (gen-fmap
   (lambda (s) (intern (string-upcase s)))
   (gen-string :max-length max-length
               :char-gen (gen-char :min #\a :max #\z))))

(defun gen-list (element-gen &key (max-length 50))
  "Generator for lists of elements from ELEMENT-GEN.

   Example:
   (sample (gen-list (gen-integer :min 0 :max 10)))"
  (make-gen
   :generate (lambda (size rng)
               (let ((length (mod rng (1+ (min size max-length)))))
                 (loop repeat length
                       for i from 0
                       collect (funcall (gen-generate element-gen)
                                       size
                                       (mod (* rng (1+ i)) most-positive-fixnum)))))
   :shrink (lambda (lst)
             (shrink-list lst (gen-shrink element-gen)))))

(defun gen-vector (element-gen &key (max-length 50))
  "Generator for vectors of elements from ELEMENT-GEN.

   Example:
   (sample (gen-vector (gen-integer)))"
  (gen-fmap #'list-to-vector (gen-list element-gen :max-length max-length)))

(defun list-to-vector (lst)
  (coerce lst 'vector))

;;; Generator combinators

(defun gen-one-of (&rest generators)
  "Generator that randomly chooses from GENERATORS.

   Example:
   (sample (gen-one-of (gen-return :a) (gen-return :b)))"
  (make-gen
   :generate (lambda (size rng)
               (let ((gen (nth (mod rng (length generators)) generators)))
                 (funcall (gen-generate gen) size (mod (* rng 7) most-positive-fixnum))))
   :shrink (constantly nil)))

(defun gen-elements (&rest elements)
  "Generator that chooses from ELEMENTS.

   Example:
   (sample (gen-elements :red :green :blue))"
  (apply #'gen-one-of (mapcar #'gen-return elements)))

(defun gen-frequency (weighted-generators)
  "Generator with weighted random selection.
   WEIGHTED-GENERATORS is list of (weight . generator).

   Example:
   (sample (gen-frequency '((9 . ,(gen-integer :min 0 :max 100))
                            (1 . ,(gen-return nil)))))"
  (let* ((total (reduce #'+ weighted-generators :key #'car))
         (sorted (sort (copy-list weighted-generators) #'< :key #'car)))
    (make-gen
     :generate (lambda (size rng)
                 (let ((choice (mod rng total))
                       (cumulative 0))
                   (dolist (wg sorted)
                     (incf cumulative (car wg))
                     (when (< choice cumulative)
                       (return (funcall (gen-generate (cdr wg)) size
                                       (mod (* rng 11) most-positive-fixnum))))))))))

(defun gen-such-that (predicate generator &key (max-tries 100))
  "Generator that filters values satisfying PREDICATE.

   Example:
   (sample (gen-such-that #'evenp (gen-integer :min 0 :max 100)))"
  (make-gen
   :generate (lambda (size rng)
               (loop repeat max-tries
                     for i from 0
                     for value = (funcall (gen-generate generator) size
                                         (mod (* rng (1+ i)) most-positive-fixnum))
                     when (funcall predicate value)
                       do (return value)
                     finally (error "gen-such-that: couldn't find value after ~A tries" max-tries)))
   :shrink (lambda (value)
             (remove-if-not predicate
                           (when (gen-shrink generator)
                             (funcall (gen-shrink generator) value))))))

(defun gen-fmap (f generator)
  "Apply function F to generated values.

   Example:
   (sample (gen-fmap #'1+ (gen-integer :min 0 :max 10)))"
  (make-gen
   :generate (lambda (size rng)
               (funcall f (funcall (gen-generate generator) size rng)))
   :shrink (when (gen-shrink generator)
             (lambda (value)
               ;; Note: shrinking mapped values is tricky
               ;; This is a simplified version
               nil))))

(defun gen-bind (generator f)
  "Monadic bind for generators - F takes a value and returns a generator.

   Example:
   (sample (gen-bind (gen-integer :min 1 :max 5)
                     (lambda (n) (gen-list (gen-integer) :max-length n))))"
  (make-gen
   :generate (lambda (size rng)
               (let* ((value (funcall (gen-generate generator) size rng))
                      (next-gen (funcall f value)))
                 (funcall (gen-generate next-gen) size
                         (mod (* rng 13) most-positive-fixnum))))
   :shrink (constantly nil)))

(defun gen-tuple (&rest generators)
  "Generate a tuple (list) of values from each generator.

   Example:
   (sample (gen-tuple (gen-integer) (gen-string) (gen-boolean)))"
  (make-gen
   :generate (lambda (size rng)
               (loop for gen in generators
                     for i from 0
                     collect (funcall (gen-generate gen) size
                                     (mod (* rng (1+ i)) most-positive-fixnum))))
   :shrink (lambda (tuple)
             ;; Shrink each element
             (loop for value in tuple
                   for gen in generators
                   for i from 0
                   when (gen-shrink gen)
                     append (loop for shrunk in (funcall (gen-shrink gen) value)
                                  collect (let ((new-tuple (copy-list tuple)))
                                           (setf (nth i new-tuple) shrunk)
                                           new-tuple))))))

;;; Shrinking

(defun shrink-integer (n &key (min most-negative-fixnum))
  "Shrink an integer toward MIN (default 0 or MIN).

   Example:
   (shrink-integer 100) => (0 50 75 88 94 97 99)"
  (let ((target (max 0 min)))
    (when (/= n target)
      (remove-duplicates
       (list* target
              (floor (+ n target) 2)
              (if (> n target)
                  (list (1- n))
                  (list (1+ n))))))))

(defun shrink-list (lst element-shrinker)
  "Shrink a list by removing elements or shrinking elements.

   Example:
   (shrink-list '(1 2 3) #'shrink-integer)"
  (let ((shrinks '()))
    ;; Try removing each element
    (loop for i from 0 below (length lst)
          do (push (append (subseq lst 0 i) (subseq lst (1+ i))) shrinks))
    ;; Try shrinking each element
    (when element-shrinker
      (loop for i from 0 below (length lst)
            for elem = (nth i lst)
            for elem-shrinks = (funcall element-shrinker elem)
            do (dolist (shrunk elem-shrinks)
                 (let ((new-list (copy-list lst)))
                   (setf (nth i new-list) shrunk)
                   (push new-list shrinks)))))
    (nreverse shrinks)))

(defun shrink-string (s)
  "Shrink a string by removing characters.

   Example:
   (shrink-string \"abc\") => (\"bc\" \"ac\" \"ab\" \"\")"
  (when (plusp (length s))
    (list* ""
           (loop for i from 0 below (length s)
                 collect (concatenate 'string
                                     (subseq s 0 i)
                                     (subseq s (1+ i)))))))

(defun shrink (value generator)
  "Get shrunk versions of VALUE using GENERATOR's shrink function.

   Example:
   (shrink 100 (gen-integer)) => (0 50 75 ...)"
  (when (gen-shrink generator)
    (funcall (gen-shrink generator) value)))

;;; Property checking

(defstruct property-result
  "Result of checking a property"
  (passed-p nil :type boolean)
  (num-tests 0 :type integer)
  (failing-input nil)
  (shrunk-input nil)
  (exception nil))

(defun check-property (property generators &key (num-tests *num-tests*))
  "Check a property over random inputs from GENERATORS.

   PROPERTY is a function that takes generated values and returns T or NIL.
   GENERATORS is a list of generators for the property's arguments.

   Returns a PROPERTY-RESULT."
  (let ((tuple-gen (apply #'gen-tuple generators)))
    (loop repeat num-tests
          for test-num from 1
          for size = (mod test-num 100)
          for rng = (random most-positive-fixnum)
          for inputs = (funcall (gen-generate tuple-gen) size rng)
          do (handler-case
                 (unless (apply property inputs)
                   (return (make-property-result
                            :passed-p nil
                            :num-tests test-num
                            :failing-input inputs
                            :shrunk-input (shrink-failing-input property inputs tuple-gen))))
               (error (e)
                 (return (make-property-result
                          :passed-p nil
                          :num-tests test-num
                          :failing-input inputs
                          :exception e))))
          finally (return (make-property-result
                          :passed-p t
                          :num-tests num-tests)))))

(defun shrink-failing-input (property inputs generator)
  "Attempt to shrink failing inputs to a minimal failing case."
  (let ((shrunk-inputs inputs)
        (attempts 0))
    (loop while (< attempts *max-shrinks*)
          for candidates = (shrink shrunk-inputs generator)
          for found-smaller = nil
          do (dolist (candidate candidates)
               (incf attempts)
               (when (>= attempts *max-shrinks*)
                 (return-from shrink-failing-input shrunk-inputs))
               (handler-case
                   (unless (apply property candidate)
                     (setf shrunk-inputs candidate
                           found-smaller t)
                     (return))
                 (error ()
                   (setf shrunk-inputs candidate
                         found-smaller t)
                   (return))))
             (unless found-smaller
               (return)))
    shrunk-inputs))

;;; Property DSL

(defmacro for-all (bindings &body property-body)
  "Define a property over generated values.

   Example:
   (for-all ((x (gen-integer))
             (y (gen-integer)))
     (= (+ x y) (+ y x)))"
  (let ((vars (mapcar #'first bindings))
        (gens (mapcar #'second bindings)))
    `(check-property
      (lambda ,vars ,@property-body)
      (list ,@gens))))

(defmacro prop (name bindings &body body)
  "Define a named property.

   Example:
   (prop addition-commutative ((x (gen-integer)) (y (gen-integer)))
     (= (+ x y) (+ y x)))"
  (let ((vars (mapcar #'first bindings))
        (gens (mapcar #'second bindings)))
    `(defun ,name ()
       (check-property
        (lambda ,vars ,@body)
        (list ,@gens)))))

(defmacro ==> (condition property)
  "Implication: only check PROPERTY when CONDITION is true.
   Discards test cases where CONDITION is false.

   Example:
   (for-all ((x (gen-integer)))
     (==> (> x 0)
          (> (abs x) 0)))"
  `(or (not ,condition) ,property))

(defun quickcheck (property-fn generators &key (num-tests *num-tests*) (verbose nil))
  "Run QuickCheck on a property.

   Example:
   (quickcheck (lambda (x y) (= (+ x y) (+ y x)))
               (list (gen-integer) (gen-integer)))"
  (let ((result (check-property property-fn generators :num-tests num-tests)))
    (if (property-result-passed-p result)
        (when verbose
          (format t "OK, passed ~A tests.~%" (property-result-num-tests result)))
        (progn
          (format t "FAILED after ~A tests.~%" (property-result-num-tests result))
          (format t "Failing input: ~S~%" (property-result-failing-input result))
          (when (property-result-shrunk-input result)
            (format t "Shrunk to: ~S~%" (property-result-shrunk-input result)))
          (when (property-result-exception result)
            (format t "Exception: ~A~%" (property-result-exception result)))))
    result))

;;; Test integration

(defmacro defproptest (name bindings &body body)
  "Define a property-based test that integrates with epsilon.test.

   Example:
   (defproptest addition-is-commutative
       ((x (gen-integer :min -100 :max 100))
        (y (gen-integer :min -100 :max 100)))
     (= (+ x y) (+ y x)))"
  (let* ((vars (mapcar #'first bindings))
         (gens (mapcar #'second bindings))
         (deftest-sym (intern "DEFTEST" (find-package :epsilon.test)))
         (is-sym (intern "IS" (find-package :epsilon.test))))
    `(,deftest-sym ,name
       (let ((result (check-property
                      (lambda ,vars ,@body)
                      (list ,@gens))))
         (,is-sym (property-result-passed-p result)
                  (format nil "Property failed on input: ~S~@[~%Shrunk to: ~S~]"
                          (property-result-failing-input result)
                          (property-result-shrunk-input result)))))))
