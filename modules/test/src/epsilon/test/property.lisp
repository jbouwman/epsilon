;;;; Property-Based Testing for epsilon.test
;;;;
;;;; Provides QuickCheck-style property-based testing where properties
;;;; are tested against randomly generated inputs.
;;;;
;;;; Inspired by QuickCheck (Haskell), proptest (Rust), Hypothesis (Python).

(defpackage epsilon.test.property
  (:use :cl :epsilon.symbol)
  (:require (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.string str)
            (epsilon.log log))
  (:enter t))

;;; Configuration

(defvar *num-tests* 100
  "Default number of test iterations for property tests.")

(defvar *seed* nil
  "Random seed for reproducibility. NIL means use current time.")

(defvar *max-shrink-iterations* 100
  "Maximum number of shrinking attempts.")

(defvar *current-iteration* 0
  "Current test iteration number.")

(defvar *shrinking* nil
  "T when currently shrinking a failing case.")

;;; Random State Management

(defvar *property-random-state* nil
  "Random state for property tests.")

(defun init-random-state (&optional seed)
  "Initialize random state with optional seed for reproducibility.
   SBCL doesn't support seeding make-random-state with integers,
   so we use sb-ext:seed-random-state for reproducibility."
  (setf *property-random-state*
        (if seed
            ;; Use SBCL's seed-random-state for reproducible sequences
            (sb-ext:seed-random-state seed)
            (make-random-state t))))

(defun property-random (n)
  "Generate random integer in [0, n) using property test random state."
  (unless *property-random-state*
    (init-random-state *seed*))
  (random n *property-random-state*))

;;; Generator Protocol

(defclass generator ()
  ((generate-fn :initarg :generate
                :reader generator-fn
                :documentation "Function that generates a random value")
   (shrink-fn :initarg :shrink
              :initform nil
              :reader shrink-fn
              :documentation "Function that returns smaller versions of a value"))
  (:documentation "A generator produces random values and can shrink them."))

(defun generate (gen)
  "Generate a random value from generator GEN."
  (funcall (generator-fn gen)))

(defun shrink (gen value)
  "Return a list of smaller versions of VALUE using generator GEN's shrinker."
  (if (shrink-fn gen)
      (funcall (shrink-fn gen) value)
      nil))

(defun make-generator (generate-fn &optional shrink-fn)
  "Create a new generator with the given generate and shrink functions."
  (make-instance 'generator
                 :generate generate-fn
                 :shrink shrink-fn))

;;; Built-in Generators

;; Integer generators

(defun gen-integer (&key (min most-negative-fixnum) (max most-positive-fixnum))
  "Generate random integers in range [MIN, MAX]."
  (make-generator
   (lambda ()
     (+ min (property-random (1+ (- max min)))))
   (lambda (n)
     ;; Shrink towards zero
     (remove-duplicates
      (remove-if-not
       (lambda (x) (and (>= x min) (<= x max)))
       (list 0
             (truncate n 2)
             (- n 1)
             (+ n 1)
             (- n (truncate n 2))))))))

(defun gen-natural ()
  "Generate non-negative integers."
  (gen-integer :min 0 :max 1000000))

(defun gen-positive ()
  "Generate positive integers (>= 1)."
  (gen-integer :min 1 :max 1000000))

;; Boolean generator

(defun gen-boolean ()
  "Generate random booleans."
  (make-generator
   (lambda () (= 1 (property-random 2)))
   (lambda (b) (if b (list nil) nil))))

;; Character generators

(defun gen-char (&key (min 0) (max 127))
  "Generate random characters in code range [MIN, MAX]."
  (make-generator
   (lambda ()
     (code-char (+ min (property-random (- max min)))))
   nil))

(defun gen-ascii-char ()
  "Generate printable ASCII characters (32-126)."
  (gen-char :min 32 :max 126))

(defun gen-alpha-char ()
  "Generate alphabetic characters."
  (make-generator
   (lambda ()
     (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
       (char chars (property-random (length chars)))))
   nil))

(defun gen-alphanumeric-char ()
  "Generate alphanumeric characters."
  (make-generator
   (lambda ()
     (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
       (char chars (property-random (length chars)))))
   nil))

;; String generators

(defun gen-string (&key (max-length 100) (char-gen (gen-ascii-char)))
  "Generate random strings up to MAX-LENGTH using CHAR-GEN for characters."
  (make-generator
   (lambda ()
     (let* ((len (property-random (1+ max-length)))
            (chars (make-string len)))
       (dotimes (i len)
         (setf (char chars i) (generate char-gen)))
       chars))
   (lambda (s)
     ;; Shrink by removing characters or taking prefixes
     (let ((len (length s)))
       (when (> len 0)
         (remove-duplicates
          (list ""
                (subseq s 0 (truncate len 2))
                (subseq s 1)
                (subseq s 0 (1- len)))
          :test #'string=))))))

(defun gen-ascii-string (&key (max-length 100))
  "Generate ASCII strings up to MAX-LENGTH."
  (gen-string :max-length max-length :char-gen (gen-ascii-char)))

(defun gen-alpha-string (&key (max-length 100))
  "Generate alphabetic strings up to MAX-LENGTH."
  (gen-string :max-length max-length :char-gen (gen-alpha-char)))

;; List generators

(defun gen-list (element-gen &key (max-length 50))
  "Generate lists of elements from ELEMENT-GEN up to MAX-LENGTH."
  (make-generator
   (lambda ()
     (let ((len (property-random (1+ max-length))))
       (loop repeat len collect (generate element-gen))))
   (lambda (lst)
     ;; Shrink by removing elements or shrinking individual elements
     (when lst
       (let ((shrinks nil))
         ;; Empty list
         (push nil shrinks)
         ;; Remove first element
         (push (cdr lst) shrinks)
         ;; Remove last element
         (when (> (length lst) 1)
           (push (butlast lst) shrinks))
         ;; Take first half
         (let ((half (truncate (length lst) 2)))
           (when (> half 0)
             (push (subseq lst 0 half) shrinks)))
         ;; Shrink individual elements
         (loop for i from 0 below (min 3 (length lst))
               do (let* ((elem (nth i lst))
                         (elem-shrinks (shrink element-gen elem)))
                    (dolist (shrunk elem-shrinks)
                      (let ((new-list (copy-list lst)))
                        (setf (nth i new-list) shrunk)
                        (push new-list shrinks)))))
         (remove-duplicates shrinks :test #'equal))))))

(defun gen-vector (element-gen &key (max-length 50))
  "Generate vectors of elements from ELEMENT-GEN up to MAX-LENGTH."
  (make-generator
   (lambda ()
     (let ((len (property-random (1+ max-length))))
       (coerce (loop repeat len collect (generate element-gen)) 'vector)))
   (lambda (vec)
     (mapcar (lambda (lst) (coerce lst 'vector))
             (shrink (gen-list element-gen :max-length max-length)
                     (coerce vec 'list))))))

;; Combinators

(defun gen-one-of (&rest generators)
  "Generate a value from one of the given generators, chosen randomly."
  (make-generator
   (lambda ()
     (generate (nth (property-random (length generators)) generators)))
   nil))

(defun gen-element (elements)
  "Generate one of the given ELEMENTS."
  (make-generator
   (lambda ()
     (nth (property-random (length elements)) elements))
   (lambda (elem)
     ;; Shrink to earlier elements in the list
     (let ((pos (position elem elements :test #'equal)))
       (when (and pos (> pos 0))
         (list (first elements)))))))

(defun gen-frequency (weighted-generators)
  "Generate from weighted generators. WEIGHTED-GENERATORS is ((weight . gen) ...)."
  (let* ((total (reduce #'+ weighted-generators :key #'car))
         (gen-list weighted-generators))
    (make-generator
     (lambda ()
       (let ((n (property-random total)))
         (loop for (weight . gen) in gen-list
               summing weight into running
               when (< n running)
               return (generate gen))))
     nil)))

(defun gen-such-that (predicate gen &key (max-tries 100))
  "Generate values from GEN that satisfy PREDICATE."
  (make-generator
   (lambda ()
     (loop repeat max-tries
           for val = (generate gen)
           when (funcall predicate val)
           return val
           finally (error "Could not generate value satisfying predicate after ~D tries"
                          max-tries)))
   (lambda (val)
     (remove-if-not predicate (shrink gen val)))))

(defun gen-map (fn gen)
  "Transform generated values using FN."
  (make-generator
   (lambda ()
     (funcall fn (generate gen)))
   nil))

(defun gen-bind (fn gen)
  "Monadic bind: generate from GEN, then use result to create new generator via FN."
  (make-generator
   (lambda ()
     (let* ((val (generate gen))
            (next-gen (funcall fn val)))
       (generate next-gen)))
   nil))

(defun gen-tuple (&rest generators)
  "Generate a list of values, one from each generator."
  (make-generator
   (lambda ()
     (mapcar #'generate generators))
   (lambda (tuple)
     ;; Shrink each element
     (let ((shrinks nil))
       (loop for i from 0 below (length tuple)
             for gen in generators
             for elem = (nth i tuple)
             do (dolist (shrunk (shrink gen elem))
                  (let ((new-tuple (copy-list tuple)))
                    (setf (nth i new-tuple) shrunk)
                    (push new-tuple shrinks))))
       shrinks))))

;; Special generators

(defun gen-constant (value)
  "Always generate the same VALUE."
  (make-generator
   (lambda () value)
   (lambda (v) (declare (ignore v)) nil)))

(defun gen-nil ()
  "Always generate NIL."
  (gen-constant nil))

;;; Property Test Execution

(defclass property-result ()
  ((success :initarg :success :reader property-success-p)
   (num-tests :initarg :num-tests :reader property-num-tests)
   (failing-input :initarg :failing-input :initform nil :reader property-failing-input)
   (shrunk-input :initarg :shrunk-input :initform nil :reader property-shrunk-input)
   (seed :initarg :seed :reader property-seed)
   (error-info :initarg :error :initform nil :reader property-error))
  (:documentation "Result of running a property test."))

(defun run-property (property generators &key (num-tests *num-tests*) seed)
  "Run PROPERTY with values from GENERATORS for NUM-TESTS iterations.
   PROPERTY is a function that takes generated values and returns T for success.
   GENERATORS is a list of generators matching the property's arguments.
   Returns a property-result."
  (let* ((actual-seed (or seed (get-universal-time)))
         (*seed* actual-seed))
    (init-random-state actual-seed)
    (loop for i from 1 to num-tests
          do (setf *current-iteration* i)
          do (let ((inputs (mapcar #'generate generators)))
               (handler-case
                   (unless (apply property inputs)
                     ;; Property failed - try to shrink
                     (let ((shrunk (shrink-inputs property generators inputs)))
                       (return (make-instance 'property-result
                                              :success nil
                                              :num-tests i
                                              :failing-input inputs
                                              :shrunk-input shrunk
                                              :seed actual-seed))))
                 (error (e)
                   (return (make-instance 'property-result
                                          :success nil
                                          :num-tests i
                                          :failing-input inputs
                                          :seed actual-seed
                                          :error e)))))
          finally (return (make-instance 'property-result
                                         :success t
                                         :num-tests num-tests
                                         :seed actual-seed)))))

(defun shrink-inputs (property generators inputs)
  "Try to find a smaller failing input by shrinking."
  (let ((*shrinking* t)
        (smallest inputs)
        (iterations 0))
    (loop while (< iterations *max-shrink-iterations*)
          do (let ((found-smaller nil))
               (loop for i from 0 below (length inputs)
                     for gen in generators
                     for shrinks = (shrink gen (nth i smallest))
                     do (dolist (shrunk shrinks)
                          (when (>= iterations *max-shrink-iterations*)
                            (return))
                          (incf iterations)
                          (let ((new-inputs (copy-list smallest)))
                            (setf (nth i new-inputs) shrunk)
                            (handler-case
                                (unless (apply property new-inputs)
                                  (setf smallest new-inputs)
                                  (setf found-smaller t))
                              (error ()
                                (setf smallest new-inputs)
                                (setf found-smaller t))))))
               (unless found-smaller
                 (return))))
    smallest))

;;; Reporting

(defun format-property-failure (result)
  "Format a property test failure for display."
  (with-output-to-string (s)
    (format s "Property test failed after ~D tests~%" (property-num-tests result))
    (format s "Seed: ~D (use to reproduce)~%" (property-seed result))
    (when (property-error result)
      (format s "Error: ~A~%" (property-error result)))
    (format s "Failing input: ~S~%" (property-failing-input result))
    (when (and (property-shrunk-input result)
               (not (equal (property-failing-input result)
                           (property-shrunk-input result))))
      (format s "Shrunk to: ~S~%" (property-shrunk-input result)))))
