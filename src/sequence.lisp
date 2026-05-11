;;;; This module provides lazy, immutable sequences with delayed evaluation.
;;;; Sequences are computed on-demand and cached.

(cl:defpackage :epsilon.sequence
  (:use
   cl
   epsilon.syntax)
  (:shadow
   cons
   first
   count
   rest
   reduce
   seq
   map
   filter
   length
   find)
  (:local-nicknames
   (map epsilon.map))
  (:export
   *empty*
   empty-p
   not-empty-p
   sequence-p
   filter
   map
   cons
   first
   count
   length
   element-at
   find
   reduce
   rest
   realize
   to-vector
   seq
   drop
   iterate
   group-by
   partition-when
   take
   each
   iterator
   lazy
   every-p))

(in-package :epsilon.sequence)

;; A represention of a delayed computation

(defstruct promise
  (value nil)
  (computed nil)
  (compute-fn nil))

(defun delay (function)
  "Wrap FUNCTION in a memoising promise; the function is invoked at
   most once on the first FORCE.  Internal building block for the
   lazy CONS macro below; callers normally don't construct promises
   directly."
  (make-promise :compute-fn function :computed nil))

(defun force (promise)
  "Compute PROMISE if it hasn't been forced yet (memoising the
   result), and return its value.  Idempotent."
  (if (promise-computed promise)
      (promise-value promise)
      (progn
        (setf (promise-value promise) (funcall (promise-compute-fn promise)))
        (setf (promise-computed promise) t)
        (promise-value promise))))

(defstruct cons
  (head nil)
  (tail-promise nil))

(defmacro cons (head tail-expr)
  "Build a lazy cons cell: HEAD is evaluated now, TAIL-EXPR is
   wrapped in a promise and only evaluated when REST is called.
   This is the CL CONS shadow that gives epsilon.sequence its
   one-element-at-a-time evaluation."
  `(make-cons :head ,head
              :tail-promise (delay (lambda () ,tail-expr))))

(defvar *empty*
  (gensym "empty-sequence")
  "Sentinel for the empty sequence.  EQ-comparable; never modify.")

(defun empty-p (sequence)
  "T iff SEQUENCE is the empty sequence (the *EMPTY* sentinel)."
  (eq sequence *empty*))

(defun not-empty-p (sequence)
  "T iff SEQUENCE has at least one element."
  (not (eq sequence *empty*)))

(defun sequence-p (obj)
  "Returns true if OBJ is a lazy sequence (a cons cell or the
   *EMPTY* sentinel)."
  (or (cons-p obj)
      (eq obj *empty*)))

(defun seq (list)
  "Coerce LIST (a list or any sequence type) into a lazy sequence.
   The realisation is eager in this direction -- conversion walks
   the input -- but the resulting structure can be consumed lazily."
  (labels ((convert (remaining)
             (if (null remaining)
                 *empty*
                 (cons (car remaining)
                       (convert (cdr remaining))))))
    (if (listp list)
        (convert list)
        (convert (coerce list 'list)))))

(defun first (seq)
  "Return the first element of SEQ, or NIL if SEQ is empty.
   Shadows CL:FIRST so callers should use the EPSILON.SEQUENCE
   nickname (typically `seq:first`)."
  (if (empty-p seq)
      nil
      (cons-head seq)))

(defun rest (seq)
  "Return the tail of SEQ as a lazy sequence; *EMPTY* if SEQ has
   no more elements.  This is the step that forces the next
   promise, advancing one element at a time."
  (if (empty-p seq)
      *empty*
      (force (cons-tail-promise seq))))

(defun realize (seq)
  "Walk SEQ to completion, returning a plain CL list of its
   elements.  For infinite sequences this never returns -- pair
   with TAKE first."
  (loop for current = seq then (rest current)
        while (not (empty-p current))
        collect (first current)))

(defun count (seq)
  "Return the number of elements in SEQ.  Forces the entire
   sequence; for infinite sequences this never returns."
  (loop with i = 0
        for current = seq then (rest current)
        while (not (empty-p current))
        do (incf i)
        finally (return i)))

(defmethod print-object ((seq cons) stream)
  "Print a lazy sequence in a readable format, respecting *print-length*"
  (if *print-readably*
      (call-next-method)
      (let* ((max-elements (or *print-length* 10))
             (elements (take max-elements seq))
             (rest-seq (drop max-elements seq)))
        (format stream "#<SEQ ~{~S~^ ~}~:[~; ...~]>"
                elements
                (not (empty-p rest-seq))))))

(defun take (n seq)
  "Return the first N elements of SEQ as a plain CL list (not a
   lazy sequence).  Stops early if SEQ has fewer than N elements;
   no error in that case."
  (loop for i from 1 to n
        for current = seq then (rest current)
        until (empty-p current)
        collect (first current)))

(defun drop (n seq)
  "Returns a lazy sequence of all but the first n items in seq"
  (let ((result seq))
    (loop repeat n
          while (not (empty-p result))
          do (setf result (rest result)))
    result))

(defun map (function &rest sequences)
  "Returns a lazy sequence consisting of applying FUNCTION to the elements
of the sequences. If more than one sequence is provided, FUNCTION should
accept as many arguments as there are sequences."
  (if (null sequences)
      *empty*
      (let ((seqs (if (= 1 (cl:length sequences))
                      (car sequences)
                      sequences)))
        (labels ((map-one (s)
                   (if (empty-p s)
                       *empty*
                       (cons (funcall function (first s))
                             (map-one (rest s)))))
                 (map-many (ss)
                   (if (some #'empty-p ss)
                       *empty*
                       (cons (apply function (mapcar #'first ss))
                             (map-many (mapcar #'rest ss))))))
          (if (= 1 (cl:length sequences))
              (map-one seqs)
              (map-many seqs))))))

(defun filter (predicate seq)
  "Returns a lazy sequence of items from seq for which predicate returns true"
  (labels ((next-match (s)
             (cond ((empty-p s)
                    *empty*)
                   ((funcall predicate (first s))
                    (cons (first s)
                          (next-match (rest s))))
                   (t
                    (next-match (rest s))))))
    (next-match seq)))

(defun reduce (function seq &key (initial-value nil initial-value-p))
  "Reduces a sequence using function. If initial-value is provided, it is used
as the first value, otherwise the first element of the sequence is used."
  (cond ((empty-p seq)
         (if initial-value-p
             initial-value
             (funcall function)))
        (initial-value-p
         (loop with acc = initial-value
               for current = seq then (rest current)
               while (not (empty-p current))
               do (setf acc (funcall function acc (first current)))
               finally (return acc)))
        (t
         (loop with acc = (first seq)
               for current = (rest seq) then (rest current)
               while (not (empty-p current))
               do (setf acc (funcall function acc (first current)))
               finally (return acc)))))

(defun each (function seq)
  "Applies function to each element of seq for side effects, consuming the entire sequence.
Returns no value."
  (loop for current = seq then (rest current)
        while (not (empty-p current))
        do (funcall function (first current)))
  (values))

(defun iterator (seq)
  "Create an iterator function for the sequence.
   Returns a function that when called returns the next element or nil when exhausted."
  (let ((current seq))
    (lambda ()
      (if (empty-p current)
          nil
          (prog1 (first current)
            (setf current (rest current)))))))

(defmacro lazy (expr)
  "Create a lazy sequence from an expression that returns a sequence"
  `(make-promise (lambda () ,expr)))

(defun partition-when (predicate sequence)
  "Returns a lazy sequence of subsequences, split when predicate returns true.

The element that matches the predicate starts a new partition."
  (labels ((partition-rec (remaining current-partition)
             (if (empty-p remaining)
                 (if (null current-partition)
                     *empty*
                     (cons (seq (nreverse current-partition)) *empty*))
                 (let ((head (first remaining))
                       (tail (rest remaining)))
                   (if (funcall predicate head)
                       (if (null current-partition)
                           ;; Start new partition with this element
                           (partition-rec tail (list head))
                           ;; Emit current partition and start new one
                           (cons (seq (nreverse current-partition))
                                 (partition-rec remaining '())))
                       ;; Add to current partition
                       (partition-rec tail (cl:cons head current-partition)))))))
    (partition-rec sequence '())))

(defun iterate (function initial-value)
  "Returns a lazy sequence of (initial-value, (f initial-value), (f (f initial-value)), ...)."
  (labels ((iterate-seq (current)
             (cons current
                   (iterate-seq (funcall function current)))))
    (iterate-seq initial-value)))

(defun group-by (key-fn sequence)
  "Group elements of sequence by the result of applying key-fn to each element.
Returns a map where keys are the group keys and values are lists of elements."
  (reduce (lambda (groups element)
            (let ((key (funcall key-fn element)))
              (map:assoc groups key
                         (cons element
                               (map:get groups key *empty*)))))
          sequence
          :initial-value map:+empty+))

(defun every-p (predicate seq)
  "Returns true if predicate returns true for all elements in seq.
Returns true for empty sequences."
  (loop for current = seq then (rest current)
        while (not (empty-p current))
        always (funcall predicate (first current))))

(defun length (seq)
  "Returns the number of elements in the sequence.
Alias for count with clearer naming."
  (count seq))

(defun element-at (n seq)
  "Returns the nth element (0-indexed) of the sequence.
Consumes up to n+1 elements. Returns nil if n is out of bounds."
  (loop for i from 0 to n
        for current = seq then (rest current)
        when (empty-p current) return nil
        finally (return (first current))))

(defun find (predicate seq)
  "Returns the first element in seq for which predicate returns true.
Returns nil if no matching element is found. Uses early exit."
  (loop for current = seq then (rest current)
        while (not (empty-p current))
        when (funcall predicate (first current))
          return (first current)))

(defun to-vector (seq)
  "Realizes the lazy sequence into a vector instead of a list."
  (coerce (realize seq) 'vector))
