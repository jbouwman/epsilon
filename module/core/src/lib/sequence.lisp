(defpackage :epsilon.lib.sequence
  (:use
   cl
   epsilon.lib.syntax)
  (:shadow
   cons
   first
   count
   rest
   reduce
   seq
   map
   filter)
  (:local-nicknames
   (map epsilon.lib.map))
  (:export
   *empty*
   empty-p
   sequence-p
   filter
   from-list
   map
   cons
   first
   count
   reduce
   rest
   realize
   seq
   drop
   iterate
   group-by
   partition-when
   take
   each))

(in-package epsilon.lib.sequence)

;; A represention of a delayed computation

(defstruct promise
  (value nil)
  (computed nil)
  (compute-fn nil))

(defun delay (function)
  (make-promise :compute-fn function :computed nil))

(defun force (promise)
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
  `(make-cons :head ,head
              :tail-promise (delay (lambda () ,tail-expr))))

(defvar *empty*
  (gensym "empty-sequence"))

(defun empty-p (sequence)
  (eq sequence *empty*))

(defun sequence-p (obj)
  "Returns true if OBJ is a lazy sequence"
  (or (cons-p obj)
      (eq obj *empty*)))

;; TODO -- update all useages then remove

(defun seq (list)
  (labels ((convert (remaining)
             (if (null remaining)
                 *empty*
                 (cons (car remaining)
                       (convert (cdr remaining))))))
    (if (listp list)
        (convert list)
        (convert (coerce list 'list)))))

(defun from-list (list)
  (labels ((convert (remaining)
             (if (null remaining)
                 *empty*
                 (cons (car remaining)
                       (convert (cdr remaining))))))
    (if (listp list)
        (convert list)
        (convert (coerce list 'list)))))

(defun first (seq)
  (if (empty-p seq)
      nil
      (cons-head seq)))

(defun rest (seq)
  (if (empty-p seq)
      *empty*
      (force (cons-tail-promise seq))))

(defun realize (seq)
  (loop for current = seq then (rest current)
        while (not (empty-p current))
        collect (first current)))

(defun count (seq)
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
      (let ((seqs (if (= 1 (length sequences))
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
          (if (= 1 (length sequences))
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

