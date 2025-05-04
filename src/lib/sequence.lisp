(defpackage #:epsilon.lib.sequence
  (:use
   #:cl
   #:epsilon.lib.binding)
  (:shadow
   #:cons
   #:first
   #:rest
   #:reduce
   #:seq
   #:map
   #:filter)
  (:export
   #:*empty*
   #:empty-p
   #:sequence-p
   #:filter
   #:map
   #:cons
   #:first
   #:reduce
   #:rest
   #:realize
   #:seq
   #:drop
   #:take))

(in-package #:epsilon.lib.sequence)

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

(defun seq (list)
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
