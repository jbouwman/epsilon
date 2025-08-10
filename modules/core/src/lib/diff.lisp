(defpackage #:epsilon.diff
  (:use #:cl)
  (:export
   #:diff
   #:diff-result
   #:diff-result-p
   #:diff-result-path
   #:diff-result-expected
   #:diff-result-actual
   #:diff-result-type))

(in-package #:epsilon.diff)

(defstruct diff-result
  path
  expected
  actual
  type)

(defun diff (obj1 obj2)
  "Compare two equalp objects and return a diff-result for the first difference.
Returns NIL if objects are equalp, otherwise returns a diff-result structure."
  (labels ((compare (a b path)
             (cond
               ((equalp a b) nil)
               ((and (consp a) (consp b))
                (or (compare (car a) (car b) (cons 'car path))
                    (compare (cdr a) (cdr b) (cons 'cdr path))))
               ((and (stringp a) (stringp b))
                (unless (string= a b)
                  (make-diff-result :path (reverse path)
                                    :expected a
                                    :actual b
                                    :type :value-mispmatch)))
               ((and (arrayp a) (arrayp b))
                (cond
                  ((not (equal (array-dimensions a) (array-dimensions b)))
                   (make-diff-result :path (reverse path)
                                     :expected a
                                     :actual b
                                     :type :dimension-mismatch))
                  ((= (array-rank a) 1)
                   (loop for i from 0 below (length a)
                         for diff = (compare (aref a i) (aref b i) 
                                           (cons i path))
                         when diff return diff))
                  (t
                   (loop for i from 0 below (array-total-size a)
                         for indices = (multiple-value-list 
                                       (floor i (reduce #'* (cdr (array-dimensions a)))))
                         for diff = (compare (apply #'aref a indices)
                                           (apply #'aref b indices)
                                           (cons indices path))
                         when diff return diff))))
               ((and (consp a) (not (consp b)))
                (make-diff-result :path (reverse path)
                                  :expected a
                                  :actual b
                                  :type :type-mismatch))
               ((and (not (consp a)) (consp b))
                (make-diff-result :path (reverse path)
                                  :expected a
                                  :actual b
                                  :type :type-mismatch))
               ((and (arrayp a) (not (arrayp b)))
                (make-diff-result :path (reverse path)
                                  :expected a
                                  :actual b
                                  :type :type-mismatch))
               ((and (not (arrayp a)) (arrayp b))
                (make-diff-result :path (reverse path)
                                  :expected a
                                  :actual b
                                  :type :type-mismatch))
               (t (make-diff-result :path (reverse path)
                                    :expected a
                                    :actual b
                                    :type :value-mismatch)))))
    (compare obj1 obj2 nil)))
