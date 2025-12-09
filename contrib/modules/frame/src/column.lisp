(defpackage #:epsilon.frame.column
  (:use #:cl)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype))
  (:export
   #:column
   #:column-p
   #:make-column
   #:column-dtype
   #:column-data
   #:column-length
   #:column-get
   #:column-slice
   #:column-map
   #:column-filter
   #:column-to-list
   #:column-from-list
   #:column-equal))

(in-package #:epsilon.frame.column)

(defstruct (column (:constructor %make-column))
  "A typed column of data"
  (dtype nil :type symbol)
  (data nil :type (or null simple-array))
  (length 0 :type fixnum))

(defun make-column (dtype-name data)
  "Create a column with the specified dtype and data"
  (let* ((dtype (dtype:find-dtype dtype-name))
         (array-type (dtype:dtype-array-type dtype)))
    (etypecase data
      (list
       (let* ((len (length data))
              (arr (make-array len :element-type (dtype:dtype-type dtype))))
         (loop for i from 0
               for value in data
               do (setf (aref arr i) (dtype:coerce-to-dtype value dtype-name)))
         (%make-column :dtype dtype-name
                       :data arr
                       :length len)))
      (vector
       (let* ((len (length data))
              (arr (make-array len :element-type (dtype:dtype-type dtype))))
         (loop for i from 0 below len
               do (setf (aref arr i) (dtype:coerce-to-dtype (aref data i) dtype-name)))
         (%make-column :dtype dtype-name
                       :data arr
                       :length len)))
      (simple-array
       (%make-column :dtype dtype-name
                     :data data
                     :length (length data))))))

(defun column (dtype &rest values)
  "Convenience constructor for columns"
  (make-column dtype values))

(defun column-get (column index)
  "Get value at index"
  (when (or (< index 0) (>= index (column-length column)))
    (error "Index ~A out of bounds for column of length ~A" 
           index (column-length column)))
  (aref (column-data column) index))

(defun column-slice (column start &optional end)
  "Create a new column from a slice of this column"
  (let* ((len (column-length column))
         (end (or end len)))
    (when (or (< start 0) (> end len) (> start end))
      (error "Invalid slice bounds: [~A:~A] for column of length ~A" 
             start end len))
    (let* ((new-len (- end start))
           (dtype (dtype:find-dtype (column-dtype column)))
           (new-data (make-array new-len 
                                :element-type (dtype:dtype-type dtype))))
      (loop for i from 0 below new-len
            do (setf (aref new-data i) 
                     (aref (column-data column) (+ start i))))
      (%make-column :dtype (column-dtype column)
                    :data new-data
                    :length new-len))))

(defun column-map (function column)
  "Apply function to each element, returning a new column"
  (let* ((len (column-length column))
         (data (column-data column))
         (results (make-array len :initial-element nil)))
    ;; First pass: apply function and collect results
    (loop for i from 0 below len
          do (setf (aref results i) (funcall function (aref data i))))
    ;; Infer dtype from results
    (let ((new-dtype (if (> len 0)
                         (dtype:infer-dtype (aref results 0))
                         :any)))
      (make-column new-dtype results))))

(defun column-filter (predicate column)
  "Filter column by predicate, returning a new column"
  (let* ((data (column-data column))
         (len (column-length column))
         (matches (loop for i from 0 below len
                       when (funcall predicate (aref data i))
                       collect (aref data i))))
    (make-column (column-dtype column) matches)))

(defun column-to-list (column)
  "Convert column to a list"
  (coerce (column-data column) 'list))

(defun column-from-list (dtype-name list)
  "Create a column from a list"
  (make-column dtype-name list))

(defun column-equal (col1 col2)
  "Test if two columns are equal"
  (and (eq (column-dtype col1) (column-dtype col2))
       (= (column-length col1) (column-length col2))
       (equalp (column-data col1) (column-data col2))))

(defmethod print-object ((col column) stream)
  "Print representation of a column"
  (print-unreadable-object (col stream :type t)
    (format stream "~A[~A]" 
            (column-dtype col)
            (column-length col))
    (when (> (column-length col) 0)
      (format stream " ~S..." 
              (column-get col 0)))))