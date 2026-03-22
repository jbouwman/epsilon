;;;; Mutable API matching epsilon.map's conventions, wrapping CL
;;;; hash-tables for mutable use cases.
;;;;
;;;; Usage:
;;;;   (let ((m (mmap:make-map)))
;;;;     (mmap:put! m :key "value")
;;;;     (mmap:get m :key))

(defpackage epsilon.mutable-map
  (:use cl)
  (:nicknames mmap)
  (:shadow get)
  (:export make-map
           get
           put!
           contains?
           size
           update!
           for-each))

(in-package epsilon.mutable-map)

;;; Construction

(defun make-map (&key (test 'equal))
  "Create a new mutable map with the given equality test.
   Default test is EQUAL (suitable for string keys)."
  (make-hash-table :test test))

;;; Access

(defun get (map key &optional default)
  "Get value for key, or default if not present."
  (gethash key map default))

(defun contains? (map key)
  "Return T if map contains key."
  (nth-value 1 (gethash key map)))

(defun size (map)
  "Return the number of entries in the map."
  (hash-table-count map))

;;; Mutation

(defun put! (map key value)
  "Set key to value in map. Returns the map for chaining."
  (setf (gethash key map) value)
  map)

(defun update! (map key fn &optional default)
  "Update value at key by applying fn to current value (or default).
   Returns the map for chaining.

   Example: (update! counts item #'1+ 0) ; increment counter"
  (setf (gethash key map) (funcall fn (gethash key map default)))
  map)

;;; Iteration

(defun for-each (map fn)
  "Call fn with each key and value in the map.
   Returns NIL."
  (maphash fn map)
  nil)
