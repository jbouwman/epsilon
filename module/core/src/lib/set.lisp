(defpackage epsilon.lib.set
  (:use
   cl
   epsilon.lib.syntax)
  (:shadow
   count
   filter
   map
   reduce
   intersection
   remove
   union)
  (:export
   +empty+
   add
   contains-p
   count
   difference
   disj
   enable-syntax
   filter
   intersection
   make-set
   map
   reduce
   remove
   seq
   set=
   set-p
   size
   subset-p
   union))

(in-package epsilon.lib.set)

;; HAMT constants (same as map)

(defconstant +bit-partition+ 5)
(defconstant +partition-size+ (ash 1 +bit-partition+))
(defconstant +partition-mask+ (1- +partition-size+))

;; Internal node protocol

(defgeneric node-get (node hash shift value default)
  (:documentation "Get value from node structure"))

(defgeneric node-add (node hash shift value)
  (:documentation "Add value to node structure.
  Returns (values new-node inserted) where inserted is true for new insertions."))

(defgeneric node-remove (node hash shift value parent)
  (:documentation "Remove value from node structure"))

(defgeneric node-hash (node)
  (:documentation "Get the hash value for a node"))

;; Set structure

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (hamt-set
              (:constructor make-hamt-set (root count)))
    (count 0 :type fixnum)
    root))

(defun size (set)
  (hamt-set-count set))

(defun count (set)
  "Get the number of elements in the set"
  (hamt-set-count set))

(defmethod make-load-form ((self hamt-set) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(define-constant +empty+ (make-hamt-set nil 0))

;; Node types (same as map but adapted for sets)

(defstruct bitmap-node
  bitmap
  array)

(defstruct leaf-node
  (hash 0 :type fixnum)
  value)

(defstruct (collision-node
            (:constructor make-collision-node (hash values)))
  (hash 0 :type fixnum)
  values)

;; Utility functions (same as map)

(declaim (inline get-index bitmap-present-p bitmap-index))

(defun get-index (hash shift)
  "Get index in the current level's array based on hash and shift"
  (logand (ash hash (- shift)) +partition-mask+))

(defun bitmap-present-p (bitmap index)
  "Check if a bit is set in the bitmap"
  (logtest bitmap (ash 1 index)))

(defun bitmap-index (bitmap index)
  "Convert bitmap index to array index"
  (logcount (logand bitmap (1- (ash 1 index)))))

;; Public functions

(defun contains-p (set value)
  "Return true if set contains value"
  (let ((root (hamt-set-root set)))
    (if root
        (let ((not-found (gensym)))
          (not (eq (node-get root (sxhash value) 0 value not-found)
                   not-found)))
        nil)))

(defun seq (set)
  "Return a sequence of values from the set"
  (let ((result nil))
    (labels ((collect (node)
               (etypecase node
                 (null nil)
                 (leaf-node
                  (push (leaf-node-value node) result))
                 (bitmap-node
                  (loop for child across (bitmap-node-array node)
                        do (collect child)))
                 (collision-node
                  (setf result (append (collision-node-values node) result))))))
      (collect (hamt-set-root set))
      (nreverse result))))

(defun set= (set1 set2)
  "Return true if sets are equal"
  (and (= (hamt-set-count set1) (hamt-set-count set2))
    (every (lambda (value)
             (contains-p set2 value))
           (seq set1))))

(defun print-set-values (set stream &key (format-fn #'write))
  (write-string "#{" stream)
  (let ((first t))
    (dolist (value (seq set))
      (if first
          (setf first nil)
          (write-char #\Space stream))
      (funcall format-fn value stream)))
  (write-char #\} stream))

(defmethod print-object ((set hamt-set) stream)
  (if *print-readably*
      (print-set-values set stream)
      (print-unreadable-object (set stream :type t)
        (print-set-values set stream :format-fn
                          (lambda (obj s) (format s "~S" obj))))))

;; leaf-node operations

(defun merge-nodes (parent node1 node2 shift)
  "Merge two nodes at the given shift level"
  (let* ((h1 (node-hash node1))
         (h2 (node-hash node2))
         (index1 (get-index h1 shift))
         (index2 (get-index h2 shift)))
    (cond
      ;; Check for hash collision
      ((and (= h1 h2) (>= shift 32))
       (make-collision-node
        h1
        (append (if (typep node1 'collision-node)
                    (collision-node-values node1)
                    (list (leaf-node-value node1)))
                (if (typep node2 'collision-node)
                    (collision-node-values node2)
                    (list (leaf-node-value node2))))))
      
      ;; Same index: recurse deeper
      ((= index1 index2)
       (let ((new-child (merge-nodes (make-bitmap-node :bitmap 0 :array #())
                                     node1 node2 (+ shift +bit-partition+))))
         (insert-node parent (ash 1 index1) 0 new-child)))
      
      ;; Different indices: store both
      (t
       (let ((bitmap (logior (ash 1 index1) (ash 1 index2)))
             (array (make-array 2)))
         (if (< index1 index2)
             (setf (aref array 0) node1
                   (aref array 1) node2)
             (setf (aref array 0) node2
                   (aref array 1) node1))
         (make-bitmap-node :bitmap bitmap :array array))))))

(defun insert-node (node bit index new-child)
  "Insert a new child node at the given position"
  (let* ((bitmap (bitmap-node-bitmap node))
         (old-array (bitmap-node-array node))
         (new-size (1+ (length old-array)))
         (new-array (make-array new-size)))
    ;; Copy elements before insertion point
    (replace new-array old-array :end1 index :end2 index)
    ;; Insert new element
    (setf (aref new-array index) new-child)
    ;; Copy elements after insertion point
    (replace new-array old-array
             :start1 (1+ index) :start2 index)
    (make-bitmap-node :bitmap (logior bitmap bit)
                      :array new-array)))

(defmethod node-hash ((node leaf-node))
  (leaf-node-hash node))

(defmethod node-get ((node leaf-node) hash shift value default)
  (if (and (= (leaf-node-hash node) hash)
        (equalp (leaf-node-value node) value))
      (leaf-node-value node)
      default))

(defmethod node-add ((node leaf-node) hash shift value)
  (cond
    ;; Same value - no change needed
    ((and (= (leaf-node-hash node) hash)
       (equalp (leaf-node-value node) value))
     (values node nil))  ; no insertion
    ;; Hash collision at maximum depth - create collision node
    ((>= shift 32)
     (if (= hash (leaf-node-hash node))
         (values
          (make-collision-node hash
                               (list value (leaf-node-value node)))
          t)
         ;; Different hashes at max depth - preserve original
         (values node nil)))
    ;; Different slots - create bitmap node with both entries
    (t
     (let ((new-node (make-bitmap-node :bitmap 0 :array #())))
       (values
        (merge-nodes new-node node
                     (make-leaf-node :hash hash :value value)
                     shift)
        t)))))

(defmethod node-remove ((node leaf-node) hash shift value parent)
  (if (and (= (leaf-node-hash node) hash)
        (equalp (leaf-node-value node) value))
      nil  ; Remove this leaf
      node))  ; Value not found, return unchanged

;; collision-node operations

(defmethod node-hash ((node collision-node))
  (collision-node-hash node))

(defmethod node-get ((node collision-node) hash shift value default)
  (if (= hash (collision-node-hash node))
      (if (member value (collision-node-values node) :test #'equalp)
          value
          default)
      default))

(defmethod node-add ((node collision-node) hash shift value)
  (if (= hash (collision-node-hash node))
      ;; Same hash - check if value already exists
      (let ((values (collision-node-values node)))
        (if (member value values :test #'equalp)
            (values node nil)  ; value already exists
            (values
             (make-collision-node hash (cons value values))
             t)))  ; new insertion
      (values node nil)))

(defmethod node-remove ((node collision-node) hash shift value parent)
  (if (= hash (collision-node-hash node))
      (let ((new-values (cl:remove value (collision-node-values node) :test #'equalp)))
        (cond
          ;; No values left
          ((null new-values) nil)
          ;; Only one value left
          ((null (cdr new-values))
           (make-leaf-node :hash hash :value (car new-values)))
          ;; Multiple values
          (t
           (make-collision-node hash new-values))))
      node))

;; bitmap-node operations

(defun replace-node (node index new-child)
  "Create a new bitmap-node with the child at index replaced"
  (let ((new-array (copy-seq (bitmap-node-array node))))
    (setf (aref new-array index) new-child)
    (make-bitmap-node :bitmap (bitmap-node-bitmap node)
                      :array new-array)))

(defmethod node-add ((node bitmap-node) hash shift value)
  (let* ((bit (ash 1 (get-index hash shift)))
         (index (bitmap-index (bitmap-node-bitmap node) (get-index hash shift))))
    (cond
      ;; Bit not set - insert new entry
      ((zerop (logand (bitmap-node-bitmap node) bit))
       (values
        (insert-node node bit index
                     (make-leaf-node :hash hash :value value))
        t))
      ;; Bit set - recurse and update existing entry
      (t
       (let ((existing (aref (bitmap-node-array node) index)))
         (multiple-value-bind (new-node inserted)
             (node-add existing hash (+ shift +bit-partition+) value)
           (values
            (replace-node node index new-node)
            inserted)))))))

(defmethod node-hash ((node bitmap-node))
  0)  ; Bitmap nodes don't have a hash

(defmethod node-get ((node bitmap-node) hash shift value default)
  (let ((index (get-index hash shift)))
    (if (bitmap-present-p (bitmap-node-bitmap node) index)
        (let* ((array-index (bitmap-index (bitmap-node-bitmap node) index))
               (child (aref (bitmap-node-array node) array-index)))
          (node-get child hash (+ shift +bit-partition+) value default))
        default)))

(defun compact-node (node array-index)
  "Attempt to collapse a bitmap-node with only one child"
  (let ((remaining-child (aref (bitmap-node-array node) array-index)))
    (etypecase remaining-child
      (leaf-node remaining-child)
      (bitmap-node node))))  ; Don't collapse bitmap nodes

(defun delete-bitmap-entry (node new-bitmap index)
  "Create a new bitmap-node without the entry at index"
  (let* ((old-array (bitmap-node-array node))
         (new-size (1- (length old-array)))
         (new-array (make-array new-size)))
    ;; Copy elements before the removed index
    (replace new-array old-array :end1 index :end2 index)
    ;; Copy elements after the removed index
    (replace new-array old-array
             :start1 index :start2 (1+ index)
             :end2 (length old-array))
    (make-bitmap-node :bitmap new-bitmap
                      :array new-array)))

(defmethod node-remove ((node bitmap-node) hash shift value parent)
  (let* ((index (get-index hash shift))
         (bit (ash 1 index)))
    (unless (bitmap-present-p (bitmap-node-bitmap node) index)
      ;; Value not found in this path
      (return-from node-remove node))
    (let* ((array-index (bitmap-index (bitmap-node-bitmap node) index))
           (child (aref (bitmap-node-array node) array-index))
           (new-child (node-remove child hash (+ shift +bit-partition+) value node)))
      (cond
        ;; Child was not removed
        (new-child
         (if (eq new-child child)
             node  ; No change needed
             (replace-node node array-index new-child)))
        ;; Child was removed, need to update bitmap and possibly compact
        (t
         (let ((new-bitmap (logxor (bitmap-node-bitmap node) bit)))
           (cond
             ;; If this was the last entry, remove the entire node
             ((zerop new-bitmap) nil)
             ;; If we're down to one child, we might be able to collapse
             ((and (= (logcount new-bitmap) 1)
                parent)  ; Don't collapse the root
              (compact-node node array-index))
             ;; Otherwise create new node with updated bitmap and array
             (t (delete-bitmap-entry node new-bitmap array-index)))))))))

;; Public API

(defun add (set value)
  "Return a new set with value added"
  (let ((hash (sxhash value)))
    (multiple-value-bind (new-root inserted)
        (if (hamt-set-root set)
            (node-add (hamt-set-root set) hash 0 value)
            (values (make-leaf-node :hash hash :value value) t))
      (make-hamt-set new-root (+ (hamt-set-count set)
                                 (if inserted 1 0))))))

(defun disj (set value)
  "Return a new set with value removed"
  (unless (contains-p set value)
    ;; if value doesn't exist, return unchanged set
    (return-from disj set))
  (let* ((hash (sxhash value))
         (new-root (and (hamt-set-root set)
                     (node-remove (hamt-set-root set) hash 0 value nil))))
    (make-hamt-set new-root (1- (hamt-set-count set)))))

(defun remove (set value)
  "Alias for disj - return a new set with value removed"
  (disj set value))

(defun reduce (function set &optional (initial-value +empty+))
  "Reduce over values in the set"
  (cl:reduce function (seq set) :initial-value initial-value))

(defun map (set fn)
  "Apply FN to each value in SET, returning a new set with transformed values"
  (reduce (lambda (result value)
            (add result (funcall fn value)))
          set
          +empty+))

(defun filter (pred set)
  "Return a new set containing only values satisfying pred"
  (reduce (lambda (result value)
            (if (funcall pred value)
                (add result value)
                result))
          set
          +empty+))

(defun make-set (&rest values)
  "Create a set from values"
  (cl:reduce #'add values :initial-value +empty+))

(defun subset-p (set1 set2)
  "Return true if set1 is a subset of set2"
  (every (lambda (value)
           (contains-p set2 value))
         (seq set1)))

(defun union (set1 set2)
  "Return the union of two sets"
  (reduce #'add set2 set1))

(defun intersection (set1 set2)
  "Return the intersection of two sets"
  (let ((smaller (if (<= (size set1) (size set2)) set1 set2))
        (larger (if (<= (size set1) (size set2)) set2 set1)))
    (filter (lambda (value) (contains-p larger value)) smaller)))

(defun difference (set1 set2)
  "Return the difference of two sets (values in set1 but not in set2)"
  (filter (lambda (value) (not (contains-p set2 value))) set1))

(defun set-p (x)
  "Returns true if x is a set"
  (typep x 'hamt-set))
