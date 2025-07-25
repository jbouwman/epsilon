;;;; This module provides immutable, persistent hash maps using the Hash Array
;;;; Mapped Trie data structure. HAMT offers O(log32 n) operations for get, assoc,
;;;; and dissoc with structural sharing for memory efficiency.

(defpackage :epsilon.map
  (:use
   :cl
   :epsilon.syntax)
  (:shadow
   :assoc
   :count
   :dissoc
   :filter
   :get
   :map
   :merge
   :reduce
   :seq)
  (:export
   :+empty+
   :assoc
   :assoc!
   :assoc-in
   :contains-p
   :count
   :difference
   :dissoc
   :dissoc!
   :each
   :enable-syntax
   :filter
   :from-pairs
   :get
   :get-in
   :invert
   :keys
   :make-map
   :map
   :map=
   :map-p
   :merge
   :reduce
   :select-keys
   :seq
   :size
   :subset-p
   :update
   :update!
   :update-in
   :vals))

(in-package :epsilon.map)

;; HAMT constants

(defconstant +bit-partition+ 5)  ; 32 children per node
(defconstant +partition-size+ (ash 1 +bit-partition+))
(defconstant +partition-mask+ (1- +partition-size+))

;; Internal node protocol

(defgeneric node-get (node hash shift key default)
  (:documentation "Get value from node structure"))

(defgeneric node-assoc (node hash shift key value)
  (:documentation "Associate key-value pair in node structure.
Returns (values new-node inserted) where inserted is true for new insertions."))

(defgeneric node-dissoc (node hash shift key parent)
  (:documentation "Remove key from node structure"))

(defgeneric node-hash (node)
  (:documentation "Get the hash value for a node"))

;; Node types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (hamt
              (:constructor make-hamt (root count)))
    (count 0 :type fixnum)
    root))

(defun size (hamt)
  (hamt-count hamt))

(defun count (hamt)
  "Get the number of key-value pairs in the map"
  (hamt-count hamt))

(defmethod make-load-form ((self hamt) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(define-constant +empty+ (make-hamt nil 0))

(defstruct bitmap-node
  bitmap
  array)

(defstruct leaf-node
  (hash 0 :type fixnum)
  key
  value)

(defstruct (collision-node 
            (:constructor make-collision-node (hash entries)))
  (hash 0 :type fixnum)
  entries)

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

(defun get (map key &optional default)
  "Get value for key from map, returning default if not found"
  (let ((root (hamt-root map)))
    (if root
        (node-get root (sxhash key) 0 key default)
        default)))

(defun contains-p (map key)
  "Return true if map contains key"
  (let ((not-found (gensym)))
    (not (eq (get map key not-found)
             not-found))))

;; FIXME rename to to-alist

(defun seq (map)
  "Return a sequence of key-value pairs from the map"
  (let ((result nil))
    (labels ((collect (node)
               (etypecase node
                 (null nil)
                 (leaf-node 
                  (push (cons (leaf-node-key node)
                              (leaf-node-value node))
                        result))
                 (bitmap-node
                  (loop for child across (bitmap-node-array node)
                        do (collect child)))
                 (collision-node
                  (setf result (append (collision-node-entries node) result))))))
      (collect (hamt-root map))
      (nreverse result))))

(defun map= (map1 map2)
  "Return true if maps are equal"
  (and (= (hamt-count map1) (hamt-count map2))
       (every (lambda (pair)
                (and (contains-p map2 (car pair))
                     (equalp (get map2 (car pair))
                             (cdr pair))))
              (seq map1))))

(defun print-map-pairs (map stream &key (format-fn #'write))
  (write-char #\{ stream)
  (let ((first t))
    (dolist (pair (seq map))
      (if first
          (setf first nil)
          (write-char #\Space stream))
      (funcall format-fn (car pair) stream)
      (write-char #\Space stream)
      (funcall format-fn (cdr pair) stream)))
  (write-char #\} stream))

(defmethod print-object ((map hamt) stream)
  (if *print-readably*
      (print-map-pairs map stream)
      (print-unreadable-object (map stream :type t)
        (print-map-pairs map stream :format-fn 
                         (lambda (obj s) (format s "~S" obj))))))

;;; leaf-node operations

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
                    (collision-node-entries node1)
                    (list (cons (leaf-node-key node1)
                                (leaf-node-value node1))))
                (if (typep node2 'collision-node)
                    (collision-node-entries node2)
                    (list (cons (leaf-node-key node2)
                                (leaf-node-value node2)))))))

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

(defmethod node-get ((node leaf-node) hash shift key default)
  (if (and (= (leaf-node-hash node) hash)
           (equalp (leaf-node-key node) key))
      (leaf-node-value node)
      default))

(defmethod node-assoc ((node leaf-node) hash shift key value)
  (cond
    ;; Same key - replace value
    ((and (= (leaf-node-hash node) hash)
          (equalp (leaf-node-key node) key))
     (values
      (make-leaf-node :hash hash :key key :value value)
      nil))                      ; update, not insert
    ;; Hash collision at maximum depth - create collision node
    ((>= shift 32)
     (if (= hash (leaf-node-hash node))
         (values
          (make-collision-node hash
                               (list (cons key value)
                                     (cons (leaf-node-key node)
                                           (leaf-node-value node))))
          t)
         ;; Different hashes at max depth - preserve original
         (values node nil)))
    ;; Different slots - create bitmap node with both entries
    (t
     (let ((new-node (make-bitmap-node :bitmap 0 :array #())))
       (values
        (merge-nodes new-node node
                     (make-leaf-node :hash hash :key key :value value)
                     shift)
        t)))))

(defmethod node-dissoc ((node leaf-node) hash shift key parent)
  (if (and (= (leaf-node-hash node) hash)
           (equalp (leaf-node-key node) key))
      nil  ; Remove this leaf
      node))  ; Key not found, return unchanged

;;; node protocol - collision-node

(defmethod node-hash ((node collision-node))
  (collision-node-hash node))

(defmethod node-get ((node collision-node) hash shift key default)
  (if (= hash (collision-node-hash node))
      (let ((entry (cl:assoc key (collision-node-entries node) :test #'equalp)))
        (if entry
            (cdr entry)
            default))
      default))

(defmethod node-assoc ((node collision-node) hash shift key value)
  (if (= hash (collision-node-hash node))
      ;; Same hash - update or add to collision entries
      (let ((entries (collision-node-entries node)))
        (if (cl:assoc key entries :test #'equalp)
            (values
             (make-collision-node
              hash
              (if (cl:assoc key entries :test #'equalp)
                  ;; Update existing entry
                  (mapcar (lambda (entry)
                            (if (equalp (car entry) key)
                                (cons key value)
                                entry))
                          entries)))
             nil)
            (values
             (make-collision-node 
              hash
              (cons (cons key value) entries))
             t)))  ; new insertion
      (values node nil)))

(defmethod node-dissoc ((node collision-node) hash shift key parent)
  (if (= hash (collision-node-hash node))
      (let ((new-entries (remove key (collision-node-entries node)
                                 :key #'car :test #'equalp)))
        (cond
          ;; No entries left
          ((null new-entries) nil)
          ;; Only one entry left
          ((null (cdr new-entries))
           (make-leaf-node :hash hash
                           :key (caar new-entries)
                           :value (cdar new-entries)))
          ;; Multiple entries
          (t
           (make-collision-node hash new-entries))))
      node))

;;; Bitmap node operations

(defun replace-node (node index new-child)
  "Create a new bitmap-node with the child at index replaced"
  (let ((new-array (copy-seq (bitmap-node-array node))))
    (setf (aref new-array index) new-child)
    (make-bitmap-node :bitmap (bitmap-node-bitmap node)
                      :array new-array)))

(defmethod node-assoc ((node bitmap-node) hash shift key value)
  (let* ((bit (ash 1 (get-index hash shift)))
         (index (bitmap-index (bitmap-node-bitmap node) (get-index hash shift))))
    (cond
      ;; Bit not set - insert new entry
      ((zerop (logand (bitmap-node-bitmap node) bit))
       (values
        (insert-node node bit index
                     (make-leaf-node :hash hash :key key :value value))
        t))
      ;; Bit set - recurse and update existing entry
      (t
       (let ((existing (aref (bitmap-node-array node) index)))
         (multiple-value-bind (new-node inserted)
             (node-assoc existing hash (+ shift +bit-partition+) key value)
           (values
            (replace-node node index new-node)
            inserted)))))))

(defmethod node-hash ((node bitmap-node))
  0)  ; Bitmap nodes don't have a hash

(defmethod node-get ((node bitmap-node) hash shift key default)
  (let ((index (get-index hash shift)))
    (if (bitmap-present-p (bitmap-node-bitmap node) index)
        (let* ((array-index (bitmap-index (bitmap-node-bitmap node) index))
               (child (aref (bitmap-node-array node) array-index)))
          (node-get child hash (+ shift +bit-partition+) key default))
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

(defmethod node-dissoc ((node bitmap-node) hash shift key parent)
  (let* ((index (get-index hash shift))
         (bit (ash 1 index)))
    (unless (bitmap-present-p (bitmap-node-bitmap node) index)
      ;; Key not found in this path
      (return-from node-dissoc node))
    (let* ((array-index (bitmap-index (bitmap-node-bitmap node) index))
           (child (aref (bitmap-node-array node) array-index))
           (new-child (node-dissoc child hash (+ shift +bit-partition+) key node)))
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

;;; Public functions

(defun assoc (map key value)
  "Return a new map with key-value pair added/updated"
  (let ((hash (sxhash key)))
    (multiple-value-bind (new-root inserted)
        (if (hamt-root map)
            (node-assoc (hamt-root map) hash 0 key value)
            (values (make-leaf-node :hash hash :key key :value value) t))
      (make-hamt new-root (+ (hamt-count map)
                             (if inserted 1 0))))))

(defun assoc-in (map keys value)
  "Associate a value in a nested map structure, where keys is a sequence of keys
   and value is the new value to be associated and return a new nested structure.
   Maps are created for nonexistent intermediate levels."
  (if (null keys)
      value
      (let ((k (car keys)))
        (if (null (cdr keys))
            (assoc map k value)
            (assoc map k (assoc-in (get map k +empty+) (cdr keys) value))))))

(defun reduce (function map &optional (initial-value +empty+))
  "Reduce over key-value pairs in the map"
  (cl:reduce (lambda (acc pair)
               (funcall function acc (car pair) (cdr pair)))
             (seq map)
             :initial-value initial-value))

(defun map (fn map)
  "Apply FN to each value in MAP, returning a new map with the same keys but transformed values.
   FN should take two arguments: key and value."
  (reduce (lambda (result k v)
            (assoc result k (funcall fn v)))
          map
          +empty+))

(defun each (fn map)
  "Apply FN to each key-value pair in MAP for side effects. 
   FN should take two arguments: key and value. Returns nil."
  (reduce (lambda (acc k v)
            (declare (ignore acc))
            (funcall fn k v)
            nil)
          map
          nil))

(defun select-keys (map keys)
  "Return a new map containing only the specified keys"
  (cl:reduce (lambda (m k)
               (if (contains-p map k)
                   (assoc m k (get map k))
                   m))
             keys
             :initial-value +empty+))

(defun filter (pred map)
  "Return a new map containing only entries satisfying pred"
  (reduce (lambda (m k v)
            (if (funcall pred k v)
                (assoc m k v)
                m))
          map))

(defun make-map (&rest kvs)
  "Create a map from alternating keys and values"
  (when (oddp (length kvs))
    (error "Odd number of arguments to map"))
  (loop :with m := +empty+
        :for (k v) :on kvs :by #'cddr
        :do (setf m (assoc m k v))
        :finally (return m)))

(defun from-pairs (pairs)
  "Create a map from a list of cons pairs"
  (cl:reduce (lambda (m pair)
               (assoc m (car pair) (cdr pair)))
             pairs
             :initial-value +empty+))

(defun subset-p (map1 map2)
  "Return true if map1 is a subset of map2"
  (every (lambda (pair)
           (and (contains-p map2 (car pair))
                (equalp (get map2 (car pair))
                        (cdr pair))))
         (seq map1)))

(defun difference (map1 map2)
  "Return a map of key/value pairs in map1 but not in map2"
  (reduce (lambda (m k v)
            (if (contains-p map2 k)
                m
                (assoc m k v)))
          map1))

(defun dissoc (map key)
  "Return a new map with key removed"
  (unless (contains-p map key)
    ;; if key doesn't exist, return unchanged map
    (return-from dissoc map))
  (let* ((hash (sxhash key))
         (new-root (and (hamt-root map)
                        (node-dissoc (hamt-root map) hash 0 key nil))))
    (make-hamt new-root (1- (hamt-count map)))))

(defun keys (map)
  "Return a list of all keys in the map"
  (mapcar #'car (seq map)))

(defun vals (map)
  "Return a list of all values in the map"
  (mapcar #'cdr (seq map)))

(defun merge (map1 map2)
  "Merge two maps, with map2 values taking precedence"
  (reduce #'assoc map2 map1))

(defun invert (map)
  "Return a new map with keys and values swapped"
  (reduce (lambda (m a b)
            (assoc m b a))
          map))

(defun get-in (map keys &optional default)
  "Get value in nested map structure following key sequence"
  (if (null keys)
      map
      (let ((v (get map (car keys) nil)))
        (if (null (cdr keys))
            (or v default)
            (get-in v (cdr keys) default)))))

(defun update (map key f &rest args)
  "Update value in map at key by applying f"
  (assoc map key (apply f (get map key) args)))

(defun update-in (map keys f &rest args)
  "Update value in nested map structure at key sequence by applying f"
  (if (null keys)
      (apply f map args)
      (let* ((k (car keys))
             (v (get map k nil)))
        (assoc map k
               (if (null (cdr keys))
                   (apply f (or v nil) args)
                   (apply #'update-in v (cdr keys) f args))))))

(defun map-entry-p (x)
  "Returns true if x is a map entry (key-value pair)"
  (and (consp x)
       (not (consp (cdr x)))))

(defun map-p (x)
  "Returns true if x is a map"
  (typep x 'hamt))

(defun map-concat (&rest maps)
  "Returns a map that consists of the rest of the maps conj-ed onto
      the first. If a key occurs in more than one map, the mapping from
      the latter (left-to-right) will be the mapping in the result."
  (cl:reduce #'merge maps :initial-value +empty+))

(defun rename-keys (map kmap)
  "Returns the map with the keys in kmap renamed to the vals in kmap"
  (reduce (lambda (m k v)
            (let ((new-key (get kmap k k)))
              (if (eq k new-key)
                  m
                  (-> m
                      (dissoc k)
                      (assoc new-key v)))))
          map))

;;; Reader syntax

(defun read-map (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (when (oddp (length forms))
      (error "Map literal must have an even number of forms"))
    `(make-map ,@forms)))

(defun enable-map-syntax ()
  "Enable {} reader syntax for maps"
  (set-macro-character #\{ #'read-map)
  (set-macro-character #\} (get-macro-character #\))))

;; Destructive update macros

(defmacro assoc! (place key value)
  "Destructively associate KEY with VALUE in the map at PLACE"
  `(setf ,place (assoc ,place ,key ,value)))

(defmacro dissoc! (place key)
  "Destructively remove KEY from the map at PLACE"
  `(setf ,place (dissoc ,place ,key)))

(defmacro update! (place key function &rest args)
  "Destructively update the value at KEY in the map at PLACE using FUNCTION"
  `(setf ,place (update ,place ,key ,function ,@args)))
