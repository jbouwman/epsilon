;;;; Shared HAMT (Hash Array Mapped Trie) infrastructure
;;;;
;;;; Constants, utility functions, and bitmap-node operations shared
;;;; between epsilon.map and epsilon.set.

(defpackage :epsilon.hamt
  (:use :cl)
  (:export
   ;; Constants
   #:+bit-partition+
   #:+partition-size+
   #:+partition-mask+
   ;; Bitmap node structure
   #:bitmap-node
   #:make-bitmap-node
   #:bitmap-node-bitmap
   #:bitmap-node-array
   ;; Utility functions
   #:get-index
   #:bitmap-present-p
   #:bitmap-index
   ;; Bitmap node operations
   #:insert-node
   #:replace-node
   #:delete-bitmap-entry))

(in-package :epsilon.hamt)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconstant +bit-partition+ 5)
(defconstant +partition-size+ (ash 1 +bit-partition+))
(defconstant +partition-mask+ (1- +partition-size+))

;;; ---------------------------------------------------------------------------
;;; Bitmap node structure
;;; ---------------------------------------------------------------------------

(defstruct bitmap-node
  bitmap
  array)

(defmethod make-load-form ((self bitmap-node) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;; ---------------------------------------------------------------------------
;;; Utility functions
;;; ---------------------------------------------------------------------------

(declaim (inline get-index bitmap-present-p bitmap-index))

(defun get-index (hash shift)
  "Get index in the current level's array based on HASH and SHIFT."
  (logand (ash hash (- shift)) +partition-mask+))

(defun bitmap-present-p (bitmap index)
  "Check if bit at INDEX is set in BITMAP."
  (logtest bitmap (ash 1 index)))

(defun bitmap-index (bitmap index)
  "Convert bitmap INDEX to array index by counting set bits."
  (logcount (logand bitmap (1- (ash 1 index)))))

;;; ---------------------------------------------------------------------------
;;; Bitmap node operations
;;; ---------------------------------------------------------------------------

(defun insert-node (node bit index new-child)
  "Insert a new child node at the given position."
  (let* ((bitmap (bitmap-node-bitmap node))
         (old-array (bitmap-node-array node))
         (new-size (1+ (length old-array)))
         (new-array (make-array new-size)))
    (replace new-array old-array :end1 index :end2 index)
    (setf (aref new-array index) new-child)
    (replace new-array old-array
             :start1 (1+ index) :start2 index)
    (make-bitmap-node :bitmap (logior bitmap bit)
                      :array new-array)))

(defun replace-node (node index new-child)
  "Create a new bitmap-node with the child at index replaced."
  (let ((new-array (copy-seq (bitmap-node-array node))))
    (setf (aref new-array index) new-child)
    (make-bitmap-node :bitmap (bitmap-node-bitmap node)
                      :array new-array)))

(defun delete-bitmap-entry (node new-bitmap index)
  "Create a new bitmap-node without the entry at index."
  (let* ((old-array (bitmap-node-array node))
         (new-size (1- (length old-array)))
         (new-array (make-array new-size)))
    (replace new-array old-array :end1 index :end2 index)
    (replace new-array old-array
             :start1 index :start2 (1+ index)
             :end2 (length old-array))
    (make-bitmap-node :bitmap new-bitmap
                      :array new-array)))
