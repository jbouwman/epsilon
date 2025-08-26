;;;; HTTP/2 Stream Priority Implementation
;;;;
;;;; Implements stream priority tree and scheduling per RFC 7540 Section 5.3

(defpackage :epsilon.http2.priority
  (:use :cl)
  (:export
   ;; Priority tree
   #:priority-tree
   #:make-priority-tree
   #:add-stream
   #:remove-stream
   #:update-priority
   #:get-next-stream
   
   ;; Stream priority info
   #:stream-priority
   #:make-stream-priority
   #:stream-priority-dependency
   #:stream-priority-weight
   #:stream-priority-exclusive-p
   
   ;; Tree operations
   #:reprioritize-stream
   #:get-stream-children
   #:get-stream-parent
   #:calculate-effective-weight))

(in-package :epsilon.http2.priority)

;;;; Stream Priority Structure

(defstruct stream-priority
  "Priority information for a stream"
  (stream-id 0 :type (unsigned-byte 31))
  (dependency 0 :type (unsigned-byte 31))
  (weight 16 :type (unsigned-byte 8))
  (exclusive-p nil :type boolean)
  (children nil :type list)
  (parent nil :type (or null stream-priority))
  (effective-weight 1.0 :type float)
  (bytes-sent 0 :type integer)
  (bytes-queued 0 :type integer))

;;;; Priority Tree

(defclass priority-tree ()
  ((root :initform nil :accessor tree-root)
   (nodes :initform (make-hash-table) :accessor tree-nodes)
   (ready-queue :initform nil :accessor tree-ready-queue)))

(defun make-priority-tree ()
  "Create a new priority tree"
  (let ((tree (make-instance 'priority-tree)))
    ;; Create virtual root stream (ID 0)
    (let ((root (make-stream-priority :stream-id 0 :weight 16)))
      (setf (tree-root tree) root)
      (setf (gethash 0 (tree-nodes tree)) root))
    tree))

;;;; Tree Operations

(defun add-stream (tree stream-id &key (dependency 0) (weight 16) exclusive-p)
  "Add a stream to the priority tree"
  (let* ((parent-node (or (gethash dependency (tree-nodes tree))
                         (tree-root tree)))
         (new-node (make-stream-priority
                   :stream-id stream-id
                   :dependency dependency
                   :weight weight
                   :exclusive-p exclusive-p
                   :parent parent-node)))
    
    ;; Handle exclusive dependency
    (if exclusive-p
        ;; New stream becomes sole child, existing children become its children
        (let ((existing-children (stream-priority-children parent-node)))
          (setf (stream-priority-children parent-node) (list new-node))
          (setf (stream-priority-children new-node) existing-children)
          ;; Update parent pointers
          (dolist (child existing-children)
            (setf (stream-priority-parent child) new-node)))
        ;; Normal dependency - add as child
        (push new-node (stream-priority-children parent-node)))
    
    ;; Store in hash table
    (setf (gethash stream-id (tree-nodes tree)) new-node)
    
    ;; Recalculate effective weights
    (calculate-effective-weights tree parent-node)
    
    new-node))

(defun remove-stream (tree stream-id)
  "Remove a stream from the priority tree"
  (let ((node (gethash stream-id (tree-nodes tree))))
    (when node
      (let ((parent (stream-priority-parent node))
            (children (stream-priority-children node)))
        
        ;; Move children to parent
        (when parent
          ;; Remove this node from parent's children
          (setf (stream-priority-children parent)
                (remove node (stream-priority-children parent)))
          
          ;; Add this node's children to parent
          (dolist (child children)
            (setf (stream-priority-parent child) parent)
            (push child (stream-priority-children parent))))
        
        ;; Remove from hash table
        (remhash stream-id (tree-nodes tree))
        
        ;; Recalculate weights
        (when parent
          (calculate-effective-weights tree parent)))))
  tree)

(defun update-priority (tree stream-id &key dependency weight exclusive-p)
  "Update stream priority"
  (let ((node (gethash stream-id (tree-nodes tree))))
    (when node
      ;; Check for circular dependency
      (when (and dependency (would-create-cycle-p tree stream-id dependency))
        (error "Priority update would create circular dependency"))
      
      ;; Remove from current position
      (let ((old-parent (stream-priority-parent node)))
        (when old-parent
          (setf (stream-priority-children old-parent)
                (remove node (stream-priority-children old-parent)))))
      
      ;; Update priority values
      (when dependency
        (setf (stream-priority-dependency node) dependency))
      (when weight
        (setf (stream-priority-weight node) weight))
      (when exclusive-p
        (setf (stream-priority-exclusive-p node) exclusive-p))
      
      ;; Re-add at new position
      (let ((new-parent (gethash (stream-priority-dependency node) 
                                 (tree-nodes tree))))
        (when new-parent
          (if (stream-priority-exclusive-p node)
              ;; Exclusive: become sole child
              (let ((siblings (stream-priority-children new-parent)))
                (setf (stream-priority-children new-parent) (list node))
                (setf (stream-priority-children node) 
                      (append (stream-priority-children node) siblings))
                (dolist (sibling siblings)
                  (setf (stream-priority-parent sibling) node)))
              ;; Non-exclusive: add as child
              (push node (stream-priority-children new-parent)))
          
          (setf (stream-priority-parent node) new-parent)
          
          ;; Recalculate weights
          (calculate-effective-weights tree new-parent)))))
  tree)

;;;; Weight Calculations

(defun calculate-effective-weights (tree node)
  "Calculate effective weights for scheduling"
  (let ((total-weight (loop for child in (stream-priority-children node)
                           sum (stream-priority-weight child))))
    (when (> total-weight 0)
      (dolist (child (stream-priority-children node))
        (setf (stream-priority-effective-weight child)
              (/ (float (stream-priority-weight child)) total-weight))
        ;; Recursively calculate for children
        (when (stream-priority-children child)
          (calculate-effective-weights tree child))))))

(defun calculate-effective-weight (tree stream-id)
  "Get effective weight of a stream for scheduling"
  (let ((node (gethash stream-id (tree-nodes tree))))
    (if node
        (stream-priority-effective-weight node)
        0.0)))

;;;; Scheduling

(defun get-next-stream (tree)
  "Get next stream to send data for (weighted fair queuing)"
  (let ((candidates (collect-ready-streams tree (tree-root tree))))
    (when candidates
      ;; Select stream with lowest (bytes-sent / effective-weight)
      (let ((best-stream nil)
            (best-ratio most-positive-double-float))
        (dolist (node candidates)
          (when (> (stream-priority-bytes-queued node) 0)
            (let ((ratio (if (zerop (stream-priority-effective-weight node))
                            most-positive-double-float
                            (/ (float (stream-priority-bytes-sent node))
                               (stream-priority-effective-weight node)))))
              (when (< ratio best-ratio)
                (setf best-ratio ratio
                      best-stream node)))))
        (when best-stream
          (stream-priority-stream-id best-stream))))))

(defun collect-ready-streams (tree node)
  "Collect all streams with data ready to send"
  (let ((ready nil))
    ;; Check if this node has data
    (when (and (> (stream-priority-stream-id node) 0)
               (> (stream-priority-bytes-queued node) 0))
      (push node ready))
    
    ;; Recursively check children
    (dolist (child (stream-priority-children node))
      (setf ready (append ready (collect-ready-streams tree child))))
    
    ready))

;;;; Utility Functions

(defun would-create-cycle-p (tree stream-id new-dependency)
  "Check if making stream-id depend on new-dependency would create a cycle"
  (let ((current new-dependency))
    (loop while current
          do (when (= current stream-id)
               (return-from would-create-cycle-p t))
             (let ((node (gethash current (tree-nodes tree))))
               (setf current (when node 
                              (stream-priority-dependency node)))))
    nil))

(defun get-stream-children (tree stream-id)
  "Get children of a stream"
  (let ((node (gethash stream-id (tree-nodes tree))))
    (when node
      (mapcar #'stream-priority-stream-id 
              (stream-priority-children node)))))

(defun get-stream-parent (tree stream-id)
  "Get parent of a stream"
  (let ((node (gethash stream-id (tree-nodes tree))))
    (when (and node (stream-priority-parent node))
      (stream-priority-stream-id (stream-priority-parent node)))))

(defun reprioritize-stream (tree stream-id new-dependency new-weight &key exclusive-p)
  "Reprioritize a stream in the tree"
  (update-priority tree stream-id 
                  :dependency new-dependency
                  :weight new-weight
                  :exclusive-p exclusive-p))

;;;; Debug/Inspection

(defun print-priority-tree (tree &optional (stream *standard-output*))
  "Print priority tree structure for debugging"
  (format stream "Priority Tree:~%")
  (print-node (tree-root tree) stream 0))

(defun print-node (node stream indent)
  "Print a node and its children"
  (dotimes (i indent)
    (write-char #\Space stream))
  (format stream "Stream ~D (weight: ~D, effective: ~,2F)~%"
          (stream-priority-stream-id node)
          (stream-priority-weight node)
          (stream-priority-effective-weight node))
  (dolist (child (stream-priority-children node))
    (print-node child stream (+ indent 2))))