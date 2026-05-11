;;;; HTTP/2 Priority Tests
;;;;
;;;; Unit tests for HTTP/2 stream priority tree and scheduling
;;;; per RFC 7540 Section 5.3.

(defpackage :epsilon.http.h2.priority-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.priority prio)))

;;;; Priority Tree Creation Tests

(deftest test-make-priority-tree ()
  "Test creating a new priority tree with virtual root"
  (let ((tree (prio:make-priority-tree)))
    (assert-true (not (null tree)))
    ;; Virtual root should exist at stream ID 0
    (assert-true (not (null (gethash 0 (prio:tree-nodes tree)))))
    ;; Root should have ID 0
    (let ((root (gethash 0 (prio:tree-nodes tree))))
      (assert-true (= (prio::stream-priority-stream-id root) 0)))))

;;;; Add Stream Tests

(deftest test-add-stream-default ()
  "Test adding a stream with default dependency and weight"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    ;; Stream should exist in tree
    (assert-true (not (null (gethash 1 (prio:tree-nodes tree)))))
    ;; Should be child of root (dependency 0)
    (let ((node (gethash 1 (prio:tree-nodes tree))))
      (assert-true (= (prio:stream-priority-dependency node) 0))
      (assert-true (= (prio:stream-priority-weight node) 16)))))

(deftest test-add-stream-custom-values ()
  "Test adding a stream with custom dependency and weight"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1 :weight 32)
    (let ((node (gethash 3 (prio:tree-nodes tree))))
      (assert-true (= (prio:stream-priority-dependency node) 1))
      (assert-true (= (prio:stream-priority-weight node) 32)))))

(deftest test-add-stream-appears-in-parent-children ()
  "Test that added stream appears in parent's children list"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (let ((children (prio:get-stream-children tree 0)))
      (assert-true (member 1 children)))))

(deftest test-add-stream-exclusive ()
  "Test adding stream with exclusive flag reparents existing children"
  (let ((tree (prio:make-priority-tree)))
    ;; Add two children to root
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    ;; Add stream 5 as exclusive child of root - should reparent 1 and 3
    (prio:add-stream tree 5 :exclusive-p t)
    ;; Stream 5 should be the only child of root
    (let ((root-children (prio:get-stream-children tree 0)))
      (assert-true (= (length root-children) 1))
      (assert-true (member 5 root-children)))
    ;; Streams 1 and 3 should now be children of stream 5
    (let ((children-of-5 (prio:get-stream-children tree 5)))
      (assert-true (member 1 children-of-5))
      (assert-true (member 3 children-of-5)))))

;;;; Remove Stream Tests

(deftest test-remove-stream ()
  "Test removing a stream reparents children to grandparent"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1)
    (prio:add-stream tree 5 :dependency 1)
    ;; Remove stream 1 - children 3 and 5 should move to root
    (prio:remove-stream tree 1)
    ;; Stream 1 should no longer exist
    (assert-true (null (gethash 1 (prio:tree-nodes tree))))
    ;; Streams 3 and 5 should be children of root
    (let ((root-children (prio:get-stream-children tree 0)))
      (assert-true (member 3 root-children))
      (assert-true (member 5 root-children)))))

(deftest test-remove-stream-leaf ()
  "Test removing a leaf stream"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:remove-stream tree 1)
    (assert-true (null (gethash 1 (prio:tree-nodes tree))))
    ;; Root should have no children
    (assert-true (null (prio:get-stream-children tree 0)))))

(deftest test-remove-nonexistent-stream ()
  "Test removing a stream that does not exist"
  (let ((tree (prio:make-priority-tree)))
    ;; Should not error
    (prio:remove-stream tree 999)
    (assert-true t)))

;;;; Update Priority Tests

(deftest test-update-priority-change-dependency ()
  "Test updating stream dependency"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    ;; Move stream 3 under stream 1
    (prio:update-priority tree 3 :dependency 1)
    (assert-true (= (prio:get-stream-parent tree 3) 1))
    (assert-true (member 3 (prio:get-stream-children tree 1)))))

(deftest test-update-priority-change-weight ()
  "Test updating stream weight"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 16)
    (prio:update-priority tree 1 :weight 64)
    (let ((node (gethash 1 (prio:tree-nodes tree))))
      (assert-true (= (prio:stream-priority-weight node) 64)))))

(deftest test-update-priority-exclusive ()
  "Test updating priority with exclusive reparenting"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    (prio:add-stream tree 5)
    ;; Make stream 5 exclusive child of root - reparents 1 and 3
    (prio:update-priority tree 5 :dependency 0 :exclusive-p t)
    (let ((root-children (prio:get-stream-children tree 0)))
      (assert-true (= (length root-children) 1))
      (assert-true (member 5 root-children)))
    (let ((children-of-5 (prio:get-stream-children tree 5)))
      (assert-true (member 1 children-of-5))
      (assert-true (member 3 children-of-5)))))

(deftest test-update-priority-circular-dependency ()
  "Test that circular dependency is detected"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1)
    ;; Making stream 1 depend on stream 3 would create a cycle
    (assert-true (handler-case
            (progn (prio:update-priority tree 1 :dependency 3) nil)
          (error () t)))))

;;;; Effective Weight Tests

(deftest test-calculate-effective-weight-single-child ()
  "Test effective weight for single child is 1.0"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 16)
    ;; Single child of root gets weight 1.0
    (assert-true (= (prio:calculate-effective-weight tree 1) 1.0))))

(deftest test-calculate-effective-weight-proportional ()
  "Test effective weight is proportional to sibling weights"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 64)
    (prio:add-stream tree 3 :weight 64)
    ;; Each should get 0.5
    (let ((w1 (prio:calculate-effective-weight tree 1))
          (w3 (prio:calculate-effective-weight tree 3)))
      (assert-true (< (abs (- w1 0.5)) 0.01))
      (assert-true (< (abs (- w3 0.5)) 0.01)))))

(deftest test-calculate-effective-weight-unequal ()
  "Test effective weight with unequal weights"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 192)
    (prio:add-stream tree 3 :weight 64)
    ;; Stream 1 should get 0.75, stream 3 should get 0.25
    (let ((w1 (prio:calculate-effective-weight tree 1))
          (w3 (prio:calculate-effective-weight tree 3)))
      (assert-true (< (abs (- w1 0.75)) 0.01))
      (assert-true (< (abs (- w3 0.25)) 0.01)))))

(deftest test-calculate-effective-weight-nonexistent ()
  "Test effective weight for nonexistent stream is 0.0"
  (let ((tree (prio:make-priority-tree)))
    (assert-true (= (prio:calculate-effective-weight tree 999) 0.0))))

;;;; Scheduling Tests

(deftest test-get-next-stream-empty ()
  "Test get-next-stream returns nil when no streams have data"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    (assert-true (null (prio:get-next-stream tree)))))

(deftest test-get-next-stream-single-ready ()
  "Test get-next-stream returns the stream with queued data"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    ;; Queue data on stream 3
    (let ((node (gethash 3 (prio:tree-nodes tree))))
      (setf (prio:stream-priority-bytes-queued node) 100))
    (assert-true (= (prio:get-next-stream tree) 3))))

(deftest test-get-next-stream-weighted-selection ()
  "Test get-next-stream selects based on weighted fair queuing"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 64)
    (prio:add-stream tree 3 :weight 64)
    ;; Both have data queued
    (let ((node1 (gethash 1 (prio:tree-nodes tree)))
          (node3 (gethash 3 (prio:tree-nodes tree))))
      (setf (prio:stream-priority-bytes-queued node1) 100)
      (setf (prio:stream-priority-bytes-queued node3) 100)
      ;; Stream 1 has sent more data, so stream 3 should be selected
      (setf (prio::stream-priority-bytes-sent node1) 1000)
      (setf (prio::stream-priority-bytes-sent node3) 0))
    (assert-true (= (prio:get-next-stream tree) 3))))

;;;; Tree Navigation Tests

(deftest test-get-stream-children ()
  "Test get-stream-children returns child stream IDs"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    (let ((children (prio:get-stream-children tree 0)))
      (assert-true (member 1 children))
      (assert-true (member 3 children)))))

(deftest test-get-stream-parent ()
  "Test get-stream-parent returns parent stream ID"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1)
    (assert-true (= (prio:get-stream-parent tree 3) 1))
    (assert-true (= (prio:get-stream-parent tree 1) 0))))

(deftest test-get-stream-parent-root ()
  "Test get-stream-parent returns nil for root"
  (let ((tree (prio:make-priority-tree)))
    (assert-true (null (prio:get-stream-parent tree 0)))))

(deftest test-get-stream-parent-nonexistent ()
  "Test get-stream-parent returns nil for nonexistent stream"
  (let ((tree (prio:make-priority-tree)))
    (assert-true (null (prio:get-stream-parent tree 999)))))

;;;; Cycle Detection Tests

(deftest test-would-create-cycle-direct ()
  "Test would-create-cycle-p detects direct cycle"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1)
    ;; Making 1 depend on 3 creates: 1->3->1
    (assert-true (prio::would-create-cycle-p tree 1 3))))

(deftest test-would-create-cycle-indirect ()
  "Test would-create-cycle-p detects indirect cycle"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3 :dependency 1)
    (prio:add-stream tree 5 :dependency 3)
    ;; Making 1 depend on 5 creates: 1->5->3->1
    (assert-true (prio::would-create-cycle-p tree 1 5))))

(deftest test-would-create-cycle-no-cycle ()
  "Test would-create-cycle-p returns nil for valid dependency"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1)
    (prio:add-stream tree 3)
    ;; Making 3 depend on 1 is fine
    (assert-true (not (prio::would-create-cycle-p tree 3 1)))))

;;;; Reprioritize Stream Tests

(deftest test-reprioritize-stream ()
  "Test reprioritize-stream delegates to update-priority"
  (let ((tree (prio:make-priority-tree)))
    (prio:add-stream tree 1 :weight 16)
    (prio:add-stream tree 3 :weight 16)
    (prio:reprioritize-stream tree 3 1 32)
    ;; Stream 3 should be child of stream 1 with weight 32
    (assert-true (= (prio:get-stream-parent tree 3) 1))
    (let ((node (gethash 3 (prio:tree-nodes tree))))
      (assert-true (= (prio:stream-priority-weight node) 32)))))
