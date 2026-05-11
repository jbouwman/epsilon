;;;; Tests for the Chase-Lev work-stealing deque.

(defpackage :epsilon.scheduler.deque-tests
  (:import
   (epsilon.sys.thread thread)
   (epsilon.sys.semaphore sem))
  (:use :cl :epsilon.test :epsilon.scheduler.deque))

;;; ============================================================================
;;; Basic Operations (Single Thread)
;;; ============================================================================

(deftest test-deque-creation
  "A fresh deque is empty."
  (let ((d (make-ws-deque)))
    (assert-true (ws-deque-empty-p d))
    (assert-= 0 (ws-deque-size d))))

(deftest test-deque-push-pop-single
  "Push one item, pop it back."
  (let ((d (make-ws-deque)))
    (ws-deque-push d :a)
    (assert-= 1 (ws-deque-size d))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-eq :a item)
      (assert-true found))
    (assert-true (ws-deque-empty-p d))))

(deftest test-deque-lifo-order
  "Pop returns items in LIFO order (stack semantics for owner)."
  (let ((d (make-ws-deque)))
    (ws-deque-push d 1)
    (ws-deque-push d 2)
    (ws-deque-push d 3)
    (assert-= 3 (ws-deque-size d))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-= 3 item)
      (assert-true found))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-= 2 item)
      (assert-true found))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-= 1 item)
      (assert-true found))))

(deftest test-deque-pop-empty
  "Popping from an empty deque returns NIL, NIL."
  (let ((d (make-ws-deque)))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-nil item)
      (assert-nil found))))

(deftest test-deque-pop-empty-after-drain
  "Popping after draining all items returns NIL, NIL."
  (let ((d (make-ws-deque)))
    (ws-deque-push d :x)
    (ws-deque-pop d)
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-nil item)
      (assert-nil found))))

(deftest test-deque-steal-single
  "Steal returns the oldest item (FIFO from top)."
  (let ((d (make-ws-deque)))
    (ws-deque-push d :first)
    (ws-deque-push d :second)
    (ws-deque-push d :third)
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-eq :first item)
      (assert-true found))
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-eq :second item)
      (assert-true found))))

(deftest test-deque-steal-empty
  "Stealing from an empty deque returns NIL, NIL."
  (let ((d (make-ws-deque)))
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-nil item)
      (assert-nil found))))

(deftest test-deque-push-many
  "Push and pop many items to exercise the buffer."
  (let ((d (make-ws-deque :initial-size 4))
        (n 100))
    (dotimes (i n)
      (ws-deque-push d i))
    (assert-= n (ws-deque-size d))
    ;; Pop all in LIFO order
    (loop for expected from (1- n) downto 0
          do (multiple-value-bind (item found) (ws-deque-pop d)
               (assert-= expected item)
               (assert-true found)))
    (assert-true (ws-deque-empty-p d))))

;;; ============================================================================
;;; Buffer Growth
;;; ============================================================================

(deftest test-deque-buffer-growth
  "Pushing beyond initial capacity triggers buffer growth."
  (let ((d (make-ws-deque :initial-size 2)))
    ;; Push more items than the initial capacity (2)
    (dotimes (i 20)
      (ws-deque-push d i))
    (assert-= 20 (ws-deque-size d))
    ;; All items should still be accessible
    (loop for expected from 19 downto 0
          do (multiple-value-bind (item found) (ws-deque-pop d)
               (assert-= expected item)
               (assert-true found)))))

(deftest test-deque-growth-preserves-steal-order
  "Buffer growth preserves FIFO steal order."
  (let ((d (make-ws-deque :initial-size 2)))
    (dotimes (i 20)
      (ws-deque-push d i))
    ;; Steal should return items in FIFO order
    (loop for expected from 0 below 20
          do (multiple-value-bind (item found) (ws-deque-steal d)
               (assert-= expected item)
               (assert-true found)))))

;;; ============================================================================
;;; Mixed Push/Pop/Steal (Single Thread)
;;; ============================================================================

(deftest test-deque-interleaved-ops
  "Interleave push, pop, and steal on a single thread."
  (let ((d (make-ws-deque)))
    ;; Push 1,2,3
    (ws-deque-push d 1)
    (ws-deque-push d 2)
    (ws-deque-push d 3)
    ;; Steal takes from top (oldest = 1)
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-= 1 item)
      (assert-true found))
    ;; Pop takes from bottom (newest = 3)
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-= 3 item)
      (assert-true found))
    ;; Only 2 remains
    (assert-= 1 (ws-deque-size d))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-= 2 item)
      (assert-true found))
    (assert-true (ws-deque-empty-p d))))

(deftest test-deque-single-element-pop
  "Pop succeeds when exactly one element exists (no competing stealer)."
  (let ((d (make-ws-deque)))
    (ws-deque-push d :solo)
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-eq :solo item)
      (assert-true found))
    (assert-true (ws-deque-empty-p d))))

(deftest test-deque-single-element-steal
  "Steal succeeds when exactly one element exists (no competing popper)."
  (let ((d (make-ws-deque)))
    (ws-deque-push d :solo)
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-eq :solo item)
      (assert-true found))
    (assert-true (ws-deque-empty-p d))))

;;; ============================================================================
;;; Concurrent Tests (Multi-Thread)
;;; ============================================================================

(deftest test-deque-concurrent-push-steal
  "Owner pushes items while multiple thieves steal concurrently.
All items pushed must be accounted for (stolen or remaining in deque)."
  (let* ((d (make-ws-deque :initial-size 8))
         (n 10000)
         (num-thieves 4)
         (stolen-items (make-array num-thieves :initial-element nil))
         (done (make-array 1 :element-type 'fixnum :initial-element 0))
         (thief-threads nil))
    ;; Start thief threads
    (dotimes (i num-thieves)
      (let ((idx i))
        (push (thread:make-thread
               (lambda ()
                 (let ((collected nil))
                   (loop
                     (multiple-value-bind (item status)
                         (ws-deque-steal d)
                       (cond
                         ((eq status t)
                          (push item collected))
                         ((eq status :abort)
                          ;; Lost race, retry immediately
                          nil)
                         (t
                          ;; Empty -- check if producer is done
                          (when (plusp (aref done 0))
                            ;; Drain remaining
                            (loop
                              (multiple-value-bind (item2 status2)
                                  (ws-deque-steal d)
                                (cond
                                  ((eq status2 t)
                                   (push item2 collected))
                                  ((eq status2 :abort) nil)
                                  (t (return)))))
                            (return))
                          (thread:thread-yield)))))
                   (setf (aref stolen-items idx) collected)))
               :name (format nil "thief-~d" i))
              thief-threads)))
    ;; Owner pushes all items
    (dotimes (i n)
      (ws-deque-push d i))
    ;; Signal done
    (setf (aref done 0) 1)
    ;; Owner pops remaining
    (let ((owner-items nil))
      (loop
        (multiple-value-bind (item found) (ws-deque-pop d)
          (if found
              (push item owner-items)
              (return))))
      ;; Wait for thieves
      (dolist (th thief-threads)
        (thread:join-thread th))
      ;; Collect all items and verify
      (let ((all-items (copy-list owner-items)))
        (dotimes (i num-thieves)
          (setf all-items (nconc all-items (copy-list (aref stolen-items i)))))
        ;; Every item 0..n-1 must appear exactly once
        (let ((sorted (sort all-items #'<)))
          (assert-= n (length sorted))
          (dotimes (i n)
            (assert-= i (nth i sorted))))))))

(deftest test-deque-concurrent-multiple-stealers
  "Multiple stealers contend on a pre-filled deque. No items are lost or
duplicated."
  (let* ((d (make-ws-deque))
         (n 5000)
         (num-thieves 4)
         (stolen-items (make-array num-thieves :initial-element nil))
         (barrier (sem:make-semaphore :name "start-barrier"))
         (thief-threads nil))
    ;; Pre-fill the deque
    (dotimes (i n)
      (ws-deque-push d i))
    ;; Start thieves (they wait on barrier)
    (dotimes (i num-thieves)
      (let ((idx i))
        (push (thread:make-thread
               (lambda ()
                 (sem:wait-on-semaphore barrier)
                 (let ((collected nil))
                   (loop
                     (multiple-value-bind (item status)
                         (ws-deque-steal d)
                       (cond
                         ((eq status t)
                          (push item collected))
                         ((eq status :abort) nil)
                         (t (return)))))
                   (setf (aref stolen-items idx) collected)))
               :name (format nil "thief-~d" i))
              thief-threads)))
    ;; Release all thieves simultaneously
    (dotimes (i num-thieves)
      (sem:signal-semaphore barrier))
    ;; Wait for all thieves
    (dolist (th thief-threads)
      (thread:join-thread th))
    ;; Owner takes what's left
    (let ((owner-items nil))
      (loop
        (multiple-value-bind (item found) (ws-deque-pop d)
          (if found
              (push item owner-items)
              (return))))
      ;; Verify no items lost or duplicated
      (let* ((all-items (copy-list owner-items)))
        (dotimes (i num-thieves)
          (setf all-items (nconc all-items (copy-list (aref stolen-items i)))))
        (let ((sorted (sort all-items #'<)))
          (assert-= n (length sorted))
          (dotimes (i n)
            (assert-= i (nth i sorted))))))))

(deftest test-deque-concurrent-push-pop-steal
  "Owner alternates push/pop while thieves steal. All items are accounted for."
  (let* ((d (make-ws-deque :initial-size 4))
         (n 5000)
         (num-thieves 2)
         (stolen-items (make-array num-thieves :initial-element nil))
         (done (make-array 1 :element-type 'fixnum :initial-element 0))
         (thief-threads nil))
    ;; Start thieves
    (dotimes (i num-thieves)
      (let ((idx i))
        (push (thread:make-thread
               (lambda ()
                 (let ((collected nil))
                   (loop
                     (multiple-value-bind (item status)
                         (ws-deque-steal d)
                       (cond
                         ((eq status t)
                          (push item collected))
                         ((eq status :abort) nil)
                         (t
                          (when (plusp (aref done 0))
                            ;; Final drain
                            (loop
                              (multiple-value-bind (item2 status2)
                                  (ws-deque-steal d)
                                (cond
                                  ((eq status2 t)
                                   (push item2 collected))
                                  ((eq status2 :abort) nil)
                                  (t (return)))))
                            (return))
                          (thread:thread-yield)))))
                   (setf (aref stolen-items idx) collected)))
               :name (format nil "thief-~d" i))
              thief-threads)))
    ;; Owner pushes items, occasionally popping
    (let ((owner-items nil)
          (pushed-count 0))
      (dotimes (i n)
        (ws-deque-push d i)
        (incf pushed-count)
        ;; Pop every 3rd item
        (when (zerop (mod i 3))
          (multiple-value-bind (item found) (ws-deque-pop d)
            (when found
              (push item owner-items)))))
      ;; Drain remaining via pop
      (loop
        (multiple-value-bind (item found) (ws-deque-pop d)
          (if found
              (push item owner-items)
              (return))))
      ;; Signal done
      (setf (aref done 0) 1)
      ;; Wait for thieves
      (dolist (th thief-threads)
        (thread:join-thread th))
      ;; Verify: every pushed item appears exactly once
      (let ((all-items (copy-list owner-items)))
        (dotimes (i num-thieves)
          (setf all-items (nconc all-items (copy-list (aref stolen-items i)))))
        (let ((sorted (sort all-items #'<)))
          ;; Total should equal n (all pushed items accounted for)
          (assert-= n (length sorted))
          ;; No duplicates
          (dotimes (i n)
            (assert-= i (nth i sorted))))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(deftest test-deque-push-pop-push-cycle
  "Repeated push/pop cycles don't corrupt state."
  (let ((d (make-ws-deque :initial-size 2)))
    (dotimes (cycle 50)
      (dotimes (i 10)
        (ws-deque-push d (+ (* cycle 10) i)))
      (dotimes (i 10)
        (multiple-value-bind (item found) (ws-deque-pop d)
          (declare (ignore item))
          (assert-true found))))
    (assert-true (ws-deque-empty-p d))))

(deftest test-deque-steal-all
  "Stealing all items leaves the deque empty."
  (let ((d (make-ws-deque)))
    (dotimes (i 10)
      (ws-deque-push d i))
    (dotimes (i 10)
      (multiple-value-bind (item found) (ws-deque-steal d)
        (assert-= i item)
        (assert-true found)))
    (assert-true (ws-deque-empty-p d))
    ;; Further steal returns empty
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-nil item)
      (assert-nil found))))

(deftest test-deque-nil-values
  "NIL is a valid item value and can be pushed/popped/stolen."
  (let ((d (make-ws-deque)))
    (ws-deque-push d nil)
    (ws-deque-push d nil)
    (assert-= 2 (ws-deque-size d))
    (multiple-value-bind (item found) (ws-deque-pop d)
      (assert-nil item)
      (assert-true found))
    (multiple-value-bind (item found) (ws-deque-steal d)
      (assert-nil item)
      (assert-true found))
    (assert-true (ws-deque-empty-p d))))
