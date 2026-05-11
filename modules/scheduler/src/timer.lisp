;;;; Timer Heap -- binary min-heap for deadline-ordered coroutine wakeups
;;;;
;;;; A min-heap indexed by deadline (internal-real-time fixnum). Used by the
;;;; scheduler to efficiently determine the next coroutine timeout to expire.
;;;;
;;;; Operations:
;;;;   timer-heap-insert   O(log n) -- add a deadline entry
;;;;   timer-heap-peek     O(1)     -- inspect soonest deadline
;;;;   timer-heap-pop      O(log n) -- remove soonest deadline
;;;;   timer-heap-drain    O(k log n) -- pop all entries at or before a deadline
;;;;
;;;; The heap stores (deadline . coroutine) pairs. Lazy deletion is supported:
;;;; callers should check if the popped coroutine is still :suspended before
;;;; waking it, since a coroutine may have been woken by a predicate before
;;;; its timeout fired.

(defpackage :epsilon.scheduler.timer
  (:use :cl)
  (:export
   #:timer-heap
   #:make-timer-heap
   #:timer-heap-p
   #:timer-heap-size
   #:timer-heap-empty-p
   #:timer-heap-insert
   #:timer-heap-peek
   #:timer-heap-pop
   #:timer-heap-drain))

;;; ---------------------------------------------------------------------------
;;; Timer entry
;;; ---------------------------------------------------------------------------

(defstruct (timer-entry (:constructor make-timer-entry (deadline coroutine)))
  "A deadline/coroutine pair stored in the timer heap."
  (deadline 0 :type fixnum)
  (coroutine nil))

;;; ---------------------------------------------------------------------------
;;; Timer heap
;;; ---------------------------------------------------------------------------

(defstruct (timer-heap (:constructor %make-timer-heap))
  "Binary min-heap of timer entries, ordered by deadline.
SIZE tracks the number of live entries. DATA is a simple-vector used as
the backing array, grown as needed."
  (data (make-array 64 :initial-element nil) :type simple-vector)
  (size 0 :type fixnum))

(defun make-timer-heap (&key (initial-capacity 64))
  "Create a timer heap with the given initial capacity."
  (%make-timer-heap
   :data (make-array (max 4 initial-capacity) :initial-element nil)))

(defmethod print-object ((h timer-heap) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "size=~D" (timer-heap-size h))))

(defun timer-heap-empty-p (heap)
  "Return T if HEAP has no entries."
  (zerop (timer-heap-size heap)))

;;; ---------------------------------------------------------------------------
;;; Heap internals
;;; ---------------------------------------------------------------------------

(declaim (inline th-parent th-left th-right th-ref th-set th-swap th-deadline<))

(defun th-parent (i) (declare (type fixnum i)) (ash (1- i) -1))
(defun th-left (i) (declare (type fixnum i)) (1+ (ash i 1)))
(defun th-right (i) (declare (type fixnum i)) (+ 2 (ash i 1)))

(defun th-ref (heap i)
  (declare (type timer-heap heap) (type fixnum i))
  (svref (timer-heap-data heap) i))

(defun th-set (heap i entry)
  (declare (type timer-heap heap) (type fixnum i))
  (setf (svref (timer-heap-data heap) i) entry))

(defun th-swap (heap i j)
  (declare (type timer-heap heap) (type fixnum i j))
  (let ((data (timer-heap-data heap)))
    (rotatef (svref data i) (svref data j))))

(defun th-deadline< (a b)
  "Return T if entry A has an earlier deadline than entry B."
  (declare (type timer-entry a b))
  (< (timer-entry-deadline a) (timer-entry-deadline b)))

(defun th-ensure-capacity (heap needed)
  "Grow the backing array if needed."
  (declare (type timer-heap heap) (type fixnum needed))
  (let ((data (timer-heap-data heap)))
    (when (>= needed (length data))
      (let ((new-data (make-array (* 2 (length data)) :initial-element nil)))
        (dotimes (i (timer-heap-size heap))
          (setf (svref new-data i) (svref data i)))
        (setf (timer-heap-data heap) new-data)))))

(defun th-sift-up (heap i)
  "Restore heap property by bubbling entry at I upward."
  (declare (type timer-heap heap) (type fixnum i))
  (loop while (> i 0)
        for parent = (th-parent i)
        while (th-deadline< (th-ref heap i) (th-ref heap parent))
        do (th-swap heap i parent)
           (setf i parent)))

(defun th-sift-down (heap i)
  "Restore heap property by sinking entry at I downward."
  (declare (type timer-heap heap) (type fixnum i))
  (let ((size (timer-heap-size heap)))
    (loop
      (let ((smallest i)
            (left (th-left i))
            (right (th-right i)))
        (when (and (< left size)
                   (th-deadline< (th-ref heap left) (th-ref heap smallest)))
          (setf smallest left))
        (when (and (< right size)
                   (th-deadline< (th-ref heap right) (th-ref heap smallest)))
          (setf smallest right))
        (when (= smallest i)
          (return))
        (th-swap heap i smallest)
        (setf i smallest)))))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun timer-heap-insert (heap deadline coroutine)
  "Insert a deadline/coroutine pair into HEAP. O(log n)."
  (declare (type timer-heap heap) (type fixnum deadline))
  (let ((idx (timer-heap-size heap)))
    (th-ensure-capacity heap idx)
    (th-set heap idx (make-timer-entry deadline coroutine))
    (incf (timer-heap-size heap))
    (th-sift-up heap idx))
  (values))

(defun timer-heap-peek (heap)
  "Return the deadline and coroutine of the soonest entry without removing it.
Returns two values: deadline, coroutine. Returns NIL, NIL if empty."
  (declare (type timer-heap heap))
  (if (timer-heap-empty-p heap)
      (values nil nil)
      (let ((entry (th-ref heap 0)))
        (values (timer-entry-deadline entry)
                (timer-entry-coroutine entry)))))

(defun timer-heap-pop (heap)
  "Remove and return the soonest entry. O(log n).
Returns two values: deadline, coroutine. Returns NIL, NIL if empty."
  (declare (type timer-heap heap))
  (when (timer-heap-empty-p heap)
    (return-from timer-heap-pop (values nil nil)))
  (let ((entry (th-ref heap 0))
        (last-idx (1- (timer-heap-size heap))))
    ;; Move last entry to root and sift down
    (th-set heap 0 (th-ref heap last-idx))
    (th-set heap last-idx nil)
    (decf (timer-heap-size heap))
    (when (> (timer-heap-size heap) 0)
      (th-sift-down heap 0))
    (values (timer-entry-deadline entry)
            (timer-entry-coroutine entry))))

(defun timer-heap-drain (heap now)
  "Pop all entries with deadline <= NOW. Returns a list of coroutines.
O(k log n) where k is the number of expired entries."
  (declare (type timer-heap heap) (type fixnum now))
  (let ((result nil))
    (loop
      (when (timer-heap-empty-p heap)
        (return))
      (let ((entry (th-ref heap 0)))
        (when (> (timer-entry-deadline entry) now)
          (return))
        (timer-heap-pop heap)
        (push (timer-entry-coroutine entry) result)))
    (nreverse result)))
