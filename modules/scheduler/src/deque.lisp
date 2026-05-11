;;;; Chase-Lev Work-Stealing Deque
;;;;
;;;; A lock-free concurrent deque supporting three operations:
;;;;   push  - add item to bottom (owner thread only)
;;;;   pop   - remove item from bottom (owner thread only, LIFO)
;;;;   steal - remove item from top (any thread, FIFO)
;;;;
;;;; Based on "Dynamic Circular Work-Stealing Deque" by Chase and Lev (2005),
;;;; with the Le et al. (2013) simplification for correct memory ordering.
;;;;
;;;; The deque uses a growable circular buffer. The owner thread pushes and
;;;; pops from the bottom; thief threads steal from the top. Push/pop are
;;;; wait-free; steal is lock-free (may fail and retry under contention).

(defpackage :epsilon.scheduler.deque
  (:use :cl)
  (:import (epsilon.sys.thread thread))
  (:export
   ;; Deque construction
   #:make-ws-deque
   #:ws-deque
   #:ws-deque-p

   ;; Owner operations (single-thread)
   #:ws-deque-push
   #:ws-deque-pop

   ;; Thief operations (any thread)
   #:ws-deque-steal

   ;; Inspection
   #:ws-deque-size
   #:ws-deque-empty-p))

;;; ---------------------------------------------------------------------------
;;; Circular Buffer
;;; ---------------------------------------------------------------------------

(defstruct (circle-buf (:constructor %make-circle-buf))
  "Power-of-two sized circular buffer backing a ws-deque.
Each buffer is immutable once published; growth creates a new buffer."
  (log-size 0 :type fixnum)
  (data #() :type simple-vector))

(defun make-circle-buf (log-size)
  "Create a circular buffer with capacity 2^LOG-SIZE."
  (declare (type fixnum log-size))
  (%make-circle-buf :log-size log-size
                    :data (make-array (ash 1 log-size) :initial-element nil)))

(declaim (inline circle-buf-capacity))
(defun circle-buf-capacity (buf)
  "Return the capacity of BUF."
  (declare (type circle-buf buf))
  (ash 1 (circle-buf-log-size buf)))

(declaim (inline circle-buf-ref))
(defun circle-buf-ref (buf index)
  "Read the element at INDEX (modulo capacity)."
  (declare (type circle-buf buf)
           (type fixnum index))
  (svref (circle-buf-data buf)
         (logand index (1- (circle-buf-capacity buf)))))

(declaim (inline (setf circle-buf-ref)))
(defun (setf circle-buf-ref) (value buf index)
  "Write VALUE at INDEX (modulo capacity)."
  (declare (type circle-buf buf)
           (type fixnum index))
  (setf (svref (circle-buf-data buf)
               (logand index (1- (circle-buf-capacity buf))))
        value))

(defun circle-buf-grow (old-buf bottom top)
  "Create a new buffer with double the capacity, copying active elements
from TOP to BOTTOM."
  (declare (type circle-buf old-buf)
           (type fixnum bottom top))
  (let ((new-buf (make-circle-buf (1+ (circle-buf-log-size old-buf)))))
    (loop for i from top below bottom
          do (setf (circle-buf-ref new-buf i)
                   (circle-buf-ref old-buf i)))
    new-buf))

;;; ---------------------------------------------------------------------------
;;; Work-Stealing Deque
;;; ---------------------------------------------------------------------------

(defstruct (ws-deque (:constructor %make-ws-deque))
  "Lock-free work-stealing deque.

BOTTOM is modified only by the owner thread.
TOP is modified by any thread via CAS.
BUFFER may be replaced by the owner on growth; stale references are safe
because old buffers remain valid until GC collects them."
  (bottom 0 :type fixnum)
  (top 0 :type fixnum)
  (buffer nil :type (or null circle-buf)))

(defun make-ws-deque (&key (initial-size 8))
  "Create a work-stealing deque. INITIAL-SIZE is rounded up to the next
power of two."
  (let ((log-size (max 1 (ceiling (log (max initial-size 2) 2)))))
    (%make-ws-deque :buffer (make-circle-buf log-size))))

(defun ws-deque-size (deque)
  "Return the approximate number of items in DEQUE.
This is a snapshot and may be stale by the time the caller acts on it."
  (declare (type ws-deque deque))
  (max 0 (- (ws-deque-bottom deque) (ws-deque-top deque))))

(defun ws-deque-empty-p (deque)
  "Return T if DEQUE appears empty."
  (declare (type ws-deque deque))
  (<= (ws-deque-size deque) 0))

;;; ---------------------------------------------------------------------------
;;; Owner Operations
;;; ---------------------------------------------------------------------------

(defun ws-deque-push (deque item)
  "Push ITEM onto the bottom of DEQUE. Only the owner thread may call this.
Grows the backing buffer if necessary."
  (declare (type ws-deque deque))
  (let* ((b (ws-deque-bottom deque))
         (tt (ws-deque-top deque))
         (buf (ws-deque-buffer deque))
         (size (- b tt)))
    ;; Grow if the buffer is full
    (when (>= size (circle-buf-capacity buf))
      (setf buf (circle-buf-grow buf b tt))
      (setf (ws-deque-buffer deque) buf))
    ;; Store the item
    (setf (circle-buf-ref buf b) item)
    ;; Write barrier: ensure item is visible before bottom is incremented.
    ;; On x86-64 stores are ordered, but the barrier prevents compiler
    ;; reordering.
    (thread:memory-barrier :write)
    ;; Publish by advancing bottom
    (setf (ws-deque-bottom deque) (1+ b)))
  (values))

(defun ws-deque-pop (deque)
  "Pop an item from the bottom of DEQUE (LIFO). Only the owner thread may
call this.

Returns two values:
  item, T     -- an item was popped
  NIL,  NIL   -- the deque was empty"
  (declare (type ws-deque deque))
  (let ((b (1- (ws-deque-bottom deque))))
    ;; Speculatively decrement bottom
    (setf (ws-deque-bottom deque) b)
    ;; Full barrier: ensures the bottom store is visible before we read top.
    ;; This prevents the race where a stealer reads the old (higher) bottom
    ;; and believes there are more items than there actually are.
    (thread:memory-barrier :memory)
    (let* ((tt (ws-deque-top deque))
           (size (- b tt)))
      (cond
        ;; Empty: bottom went below top
        ((< size 0)
         (setf (ws-deque-bottom deque) tt)
         (values nil nil))
        ;; Multiple items: safe to take without competing
        ((> size 0)
         (values (circle-buf-ref (ws-deque-buffer deque) b) t))
        ;; Exactly one item: may race with a stealer
        (t
         (let ((item (circle-buf-ref (ws-deque-buffer deque) b)))
           ;; CAS top from tt to tt+1. If it succeeds, we got the item.
           ;; If it fails, a stealer took it.
           (if (eql tt (sb-ext:compare-and-swap
                        (ws-deque-top deque) tt (1+ tt)))
               (progn
                 (setf (ws-deque-bottom deque) (1+ tt))
                 (values item t))
               (progn
                 (setf (ws-deque-bottom deque) (1+ tt))
                 (values nil nil)))))))))

;;; ---------------------------------------------------------------------------
;;; Thief Operations
;;; ---------------------------------------------------------------------------

(defun ws-deque-steal (deque)
  "Steal an item from the top of DEQUE (FIFO). Any thread may call this.

Returns two values:
  item, T      -- an item was stolen
  NIL,  NIL    -- the deque was empty
  NIL,  :ABORT -- lost a CAS race; caller should retry"
  (declare (type ws-deque deque))
  (let ((tt (ws-deque-top deque)))
    ;; Read barrier: ensure we see top before reading bottom and buffer.
    (thread:memory-barrier :read)
    (let* ((b (ws-deque-bottom deque))
           (size (- b tt)))
      (if (<= size 0)
          ;; Empty
          (values nil nil)
          ;; Non-empty: try to steal from top
          (let ((item (circle-buf-ref (ws-deque-buffer deque) tt)))
            ;; CAS top from tt to tt+1
            (if (eql tt (sb-ext:compare-and-swap
                         (ws-deque-top deque) tt (1+ tt)))
                (values item t)
                (values nil :abort)))))))
