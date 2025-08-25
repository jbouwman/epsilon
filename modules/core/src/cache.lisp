;;;; This module provides common caching patterns and utilities.

(defpackage epsilon.cache
  (:use cl)
  (:local-nicknames
   (map epsilon.map))
  (:export
   ;; Cache structures
   #:timed-cache
   #:timed-cache-p
   #:make-timed-cache
   #:timed-cache-table
   #:timed-cache-ttl
   #:timed-cache-max-size
   #:timed-cache-hits
   #:timed-cache-misses
   
   ;; Cache operations
   #:cache-get
   #:cache-put
   #:cache-remove
   #:cache-clear
   #:cache-size
   #:cache-keys
   
   ;; Cache management
   #:cache-cleanup
   #:cache-expired-p
   #:with-cache
   
   ;; LRU Cache
   #:lru-cache
   #:lru-cache-p
   #:make-lru-cache
   #:lru-cache-size
   #:lru-cache-capacity
   #:lru-get
   #:lru-put
   #:lru-remove
   #:lru-clear
   
   ;; Cache statistics
   #:cache-stats
   #:cache-hits
   #:cache-misses
   #:cache-hit-rate))

(in-package epsilon.cache)

;;; Timed cache with TTL support

(defstruct (cache-entry (:constructor make-cache-entry (value timestamp)))
  value
  timestamp)

(defstruct timed-cache
  "Cache with time-to-live support"
  (table (make-hash-table :test 'equal) :type hash-table)
  (ttl 300 :type number)  ; 5 minutes default
  (max-size 1000 :type number)
  (hits 0 :type fixnum)
  (misses 0 :type fixnum))

(defun cache-expired-p (entry ttl)
  "Check if a cache entry has expired"
  (when entry
    (> (- (get-universal-time) (cache-entry-timestamp entry)) ttl)))

(defun cache-get (cache key &optional default)
  "Get value from cache, returning default if not found or expired"
  (let ((entry (gethash key (timed-cache-table cache))))
    (cond
      ((null entry)
       (incf (timed-cache-misses cache))
       default)
      ((cache-expired-p entry (timed-cache-ttl cache))
       (remhash key (timed-cache-table cache))
       (incf (timed-cache-misses cache))
       default)
      (t
       (incf (timed-cache-hits cache))
       (cache-entry-value entry)))))

(defun cache-put (cache key value)
  "Put value in cache"
  ;; Check if we need to cleanup old entries
  (when (>= (hash-table-count (timed-cache-table cache))
            (timed-cache-max-size cache))
    (cache-cleanup cache))
  
  (setf (gethash key (timed-cache-table cache))
        (make-cache-entry value (get-universal-time)))
  value)

(defun cache-remove (cache key)
  "Remove key from cache"
  (remhash key (timed-cache-table cache)))

(defun cache-clear (cache)
  "Clear all entries from cache"
  (clrhash (timed-cache-table cache))
  (setf (timed-cache-hits cache) 0)
  (setf (timed-cache-misses cache) 0))

(defun cache-size (cache)
  "Get current cache size"
  (hash-table-count (timed-cache-table cache)))

(defun cache-keys (cache)
  "Get all keys in cache"
  (loop for key being the hash-keys of (timed-cache-table cache)
        collect key))

(defun cache-cleanup (cache)
  "Remove expired entries from cache, and enforce max-size limit"
  (let ((current-time (get-universal-time))
        (ttl (timed-cache-ttl cache))
        (table (timed-cache-table cache))
        (max-size (timed-cache-max-size cache)))
    ;; First remove expired entries
    (loop for key being the hash-keys of table using (hash-value entry)
          when (> (- current-time (cache-entry-timestamp entry)) ttl)
          do (remhash key table))
    ;; If still at or over max-size, remove oldest entry to make room for new one
    (when (>= (hash-table-count table) max-size)
      (let ((entries (loop for key being the hash-keys of table using (hash-value entry)
                          collect (cons key (cache-entry-timestamp entry)))))
        ;; Sort by timestamp (oldest first)
        (setf entries (sort entries #'< :key #'cdr))
        ;; Remove just one oldest entry to make room
        (when entries
          (remhash (car (first entries)) table))))))

(defun cache-hits (cache)
  "Get the number of cache hits"
  (timed-cache-hits cache))

(defun cache-misses (cache)
  "Get the number of cache misses"
  (timed-cache-misses cache))

(defun cache-hit-rate (cache)
  "Calculate cache hit rate"
  (let ((total (+ (timed-cache-hits cache) (timed-cache-misses cache))))
    (if (zerop total)
        0.0
        (/ (timed-cache-hits cache) total))))

;;; LRU Cache implementation

(defstruct lru-node
  key
  value
  prev
  next)

(defstruct (lru-cache
            (:constructor %make-lru-cache))
  "Least Recently Used cache"
  (table (make-hash-table :test 'equal) :type hash-table)
  (capacity 100 :type fixnum)
  (size 0 :type fixnum)
  head  ; dummy head node
  tail) ; dummy tail node

(defun make-lru-cache (&key (capacity 100))
  "Create a new LRU cache with given capacity"
  (let ((cache (%make-lru-cache 
                :table (make-hash-table :test 'equal)
                :capacity (max 0 capacity)  ; Ensure non-negative capacity
                :size 0)))
    ;; Initialize dummy head and tail nodes
    (setf (lru-cache-head cache) (make-lru-node))
    (setf (lru-cache-tail cache) (make-lru-node))
    (setf (lru-node-next (lru-cache-head cache)) (lru-cache-tail cache))
    (setf (lru-node-prev (lru-cache-tail cache)) (lru-cache-head cache))
    cache))

(defun lru-add-to-head (cache node)
  "Add node right after head"
  (let ((head (lru-cache-head cache)))
    (setf (lru-node-prev node) head)
    (setf (lru-node-next node) (lru-node-next head))
    (setf (lru-node-prev (lru-node-next head)) node)
    (setf (lru-node-next head) node)))

(defun lru-remove-node (node)
  "Remove node from doubly linked list"
  (setf (lru-node-next (lru-node-prev node)) (lru-node-next node))
  (setf (lru-node-prev (lru-node-next node)) (lru-node-prev node)))

(defun lru-move-to-head (cache node)
  "Move node to head (mark as most recently used)"
  (lru-remove-node node)
  (lru-add-to-head cache node))

(defun lru-remove-tail (cache)
  "Remove last node before tail"
  (let ((last (lru-node-prev (lru-cache-tail cache))))
    (lru-remove-node last)
    last))

(defun lru-get (cache key &optional default)
  "Get value from LRU cache"
  (let ((node (gethash key (lru-cache-table cache))))
    (if node
        (progn
          (lru-move-to-head cache node)
          (lru-node-value node))
        default)))

(defun lru-put (cache key value)
  "Put value in LRU cache"
  ;; Handle zero capacity edge case
  (when (zerop (lru-cache-capacity cache))
    (return-from lru-put value))
  
  (let ((node (gethash key (lru-cache-table cache))))
    (cond
      ;; Key exists, update value and move to head
      (node
       (setf (lru-node-value node) value)
       (lru-move-to-head cache node))
      
      ;; New key
      (t
       (let ((new-node (make-lru-node :key key :value value)))
         ;; Cache is full, remove LRU item
         (when (>= (lru-cache-size cache) (lru-cache-capacity cache))
           (let ((tail (lru-remove-tail cache)))
             (remhash (lru-node-key tail) (lru-cache-table cache))
             (decf (lru-cache-size cache))))
         
         ;; Add new node
         (lru-add-to-head cache new-node)
         (setf (gethash key (lru-cache-table cache)) new-node)
         (incf (lru-cache-size cache)))))
    value))

(defun lru-remove (cache key)
  "Remove key from LRU cache"
  (let ((node (gethash key (lru-cache-table cache))))
    (when node
      (lru-remove-node node)
      (remhash key (lru-cache-table cache))
      (decf (lru-cache-size cache))
      (lru-node-value node))))

(defun lru-clear (cache)
  "Clear all entries from LRU cache"
  (clrhash (lru-cache-table cache))
  (setf (lru-cache-size cache) 0)
  (setf (lru-node-next (lru-cache-head cache)) (lru-cache-tail cache))
  (setf (lru-node-prev (lru-cache-tail cache)) (lru-cache-head cache)))

;;; Convenience macro for temporary cache usage

(defmacro with-cache ((cache-var &key (type :timed) (ttl 300) (capacity 100)) &body body)
  "Execute body with a temporary cache"
  `(let ((,cache-var ,(case type
                        (:timed `(make-timed-cache :ttl ,ttl :max-size ,capacity))
                        (:lru `(make-lru-cache :capacity ,capacity))
                        (t `(make-hash-table :test 'equal)))))
     ,@body))

;;; Cache statistics utilities

(defun cache-stats (cache)
  "Get cache statistics"
  (typecase cache
    (timed-cache
     (list :type :timed
           :size (cache-size cache)
           :max-size (timed-cache-max-size cache)
           :hits (timed-cache-hits cache)
           :misses (timed-cache-misses cache)
           :hit-rate (cache-hit-rate cache)
           :ttl (timed-cache-ttl cache)))
    (lru-cache
     (list :type :lru
           :size (lru-cache-size cache)
           :capacity (lru-cache-capacity cache)))
    (hash-table
     (list :type :hash-table
           :size (hash-table-count cache)
           :test (hash-table-test cache)))
    (t
     (list :type :unknown))))
