;;;; Channels and Streaming
;;;;
;;;; Go-style channels and Rust-style streaming abstractions for epsilon.
;;;; Provides backpressure-aware streaming, fan-in/fan-out patterns,
;;;; and composable stream processing operations.

(defpackage :epsilon.channel
  (:use :cl)
  (:local-nicknames
   (#:thread #:epsilon.sys.thread)
   (#:seq #:epsilon.sequence)
   (#:fn #:epsilon.function))
  (:export
   ;; Channel creation and management
   #:channel
   #:make-channel
   #:channel-p
   #:close-channel
   #:channel-closed-p
   
   ;; Channel operations
   #:send
   #:receive
   #:try-send
   #:try-receive
   #:select
   
   ;; Stream abstraction
   #:data-stream
   #:make-data-stream
   #:data-stream-p
   #:stream-next
   #:stream-close
   #:stream-closed-p
   
   ;; Stream operations (inspired by Rust Iterator trait)
   #:map-stream
   #:filter-stream
   #:take-stream
   #:drop-stream
   #:fold-stream
   #:for-each-stream
   #:collect-stream
   #:zip-stream
   #:merge-streams
   #:split-stream
   
   ;; Backpressure and flow control
   #:with-backpressure
   #:throttle-stream
   #:buffer-stream
   #:batch-stream
   
   ;; Error handling
   #:channel-error
   #:channel-closed-error
   #:channel-timeout-error
   
   ;; Utilities
   #:from-sequence
   #:to-sequence
   #:from-function
   #:from-channel
   #:channel-stats

   ;; CSP-style shorthand operators
   #:>!
   #:<!
   #:close!

   ;; Goroutines and async
   #:spawn
   #:spawn-loop
   #:async
   #:await
   #:await-all
   #:await-any
   #:promise
   #:promise-p
   #:promise-ready-p

   ;; Select for multiplexing
   #:select
   #:alts!
   #:timeout-channel))

(in-package :epsilon.channel)

;;;; Error Conditions

(define-condition channel-error (error)
  ((channel :initarg :channel :reader channel-error-channel))
  (:documentation "Base condition for channel errors"))

(define-condition channel-closed-error (channel-error)
  ()
  (:documentation "Channel is closed")
  (:report (lambda (condition stream)
             (format stream "Channel ~A is closed" 
                     (channel-error-channel condition)))))

(define-condition channel-timeout-error (channel-error)
  ((timeout :initarg :timeout :reader channel-timeout))
  (:documentation "Channel operation timed out")
  (:report (lambda (condition stream)
             (format stream "Channel ~A timed out after ~A seconds"
                     (channel-error-channel condition)
                     (channel-timeout condition)))))

;;;; Channel Implementation

(defstruct channel-stats
  "Channel operation statistics"
  (sends 0 :type integer)
  (receives 0 :type integer)
  (timeouts 0 :type integer)
  (closed-sends 0 :type integer))

(defstruct (channel
            (:constructor %make-channel))
  "Bounded FIFO channel with backpressure"
  (buffer '() :type list)
  (capacity 0 :type integer) ; 0 = unbounded
  (size 0 :type integer)
  (closed-p nil :type boolean)
  (senders '() :type list) ; Waiting senders: (value . condition)
  (receivers '() :type list) ; Waiting receivers: condition
  (lock (sb-thread:make-mutex) :type sb-thread:mutex)
  (stats (make-channel-stats) :type channel-stats))

(defun make-channel (&key (capacity 0))
  "Create a new channel with optional capacity validation
  
  - CAPACITY: Maximum buffer size (0 = unbounded, 1 = synchronous)"
  (when (< capacity 0)
    (error "Channel capacity must be non-negative, got ~A" capacity))
  (%make-channel :capacity capacity))

(defun close-channel (channel)
  "Close a channel, waking all waiting operations"
  (sb-thread:with-mutex ((channel-lock channel))
    (setf (channel-closed-p channel) t)
    
    ;; Wake up all waiting senders and receivers
    (dolist (sender (channel-senders channel))
      (sb-thread:condition-notify (cdr sender)))
    (dolist (receiver (channel-receivers channel))
      (sb-thread:condition-notify receiver))
    
    (setf (channel-senders channel) '()
          (channel-receivers channel) '())))

;;;; Channel Operations

(defun send (channel value &key timeout)
  "Send a value to channel, blocking if buffer is full
  
  Returns T on success, signals error if channel is closed."
  (when (channel-closed-p channel)
    (error 'channel-closed-error :channel channel))
  
  (let ((deadline (when timeout (+ (get-universal-time) timeout))))
    (sb-thread:with-mutex ((channel-lock channel))
      (loop
        ;; Check if channel was closed while waiting
        (when (channel-closed-p channel)
          (incf (channel-stats-closed-sends (channel-stats channel)))
          (error 'channel-closed-error :channel channel))
        
        ;; Try to send immediately
        (cond
          ;; Receiver is waiting
          ((channel-receivers channel)
           (let ((receiver (pop (channel-receivers channel))))
             (incf (channel-stats-sends (channel-stats channel)))
             (sb-thread:condition-notify receiver)
             (return t)))
          
          ;; Buffer has space
          ((or (zerop (channel-capacity channel)) ; unbounded
               (< (channel-size channel) (channel-capacity channel)))
           (setf (channel-buffer channel) 
                 (append (channel-buffer channel) (list value)))
           (incf (channel-size channel))
           (incf (channel-stats-sends (channel-stats channel)))
           (return t))
          
          ;; Need to wait
          (t
           (when (and deadline (<= deadline (get-universal-time)))
             (incf (channel-stats-timeouts (channel-stats channel)))
             (error 'channel-timeout-error :channel channel :timeout timeout))
           
           (let ((condition (sb-thread:make-waitqueue)))
             (push (cons value condition) (channel-senders channel))
             (sb-thread:condition-wait condition (channel-lock channel)
                                       :timeout (when deadline 
                                                 (- deadline (get-universal-time)))))))))))

(defun try-send (channel value)
  "Try to send value without blocking
  
  Returns T if sent, NIL if would block, error if closed."
  (when (channel-closed-p channel)
    (error 'channel-closed-error :channel channel))
  
  (sb-thread:with-mutex ((channel-lock channel))
    (cond
      ;; Receiver waiting
      ((channel-receivers channel)
       (let ((receiver (pop (channel-receivers channel))))
         (incf (channel-stats-sends (channel-stats channel)))
         (sb-thread:condition-notify receiver)
         t))
      
      ;; Buffer has space
      ((or (zerop (channel-capacity channel))
           (< (channel-size channel) (channel-capacity channel)))
       (setf (channel-buffer channel)
             (append (channel-buffer channel) (list value)))
       (incf (channel-size channel))
       (incf (channel-stats-sends (channel-stats channel)))
       t)
      
      ;; Would block
      (t nil))))

(defun receive (channel &key timeout)
  "Receive a value from channel, blocking if empty
  
  Returns (values value t) on success, (values nil nil) if closed and empty."
  (let ((deadline (when timeout (+ (get-universal-time) timeout))))
    (sb-thread:with-mutex ((channel-lock channel))
      (loop
        ;; Try to receive immediately
        (cond
          ;; Buffer has data
          ((plusp (channel-size channel))
           (let ((value (pop (channel-buffer channel))))
             (decf (channel-size channel))
             (incf (channel-stats-receives (channel-stats channel)))
             
             ;; Wake up waiting sender if any
             (when (channel-senders channel)
               (let ((sender (pop (channel-senders channel))))
                 (setf (channel-buffer channel)
                       (append (channel-buffer channel) (list (car sender))))
                 (incf (channel-size channel))
                 (sb-thread:condition-notify (cdr sender))))
             
             (return (values value t))))
          
          ;; Channel closed and empty
          ((channel-closed-p channel)
           (return (values nil nil)))
          
          ;; Need to wait
          (t
           (when (and deadline (<= deadline (get-universal-time)))
             (incf (channel-stats-timeouts (channel-stats channel)))
             (error 'channel-timeout-error :channel channel :timeout timeout))
           
           (let ((condition (sb-thread:make-waitqueue)))
             (push condition (channel-receivers channel))
             (sb-thread:condition-wait condition (channel-lock channel)
                                       :timeout (when deadline
                                                 (- deadline (get-universal-time)))))))))))

(defun try-receive (channel)
  "Try to receive value without blocking
  
  Returns (values value t) if received, (values nil nil) if empty/closed."
  (sb-thread:with-mutex ((channel-lock channel))
    (cond
      ;; Buffer has data
      ((plusp (channel-size channel))
       (let ((value (pop (channel-buffer channel))))
         (decf (channel-size channel))
         (incf (channel-stats-receives (channel-stats channel)))
         
         ;; Wake up waiting sender if any
         (when (channel-senders channel)
           (let ((sender (pop (channel-senders channel))))
             (setf (channel-buffer channel)
                   (append (channel-buffer channel) (list (car sender))))
             (incf (channel-size channel))
             (sb-thread:condition-notify (cdr sender))))
         
         (values value t)))
      
      ;; Empty or closed
      (t (values nil nil)))))

;;;; Stream Abstraction

(defstruct data-stream
  "Lazy stream interface with backpressure support"
  (next-fn nil :type (or null function)) ; () -> (values item more-p)
  (close-fn nil :type (or null function)) ; () -> nil
  (closed-p nil :type boolean))

(defun stream-next (stream)
  "Get next item from stream
  
  Returns (values item more-p) where more-p indicates if stream continues."
  (when (data-stream-closed-p stream)
    (return-from stream-next (values nil nil)))
  
  (if (data-stream-next-fn stream)
      (funcall (data-stream-next-fn stream))
      (values nil nil)))

(defun stream-close (stream)
  "Close a stream and cleanup resources"
  (unless (data-stream-closed-p stream)
    (setf (data-stream-closed-p stream) t)
    (when (data-stream-close-fn stream)
      (funcall (data-stream-close-fn stream)))))

(defun stream-closed-p (stream)
  "Check if stream is closed"
  (data-stream-closed-p stream))

;;;; Stream Constructors

(defun from-sequence (sequence)
  "Create a stream from a sequence"
  (let ((index 0)
        (length (length sequence)))
    (make-data-stream
     :next-fn (lambda ()
                (if (< index length)
                    (values (elt sequence index) 
                            (progn (incf index) (< index length)))
                    (values nil nil))))))

(defun from-function (function &key (while-fn (constantly t)))
  "Create a stream from a function
  
  FUNCTION: () -> value
  WHILE-FN: () -> boolean (continue predicate)"
  (make-data-stream
   :next-fn (lambda ()
              (if (funcall while-fn)
                  (values (funcall function) t)
                  (values nil nil)))))

(defun from-channel (channel)
  "Create a stream from a channel"
  (make-data-stream
   :next-fn (lambda ()
              (multiple-value-bind (value received-p) (try-receive channel)
                (values value received-p)))
   :close-fn (lambda () (close-channel channel))))

;;;; Stream Operations (Rust Iterator-style)

(defun map-stream (stream mapper)
  "Transform stream elements with mapper function"
  (make-data-stream
   :next-fn (lambda ()
              (multiple-value-bind (item more-p) (stream-next stream)
                (if item
                    (values (funcall mapper item) more-p)
                    (values nil nil))))
   :close-fn (lambda () (stream-close stream))))

(defun filter-stream (stream predicate)
  "Keep only elements matching predicate"
  (make-data-stream
   :next-fn (lambda ()
              (loop
               (multiple-value-bind (item more-p) (stream-next stream)
                 (cond
                   ;; Check if item matches predicate before checking more-p
                   ((and item (funcall predicate item)) 
                    (return (values item more-p)))
                   ;; If no more items and current doesn't match, we're done
                   ((not more-p) (return (values nil nil)))))))
   :close-fn (lambda () (stream-close stream))))

(defun take-stream (stream n)
  "Take at most n elements from stream"
  (let ((count 0))
    (make-data-stream
     :next-fn (lambda ()
                (if (< count n)
                    (multiple-value-bind (item more-p) (stream-next stream)
                      (incf count)
                      (values item (and more-p (< count n))))
                    (values nil nil)))
     :close-fn (lambda () (stream-close stream)))))

(defun drop-stream (stream n)
  "Skip first n elements of stream"
  (let ((dropped 0))
    (make-data-stream
     :next-fn (lambda ()
                (loop while (< dropped n)
                      do (multiple-value-bind (item more-p) (stream-next stream)
                           (declare (ignore item))
                           (incf dropped)
                           (unless more-p
                             (return (values nil nil)))))
                (stream-next stream))
     :close-fn (lambda () (stream-close stream)))))

(defun fold-stream (stream initial-value folder)
  "Fold stream into a single value
  
  FOLDER: (accumulator item) -> new-accumulator"
  (let ((acc initial-value))
    (loop
     (multiple-value-bind (item more-p) (stream-next stream)
       (unless item (return acc))  ; Exit when no item, not when more-p is nil
       (setf acc (funcall folder acc item))
       (unless more-p (return acc))))))  ; Exit after processing the last item

(defun for-each-stream (stream consumer)
  "Apply consumer to each stream element"
  (loop
   (multiple-value-bind (item more-p) (stream-next stream)
     (unless item (return))  ; Exit when no item
     (funcall consumer item)
     (unless more-p (return)))))  ; Exit after processing the last item

(defun collect-stream (stream)
  "Collect all stream elements into a list"
  (let ((result '()))
    (for-each-stream stream (lambda (item) (push item result)))
    (nreverse result)))

;;;; Advanced Stream Operations

(defun zip-stream (stream1 stream2)
  "Zip two streams together"
  (make-data-stream
   :next-fn (lambda ()
              (multiple-value-bind (item1 more1-p) (stream-next stream1)
                (multiple-value-bind (item2 more2-p) (stream-next stream2)
                  (if (and item1 item2)
                      (values (list item1 item2) (and more1-p more2-p))
                      (values nil nil)))))
   :close-fn (lambda ()
               (stream-close stream1)
               (stream-close stream2))))

(defun merge-streams (&rest streams)
  "Merge multiple streams (round-robin style)"
  (let ((active-streams (copy-list streams))
        (current-index 0))
    (make-data-stream
     :next-fn (lambda ()
                (loop while active-streams
                      do (when (>= current-index (length active-streams))
                           (setf current-index 0))
                         (let ((stream (nth current-index active-streams)))
                           (multiple-value-bind (item more-p) (stream-next stream)
                             (if more-p
                                 (progn
                                   (incf current-index)
                                   (return (values item t)))
                                 (progn
                                   (setf active-streams 
                                         (remove stream active-streams))
                                   (when (> current-index 0) (decf current-index))))))
                      finally (return (values nil nil))))
     :close-fn (lambda ()
                 (dolist (stream streams)
                   (stream-close stream))))))

;;;; Backpressure and Flow Control

(defun buffer-stream (stream buffer-size)
  "Add buffering to stream with specified buffer size"
  (let ((channel (make-channel :capacity buffer-size))
        (producer-thread nil))
    
    ;; Start producer thread
    (setf producer-thread
          (sb-thread:make-thread
           (lambda ()
             (loop
              (multiple-value-bind (item more-p) (stream-next stream)
                (if more-p
                    (send channel item)
                    (progn
                      (close-channel channel)
                      (return))))))
           :name "stream-buffer-producer"))
    
    (make-data-stream
     :next-fn (lambda () (receive channel))
     :close-fn (lambda ()
                 (stream-close stream)
                 (close-channel channel)
                 (when producer-thread
                   (sb-thread:join-thread producer-thread))))))

(defun throttle-stream (stream rate-per-second)
  "Throttle stream to maximum rate"
  (let ((last-time 0)
        (interval (/ 1.0 rate-per-second)))
    (make-data-stream
     :next-fn (lambda ()
                (let ((now (get-universal-time)))
                  (when (> (- now last-time) interval)
                    (sleep (- interval (- now last-time)))))
                (setf last-time (get-universal-time))
                (stream-next stream))
     :close-fn (lambda () (stream-close stream)))))

(defun batch-stream (stream batch-size)
  "Group stream elements into batches"
  (make-data-stream
   :next-fn (lambda ()
              (let ((batch '())
                    (count 0))
                (loop while (< count batch-size)
                      do (multiple-value-bind (item more-p) (stream-next stream)
                           (unless more-p
                             (return (if batch
                                         (values (nreverse batch) nil)
                                         (values nil nil))))
                           (push item batch)
                           (incf count)))
                (values (nreverse batch) t)))
   :close-fn (lambda () (stream-close stream))))

;;;; CSP-Style Shorthand Operators

(defun >! (channel value &key timeout)
  "Send VALUE to CHANNEL. CSP-style shorthand for send.

   Example: (>! ch 42)"
  (send channel value :timeout timeout))

(defun <! (channel &key timeout)
  "Receive from CHANNEL. CSP-style shorthand for receive.

   Example: (multiple-value-bind (val ok) (<! ch) ...)"
  (receive channel :timeout timeout))

(defun close! (channel)
  "Close CHANNEL. CSP-style shorthand for close-channel."
  (close-channel channel))

;;;; Promise Implementation

(defstruct (promise (:constructor %make-promise)
                     (:predicate promise-p))
  "A promise representing an asynchronous computation"
  (value nil)
  (ready nil :type boolean)
  (error-value nil)
  (lock (sb-thread:make-mutex :name "promise-lock"))
  (condition (sb-thread:make-waitqueue)))

(defun promise-ready-p (promise)
  "Check if PROMISE has completed"
  (promise-ready promise))

(defmacro async (&body body)
  "Execute BODY asynchronously, returning a promise.

   Example:
   (let ((p (async (expensive-computation))))
     (do-other-work)
     (await p))"
  (let ((promise-var (gensym "PROMISE")))
    `(let ((,promise-var (%make-promise)))
       (sb-thread:make-thread
        (lambda ()
          (handler-case
              (let ((result (progn ,@body)))
                (sb-thread:with-mutex ((promise-lock ,promise-var))
                  (setf (promise-value ,promise-var) result)
                  (setf (promise-ready ,promise-var) t)
                  (sb-thread:condition-broadcast (promise-condition ,promise-var))))
            (error (e)
              (sb-thread:with-mutex ((promise-lock ,promise-var))
                (setf (promise-error-value ,promise-var) e)
                (setf (promise-ready ,promise-var) t)
                (sb-thread:condition-broadcast (promise-condition ,promise-var))))))
        :name "async-worker")
       ,promise-var)))

(defun await (promise &optional timeout)
  "Wait for PROMISE to complete and return its value.
   Optionally specify TIMEOUT in seconds.

   Example: (await (async (compute-something)))"
  (sb-thread:with-mutex ((promise-lock promise))
    (loop until (promise-ready promise)
          do (if timeout
                 (unless (sb-thread:condition-wait
                          (promise-condition promise)
                          (promise-lock promise)
                          :timeout timeout)
                   (error 'channel-timeout-error :channel nil :timeout timeout))
                 (sb-thread:condition-wait (promise-condition promise)
                                           (promise-lock promise))))
    (if (promise-error-value promise)
        (error (promise-error-value promise))
        (promise-value promise))))

(defun await-all (&rest promises)
  "Wait for all PROMISES to complete, returning list of values.

   Example:
   (await-all
     (async (fetch url1))
     (async (fetch url2))
     (async (fetch url3)))"
  (mapcar #'await promises))

(defun await-any (&rest promises)
  "Wait for any PROMISE to complete, returning (values value index).

   Example:
   (multiple-value-bind (result idx) (await-any p1 p2 p3)
     (format t \"Promise ~a finished first with ~a~%\" idx result))"
  (loop
    (loop for p in promises
          for i from 0
          when (promise-ready p)
            do (return-from await-any
                 (values (if (promise-error-value p)
                             (error (promise-error-value p))
                             (promise-value p))
                         i)))
    (sleep 0.001)))

;;;; Spawn Macro (Goroutines)

(defmacro spawn (&body body)
  "Spawn a lightweight concurrent process (goroutine).
   Like async but doesn't return a promise.

   Example:
   (let ((ch (make-channel)))
     (spawn (loop for i from 1 to 10
                  do (>! ch i))
            (close! ch))
     (spawn (loop (multiple-value-bind (v ok) (<! ch)
                    (unless ok (return))
                    (print v)))))"
  `(sb-thread:make-thread (lambda () ,@body) :name "goroutine"))

(defmacro spawn-loop (bindings &body body)
  "Spawn a loop in a goroutine with BINDINGS as initial values.
   Use (recur ...) to loop with new values.

   Example:
   (spawn-loop ((count 0))
     (let ((msg (<! ch)))
       (when msg
         (process msg)
         (recur (1+ count)))))"
  (let ((loop-name (gensym "SPAWN-LOOP")))
    `(spawn
       (labels ((,loop-name ,(mapcar #'first bindings)
                  (flet ((recur (&rest args)
                           (apply #',loop-name args)))
                    ,@body)))
         (,loop-name ,@(mapcar #'second bindings))))))

;;;; Select for Multiplexing

(defmacro select (&body clauses)
  "Select from multiple channel operations. Non-deterministically chooses
   the first operation that can proceed without blocking.

   Clause forms:
     ((var <- channel) body...) - Receive from channel
     ((channel <- value) body...) - Send to channel
     ((timeout seconds) body...) - Timeout clause
     (default body...) - Execute if nothing else ready

   Example:
   (select
     ((msg <- input-ch)
      (process msg))
     ((output-ch <- result)
      (log \"sent result\"))
     ((timeout 1.0)
      (log \"timed out\"))
     (default
      (do-other-work)))"
  (let ((ready-ops (gensym "READY-OPS")))
    `(block select-block
       ;; Try each clause without blocking
       (let ((,ready-ops '()))
         ,@(loop for clause in clauses
                 for i from 0
                 unless (or (eq (first clause) 'default)
                            (and (listp (first clause))
                                 (eq (first (first clause)) 'timeout)))
                   collect (expand-select-try clause i ready-ops))

         ;; If something is ready, pick one randomly
         (when ,ready-ops
           (let ((chosen (nth (random (length ,ready-ops)) ,ready-ops)))
             (case (first chosen)
               ,@(loop for clause in clauses
                       for i from 0
                       unless (or (eq (first clause) 'default)
                                  (and (listp (first clause))
                                       (eq (first (first clause)) 'timeout)))
                         collect (expand-select-execute clause i))))))

       ;; Handle timeout clauses
       ,@(loop for clause in clauses
               when (and (listp (first clause))
                         (eq (first (first clause)) 'timeout))
                 collect (expand-timeout-clause clause))

       ;; Handle default clause
       ,@(loop for clause in clauses
               when (eq (first clause) 'default)
                 collect `(return-from select-block (progn ,@(rest clause))))

       ;; If nothing ready and no default, block on first available
       (error "All channels blocked with no default clause"))))

(defun expand-select-try (clause idx ready-ops)
  "Expand a select clause into a try operation"
  (let ((op (first clause)))
    (cond
      ;; Receive: (var <- channel)
      ((and (listp op) (>= (length op) 3)
            (string= (symbol-name (second op)) "<-"))
       (let ((ch (third op)))
         `(multiple-value-bind (val ok) (try-receive ,ch)
            (when ok
              (push (list ,idx val) ,ready-ops)))))

      ;; Send: (channel <- value)
      ((and (listp op) (>= (length op) 3)
            (string= (symbol-name (second op)) "<-"))
       (let ((ch (first op))
             (val (third op)))
         `(when (try-send ,ch ,val)
            (push (list ,idx nil) ,ready-ops)))))))

(defun expand-select-execute (clause idx)
  "Expand a select clause into execution code"
  (let ((op (first clause))
        (body (rest clause)))
    (cond
      ;; Receive: bind the variable
      ((and (listp op) (>= (length op) 3)
            (string= (symbol-name (second op)) "<-")
            (not (channel-form-p (first op))))
       (let ((var (first op)))
         `(,idx (let ((,var (second chosen)))
                  (return-from select-block (progn ,@body))))))

      ;; Send: just execute body
      (t
       `(,idx (return-from select-block (progn ,@body)))))))

(defun channel-form-p (form)
  "Check if FORM looks like a channel (not a simple variable for receive)"
  ;; Heuristic: if it's a function call or has special characters, it's likely a channel
  (and (listp form) (not (eq (first form) 'quote))))

(defun expand-timeout-clause (clause)
  "Expand a timeout clause"
  (let ((seconds (second (first clause)))
        (body (rest clause)))
    `(progn
       (sleep ,seconds)
       (return-from select-block (progn ,@body)))))

(defmacro alts! (&rest channel-ops)
  "Simplified select - just channel operations, returns first ready value.

   Example:
   (alts! ch1 ch2 ch3) ; Returns (values value channel-index)"
  (let ((ops (gensym "OPS"))
        (ready (gensym "READY")))
    `(let ((,ops (list ,@channel-ops)))
       ;; Try each channel
       (loop for ch in ,ops
             for i from 0
             do (multiple-value-bind (val ok) (try-receive ch)
                  (when ok
                    (return (values val i)))))
       ;; Nothing ready - block on first
       (values (receive (first ,ops)) 0))))

;;;; Timeout Channel

(defun timeout-channel (seconds)
  "Create a channel that closes after SECONDS.
   Useful in select for implementing timeouts.

   Example:
   (select
     ((msg <- work-ch) (process msg))
     ((_ <- (timeout-channel 5.0)) (log \"timed out\")))"
  (let ((ch (make-channel :capacity 1)))
    (sb-thread:make-thread
     (lambda ()
       (sleep seconds)
       (close-channel ch))
     :name "timeout-channel")
    ch))
