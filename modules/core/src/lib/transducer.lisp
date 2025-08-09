;;;; Transducers - Composable algorithmic transformations
;;;;
;;;; Based on Clojure's transducer design, providing efficient composable
;;;; transformations that are decoupled from the data structures they operate on.
;;;; Transducers eliminate intermediate allocations and enable early termination.

(defpackage epsilon.transducer
  (:use cl)
  (:shadow map filter remove replace comp sequence)
  (:local-nicknames
   (seq epsilon.sequence)
   (map epsilon.map))
  (:export
   ;; Core protocol
   reduced
   reduced-p
   reduced-value
   ensure-reduced
   preserving-reduced
   
   ;; Basic transducers
   map
   filter
   remove
   take
   take-while
   take-nth
   drop
   drop-while
   dedupe
   partition-by
   partition-all
   keep
   keep-indexed
   replace
   mapcat
   halt-when
   cat
   
   ;; Transducer composition
   comp
   
   ;; Reduction functions
   transduce
   into
   
   ;; Advanced constructs
   eduction
   eduction->seq
   sequence
   sequence))

(in-package epsilon.transducer)

;;; Reduced protocol - for early termination

(defstruct (tx-reduced (:constructor make-reduced (value)))
  value)

(defun reduced (value)
  "Wrap a value to signal early termination of reduction"
  (make-reduced value))

(defun reduced-p (x)
  "Test if a value is reduced"
  (typep x 'tx-reduced))

(defun reduced-value (x)
  "Extract value from reduced wrapper"
  (if (reduced-p x)
      (tx-reduced-value x)
      x))

(defun ensure-reduced (x)
  "Ensure value is wrapped as reduced"
  (if (reduced-p x)
      x
      (reduced x)))

(defun preserving-reduced (rf)
  "Wrap reducing function to preserve reduced state"
  (lambda (result input)
    (let ((ret (funcall rf result input)))
      (if (reduced-p ret)
          (ensure-reduced ret)
          ret))))

;;; Core transducer implementations

(defun map (f)
  "Returns a transducer that applies f to each input"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (funcall rf result (funcall f input))
          (funcall rf result)))))

(defun filter (pred)
  "Returns a transducer that passes through items where (pred item) is true"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (if (funcall pred input)
              (funcall rf result input)
              result)
          (funcall rf result)))))

(defun remove (pred)
  "Returns a transducer that removes items where (pred item) is true"
  (filter (complement pred)))

(defun take (n)
  "Returns a transducer that passes through at most n items"
  (lambda (rf)
    (let ((count 0))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (if (< count n)
                (progn
                  (incf count)
                  (if (= count n)
                      (ensure-reduced (funcall rf result input))
                      (funcall rf result input)))
                result)
            (funcall rf result))))))

(defun take-while (pred)
  "Returns a transducer that passes through items while (pred item) is true"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (if (funcall pred input)
              (funcall rf result input)
              (ensure-reduced result))
          (funcall rf result)))))

(defun take-nth (n)
  "Returns a transducer that takes every nth item (1-based indexing)"
  (lambda (rf)
    (let ((count 0))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (progn
              (incf count)
              (if (zerop (mod count n))
                  (funcall rf result input)
                  result))
            (funcall rf result))))))

(defun drop (n)
  "Returns a transducer that drops the first n items"
  (lambda (rf)
    (let ((count 0))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (if (< count n)
                (progn
                  (incf count)
                  result)
                (funcall rf result input))
            (funcall rf result))))))

(defun drop-while (pred)
  "Returns a transducer that drops items while (pred item) is true"
  (lambda (rf)
    (let ((dropping t))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (if (and dropping (funcall pred input))
                result
                (progn
                  (setf dropping nil)
                  (funcall rf result input)))
            (funcall rf result))))))

(defun dedupe (&optional (keyfn #'identity))
  "Returns a transducer that removes consecutive duplicate items.
   Optional keyfn is used to compute comparison key."
  (lambda (rf)
    (let ((previous-key nil)
          (has-previous nil))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (let ((current-key (funcall keyfn input)))
              (if (and has-previous (equal current-key previous-key))
                  result
                  (progn
                    (setf previous-key current-key
                          has-previous t)
                    (funcall rf result input))))
            (funcall rf result))))))

(defun partition-by (f)
  "Returns a transducer that partitions consecutive items based on the result of applying f"
  (lambda (rf)
    (let ((previous-key nil)
          (has-previous nil)
          (current-partition '()))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (let ((current-key (funcall f input)))
              (cond
                ;; First item or same partition
                ((or (not has-previous) (equal current-key previous-key))
                 (push input current-partition)
                 (setf previous-key current-key
                       has-previous t)
                 result)
                ;; New partition - emit current and start new
                (t
                 (let ((partition-to-emit (nreverse current-partition)))
                   (setf current-partition (list input)
                         previous-key current-key)
                   (funcall rf result partition-to-emit)))))
            ;; Final call - emit any remaining partition
            (if current-partition
                (let ((partition-to-emit (nreverse current-partition)))
                  (setf current-partition nil)
                  (funcall rf result partition-to-emit))
                (funcall rf result)))))))

(defun partition-all (n &optional (step n))
  "Returns a transducer that partitions items into chunks of size n, stepping by step"
  (lambda (rf)
    (let ((current-partition '())
          (count 0)
          (step-count 0))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (progn
              (when (< step-count step)
                (push input current-partition)
                (incf count))
              (incf step-count)
              (cond
                ;; Partition is full, emit it
                ((= count n)
                 (let ((partition-to-emit (nreverse current-partition)))
                   (setf current-partition '()
                         count 0
                         step-count 0)
                   (funcall rf result partition-to-emit)))
                ;; Need to step without adding to partition
                ((= step-count step)
                 (setf step-count 0)
                 result)
                ;; Continue building partition
                (t result)))
            ;; Final call - emit any remaining partition
            (if current-partition
                (let ((partition-to-emit (nreverse current-partition)))
                  (setf current-partition nil)
                  (funcall rf result partition-to-emit))
                (funcall rf result)))))))

(defun keep (f)
  "Returns a transducer that applies f to each input and keeps only non-nil results"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (let ((output (funcall f input)))
            (if output
                (funcall rf result output)
                result))
          (funcall rf result)))))

(defun keep-indexed (f)
  "Returns a transducer that applies f to (index, input) and keeps only non-nil results"
  (lambda (rf)
    (let ((index -1))
      (lambda (result &optional (input nil input-p))
        (if input-p
            (progn
              (incf index)
              (let ((output (funcall f index input)))
                (if output
                    (funcall rf result output)
                    result)))
            (funcall rf result))))))

(defun replace (replacement-map)
  "Returns a transducer that replaces items using the replacement map"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (let ((replacement (if (map:contains-p replacement-map input)
                                 (map:get replacement-map input)
                                 input)))
            (funcall rf result replacement))
          (funcall rf result)))))

(defun mapcat (f)
  "Returns a transducer that maps f over inputs and concatenates the results"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (let ((mapped (funcall f input)))
            (if (listp mapped)
                (reduce rf mapped :initial-value result)
                (funcall rf result mapped)))
          (funcall rf result)))))

(defun halt-when (pred &optional (retf nil))
  "Returns a transducer that stops when pred returns true"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (if (funcall pred input)
              (if retf
                  (ensure-reduced (funcall retf result input))
                  (ensure-reduced result))
              (funcall rf result input))
          (funcall rf result)))))

(defun cat ()
  "Returns a transducer that concatenates collections"
  (lambda (rf)
    (lambda (result &optional (input nil input-p))
      (if input-p
          (cond
            ((listp input)
             (loop with acc = result
                   for item in input
                   do (setf acc (funcall rf acc item))
                   when (reduced-p acc)
                     return acc
                   finally (return acc)))
            ((vectorp input)
             (loop with acc = result
                   for item across input
                   do (setf acc (funcall rf acc item))
                   when (reduced-p acc)
                     return acc
                   finally (return acc)))
            (t
             (funcall rf result input)))
          (funcall rf result)))))

;;; Transducer composition

(defun comp (&rest xforms)
  "Compose transducers right-to-left (same order as function composition)"
  (lambda (rf)
    (reduce (lambda (rf xf) (funcall xf rf))
            (reverse xforms)
            :initial-value rf)))

;;; Reduction functions

(defun transduce (xform f init coll)
  "Transform coll with xform, then reduce with f starting with init"
  ;; Wrap the user function to handle the completion arity
  (let* ((wrapped-f (lambda (result &optional (input nil input-p))
                      (if input-p
                          (funcall f result input)
                          result)))
         (rf (funcall xform wrapped-f))
         (result init))
    (if (listp coll)
        ;; Handle regular Lisp lists
        (dolist (item coll)
          (setf result (funcall rf result item))
          (when (reduced-p result)
            (return-from transduce (reduced-value result))))
        ;; Handle epsilon sequences
        (seq:each (lambda (item)
                    (setf result (funcall rf result item))
                    (when (reduced-p result)
                      (return-from transduce (reduced-value result))))
                  coll))
    (funcall rf result)))

(defun into (to xform from)
  "Transform elements from FROM with XFORM and add to TO"
  (let ((rf (make-collection-reducer to)))
    (transduce xform rf to from)))

(defun make-collection-reducer (to)
  "Create a reducing function appropriate for the target collection type"
  (typecase to
    (vector
     (lambda (result &optional (input nil input-p))
       (if input-p
           (let ((new-vec (make-array (1+ (length result))
                                     :element-type (array-element-type result)
                                     :initial-contents (concatenate 'list result (list input)))))
             new-vec)
           result)))
    ((satisfies map:map-p)
     (lambda (result &optional (input nil input-p))
       (if input-p
           (if (and (listp input) (= (length input) 2))
               (map:assoc result (first input) (second input))
               (error "Map transducer must produce key-value pairs"))
           result)))
    (list
     (lambda (result &optional (input nil input-p))
       (if input-p
           (cons input result)
           result)))
    (t
     (lambda (result &optional (input nil input-p))
       (if input-p
           (cons input result)
           result)))))

(defun sequence (xform coll)
  "Create a sequence by transforming coll with xform"
  (seq:seq (reverse (into '() xform coll))))

;;; Eduction - lazy computation construct

(defstruct eduction
  "An eduction is a lazy application of transducers to a collection"
  xform
  coll)

(defun eduction (xform coll)
  "Create an eduction that lazily applies xform to coll"
  (make-eduction :xform xform :coll coll))

(defun eduction->seq (eduction)
  "Convert an eduction to a lazy sequence"
  (let ((xform (eduction-xform eduction))
        (coll (eduction-coll eduction)))
    (sequence xform coll)))