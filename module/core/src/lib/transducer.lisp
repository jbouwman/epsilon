;;;; Transducers - Composable algorithmic transformations
;;;;
;;;; Based on Clojure's transducer design, providing efficient composable
;;;; transformations that are decoupled from the data structures they operate on.
;;;; Transducers eliminate intermediate allocations and enable early termination.

(defpackage epsilon.lib.transducer
  (:use cl)
  (:shadow map filter remove replace comp sequence)
  (:local-nicknames
   (seq epsilon.lib.sequence)
   (map epsilon.lib.map)
   (vec epsilon.lib.vector))
  (:export
   ;; Core protocol
   reduced
   reduced-p
   reduced-value
   ensure-reduced
   preserving-reduced
   
   ;; Transducer creation
   map
   filter
   remove
   take
   take-while
   drop
   drop-while
   take-nth
   replace
   partition-by
   partition-all
   keep
   keep-indexed
   mapcat
   dedupe
   random-sample
   
   ;; Transducer composition
   comp
   
   ;; Reduction functions
   transduce
   into
   sequence
   eduction
   eduction->seq
   
   ;; Reducing functions
   completing
   cat
   
   ;; Early termination
   halt-when
   
   ;; Utilities
   iterator))

(in-package :epsilon.lib.transducer)

;;;; Reduced Protocol
;;;; Signals early termination in reduction

(defstruct (reduced (:constructor %make-reduced (value)))
  value)

(defun reduced (x)
  "Wrap a value to signal early termination of reduction"
  (%make-reduced x))

(defun unreduced (x)
  "Extract value from reduced wrapper, unwrapping nested reduced values"
  (if (reduced-p x)
      (unreduced (reduced-value x))
    x))

(defun ensure-reduced (x)
  "Ensure value is wrapped as reduced"
  (if (reduced-p x) x (reduced x)))

(defmacro preserving-reduced (expr)
  "Evaluate expr and preserve reduced status"
  (let ((result (gensym)))
    `(let ((,result ,expr))
       (if (reduced-p ,result)
           ,result
         ,result))))

;;;; Reducing Function Protocol
;;;; A reducing function is a function of 2 arguments that may also support:
;;;; - 0 arguments: initialization
;;;; - 1 argument: completion

(defun completing (f &optional cf)
  "Create a completing reducing function.
   f is the 2-arg reducing function.
   cf is the optional 1-arg completion function."
  (lambda (&rest args)
    (case (length args)
	  (0 (handler-case (funcall f)  ; init - try calling f with no args
               (error () nil)))  ; if it fails, return nil
	  (1 (if cf
		 (funcall cf (first args))
               (first args)))  ; completion
	  (2 (apply f args))  ; step
	  (t (error "Invalid arity for reducing function")))))

;;;; Core Transducer Implementations

(defun map (f)
  "Returns a transducer that applies f to each input"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
            (0 (funcall rf))  ; init
            (1 (funcall rf (first args)))  ; completion
            (2 (funcall rf (first args) (funcall f (second args))))  ; step
            (t (error "Invalid arity for reducing function"))))))

(defun filter (pred)
  "Returns a transducer that filters inputs by pred"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
            (0 (funcall rf))  ; init
            (1 (funcall rf (first args)))  ; completion
            (2 (let ((result (first args))
                     (input (second args)))
		 (if (funcall pred input)
                     (funcall rf result input)
                   result)))
            (t (error "Invalid arity for reducing function"))))))

(defun remove (pred)
  "Returns a transducer that removes inputs matching pred"
  (filter (complement pred)))

(defun take (n)
  "Returns a transducer that takes first n inputs"
  (lambda (rf)
    (let ((remaining n))
      (lambda (&rest args)
        (case (length args)
              (0 (funcall rf))  ; init
              (1 (funcall rf (first args)))  ; completion
              (2 (let ((result (first args))
                       (input (second args)))
		   (if (plusp remaining)
                       (let ((result (funcall rf result input)))
			 (decf remaining)
			 (if (zerop remaining)
                             (ensure-reduced result)
                           result))
                     (ensure-reduced result))))
              (t (error "Invalid arity for reducing function")))))))

(defun take-while (pred)
  "Returns a transducer that takes inputs while pred is true"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
            (0 (funcall rf))  ; init
            (1 (funcall rf (first args)))  ; completion
            (2 (let ((result (first args))
                     (input (second args)))
		 (if (funcall pred input)
                     (funcall rf result input)
                   (reduced result))))
            (t (error "Invalid arity for reducing function"))))))

(defun drop (n)
  "Returns a transducer that drops first n inputs"
  (lambda (rf)
    (let ((remaining n))
      (lambda (&rest args)
        (case (length args)
              (0 (funcall rf))  ; init
              (1 (funcall rf (first args)))  ; completion
              (2 (let ((result (first args))
                       (input (second args)))
		   (if (plusp remaining)
                       (progn
			 (decf remaining)
			 result)
                     (funcall rf result input))))
              (t (error "Invalid arity for reducing function")))))))

(defun drop-while (pred)
  "Returns a transducer that drops inputs while pred is true"
  (lambda (rf)
    (let ((dropping t))
      (lambda (&rest args)
        (case (length args)
              (0 (funcall rf))  ; init
              (1 (funcall rf (first args)))  ; completion
              (2 (let ((result (first args))
                       (input (second args)))
		   (if (and dropping (funcall pred input))
                       result
                     (progn
                       (setf dropping nil)
                       (funcall rf result input)))))
              (t (error "Invalid arity for reducing function")))))))

(defun take-nth (n)
  "Returns a transducer that takes every nth input"
  (lambda (rf)
    (let ((counter 0))
      (lambda (&rest args)
        (case (length args)
              (0 (funcall rf))  ; init
              (1 (funcall rf (first args)))  ; completion
              (2 (let ((result (first args))
                       (input (second args)))
		   (if (zerop (mod counter n))
                       (prog1 (funcall rf result input)
			 (incf counter))
                     (progn
                       (incf counter)
                       result))))
              (t (error "Invalid arity for reducing function")))))))

(defun replace (smap)
  "Returns a transducer that replaces values found in smap"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
            (0 (funcall rf))  ; init
            (1 (funcall rf (first args)))  ; completion
            (2 (funcall rf (first args) 
			(or (map:get smap (second args)) (second args))))  ; step
            (t (error "Invalid arity for reducing function"))))))

(defun partition-by (f)
  "Returns a transducer that partitions inputs by f"
  (lambda (rf)
    (let ((previous-key :none)
          (part nil))
      (lambda (&rest args)
        (case (length args)
              (0 (funcall rf))  ; init
              (1 ;; completion - flush any remaining partition
               (let ((result (first args)))
		 (if part
                     (funcall rf (funcall rf result (nreverse part)))
                   (funcall rf result))))
              (2 (let ((result (first args))
                       (input (second args))
                       (key (funcall f (second args))))
		   (if (or (eq previous-key :none)
			   (equal key previous-key))
                       (progn
			 (push input part)
			 (setf previous-key key)
			 result)
                     (let ((result (funcall rf result (nreverse part))))
                       (if (reduced-p result)
                           result
                         (progn
                           (setf part (list input)
                                 previous-key key)
                           result))))))
              (t (error "Invalid arity for reducing function")))))))

(defun partition-all (n &optional step)
  "Returns a transducer that partitions inputs into groups of n"
  (let ((step (or step n)))
    (lambda (rf)
      (let ((part nil)
            (skip 0))
        (lambda (&rest args)
          (case (length args)
		(0 (funcall rf))  ; init
		(1 ;; completion - flush any remaining partition
		 (let ((result (first args)))
		   (if part
                       (funcall rf (funcall rf result (nreverse part)))
                     (funcall rf result))))
		(2 (let ((result (first args))
			 (input (second args)))
                     (if (plusp skip)
			 (progn
			   (decf skip)
			   result)
                       (progn
			 (push input part)
			 (if (= (length part) n)
                             (let ((result (funcall rf result (nreverse part))))
                               (setf part nil
                                     skip (- step n))
                               result)
                           result)))))
		(t (error "Invalid arity for reducing function"))))))))

(defun keep (f)
  "Returns a transducer that keeps non-nil results of f"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
	    (0 (funcall rf))  ; init
	    (1 (funcall rf (first args)))  ; completion
	    (2 (let ((result (first args))
		     (input (second args))
		     (v (funcall f (second args))))
		 (if v
		     (funcall rf result v)
                   result)))
	    (t (error "Invalid arity for reducing function"))))))

(defun keep-indexed (f)
  "Returns a transducer that keeps non-nil results of (f index input)"
  (lambda (rf)
    (let ((index 0))
      (lambda (&rest args)
        (case (length args)
	      (0 (funcall rf))  ; init
	      (1 (funcall rf (first args)))  ; completion
	      (2 (let ((result (first args))
		       (input (second args))
		       (v (funcall f index input)))
		   (incf index)
		   (if v
		       (funcall rf result v)
		     result)))
	      (t (error "Invalid arity for reducing function")))))))

(defun mapcat (f)
  "Returns a transducer that applies f and cats the results"
  (comp (map f) (cat)))

(defun dedupe ()
  "Returns a transducer that removes consecutive duplicates"
  (lambda (rf)
    (let ((previous :none))
      (lambda (&rest args)
        (case (length args)
	      (0 (funcall rf))  ; init
	      (1 (funcall rf (first args)))  ; completion
	      (2 (let ((result (first args))
		       (input (second args)))
		   (if (and (not (eq previous :none))
			    (equal input previous))
		       result
		     (progn
		       (setf previous input)
		       (funcall rf result input)))))
	      (t (error "Invalid arity for reducing function")))))))

(defun random-sample (prob)
  "Returns a transducer that randomly samples with probability prob"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
	    (0 (funcall rf))  ; init
	    (1 (funcall rf (first args)))  ; completion
	    (2 (if (< (random 1.0) prob)
		   (funcall rf (first args) (second args))
		 (first args)))  ; step - return unchanged result if not sampled
	    (t (error "Invalid arity for reducing function"))))))

(defun halt-when (pred &optional retf)
  "Returns a transducer that halts when pred returns true"
  (lambda (rf)
    (lambda (&rest args)
      (case (length args)
	    (0 (funcall rf))  ; init
	    (1 (funcall rf (first args)))  ; completion
	    (2 (let ((result (first args))
		     (input (second args)))
		 (if (funcall pred input)
		     (reduced (if retf
				  (funcall retf (funcall rf result) input)
				result))
                   (funcall rf result input))))
	    (t (error "Invalid arity for reducing function"))))))

;;;; Cat - Concatenation transducer

(defun cat-step (rf)
  "Step function for cat transducer"
  (lambda (&rest args)
    (case (length args)
	  (0 (funcall rf))  ; init
	  (1 (funcall rf (first args)))  ; completion
	  (2 (let ((result (first args))
		   (input (second args)))
	       (if (listp input)
		   ;; Handle regular Lisp lists
		   (cl:reduce rf input :initial-value result)
		 ;; Handle epsilon sequences
		 (seq:reduce rf input :initial-value result))))
	  (t (error "Invalid arity for reducing function")))))

(defun cat ()
  "Returns a transducer that concatenates nested colls"
  (lambda (rf)
    (cat-step rf)))

;;;; Transducer Composition

(defun comp (&rest xforms)
  "Compose multiple transducers into one"
  (lambda (rf)
    (cl:reduce (lambda (xf rf)  ; swapped order for :from-end t
                 (funcall xf rf))
	       xforms
	       :initial-value rf
	       :from-end t)))

;;;; Core Reduction Functions

(defun transduce (xform f init coll)
  "Reduce coll with f+xform, starting with init"
  (let* ((rf (funcall xform (completing f)))  ; wrap f with completing
         (result (catch 'reduced-exit
                   (if (listp coll)
		       ;; Handle regular Lisp lists
		       (cl:reduce (lambda (acc x)
                                    (let ((res (funcall rf acc x)))
				      (if (reduced-p res)
                                          (throw 'reduced-exit (unreduced res))
                                        res)))
                                  coll
                                  :initial-value init)
                     ;; Handle epsilon sequences
                     (seq:reduce (lambda (acc x)
                                   (let ((res (funcall rf acc x)))
                                     (if (reduced-p res)
                                         (throw 'reduced-exit (unreduced res))
                                       res)))
                                 coll
                                 :initial-value init)))))
    (funcall rf result)))

(defun into (to xform from)
  "Transduce items from 'from' into 'to' with xform"
  (cond
   ;; into vector
   ((vectorp to)
    (transduce xform 
               (lambda (v x) 
                 (let ((new-v (make-array (1+ (length v))
                                          :element-type (array-element-type v))))
                   (cl:replace new-v v)
                   (setf (aref new-v (length v)) x)
                   new-v))
               to from))
   ;; into map
   ((typep to 'map::hamt)
    (transduce xform 
               (lambda (m kv)
                 (map:assoc m (cl:first kv) (cl:second kv)))
               to from))
   ;; into list
   ((listp to)
    (nreverse (transduce xform 
                         (lambda (lst x) (cl:cons x lst))
                         to from)))
   ;; default: into sequence
   (t (sequence xform from))))

(defun sequence (xform coll)
  "Create a lazy sequence by applying xform to coll"
  (eduction->seq (eduction xform coll)))

;;;; Eduction - Delayed application of transducers

(defstruct (eduction (:constructor make-eduction (xform coll)))
  xform
  coll)

(defun eduction (xform coll)
  "Returns an eduction - a delayed application of transducer to collection"
  (make-eduction xform coll))

(defun eduction->seq (ed)
  "Convert eduction to sequence"
  (iterator-seq (iterator ed)))

(defun iterator (ed)
  "Create an iterator from an eduction"
  (let* ((coll (eduction-coll ed))
         (coll-iter (if (listp coll)
                        ;; Create iterator for regular lists
                        (let ((remaining coll))
                          (lambda ()
                            (when remaining
			      (pop remaining))))
                      ;; Use seq iterator for epsilon sequences
                      (seq:iterator coll)))
         (xform (eduction-xform ed))
         (buffer nil)
         (done nil))
    
    ;; Create a reducing function that accumulates into buffer
    (let ((rf (funcall xform
		       (lambda (result input)
                         (declare (ignore result))
                         (push input buffer)
                         nil))))
      
      (lambda ()
        (loop
         ;; If we have buffered items, return the next one
         (when buffer
           (return (pop buffer)))
         
         ;; If done, return nil
         (when done
           (return nil))
         
         ;; Try to get next item from source
         (let ((next (funcall coll-iter)))
           (if next
               ;; Process through transducer
               (let ((result (funcall rf nil next)))
                 (when (reduced-p result)
                   (setf done t)))
             ;; Source exhausted, flush transducer
             (progn
               (funcall rf nil)  ; completion
               (setf done t)
               (setf buffer (nreverse buffer))))))))))

(defun iterator-seq (iter)
  "Create a lazy sequence from an iterator"
  (labels ((next-seq ()
		     (let ((val (funcall iter)))
		       (if val
			   (seq::make-cons :head val
					   :tail-promise (seq::delay (lambda () (next-seq))))
			 seq:*empty*))))
	  (next-seq)))
