;;;; Collection protocols for epsilon
;;;;
;;;; Defines extensible protocols for functional collections. These protocols
;;;; enable polymorphic operations across different collection types while
;;;; maintaining functional programming principles.

(defpackage :epsilon.collection.protocols
  (:use :cl)
  (:local-nicknames
   (:proto :epsilon.protocol))
  (:export
   ;; Core collection protocol
   #:icollection
   #:coll-empty
   #:coll-empty-p
   #:coll-count
   #:coll-cons
   
   ;; Sequence protocol
   #:iseq
   #:seq-first
   #:seq-rest
   #:seq-next
   #:seq
   
   ;; Indexed access protocol
   #:iindexed
   #:nth-at
   
   ;; Associative protocol
   #:iassoc
   #:get-at
   #:contains-key-p
   #:with
   #:without
   #:keys-of
   #:vals-of
   #:entries-of
   
   ;; Stack protocol
   #:istack
   #:stack-peek
   #:stack-pop
   #:stack-push
   
   ;; Set protocol
   #:iset
   #:set-member-p
   #:set-conj
   #:set-disj
   #:set-unite
   #:set-intersect
   #:set-diff
   
   ;; Reducible protocol
   #:ireducible
   #:coll-reduce
   #:coll-reduce-kv
   
   ;; Transducible protocol
   #:itransducible
   #:coll-transduce
   
   ;; Persistent collection protocol
   #:ipersistent
   #:pers-assoc
   #:pers-dissoc
   #:pers-conj
   #:pers-pop
   
   ;; Mutable collection protocol
   #:imutable
   #:mut-assoc!
   #:mut-dissoc!
   #:mut-conj!
   #:mut-pop!
   
   ;; Equality protocol
   #:iequiv
   #:equiv
   
   ;; Hashable protocol
   #:ihashable
   #:hash-code
   
   ;; Printable protocol
   #:iprintable
   #:print-coll
   
   ;; Metadata protocol
   #:imeta
   #:meta-get
   #:meta-with
   
   ;; Named protocol
   #:inamed
   #:get-name
   #:get-namespace
   
   ;; Function-like protocol
   #:ifn
   #:invoke
   
   ;; Lookup protocol
   #:ilookup
   #:lookup
   #:lookup-default
   
   ;; Reversible protocol
   #:ireversible
   #:rseq
   
   ;; Sorted protocol
   #:isorted
   #:sorted-seq
   #:sorted-seq-from
   #:sorted-comparator
   
   ;; Counted protocol
   #:icounted
   #:counted-p
   
   ;; Chunked sequence protocol
   #:ichunked
   #:chunk-first
   #:chunk-rest
   #:chunk-size
   
   ;; Edit protocol for transients
   #:ieditable
   #:as-transient
   #:as-persistent!))

(in-package :epsilon.collection.protocols)

;;; Core Collection Protocol

(proto:define-protocol icollection
  (:version "1.0")
  (:documentation "Core protocol for all collections")
  (:method coll-empty (coll) "Return an empty instance of the same collection type")
  (:method coll-empty-p (coll) "Test if collection is empty")
  (:method coll-count (coll) "Return the number of items in collection")
  (:method coll-cons (coll item) "Add item to collection, position depends on type"))

;;; Sequence Protocol

(proto:define-protocol iseq
  (:version "1.0")
  (:documentation "Protocol for sequential access to collections")
  (:method seq-first (coll) "Return first element or nil")
  (:method seq-rest (coll) "Return collection without first element, never nil")
  (:method seq-next (coll) "Return rest or nil if empty")
  (:method seq (coll) "Return a sequence view of collection or nil if empty"))

;;; Indexed Access Protocol

(proto:define-protocol iindexed
  (:version "1.0")
  (:documentation "Protocol for indexed/positional access")
  (:method nth-at (coll n) "Return element at index n")
  (:method nth-at (coll n not-found) "Return element at index n or not-found"))

;;; Associative Protocol

(proto:define-protocol iassoc
  (:version "1.0")
  (:documentation "Protocol for associative collections (maps)")
  (:method get-at (coll key) "Get value for key or nil")
  (:method get-at (coll key not-found) "Get value for key or not-found")
  (:method contains-key-p (coll key) "Test if collection contains key")
  (:method with (coll key val) "Return collection with key mapped to val")
  (:method without (coll key) "Return collection without key")
  (:method keys-of (coll) "Return sequence of keys")
  (:method vals-of (coll) "Return sequence of values")
  (:method entries-of (coll) "Return sequence of [key val] pairs"))

;;; Stack Protocol

(proto:define-protocol istack
  (:version "1.0")
  (:documentation "Protocol for stack operations")
  (:method stack-peek (coll) "Return top element without removing")
  (:method stack-pop (coll) "Return collection without top element")
  (:method stack-push (coll item) "Return collection with item added to top"))

;;; Set Protocol

(proto:define-protocol iset
  (:version "1.0")
  (:documentation "Protocol for set operations")
  (:method set-member-p (coll item) "Test if item is in set")
  (:method set-conj (coll item) "Return set with item added")
  (:method set-disj (coll item) "Return set without item")
  (:method set-unite (coll other) "Return union of sets")
  (:method set-intersect (coll other) "Return intersection of sets")
  (:method set-diff (coll other) "Return difference of sets"))

;;; Reducible Protocol

(proto:define-protocol ireducible
  (:version "1.0")
  (:documentation "Protocol for efficient reduction")
  (:method coll-reduce (coll f) "Reduce collection with function f")
  (:method coll-reduce (coll f init) "Reduce collection with function f and initial value")
  (:method coll-reduce-kv (coll f init) "Reduce associative collection with (f accum key val)"))

;;; Transducible Protocol

(proto:define-protocol itransducible
  (:version "1.0")
  (:documentation "Protocol for transducer support")
  (:method coll-transduce (coll xform f) "Apply transducer xform with reducing function f")
  (:method coll-transduce (coll xform f init) "Apply transducer with initial value"))

;;; Persistent Collection Protocol

(proto:define-protocol ipersistent
  (:version "1.0")
  (:documentation "Protocol for persistent/immutable operations")
  (:method pers-assoc (coll key val) "Return new collection with association")
  (:method pers-dissoc (coll key) "Return new collection without key")
  (:method pers-conj (coll item) "Return new collection with item added")
  (:method pers-pop (coll) "Return new collection with item removed"))

;;; Mutable Collection Protocol

(proto:define-protocol imutable
  (:version "1.0")
  (:documentation "Protocol for mutable/transient operations")
  (:method mut-assoc! (coll key val) "Mutate collection with association")
  (:method mut-dissoc! (coll key) "Mutate collection removing key")
  (:method mut-conj! (coll item) "Mutate collection adding item")
  (:method mut-pop! (coll) "Mutate collection removing item"))

;;; Equality Protocol

(proto:define-protocol iequiv
  (:version "1.0")
  (:documentation "Protocol for value equality")
  (:method equiv (a b) "Test if two values are equivalent"))

;;; Hashable Protocol

(proto:define-protocol ihashable
  (:version "1.0")
  (:documentation "Protocol for hash code generation")
  (:method hash-code (coll) "Return hash code for collection"))

;;; Printable Protocol

(proto:define-protocol iprintable
  (:version "1.0")
  (:documentation "Protocol for custom printing")
  (:method print-coll (coll stream) "Print collection to stream"))

;;; Metadata Protocol

(proto:define-protocol imeta
  (:version "1.0")
  (:documentation "Protocol for metadata support")
  (:method meta-get (obj) "Get metadata map")
  (:method meta-with (obj meta) "Return object with metadata"))

;;; Named Protocol

(proto:define-protocol inamed
  (:version "1.0")
  (:documentation "Protocol for named entities")
  (:method get-name (obj) "Get name part")
  (:method get-namespace (obj) "Get namespace part"))

;;; Function-like Protocol

(proto:define-protocol ifn
  (:version "1.0")
  (:documentation "Protocol for function-like invocation")
  (:method invoke (obj &rest args) "Invoke object as function"))

;;; Lookup Protocol

(proto:define-protocol ilookup
  (:version "1.0")
  (:documentation "Protocol for key lookup")
  (:method lookup (obj key) "Look up key in object")
  (:method lookup-default (obj key default) "Look up key with default"))

;;; Reversible Protocol

(proto:define-protocol ireversible
  (:version "1.0")
  (:documentation "Protocol for reversible collections")
  (:method rseq (coll) "Return reversed sequence view"))

;;; Sorted Protocol

(proto:define-protocol isorted
  (:version "1.0")
  (:documentation "Protocol for sorted collections")
  (:method sorted-seq (coll) "Return sorted sequence")
  (:method sorted-seq-from (coll key) "Return sorted sequence starting from key")
  (:method sorted-comparator (coll) "Return comparator function"))

;;; Counted Protocol

(proto:define-protocol icounted
  (:version "1.0")
  (:documentation "Protocol for O(1) count")
  (:method counted-p (coll) "Return true if count is O(1)"))

;;; Chunked Sequence Protocol

(proto:define-protocol ichunked
  (:version "1.0")
  (:documentation "Protocol for chunked sequences for performance")
  (:method chunk-first (coll) "Return first chunk")
  (:method chunk-rest (coll) "Return sequence after first chunk")
  (:method chunk-size (chunk) "Return size of chunk"))

;;; Edit Protocol

(proto:define-protocol ieditable
  (:version "1.0")
  (:documentation "Protocol for transient/persistent conversion")
  (:method as-transient (coll) "Return mutable transient version")
  (:method as-persistent! (coll) "Convert transient to persistent, invalidating transient"))

;;; Utility functions for protocol usage

(defun empty-p (coll)
  "Universal empty test using protocols"
  (if (typep coll 'sequence)
      (zerop (length coll))
      (coll-empty-p coll)))

(defun conj (coll &rest items)
  "Universal collection construction"
  (reduce #'coll-cons items :initial-value coll))

(defun into (to from)
  "Pour from collection into to collection"
  (coll-reduce from #'coll-cons to))

(defun get-in (coll keys &optional not-found)
  "Navigate nested associative structures"
  (reduce (lambda (c k)
            (if (and c (contains-key-p c k))
                (get-at c k)
                (return-from get-in not-found)))
          keys
          :initial-value coll))

(defun assoc-in (coll keys val)
  "Associate value deep in nested structure"
  (let ((k (first keys))
        (ks (rest keys)))
    (if ks
        (with coll k (assoc-in (get-at coll k) ks val))
        (with coll k val))))

(defun update-in (coll keys f &rest args)
  "Update value deep in nested structure with function"
  (let ((k (first keys))
        (ks (rest keys)))
    (if ks
        (with coll k (apply #'update-in (get-at coll k) ks f args))
        (with coll k (apply f (get-at coll k) args)))))