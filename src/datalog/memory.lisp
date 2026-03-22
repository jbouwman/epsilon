;;;; memory.lisp - In-Memory Fact Store
;;;;
;;;; HAMT-based persistent fact store. A store maps relation names (keywords)
;;;; to hash-tables of tuples (each tuple is a list of values, stored as
;;;; hash-table keys with test EQUAL for O(1) dedup and membership).
;;;;
;;;; Hash-tables are treated as immutable values within the store. Any
;;;; mutation (insert, delete) produces a new hash-table via copy-on-write,
;;;; preserving persistent semantics for the outer HAMT.

(defpackage :epsilon.datalog.backend.memory
  (:use :cl :epsilon.syntax)
  (:local-nicknames (:m :epsilon.map)
                    (:s :epsilon.set))
  (:export
   #:make-store #:store-p
   #:assert-fact #:assert-facts
   #:all-facts #:has-fact-p #:fact-count #:total-fact-count #:relations
   #:merge-stores #:retract-fact #:clear-relation
   #:match-tuple-against-pattern
   #:count-pattern))

(in-package :epsilon.datalog.backend.memory)

;;; ============================================================
;;; Store Type
;;; ============================================================

;; A store is a HAMT map from keyword (relation name) to a hash-table
;; of tuples. Each hash-table uses EQUAL test with tuples as keys and T
;; as values, giving O(1) dedup and membership checks.

(defun make-store ()
  "Create an empty fact store."
  m:+empty+)

(defun store-p (x)
  "Check if X is a fact store (a map)."
  (m:map-p x))

;;; ============================================================
;;; Internal Helpers
;;; ============================================================

(defun copy-hash-table (ht)
  "Create a shallow copy of a hash-table."
  (let ((new-ht (make-hash-table :test (hash-table-test ht)
                                 :size (max 16 (hash-table-count ht)))))
    (maphash (lambda (k v) (setf (gethash k new-ht) v)) ht)
    new-ht))

;;; ============================================================
;;; Fact Assertion
;;; ============================================================

(defun assert-fact (store relation tuple)
  "Assert a fact (tuple) into the store for the given relation.
   Returns a new store. Duplicates are avoided via hash-table lookup (O(1)).
   Copy-on-write: a new hash-table is created only when the fact is new."
  (let ((ht (m:get store relation nil)))
    (cond
      ;; Existing hash-table for this relation
      (ht
       (if (gethash tuple ht)
           store
           (let ((new-ht (copy-hash-table ht)))
             (setf (gethash tuple new-ht) t)
             (m:assoc store relation new-ht))))
      ;; New relation: create hash-table and insert into outer map
      (t
       (let ((new-ht (make-hash-table :test 'equal)))
         (setf (gethash tuple new-ht) t)
         (m:assoc store relation new-ht))))))

(defun assert-facts (store relation tuples)
  "Assert multiple tuples into the store for a relation.
   Optimized batch insert: copies the hash-table at most once."
  (when (null tuples)
    (return-from assert-facts store))
  (let ((ht (m:get store relation nil)))
    (if ht
        ;; Existing relation: filter to genuinely new tuples, copy once
        (let ((new-tuples (remove-if (lambda (tuple) (gethash tuple ht)) tuples)))
          (if (null new-tuples)
              store
              (let ((new-ht (copy-hash-table ht)))
                (dolist (tuple new-tuples)
                  (setf (gethash tuple new-ht) t))
                (m:assoc store relation new-ht))))
        ;; New relation: create hash-table with all tuples
        (let ((new-ht (make-hash-table :test 'equal)))
          (dolist (tuple tuples)
            (setf (gethash tuple new-ht) t))
          (m:assoc store relation new-ht)))))

;;; ============================================================
;;; Fact Querying
;;; ============================================================

(defun all-facts (store relation)
  "Return all tuples for a given relation as a list of lists."
  (let ((ht (m:get store relation nil)))
    (when ht
      (loop for tuple being the hash-keys of ht collect tuple))))

(defun has-fact-p (store relation tuple)
  "Check if the store contains the given tuple for the relation."
  (let ((ht (m:get store relation nil)))
    (and ht (gethash tuple ht))))

(defun fact-count (store relation)
  "Return the number of tuples for a relation."
  (let ((ht (m:get store relation nil)))
    (if ht (hash-table-count ht) 0)))

(defun count-pattern (store relation)
  "Return the number of tuples for a relation. Alias for fact-count,
   consistent with Naga naming conventions."
  (fact-count store relation))

(defun total-fact-count (store)
  "Return the total number of facts across all relations."
  (m:reduce (lambda (acc _key ht)
              (declare (ignore _key))
              (+ acc (hash-table-count ht)))
            store 0))

(defun relations (store)
  "Return a list of all relation names in the store."
  (m:keys store))

;;; ============================================================
;;; Store Operations
;;; ============================================================

(defun merge-stores (store1 store2)
  "Merge two stores. Facts from both are included; duplicates removed.
   Neither input store is modified."
  (m:reduce (lambda (result relation ht2)
              (let ((ht1 (m:get result relation nil)))
                (if ht1
                    ;; Merge: copy ht1 and add ht2 entries
                    (let ((new-ht (copy-hash-table ht1)))
                      (maphash (lambda (tuple _v)
                                 (declare (ignore _v))
                                 (setf (gethash tuple new-ht) t))
                               ht2)
                      (m:assoc result relation new-ht))
                    ;; No existing table: copy ht2
                    (let ((new-ht (copy-hash-table ht2)))
                      (m:assoc result relation new-ht)))))
            store2 store1))

(defun retract-fact (store relation tuple)
  "Remove a fact from the store. Returns a new store."
  (let ((ht (m:get store relation nil)))
    (cond
      ((null ht) store)
      ((not (gethash tuple ht)) store)
      (t (let ((new-ht (copy-hash-table ht)))
           (remhash tuple new-ht)
           (m:assoc store relation new-ht))))))

(defun clear-relation (store relation)
  "Remove all facts for a relation. Returns a new store."
  (m:dissoc store relation))

;;; ============================================================
;;; Matching Against Patterns
;;; ============================================================

(defun match-tuple-against-pattern (tuple pattern-terms subst-bindings)
  "Match a ground tuple (list of values) against pattern terms under given bindings.
   SUBST-BINDINGS is an alist of (var-keyword . value) pairs.
   Returns T if the tuple matches all bound variables and constants.

   Pattern terms: :?var keywords are looked up in subst-bindings,
   unbound variables match anything, constants must match exactly."
  (when (not (= (length tuple) (length pattern-terms)))
    (return-from match-tuple-against-pattern nil))
  (loop for val in tuple
        for term in pattern-terms
        always (cond
                 ;; Variable keyword (like :?user)
                 ((and (keywordp term)
                       (> (length (symbol-name term)) 1)
                       (char= #\? (char (symbol-name term) 0)))
                  (let ((bound (cdr (assoc term subst-bindings))))
                    (if bound
                        (equal val bound)
                        t)))  ; unbound variable matches anything
                 ;; Constant: must match
                 (t (equal val term)))))
