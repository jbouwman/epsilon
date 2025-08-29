;;;; Functional Data Structures Benchmarks
;;;;
;;;; This module provides comprehensive benchmarks for HAMT-based functional
;;;; data structures (maps and sets) which are critical to Epsilon's performance

(defpackage epsilon.tool.benchmark.functional-data
  (:use cl)
  (:local-nicknames
   (benchmark epsilon.tool.benchmark)
   (suites epsilon.tool.benchmark.suites)
   (map epsilon.map)
   (set epsilon.set))
  (:export
   register-functional-data-benchmarks))

(in-package epsilon.tool.benchmark.functional-data)

;;; HAMT Map Benchmarks

(defun register-map-benchmarks ()
  "Register HAMT map benchmarks"
  
  ;; Basic map operations
  (benchmark:defbenchmark map-assoc-single ()
    (map:assoc map:+empty+ :key "value"))
  
  (benchmark:defbenchmark map-get-single ()
    (let ((m (map:assoc map:+empty+ :key "value")))
      (map:get m :key)))
  
  (benchmark:defbenchmark map-dissoc-single ()
    (let ((m (map:assoc map:+empty+ :key "value")))
      (map:dissoc m :key)))
  
  ;; Small map operations (10 entries)
  (let ((small-map (loop with m = map:+empty+
                        for i from 0 below 10
                        do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                        finally (return m))))
    
    (benchmark:defbenchmark map-assoc-small-10 ()
      (map:assoc small-map :new-key "new-value"))
    
    (benchmark:defbenchmark map-get-small-10 ()
      (map:get small-map 'key5))
    
    (benchmark:defbenchmark map-dissoc-small-10 ()
      (map:dissoc small-map 'key5))
    
    (benchmark:defbenchmark map-contains-small-10 ()
      (map:contains-key-p small-map 'key5)))
  
  ;; Medium map operations (100 entries)
  (let ((medium-map (loop with m = map:+empty+
                         for i from 0 below 100
                         do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                         finally (return m))))
    
    (benchmark:defbenchmark map-assoc-medium-100 ()
      (map:assoc medium-map :new-key "new-value"))
    
    (benchmark:defbenchmark map-get-medium-100 ()
      (map:get medium-map 'key50))
    
    (benchmark:defbenchmark map-dissoc-medium-100 ()
      (map:dissoc medium-map 'key50))
    
    (benchmark:defbenchmark map-contains-medium-100 ()
      (map:contains-key-p medium-map 'key50))
    
    (benchmark:defbenchmark map-seq-medium-100 ()
      (map:seq medium-map))
    
    (benchmark:defbenchmark map-keys-medium-100 ()
      (map:keys medium-map))
    
    (benchmark:defbenchmark map-count-medium-100 ()
      (map:count medium-map)))
  
  ;; Large map operations (1000 entries)
  (let ((large-map (loop with m = map:+empty+
                        for i from 0 below 1000
                        do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                        finally (return m))))
    
    (benchmark:defbenchmark map-assoc-large-1000 ()
      (map:assoc large-map :new-key "new-value"))
    
    (benchmark:defbenchmark map-get-large-1000 ()
      (map:get large-map 'key500))
    
    (benchmark:defbenchmark map-dissoc-large-1000 ()
      (map:dissoc large-map 'key500))
    
    (benchmark:defbenchmark map-contains-large-1000 ()
      (map:contains-key-p large-map 'key500))
    
    (benchmark:defbenchmark map-seq-large-1000 ()
      (map:seq large-map))
    
    (benchmark:defbenchmark map-keys-large-1000 ()
      (map:keys large-map))
    
    (benchmark:defbenchmark map-count-large-1000 ()
      (map:count large-map)))
  
  ;; Very large map operations (10000 entries)
  (let ((xl-map (loop with m = map:+empty+
                     for i from 0 below 10000
                     do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                     finally (return m))))
    
    (benchmark:defbenchmark map-assoc-xl-10000 ()
      (map:assoc xl-map :new-key "new-value"))
    
    (benchmark:defbenchmark map-get-xl-10000 ()
      (map:get xl-map 'key5000))
    
    (benchmark:defbenchmark map-dissoc-xl-10000 ()
      (map:dissoc xl-map 'key5000))
    
    (benchmark:defbenchmark map-contains-xl-10000 ()
      (map:contains-key-p xl-map 'key5000)))
  
  ;; Map construction benchmarks
  (benchmark:defbenchmark map-build-small-10 ()
    (loop with m = map:+empty+
          for i from 0 below 10
          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
          finally (return m)))
  
  (benchmark:defbenchmark map-build-medium-100 ()
    (loop with m = map:+empty+
          for i from 0 below 100
          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
          finally (return m)))
  
  (benchmark:defbenchmark map-build-large-1000 ()
    (loop with m = map:+empty+
          for i from 0 below 1000
          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
          finally (return m)))
  
  ;; Map merge benchmarks
  (let ((map1 (loop with m = map:+empty+
                   for i from 0 below 50
                   do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                   finally (return m)))
        (map2 (loop with m = map:+empty+
                   for i from 50 below 100
                   do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                   finally (return m))))
    
    (benchmark:defbenchmark map-merge-50-50 ()
      (map:merge map1 map2)))
  
  ;; Nested map operations
  (let ((nested-map (map:assoc-in map:+empty+ '(:level1 :level2 :level3) "deep-value")))
    
    (benchmark:defbenchmark map-get-in-nested ()
      (map:get-in nested-map '(:level1 :level2 :level3)))
    
    (benchmark:defbenchmark map-assoc-in-nested ()
      (map:assoc-in nested-map '(:level1 :level2 :level4) "another-deep-value"))))

;;; HAMT Set Benchmarks

(defun register-set-benchmarks ()
  "Register HAMT set benchmarks"
  
  ;; Basic set operations
  (benchmark:defbenchmark set-add-single ()
    (set:add set:+empty+ :element))
  
  (benchmark:defbenchmark set-contains-single ()
    (let ((s (set:add set:+empty+ :element)))
      (set:contains-p s :element)))
  
  (benchmark:defbenchmark set-disj-single ()
    (let ((s (set:add set:+empty+ :element)))
      (set:disj s :element)))
  
  ;; Small set operations (10 elements)
  (let ((small-set (loop with s = set:+empty+
                        for i from 0 below 10
                        do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                        finally (return s))))
    
    (benchmark:defbenchmark set-add-small-10 ()
      (set:add small-set :new-element))
    
    (benchmark:defbenchmark set-contains-small-10 ()
      (set:contains-p small-set 'elem5))
    
    (benchmark:defbenchmark set-disj-small-10 ()
      (set:disj small-set 'elem5))
    
    (benchmark:defbenchmark set-count-small-10 ()
      (set:count small-set)))
  
  ;; Medium set operations (100 elements)
  (let ((medium-set (loop with s = set:+empty+
                         for i from 0 below 100
                         do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                         finally (return s))))
    
    (benchmark:defbenchmark set-add-medium-100 ()
      (set:add medium-set :new-element))
    
    (benchmark:defbenchmark set-contains-medium-100 ()
      (set:contains-p medium-set 'elem50))
    
    (benchmark:defbenchmark set-disj-medium-100 ()
      (set:disj medium-set 'elem50))
    
    (benchmark:defbenchmark set-seq-medium-100 ()
      (set:seq medium-set))
    
    (benchmark:defbenchmark set-count-medium-100 ()
      (set:count medium-set)))
  
  ;; Large set operations (1000 elements)
  (let ((large-set (loop with s = set:+empty+
                        for i from 0 below 1000
                        do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                        finally (return s))))
    
    (benchmark:defbenchmark set-add-large-1000 ()
      (set:add large-set :new-element))
    
    (benchmark:defbenchmark set-contains-large-1000 ()
      (set:contains-p large-set 'elem500))
    
    (benchmark:defbenchmark set-disj-large-1000 ()
      (set:disj large-set 'elem500))
    
    (benchmark:defbenchmark set-seq-large-1000 ()
      (set:seq large-set))
    
    (benchmark:defbenchmark set-count-large-1000 ()
      (set:count large-set)))
  
  ;; Very large set operations (10000 elements)
  (let ((xl-set (loop with s = set:+empty+
                     for i from 0 below 10000
                     do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                     finally (return s))))
    
    (benchmark:defbenchmark set-add-xl-10000 ()
      (set:add xl-set :new-element))
    
    (benchmark:defbenchmark set-contains-xl-10000 ()
      (set:contains-p xl-set 'elem5000))
    
    (benchmark:defbenchmark set-disj-xl-10000 ()
      (set:disj xl-set 'elem5000)))
  
  ;; Set construction benchmarks
  (benchmark:defbenchmark set-build-small-10 ()
    (loop with s = set:+empty+
          for i from 0 below 10
          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
          finally (return s)))
  
  (benchmark:defbenchmark set-build-medium-100 ()
    (loop with s = set:+empty+
          for i from 0 below 100
          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
          finally (return s)))
  
  (benchmark:defbenchmark set-build-large-1000 ()
    (loop with s = set:+empty+
          for i from 0 below 1000
          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
          finally (return s)))
  
  ;; Set operations benchmarks
  (let ((set1 (loop with s = set:+empty+
                   for i from 0 below 50
                   do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                   finally (return s)))
        (set2 (loop with s = set:+empty+
                   for i from 25 below 75
                   do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                   finally (return s))))
    
    (benchmark:defbenchmark set-union-50-50 ()
      (set:union set1 set2))
    
    (benchmark:defbenchmark set-intersection-50-50 ()
      (set:intersection set1 set2))
    
    (benchmark:defbenchmark set-difference-50-50 ()
      (set:difference set1 set2))))

;;; Hash function benchmarks (critical for HAMT performance)

(defun register-hash-benchmarks ()
  "Register hash function benchmarks"
  
  ;; String hashing
  (let ((short-string "hello")
        (medium-string (make-string 100 :initial-element #\a))
        (long-string (make-string 1000 :initial-element #\b)))
    
    (benchmark:defbenchmark hash-string-short ()
      (sxhash short-string))
    
    (benchmark:defbenchmark hash-string-medium ()
      (sxhash medium-string))
    
    (benchmark:defbenchmark hash-string-long ()
      (sxhash long-string)))
  
  ;; Symbol hashing
  (benchmark:defbenchmark hash-symbol ()
    (sxhash 'some-symbol))
  
  (benchmark:defbenchmark hash-keyword ()
    (sxhash :some-keyword))
  
  ;; Number hashing
  (benchmark:defbenchmark hash-fixnum ()
    (sxhash 12345))
  
  (benchmark:defbenchmark hash-bignum ()
    (sxhash 123456789012345678901234567890))
  
  (benchmark:defbenchmark hash-float ()
    (sxhash 3.14159))
  
  ;; List hashing (for composite keys)
  (let ((short-list '(1 2 3))
        (medium-list (loop for i from 0 below 50 collect i))
        (nested-list '((a b) (c d) (e f))))
    
    (benchmark:defbenchmark hash-list-short ()
      (sxhash short-list))
    
    (benchmark:defbenchmark hash-list-medium ()
      (sxhash medium-list))
    
    (benchmark:defbenchmark hash-list-nested ()
      (sxhash nested-list))))

;;; Memory efficiency benchmarks

(defun register-memory-benchmarks ()
  "Register memory usage and structural sharing benchmarks"
  
  ;; Test structural sharing in maps
  (benchmark:defbenchmark map-structural-sharing ()
    (let ((base-map (loop with m = map:+empty+
                         for i from 0 below 1000
                         do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                         finally (return m))))
      ;; Create many derived maps - should share structure
      (loop for i from 1000 below 1100
            collect (map:assoc base-map (intern (format nil "KEY~D" i)) i))))
  
  ;; Test structural sharing in sets
  (benchmark:defbenchmark set-structural-sharing ()
    (let ((base-set (loop with s = set:+empty+
                         for i from 0 below 1000
                         do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                         finally (return s))))
      ;; Create many derived sets - should share structure
      (loop for i from 1000 below 1100
            collect (set:add base-set (intern (format nil "ELEM~D" i))))))
  
  ;; Test transient operations if available
  (when (fboundp 'map:transient)
    (benchmark:defbenchmark map-transient-build-1000 ()
      (let ((t-map (map:transient map:+empty+)))
        (loop for i from 0 below 1000
              do (setf t-map (map:assoc! t-map (intern (format nil "KEY~D" i)) i)))
        (map:persistent t-map))))
  
  (when (fboundp 'set:transient)
    (benchmark:defbenchmark set-transient-build-1000 ()
      (let ((t-set (set:transient set:+empty+)))
        (loop for i from 0 below 1000
              do (setf t-set (set:add! t-set (intern (format nil "ELEM~D" i)))))
        (set:persistent t-set)))))

;;; Comparative benchmarks against built-in structures

(defun register-comparative-benchmarks ()
  "Register benchmarks comparing HAMT vs built-in hash tables and lists"
  
  ;; Map vs Hash Table
  (let ((hash-table (make-hash-table :test 'equal)))
    (loop for i from 0 below 1000
          do (setf (gethash (format nil "KEY~D" i) hash-table) i))
    
    (benchmark:defbenchmark hash-table-get-1000 ()
      (gethash "KEY500" hash-table))
    
    (benchmark:defbenchmark hash-table-put-1000 ()
      (setf (gethash "NEW-KEY" hash-table) "new-value")))
  
  ;; Set vs List (for membership testing)
  (let ((test-list (loop for i from 0 below 100 collect (intern (format nil "ELEM~D" i)))))
    
    (benchmark:defbenchmark list-member-100 ()
      (member 'elem50 test-list)))
  
  ;; Map construction vs hash table construction
  (benchmark:defbenchmark hash-table-build-100 ()
    (let ((ht (make-hash-table :test 'equal)))
      (loop for i from 0 below 100
            do (setf (gethash (format nil "KEY~D" i) ht) i))
      ht)))

;;; Main registration function

(defun register-functional-data-benchmarks ()
  "Register all functional data structure benchmarks"
  (register-map-benchmarks)
  (register-set-benchmarks) 
  (register-hash-benchmarks)
  (register-memory-benchmarks)
  (register-comparative-benchmarks)
  
  ;; Register the functional data suite
  (suites:register-suite 'functional-data-structures
                        :description "HAMT-based functional data structures (maps, sets)"
                        :benchmarks '(;; Map operations
                                     map-assoc-single
                                     map-get-single
                                     map-dissoc-single
                                     map-assoc-small-10
                                     map-get-small-10
                                     map-contains-small-10
                                     map-assoc-medium-100
                                     map-get-medium-100
                                     map-contains-medium-100
                                     map-count-medium-100
                                     map-assoc-large-1000
                                     map-get-large-1000
                                     map-contains-large-1000
                                     map-assoc-xl-10000
                                     map-get-xl-10000
                                     map-contains-xl-10000
                                     ;; Map construction
                                     map-build-small-10
                                     map-build-medium-100
                                     map-build-large-1000
                                     ;; Map operations
                                     map-merge-50-50
                                     map-get-in-nested
                                     map-assoc-in-nested
                                     ;; Set operations
                                     set-add-single
                                     set-contains-single
                                     set-add-small-10
                                     set-contains-small-10
                                     set-add-medium-100
                                     set-contains-medium-100
                                     set-count-medium-100
                                     set-add-large-1000
                                     set-contains-large-1000
                                     set-add-xl-10000
                                     set-contains-xl-10000
                                     ;; Set construction
                                     set-build-small-10
                                     set-build-medium-100
                                     set-build-large-1000
                                     ;; Set operations
                                     set-union-50-50
                                     set-intersection-50-50
                                     set-difference-50-50
                                     ;; Hash functions
                                     hash-string-short
                                     hash-string-medium
                                     hash-symbol
                                     hash-fixnum
                                     hash-list-short
                                     hash-list-medium
                                     ;; Memory efficiency
                                     map-structural-sharing
                                     set-structural-sharing
                                     ;; Comparisons
                                     hash-table-get-1000
                                     list-member-100
                                     hash-table-build-100)))