;;;; HAMT Performance Benchmarks
;;;;
;;;; Critical benchmarks for Hash Array Mapped Trie data structures

(defpackage epsilon.core.benchmarks.hamt
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (set epsilon.set))
  (:export
   ;; Benchmark functions for external use
   benchmark-map-operations
   benchmark-set-operations
   benchmark-construction
   benchmark-comparative
   run-all-hamt-benchmarks
   
   ;; For integration with benchmark framework when available
   register-hamt-benchmarks-with-framework))

(in-package epsilon.core.benchmarks.hamt)

;;; Timing utilities

(defun time-operation (fn &optional (iterations 1000))
  "Time an operation and return microseconds per operation"
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall fn))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (per-op (* (/ elapsed iterations) 1000000)))
      per-op)))

(defun format-time (microseconds)
  "Format time in appropriate units"
  (cond
    ((< microseconds 1) (format nil "~,2F ns" (* microseconds 1000)))
    ((< microseconds 1000) (format nil "~,2F µs" microseconds))
    ((< microseconds 1000000) (format nil "~,2F ms" (/ microseconds 1000)))
    (t (format nil "~,2F s" (/ microseconds 1000000)))))

(defun run-and-report (name fn &optional (iterations 1000))
  "Run benchmark and report results"
  (let ((time (time-operation fn iterations)))
    (format t "~30A: ~A~%" name (format-time time))
    time))

;;; HAMT Map Benchmarks

(defun benchmark-map-operations ()
  "Benchmark core HAMT map operations"
  (format t "~%=== HAMT Map Operations ===~%")
  
  ;; Single element operations
  (run-and-report "map-assoc (single)" 
                  (lambda () (map:assoc map:+empty+ :key "value")))
  
  (let ((single-map (map:assoc map:+empty+ :key "value")))
    (run-and-report "map-get (single)"
                    (lambda () (map:get single-map :key))))
  
  ;; Small map (10 elements)
  (let ((small-map (loop with m = map:+empty+
                        for i from 0 below 10
                        do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                        finally (return m))))
    
    (run-and-report "map-assoc (10 elements)"
                    (lambda () (map:assoc small-map :new-key "new")))
    
    (run-and-report "map-get (10 elements)"
                    (lambda () (map:get small-map 'key5)))
    
    (run-and-report "map-contains-p (10 elements)"
                    (lambda () (map:contains-key-p small-map 'key5))))
  
  ;; Medium map (100 elements)
  (let ((medium-map (loop with m = map:+empty+
                         for i from 0 below 100
                         do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                         finally (return m))))
    
    (run-and-report "map-assoc (100 elements)"
                    (lambda () (map:assoc medium-map :new-key "new")))
    
    (run-and-report "map-get (100 elements)"
                    (lambda () (map:get medium-map 'key50)))
    
    (run-and-report "map-contains-p (100 elements)"
                    (lambda () (map:contains-key-p medium-map 'key50)))
    
    (run-and-report "map-count (100 elements)"
                    (lambda () (map:count medium-map))))
  
  ;; Large map (1000 elements)
  (let ((large-map (loop with m = map:+empty+
                        for i from 0 below 1000
                        do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                        finally (return m))))
    
    (run-and-report "map-assoc (1000 elements)"
                    (lambda () (map:assoc large-map :new-key "new")))
    
    (run-and-report "map-get (1000 elements)"
                    (lambda () (map:get large-map 'key500)))
    
    (run-and-report "map-contains-p (1000 elements)"
                    (lambda () (map:contains-key-p large-map 'key500))))
  
  ;; Extra large map (10000 elements) - most critical test
  (let ((xl-map (loop with m = map:+empty+
                     for i from 0 below 10000
                     do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                     finally (return m))))
    
    (run-and-report "map-assoc (10K elements)"
                    (lambda () (map:assoc xl-map :new-key "new")))
    
    (run-and-report "map-get (10K elements)"
                    (lambda () (map:get xl-map 'key5000)))
    
    (run-and-report "map-contains-p (10K elements)"
                    (lambda () (map:contains-key-p xl-map 'key5000)))))

(defun benchmark-set-operations ()
  "Benchmark core HAMT set operations"
  (format t "~%=== HAMT Set Operations ===~%")
  
  ;; Single element operations
  (run-and-report "set-add (single)"
                  (lambda () (set:add set:+empty+ :element)))
  
  (let ((single-set (set:add set:+empty+ :element)))
    (run-and-report "set-contains-p (single)"
                    (lambda () (set:contains-p single-set :element))))
  
  ;; Small set (10 elements)
  (let ((small-set (loop with s = set:+empty+
                        for i from 0 below 10
                        do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                        finally (return s))))
    
    (run-and-report "set-add (10 elements)"
                    (lambda () (set:add small-set :new-element)))
    
    (run-and-report "set-contains-p (10 elements)"
                    (lambda () (set:contains-p small-set 'elem5))))
  
  ;; Medium set (100 elements)  
  (let ((medium-set (loop with s = set:+empty+
                         for i from 0 below 100
                         do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                         finally (return s))))
    
    (run-and-report "set-add (100 elements)"
                    (lambda () (set:add medium-set :new-element)))
    
    (run-and-report "set-contains-p (100 elements)"
                    (lambda () (set:contains-p medium-set 'elem50)))
    
    (run-and-report "set-count (100 elements)"
                    (lambda () (set:count medium-set))))
  
  ;; Large set (1000 elements)
  (let ((large-set (loop with s = set:+empty+
                        for i from 0 below 1000
                        do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                        finally (return s))))
    
    (run-and-report "set-add (1000 elements)"
                    (lambda () (set:add large-set :new-element)))
    
    (run-and-report "set-contains-p (1000 elements)"
                    (lambda () (set:contains-p large-set 'elem500))))
  
  ;; Extra large set (10000 elements)
  (let ((xl-set (loop with s = set:+empty+
                     for i from 0 below 10000
                     do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                     finally (return s))))
    
    (run-and-report "set-add (10K elements)"
                    (lambda () (set:add xl-set :new-element)))
    
    (run-and-report "set-contains-p (10K elements)"
                    (lambda () (set:contains-p xl-set 'elem5000)))))

(defun benchmark-construction ()
  "Benchmark HAMT construction performance"
  (format t "~%=== HAMT Construction Performance ===~%")
  
  (run-and-report "map-build (100 elements)"
                  (lambda () 
                    (loop with m = map:+empty+
                          for i from 0 below 100
                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                          finally (return m)))
                  10)
  
  (run-and-report "map-build (1000 elements)"
                  (lambda () 
                    (loop with m = map:+empty+
                          for i from 0 below 1000
                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                          finally (return m)))
                  1)
  
  (run-and-report "set-build (100 elements)"
                  (lambda () 
                    (loop with s = set:+empty+
                          for i from 0 below 100
                          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                          finally (return s)))
                  10)
  
  (run-and-report "set-build (1000 elements)"
                  (lambda () 
                    (loop with s = set:+empty+
                          for i from 0 below 1000
                          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                          finally (return s)))
                  1)
  
  ;; Test set operations
  (let ((set1 (loop with s = set:+empty+
                   for i from 0 below 100
                   do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                   finally (return s)))
        (set2 (loop with s = set:+empty+
                   for i from 50 below 150
                   do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                   finally (return s))))
    
    (run-and-report "set-union (100+100 elements)"
                    (lambda () (set:union set1 set2))
                    10)
    
    (run-and-report "set-intersection (100+100 elements)" 
                    (lambda () (set:intersection set1 set2))
                    10)
    
    (run-and-report "set-difference (100+100 elements)"
                    (lambda () (set:difference set1 set2))
                    10)))

(defun benchmark-comparative ()
  "Compare HAMT performance vs built-in data structures"
  (format t "~%=== HAMT vs Built-in Structures ===~%")
  
  ;; Map vs Hash Table for lookups
  (let ((hamt-map (loop with m = map:+empty+
                       for i from 0 below 1000
                       do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                       finally (return m)))
        (hash-table (make-hash-table :test 'eq)))
    
    (loop for i from 0 below 1000
          do (setf (gethash (intern (format nil "KEY~D" i)) hash-table) i))
    
    (run-and-report "HAMT map-get (1000 elements)"
                    (lambda () (map:get hamt-map 'key500)))
    
    (run-and-report "Hash table gethash (1000 elements)"
                    (lambda () (gethash 'key500 hash-table))))
  
  ;; Set vs List for membership
  (let ((hamt-set (loop with s = set:+empty+
                       for i from 0 below 100
                       do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                       finally (return s)))
        (test-list (loop for i from 0 below 100 
                        collect (intern (format nil "ELEM~D" i)))))
    
    (run-and-report "HAMT set-contains-p (100 elements)"
                    (lambda () (set:contains-p hamt-set 'elem50)))
    
    (run-and-report "List member (100 elements)"
                    (lambda () (member 'elem50 test-list))))
  
  ;; Construction comparison
  (run-and-report "HAMT map construction (100 elements)"
                  (lambda () 
                    (loop with m = map:+empty+
                          for i from 0 below 100
                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                          finally (return m)))
                  10)
  
  (run-and-report "Hash table construction (100 elements)"
                  (lambda () 
                    (let ((ht (make-hash-table :test 'eq)))
                      (loop for i from 0 below 100
                            do (setf (gethash (intern (format nil "KEY~D" i)) ht) i))
                      ht))
                  10))

(defun run-all-hamt-benchmarks ()
  "Run all HAMT benchmarks"
  (format t "~%========================================~%")
  (format t "    HAMT Performance Benchmarks~%")  
  (format t "========================================~%")
  
  (benchmark-map-operations)
  (benchmark-set-operations) 
  (benchmark-construction)
  (benchmark-comparative)
  
  (format t "~%Benchmark complete.~%"))

;;; Integration with benchmark framework (when available)

(defun register-hamt-benchmarks-with-framework ()
  "Register HAMT benchmarks with the formal benchmark framework"
  (let ((benchmark-pkg (find-package "EPSILON.TOOL.BENCHMARK"))
        (suites-pkg (find-package "EPSILON.TOOL.BENCHMARK.SUITES")))
    
    (when (and benchmark-pkg suites-pkg)
      (let ((defbenchmark (find-symbol "DEFBENCHMARK" benchmark-pkg))
            (register-suite (find-symbol "REGISTER-SUITE" suites-pkg)))
        
        (when (and defbenchmark register-suite)
          ;; Define key benchmarks for the framework
          (funcall defbenchmark 'hamt-map-get-1000 '()
                   '(let ((large-map (loop with m = map:+empty+
                                          for i from 0 below 1000
                                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                                          finally (return m))))
                      (map:get large-map 'key500)))
          
          (funcall defbenchmark 'hamt-map-assoc-1000 '()
                   '(let ((large-map (loop with m = map:+empty+
                                          for i from 0 below 1000
                                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                                          finally (return m))))
                      (map:assoc large-map :new-key "new")))
          
          (funcall defbenchmark 'hamt-set-contains-1000 '()
                   '(let ((large-set (loop with s = set:+empty+
                                          for i from 0 below 1000
                                          do (setf s (set:add s (intern (format nil "ELEM~D" i))))
                                          finally (return s))))
                      (set:contains-p large-set 'elem500)))
          
          (funcall defbenchmark 'hamt-map-build-100 '()
                   '(loop with m = map:+empty+
                          for i from 0 below 100
                          do (setf m (map:assoc m (intern (format nil "KEY~D" i)) i))
                          finally (return m)))
          
          ;; Register the suite
          (funcall register-suite 'hamt-performance
                   :description "Hash Array Mapped Trie (HAMT) performance benchmarks"
                   :benchmarks '(hamt-map-get-1000
                                hamt-map-assoc-1000
                                hamt-set-contains-1000
                                hamt-map-build-100)))))))

;; Entry point for standalone use
(defun benchmark ()
  "Entry point for standalone HAMT benchmarking"
  (run-all-hamt-benchmarks))