(defpackage epsilon.tool.benchmark.tests
  (:use
   cl
   epsilon.tool.test)
  (:local-nicknames 
   (bench epsilon.tool.benchmark)))

(in-package epsilon.tool.benchmark.tests)

(deftest basic-benchmark ()
  "Test basic benchmark functionality"
  (let ((result (bench:run-benchmark
                 (lambda () (+ 1 2 3))
                 :name "simple-addition"
                 :min-time 0.1)))
    
    (is (bench:benchmark-result-p result))
    (is-equal "simple-addition" (bench:benchmark-result-name result))
    (is (> (bench:benchmark-result-iterations result) 0))
    (is (> (bench:benchmark-result-ops-per-sec result) 0))
    (is (> (bench:benchmark-result-time result) 0))))

(deftest benchmark-comparison ()
  "Test benchmark comparison functionality"
  (let* ((fast (bench:run-benchmark 
                (lambda () (+ 1 1))
                :name "fast" :min-time 0.1))
         (slow (bench:run-benchmark 
                (lambda () (loop repeat 100 do (+ 1 1)))
                :name "slow" :min-time 0.1))
         (comparison (bench:compare-benchmarks fast slow)))
    
    (is (listp comparison))
    (is (member :fastest comparison))
    (is (member :comparisons comparison))))

(deftest defbenchmark-macro ()
  "Test defbenchmark macro"
  (bench:defbenchmark test-macro-bench ()
    (+ 1 2 3 4 5))
  
  (let ((fn (bench:get-benchmark 'test-macro-bench)))
    (is (functionp fn))))
