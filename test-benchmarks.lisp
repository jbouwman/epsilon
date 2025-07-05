#!/usr/bin/env sbcl --script

(load "module/core/src/tool/boot.lisp")
(epsilon.tool.boot:boot)

;; Load example benchmarks
(load "module/core/tests/tool/example-benchmarks.lisp")

;; Test the benchmark functionality
(in-package :epsilon.tool.benchmark.examples)

;; Run a simple benchmark
(format t "=== Single Benchmark Example ===~%")
(let ((result (bench:run-benchmark 
               (bench:get-benchmark 'arithmetic)
               :name "Arithmetic Test")))
  (bench:format-benchmark-result result))

;; Compare two benchmarks
(format t "~%=== Benchmark Comparison Example ===~%")
(let* ((fast (bench:run-benchmark 
              (bench:get-benchmark 'arithmetic)
              :name "Fast Operation"))
       (slow (bench:run-benchmark 
              (bench:get-benchmark 'slow-operation)
              :name "Slow Operation"))
       (comparison (bench:compare-benchmarks fast slow)))
  
  (bench:format-benchmark-result fast)
  (bench:format-benchmark-result slow)
  (format t "~%")
  (bench:format-comparison comparison))

(format t "~%=== Available Benchmarks ===~%")
(dolist (name (bench:list-benchmarks))
  (format t "  ~A~%" name))