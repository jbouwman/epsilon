(defpackage epsilon.tool.benchmark.examples
  (:use cl)
  (:local-nicknames 
   (bench epsilon.tool.benchmark)))

(in-package :epsilon.tool.benchmark.examples)

;;;; Example benchmarks for demonstration

(bench:defbenchmark arithmetic ()
  "Simple arithmetic operations"
  (+ 1 2 3 4 5))

(bench:defbenchmark string-concat ()
  "String concatenation"
  (concatenate 'string "hello" " " "world"))

(bench:defbenchmark list-ops ()
  "Basic list operations"
  (length (append '(1 2 3) '(4 5 6))))

(bench:defbenchmark vector-access ()
  "Vector element access"
  (let ((v #(1 2 3 4 5)))
    (aref v 2)))

(bench:defbenchmark slow-operation ()
  "Intentionally slow operation for comparison"
  (loop repeat 1000 sum 1))