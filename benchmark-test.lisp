#!/usr/bin/env sbcl --script

(load "module/core/src/tool/boot.lisp")
(epsilon.tool.boot:boot)

;; Load the benchmark test
(load "module/core/tests/lib/msgpack-binary-benchmark.lisp")

;; Run the benchmark suite
(in-package :epsilon.lib.msgpack.binary.benchmark)
(run-complete-benchmark-suite)