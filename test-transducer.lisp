#!/usr/bin/env sbcl --script

(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

;; Test that transducers are available
(format t "Testing transducer module...~%")

;; Check if the package exists
(if (find-package "EPSILON.LIB.TRANSDUCER")
    (progn
      (format t "✓ epsilon.lib.transducer package found~%")
      
      ;; Test basic transducer functionality
      (let ((result (epsilon.lib.transducer:into 
                     '() 
                     (epsilon.lib.transducer:map #'1+) 
                     '(1 2 3 4 5))))
        (format t "✓ map transducer test: ~A~%" result)
        (assert (equal result '(2 3 4 5 6))))
      
      ;; Test filter transducer
      (let ((result (epsilon.lib.transducer:into 
                     '() 
                     (epsilon.lib.transducer:filter #'evenp) 
                     '(1 2 3 4 5))))
        (format t "✓ filter transducer test: ~A~%" result)
        (assert (equal result '(2 4))))
      
      ;; Test composition
      (let ((result (epsilon.lib.transducer:into 
                     '() 
                     (epsilon.lib.transducer:comp 
                      (epsilon.lib.transducer:filter #'evenp)
                      (epsilon.lib.transducer:map (lambda (x) (* x 2))))
                     '(1 2 3 4 5))))
        (format t "✓ composition test: ~A~%" result)
        (assert (equal result '(4 8))))
      
      (format t "~%All transducer tests passed!~%"))
    (format t "✗ epsilon.lib.transducer package not found~%"))