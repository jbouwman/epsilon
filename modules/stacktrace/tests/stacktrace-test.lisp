;;;; Stack Trace System Test

(defpackage epsilon.stacktrace-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.record record)
            (epsilon.map map))
  (:enter t))

(deftest basic-capture
    (let ((trace (handler-case
                     (error "Test error")
                   (error (c)
                     (epsilon.stacktrace:capture-stack-trace c)))))
      (assert-true (epsilon.stacktrace:stack-trace-p trace))
    (assert-true (> (length (epsilon.stacktrace:stack-trace-frames trace)) 0))
    (assert-true (eq 'simple-error
            (epsilon.stacktrace:stack-trace-condition-type trace)))
    (assert-true (< 0
           (length (epsilon.stacktrace:stack-trace-frames trace))))
    (assert-true (< 0
           (length (epsilon.stacktrace:user-frames trace))))))

(deftest compact-format
    (let ((trace (handler-case
                   (let ((zero (- 1 1))) (/ 1 zero))
                 (error (c)
                   (epsilon.stacktrace:capture-stack-trace c)))))
    (let ((output (epsilon.stacktrace:format-stack-trace trace nil
                    :max-frames 3 :format :compact)))
      (assert-true (stringp output))
      (assert-true (> (length output) 0)))))

(deftest default-format
    (let ((trace (handler-case
                   (error "Source context test")
                 (error (c)
                   (epsilon.stacktrace:capture-stack-trace c)))))
    (let ((output (epsilon.stacktrace:format-stack-trace trace nil
                    :max-frames 2 :show-source t :format :default)))
      (assert-true (stringp output)))))

(deftest json-serialization
    (let* ((trace (handler-case
                   (error "Serialization test")
                 (error (c)
                   (epsilon.stacktrace:capture-stack-trace c))))
         (serialized (epsilon.stacktrace:serialize-stack-trace trace)))
    (assert-true (listp serialized))
    (assert-true (assoc "conditionType" serialized :test #'string=))
    (assert-true (assoc "message" serialized :test #'string=))
    (assert-true (assoc "frames" serialized :test #'string=))))

(deftest internal-frame-filtering
    (let ((trace (handler-case
                     (error "Filter test")
                   (error (c)
                     (epsilon.stacktrace:capture-stack-trace c)))))
      (let ((all-frames (length (epsilon.stacktrace:stack-trace-frames trace)))
            (user-frames (length (epsilon.stacktrace:user-frames trace))))
        (assert-true (>= all-frames user-frames)))))

(deftest colors
    (let ((epsilon.stacktrace:*stack-trace-color* t))
      (let ((output (epsilon.stacktrace:colorize "ERROR" :error)))
        (assert-true (search (string #\Escape) output))))
  (let ((epsilon.stacktrace:*stack-trace-color* nil))
    (let ((output (epsilon.stacktrace:colorize "ERROR" :error)))
      (assert-true (not (search (string #\Escape) output))))))
