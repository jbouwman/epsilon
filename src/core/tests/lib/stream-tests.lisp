(defpackage :epsilon.stream-tests
  (:use
   :cl
   :epsilon.test
   :epsilon.syntax)
  (:local-nicknames
   (:array :epsilon.array)
   (:stream :epsilon.stream)))

(in-package :epsilon.stream-tests)

(define-constant +hello->u8+
  #(104 101 108 108 111 32 119 111 114 108 100))

(deftest binary-stream
  (let ((s (stream:make-output-stream)))
    (loop :for c :across "hello world"
          :do (write-byte (char-code c) s))
    (is (array:array-elts-eql
         +hello->u8+
         (stream:buffer s)))))
