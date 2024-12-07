(epsilon.tool.test:define-test-package #:epsilon.lib.stream/tests
  (:use
   #:epsilon.lib.binding)
  (:local-nicknames
   (#:stream #:epsilon.lib.stream)))

(in-package #:epsilon.lib.stream/tests)

(define-constant +hello->u8+
  #(104 101 108 108 111 32 119 111 114 108 100))

(deftest vector-stream ()
  (let ((s (make-instance 'stream:fast-output-stream)))
    (loop :for c :across "hello world"
          :do (write-byte (char-code c) s))
    (is (epsilon.lib.array:array-elts-eql
         +hello->u8+
         (stream:finish-output-stream s)))))
