(defpackage #:epsilon.lib.s!tream-2.tests
  (:use
   #:cl
   #:epsilon.tool.test
   #:epsilon.lib.binding)
  (:local-nicknames
   (#:stream-2 #:epsilon.lib.stream-2)))

(in-package #:epsilon.lib.stream.tests)

(define-constant +hello->u8+
  #(104 101 108 108 111 32 119 111 114 108 100))

(deftest binary-stream ()
  (let ((s (stream-2:make-binary-stream)))
    (loop :for c :across "hello world"
          :do (write-byte (char-code c) s))
    (is (epsilon.lib.array:array-elts-eql
         +hello->u8+
         (stream-2:buffer s)))))
