(defpackage #:epsilon.lib.writer-tests
  (:use
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:stream #:epsilon.lib.stream)
   (#:reader #:epsilon.lib.reader)
   (#:writer #:epsilon.lib.writer)
   (#:char #:epsilon.lib.char)))

(in-package #:epsilon.lib.writer-tests)

(deftest writer-char-by-char ()
  "Test writing a string character by character."
  (let* ((enc (char:make-encoding :utf-8))
         (original-text "Hello, 世界!")
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write each character individually
    (loop for char across original-text do
          (writer:write-char writer char))
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (result (reader:read-string reader)))

      ;; Compare the original string with the result
      (is (string= original-text result)))))

(deftest writer-mixed-chars ()
  "Test writing a mix of ASCII and Unicode characters."
  (let* ((enc (char:make-encoding :utf-8))
         (text-parts '("Hello" ", " "世界" "!" "😊"))
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write each part as a separate write operation
    (loop for part in text-parts do
          (writer:write-string writer part))
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (result (reader:read-string reader))
           (expected (format nil "~{~A~}" text-parts)))
      
      ;; Compare the original string with the result
      (is (string= expected result)))))
