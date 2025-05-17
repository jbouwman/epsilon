(defpackage #:epsilon.lib.reader-tests
  (:use
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:stream #:epsilon.lib.stream)
   (#:reader #:epsilon.lib.reader)
   (#:writer #:epsilon.lib.writer)
   (#:char #:epsilon.lib.char)))

(in-package #:epsilon.lib.reader-tests)

;; Helper functions for testing

(defun test-roundtrip (string &key (encoding :utf-8))
  "Test if a string can be written and read back with the same encoding."
  (let* ((enc (char:make-encoding encoding))
         (in-buffer (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write the string to the output stream
    (writer:write-string writer string)
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (result (reader:read-string reader)))
      
      ;; Compare the original string with the result
      (values (string= string result) string result))))

(deftest reader-writer-ascii ()
  "Test reader and writer with ASCII encoding."
  (multiple-value-bind (match original result)
      (test-roundtrip "Hello, world!" :encoding :ascii)
    (is match)))

(deftest reader-writer-utf8-simple ()
  "Test reader and writer with UTF-8 encoding (ASCII characters)."
  (multiple-value-bind (match original result)
      (test-roundtrip "Hello, world!" :encoding :utf-8)
    (is match)))

(deftest reader-writer-utf8-extended ()
  "Test reader and writer with UTF-8 encoding (non-ASCII characters)."
  (multiple-value-bind (match original result)
      (test-roundtrip "こんにちは世界! ñ ö ü ß Привет мир!" :encoding :utf-8)
    (is match)))

(deftest reader-writer-utf8-emojis ()
  "Test reader and writer with UTF-8 encoding (including emojis)."
  (multiple-value-bind (match original result)
      (test-roundtrip "Hello 🌍 world! 😊 👋" :encoding :utf-8)
    (is match)))

(deftest reader-writer-line-by-line ()
  "Test reading and writing line by line."
  (let* ((enc (char:make-encoding :utf-8))
         (original-text "Line 1
Line 2
Line 3")
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write the string to the output stream
    (writer:write-string writer original-text)
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (lines nil))
      
      ;; Read character by character to collect lines
      (let ((current-line (make-string 0))
            (char nil))
        (loop
          (setf char (reader:read-char reader))
          (unless char (progn (push current-line lines) (return)))
          
          (if (char= char #\Newline)
              (progn 
                (push current-line lines)
                (setf current-line (make-string 0)))
              (setf current-line (concatenate 'string current-line (string char))))))
      
      ;; Compare the original lines with the result
      (setf lines (reverse lines))
      (is (= (length lines) 3))
      (is (string= (first lines) "Line 1"))
      (is (string= (second lines) "Line 2"))
      (is (string= (third lines) "Line 3")))))

(deftest reader-char-by-char ()
  "Test reading a string character by character."
  (let* ((enc (char:make-encoding :utf-8))
         (original-text "Hello, 世界!")
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write the string to the output stream
    (writer:write-string writer original-text)
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (result (make-string 0)))
      
      ;; Read each character individually
      (loop
        (let ((char (reader:read-char reader)))
          (unless char (return))
          (setf result (concatenate 'string result (string char)))))
      
      ;; Compare the original string with the result
      (is (string= original-text result)))))

(deftest reader-partial-reads ()
  "Test reading a string in chunks of specified lengths."
  (let* ((enc (char:make-encoding :utf-8))
         (original-text "Hello, 世界! This is a test.")
         (output-stream (stream:make-output-stream))
         (writer (writer:make-writer output-stream :encoding enc)))
    
    ;; Write the string to the output stream
    (writer:write-string writer original-text)
    
    ;; Get the bytes from the output stream
    (let* ((bytes (stream:buffer output-stream))
           (input-stream (stream:make-input-stream bytes))
           (reader (reader:make-reader input-stream :encoding enc))
           (chunk1 (reader:read-string reader 7))  ;; "Hello, "
           (chunk2 (reader:read-string reader 3))  ;; "世界"
           (chunk3 (reader:read-string reader)))   ;; "! This is a test."
      
      ;; Compare the chunks with expected results
      (is (string= chunk1 "Hello, "))
      (is (string= chunk2 "世界"))
      (is (string= chunk3 "! This is a test."))
      (is (string= (concatenate 'string chunk1 chunk2 chunk3) original-text)))))

(deftest reader-writer-ascii-special-chars ()
  "Test reader and writer with ASCII special characters."
  (multiple-value-bind (match original result)
      (test-roundtrip "Tab: 	 Newline:
 Carriage Return: 
 Backspace:  Special: !@#$%^&*()" :encoding :ascii)
    (is match)))
