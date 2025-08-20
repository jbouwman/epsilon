;;;; BLAKE2 Hash Function Tests for epsilon.crypto
;;;;
;;;; Tests for BLAKE2b and BLAKE2s hash functions

(defpackage :epsilon.crypto.blake2-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:crypto #:epsilon.crypto))
  (:import-from :epsilon.crypto
                #:crypto-error))

(in-package :epsilon.crypto.blake2-tests)

;;;; BLAKE2b Tests

(deftest test-blake2b-basic
  "Test basic BLAKE2b hashing"
  (let* ((data "The quick brown fox jumps over the lazy dog")
         (hash1 (crypto:blake2b data))
         (hash2 (crypto:blake2b data)))
    ;; Same input should produce same hash
    (is (equalp hash1 hash2))
    ;; Default output should be 64 bytes
    (is (= 64 (length hash1)))
    ;; Different data should produce different hash
    (let ((hash3 (crypto:blake2b "Different data")))
      (is (not (equalp hash1 hash3))))))

(deftest test-blake2b-output-lengths
  "Test BLAKE2b with different output lengths"
  (let ((data "Test data"))
    (dolist (length '(1 16 32 48 64))
      (let ((hash (crypto:blake2b data :output-length length)))
        (is (= length (length hash)))))))

(deftest test-blake2b-keyed
  "Test BLAKE2b keyed hashing (MAC mode)"
  (let* ((data "Message to authenticate")
         (key1 (crypto:crypto-random-bytes 32))
         (key2 (crypto:crypto-random-bytes 32))
         (mac1 (crypto:blake2b data :key key1))
         (mac2 (crypto:blake2b data :key key1))
         (mac3 (crypto:blake2b data :key key2)))
    ;; Same key should produce same MAC
    (is (equalp mac1 mac2))
    ;; Different keys should produce different MACs
    (is (not (equalp mac1 mac3)))
    ;; Output should be 64 bytes by default
    (is (= 64 (length mac1)))))

(deftest test-blake2b-empty-input
  "Test BLAKE2b with empty input"
  (let ((hash (crypto:blake2b "")))
    ;; Should produce consistent hash for empty input
    (is (= 64 (length hash)))
    (is (equalp hash (crypto:blake2b "")))))

(deftest test-blake2b-binary-input
  "Test BLAKE2b with binary input"
  (let* ((binary-data (make-array 256 
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents (loop for i from 0 to 255 collect i)))
         (hash (crypto:blake2b binary-data :output-length 32)))
    (is (= 32 (length hash)))
    (is (typep hash '(vector (unsigned-byte 8))))))

;;;; BLAKE2s Tests

(deftest test-blake2s-basic
  "Test basic BLAKE2s hashing"
  (let* ((data "The quick brown fox jumps over the lazy dog")
         (hash1 (crypto:blake2s data))
         (hash2 (crypto:blake2s data)))
    ;; Same input should produce same hash
    (is (equalp hash1 hash2))
    ;; Default output should be 32 bytes
    (is (= 32 (length hash1)))
    ;; Different data should produce different hash
    (let ((hash3 (crypto:blake2s "Different data")))
      (is (not (equalp hash1 hash3))))))

(deftest test-blake2s-output-lengths
  "Test BLAKE2s with different output lengths"
  (let ((data "Test data"))
    (dolist (length '(1 8 16 24 32))
      (let ((hash (crypto:blake2s data :output-length length)))
        (is (= length (length hash)))))))

(deftest test-blake2s-keyed
  "Test BLAKE2s keyed hashing"
  (let* ((data "Message to authenticate")
         (key (crypto:crypto-random-bytes 16))
         (mac1 (crypto:blake2s data :key key))
         (mac2 (crypto:blake2s data :key key)))
    ;; Same key should produce same MAC
    (is (equalp mac1 mac2))
    ;; Output should be 32 bytes by default
    (is (= 32 (length mac1)))))

;;;; Performance Comparison Tests

(deftest benchmark-blake2-vs-sha
  "Benchmark BLAKE2 against SHA-256"
  (let ((data (make-array 1024
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    
    (format t "~%Hash Function Performance Comparison:~%")
    (format t "=====================================~%")
    
    ;; Benchmark BLAKE2b
    (let* ((iterations 1000)
           (start (get-internal-real-time)))
      (dotimes (i iterations)
        (crypto:blake2b data :output-length 32))
      (let ((elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                       internal-time-units-per-second)))
        (format t "BLAKE2b (32 bytes): ~,1Fms for ~D iterations~%" elapsed iterations)))
    
    ;; Benchmark BLAKE2s
    (let* ((iterations 1000)
           (start (get-internal-real-time)))
      (dotimes (i iterations)
        (crypto:blake2s data))
      (let ((elapsed (/ (* (- (get-internal-real-time) start) 1000.0)
                       internal-time-units-per-second)))
        (format t "BLAKE2s (32 bytes): ~,1Fms for ~D iterations~%" elapsed iterations)))
    
    ;; Compare with SHA-256 if available
    ;; (Note: SHA-256 comparison would go here if implemented)
    ))

;;;; Edge Cases and Error Handling

(deftest test-blake2b-invalid-parameters
  "Test BLAKE2b with invalid parameters"
  ;; Output length too large
  (is-thrown (crypto-error)
             (crypto:blake2b "data" :output-length 65))
  ;; Output length of zero
  (is-thrown (crypto-error)
             (crypto:blake2b "data" :output-length 0))
  ;; Key too large
  (let ((large-key (make-array 65 :element-type '(unsigned-byte 8))))
    (is-thrown (crypto-error)
               (crypto:blake2b "data" :key large-key))))

(deftest test-blake2s-invalid-parameters
  "Test BLAKE2s with invalid parameters"
  ;; Output length too large
  (is-thrown (crypto-error)
             (crypto:blake2s "data" :output-length 33))
  ;; Key too large
  (let ((large-key (make-array 33 :element-type '(unsigned-byte 8))))
    (is-thrown (crypto-error)
               (crypto:blake2s "data" :key large-key))))

;;;; Test Runner

(defun run-all-tests ()
  "Run all BLAKE2 tests"
  (format t "Running BLAKE2 Tests~%")
  (format t "===================~%")
  
  ;; BLAKE2b tests
  (format t "~%Testing BLAKE2b...~%")
  (test-blake2b-basic)
  (test-blake2b-output-lengths)
  (test-blake2b-keyed)
  (test-blake2b-empty-input)
  (test-blake2b-binary-input)
  
  ;; BLAKE2s tests
  (format t "~%Testing BLAKE2s...~%")
  (test-blake2s-basic)
  (test-blake2s-output-lengths)
  (test-blake2s-keyed)
  
  ;; Error handling tests
  (format t "~%Testing error handling...~%")
  (test-blake2b-invalid-parameters)
  (test-blake2s-invalid-parameters)
  
  ;; Performance benchmarks
  (format t "~%Running benchmarks...~%")
  (benchmark-blake2-vs-sha)
  
  (format t "~%All BLAKE2 tests completed!~%"))