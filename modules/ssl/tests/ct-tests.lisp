;;;; Tests for constant-time primitives

(defpackage epsilon.ssl.ct-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:ct #:epsilon.ssl.ct))
  (:enter t))

(in-package :epsilon.ssl.ct-tests)

;;; ---------------------------------------------------------------------------
;;; ct-equal tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-equal-identical
  "Equal byte arrays should return T"
  (let ((a (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (assert-true (ct:ct-equal a b))))

(deftest test-ct-equal-different
  "Different byte arrays should return NIL"
  (let ((a (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4)))
        (b (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 5))))
    (assert-not (ct:ct-equal a b))))

(deftest test-ct-equal-different-lengths
  "Arrays of different lengths should return NIL"
  (let ((a (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
        (b (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))
    (assert-not (ct:ct-equal a b))))

(deftest test-ct-equal-empty
  "Two empty arrays should be equal"
  (let ((a (make-array 0 :element-type '(unsigned-byte 8)))
        (b (make-array 0 :element-type '(unsigned-byte 8))))
    (assert-true (ct:ct-equal a b))))

(deftest test-ct-equal-single-bit-diff
  "Single bit difference should be detected"
  (let ((a (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xFF)))
        (b (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xFE))))
    (assert-not (ct:ct-equal a b))))

(deftest test-ct-equal-with-ranges
  "ct-equal with start/end parameters"
  (let ((a (make-array 6 :element-type '(unsigned-byte 8)
                         :initial-contents '(0 0 1 2 3 0)))
        (b (make-array 4 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3 4))))
    ;; a[2..5) = (1 2 3) vs b[0..3) = (1 2 3) -> equal
    (assert-true (ct:ct-equal a b :start1 2 :end1 5 :start2 0 :end2 3))))

;;; ---------------------------------------------------------------------------
;;; ct-zerop tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-zerop-all-zeros
  "All-zero buffer should return T"
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-true (ct:ct-zerop buf))))

(deftest test-ct-zerop-nonzero
  "Buffer with any nonzero byte should return NIL"
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref buf 7) 1)
    (assert-not (ct:ct-zerop buf))))

(deftest test-ct-zerop-empty
  "Empty buffer should be considered all-zeros"
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8))))
    (assert-true (ct:ct-zerop buf))))

;;; ---------------------------------------------------------------------------
;;; ct-select tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-select-true
  "ct-select with true condition should return first value"
  (assert-= (ct:ct-select t 42 99) 42))

(deftest test-ct-select-false
  "ct-select with false condition should return second value"
  (assert-= (ct:ct-select nil 42 99) 99))

(deftest test-ct-select-zero-values
  "ct-select with zero values"
  (assert-= (ct:ct-select t 0 1) 0)
  (assert-= (ct:ct-select nil 0 1) 1))

(deftest test-ct-select-negative
  "ct-select with negative fixnums"
  (assert-= (ct:ct-select t -5 10) -5)
  (assert-= (ct:ct-select nil -5 10) 10))

;;; ---------------------------------------------------------------------------
;;; ct-select-byte tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-select-byte-true
  "ct-select-byte with true condition"
  (assert-= (ct:ct-select-byte t #xAA #x55) #xAA))

(deftest test-ct-select-byte-false
  "ct-select-byte with false condition"
  (assert-= (ct:ct-select-byte nil #xAA #x55) #x55))

;;; ---------------------------------------------------------------------------
;;; ct-swap tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-swap-true
  "ct-swap with true condition should swap"
  (let ((a (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
        (b (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(4 5 6))))
    (ct:ct-swap t a b)
    (assert-true (ct:ct-equal a (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(4 5 6))))
    (assert-true (ct:ct-equal b (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(1 2 3))))))

(deftest test-ct-swap-false
  "ct-swap with false condition should not swap"
  (let ((a (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
        (b (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(4 5 6))))
    (ct:ct-swap nil a b)
    (assert-true (ct:ct-equal a (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(1 2 3))))
    (assert-true (ct:ct-equal b (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(4 5 6))))))

;;; ---------------------------------------------------------------------------
;;; ct-lookup tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-lookup-basic
  "ct-lookup should return the correct value"
  (let ((table (make-array 5 :element-type '(unsigned-byte 8)
                             :initial-contents '(10 20 30 40 50))))
    (assert-= (ct:ct-lookup table 0) 10)
    (assert-= (ct:ct-lookup table 2) 30)
    (assert-= (ct:ct-lookup table 4) 50)))

(deftest test-ct-lookup-all-indices
  "ct-lookup should work for all valid indices"
  (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
    ;; Fill with identity: table[i] = i
    (loop for i from 0 below 256
          do (setf (aref table i) i))
    ;; Verify all lookups
    (loop for i from 0 below 256
          do (assert-= (ct:ct-lookup table i) i))))

;;; ---------------------------------------------------------------------------
;;; ct-copy-if tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-copy-if-true
  "ct-copy-if with true condition should copy"
  (let ((dest (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0))
        (src (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (ct:ct-copy-if t dest src)
    (assert-true (ct:ct-equal dest src))))

(deftest test-ct-copy-if-false
  "ct-copy-if with false condition should not modify dest"
  (let ((dest (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(10 20 30)))
        (src (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
        (expected (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(10 20 30))))
    (ct:ct-copy-if nil dest src)
    (assert-true (ct:ct-equal dest expected))))

;;; ---------------------------------------------------------------------------
;;; ct-zero-memory tests
;;; ---------------------------------------------------------------------------

(deftest test-ct-zero-memory
  "ct-zero-memory should zero the buffer"
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4 5 6 7 8))))
    (ct:ct-zero-memory buf)
    (assert-true (ct:ct-zerop buf))))

(deftest test-ct-zero-memory-range
  "ct-zero-memory with start/end should only zero the range"
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4 5 6 7 8))))
    (ct:ct-zero-memory buf :start 2 :end 6)
    (assert-= (aref buf 0) 1)
    (assert-= (aref buf 1) 2)
    (assert-= (aref buf 2) 0)
    (assert-= (aref buf 5) 0)
    (assert-= (aref buf 6) 7)
    (assert-= (aref buf 7) 8)))

;;; ---------------------------------------------------------------------------
;;; with-secure-buffer tests
;;; ---------------------------------------------------------------------------

(deftest test-with-secure-buffer-zeroed-on-exit
  "with-secure-buffer should zero the buffer after body executes"
  (let ((leaked-buf nil))
    (ct:with-secure-buffer (buf 16)
      (loop for i from 0 below 16
            do (setf (aref buf i) (1+ i)))
      ;; Capture reference to buf
      (setf leaked-buf buf))
    ;; After the macro exits, the buffer should be zeroed
    (assert-true (ct:ct-zerop leaked-buf))))

(deftest test-with-secure-buffer-zeroed-on-error
  "with-secure-buffer should zero the buffer even on error"
  (let ((leaked-buf nil))
    (ignore-errors
      (ct:with-secure-buffer (buf 16)
        (loop for i from 0 below 16
              do (setf (aref buf i) #xFF))
        (setf leaked-buf buf)
        (error "test error")))
    ;; Buffer should still be zeroed despite the error
    (assert-true (ct:ct-zerop leaked-buf))))

(deftest test-with-secure-buffer-returns-body-value
  "with-secure-buffer should return the value of the body"
  (let ((result (ct:with-secure-buffer (buf 4)
                  (setf (aref buf 0) 42)
                  :success)))
    (assert-eq result :success)))

(deftest test-with-secure-buffers-multiple
  "with-secure-buffers should handle multiple buffers"
  (let ((leaked-a nil)
        (leaked-b nil))
    (ct:with-secure-buffers ((a 8) (b 16))
      (loop for i from 0 below 8 do (setf (aref a i) #xAA))
      (loop for i from 0 below 16 do (setf (aref b i) #xBB))
      (setf leaked-a a leaked-b b))
    (assert-true (ct:ct-zerop leaked-a))
    (assert-true (ct:ct-zerop leaked-b))))
