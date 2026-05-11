;;;; Tests for ML-KEM sampling primitives (FIPS 203 §4.2.2)

(defpackage epsilon.crypto.ml-kem-sample-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.crypto.ml-kem-ntt ml)
            (epsilon.crypto.ml-kem-sample sample)))

(in-package :epsilon.crypto.ml-kem-sample-tests)

(defun make-seed (byte)
  "Build a 34-byte SampleNTT seed filled with BYTE, with the last two
   bytes (the matrix index) overridden to 0, 0."
  (let ((out (make-array 34 :element-type '(unsigned-byte 8)
                            :initial-element byte)))
    (setf (aref out 32) 0)
    (setf (aref out 33) 0)
    out))

;;; ---- SampleNTT ----

(deftest test-sample-ntt-output-shape
  "SampleNTT returns a 256-element polynomial in u16 form."
  (let ((p (sample:sample-ntt (make-seed 0))))
    (assert-= (length p) ml:+n+)
    (assert-true (typep p '(simple-array (unsigned-byte 16) (256))))))

(deftest test-sample-ntt-all-coefficients-in-range
  "Every coefficient is strictly less than q (the whole point of the
   rejection sampling step is to ensure this)."
  (dotimes (byte 4)
    (let ((p (sample:sample-ntt (make-seed byte))))
      (loop for i from 0 below ml:+n+
            do (assert-true (< (aref p i) ml:+q+))))))

(deftest test-sample-ntt-deterministic
  "Same seed -> same polynomial (pure function of the seed)."
  (let ((a (sample:sample-ntt (make-seed 0)))
        (b (sample:sample-ntt (make-seed 0))))
    (assert-true (equalp a b))))

(deftest test-sample-ntt-different-seeds-differ
  "Different seeds give different polynomials (overwhelming probability)."
  (let ((a (sample:sample-ntt (make-seed 0)))
        (b (sample:sample-ntt (make-seed 1))))
    (assert-not (equalp a b))))

(deftest test-sample-ntt-coverage
  "A SampleNTT output should use a reasonable spread of the Z_q range.
   This is a loose sanity check: across 256 coefficients drawn from a
   uniform-ish distribution on [0, 3329), the average should fall well
   within ±25% of 3329/2 = 1664. The actual test accepts [1200, 2100]."
  (let* ((p (sample:sample-ntt (make-seed 42)))
         (total 0))
    (loop for i from 0 below ml:+n+ do (incf total (aref p i)))
    (let ((avg (floor total ml:+n+)))
      (assert-true (<= 1200 avg 2100)))))

;;; ---- SamplePolyCBD ----

(deftest test-sample-poly-cbd-eta-2-output-shape
  "η=2 consumes 128 bytes and returns a 256-element polynomial."
  (let ((bytes (make-array 128 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 128 do (setf (aref bytes i) i))
    (let ((p (sample:sample-poly-cbd bytes 2)))
      (assert-= (length p) ml:+n+))))

(deftest test-sample-poly-cbd-range-eta-2
  "For η=2 the coefficient is in {-2,-1,0,1,2}, which after reduction
   mod q becomes {0, 1, 2, q-2, q-1}."
  (let ((bytes (make-array 128 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 128 do (setf (aref bytes i) (mod (* i 7) 256)))
    (let ((p (sample:sample-poly-cbd bytes 2)))
      (loop for i from 0 below ml:+n+
            for c = (aref p i)
            do (assert-true
                (or (<= c 2)
                    (>= c (- ml:+q+ 2))))))))

(deftest test-sample-poly-cbd-all-zeros-input
  "An all-zero byte input produces an all-zero polynomial: every bit
   of every popcount is 0, so each coefficient is 0 - 0 = 0."
  (let* ((bytes (make-array 128 :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (p (sample:sample-poly-cbd bytes 2)))
    (assert-true (equalp p (ml:make-poly)))))

(deftest test-sample-poly-cbd-all-ones-input
  "An all-0xFF byte input has every bit set, so for η=2 each
   coefficient reads a=bits{1,1} (popcount=2) and b=bits{1,1}
   (popcount=2), giving a-b = 0. Result: all zeros."
  (let* ((bytes (make-array 128 :element-type '(unsigned-byte 8)
                                :initial-element #xff))
         (p (sample:sample-poly-cbd bytes 2)))
    (assert-true (equalp p (ml:make-poly)))))

(deftest test-sample-poly-cbd-eta-2-specific
  "Exercise specific 2η=4-bit inputs to confirm the bit-unpacking
   ordering. For η=2, the first coefficient reads bits 0,1 for `a`
   and bits 2,3 for `b`.

   byte 0 = 0b00001101:
     bit0=1  bit1=0  bit2=1  bit3=1
     a = popcount(1,0) = 1
     b = popcount(1,1) = 2
     coefficient[0] = 1 - 2 = -1 = q - 1

   byte 0 = 0b11100000 (bits 0..3 = 0,0,0,0; coefficient 0 is (a=0)-(b=0)=0)
     bit4=0  bit5=0  bit6=0  bit7=1  -> coefficient[1] reads bits 4..7
     Wait: coefficient 1 reads bits 4,5 for a and 6,7 for b.
     a = popcount(0,0) = 0, b = popcount(0,1) = 1
     coefficient[1] = -1 = q - 1"
  (let ((b1 (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref b1 0) #b00001101)
    (let ((p (sample:sample-poly-cbd b1 2)))
      (assert-= (aref p 0) (- ml:+q+ 1))
      (assert-= (aref p 1) 0))   ; bits 4..7 of byte 0 are all zero
    (let ((b2 (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref b2 0) #b11100000)
      (let ((p2 (sample:sample-poly-cbd b2 2)))
        (assert-= (aref p2 0) 0)  ; bits 0..3 of byte 0 are zero
        (assert-= (aref p2 1) (- ml:+q+ 1))))))

(deftest test-sample-poly-cbd-wrong-length-rejected
  "An input byte vector whose length is not 64·η is rejected."
  (let ((bytes (make-array 100 :element-type '(unsigned-byte 8))))
    (assert-condition (error) (sample:sample-poly-cbd bytes 2))))
