(defpackage :epsilon.digest.md5
  (:use
   :cl
   :epsilon.syntax
   :epsilon.list
   :epsilon.type
   :epsilon.symbol
   :epsilon.digest.common
   :epsilon.digest.generic
   :epsilon.digest.reader)
  (:export
   :make-md5-digest))

;;; implementation of MD5
;;; Based on ironclad implementation, adapted for epsilon infrastructure

(in-package :epsilon.digest.md5)

(in-ironclad-readtable)

;;; MD5 constants - Table T from RFC 1321 Section 3.4
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *t* (make-array 64 :element-type '(unsigned-byte 32)
                                :initial-contents
                                (loop for i from 1 to 64
                                      collect
                                      (truncate
                                       (* 4294967296
                                          (abs (sin (float i 0.0d0)))))))))

;;; MD5 working set (Initial values from RFC 1321 Section 3.3)

(define-digest-registers (md5 :endian :little)
  (a #x67452301)
  (b #xefcdab89)
  (c #x98badcfe)
  (d #x10325476))

(define-constant +pristine-md5-registers+ (initial-md5-regs))

(defun update-md5-block (regs block)
  "This is the core part of the MD5 algorithm. It takes a complete 16
word block of input, and updates the working state in A, B, C, and D
accordingly."
  (declare (type md5-regs regs)
           (type (->u32 16) block))
  (let ((a (md5-regs-a regs)) (b (md5-regs-b regs))
        (c (md5-regs-c regs)) (d (md5-regs-d regs)))
    (declare (type (unsigned-byte 32) a b c d))
    (flet ((f (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logxor z (logand x (logxor y z))))
           (g (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logxor y (logand z (logxor x y))))
           (h (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (logxor x y z))
           (i (x y z)
             (declare (type (unsigned-byte 32) x y z))
             (ldb (byte 32 0) (logxor y (logorc2 x z)))))
      #+ironclad-fast-mod32-arithmetic
      (declare (inline f g h i))
      (macrolet ((with-md5-round ((op block) &rest clauses)
                   (loop for (a b c d k s i) in clauses
                         collect
                         `(setq ,a (mod32+ ,b
                                           (rol32 (mod32+ (mod32+ ,a (,op ,b ,c ,d))
                                                          (mod32+ (aref ,block ,k)
                                                                  ,(aref *t* (1- i))))
                                                  ,s)))
                         into result
                         finally (return `(progn ,@result)))))
        ;; Round 1
        (with-md5-round (f block)
          (a b c d  0  7  1)(d a b c  1 12  2)(c d a b  2 17  3)(b c d a  3 22  4)
          (a b c d  4  7  5)(d a b c  5 12  6)(c d a b  6 17  7)(b c d a  7 22  8)
          (a b c d  8  7  9)(d a b c  9 12 10)(c d a b 10 17 11)(b c d a 11 22 12)
          (a b c d 12  7 13)(d a b c 13 12 14)(c d a b 14 17 15)(b c d a 15 22 16))
        ;; Round 2
        (with-md5-round (g block)
          (a b c d  1  5 17)(d a b c  6  9 18)(c d a b 11 14 19)(b c d a  0 20 20)
          (a b c d  5  5 21)(d a b c 10  9 22)(c d a b 15 14 23)(b c d a  4 20 24)
          (a b c d  9  5 25)(d a b c 14  9 26)(c d a b  3 14 27)(b c d a  8 20 28)
          (a b c d 13  5 29)(d a b c  2  9 30)(c d a b  7 14 31)(b c d a 12 20 32))
        ;; Round 3
        (with-md5-round (h block)
          (a b c d  5  4 33)(d a b c  8 11 34)(c d a b 11 16 35)(b c d a 14 23 36)
          (a b c d  1  4 37)(d a b c  4 11 38)(c d a b  7 16 39)(b c d a 10 23 40)
          (a b c d 13  4 41)(d a b c  0 11 42)(c d a b  3 16 43)(b c d a  6 23 44)
          (a b c d  9  4 45)(d a b c 12 11 46)(c d a b 15 16 47)(b c d a  2 23 48))
        ;; Round 4
        (with-md5-round (i block)
          (a b c d  0  6 49)(d a b c  7 10 50)(c d a b 14 15 51)(b c d a  5 21 52)
          (a b c d 12  6 53)(d a b c  3 10 54)(c d a b 10 15 55)(b c d a  1 21 56)
          (a b c d  8  6 57)(d a b c 15 10 58)(c d a b  6 15 59)(b c d a 13 21 60)
          (a b c d  4  6 61)(d a b c 11 10 62)(c d a b  2 15 63)(b c d a  9 21 64))
        ;; Update and return
        (setf (md5-regs-a regs) (mod32+ (md5-regs-a regs) a)
              (md5-regs-b regs) (mod32+ (md5-regs-b regs) b)
              (md5-regs-c regs) (mod32+ (md5-regs-c regs) c)
              (md5-regs-d regs) (mod32+ (md5-regs-d regs) d))
        regs))))

;;; mid-level

(defstruct (md5
             (:constructor %make-md5-digest nil)
             (:constructor %make-md5-state (regs amount block buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (initial-md5-regs) :type md5-regs :read-only t)
  (block (make-array 16 :element-type '(unsigned-byte 32))
    :type (->u32 16) :read-only t))

(defmethod reinitialize-instance ((state md5) &rest initargs)
  (declare (ignore initargs))
  (replace (md5-regs state) +pristine-md5-registers+)
  (setf (md5-amount state) 0
        (md5-buffer-index state) 0)
  state)

(defmethod copy-digest ((state md5) &optional copy)
  (check-type copy (or null md5))
  (cond
    (copy
     (replace (md5-regs copy) (md5-regs state))
     (replace (md5-buffer copy) (md5-buffer state))
     (setf (md5-amount copy) (md5-amount state)
           (md5-buffer-index copy) (md5-buffer-index state))
     copy)
    (t
     (%make-md5-state (copy-seq (md5-regs state))
                      (md5-amount state)
                      (copy-seq (md5-block state))
                      (copy-seq (md5-buffer state))
                      (md5-buffer-index state)))))

(define-digest-updater md5
  "Update the given md5-state from sequence, which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by start and end, which must be numeric bounding-indices."
  (flet ((compress (state sequence offset)
           (let ((block (md5-block state)))
             (fill-block-u8-le block sequence offset)
             (update-md5-block (md5-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (md5 16)
  "If the given md5-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD5 standard.

The resulting MD5 message-digest is returned as an array of sixteen
 (unsigned-byte 8) values."
  (let ((regs (md5-regs state))
        (block (md5-block state))
        (buffer (md5-buffer state))
        (buffer-index (md5-buffer-index state))
        (total-length (* 8 (md5-amount state))))
    (declare (type md5-regs regs)
             (type (integer 0 63) buffer-index)
             (type (->u32 16) block)
             (type ->u8 buffer))
    (declare (notinline update-md5-block))
    ;; Add padding
    (setf (aref buffer buffer-index) #x80)
    (when (> buffer-index 55)
      ;; Not enough room for padding and length, need extra block
      (loop for index of-type (integer 0 64)
         from (1+ buffer-index) below 64
         do (setf (aref buffer index) #x00))
      (fill-block-u8-le block buffer 0)
      (update-md5-block regs block)
      (loop for index of-type (integer 0 16)
         from 0 below 16
         do (setf (aref block index) #x00000000)))
    (when (<= buffer-index 55)
      ;; Enough room for padding and length in current block
      (loop for index of-type (integer 0 64)
         from (1+ buffer-index) below 64
         do (setf (aref buffer index) #x00))
      ;; copy the data to BLOCK prematurely
      (fill-block-u8-le block buffer 0))
    ;; fill in the remaining block data (length in bits, little-endian)
    (store-data-length block total-length 14 nil) ; nil = little-endian
    (update-md5-block regs block)
    (finalize-registers state regs)))

(defdigest md5 :digest-length 16 :block-length 64)

(defun make-md5-digest ()
  "Create a new MD5 digest state"
  (%make-md5-digest))