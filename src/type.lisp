(in-package #:encode)

;; jx

(deftype input-index ()
  'u16)

(deftype input-buffer ()
  `(simple-array u8 (,+input-size+)))

(deftype chains-buffer ()
  `(simple-array u16 (,+input-size+)))

(deftype hashes-buffer ()
  `(simple-array u16 (,+hashes-size+)))

(deftype hash ()
  `(integer 0 ,+hashes-size+))

(deftype bitstream-buffer ()
  `(simple-array u8 (,+bitstream-buffer-size+)))

(deftype bitstream-buffer-bit-count ()
  `(integer 0 ,+bitstream-buffer-bits+))

(deftype index () '(mod #.array-dimension-limit))

(deftype deflate-code-length () '(integer 0 #.+max-code-length+))

(deftype deflate-code () '(unsigned-byte #.+max-code-length+))

(deftype deflate-code-value () '(integer 0 (#.+max-codes+)))

(defparameter *distance-code-extra-bits*
  ;; codes 30 and 31 will never actually appear, but we represent them
  ;; for completeness' sake
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 0 0))

(defparameter *distance-code-base-distances*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
      1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))

(declaim (inline n-length-extra-bits n-distance-extra-bits length-base distance-base))
(defun n-length-extra-bits (value)
  (aref +length-code-extra-bits+ value))

(defun n-distance-extra-bits (distance-code)
  (svref *distance-code-extra-bits* distance-code))

(defun length-base (value)
  (aref +length-code-base-lengths+ value))

(defun distance-base (distance-code)
  (svref *distance-code-base-distances* distance-code))

(defparameter *code-length-code-order*
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct (code-range-descriptor
             (:conc-name code-)
             (:constructor make-crd (n-bits start-value end-value)))
  (n-bits 0 :type deflate-code-length)
  (start-value 0 :type deflate-code-value)
  (end-value 0 :type deflate-code-value))

(defstruct (huffman-decode-table
             (:conc-name hdt-)
             (:constructor make-hdt (counts offsets symbols bits)))
  ;; FIXME: look into combining these two into one array for speed.
  (counts #1=(error "required parameter")
          :type (simple-array u16 (#.+max-code-length+))
          :read-only t)
  (offsets #1# :type (simple-array u16 (#.(1+ +max-code-length+)))
           :read-only t)
  (symbols nil :read-only t :type (simple-array fixnum (*)))
  (bits nil :read-only t))
) ; EVAL-WHEN

;;; decode table construction

(defun construct-huffman-decode-table (code-lengths &optional n-syms start)
  (let* ((n-syms (or n-syms (length code-lengths)))
         (start (or start 0))
         (min-code-length +max-code-length+)
         (max-code-length 0)
         (counts (make-array +max-code-length+ :initial-element 0
                            :element-type 'u16))
         (offsets (make-array (1+ +max-code-length+) :initial-element 0
                             :element-type 'u16))
         (symbols (make-array n-syms :initial-element 0 :element-type 'fixnum)))
    (declare (type (simple-array u16 (*)) counts)
             (type (simple-array fixnum (*)) symbols))
    (loop for i from start below (+ start n-syms) do
      (let ((c (aref code-lengths i)))
        (setf min-code-length (min min-code-length c))
        (setf max-code-length (max max-code-length c))
        (incf (aref counts c))))
    ;; generate offsets
    (loop for i from 1 below +deflate-max-bits+
          do (setf (aref offsets (1+ i)) (+ (aref offsets i) (aref counts i))))
    (dotimes (i n-syms (make-hdt counts offsets symbols max-code-length))
      (let ((l (aref code-lengths (+ start i))))
        (unless (zerop l)
          (setf (aref symbols (aref offsets l)) i)
          (incf (aref offsets l)))))))
