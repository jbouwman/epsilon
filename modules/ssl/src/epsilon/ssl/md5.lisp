;;;; MD5 (RFC 1321)
;;;;
;;;; Legacy hash function for X.509 fingerprints. Not for security use.

(defpackage epsilon.ssl.md5
  (:use :cl :epsilon.ssl.primitives)
  (:export
   #:md5 #:md5-hex
   #:make-md5-state #:md5-update #:md5-finalize #:md5-copy))

(in-package :epsilon.ssl.md5)

(defconstant +md5-block-size+ 64)
(defconstant +md5-digest-size+ 16)

;;; ---------------------------------------------------------------------------
;;; Round constants: T[i] = floor(2^32 * abs(sin(i+1)))
;;; ---------------------------------------------------------------------------

(defparameter +t-constants+
  (make-array 64
    :element-type '(unsigned-byte 32)
    :initial-contents
    '(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
      #xf57c0faf #x4787c62a #xa8304613 #xfd469501
      #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
      #x6b901122 #xfd987193 #xa679438e #x49b40821
      #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
      #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
      #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
      #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
      #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
      #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
      #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
      #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
      #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
      #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
      #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
      #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391)))

;; Shift amounts per round
(defparameter +s+
  (make-array 64
    :element-type '(unsigned-byte 8)
    :initial-contents
    '( 7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
       5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
       4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
       6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21)))

;;; ---------------------------------------------------------------------------
;;; MD5 state
;;; ---------------------------------------------------------------------------

(defstruct (md5-state (:constructor %make-md5-state))
  (a #x67452301 :type (unsigned-byte 32))
  (b #xefcdab89 :type (unsigned-byte 32))
  (c #x98badcfe :type (unsigned-byte 32))
  (d #x10325476 :type (unsigned-byte 32))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (64)))
  (buffer-count 0 :type (integer 0 64))
  (total-length 0 :type (unsigned-byte 64)))

(defun make-md5-state ()
  (%make-md5-state))

(defun md5-copy (state)
  (let ((new (%make-md5-state)))
    (setf (md5-state-a new) (md5-state-a state))
    (setf (md5-state-b new) (md5-state-b state))
    (setf (md5-state-c new) (md5-state-c state))
    (setf (md5-state-d new) (md5-state-d state))
    (replace (md5-state-buffer new) (md5-state-buffer state))
    (setf (md5-state-buffer-count new) (md5-state-buffer-count state))
    (setf (md5-state-total-length new) (md5-state-total-length state))
    new))

;;; ---------------------------------------------------------------------------
;;; Block processing
;;; ---------------------------------------------------------------------------

(defun md5-process-block (state block offset)
  "Process a 64-byte block."
  (declare (type md5-state state)
           (type (simple-array (unsigned-byte 8) (*)) block)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  ;; Read 16 little-endian 32-bit words
  (let ((m (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 0 below 16
          for base = (+ offset (* i 4))
          do (setf (aref m i)
                   (logior (aref block base)
                           (ash (aref block (+ base 1)) 8)
                           (ash (aref block (+ base 2)) 16)
                           (ash (aref block (+ base 3)) 24))))
    (let ((a (md5-state-a state))
          (b (md5-state-b state))
          (c (md5-state-c state))
          (d (md5-state-d state)))
      (declare (type (unsigned-byte 32) a b c d))
      (loop for i from 0 below 64
            do (let ((f 0) (g 0))
                 (declare (type (unsigned-byte 32) f)
                          (type fixnum g))
                 (cond
                   ((< i 16)
                    (setf f (logand +u32-mask+
                                    (logior (logand b c)
                                            (logand (logand +u32-mask+ (lognot b)) d))))
                    (setf g i))
                   ((< i 32)
                    (setf f (logand +u32-mask+
                                    (logior (logand d b)
                                            (logand (logand +u32-mask+ (lognot d)) c))))
                    (setf g (mod (+ (* 5 i) 1) 16)))
                   ((< i 48)
                    (setf f (logand +u32-mask+ (logxor b c d)))
                    (setf g (mod (+ (* 3 i) 5) 16)))
                   (t
                    (setf f (logand +u32-mask+
                                    (logxor c (logior b
                                                      (logand +u32-mask+ (lognot d))))))
                    (setf g (mod (* 7 i) 16))))
                 (let ((temp d))
                   (setf d c)
                   (setf c b)
                   (setf b (u32+ b (u32-rotl (u32+ a f (aref +t-constants+ i) (aref m g))
                                             (aref +s+ i))))
                   (setf a temp))))
      (setf (md5-state-a state) (u32+ (md5-state-a state) a))
      (setf (md5-state-b state) (u32+ (md5-state-b state) b))
      (setf (md5-state-c state) (u32+ (md5-state-c state) c))
      (setf (md5-state-d state) (u32+ (md5-state-d state) d)))))

;;; ---------------------------------------------------------------------------
;;; Incremental API
;;; ---------------------------------------------------------------------------

(defun md5-update (state data &key (start 0) (end nil))
  (declare (type md5-state state)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (let* ((end (or end (length data)))
         (buf (md5-state-buffer state))
         (buf-count (md5-state-buffer-count state)))
    (incf (md5-state-total-length state) (- end start))
    (let ((pos start))
      (when (> buf-count 0)
        (let ((copy-len (min (- 64 buf-count) (- end pos))))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 64)
            (md5-process-block state buf 0)
            (setf buf-count 0))))
      (loop while (<= (+ pos 64) end)
            do (md5-process-block state data pos)
               (incf pos 64))
      (when (< pos end)
        (replace buf data :start1 0 :end1 (- end pos)
                          :start2 pos :end2 end)
        (setf buf-count (- end pos)))
      (setf (md5-state-buffer-count state) buf-count)))
  state)

(defun md5-finalize (state)
  "Finalize and return 16-byte digest."
  (let* ((buf (md5-state-buffer state))
         (buf-count (md5-state-buffer-count state))
         (total-bits (* (md5-state-total-length state) 8)))
    (setf (aref buf buf-count) #x80)
    (incf buf-count)
    (loop for i from buf-count below 64
          do (setf (aref buf i) 0))
    (when (> buf-count 56)
      (md5-process-block state buf 0)
      (loop for i from 0 below 64
            do (setf (aref buf i) 0)))
    ;; Little-endian 64-bit length
    (loop for i from 0 below 8
          do (setf (aref buf (+ 56 i))
                   (logand #xFF (ash total-bits (- (* i 8))))))
    (md5-process-block state buf 0)
    ;; Extract digest (little-endian)
    (let ((digest (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for (word idx) in (list (list (md5-state-a state) 0)
                                    (list (md5-state-b state) 4)
                                    (list (md5-state-c state) 8)
                                    (list (md5-state-d state) 12))
            do (setf (aref digest idx) (logand #xFF word))
               (setf (aref digest (+ idx 1)) (logand #xFF (ash word -8)))
               (setf (aref digest (+ idx 2)) (logand #xFF (ash word -16)))
               (setf (aref digest (+ idx 3)) (logand #xFF (ash word -24))))
      digest)))

;;; ---------------------------------------------------------------------------
;;; One-shot API
;;; ---------------------------------------------------------------------------

(defun md5 (data &key (start 0) (end nil))
  "Compute MD5 of DATA. Returns 16-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-md5-state)))
    (md5-update state data :start start :end end)
    (md5-finalize state)))

(defun md5-hex (data &key (start 0) (end nil))
  (let ((digest (md5 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))
