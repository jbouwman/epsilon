;;;; SHA-1 (FIPS 180-4)
;;;;
;;;; Legacy hash function for X.509 fingerprints. Not for security use.

(defpackage epsilon.ssl.sha1
  (:use :cl)
  (:import-from #:epsilon.ssl.primitives #:u32+ #:u32-rotl #:+u32-mask+)
  (:export
   #:sha1 #:sha1-hex
   #:make-sha1-state #:sha1-update #:sha1-finalize #:sha1-copy
   #:+sha1-block-size+ #:+sha1-digest-size+))

(in-package :epsilon.ssl.sha1)

(defconstant +sha1-block-size+ 64)
(defconstant +sha1-digest-size+ 20)

;;; ---------------------------------------------------------------------------
;;; SHA-1 state
;;; ---------------------------------------------------------------------------

(defstruct (sha1-state (:constructor %make-sha1-state))
  (h (make-array 5
       :element-type '(unsigned-byte 32)
       :initial-contents '(#x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0))
   :type (simple-array (unsigned-byte 32) (5)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (64)))
  (buffer-count 0 :type (integer 0 64))
  (total-length 0 :type (unsigned-byte 64)))

(defun make-sha1-state ()
  (%make-sha1-state))

(defun sha1-copy (state)
  (let ((new (%make-sha1-state)))
    (replace (sha1-state-h new) (sha1-state-h state))
    (replace (sha1-state-buffer new) (sha1-state-buffer state))
    (setf (sha1-state-buffer-count new) (sha1-state-buffer-count state))
    (setf (sha1-state-total-length new) (sha1-state-total-length state))
    new))

;;; ---------------------------------------------------------------------------
;;; Block processing
;;; ---------------------------------------------------------------------------

(defun sha1-process-block (state block offset)
  "Process a 64-byte block."
  (declare (type sha1-state state)
           (type (simple-array (unsigned-byte 8) (*)) block)
           (type fixnum offset)
           (optimize (speed 3) (safety 1)))
  (let ((w (make-array 80 :element-type '(unsigned-byte 32) :initial-element 0))
        (hv (sha1-state-h state)))
    ;; W[0..15] = big-endian 32-bit words
    (loop for t-idx from 0 below 16
          for byte-idx = (+ offset (* t-idx 4))
          do (setf (aref w t-idx)
                   (logior (ash (aref block byte-idx) 24)
                           (ash (aref block (+ byte-idx 1)) 16)
                           (ash (aref block (+ byte-idx 2)) 8)
                           (aref block (+ byte-idx 3)))))
    ;; W[16..79]
    (loop for t-idx from 16 below 80
          do (setf (aref w t-idx)
                   (u32-rotl (logxor (aref w (- t-idx 3))
                                     (aref w (- t-idx 8))
                                     (aref w (- t-idx 14))
                                     (aref w (- t-idx 16)))
                             1)))
    (let ((a (aref hv 0)) (b (aref hv 1)) (c (aref hv 2))
          (d (aref hv 3)) (e (aref hv 4)))
      (declare (type (unsigned-byte 32) a b c d e))
      (loop for t-idx from 0 below 80
            do (let ((f 0) (k 0))
                 (declare (type (unsigned-byte 32) f k))
                 (cond
                   ((< t-idx 20)
                    (setf f (logand +u32-mask+
                                    (logior (logand b c)
                                            (logand (logand +u32-mask+ (lognot b)) d))))
                    (setf k #x5A827999))
                   ((< t-idx 40)
                    (setf f (logand +u32-mask+ (logxor b c d)))
                    (setf k #x6ED9EBA1))
                   ((< t-idx 60)
                    (setf f (logand +u32-mask+
                                    (logior (logand b c)
                                            (logand b d)
                                            (logand c d))))
                    (setf k #x8F1BBCDC))
                   (t
                    (setf f (logand +u32-mask+ (logxor b c d)))
                    (setf k #xCA62C1D6)))
                 (let ((temp (u32+ (u32-rotl a 5) f e k (aref w t-idx))))
                   (setf e d  d c  c (u32-rotl b 30)  b a  a temp))))
      (setf (aref hv 0) (u32+ (aref hv 0) a))
      (setf (aref hv 1) (u32+ (aref hv 1) b))
      (setf (aref hv 2) (u32+ (aref hv 2) c))
      (setf (aref hv 3) (u32+ (aref hv 3) d))
      (setf (aref hv 4) (u32+ (aref hv 4) e)))))

;;; ---------------------------------------------------------------------------
;;; Incremental API
;;; ---------------------------------------------------------------------------

(defun sha1-update (state data &key (start 0) (end nil))
  (declare (type sha1-state state)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (let* ((end (or end (length data)))
         (buf (sha1-state-buffer state))
         (buf-count (sha1-state-buffer-count state)))
    (incf (sha1-state-total-length state) (- end start))
    (let ((pos start))
      (when (> buf-count 0)
        (let ((copy-len (min (- 64 buf-count) (- end pos))))
          (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                            :start2 pos :end2 (+ pos copy-len))
          (incf pos copy-len)
          (incf buf-count copy-len)
          (when (= buf-count 64)
            (sha1-process-block state buf 0)
            (setf buf-count 0))))
      (loop while (<= (+ pos 64) end)
            do (sha1-process-block state data pos)
               (incf pos 64))
      (when (< pos end)
        (replace buf data :start1 0 :end1 (- end pos)
                          :start2 pos :end2 end)
        (setf buf-count (- end pos)))
      (setf (sha1-state-buffer-count state) buf-count)))
  state)

(defun sha1-finalize (state)
  "Finalize and return 20-byte digest."
  (let* ((buf (sha1-state-buffer state))
         (buf-count (sha1-state-buffer-count state))
         (total-bits (* (sha1-state-total-length state) 8)))
    (setf (aref buf buf-count) #x80)
    (incf buf-count)
    (loop for i from buf-count below 64
          do (setf (aref buf i) 0))
    (when (> buf-count 56)
      (sha1-process-block state buf 0)
      (loop for i from 0 below 64
            do (setf (aref buf i) 0)))
    ;; Big-endian 64-bit length
    (setf (aref buf 56) (logand #xFF (ash total-bits -56)))
    (setf (aref buf 57) (logand #xFF (ash total-bits -48)))
    (setf (aref buf 58) (logand #xFF (ash total-bits -40)))
    (setf (aref buf 59) (logand #xFF (ash total-bits -32)))
    (setf (aref buf 60) (logand #xFF (ash total-bits -24)))
    (setf (aref buf 61) (logand #xFF (ash total-bits -16)))
    (setf (aref buf 62) (logand #xFF (ash total-bits -8)))
    (setf (aref buf 63) (logand #xFF total-bits))
    (sha1-process-block state buf 0)
    ;; Extract digest (big-endian)
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8)))
          (hv (sha1-state-h state)))
      (loop for i from 0 below 5
            for word = (aref hv i)
            for base = (* i 4)
            do (setf (aref digest base)       (logand #xFF (ash word -24)))
               (setf (aref digest (+ base 1)) (logand #xFF (ash word -16)))
               (setf (aref digest (+ base 2)) (logand #xFF (ash word -8)))
               (setf (aref digest (+ base 3)) (logand #xFF word)))
      digest)))

;;; ---------------------------------------------------------------------------
;;; One-shot API
;;; ---------------------------------------------------------------------------

(defun sha1 (data &key (start 0) (end nil))
  "Compute SHA-1 of DATA. Returns 20-byte digest."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-sha1-state)))
    (sha1-update state data :start start :end end)
    (sha1-finalize state)))

(defun sha1-hex (data &key (start 0) (end nil))
  (let ((digest (sha1 data :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))
