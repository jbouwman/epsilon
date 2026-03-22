;;;; BLAKE2b and BLAKE2s (RFC 7693)
;;;;
;;;; Pure-Lisp implementation of BLAKE2b (64-bit) and BLAKE2s (32-bit).

(defpackage epsilon.ssl.blake2
  (:use :cl :epsilon.ssl.primitives)
  (:export
   ;; BLAKE2b
   #:blake2b #:blake2b-hex
   #:make-blake2b-state #:blake2b-update #:blake2b-finalize #:blake2b-copy
   ;; BLAKE2s
   #:blake2s #:blake2s-hex
   #:make-blake2s-state #:blake2s-update #:blake2s-finalize #:blake2s-copy))

(in-package :epsilon.ssl.blake2)

;;; ---------------------------------------------------------------------------
;;; BLAKE2b constants (64-bit)
;;; ---------------------------------------------------------------------------

(defparameter +blake2b-iv+
  (coerce
   '(#x6a09e667f3bcc908 #xbb67ae8584caa73b
     #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
     #x510e527fade682d1 #x9b05688c2b3e6c1f
     #x1f83d9abfb41bd6b #x5be0cd19137e2179)
   '(simple-array (unsigned-byte 64) (8))))

(defparameter +sigma+
  #2A(( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
      (14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3)
      (11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4)
      ( 7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8)
      ( 9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13)
      ( 2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9)
      (12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11)
      (13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10)
      ( 6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5)
      (10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0)))

;;; ---------------------------------------------------------------------------
;;; BLAKE2b mixing
;;; ---------------------------------------------------------------------------

(defun blake2b-g (v a b c d x y)
  "BLAKE2b mixing function G."
  (declare (type (simple-array (unsigned-byte 64) (16)) v)
           (type fixnum a b c d)
           (type (unsigned-byte 64) x y))
  (setf (aref v a) (u64+ (aref v a) (aref v b) x))
  (setf (aref v d) (u64-rotr (logxor (aref v d) (aref v a)) 32))
  (setf (aref v c) (u64+ (aref v c) (aref v d)))
  (setf (aref v b) (u64-rotr (logxor (aref v b) (aref v c)) 24))
  (setf (aref v a) (u64+ (aref v a) (aref v b) y))
  (setf (aref v d) (u64-rotr (logxor (aref v d) (aref v a)) 16))
  (setf (aref v c) (u64+ (aref v c) (aref v d)))
  (setf (aref v b) (u64-rotr (logxor (aref v b) (aref v c)) 63)))

(defun blake2b-compress (h m t-lo t-hi final-p)
  "BLAKE2b compression function."
  (declare (type (simple-array (unsigned-byte 64) (8)) h)
           (type (simple-array (unsigned-byte 64) (16)) m)
           (type (unsigned-byte 64) t-lo t-hi))
  (let ((v (make-array 16 :element-type '(unsigned-byte 64) :initial-element 0)))
    ;; Initialize working vector
    (replace v h :end1 8)
    (replace v +blake2b-iv+ :start1 8)
    ;; XOR counters and finalization flag
    (setf (aref v 12) (logxor (aref v 12) t-lo))
    (setf (aref v 13) (logxor (aref v 13) t-hi))
    (when final-p
      (setf (aref v 14) (logand +u64-mask+ (lognot (aref v 14)))))
    ;; 12 rounds of mixing
    (loop for round-idx from 0 below 12
          for s-row = (mod round-idx 10)
          do (blake2b-g v 0 4  8 12 (aref m (aref +sigma+ s-row  0)) (aref m (aref +sigma+ s-row  1)))
             (blake2b-g v 1 5  9 13 (aref m (aref +sigma+ s-row  2)) (aref m (aref +sigma+ s-row  3)))
             (blake2b-g v 2 6 10 14 (aref m (aref +sigma+ s-row  4)) (aref m (aref +sigma+ s-row  5)))
             (blake2b-g v 3 7 11 15 (aref m (aref +sigma+ s-row  6)) (aref m (aref +sigma+ s-row  7)))
             (blake2b-g v 0 5 10 15 (aref m (aref +sigma+ s-row  8)) (aref m (aref +sigma+ s-row  9)))
             (blake2b-g v 1 6 11 12 (aref m (aref +sigma+ s-row 10)) (aref m (aref +sigma+ s-row 11)))
             (blake2b-g v 2 7  8 13 (aref m (aref +sigma+ s-row 12)) (aref m (aref +sigma+ s-row 13)))
             (blake2b-g v 3 4  9 14 (aref m (aref +sigma+ s-row 14)) (aref m (aref +sigma+ s-row 15))))
    ;; Finalize
    (loop for i from 0 below 8
          do (setf (aref h i)
                   (logxor (aref h i) (aref v i) (aref v (+ i 8)))))))

;;; ---------------------------------------------------------------------------
;;; BLAKE2b state
;;; ---------------------------------------------------------------------------

(defstruct (blake2b-state (:constructor %make-blake2b-state))
  (h (let ((arr (make-array 8 :element-type '(unsigned-byte 64))))
       (replace arr +blake2b-iv+)
       arr)
   :type (simple-array (unsigned-byte 64) (8)))
  (buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (128)))
  (buffer-count 0 :type fixnum)
  (t-lo 0 :type (unsigned-byte 64))
  (t-hi 0 :type (unsigned-byte 64))
  (digest-length 64 :type fixnum))

(defun bytes-to-words64 (buf offset count)
  "Convert COUNT little-endian 64-bit words from BUF at OFFSET."
  (let ((m (make-array count :element-type '(unsigned-byte 64) :initial-element 0)))
    (loop for i from 0 below count
          for base = (+ offset (* i 8))
          do (setf (aref m i)
                   (logior (aref buf base)
                           (ash (aref buf (+ base 1)) 8)
                           (ash (aref buf (+ base 2)) 16)
                           (ash (aref buf (+ base 3)) 24)
                           (ash (aref buf (+ base 4)) 32)
                           (ash (aref buf (+ base 5)) 40)
                           (ash (aref buf (+ base 6)) 48)
                           (ash (aref buf (+ base 7)) 56))))
    m))

(defun make-blake2b-state (&key (digest-length 64) (key nil))
  "Create a BLAKE2b state. DIGEST-LENGTH is 1..64, KEY is optional (up to 64 bytes)."
  (let ((state (%make-blake2b-state)))
    (setf (blake2b-state-digest-length state) digest-length)
    ;; XOR parameter block into h[0]
    ;; Parameter block word 0: fanout=1, depth=1, leaf-length=0, digest-length
    (let ((key-len (if key (length key) 0)))
      (setf (aref (blake2b-state-h state) 0)
            (logxor (aref +blake2b-iv+ 0)
                    (logior digest-length
                            (ash key-len 8)
                            (ash 1 16)    ; fanout
                            (ash 1 24))))) ; depth
    ;; If keyed, pad key to block size and process
    (when (and key (> (length key) 0))
      (let ((key-block (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace key-block key)
        (blake2b-update state key-block)))
    state))

(defun blake2b-copy (state)
  (let ((new (%make-blake2b-state)))
    (replace (blake2b-state-h new) (blake2b-state-h state))
    (replace (blake2b-state-buffer new) (blake2b-state-buffer state))
    (setf (blake2b-state-buffer-count new) (blake2b-state-buffer-count state))
    (setf (blake2b-state-t-lo new) (blake2b-state-t-lo state))
    (setf (blake2b-state-t-hi new) (blake2b-state-t-hi state))
    (setf (blake2b-state-digest-length new) (blake2b-state-digest-length state))
    new))

(defun blake2b-increment-counter (state n)
  "Increment the counter by N bytes."
  (let ((new-lo (+ (blake2b-state-t-lo state) n)))
    (when (> new-lo +u64-mask+)
      (incf (blake2b-state-t-hi state)))
    (setf (blake2b-state-t-lo state) (logand +u64-mask+ new-lo))))

(defun blake2b-update (state data &key (start 0) (end nil))
  "Feed data into BLAKE2b."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let* ((end (or end (length data)))
         (buf (blake2b-state-buffer state))
         (buf-count (blake2b-state-buffer-count state))
         (pos start))
    ;; If buffer has data, try to fill it
    (when (> buf-count 0)
      (let ((copy-len (min (- 128 buf-count) (- end pos))))
        (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                          :start2 pos :end2 (+ pos copy-len))
        (incf pos copy-len)
        (incf buf-count copy-len)
        (when (and (= buf-count 128) (< pos end))
          ;; Buffer full and more data coming, compress it
          (blake2b-increment-counter state 128)
          (let ((m (bytes-to-words64 buf 0 16)))
            (blake2b-compress (blake2b-state-h state) m
                              (blake2b-state-t-lo state)
                              (blake2b-state-t-hi state) nil))
          (setf buf-count 0))))
    ;; Process full blocks (but keep the last block in buffer for finalization)
    (loop while (> (- end pos) 128)
          do (blake2b-increment-counter state 128)
             (let ((m (bytes-to-words64 data pos 16)))
               (blake2b-compress (blake2b-state-h state) m
                                 (blake2b-state-t-lo state)
                                 (blake2b-state-t-hi state) nil))
             (incf pos 128))
    ;; Buffer remaining bytes
    (when (< pos end)
      (let ((remaining (- end pos)))
        (replace buf data :start1 buf-count :end1 (+ buf-count remaining)
                          :start2 pos :end2 end)
        (incf buf-count remaining)))
    (setf (blake2b-state-buffer-count state) buf-count))
  state)

(defun blake2b-finalize (state)
  "Finalize BLAKE2b and return the digest."
  (let ((buf (blake2b-state-buffer state))
        (buf-count (blake2b-state-buffer-count state))
        (digest-len (blake2b-state-digest-length state)))
    ;; Zero remaining buffer
    (loop for i from buf-count below 128
          do (setf (aref buf i) 0))
    ;; Increment counter by actual bytes in buffer
    (blake2b-increment-counter state buf-count)
    ;; Final compress
    (let ((m (bytes-to-words64 buf 0 16)))
      (blake2b-compress (blake2b-state-h state) m
                        (blake2b-state-t-lo state)
                        (blake2b-state-t-hi state) t))
    ;; Extract digest (little-endian)
    (let ((digest (make-array digest-len :element-type '(unsigned-byte 8)))
          (h (blake2b-state-h state)))
      (loop for i from 0 below digest-len
            do (setf (aref digest i)
                     (logand #xFF (ash (aref h (floor i 8))
                                       (- (* (mod i 8) 8))))))
      digest)))

(defun blake2b (data &key (digest-length 64) (key nil) (start 0) (end nil))
  "Compute BLAKE2b of DATA. Returns DIGEST-LENGTH bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-blake2b-state :digest-length digest-length :key key)))
    (blake2b-update state data :start start :end end)
    (blake2b-finalize state)))

(defun blake2b-hex (data &key (digest-length 64) (key nil) (start 0) (end nil))
  (let ((digest (blake2b data :digest-length digest-length :key key
                              :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

;;; ---------------------------------------------------------------------------
;;; BLAKE2s constants (32-bit)
;;; ---------------------------------------------------------------------------

(defparameter +blake2s-iv+
  (make-array 8
    :element-type '(unsigned-byte 32)
    :initial-contents
    '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
      #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))

;;; ---------------------------------------------------------------------------
;;; BLAKE2s mixing
;;; ---------------------------------------------------------------------------

(defun blake2s-g (v a b c d x y)
  "BLAKE2s mixing function G."
  (declare (type (simple-array (unsigned-byte 32) (16)) v)
           (type fixnum a b c d)
           (type (unsigned-byte 32) x y))
  (setf (aref v a) (u32+ (aref v a) (aref v b) x))
  (setf (aref v d) (u32-rotr (logxor (aref v d) (aref v a)) 16))
  (setf (aref v c) (u32+ (aref v c) (aref v d)))
  (setf (aref v b) (u32-rotr (logxor (aref v b) (aref v c)) 12))
  (setf (aref v a) (u32+ (aref v a) (aref v b) y))
  (setf (aref v d) (u32-rotr (logxor (aref v d) (aref v a)) 8))
  (setf (aref v c) (u32+ (aref v c) (aref v d)))
  (setf (aref v b) (u32-rotr (logxor (aref v b) (aref v c)) 7)))

(defun blake2s-compress (h m t-lo t-hi final-p)
  "BLAKE2s compression function."
  (declare (type (simple-array (unsigned-byte 32) (8)) h)
           (type (simple-array (unsigned-byte 32) (16)) m)
           (type (unsigned-byte 32) t-lo t-hi))
  (let ((v (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
    (replace v h :end1 8)
    (replace v +blake2s-iv+ :start1 8)
    (setf (aref v 12) (logxor (aref v 12) t-lo))
    (setf (aref v 13) (logxor (aref v 13) t-hi))
    (when final-p
      (setf (aref v 14) (logand +u32-mask+ (lognot (aref v 14)))))
    ;; 10 rounds
    (loop for round-idx from 0 below 10
          for s-row = (mod round-idx 10)
          do (blake2s-g v 0 4  8 12 (aref m (aref +sigma+ s-row  0)) (aref m (aref +sigma+ s-row  1)))
             (blake2s-g v 1 5  9 13 (aref m (aref +sigma+ s-row  2)) (aref m (aref +sigma+ s-row  3)))
             (blake2s-g v 2 6 10 14 (aref m (aref +sigma+ s-row  4)) (aref m (aref +sigma+ s-row  5)))
             (blake2s-g v 3 7 11 15 (aref m (aref +sigma+ s-row  6)) (aref m (aref +sigma+ s-row  7)))
             (blake2s-g v 0 5 10 15 (aref m (aref +sigma+ s-row  8)) (aref m (aref +sigma+ s-row  9)))
             (blake2s-g v 1 6 11 12 (aref m (aref +sigma+ s-row 10)) (aref m (aref +sigma+ s-row 11)))
             (blake2s-g v 2 7  8 13 (aref m (aref +sigma+ s-row 12)) (aref m (aref +sigma+ s-row 13)))
             (blake2s-g v 3 4  9 14 (aref m (aref +sigma+ s-row 14)) (aref m (aref +sigma+ s-row 15))))
    (loop for i from 0 below 8
          do (setf (aref h i)
                   (logxor (aref h i) (aref v i) (aref v (+ i 8)))))))

;;; ---------------------------------------------------------------------------
;;; BLAKE2s state
;;; ---------------------------------------------------------------------------

(defstruct (blake2s-state (:constructor %make-blake2s-state))
  (h (let ((arr (make-array 8 :element-type '(unsigned-byte 32))))
       (replace arr +blake2s-iv+)
       arr)
   :type (simple-array (unsigned-byte 32) (8)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (64)))
  (buffer-count 0 :type fixnum)
  (t-lo 0 :type (unsigned-byte 32))
  (t-hi 0 :type (unsigned-byte 32))
  (digest-length 32 :type fixnum))

(defun bytes-to-words32 (buf offset count)
  "Convert COUNT little-endian 32-bit words from BUF at OFFSET."
  (let ((m (make-array count :element-type '(unsigned-byte 32) :initial-element 0)))
    (loop for i from 0 below count
          for base = (+ offset (* i 4))
          do (setf (aref m i)
                   (logior (aref buf base)
                           (ash (aref buf (+ base 1)) 8)
                           (ash (aref buf (+ base 2)) 16)
                           (ash (aref buf (+ base 3)) 24))))
    m))

(defun make-blake2s-state (&key (digest-length 32) (key nil))
  "Create a BLAKE2s state. DIGEST-LENGTH is 1..32, KEY is optional (up to 32 bytes)."
  (let ((state (%make-blake2s-state)))
    (setf (blake2s-state-digest-length state) digest-length)
    (let ((key-len (if key (length key) 0)))
      (setf (aref (blake2s-state-h state) 0)
            (logxor (aref +blake2s-iv+ 0)
                    (logior digest-length
                            (ash key-len 8)
                            (ash 1 16)
                            (ash 1 24)))))
    (when (and key (> (length key) 0))
      (let ((key-block (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace key-block key)
        (blake2s-update state key-block)))
    state))

(defun blake2s-copy (state)
  (let ((new (%make-blake2s-state)))
    (replace (blake2s-state-h new) (blake2s-state-h state))
    (replace (blake2s-state-buffer new) (blake2s-state-buffer state))
    (setf (blake2s-state-buffer-count new) (blake2s-state-buffer-count state))
    (setf (blake2s-state-t-lo new) (blake2s-state-t-lo state))
    (setf (blake2s-state-t-hi new) (blake2s-state-t-hi state))
    (setf (blake2s-state-digest-length new) (blake2s-state-digest-length state))
    new))

(defun blake2s-increment-counter (state n)
  (let ((new-lo (+ (blake2s-state-t-lo state) n)))
    (when (> new-lo +u32-mask+)
      (incf (blake2s-state-t-hi state)))
    (setf (blake2s-state-t-lo state) (logand +u32-mask+ new-lo))))

(defun blake2s-update (state data &key (start 0) (end nil))
  "Feed data into BLAKE2s."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let* ((end (or end (length data)))
         (buf (blake2s-state-buffer state))
         (buf-count (blake2s-state-buffer-count state))
         (pos start))
    (when (> buf-count 0)
      (let ((copy-len (min (- 64 buf-count) (- end pos))))
        (replace buf data :start1 buf-count :end1 (+ buf-count copy-len)
                          :start2 pos :end2 (+ pos copy-len))
        (incf pos copy-len)
        (incf buf-count copy-len)
        (when (and (= buf-count 64) (< pos end))
          (blake2s-increment-counter state 64)
          (let ((m (bytes-to-words32 buf 0 16)))
            (blake2s-compress (blake2s-state-h state) m
                              (blake2s-state-t-lo state)
                              (blake2s-state-t-hi state) nil))
          (setf buf-count 0))))
    (loop while (> (- end pos) 64)
          do (blake2s-increment-counter state 64)
             (let ((m (bytes-to-words32 data pos 16)))
               (blake2s-compress (blake2s-state-h state) m
                                 (blake2s-state-t-lo state)
                                 (blake2s-state-t-hi state) nil))
             (incf pos 64))
    (when (< pos end)
      (let ((remaining (- end pos)))
        (replace buf data :start1 buf-count :end1 (+ buf-count remaining)
                          :start2 pos :end2 end)
        (incf buf-count remaining)))
    (setf (blake2s-state-buffer-count state) buf-count))
  state)

(defun blake2s-finalize (state)
  "Finalize BLAKE2s and return the digest."
  (let ((buf (blake2s-state-buffer state))
        (buf-count (blake2s-state-buffer-count state))
        (digest-len (blake2s-state-digest-length state)))
    (loop for i from buf-count below 64
          do (setf (aref buf i) 0))
    (blake2s-increment-counter state buf-count)
    (let ((m (bytes-to-words32 buf 0 16)))
      (blake2s-compress (blake2s-state-h state) m
                        (blake2s-state-t-lo state)
                        (blake2s-state-t-hi state) t))
    (let ((digest (make-array digest-len :element-type '(unsigned-byte 8)))
          (h (blake2s-state-h state)))
      (loop for i from 0 below digest-len
            do (setf (aref digest i)
                     (logand #xFF (ash (aref h (floor i 4))
                                       (- (* (mod i 4) 8))))))
      digest)))

(defun blake2s (data &key (digest-length 32) (key nil) (start 0) (end nil))
  "Compute BLAKE2s of DATA. Returns DIGEST-LENGTH bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-blake2s-state :digest-length digest-length :key key)))
    (blake2s-update state data :start start :end end)
    (blake2s-finalize state)))

(defun blake2s-hex (data &key (digest-length 32) (key nil) (start 0) (end nil))
  (let ((digest (blake2s data :digest-length digest-length :key key
                              :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))
