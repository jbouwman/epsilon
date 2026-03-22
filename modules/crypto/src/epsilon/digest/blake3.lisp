;;;; epsilon.digest.blake3 - BLAKE3 (RFC 9659)
;;;;
;;;; Pure-Lisp implementation of BLAKE3 (sequential mode).
;;;; Supports hash, keyed-hash (MAC), and key-derivation (KDF) modes.
;;;; Uses pre-computed message schedule to avoid per-round allocation.
;;;; Integrates with the epsilon.digest hasher protocol.

(defpackage epsilon.digest.blake3
  (:use :cl)
  (:require (epsilon.digest.protocol proto)
            (epsilon.typeclass tc)
            (epsilon.ssl.primitives prim))
  (:enter t))

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconstant +block-len+ 64)
(defconstant +chunk-len+ 1024)
(defconstant +key-len+ 32)
(defconstant +out-len+ 32)

;; Domain separation flags
(defconstant +chunk-start+          1)
(defconstant +chunk-end+            2)
(defconstant +parent+               4)
(defconstant +root+                 8)
(defconstant +keyed-hash+          16)
(defconstant +derive-key-context+  32)
(defconstant +derive-key-material+ 64)

;; IV (first 32 bits of fractional parts of square roots of first 8 primes)
(defparameter +iv+
  (make-array 8 :element-type '(unsigned-byte 32)
              :initial-contents '(#x6A09E667 #xBB67AE85 #x3C6EF372 #xA54FF53A
                                  #x510E527F #x9B05688C #x1F83D9AB #x5BE0CD19)))

;; Pre-computed 7-round message schedule.
;; Row i gives the 16 message-word indices for round i.
;; Computed by repeatedly applying the BLAKE3 permutation to (0..15).
(defparameter +msg-schedule+
  #2A(( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
      ( 2  6  3 10  7  0  4 13  1 11 12  5  9 14 15  8)
      ( 3  4 10 12 13  2  7 14  6  5  9  0 11 15  8  1)
      (10  7 12  9 14  3 13 15  4  0 11  2  5  8  1  6)
      (12 13  9 11 15 10 14  8  7  2  5  3  0  1  6  4)
      ( 9 14 11  5  8 12 15  1 13  3  0 10  2  6  4  7)
      (11 15  5  0  1  9  8  6 14 10  2 12  3  4  7 13)))

;;; ============================================================================
;;; Compression function
;;; ============================================================================

(defvar *compress-fn* nil
  "Optional SIMD-accelerated compression function.
   Signature: (cv block-words counter block-len flags result) -> result.
   When non-nil, blake3-compress dispatches to this function instead of
   the pure-Lisp implementation. Set by SIMD backend initialization.")

(defun blake3-compress (cv block-words counter block-len flags result)
  "BLAKE3 compression. Writes 16 u32 words into RESULT.
   Dispatches to *compress-fn* when available, otherwise uses pure Lisp."
  (if *compress-fn*
      (funcall *compress-fn* cv block-words counter block-len flags result)
      (blake3-compress-lisp cv block-words counter block-len flags result)))

(defun blake3-compress-lisp (cv block-words counter block-len flags result)
  "Pure-Lisp BLAKE3 compression. Writes 16 u32 words into RESULT."
  (declare (type (simple-array (unsigned-byte 32) (8)) cv)
           (type (simple-array (unsigned-byte 32) (16)) block-words result)
           (type (unsigned-byte 64) counter)
           (type fixnum block-len flags)
           (optimize (speed 3) (safety 1)))
  (let ((s0  (aref cv 0)) (s1  (aref cv 1)) (s2  (aref cv 2)) (s3  (aref cv 3))
        (s4  (aref cv 4)) (s5  (aref cv 5)) (s6  (aref cv 6)) (s7  (aref cv 7))
        (s8  (aref +iv+ 0)) (s9  (aref +iv+ 1)) (s10 (aref +iv+ 2)) (s11 (aref +iv+ 3))
        (s12 (logand prim:+u32-mask+ counter))
        (s13 (logand prim:+u32-mask+ (ash counter -32)))
        (s14 (logand prim:+u32-mask+ block-len))
        (s15 (logand prim:+u32-mask+ flags)))
    (declare (type (unsigned-byte 32) s0 s1 s2 s3 s4 s5 s6 s7
                   s8 s9 s10 s11 s12 s13 s14 s15))
    (macrolet ((g (a b c d mx my)
                 `(progn
                    (setf ,a (prim:u32+ ,a ,b ,mx))
                    (setf ,d (prim:u32-rotr (logxor ,d ,a) 16))
                    (setf ,c (prim:u32+ ,c ,d))
                    (setf ,b (prim:u32-rotr (logxor ,b ,c) 12))
                    (setf ,a (prim:u32+ ,a ,b ,my))
                    (setf ,d (prim:u32-rotr (logxor ,d ,a) 8))
                    (setf ,c (prim:u32+ ,c ,d))
                    (setf ,b (prim:u32-rotr (logxor ,b ,c) 7))))
              (msg (round-idx col)
                `(aref block-words (aref +msg-schedule+ ,round-idx ,col)))
              (do-round (r)
                `(progn
                   ;; Column step
                   (g s0 s4 s8  s12 (msg ,r  0) (msg ,r  1))
                   (g s1 s5 s9  s13 (msg ,r  2) (msg ,r  3))
                   (g s2 s6 s10 s14 (msg ,r  4) (msg ,r  5))
                   (g s3 s7 s11 s15 (msg ,r  6) (msg ,r  7))
                   ;; Diagonal step
                   (g s0 s5 s10 s15 (msg ,r  8) (msg ,r  9))
                   (g s1 s6 s11 s12 (msg ,r 10) (msg ,r 11))
                   (g s2 s7 s8  s13 (msg ,r 12) (msg ,r 13))
                   (g s3 s4 s9  s14 (msg ,r 14) (msg ,r 15)))))
      ;; 7 rounds
      (do-round 0) (do-round 1) (do-round 2) (do-round 3)
      (do-round 4) (do-round 5) (do-round 6))
    ;; Output: state[i] XOR state[i+8], then state[i+8] XOR cv[i]
    (setf (aref result  0) (logxor s0 s8)   (aref result  1) (logxor s1 s9)
          (aref result  2) (logxor s2 s10)   (aref result  3) (logxor s3 s11)
          (aref result  4) (logxor s4 s12)   (aref result  5) (logxor s5 s13)
          (aref result  6) (logxor s6 s14)   (aref result  7) (logxor s7 s15)
          (aref result  8) (logxor s8  (aref cv 0))
          (aref result  9) (logxor s9  (aref cv 1))
          (aref result 10) (logxor s10 (aref cv 2))
          (aref result 11) (logxor s11 (aref cv 3))
          (aref result 12) (logxor s12 (aref cv 4))
          (aref result 13) (logxor s13 (aref cv 5))
          (aref result 14) (logxor s14 (aref cv 6))
          (aref result 15) (logxor s15 (aref cv 7)))
    result))

;;; ============================================================================
;;; Byte/word conversion helpers
;;; ============================================================================

(defun load-block-words (buf offset count work)
  "Read COUNT bytes from BUF at OFFSET into WORK as LE u32 words.
   Zeroes any trailing words/bytes beyond COUNT."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum offset count)
           (type (simple-array (unsigned-byte 32) (16)) work))
  ;; Zero the work array
  (loop for i fixnum from 0 below 16 do (setf (aref work i) 0))
  ;; Load full 4-byte words
  (let ((full-words (ash count -2))
        (remaining (logand count 3)))
    (declare (type fixnum full-words remaining))
    (loop for i fixnum from 0 below full-words
          for base fixnum = (+ offset (ash i 2))
          do (setf (aref work i)
                   (logior (aref buf base)
                           (ash (aref buf (+ base 1)) 8)
                           (ash (aref buf (+ base 2)) 16)
                           (ash (aref buf (+ base 3)) 24))))
    ;; Load partial trailing word
    (when (> remaining 0)
      (let ((base (+ offset (ash full-words 2)))
            (w 0))
        (declare (type fixnum base) (type (unsigned-byte 32) w))
        (loop for j fixnum from 0 below remaining
              do (setf w (logior w (ash (aref buf (+ base j)) (* j 8)))))
        (setf (aref work full-words) w))))
  work)

(defun words32-to-bytes (words byte-count output output-offset)
  "Write BYTE-COUNT bytes from WORDS (little-endian) into OUTPUT at OUTPUT-OFFSET."
  (declare (type (simple-array (unsigned-byte 32) (*)) words)
           (type fixnum byte-count output-offset)
           (type (simple-array (unsigned-byte 8) (*)) output))
  (loop for i fixnum from 0 below byte-count
        do (setf (aref output (+ output-offset i))
                 (logand #xFF (ash (aref words (ash i -2))
                                   (- (ash (logand i 3) 3)))))))

;;; ============================================================================
;;; Chunk state
;;; ============================================================================

(defstruct (chunk-state (:constructor %make-chunk-state))
  "State for processing a single chunk (up to 1024 bytes)."
  (cv (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (8)))
  (chunk-counter 0 :type (unsigned-byte 64))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (64)))
  (buffer-len 0 :type fixnum)
  (blocks-compressed 0 :type fixnum)
  (flags 0 :type fixnum)
  ;; Pre-allocated scratch arrays
  (work-block (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (16)))
  (work-result (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (16))))

(defun make-chunk-state (key-words chunk-counter flags)
  (declare (type (simple-array (unsigned-byte 32) (8)) key-words)
           (type (unsigned-byte 64) chunk-counter)
           (type fixnum flags))
  (let ((cs (%make-chunk-state)))
    (replace (chunk-state-cv cs) key-words)
    (setf (chunk-state-chunk-counter cs) chunk-counter
          (chunk-state-flags cs) flags)
    cs))

(defun chunk-state-len (cs)
  "Total bytes fed into this chunk so far."
  (+ (* +block-len+ (chunk-state-blocks-compressed cs))
     (chunk-state-buffer-len cs)))

(declaim (inline chunk-state-start-flag))
(defun chunk-state-start-flag (cs)
  (if (= (chunk-state-blocks-compressed cs) 0) +chunk-start+ 0))

(defun chunk-state-update (cs data start end)
  "Feed bytes [START, END) from DATA into chunk state CS."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end))
  (let ((pos start)
        (work-block (chunk-state-work-block cs))
        (work-result (chunk-state-work-result cs)))
    (declare (type fixnum pos)
             (type (simple-array (unsigned-byte 32) (16)) work-block work-result))
    (loop while (< pos end) do
      (let ((buf-len (chunk-state-buffer-len cs)))
        ;; If buffer is full, compress it
        (when (= buf-len +block-len+)
          ;; Load buffer into work-block
          (loop for i fixnum from 0 below 16
                for base fixnum = (ash i 2)
                do (setf (aref work-block i)
                         (logior (aref (chunk-state-buffer cs) base)
                                 (ash (aref (chunk-state-buffer cs) (+ base 1)) 8)
                                 (ash (aref (chunk-state-buffer cs) (+ base 2)) 16)
                                 (ash (aref (chunk-state-buffer cs) (+ base 3)) 24))))
          (blake3-compress (chunk-state-cv cs)
                           work-block
                           (chunk-state-chunk-counter cs)
                           +block-len+
                           (logior (chunk-state-flags cs)
                                   (chunk-state-start-flag cs))
                           work-result)
          ;; First 8 words become the new cv
          (loop for i fixnum from 0 below 8
                do (setf (aref (chunk-state-cv cs) i) (aref work-result i)))
          (incf (chunk-state-blocks-compressed cs))
          (setf (chunk-state-buffer-len cs) 0)
          (setf buf-len 0))
        ;; Copy bytes into buffer
        (let ((want (min (- +block-len+ buf-len) (- end pos))))
          (declare (type fixnum want))
          (replace (chunk-state-buffer cs) data
                   :start1 buf-len :end1 (+ buf-len want)
                   :start2 pos :end2 (+ pos want))
          (incf (chunk-state-buffer-len cs) want)
          (incf pos want))))))

(defun chunk-state-output (cs)
  "Return the output node for a completed chunk.
   Returns (values cv block-words counter block-len flags).
   The block-words are loaded into the chunk's work-block scratch array."
  (let ((work-block (chunk-state-work-block cs)))
    (declare (type (simple-array (unsigned-byte 32) (16)) work-block))
    (load-block-words (chunk-state-buffer cs) 0
                      (chunk-state-buffer-len cs) work-block)
    (values (chunk-state-cv cs)
            work-block
            (chunk-state-chunk-counter cs)
            (chunk-state-buffer-len cs)
            (logior (chunk-state-flags cs)
                    (chunk-state-start-flag cs)
                    +chunk-end+))))

;;; ============================================================================
;;; Parent node helpers
;;; ============================================================================

(defun parent-cv (left-cv right-cv key-words flags work-block work-result)
  "Merge two child CVs into a parent CV. Returns WORK-RESULT (first 8 words valid)."
  (declare (type (simple-array (unsigned-byte 32) (8)) left-cv right-cv key-words)
           (type (simple-array (unsigned-byte 32) (16)) work-block work-result)
           (type fixnum flags))
  (replace work-block left-cv :end1 8)
  (loop for i fixnum from 0 below 8
        do (setf (aref work-block (+ i 8)) (aref right-cv i)))
  (blake3-compress key-words work-block 0 +block-len+
                   (logior flags +parent+) work-result)
  work-result)

;;; ============================================================================
;;; Hasher state
;;; ============================================================================

(defstruct (blake3-state (:constructor %make-blake3-state))
  (key-words (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (8)))
  (chunk (make-chunk-state +iv+ 0 0) :type chunk-state)
  (cv-stack (make-array 54 :initial-element nil) :type simple-vector)
  (cv-stack-len 0 :type fixnum)
  (flags 0 :type fixnum)
  ;; Scratch arrays for parent-cv merges during update/finalize
  (parent-block (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (16)))
  (parent-result (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)
   :type (simple-array (unsigned-byte 32) (16))))

(defun new-internal (key-words flags)
  "Create a BLAKE3 hasher with the given key words and flags."
  (declare (type (simple-array (unsigned-byte 32) (8)) key-words)
           (type fixnum flags))
  (let ((state (%make-blake3-state)))
    (replace (blake3-state-key-words state) key-words)
    (setf (blake3-state-flags state) flags)
    (setf (blake3-state-chunk state) (make-chunk-state key-words 0 flags))
    state))

(defun make-blake3-state ()
  "Create a BLAKE3 hasher for the regular hash function."
  (new-internal +iv+ 0))

(defun make-blake3-keyed-state (key)
  "Create a BLAKE3 hasher for keyed hashing (MAC). KEY must be 32 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (assert (= (length key) +key-len+))
  (let ((key-words (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 32) (8)) key-words))
    (loop for i fixnum from 0 below 8
          for base fixnum = (ash i 2)
          do (setf (aref key-words i) (prim:read-le32 key base)))
    (new-internal key-words +keyed-hash+)))

(defun make-blake3-derive-key-state (context)
  "Create a BLAKE3 hasher for key derivation. CONTEXT is an ASCII string."
  (let* ((context-bytes (map '(simple-array (unsigned-byte 8) (*))
                             #'char-code context))
         (context-hasher (new-internal +iv+ +derive-key-context+)))
    (declare (type (simple-array (unsigned-byte 8) (*)) context-bytes))
    (blake3-update context-hasher context-bytes)
    (let* ((context-key (blake3-finalize context-hasher))
           (context-key-words (make-array 8 :element-type '(unsigned-byte 32)
                                          :initial-element 0)))
      (declare (type (simple-array (unsigned-byte 8) (*)) context-key)
               (type (simple-array (unsigned-byte 32) (8)) context-key-words))
      (loop for i fixnum from 0 below 8
            for base fixnum = (ash i 2)
            do (setf (aref context-key-words i) (prim:read-le32 context-key base)))
      (new-internal context-key-words +derive-key-material+))))

;;; ============================================================================
;;; Push CV (Merkle tree merge)
;;; ============================================================================

(defun push-cv (state new-cv total-chunks)
  "Add a chunk CV and merge completed subtrees.
   The number of trailing 0-bits in total-chunks tells how many merges to do."
  (declare (type (simple-array (unsigned-byte 32) (8)) new-cv)
           (type (unsigned-byte 64) total-chunks))
  (let ((cv (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0))
        (parent-block (blake3-state-parent-block state))
        (parent-result (blake3-state-parent-result state)))
    (declare (type (simple-array (unsigned-byte 32) (8)) cv)
             (type (simple-array (unsigned-byte 32) (16)) parent-block parent-result))
    (replace cv new-cv)
    (loop while (evenp total-chunks) do
      (let* ((stack-idx (1- (blake3-state-cv-stack-len state)))
             (left (aref (blake3-state-cv-stack state) stack-idx)))
        (parent-cv left cv (blake3-state-key-words state)
                   (blake3-state-flags state) parent-block parent-result)
        ;; Copy first 8 words of result into cv
        (loop for i fixnum from 0 below 8
              do (setf (aref cv i) (aref parent-result i)))
        (decf (blake3-state-cv-stack-len state)))
      (setf total-chunks (ash total-chunks -1)))
    ;; Push onto stack (copy to avoid aliasing)
    (let ((stack-cv (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
      (declare (type (simple-array (unsigned-byte 32) (8)) stack-cv))
      (replace stack-cv cv)
      (setf (aref (blake3-state-cv-stack state) (blake3-state-cv-stack-len state))
            stack-cv)
      (incf (blake3-state-cv-stack-len state)))))

;;; ============================================================================
;;; Update
;;; ============================================================================

(defun blake3-update (state data &key (start 0) (end nil))
  "Feed data into BLAKE3."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start))
  (let ((end (or end (length data)))
        (pos start))
    (declare (type fixnum end pos))
    (loop while (< pos end) do
      (let ((cs (blake3-state-chunk state)))
        ;; If current chunk is full, finalize and start new one
        (when (= (chunk-state-len cs) +chunk-len+)
          (multiple-value-bind (cv block-words counter block-len flags)
              (chunk-state-output cs)
            (declare (ignore counter))
            (let ((chunk-cv (make-array 8 :element-type '(unsigned-byte 32)
                                        :initial-element 0))
                  (compress-result (make-array 16 :element-type '(unsigned-byte 32)
                                               :initial-element 0)))
              (declare (type (simple-array (unsigned-byte 32) (8)) chunk-cv)
                       (type (simple-array (unsigned-byte 32) (16)) compress-result))
              (blake3-compress cv block-words
                               (chunk-state-chunk-counter cs)
                               block-len flags compress-result)
              (loop for i fixnum from 0 below 8
                    do (setf (aref chunk-cv i) (aref compress-result i)))
              (push-cv state chunk-cv (1+ (chunk-state-chunk-counter cs)))))
          ;; Start new chunk
          (let ((new-counter (1+ (chunk-state-chunk-counter cs))))
            (setf (blake3-state-chunk state)
                  (make-chunk-state (blake3-state-key-words state)
                                    new-counter
                                    (blake3-state-flags state)))
            (setf cs (blake3-state-chunk state))))
        ;; Feed into current chunk
        (let ((want (min (- end pos)
                         (- +chunk-len+ (chunk-state-len cs)))))
          (declare (type fixnum want))
          (chunk-state-update cs data pos (+ pos want))
          (incf pos want)))))
  state)

;;; ============================================================================
;;; Finalize
;;; ============================================================================

(defun blake3-finalize (state &key (output-length 32))
  "Finalize BLAKE3 and return OUTPUT-LENGTH bytes."
  (declare (type fixnum output-length))
  (let ((cs (blake3-state-chunk state)))
    ;; Get output node from current (possibly partial) chunk
    (multiple-value-bind (out-cv out-block out-counter out-block-len out-flags)
        (chunk-state-output cs)
      ;; We need to work with copies to avoid mutating state
      (let ((cv (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0))
            (block-words (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0))
            (counter out-counter)
            (block-len out-block-len)
            (flags out-flags)
            (stack-len (blake3-state-cv-stack-len state))
            (compress-buf (make-array 16 :element-type '(unsigned-byte 32) :initial-element 0)))
        (declare (type (simple-array (unsigned-byte 32) (8)) cv)
                 (type (simple-array (unsigned-byte 32) (16)) block-words compress-buf)
                 (type fixnum block-len flags stack-len)
                 (type (unsigned-byte 64) counter))
        (replace cv out-cv)
        (replace block-words out-block)
        ;; Walk cv-stack from top to bottom, merging
        (loop while (> stack-len 0) do
          ;; Compress current output node to get its CV
          (blake3-compress cv block-words counter block-len flags compress-buf)
          (let ((chunk-cv (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
            (declare (type (simple-array (unsigned-byte 32) (8)) chunk-cv))
            (loop for i fixnum from 0 below 8
                  do (setf (aref chunk-cv i) (aref compress-buf i)))
            (decf stack-len)
            (let ((left (aref (blake3-state-cv-stack state) stack-len)))
              ;; Create parent output node
              (replace block-words left :end1 8)
              (loop for i fixnum from 0 below 8
                    do (setf (aref block-words (+ i 8)) (aref chunk-cv i)))
              (replace cv (blake3-state-key-words state))
              (setf counter 0)
              (setf block-len +block-len+)
              (setf flags (logior (blake3-state-flags state) +parent+)))))
        ;; Produce XOF output from root node
        (let ((output (make-array output-length :element-type '(unsigned-byte 8)
                                  :initial-element 0))
              (output-offset 0))
          (declare (type (simple-array (unsigned-byte 8) (*)) output)
                   (type fixnum output-offset))
          (loop for blk-counter from 0
                while (< output-offset output-length) do
            (blake3-compress cv block-words blk-counter block-len
                             (logior flags +root+) compress-buf)
            (let ((want (min 64 (- output-length output-offset))))
              (words32-to-bytes compress-buf want output output-offset)
              (incf output-offset want)))
          output)))))

;;; ============================================================================
;;; Copy
;;; ============================================================================

(defun blake3-copy (state)
  "Return an independent deep copy of a BLAKE3 hasher state."
  (let ((new (%make-blake3-state)))
    ;; Copy key-words
    (replace (blake3-state-key-words new) (blake3-state-key-words state))
    (setf (blake3-state-flags new) (blake3-state-flags state))
    (setf (blake3-state-cv-stack-len new) (blake3-state-cv-stack-len state))
    ;; Deep copy chunk state
    (let ((old-cs (blake3-state-chunk state))
          (new-cs (%make-chunk-state)))
      (replace (chunk-state-cv new-cs) (chunk-state-cv old-cs))
      (setf (chunk-state-chunk-counter new-cs) (chunk-state-chunk-counter old-cs))
      (replace (chunk-state-buffer new-cs) (chunk-state-buffer old-cs))
      (setf (chunk-state-buffer-len new-cs) (chunk-state-buffer-len old-cs))
      (setf (chunk-state-blocks-compressed new-cs) (chunk-state-blocks-compressed old-cs))
      (setf (chunk-state-flags new-cs) (chunk-state-flags old-cs))
      (replace (chunk-state-work-block new-cs) (chunk-state-work-block old-cs))
      (replace (chunk-state-work-result new-cs) (chunk-state-work-result old-cs))
      (setf (blake3-state-chunk new) new-cs))
    ;; Deep copy cv-stack entries
    (loop for i fixnum from 0 below (blake3-state-cv-stack-len state)
          for entry = (aref (blake3-state-cv-stack state) i)
          when entry do
            (let ((copy (make-array 8 :element-type '(unsigned-byte 32) :initial-element 0)))
              (replace copy entry)
              (setf (aref (blake3-state-cv-stack new) i) copy)))
    ;; Copy parent scratch arrays
    (replace (blake3-state-parent-block new) (blake3-state-parent-block state))
    (replace (blake3-state-parent-result new) (blake3-state-parent-result state))
    new))

;;; ============================================================================
;;; One-shot API
;;; ============================================================================

(defun blake3 (data &key (output-length 32) (start 0) (end nil))
  "Compute BLAKE3 hash of DATA. Returns OUTPUT-LENGTH bytes (default 32)."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((state (make-blake3-state)))
    (blake3-update state data :start start :end end)
    (blake3-finalize state :output-length output-length)))

(defun blake3-hex (data &key (output-length 32) (start 0) (end nil))
  "Compute BLAKE3 hash of DATA. Returns lowercase hex string."
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((digest (blake3 data :output-length output-length :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

(defun blake3-keyed (key data &key (output-length 32) (start 0) (end nil))
  "Compute BLAKE3 keyed hash (MAC). KEY must be 32 bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data))
  (let ((state (make-blake3-keyed-state key)))
    (blake3-update state data :start start :end end)
    (blake3-finalize state :output-length output-length)))

(defun blake3-keyed-hex (key data &key (output-length 32) (start 0) (end nil))
  "Compute BLAKE3 keyed hash (MAC). Returns lowercase hex string."
  (declare (type (simple-array (unsigned-byte 8) (*)) key data))
  (let ((digest (blake3-keyed key data :output-length output-length
                              :start start :end end)))
    (with-output-to-string (s)
      (loop for byte across digest do (format s "~(~2,'0x~)" byte)))))

(defun blake3-derive-key (context ikm &key (output-length 32))
  "Derive key material using BLAKE3 KDF.
   CONTEXT is an ASCII string, IKM is key material bytes."
  (declare (type string context)
           (type (simple-array (unsigned-byte 8) (*)) ikm))
  (let ((state (make-blake3-derive-key-state context)))
    (blake3-update state ikm)
    (blake3-finalize state :output-length output-length)))

;;; ============================================================================
;;; XOF (Extendable Output Function) Support
;;; ============================================================================

(defun blake3-xof (data output-length &key (start 0) (end (length data)))
  "Compute BLAKE3 hash with arbitrary output length."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum output-length start end))
  (blake3 data :start start :end end :output-length output-length))

(defun blake3-xof-seek (data seek output-length &key (start 0) (end (length data)))
  "Compute BLAKE3 output starting at SEEK bytes into the output stream."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end seek output-length))
  (let ((full (blake3 data :start start :end end
                      :output-length (+ seek output-length))))
    (subseq full seek)))

;;; ============================================================================
;;; Hasher Protocol Integration
;;; ============================================================================

(defstruct (blake3-hasher (:constructor %make-blake3-hasher))
  "BLAKE3 stateful hasher implementing the digest protocol."
  (state nil :type (or null blake3-state))
  (mode :hash :type keyword)
  (key nil :type (or null (simple-array (unsigned-byte 8) (32))))
  (context nil :type (or null string))
  (finalized-p nil :type boolean))

(defun make-blake3-hasher (&key key context)
  "Create a new BLAKE3 hasher.

   Without arguments: regular hashing mode
   With KEY: keyed hashing mode (MAC) - key must be 32 bytes
   With CONTEXT: key derivation mode (KDF) - context is a string

   KEY and CONTEXT are mutually exclusive."
  (when (and key context)
    (error 'proto:digest-error
           :message "Cannot specify both :key and :context"))
  (when (and key (not (= (length key) 32)))
    (error 'proto:invalid-key-error
           :expected 32
           :actual (length key)))
  (let* ((mode (cond (key :keyed)
                     (context :derive)
                     (t :hash)))
         (state (ecase mode
                  (:hash (make-blake3-state))
                  (:keyed (make-blake3-keyed-state key))
                  (:derive (make-blake3-derive-key-state context)))))
    (%make-blake3-hasher :state state
                         :mode mode
                         :key (when key (copy-seq key))
                         :context context)))

(tc:definstance proto:hasher blake3-hasher
  (proto:hasher-update (hasher data &key (start 0) (end (length data)))
    (when (blake3-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (blake3-update (blake3-hasher-state hasher) data :start start :end end)
    hasher)

  (proto:hasher-finalize (hasher &key output (output-length 32))
    (when (blake3-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (setf (blake3-hasher-finalized-p hasher) t)
    (let ((result (blake3-finalize (blake3-hasher-state hasher)
                                   :output-length output-length)))
      (when output
        (replace output result))
      (if output output result)))

  (proto:hasher-reset (hasher)
    (setf (blake3-hasher-state hasher)
          (ecase (blake3-hasher-mode hasher)
            (:hash (make-blake3-state))
            (:keyed (make-blake3-keyed-state (blake3-hasher-key hasher)))
            (:derive (make-blake3-derive-key-state (blake3-hasher-context hasher)))))
    (setf (blake3-hasher-finalized-p hasher) nil)
    hasher)

  (proto:hasher-copy (hasher)
    (when (blake3-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (%make-blake3-hasher :state (blake3-copy (blake3-hasher-state hasher))
                         :mode (blake3-hasher-mode hasher)
                         :key (blake3-hasher-key hasher)
                         :context (blake3-hasher-context hasher)))

  (proto:hasher-algorithm (hasher)
    (ecase (blake3-hasher-mode hasher)
      (:hash :blake3)
      (:keyed :blake3-keyed)
      (:derive :blake3-derive)))

  (proto:hasher-output-length (hasher)
    (declare (ignore hasher))
    32)

  (proto:hasher-block-length (hasher)
    (declare (ignore hasher))
    64))

;;; ============================================================================
;;; Hash-Bytes Methods
;;; ============================================================================

(defmethod proto:hash-bytes ((algorithm (eql :blake3)) data &key (start 0) (end (length data))
                                                               key (output-length 32))
  "Compute BLAKE3 hash via generic interface."
  (if key
      (blake3-keyed key data :start start :end end :output-length output-length)
      (blake3 data :start start :end end :output-length output-length)))

(defmethod proto:hash-bytes ((algorithm (eql :blake3-keyed)) data &key (start 0) (end (length data))
                                                                      key (output-length 32))
  "Compute keyed BLAKE3 hash via generic interface."
  (unless key
    (error 'proto:digest-error :message ":blake3-keyed requires a key"))
  (blake3-keyed key data :start start :end end :output-length output-length))

(defmethod proto:hash-bytes ((algorithm (eql :blake3-derive)) data &key (start 0) (end (length data))
                                                                       key (output-length 32))
  "Compute BLAKE3 key derivation via generic interface.
   KEY should be the context string for this use."
  (unless key
    (error 'proto:digest-error :message ":blake3-derive requires a context string as :key"))
  (blake3-derive-key key (subseq data start end) :output-length output-length))
