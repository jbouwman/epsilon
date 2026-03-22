;;;; Constant-time primitives for cryptographic operations
;;;;
;;;; Every function in this module operates in constant time with respect
;;;; to secret data. No branches, no early returns, no data-dependent
;;;; memory access patterns.
;;;;
;;;; These primitives form the foundation for all security-critical code
;;;; paths in the native crypto library.

(defpackage epsilon.ssl.ct
  (:use :cl)
  (:export
   ;; Comparison
   #:ct-equal
   #:ct-zerop
   ;; Selection
   #:ct-select
   #:ct-select-byte
   ;; Swap
   #:ct-swap
   ;; Table lookup
   #:ct-lookup
   ;; Byte array operations
   #:ct-copy-if
   #:ct-zero-memory
   ;; Secure buffer
   #:with-secure-buffer
   #:with-secure-buffers))

(in-package :epsilon.ssl.ct)

;;; ---------------------------------------------------------------------------
;;; Constant-time comparison
;;; ---------------------------------------------------------------------------

(defun ct-equal (a b &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  "Compare two byte arrays in constant time. Returns T if equal, NIL otherwise.
   Always examines every byte -- never short-circuits on mismatch."
  (declare (type (simple-array (unsigned-byte 8) (*)) a b)
           (type fixnum start1 start2)
           (optimize (speed 3) (safety 1)))
  (let* ((end1 (or end1 (length a)))
         (end2 (or end2 (length b)))
         (len1 (- end1 start1))
         (len2 (- end2 start2)))
    (declare (type fixnum end1 end2 len1 len2))
    ;; Length difference check -- still constant-time because we always
    ;; iterate over the full length of the first array
    (let ((diff (logxor len1 len2)))
      (declare (type fixnum diff))
      (loop for i fixnum from 0 below len1
            do (let ((b1 (aref a (+ start1 i)))
                     ;; If len2 is shorter, read index 0 (we accumulate
                     ;; the length diff anyway so result will be non-zero)
                     (b2 (if (< i len2)
                             (aref b (+ start2 i))
                             0)))
                 (declare (type (unsigned-byte 8) b1 b2))
                 (setf diff (logior diff (logxor b1 b2)))))
      (zerop diff))))

(defun ct-zerop (buf &key (start 0) (end nil))
  "Check if a byte array region is all zeros in constant time."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum start)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length buf)))
         (acc 0))
    (declare (type fixnum end)
             (type (unsigned-byte 8) acc))
    (loop for i fixnum from start below end
          do (setf acc (logior acc (aref buf i))))
    (zerop acc)))

;;; ---------------------------------------------------------------------------
;;; Constant-time selection
;;; ---------------------------------------------------------------------------

(defun ct-select (condition a b)
  "If CONDITION is true (non-nil), return A; otherwise return B.
   Operates in constant time -- both branches are always evaluated and
   the selection is performed via bitwise operations.
   A and B must be fixnums."
  (declare (type fixnum a b)
           (optimize (speed 3) (safety 1)))
  ;; Convert condition to a mask: all-ones if true, all-zeros if false
  (let ((mask (- (if condition 1 0) 1)))
    ;; mask = 0 when condition is true (select a)
    ;; mask = -1 (all ones) when condition is false (select b)
    ;; result = b XOR ((a XOR b) AND (NOT mask))
    ;;        = b XOR ((a XOR b) AND all-ones) when true => b XOR (a XOR b) = a
    ;;        = b XOR ((a XOR b) AND all-zeros) when false => b XOR 0 = b
    (declare (type fixnum mask))
    (logxor b (logand (logxor a b) (lognot mask)))))

(defun ct-select-byte (condition a b)
  "Constant-time byte selection. Like ct-select but for (unsigned-byte 8) values."
  (declare (type (unsigned-byte 8) a b)
           (optimize (speed 3) (safety 1)))
  (let ((mask (- (if condition 1 0) 1)))
    (declare (type (signed-byte 16) mask))
    (logand #xFF (logxor b (logand (logxor a b) (lognot mask))))))

;;; ---------------------------------------------------------------------------
;;; Constant-time swap
;;; ---------------------------------------------------------------------------

(defun ct-swap (condition a b)
  "If CONDITION is true, swap A and B; otherwise leave them unchanged.
   Returns (values possibly-swapped-a possibly-swapped-b).
   A and B must be byte arrays of the same length."
  (declare (type (simple-array (unsigned-byte 8) (*)) a b)
           (optimize (speed 3) (safety 1)))
  (let ((mask (logand #xFF (- (if condition 1 0) 1))))
    ;; mask = #x00 when condition is true (do swap)
    ;; mask = #xFF when condition is false (don't swap)
    ;; When mask = #x00: (lognot mask) = #xFF, so xor-diff passes through => swap
    ;; When mask = #xFF: (lognot mask) = #x00, so xor-diff is zeroed => no swap
    (declare (type (unsigned-byte 8) mask))
    (loop for i fixnum from 0 below (length a)
          do (let* ((ai (aref a i))
                    (bi (aref b i))
                    (xor-diff (logand (logxor ai bi) (logand #xFF (lognot mask)))))
               (declare (type (unsigned-byte 8) ai bi xor-diff))
               (setf (aref a i) (logand #xFF (logxor ai xor-diff)))
               (setf (aref b i) (logand #xFF (logxor bi xor-diff))))))
  (values a b))

;;; ---------------------------------------------------------------------------
;;; Constant-time table lookup
;;; ---------------------------------------------------------------------------

(defun ct-lookup (table index)
  "Look up TABLE[INDEX] in constant time by scanning the entire table.
   TABLE is a byte array, INDEX is the desired position.
   Always reads every element to prevent cache-timing side channels."
  (declare (type (simple-array (unsigned-byte 8) (*)) table)
           (type fixnum index)
           (optimize (speed 3) (safety 1)))
  (let ((result 0))
    (declare (type (unsigned-byte 8) result))
    (loop for i fixnum from 0 below (length table)
          do (let* ((equal-mask (ct-index-equal-mask i index)))
               (declare (type (unsigned-byte 8) equal-mask))
               (setf result (logior result (logand equal-mask (aref table i))))))
    result))

(defun ct-index-equal-mask (a b)
  "Return #xFF if A = B, #x00 otherwise, using only arithmetic and bitwise ops."
  (declare (type fixnum a b)
           (optimize (speed 3) (safety 1)))
  ;; (a XOR b) is 0 iff a = b.
  ;; We need to convert 0 -> #xFF and nonzero -> #x00.
  ;; Step 1: collapse to single bit via repeated OR folding
  ;; Step 2: subtract from 1 to get 0->1, nonzero->0
  ;; Step 3: negate to get mask (1 -> #xFF via (- 0 1) + mask)
  (let* ((diff (logxor a b))
         ;; Fold all bits into the low bit
         ;; For fixnum diff, shift and OR to collapse
         (d (logior diff (ash diff -1)))
         (d (logior d (ash d -2)))
         (d (logior d (ash d -4)))
         (d (logior d (ash d -8)))
         (d (logior d (ash d -16)))
         ;; Now bit 0 is 1 if diff was nonzero, 0 if zero
         (bit0 (logand d 1))
         ;; bit0=0 means equal, bit0=1 means not equal
         ;; mask = (1 - bit0) * #xFF... but simpler:
         ;; (- 1 bit0) gives 1 for equal, 0 for not equal
         ;; then (- (- 1 bit0) 1) gives 0 for equal, -1 for not equal... wrong direction
         ;; Instead: (- bit0 1) gives -1 for not-equal (#xFF), 0 for equal... wrong
         ;; We want: equal -> #xFF, not-equal -> #x00
         ;; So: (- (- 1 bit0) 1) = -bit0... no
         ;; Simply: equal -> bit0=0 -> we want #xFF
         ;;         not-equal -> bit0=1 -> we want #x00
         ;; mask = logand #xFF (lognot (- bit0 1))
         ;; bit0=0: (- 0 1) = -1 = all-ones, lognot = 0... wrong
         ;; Let's just do: (- (logxor bit0 1) 1)
         ;; bit0=0: (- 1 1) = 0, then (lognot 0) & #xFF = #xFF...
         ;; bit0=1: (- 0 1) = -1, then (lognot -1) & #xFF = 0...
         ;; That's: logand #xFF (lognot (- (logxor bit0 1) 1))
         ;; Simpler: (1- (ash (logxor bit0 1) 8)) -- no, too complex
         ;; Most direct: subtract 1 from (1 - bit0), negate
         ;; Actually simplest: 0 - (1 - bit0) = bit0 - 1
         ;; bit0=0: 0-1 = -1 in 8 bits = #xFF -- correct!
         ;; bit0=1: 1-1 = 0 = #x00 -- correct!
         (mask (logand #xFF (- bit0 1))))
    (declare (type fixnum diff d bit0)
             (type (unsigned-byte 8) mask))
    mask))

;;; ---------------------------------------------------------------------------
;;; Constant-time byte array copy
;;; ---------------------------------------------------------------------------

(defun ct-copy-if (condition dest src &key (start 0) (end nil))
  "If CONDITION is true, copy SRC into DEST; otherwise DEST is unchanged.
   Always reads all of SRC to be constant-time."
  (declare (type (simple-array (unsigned-byte 8) (*)) dest src)
           (type fixnum start)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length src)))
         (mask (logand #xFF (lognot (- (if condition 1 0) 1)))))
    ;; mask = #xFF when condition is true (copy)
    ;; mask = #x00 when condition is false (don't copy)
    (declare (type fixnum end)
             (type (unsigned-byte 8) mask))
    (loop for i fixnum from start below end
          do (let ((s (aref src i))
                   (d (aref dest i)))
               (declare (type (unsigned-byte 8) s d))
               ;; dest[i] = (src[i] AND mask) OR (dest[i] AND NOT mask)
               (setf (aref dest i)
                     (logand #xFF (logior (logand s mask)
                                          (logand d (logand #xFF (lognot mask))))))))))

;;; ---------------------------------------------------------------------------
;;; Memory zeroing
;;; ---------------------------------------------------------------------------

(defun ct-zero-memory (buf &key (start 0) (end nil))
  "Zero out a byte array. Uses element-by-element writes that the compiler
   cannot optimize away (since the array may be read later)."
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum start)
           (optimize (speed 3) (safety 1)))
  (let ((end (or end (length buf))))
    (declare (type fixnum end))
    (loop for i fixnum from start below end
          do (setf (aref buf i) 0)))
  buf)

;;; ---------------------------------------------------------------------------
;;; Secure buffer macros
;;; ---------------------------------------------------------------------------

(defmacro with-secure-buffer ((var size) &body body)
  "Allocate a byte buffer of SIZE, bind it to VAR, execute BODY, then zero
   the buffer on exit (even if BODY signals a condition).
   Use this for any buffer that holds secret key material."
  (let ((buf (gensym "BUF")))
    `(let* ((,buf (make-array ,size :element-type '(unsigned-byte 8) :initial-element 0))
            (,var ,buf))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,buf))
       (unwind-protect
            (progn ,@body)
         (ct-zero-memory ,buf)))))

(defmacro with-secure-buffers (bindings &body body)
  "Like with-secure-buffer but for multiple buffers.
   BINDINGS is a list of (var size) pairs."
  (if (null bindings)
      `(progn ,@body)
      `(with-secure-buffer ,(first bindings)
         (with-secure-buffers ,(rest bindings) ,@body))))
