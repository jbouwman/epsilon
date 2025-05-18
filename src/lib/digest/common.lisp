(defpackage :epsilon.lib.digest.common
  (:use
   :cl
   :sb-rotate-byte
   :epsilon.lib.syntax
   :epsilon.lib.type
   :epsilon.lib.symbol)
  (:export
   :%add-with-carry
   :%subtract-with-borrow
   :copy-block
   :copy-to-buffer
   :fill-block-u8-be
   :fill-block-u8-be/64
   :fill-block-u8-le
   :fill-block-u8-le/64
   :mod32*
   :mod32+
   :mod32-
   :mod32ash
   :mod32lognot
   :mod64*
   :mod64+
   :mod64-
   :mod64ash
   :mod64lognot
   :rol32
   :rol64
   :ror32
   :ror64
   :ubref-fun-name
   :xor-block))

;;;; common.lisp -- efficient implementations of mod32 arithmetic and macros

(in-package :epsilon.lib.digest.common)

;;; extracting individual bytes from integers

;;; We used to declare these functions with much stricter types (e.g.
;;; (UNSIGNED-BYTE 32) as the lone argument), but we need to access
;;; bytes of both 32-bit and 64-bit words and the types would just get
;;; in our way.  We declare these functions as inline; a good Common
;;; Lisp compiler should be able to generate efficient code from the
;;; declarations at the point of the call.

;;; These functions are named according to big-endian conventions.  The
;;; comment is here because I always forget and need to be reminded.
#.(loop for i from 1 to 8
        collect (let ((name (intern (format nil  "~:@(~:R~)-~A"  i (string '#:byte)))))
                  `(progn
                    (declaim (inline ,name))
                    (declaim (ftype (function (unsigned-byte) u8) ,name))
                    (defun ,name (ub)
                      (declare (type unsigned-byte ub))
                      (ldb (byte 8 ,(* 8 (1- i))) ub)))) into forms
        finally (return `(progn ,@forms)))

;;; fetching/storing appropriately-sized integers from octet vectors

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun ubref-fun-name (bitsize big-endian-p)
  (symbolicate '#:u
               (princ-to-string bitsize)
               (if big-endian-p '#:ref/be '#:ref/le)))
) ; EVAL-WHEN


;; TODO compare this to lib.type version

#++
(defun u16ref/le (vector offset)       ; FIXME same as nibbles
  (declare (type ->u8 vector)
           (type array-index offset))
  (sb-sys:sap-ref-16 (sb-sys:vector-sap vector) offset))

;;; efficient 32-bit arithmetic, which a lot of algorithms require

(declaim #+ironclad-fast-mod32-arithmetic (inline mod32+)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)) mod32+))

(defun mod32+ (a b)
  (declare (type (unsigned-byte 32) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod32+ a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly))
  (ldb (byte 32 0) (+ a b)))

(define-compiler-macro mod32+ (a b)
  `(ldb (byte 32 0) (+ ,a ,b)))

;;; mostly needed for CAST*
(declaim #+ironclad-fast-mod32-arithmetic (inline mod32-)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)) mod32-))

(defun mod32- (a b)
  (declare (type (unsigned-byte 32) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod32- a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly))
  (ldb (byte 32 0) (- a b)))

(define-compiler-macro mod32- (a b)
  `(ldb (byte 32 0) (- ,a ,b)))

;;; mostly needed for RC6
(declaim #+ironclad-fast-mod32-arithmetic (inline mod32*)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)) mod32*))

(defun mod32* (a b)
  (declare (type (unsigned-byte 32) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod32* a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly))
  (ldb (byte 32 0) (* a b)))

(define-compiler-macro mod32* (a b)
  `(ldb (byte 32 0) (* ,a ,b)))

(declaim #+ironclad-fast-mod32-arithmetic (inline mod32ash)
         (ftype (function ((unsigned-byte 32) (integer -31 31)) (unsigned-byte 32)) mod32ash))

(defun mod32ash (num count)
  (declare (type (unsigned-byte 32) num)
           (type (integer -31 31) count))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod32ash num count)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly))
  (ldb (byte 32 0) (ash num count)))

#+sbcl
(define-compiler-macro mod32ash (num count)
  ;; work around SBCL optimizing bug as described by APD:
  ;;  http://www.caddr.com/macho/archives/sbcl-devel/2004-8/3877.html
  `(logand #xffffffff (ash ,num ,count)))

(declaim #+ironclad-fast-mod32-arithmetic (inline mod32lognot)
         (ftype (function ((unsigned-byte 32)) (unsigned-byte 32)) mod32lognot))

(defun mod32lognot (num)
  (declare (type (unsigned-byte 32) num))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod32lognot num)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly))
  (ldb (byte 32 0) (lognot num)))

#+sbcl
(define-compiler-macro mod32lognot (num)
  `(ldb (byte 32 0) (lognot ,num)))

(declaim #+ironclad-fast-mod32-arithmetic (inline rol32 ror32)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 5)) (unsigned-byte 32)) rol32 ror32))

(defun rol32 (a s)
  (declare (type (unsigned-byte 32) a)
           (type (integer 0 32) s))
  (sb-rotate-byte:rotate-byte s (byte 32 0) a))

(defun ror32 (a s)
  (declare (type (unsigned-byte 32) a)
           (type (integer 0 32) s))
  (sb-rotate-byte:rotate-byte (- s) (byte 32 0) a))

(declaim #+ironclad-fast-mod64-arithmetic (inline mod64+ mod64- mod64*)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64)) (unsigned-byte 64)) mod64+))

(defun mod64+ (a b)
  (declare (type (unsigned-byte 64) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod64+ a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly uint64-t))
  (ldb (byte 64 0) (+ a b)))

#+sbcl
(define-compiler-macro mod64+ (a b)
  `(ldb (byte 64 0) (+ ,a ,b)))

(defun mod64- (a b)
  (declare (type (unsigned-byte 64) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod64- a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly uint64-t))
  (ldb (byte 64 0) (- a b)))

#+sbcl
(define-compiler-macro mod64- (a b)
  `(ldb (byte 64 0) (- ,a ,b)))

(defun mod64* (a b)
  (declare (type (unsigned-byte 64) a b))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod64* a b)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly uint64-t))
  (ldb (byte 64 0) (* a b)))

#+sbcl
(define-compiler-macro mod64* (a b)
  `(ldb (byte 64 0) (* ,a ,b)))

(declaim #+ironclad-fast-mod64-arithmetic (inline mod64ash)
         (ftype (function ((unsigned-byte 64) (integer -63 63)) (unsigned-byte 64)) mod64ash))

(defun mod64ash (num count)
  (declare (type (unsigned-byte 64) num)
           (type (integer -63 63) count))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod64ash num count)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly uint64-t))
  (ldb (byte 64 0) (ash num count)))

#+sbcl
(define-compiler-macro mod64ash (num count)
  ;; work around SBCL optimizing bug as described by APD:
  ;;  http://www.caddr.com/macho/archives/sbcl-devel/2004-8/3877.html
  `(logand #xffffffffffffffff (ash ,num ,count)))

(declaim #+ironclad-fast-mod64-arithmetic (inline mod64lognot)
         (ftype (function ((unsigned-byte 64)) (unsigned-byte 64)) mod64lognot))

(defun mod64lognot (num)
  (declare (type (unsigned-byte 64) num))
  #+(and ccl x86-64 ironclad-assembly)
  (%mod64lognot num)

  #-(or (and ccl x86-64 ironclad-assembly)
        (and ecl ironclad-assembly uint64-t))
  (ldb (byte 64 0) (lognot num)))

#+sbcl
(define-compiler-macro mod64lognot (num)
  `(ldb (byte 64 0) (lognot ,num)))

(declaim #+ironclad-fast-mod64-arithmetic (inline rol64 ror64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 6)) (unsigned-byte 64)) rol64 ror64))

(defun rol64 (a s)
  (declare (type (unsigned-byte 64) a)
           (type (integer 0 64) s))
  (sb-rotate-byte:rotate-byte s (byte 64 0) a))

(defun ror64 (a s)
  (declare (type (unsigned-byte 64) a)
           (type (integer 0 64) s))
  (sb-rotate-byte:rotate-byte (- s) (byte 64 0) a))

;;; 64-bit utilities

(declaim #+ironclad-fast-mod32-arithmetic
         (inline %add-with-carry %subtract-with-borrow))

;;; The names are taken from sbcl and cmucl's bignum routines.
;;; Naturally, they work the same way (which means %SUBTRACT-WITH-BORROW
;;; is a little weird).
(defun %add-with-carry (x y carry)
  (declare (type (unsigned-byte 32) x y)
           (type (mod 2) carry))
  #+(and sbcl 32-bit)
  (sb-bignum:%add-with-carry x y carry)
  #+(and cmucl 32-bit)
  (bignum:%add-with-carry x y carry)
  #-(or (and sbcl 32-bit)
        (and cmucl 32-bit))
  (let* ((temp (mod32+ x y))
         (temp-carry (if (< temp x) 1 0))
         (result (mod32+ temp carry)))
    (values result (logior temp-carry (if (< result temp) 1 0)))))

(defun %subtract-with-borrow (x y borrow)
  (declare (type (unsigned-byte 32) x y)
           (type (mod 2) borrow))
  #+(and sbcl 32-bit)
  (sb-bignum:%subtract-with-borrow x y borrow)
  #+(and cmucl 32-bit)
  (bignum:%subtract-with-borrow x y borrow)
  #-(or (and sbcl 32-bit)
        (and cmucl 32-bit))
  (let ((temp (mod32- x y)))
    (cond
      ((zerop borrow)
       (values (mod32- temp 1) (if (< y x) 1 0)))
      (t
       (values temp (logxor (if (< x y) 1 0) 1))))))

;;; efficient 8-byte -> 32-byte buffer copy routines, mostly used by
;;; the hash functions.  we provide big-endian and little-endian
;;; versions.

(declaim (inline fill-block-le-u8 fill-block-be-u8))

(declaim (inline copy-to-buffer))
(defun copy-to-buffer (from from-offset count buffer buffer-offset)
  "Copy a partial segment from input vector from starting at
from-offset and copying count elements into the 64 byte buffer
starting at buffer-offset."
  (declare (type array-index from-offset)
           (type (integer 0 127) count buffer-offset)
           (type ->u8 from)
           (type ->u8 buffer))
  (sb-kernel:ub8-bash-copy from from-offset buffer buffer-offset count))

(defun fill-block-u8-le (block buffer offset)
  "Convert a complete 64 (UNSIGNED-BYTE 8) input BUFFER starting from
OFFSET into the given (UNSIGNED-BYTE 32) BLOCK."
  (declare (type (integer 0 #.(- array-dimension-limit 64)) offset)
           (type (->u32 16) block)
           (type ->u8 buffer))
  #+(and :sbcl :little-endian)
  (sb-kernel:ub8-bash-copy buffer offset block 0 64)
  #-(or (and :sbcl :little-endian) (and :cmu :little-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type array-index
        from offset to (+ offset 63) by 4
        do
        (setf (aref block i) (u32ref/le buffer j)))
  (values))

(defun fill-block-u8-be (block buffer offset)
  "Convert a complete 64 u8 input vector segment
starting from offset into the given 16 word SHA1 block.  Calling this function
without subsequently calling EXPAND-BLOCK results in undefined behavior."
  (declare (type (integer 0 #.(- array-dimension-limit 64)) offset)
           (type ->u32 block)
           (type ->u8 buffer))
  ;; convert to 32-bit words
  #+(and :cmu :big-endian)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :big-endian)
  (sb-kernel:ub8-bash-copy buffer offset block 0 64)
  #-(or (and :sbcl :big-endian) (and :cmu :big-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type array-index
        from offset to (+ offset 63) by 4
        do (setf (aref block i) (u32ref/be buffer j)))
  (values))

(defun fill-block-u8-le/64 (block buffer offset)
  "Convert a complete 128 u8 input vector segment
starting from offset into the given 16 qword SHA1 block.  Calling this
function without subsequently calling EXPAND-BLOCK results in undefined
behavior."
  (declare (type (array-index #.(- array-dimension-limit 64)) offset)
           (type ->u64 block)
           (type ->u8 buffer))
  ;; convert to 64-bit words
  #+(and :cmu :little-endian :64-bit)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :little-endian :64-bit)
  (sb-kernel:ub8-bash-copy buffer offset block 0 64)
  #-(or (and :sbcl :little-endian :64-bit) (and :cmu :little-endian :64-bit))
  (loop for i of-type (integer 0 8) from 0
        for j of-type array-index
        from offset to (+ offset 63) by 8
        do (setf (aref block i) (ub64ref/le buffer j)))
  (values))

(defun fill-block-u8-be/64 (block buffer offset)
  "Convert a complete 128 u8 input vector segment
starting from offset into the given 16 qword SHA1 block.  Calling this
function without subsequently calling EXPAND-BLOCK results in undefined
behavior."
  (declare (type (array-index #.(- array-dimension-limit 128)) offset)
           (type ->u64 block)
           (type ->u8 buffer))
  ;; convert to 64-bit words
  #+(and :cmu :big-endian :64-bit)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 128 vm:byte-bits))
  #+(and :sbcl :big-endian :64-bit)
  (sb-kernel:ub8-bash-copy buffer offset block 0 128)
  #-(or (and :sbcl :big-endian) (and :cmu :big-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type array-index
        from offset to (+ offset 127) by 8
        do (setf (aref block i) (u64ref/be buffer j)))
  (values))

(defun xor-block (block-length input-block1 input-block1-start input-block2 input-block2-start output-block output-block-start)
  (declare (type ->u8 input-block1 input-block2 output-block)
           (type array-index block-length input-block1-start input-block2-start output-block-start))
  (macrolet ((xor-bytes (size xor-form)
               `(loop until (< block-length ,size) do
                  ,xor-form
                  (incf output-block-start ,size)
                  (incf input-block1-start ,size)
                  (incf input-block2-start ,size)
                  (decf block-length ,size))))
    #+(and sbcl x86-64 ironclad-assembly)
    (xor-bytes 16 (xor128 input-block1 input-block1-start
                          input-block2 input-block2-start
                          output-block output-block-start))
    #+(and sbcl x86-64)
    (xor-bytes 8 (setf (u64ref/le output-block output-block-start)
                       (logxor (u64ref/le input-block1 input-block1-start)
                               (u64ref/le input-block2 input-block2-start))))
    #+(and sbcl (or x86 x86-64))
    (xor-bytes 4 (setf (u32ref/le output-block output-block-start)
                       (logxor (u32ref/le input-block1 input-block1-start)
                               (u32ref/le input-block2 input-block2-start))))
    (xor-bytes 1 (setf (aref output-block output-block-start)
                       (logxor (aref input-block1 input-block1-start)
                               (aref input-block2 input-block2-start))))))

(defun copy-block (block-length input-block input-block-start output-block output-block-start)
  (declare (type ->u8 input-block output-block)
           (type array-index block-length input-block-start output-block-start))
  (macrolet ((copy-bytes (size copy-form)
               `(loop until (< block-length ,size) do
                      ,copy-form
                      (incf input-block-start ,size)
                      (incf output-block-start ,size)
                      (decf block-length ,size))))
    #+(and sbcl x86-64 ironclad-assembly)
    (copy-bytes 16 (mov128 input-block input-block-start
                           output-block output-block-start))
    #+(and sbcl x86-64)
    (copy-bytes 8 (setf (u64ref/le output-block output-block-start)
                        (u64ref/le input-block input-block-start)))
    #+(and sbcl (or x86 x86-64))
    (copy-bytes 4 (setf (u32ref/le output-block output-block-start)
                        (u32ref/le input-block input-block-start)))
    (replace output-block input-block
             :start1 output-block-start :end1 (+ output-block-start block-length)
             :start2 input-block-start :end2 (+ input-block-start block-length))))
