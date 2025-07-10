;;;; BZip2 Compression and Decompression
;;;;
;;;; This module provides BZip2 decompression support extracted from
;;;; the codec module. Currently only decompression is implemented.
;;;;
;;;; The implementation follows the bzip2 file format specification
;;;; and provides streaming decompression capabilities.

(defpackage :epsilon.lib.bzip
  (:use :cl)
  (:local-nicknames
   (:type :epsilon.lib.type)
   (:checksum :epsilon.lib.checksum.generic)
   (:crc32 :epsilon.lib.checksum.crc-32))
  (:export
   ;; Main API
   :decompress
   :make-decompressing-stream
   :bzip2-codec
   
   ;; Conditions
   :invalid-bzip2-data
   :bzip2-randomized-blocks-unimplemented
   
   ;; Low-level API
   :make-bzip2-state
   :bzip2-state
   :bzip2-state-p))

(in-package :epsilon.lib.bzip)

;;; Type imports
(deftype u8 () '(unsigned-byte 8))
(deftype u16 () '(unsigned-byte 16))
(deftype u32 () '(unsigned-byte 32))
(deftype u64 () '(unsigned-byte 64))
(deftype ->u8 (&optional (size '*)) `(simple-array u8 (,size)))
(deftype ->u16 (&optional (size '*)) `(simple-array u16 (,size)))
(deftype ->u32 (&optional (size '*)) `(simple-array u32 (,size)))

;;; Constants

(defconstant +100k+ 100000)
(defconstant +bz-max-alpha-size+ 258)
(defconstant +bz-max-code-len+ 23)
(defconstant +bz-runa+ 0)
(defconstant +bz-runb+ 1)
(defconstant +bz-n-groups+ 6)
(defconstant +bz-g-size+ 50)
(defconstant +bz-n-iters+ 4)
(defconstant +bz-max-selectors+ (+ 2 (/ (* 9 +100k+) +bz-g-size+)))
(defconstant +mtfa-size+ 4096)
(defconstant +mtfl-size+ 16)

;;; BZip2 CRC32 table
(defconstant +bzip2-crc32-table+
  (coerce '(#x00000000 #x04c11db7 #x09823b6e #x0d4326d9
            #x130476dc #x17c56b6b #x1a864db2 #x1e475005
            #x2608edb8 #x22c9f00f #x2f8ad6d6 #x2b4bcb61
            #x52b5e466 #x565e7dd1 #x5b1d4b08 #x5fdc56bf
            #x67938b3a #x635296dd #x6e11b004 #x6ad0adb3
            #x72cf105e #x765e0de9 #x7b1d2b30 #x7fdc3687
            #x97138322 #x93d29e95 #x9e91b84c #x9a50a5fb
            #x8241b21c #x86808fab #x8bc3a972 #x8f02b4c5
            #x874d0978 #x838c14cf #x8ecf3216 #x8a0e2fa1
            #x92f5c2a4 #x9634df13 #x9b77f9ca #x9fb6e47d
            #xa7f939c0 #xa338247f #xaefb0286 #xaa3a1f31
            #xb22ba234 #xb6eabf83 #xbba9995a #xbf6884ed
            #xb7273950 #xb3e624e7 #xbea5023e #xba641f89
            #xa29ff28c #xa65eef3b #xab1dc9e2 #xafdcd455
            #xa7cd57e8 #xa30c4a5f #xae4f6c86 #xaa8e7131
            #x520d46a4 #x56cc5b13 #x5b8f7dca #x5f4e607d
            #x475fd790 #x439eca27 #x4eddfcfe #x4a1ce149
            #x42535cf4 #x46924143 #x4bd1679a #x4f107a2d
            #x57eb9d28 #x532a809f #x5e69a646 #x5aa8bbf1
            #x62e7664c #x664677bf #x6b055166 #x6fc44cd1
            #x77dfb164 #x731eacd3 #x7e5d8a0a #x7a9c9fbd
            #x8247725a #x8686669d #x8bc54044 #x8f045df3
            #x97fbe04e #x9339fdf9 #x9e7adb20 #x9abbc697
            #x82f47b92 #x86356625 #x8b7640fc #x8fb75d4b
            #x87f8e0f6 #x8339fd41 #x8e7adb98 #x8abbc62f
            #xd24fee1a #xd68ee3ad #xdb8dc574 #xdf1cd8c3
            #xc75d657e #xc39c78c9 #xcadf5e10 #xce1e43a7
            #xc651fea2 #xc290e315 #xcfd3c5cc #xcb12d87b
            #xd3e935c6 #xd72a2871 #xda690ea8 #xdea8131f
            #xe6e7ce9a #xe226d32d #xef65f5f4 #xeba4e843
            #xf3b555fe #xf7744649 #xfa376090 #xfef67d27
            #xf6b9c022 #xf278dd95 #xff3bfb4c #xfbfae20b
            #xe3010fb6 #xe7c01201 #xeac334d8 #xee02296f
            #xa4308a5a #xa0f197ed #xadv2b134 #xa9309c83
            #xb121213e #xb5e03c89 #xb8a31850 #xbc6205e7
            #xb42db8ea #xb0eca55d #xbd9f8384 #xb94e9e33
            #xa1b5738e #xa5746e39 #xa8377ae0 #xacf66557
            #x74551552 #x70a408e5 #x7de72e3c #x7926338b
            #x61378436 #x65f69981 #x68b5bf58 #x6c74a2ef
            #x643b1fa2 #x60fa0215 #x6db924cc #x6978397b
            #x7183d49e #x751ec929 #x785dedf0 #x7c9cf047
            #x44d32dc2 #x401d3075 #x4d5e16ac #x49af0b1b
            #x51bebca6 #x557da111 #x586e87c8 #x5caf9a7f
            #x54e0277a #x50213acd #x5d621c14 #x59a301a3
            #x4158ec1e #x45e9f1a9 #x48aad770 #x4c6bcac7
            #xf42467d2 #xf0e57a65 #xfda65cbc #xf967410b
            #xe176f6b6 #xe5b7eb01 #xe8f4cdd8 #xec35d06f
            #xe47a6d6a #xe0bb70dd #xedf856d4 #xe93f4b3)
          '(vector u32)))

;;; Conditions

(define-condition invalid-bzip2-data (simple-error)
  ()
  (:documentation "Signaled when invalid bzip2 data is found."))

(define-condition bzip2-randomized-blocks-unimplemented (simple-error)
  ()
  (:documentation "Signaled when randomized blocks are encountered (not implemented)."))

;;; Decompression state base class

(defstruct decompression-state
  (output nil :type (or null (simple-array u8 (*))))
  (output-index 0 :type fixnum)
  (output-end 0 :type fixnum)
  (input nil :type (or null (simple-array u8 (*))))
  (input-index 0 :type fixnum)
  (input-end 0 :type fixnum)
  (bits 0 :type u32)
  (n-bits 0 :type (integer 0 32))
  (total-in 0 :type fixnum)
  (total-out 0 :type fixnum)
  (checksum nil :type (or null checksum:checksum))
  (state :start))

;;; BZip2 state structure

(defstruct (bzip2-state
            (:include decompression-state)
            (:constructor %make-bzip2-state))
  ;; For doing the final run-length decoding.
  (out-ch 0 :type u8)
  (out-len 0 :type (integer 0 260))
  (block-randomized-p nil)
  (rntogo 0 :type u32)
  (rntpos 0 :type u32)

  (100k-block-size 1 :type (integer 1 9))
  (small-decompression-p nil)
  (current-block-number 0)

  ;; For undoing the Burrows-Wheeler transform
  (original-pointer 0)
  (t-position 0 :type (integer 0 (900000)))
  (k0 0)
  (unzftab (make-array 256 :element-type 'u32)
           :type (simple-array u32 (256)))
  (n-blocks-used 0)
  (cftab (make-array 257 :element-type 'u32)
         :type (simple-array u32 (257)))
  (cftab-copy (make-array 257 :element-type 'u32)
              :type (simple-array u32 (257)))

  ;; For undoing the Burrows-Wheeler transform (FAST).
  (tt (make-array 0 :element-type 'u32)
      :type (simple-array u32 (*)))

  ;; Stored and calculated CRCs.
  (stored-block-crc 0 :type u32)
  (stored-combined-crc 0 :type u32)
  (calculated-block-crc #xffffffff :type u32)
  (calculated-combined-crc 0 :type u32)

  ;; Map of bytes used in block ("mapping table").
  (n-in-use 0 :type (integer 0 256))
  (in-use (make-array 256 :initial-element nil)
          :type (simple-array t (256)))
  ;; This was a byte array; we have chosen to make it a simple integer
  ;; and index it with LOGBITP.
  (in-use-16 0 :type u16)
  (seq-to-unseq (make-array 256 :element-type 'u8)
                :type (simple-array u8 (256)))

  ;; For decoding the MTF values.
  (mtfa (make-array +mtfa-size+ :element-type 'u8)
        :type (simple-array u8 (#.+mtfa-size+)))
  (mtfbase (make-array (/ 256 +mtfl-size+) :element-type 'u16)
           :type (simple-array u16 (#.(/ 256 +mtfl-size+))))
  (selector (make-array +bz-max-selectors+ :element-type 'u8)
            :type (simple-array u8 (#.+bz-max-selectors+)))
  (selector-mtf (make-array +bz-max-selectors+ :element-type 'u8)
                :type (simple-array u8 (#.+bz-max-selectors+)))
  (len (make-array '(#.+bz-n-groups+ #.+bz-max-alpha-size+)
                   :element-type 'u8)
       :type (simple-array u8 (#.+bz-n-groups+ #.+bz-max-alpha-size+)))
  (mtf-continuation nil :type (or null function))

  (limit (let ((w (make-array +bz-n-groups+)))
           (dotimes (i +bz-n-groups+ w)
             (setf (aref w i) (make-array +bz-max-alpha-size+
                                          :element-type 'u32))))
         :type (simple-array t (#.+bz-n-groups+)))
  (base (let ((w (make-array +bz-n-groups+)))
          (dotimes (i +bz-n-groups+ w)
            (setf (aref w i) (make-array +bz-max-alpha-size+
                                         :element-type 'u32))))
        :type (simple-array t (#.+bz-n-groups+)))
  (perm (let ((w (make-array +bz-n-groups+)))
          (dotimes (i +bz-n-groups+ w)
            (setf (aref w i) (make-array +bz-max-alpha-size+
                                         :element-type 'u32))))
        :type (simple-array t (#.+bz-n-groups+)))
  (min-lengths (make-array +bz-n-groups+ :element-type 'u32)
               :type (simple-array u32 (#.+bz-n-groups+)))

  ;; Save variables for scalars in the decompression code.
  (i 0)
  (j 0)
  (alpha-size 0 :type (integer 0 258))
  (n-groups 0)
  (n-selectors 0)
  (EOB 0 :type (integer 0 257))
  ;; FIXME: check on the declarations for these three.
  (group-number 0 :type fixnum)
  (group-position 0 :type fixnum)
  (lval 0 :type fixnum)
  (nblockMAX 0 :type (integer 0 900000))
  (nblock 0 :type (integer 0 900000))
  (es 0 :type fixnum)
  (N 0 :type fixnum)
  (curr 0 :type (integer 0 20))
  (zn 0 :type (integer 0 20))
  (zvec 0 :type (integer 0 #.(expt 2 20)))
  (g-minlen 0 :type (integer 0 23))

  ;; To save the variables for RECEIVE-MTF-VALUES when the input runs
  ;; out.
  (mtf-i 0 :type fixnum)
  (mtf-next-sym 0 :type fixnum))

(defmethod print-object ((object bzip2-state) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":STATE ~S :N-BITS ~D :BYTES ~D/~D+~D/~D"
            (bzip2-state-bits object)
            (bzip2-state-n-bits object)
            (bzip2-state-input-index object)
            (bzip2-state-input-end object)
            (bzip2-state-output-index object)
            (bzip2-state-output-end object))))

(defun make-bzip2-state ()
  (let ((state (%make-bzip2-state)))
    (setf (decompression-state-checksum state) (make-instance 'crc32:crc-32))
    state))

;;; Codec class for integration

(defclass bzip2-codec ()
  ())

(defgeneric encode (codec in out))
(defgeneric decode (codec in out))

(defmethod encode ((codec bzip2-codec) in out)
  (declare (ignore in out))
  (error "BZip2 compression not implemented"))

(defmethod decode ((codec bzip2-codec) in out)
  (decompress out (make-bzip2-state) in))

;;; Main decompression function placeholder
;;; The actual implementation will be added in subsequent parts

(defun decompress (output state input)
  "Decompress BZip2 data from INPUT to OUTPUT using STATE."
  (declare (ignore output state input))
  (error "BZip2 decompression implementation to be added"))

(defun %bzip2-decompress (state input output &key (input-start 0) input-end
                          (output-start 0) output-end)
  "Low-level BZip2 decompression function."
  (declare (type bzip2-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (bzip2-state-input state) input
          (bzip2-state-input-index state) input-start
          (bzip2-state-input-end state) input-end
          (bzip2-state-output state) output
          (bzip2-state-output-index state) output-start
          (bzip2-state-output-end state) output-end)
    ;; Implementation to be added
    (error "BZip2 decompression implementation to be added")))