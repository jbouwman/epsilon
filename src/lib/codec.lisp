(defpackage :epsilon.lib.codec
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.checksum.adler-32
   :epsilon.lib.checksum.crc-32
   :epsilon.lib.checksum.generic
   :epsilon.lib.function
   :epsilon.lib.type)
  (:local-nicknames
   (:stream :epsilon.lib.stream))
  (:export
   :encode
   :encode-file
   :encoding-error
   :decode
   :decode-file
   :decoding-error
   :make-decompressing-stream))

(in-package #:epsilon.lib.codec)

(defgeneric write-u8-vector (vector bitstream &key start end))

(defgeneric encoder (object))

(defgeneric decoder (object))

(defgeneric process (object in out))

(defgeneric write-bits (code size bitstream))

(defgeneric flush (bitstream))

(defgeneric start-data-format (compressor)
  (:documentation "Add any needed prologue data to the output bitstream."))

(defgeneric compress-octet (octet compressor)
  (:documentation "Add OCTET to the compressed data of COMPRESSOR."))

(defgeneric compress-u8-vector (vector compressor &key start end)
  (:documentation "Add the octets of VECTOR to the compressed
  data of COMPRESSOR."))

(defgeneric process-input (compressor input start count)
  (:documentation "Map over pending octets in INPUT and perform
  any needed processing. Called before the data is compressed. A
  subclass might use this to compute a checksum of all input
  data."))

(defgeneric finish-data-format (compressor)
  (:documentation "Add any needed epilogue data to the output bitstream."))

(defgeneric finish-compression (compressor)
  (:documentation "Finish the data format and flush all pending
  data in the bitstream."))

(defgeneric final-compress (compressor)
  (:documentation "Perform the final compression on pending input
  data in COMPRESSOR."))

(defgeneric make-compress-fun (compressor)
  (:documentation "Create a callback suitable for passing to
  MERGE-INPUT for performing incremental compression of the next
  32k octets of input."))


;;;; DEFLATE constants.

;;; block types
(define-constant +block-no-compress+ 0)
(define-constant +block-fixed-codes+ 1)
(define-constant +block-dynamic-codes+ 2)
(define-constant +block-invalid+ 3)

(define-constant +max-code-length+ 16)
(define-constant +max-codes+ 288)
(define-constant +max-n-code-lengths+ 19)
(define-constant +deflate-max-bits+ 15)

(define-constant +length-code-extra-bits+
  (coerce #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
          '(vector u16)))

(define-constant +length-code-base-lengths+
  (coerce #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27
            31 35 43 51 59 67 83 99 115 131 163 195 227 258)
          '(vector u16)))

;;;; BZIP constants.

(defconstant +bz-header-b+ #x42)
(defconstant +bz-header-z+ #x5a)
(defconstant +bz-header-h+ #x68)
(defconstant +bz-header-0+ #x30)
(defconstant +100k+ (expt 10 5))

(defconstant +mtfa-size+ 4096)
(defconstant +mtfl-size+ 16)
(defconstant +bz-max-alpha-size+ 258)
(defconstant +bz-max-code-len+ 23)
(defconstant +bz-runa+ 0)
(defconstant +bz-runb+ 1)
(defconstant +bz-n-groups+ 6)
(defconstant +bz-g-size+ 50)
(defconstant +bz-n-iters+ 4)
(defconstant +bz-max-selectors+ (+ 2 (/ (* 9 +100k+) +bz-g-size+)))

;;; miscellaneous

;;; for DECOMPRESS.
(defconstant +default-buffer-size+ 8192)

;;; CRC32
(declaim (type (simple-array u32 (256)) +crc32-table+ +bzip2-crc32-table+))
(define-constant +crc32-table+
  (coerce '(#x00000000 #x77073096 #xEE0E612C #x990951BA #x076DC419 #x706AF48F
            #xE963A535 #x9E6495A3 #x0EDB8832 #x79DCB8A4 #xE0D5E91E #x97D2D988
            #x09B64C2B #x7EB17CBD #xE7B82D07 #x90BF1D91 #x1DB71064 #x6AB020F2
            #xF3B97148 #x84BE41DE #x1ADAD47D #x6DDDE4EB #xF4D4B551 #x83D385C7
            #x136C9856 #x646BA8C0 #xFD62F97A #x8A65C9EC #x14015C4F #x63066CD9
            #xFA0F3D63 #x8D080DF5 #x3B6E20C8 #x4C69105E #xD56041E4 #xA2677172
            #x3C03E4D1 #x4B04D447 #xD20D85FD #xA50AB56B #x35B5A8FA #x42B2986C
            #xDBBBC9D6 #xACBCF940 #x32D86CE3 #x45DF5C75 #xDCD60DCF #xABD13D59
            #x26D930AC #x51DE003A #xC8D75180 #xBFD06116 #x21B4F4B5 #x56B3C423
            #xCFBA9599 #xB8BDA50F #x2802B89E #x5F058808 #xC60CD9B2 #xB10BE924
            #x2F6F7C87 #x58684C11 #xC1611DAB #xB6662D3D #x76DC4190 #x01DB7106
            #x98D220BC #xEFD5102A #x71B18589 #x06B6B51F #x9FBFE4A5 #xE8B8D433
            #x7807C9A2 #x0F00F934 #x9609A88E #xE10E9818 #x7F6A0DBB #x086D3D2D
            #x91646C97 #xE6635C01 #x6B6B51F4 #x1C6C6162 #x856530D8 #xF262004E
            #x6C0695ED #x1B01A57B #x8208F4C1 #xF50FC457 #x65B0D9C6 #x12B7E950
            #x8BBEB8EA #xFCB9887C #x62DD1DDF #x15DA2D49 #x8CD37CF3 #xFBD44C65
            #x4DB26158 #x3AB551CE #xA3BC0074 #xD4BB30E2 #x4ADFA541 #x3DD895D7
            #xA4D1C46D #xD3D6F4FB #x4369E96A #x346ED9FC #xAD678846 #xDA60B8D0
            #x44042D73 #x33031DE5 #xAA0A4C5F #xDD0D7CC9 #x5005713C #x270241AA
            #xBE0B1010 #xC90C2086 #x5768B525 #x206F85B3 #xB966D409 #xCE61E49F
            #x5EDEF90E #x29D9C998 #xB0D09822 #xC7D7A8B4 #x59B33D17 #x2EB40D81
            #xB7BD5C3B #xC0BA6CAD #xEDB88320 #x9ABFB3B6 #x03B6E20C #x74B1D29A
            #xEAD54739 #x9DD277AF #x04DB2615 #x73DC1683 #xE3630B12 #x94643B84
            #x0D6D6A3E #x7A6A5AA8 #xE40ECF0B #x9309FF9D #x0A00AE27 #x7D079EB1
            #xF00F9344 #x8708A3D2 #x1E01F268 #x6906C2FE #xF762575D #x806567CB
            #x196C3671 #x6E6B06E7 #xFED41B76 #x89D32BE0 #x10DA7A5A #x67DD4ACC
            #xF9B9DF6F #x8EBEEFF9 #x17B7BE43 #x60B08ED5 #xD6D6A3E8 #xA1D1937E
            #x38D8C2C4 #x4FDFF252 #xD1BB67F1 #xA6BC5767 #x3FB506DD #x48B2364B
            #xD80D2BDA #xAF0A1B4C #x36034AF6 #x41047A60 #xDF60EFC3 #xA867DF55
            #x316E8EEF #x4669BE79 #xCB61B38C #xBC66831A #x256FD2A0 #x5268E236
            #xCC0C7795 #xBB0B4703 #x220216B9 #x5505262F #xC5BA3BBE #xB2BD0B28
            #x2BB45A92 #x5CB36A04 #xC2D7FFA7 #xB5D0CF31 #x2CD99E8B #x5BDEAE1D
            #x9B64C2B0 #xEC63F226 #x756AA39C #x026D930A #x9C0906A9 #xEB0E363F
            #x72076785 #x05005713 #x95BF4A82 #xE2B87A14 #x7BB12BAE #x0CB61B38
            #x92D28E9B #xE5D5BE0D #x7CDCEFB7 #x0BDBDF21 #x86D3D2D4 #xF1D4E242
            #x68DDB3F8 #x1FDA836E #x81BE16CD #xF6B9265B #x6FB077E1 #x18B74777
            #x88085AE6 #xFF0F6A70 #x66063BCA #x11010B5C #x8F659EFF #xF862AE69
            #x616BFFD3 #x166CCF45 #xA00AE278 #xD70DD2EE #x4E048354 #x3903B3C2
            #xA7672661 #xD06016F7 #x4969474D #x3E6E77DB #xAED16A4A #xD9D65ADC
            #x40DF0B66 #x37D83BF0 #xA9BCAE53 #xDEBB9EC5 #x47B2CF7F #x30B5FFE9
            #xBDBDF21C #xCABAC28A #x53B39330 #x24B4A3A6 #xBAD03605 #xCDD70693
            #x54DE5729 #x23D967BF #xB3667A2E #xC4614AB8 #x5D681B02 #x2A6F2B94
            #xB40BBE37 #xC30C8EA1 #x5A05DF1B #x2D02EF8D)
          '(vector u32)))

(define-constant +bzip2-crc32-table+
    (coerce '(#x00000000 #x04c11db7 #x09823b6e #x0d4326d9
              #x130476dc #x17c56b6b #x1a864db2 #x1e475005
              #x2608edb8 #x22c9f00f #x2f8ad6d6 #x2b4bcb61
              #x350c9b64 #x31cd86d3 #x3c8ea00a #x384fbdbd
              #x4c11db70 #x48d0c6c7 #x4593e01e #x4152fda9
              #x5f15adac #x5bd4b01b #x569796c2 #x52568b75
              #x6a1936c8 #x6ed82b7f #x639b0da6 #x675a1011
              #x791d4014 #x7ddc5da3 #x709f7b7a #x745e66cd
              #x9823b6e0 #x9ce2ab57 #x91a18d8e #x95609039
              #x8b27c03c #x8fe6dd8b #x82a5fb52 #x8664e6e5
              #xbe2b5b58 #xbaea46ef #xb7a96036 #xb3687d81
              #xad2f2d84 #xa9ee3033 #xa4ad16ea #xa06c0b5d
              #xd4326d90 #xd0f37027 #xddb056fe #xd9714b49
              #xc7361b4c #xc3f706fb #xceb42022 #xca753d95
              #xf23a8028 #xf6fb9d9f #xfbb8bb46 #xff79a6f1
              #xe13ef6f4 #xe5ffeb43 #xe8bccd9a #xec7dd02d
              #x34867077 #x30476dc0 #x3d044b19 #x39c556ae
              #x278206ab #x23431b1c #x2e003dc5 #x2ac12072
              #x128e9dcf #x164f8078 #x1b0ca6a1 #x1fcdbb16
              #x018aeb13 #x054bf6a4 #x0808d07d #x0cc9cdca
              #x7897ab07 #x7c56b6b0 #x71159069 #x75d48dde
              #x6b93dddb #x6f52c06c #x6211e6b5 #x66d0fb02
              #x5e9f46bf #x5a5e5b08 #x571d7dd1 #x53dc6066
              #x4d9b3063 #x495a2dd4 #x44190b0d #x40d816ba
              #xaca5c697 #xa864db20 #xa527fdf9 #xa1e6e04e
              #xbfa1b04b #xbb60adfc #xb6238b25 #xb2e29692
              #x8aad2b2f #x8e6c3698 #x832f1041 #x87ee0df6
              #x99a95df3 #x9d684044 #x902b669d #x94ea7b2a
              #xe0b41de7 #xe4750050 #xe9362689 #xedf73b3e
              #xf3b06b3b #xf771768c #xfa325055 #xfef34de2
              #xc6bcf05f #xc27dede8 #xcf3ecb31 #xcbffd686
              #xd5b88683 #xd1799b34 #xdc3abded #xd8fba05a
              #x690ce0ee #x6dcdfd59 #x608edb80 #x644fc637
              #x7a089632 #x7ec98b85 #x738aad5c #x774bb0eb
              #x4f040d56 #x4bc510e1 #x46863638 #x42472b8f
              #x5c007b8a #x58c1663d #x558240e4 #x51435d53
              #x251d3b9e #x21dc2629 #x2c9f00f0 #x285e1d47
              #x36194d42 #x32d850f5 #x3f9b762c #x3b5a6b9b
              #x0315d626 #x07d4cb91 #x0a97ed48 #x0e56f0ff
              #x1011a0fa #x14d0bd4d #x19939b94 #x1d528623
              #xf12f560e #xf5ee4bb9 #xf8ad6d60 #xfc6c70d7
              #xe22b20d2 #xe6ea3d65 #xeba91bbc #xef68060b
              #xd727bbb6 #xd3e6a601 #xdea580d8 #xda649d6f
              #xc423cd6a #xc0e2d0dd #xcda1f604 #xc960ebb3
              #xbd3e8d7e #xb9ff90c9 #xb4bcb610 #xb07daba7
              #xae3afba2 #xaafbe615 #xa7b8c0cc #xa379dd7b
              #x9b3660c6 #x9ff77d71 #x92b45ba8 #x9675461f
              #x8832161a #x8cf30bad #x81b02d74 #x857130c3
              #x5d8a9099 #x594b8d2e #x5408abf7 #x50c9b640
              #x4e8ee645 #x4a4ffbf2 #x470cdd2b #x43cdc09c
              #x7b827d21 #x7f436096 #x7200464f #x76c15bf8
              #x68860bfd #x6c47164a #x61043093 #x65c52d24
              #x119b4be9 #x155a565e #x18197087 #x1cd86d30
              #x029f3d35 #x065e2082 #x0b1d065b #x0fdc1bec
              #x3793a651 #x3352bbe6 #x3e119d3f #x3ad08088
              #x2497d08d #x2056cd3a #x2d15ebe3 #x29d4f654
              #xc5a92679 #xc1683bce #xcc2b1d17 #xc8ea00a0
              #xd6ad50a5 #xd26c4d12 #xdf2f6bcb #xdbee767c
              #xe3a1cbc1 #xe760d676 #xea23f0af #xeee2ed18
              #xf0a5bd1d #xf464a0aa #xf9278673 #xfde69bc4
              #x89b8fd09 #x8d79e0be #x803ac667 #x84fbdbd0
              #x9abc8bd5 #x9e7d9662 #x933eb0bb #x97ffad0c
              #xafb010b1 #xab710d06 #xa6322bdf #xa2f33668
              #xbcb4666d #xb8757bda #xb5365d03 #xb1f740b4)
            '(vector u32)))

;;; Adler32, smallest prime < 65536

(eval-when (:compile-toplevel :load-toplevel)

(defconstant adler32-modulo 65521)

(defparameter +input-limit+ 32768)
(defparameter +input-limit-mask+ (1- +input-limit+))
(defparameter +buffer-size+ (* +input-limit+ 2))
(defparameter +buffer-size-mask+ (1- +buffer-size+))

(defparameter +input-size+ #x10000)
(defparameter +input-mask+ #x0FFFF)
(defparameter +hashes-size+ 8191)
(defparameter +radix+ 109)
(defparameter +rmax+ (* +radix+ +radix+))
  
(defparameter +bitstream-buffer-size+ 4096)
(defparameter +bitstream-buffer-mask+ (1- +bitstream-buffer-size+))
(defparameter +bitstream-buffer-bits+ (* +bitstream-buffer-size+ 8))
(defparameter +bitstream-buffer-bitmask+ (1- +bitstream-buffer-bits+))

(defconstant +final-block+ #b1)
(defconstant +fixed-tables+ #b01)

)

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


;; jx

(define-condition invalid-format-error (simple-error)
  ((format :initarg :format :reader invalid-format))
  (:report (lambda (condition stream)
             (format stream "Invalid format ~S"
                     (invalid-format condition))))
  (:documentation "Signaled when an invalid format name is passed to
MAKE-DSTATE, MAKE-INFLATE-STATE, or DECOMPRESS."))

(define-condition invalid-checksum-error (simple-error)
  ((expected-checksum :initarg :stored :reader expected-checksum)
   (actual-checksum :initarg :computed :reader actual-checksum)
   (kind :initarg :kind :reader checksum-kind))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A checksum, expected ~X, got ~X"
                     (checksum-kind condition)
                     (expected-checksum condition)
                     (actual-checksum condition))))
  (:documentation "Signaled when the checksum of decompressed data does
not match the expected value."))

(define-condition invalid-zlib-header-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid zlib header")))
  (:documentation "Signaled when a zlib header does not pass the
consistency check."))

(define-condition invalid-gzip-header-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid gzip header")))
  (:documentation "Signaled when a gzip header does not have the proper ID."))

(define-condition reserved-block-type-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid deflate block")))
  (:documentation "Signaled when an invalid deflate block is found."))

(define-condition invalid-stored-block-length-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid stored block length")))
  (:documentation "Signaled when a stored block's length does not pass
the consistency check."))

(define-condition code-lengths-bounds-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Code lengths expand out of bounds")))
  (:documentation "Signaled when the code length section of a dynamic block would produce more
code lengths than declared."))

(define-condition code-lengths-start-with-repetition-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Code lengths start with a repetition")))
  (:documentation "Signaled when the code length section of a dynamic block begins with \"repeat
last code\"."))

(define-condition unassigned-huffman-code-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Unassigned Huffman code")))
  (:documentation "Signaled when an unassigned Huffman code is referenced."))

(define-condition illegal-length-code-error (simple-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal length code: ~d" (illegal-code condition))))
  (:documentation "Signaled when the illegal length codes 286 or 287 are used."))

(define-condition illegal-distance-code-error (simple-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal distance code: ~d" (illegal-code condition))))
  (:documentation "Signaled when the illegal distance codes 30 or 31 are used."))

(define-condition invalid-bzip2-data (simple-error)
  ()
  (:documentation "Signaled when invalid bzip2 data is found."))



(defclass gzip-header ()
  ((flags :initarg :flags :accessor flags)
   (filename :initform nil :accessor filename)
   (write-date :initarg :write-date :accessor write-date)
   (mtime :initform 0 :accessor mtime)
   (comment :initform nil :accessor comment)
   (extra-flags :initarg :extra-flags :accessor extra-flags)
   (os :initarg :os :accessor os)
   (crc16 :initarg :crc16 :accessor crc16)
   (compression-method :initarg :compression-method :accessor compression-method)))

;;; individual bit meanings in the flag field

(defconstant +gzip-flag-text+ 0)
(defconstant +gzip-flag-crc+ 1)
(defconstant +gzip-flag-extra+ 2)
(defconstant +gzip-flag-name+ 3)
(defconstant +gzip-flag-comment+ 4)

;;; values of the compression method byte

(defconstant +gzip-deflate-method+ 8)

;;; values of the extra flag field

(defconstant +gzip-xfl-max-compression+ 2)

(defconstant +gzip-xfl-fast-compression+ 4)

(defvar *gzip-signature* (->u8 '(#x1F #x8B))
  "These two octets precede all data in the gzip format.")

(defconstant +gzip-fast-compression+ 4
  "Code for gzip compression level. This is present only to create valid
gzip data; it has no meaning to the compressor and is only a hint to
the decompressor.")

;;; These are all used to create valid files, not to control or modify
;;; the compression process.

(defconstant +gzip-deflate-compression+ 8)
(defconstant +gzip-flags+ 0)
(defconstant +gzip-unix-os+ 3)
(defconstant +gzip-mtime+ 0)

(defclass gzip-compressor (deflate-compressor)
  ((checksum :initform (make-instance 'crc-32))
   (data-length :initform 0)))

(defmethod start-data-format :before ((compressor gzip-compressor))
  (write-u8-vector *gzip-signature* compressor)
  (write-u8 compressor +gzip-deflate-compression+)
  (write-u8 compressor +gzip-flags+)
  (write-u32 compressor +gzip-mtime+)
  (write-u8 compressor +gzip-fast-compression+)
  (write-u8 compressor +gzip-unix-os+))

(defmethod process-input :after ((compressor gzip-compressor) input start count)
  (with-slots (data-length checksum) compressor
    (incf data-length count)
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor gzip-compressor))
  (with-slots (data-length checksum) compressor
    (write-u32 compressor (checksum checksum))
    (write-u32 compressor data-length)))

(defclass gzip-codec ()
  ())

(defmethod encode ((codec gzip-codec) in out)
  (compress-stream 'gzip-compressor in out))

(defmethod decode ((codec gzip-codec) in out)
  (decompress out (make-dstate :gzip) in))


(defclass zlib-header ()
  ((flags :initarg :flags :accessor flags)
   (cmf :initarg :cmf :accessor cmf)
   (fdict :initarg :fdict :accessor fdict)
   (adler32 :initarg :adler32 :accessor adler32)))

(defconstant +zlib-compression-method+ 8)

(defun zlib-compression-method (cmf-byte)
  (declare (type u8 cmf-byte))
  (ldb (byte 4 0) cmf-byte))

(defun zlib-compression-info (cmf-byte)
  (declare (type u8 cmf-byte))
  (ldb (byte 4 4) cmf-byte))

(defconstant +zlib-flag-fdict+ 5)

(defun zlib-flag-fcheck (flag-byte)
  (declare (type u8 flag-byte))
  (ldb (byte 4 0) flag-byte))

(defconstant +zlib-flevel-fastest+ 0)
(defconstant +zlib-flevel-fast+ 1)
(defconstant +zlib-flevel-default+ 2)
(defconstant +zlib-flevel-maximum+ 3)

(defun zlib-flag-flevel (flag-byte)
  (declare (type u8 flag-byte))
  (ldb (byte 2 6) flag-byte))

(defclass zlib-compressor (deflate-compressor)
  ((checksum :initform (make-instance 'adler-32))))

(defconstant +zlib-magic+ #x789c)

(defmethod start-data-format :before ((compressor zlib-compressor))
  (write-u16-msb compressor +zlib-magic+))

(defmethod process-input :after ((compressor zlib-compressor) input start count)
  (with-slots (checksum) compressor
    (update checksum input start count)))

(defmethod finish-data-format :after ((compressor zlib-compressor))
  (with-slots (checksum) compressor
    (write-u32-msb compressor (checksum checksum))))

(defclass zlib-codec ()
  ())

(defmethod encode ((codec zlib-codec) in out)
  (compress-stream 'zlib-compressor in out))

(defmethod decode ((codec zlib-codec) in out)
  (decompress out (make-dstate :zlib) in))

(defclass deflate-codec ()
  ())

(defmethod encode ((codec deflate-codec) in out)
  (compress-stream 'deflate-compressor in out))

(defmethod decode ((codec deflate-codec) in out)
  (decompress out (make-dstate :deflate) in))

;;; This structure is never meant to be instantiated.  It exists only to
;;; provide common framework for other decompressors.

(defstruct (decompression-state
             (:constructor)
             (:conc-name dstate-))
  (state nil :type (or null function))
  (done nil)

  (input (->u8 1) :type ->u8)
  (input-start 0 :type (and fixnum (integer 0 *)))
  (input-index 0 :type (and fixnum (integer 0 *)))
  (input-end 0 :type (and fixnum (integer 0 *)))

  (output (->u8 1) :type ->u8)
  (output-start 0 :type (and fixnum (integer 0 *)))
  (output-index 0 :type (and fixnum (integer 0 *)))
  (output-end 0 :type (and fixnum (integer 0 *)))

  (checksum nil)

  ;; Bit buffer.
  (bits 0 :type u32)
  (n-bits 0 :type (integer 0 32)))

(defun make-dstate (format)             ; FIXME remove
  "Return a structure suitable for uncompressing data in DATA-FORMAT;
DATA-FORMAT should be:

  :BZIP2    For decompressing data in the `bzip2' format;
  :GZIP     For decompressing data in the `gzip' format;
  :ZLIB     For decompressing data in the `zlib' format;
  :DEFLATE  For decompressing data in the `deflate' format.

The usual value of DATA-FORMAT will be one of :BZIP2 or :GZIP."
  (case format
    ((:deflate :zlib :gzip)
     (make-inflate-state format))
    ((:bzip2)
     (make-bzip2-state))
    (t
     (error 'invalid-format-error :format format))))

(defun finish-dstate (state)            ; FIXME dstate is jargony -- rename all -- is this generic?
  (unless (dstate-done state)
    (error 'stream-exhausted))
  t)

(defgeneric decompress (output state input &key &allow-other-keys)
  (:method (output format input &rest keys)
    (%decompress output format input keys))
  ;; Accommodate people who want to use lists as input, possibly for
  ;; experimenting with the API.
  (:method (output format (input list) &rest keys)
    (let ((vector (coerce input '(simple-array u8 (*)))))
      (%decompress output format vector keys))))

(defun %decompress (output format input keys)
  (let ((state (make-dstate format)))
    (multiple-value-prog1 (apply #'decompress output state input keys)
      (finish-dstate state))))

;;; SUBSEQ is specified to always make a copy.  But we don't want an
;;; exact copy of a freshly-consed vector; that'd be wasteful.
(defun maybe-subseq (v end)
  (if (= end (length v))
      v
      (subseq v 0 end)))

(defun decompress-fun-for-state (state)
  (typecase state
    (inflate-state #'%inflate)
    (bzip2-state #'%bzip2-decompress)))

;; For convenience.
(defun %decompress-from-pathname (output state pathname buffer-size)
  (with-open-file (stream pathname :element-type 'u8
                          :direction :input)
    (decompress output state stream
                :buffer-size (if (eq buffer-size :file-length)
                                 (file-length stream)
                                 buffer-size))))

(defmethod decompress ((output null) (state decompression-state) (input pathname)
                       &key)
  (%decompress-from-pathname output state input :file-length))

(defmethod decompress ((output pathname) (state decompression-state) (input pathname)
                       &key buffer-size)
  (check-type buffer-size (or null integer))
  (with-open-file (stream output :element-type 'u8
                          :direction :output)
    (%decompress-from-pathname stream state input buffer-size)))

(defmethod decompress ((output stream) (state decompression-state) (input pathname)
                       &key buffer-size)
  (check-type buffer-size (or null integer))
  (%decompress-from-pathname output state input buffer-size))

(defun %decompress/null-vector (state input fun
                                input-start input-end buffer-size)
  (declare (type function fun))
  (loop
    :with output = (->u8 buffer-size)
    :with output-start = 0
    do (cond
         ((= output-start (length output))
          ;; Reallocate the output buffer.
          (let ((new (->u8 (* 2 (length output)))))
            (setf output (replace new output))))
         (t
          (multiple-value-bind (consumed produced)
              (funcall fun state input output
                       :input-start input-start :input-end input-end
                       :output-start output-start :output-end (length output))
            (incf input-start consumed)
            (incf output-start produced)
            (when (or (dstate-done state)
                      (and (or (>= input-start input-end)
                               (zerop consumed))
                           (zerop produced)))
              (return-from %decompress/null-vector (maybe-subseq output output-start))))))))

(defmethod decompress ((output null) (state decompression-state) (input vector)
                       &key (input-start 0) input-end buffer-size)
  (%decompress/null-vector state input
                           (decompress-fun-for-state state)
                           input-start (or input-end (length input))
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/null-stream (state input fun buffer-size)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent input-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       with output = (make-array buffer-size :element-type 'u8)
       with output-start = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (cond
            ((= output-start (length output))
             ;; Reallocate the output buffer.
             (let ((new (make-array (* 2 (length output))
                                    :element-type 'u8)))
               (setf output (replace new output))))
            (t
             (multiple-value-bind (consumed produced)
                 (funcall fun state input-buffer output
                          :input-start input-start :input-end input-end
                          :output-start output-start)
               (incf input-start consumed)
               (incf output-start produced)
               (let ((input-consumed-p (>= input-start input-end)))
                 ;; Get more input if possible.
                 (when input-consumed-p
                   (setf input-start 0
                         input-end (read-sequence input-buffer input)))
                 (when (or (dstate-done state)
                           (and (or (and input-consumed-p (zerop input-end))
                                    (zerop consumed))
                                (zerop produced)))
                   (return-from %decompress/null-stream (maybe-subseq output output-start))))))))))

(defmethod decompress ((output null) (state decompression-state) (input stream)
                       &key buffer-size)
  (%decompress/null-stream state input
                           (decompress-fun-for-state state)
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/vector-vector (output state input fun
                                  input-start input-end
                                  output-start output-end)
  (declare (type ->u8 input output))
  (declare (type function fun))
  (loop
     with n-bytes-consumed = 0 and n-bytes-produced = 0
     do (multiple-value-bind (consumed produced)
            (funcall fun state input output
                     :input-start input-start :input-end input-end
                     :output-start output-start :output-end output-end)
          (incf input-start consumed)
          (incf output-start produced)
          (incf n-bytes-consumed consumed)
          (incf n-bytes-produced produced)
          (when (and (or (>= input-start input-end)
                         (zerop consumed))
                     (or (>= output-start output-end)
                         (zerop produced)))
            (return-from %decompress/vector-vector 
              (values n-bytes-consumed n-bytes-produced))))))

(defmethod decompress ((output vector) (state decompression-state) (input vector)
                       &key (input-start 0) input-end
                       (output-start 0) output-end)
  (%decompress/vector-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))
                             output-start (or output-end (length output))))

(defun %decompress/stream-vector (output state input fun input-start input-end)
  (declare (type function fun))
  (let ((buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent buffer))
    (loop (multiple-value-bind (consumed produced)
              (funcall fun state input buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence buffer output :end produced)
            (when (or (dstate-done state)
                      (and (or (>= input-start input-end)
                               (zerop consumed))
                           (zerop produced)))
              (return-from %decompress/stream-vector output))))))

(defmethod decompress ((output stream) (state decompression-state) (input vector)
                       &key (input-start 0) input-end)
  (%decompress/stream-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))))

(defun %decompress/stream-stream (output state input fun)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type 'u8))
        (output-buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent input-buffer output-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (multiple-value-bind (consumed produced)
              (funcall fun state input-buffer output-buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence output-buffer output :end produced)
            (let ((input-consumed-p (>= input-start input-end)))
              (when input-consumed-p
                (setf input-start 0
                      input-end (read-sequence input-buffer input)))
              (when (or (dstate-done state)
                        (and (or (and input-consumed-p (zerop input-end))
                                 (zerop consumed))
                             (zerop produced)))
                (return-from %decompress/stream-stream output)))))))

(defmethod decompress ((output stream) (state decompression-state) (input stream)
                       &key)
  (%decompress/stream-stream output state input
                             (decompress-fun-for-state state)))


;;; decoders for fixed compression blocks

(defparameter *fixed-block-code-lengths*
  (map 'list #'make-crd
       '(8   9   7   8)                 ; lengths
       '(0   144 256 280)               ; start values
       '(143 255 279 287)))             ; end values

(defparameter *fixed-block-distance-lengths*
  (list (make-crd 5 0 31)))

(defun code-n-values (c)
  (1+ (- (code-end-value c) (code-start-value c))))

(defun compute-huffman-decode-structure (code-descriptors)
  (let* ((n-syms (loop for cd in code-descriptors
                       sum (code-n-values cd)))
         (code-lengths (make-array n-syms :element-type 'u16)))
    (dolist (cd code-descriptors)
      (fill code-lengths (code-n-bits cd)
            :start (code-start-value cd) :end (1+ (code-end-value cd))))
    (construct-huffman-decode-table code-lengths)))

(defparameter *fixed-literal/length-table*
  (compute-huffman-decode-structure *fixed-block-code-lengths*))

(defparameter *fixed-distance-table*
  (compute-huffman-decode-structure *fixed-block-distance-lengths*))

(deftype sliding-window ()
  '(simple-array u8 (32768)))

(defstruct (inflate-state
            (:include decompression-state)
             (:constructor %make-inflate-state (data-format)))
  ;; whether the current block being processed is the last one
  (final-block-p nil :type (member t nil))
  ;; the number of bytes to copy for uncompressed blocks
  (length 0)
  ;; the code for length/distance codes
  (distance 0)
  (length-code 0 :type (integer 0 28))
  (distance-code 0 :type (integer 0 31))
  ;; values for dynamic blocks
  (n-length-codes 0)
  (n-distance-codes 0)
  (n-codes 0)
  (n-values-read 0)
  (code-lengths (make-array 320) :type (simple-vector 320))
  ;; sliding window
  (window (->u8 32768) :type sliding-window)
  ;; position in the sliding window
  (window-index 0 :type (mod 32768))
  ;; codes table for dynamically compressed blocks
  (codes-table nil)
  ;; literal/length table for compressed blocks
  (literal/length-table *fixed-literal/length-table*
                        :type huffman-decode-table)
  ;; distance table for compressed blocks
  (distance-table *fixed-distance-table* :type huffman-decode-table)
  ;; header for wrapped data, or NIL if raw deflate data
  (header nil)
  ;; format of the compressed data that we're reading
  (data-format 'deflate :type (member deflate zlib gzip)))

;; FIXME deleteme

(defun make-inflate-state (format)
  "Return a INFLATE-STATE structure suitable for uncompressing data in
FORMAT; FORMAT should be:

  :GZIP        For decompressing data in the `gzip' format;
  :ZLIB        For decompressing data in the `zlib' format;
  :DEFLATE     For decompressing data in the `deflate' format.

The usual value of FORMAT will be one of :GZIP or :ZLIB."
  (let* ((f (case format
              ((:gzip) 'gzip)
              ((:zlib) 'zlib)
              ((:deflate) 'deflate)
              (t
               (error 'invalid-format-error :format format))))
          (state (%make-inflate-state f)))
    (ecase f
      (gzip
       (setf (dstate-checksum state) (make-instance 'crc-32)))
      ((zlib deflate)
       (setf (dstate-checksum state) (make-instance 'adler-32))))
    state))

(defun finish-inflate-state (state)
  (unless (inflate-state-done state)
    (error 'stream-exhausted))
  t)

(defmethod print-object ((object inflate-state) stream)
  (print-unreadable-object (object stream)
    (format stream "Inflate-State input ~D/~D; output ~D/~D"
            (- (inflate-state-input-index object)
               (inflate-state-input-start object))
            (- (inflate-state-input-end object)
               (inflate-state-input-index object))
            (- (inflate-state-output-index object)
               (inflate-state-output-start object))
            (- (inflate-state-output-end object)
               (inflate-state-output-index object)))))

(defun update-window (state)
  (declare (type inflate-state state))
  (let* ((output (inflate-state-output state))
         (start (inflate-state-output-start state))
         (index (inflate-state-output-index state))
         (n-bytes-to-copy (- index start))
         (window (inflate-state-window state))
         (window-index (inflate-state-window-index state)))
    (cond
      ((>= n-bytes-to-copy (length window))
       ;; can "flush" the window
       (setf (inflate-state-window-index state) 0)
       (replace window output :start2 (- index (length window))
                :end2 index))
      (t
       (let ((window-space (- (length window) window-index)))
         (cond
           ((> n-bytes-to-copy window-space)
            (replace window output :start1 window-index
                     :start2 start :end2 index)
            (replace window output
                     :start2 (+ start window-space)
                     :end2 index)
            (setf (inflate-state-window-index state)
                  (- n-bytes-to-copy window-space)))
           (t
            (replace window output :start1 window-index
                     :start2 start :end2 index)
            (setf (inflate-state-window-index state)
                  (mod (+ window-index n-bytes-to-copy) (length window))))))))))

;;; This is used behind-the-scenes to do efficient buffer->buffer
;;; decompression.  Everything user-visible that's related to
;;; decompression ultimately comes down to this function.

(defun %inflate (state input output &key (input-start 0) input-end
                (output-start 0) output-end)
  "Decompresses data in INPUT between INPUT-START and INPUT-END
and places the result in OUTPUT between OUTPUT-START and
OUTPUT-END.  -START and -END arguments follow the convention of
the sequence functions.  Returns the number of bytes pulled from
the input and the number of bytes written to the output."
  (declare (type inflate-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (inflate-state-input state) input
          (inflate-state-input-start state) input-start
          (inflate-state-input-index state) input-start
          (inflate-state-input-end state) input-end
          (inflate-state-output state) output
          (inflate-state-output-start state) output-start
          (inflate-state-output-index state) output-start
          (inflate-state-output-end state) output-end)
    (catch 'inflate-done
      (%inflate-state-machine state))
    (update-window state)

    (update (dstate-checksum state)
            output
            output-start
            (- (inflate-state-output-index state) output-start))
      
    (values (- (inflate-state-input-index state) input-start)
            (- (inflate-state-output-index state) output-start))))

(defun record-code-length (state value)
  (setf (aref (inflate-state-code-lengths state)
              (aref *code-length-code-order*
                    (inflate-state-n-values-read state))) value)
  (incf (inflate-state-n-values-read state)))

;;; internal inflate function

(defun %inflate-state-machine (state)
  (declare (type inflate-state state))
  ;; Once upon a time, the individual functions in the LABELS below were
  ;; separate functions.  We drove the state machine of this function
  ;; using LOOP and SYMBOL-FUNCTION.  This scheme looked lovely...except
  ;; that SYMBOL-FUNCTION is a horrible thing to call in inner loops,
  ;; and we were calling it for just about every byte of input.
  ;;
  ;; So we switched to this huge LABELS.  Each function then stored a
  ;; reference to its next state in INFLATE-STATE-STATE before jumping
  ;; to the next function.  Some compilers were even able to optimize
  ;; the call into a fallthru, which provides a nice approximation of a
  ;; C switch statement.  That was fine and dandy...except that the jump
  ;; is a tail call, Common Lisp is not Scheme, and some implementations
  ;; do not optimize tail calls.  This combination led to stack
  ;; overflows if you handed a large input buffer to this function.
  ;;
  ;; So we provide alternatives now through the TRANSITION-TO macro.  On
  ;; implementations we're sure we can trust to DTRT, we keep the second
  ;; scheme above.  On other implementations, we use a variant of the
  ;; first scheme we tried, which is to simply store the next state's
  ;; function in INFLATE-STATE-STATE and return.  This at least avoids
  ;; SYMBOL-FUNCTION and keeps constant stack space; the LOOP in the
  ;; body of the LABELS (waaay down there) makes sure that we don't stop
  ;; until we THROW.
  (macrolet ((transition-to (next-state)
               `(progn
                  (setf (inflate-state-state state) #',next-state)
                  #+(or sbcl cmu)
                  (,next-state state)
                  ;; Just fall through for other implementations and
                  ;; return normally.
                  )))
    (labels ((read-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type inflate-state state))
               (prog1 (ldb (byte n 0) (inflate-state-bits state))
                 (setf (inflate-state-bits state)
                       (ash (inflate-state-bits state) (- n)))
                 (decf (inflate-state-n-bits state) n)))

             (ensure-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type inflate-state state))
               (let ((bits (inflate-state-bits state))
                     (n-bits (inflate-state-n-bits state))
                     (input-index (inflate-state-input-index state)))
                 (declare (type u32 bits))
                 (loop while (< n-bits n)
                       when (>= input-index (inflate-state-input-end state))
                         do (progn
                              (setf (inflate-state-bits state) bits
                                    (inflate-state-n-bits state) n-bits
                                    (inflate-state-input-index state) input-index)
                              (throw 'inflate-done nil))
                       do (let ((byte (aref (inflate-state-input state) input-index)))
                            (declare (type u8 byte))
                            (setf bits
                                  (logand #xffffffff (logior (ash byte n-bits) bits)))
                            (incf n-bits 8)
                            (incf input-index))
                       finally (setf (inflate-state-bits state) bits
                                     (inflate-state-n-bits state) n-bits
                                     (inflate-state-input-index state) input-index))))

             (ensure-and-read-bits (n state)
               (ensure-bits n state)
               (read-bits n state))

             (align-bits-bytewise (state)
               (declare (type inflate-state state))
               (let ((n-bits (inflate-state-n-bits state)))
                 (decf (inflate-state-n-bits state) (rem n-bits 8))
                 (setf (inflate-state-bits state)
                       (ash (inflate-state-bits state)
                            (- (rem n-bits 8))))
                 (values)))

             (decode-value (table state)
               (declare (type huffman-decode-table table))
               (declare (type inflate-state state))
               (ensure-bits (hdt-bits table) state)
               (let ((bits (inflate-state-bits state)))
                 (declare (type u32 bits))
                 (do ((counts (hdt-counts table))
                      (len 1 (1+ len))
                      (first 0 (sb-ext:truly-the fixnum (ash first 1)))
                      (code 0 (sb-ext:truly-the fixnum (ash code 1))))
                     ((>= len +max-code-length+)
                      (error 'unassigned-huffman-code-error))
                   (declare (type (and fixnum (integer 0 *)) first code))
                   ;; We would normally do this with READ-BITS, but DECODE-VALUE
                   ;; is a hotspot in profiles along with this would-be call to
                   ;; READ-BITS, so we inline it all here.
                   (setf code (logior code (logand bits 1))
                         bits (ash bits -1))
                   (let ((count (aref counts len)))
                     (when (< (- code count) first)
                       (setf (inflate-state-bits state) bits)
                       (decf (inflate-state-n-bits state) len)
                       (return-from decode-value (aref (hdt-symbols table)
                                                       (sb-ext:truly-the fixnum 
                                                        (+ (aref (hdt-offsets table) (1- len))
                                                           (- code first))))))
                     (setf first
                           (sb-ext:truly-the fixnum (+ first count)))))))

             (read-dynamic-table (state decoder n-values)
               (declare (type inflate-state state))
               (loop with lengths = (inflate-state-code-lengths state)
                     while (< (inflate-state-n-values-read state) n-values)
                     do (ensure-bits (+ (hdt-bits decoder) 7) state)
                        (let ((value (decode-value decoder state)))
                          (cond
                            ((< value 16)
                             (setf (aref lengths (inflate-state-n-values-read state)) value)
                             (incf (inflate-state-n-values-read state)))
                            (t
                             (let ((len 0) (sym 0))
                               (cond
                                 ((= value 16)
                                  (when (zerop (inflate-state-n-values-read state))
                                    (error 'code-lengths-start-with-repetition-error))
                                  (setf sym (aref lengths (1- (inflate-state-n-values-read state))))
                                  (setf len (+ 3 (read-bits 2 state))))
                                 ((= value 17)
                                  (setf len (+ 3 (read-bits 3 state))))
                                 ((= value 18)
                                  (setf len (+ 11 (read-bits 7 state)))))
                               (when (< n-values (+ (inflate-state-n-values-read state) len))
                                 (error 'code-lengths-bounds-error))
                               (fill lengths sym :start (inflate-state-n-values-read state)
                                     :end (+ (inflate-state-n-values-read state) len))
                               (incf (inflate-state-n-values-read state) len)))))
                     finally (assert (= n-values (inflate-state-n-values-read state)))))

             ;; Basic starter functions.
             (done (state)
               (declare (ignore state))
               (throw 'inflate-done t))

             (block-type (state)
               (cond
                 ((inflate-state-final-block-p state)
                  (align-bits-bytewise state)
                  (setf (inflate-state-state state)
                        (ecase (inflate-state-data-format state)
                          (deflate
                              (setf (inflate-state-done state) t)
                              #'done)
                          (zlib #'check-zlib-adler32)
                          (gzip #'gzip-crc32))))
                 (t
                  (ensure-bits 3 state)
                  (setf (inflate-state-final-block-p state) (= 1 (read-bits 1 state)))
                  (ecase (read-bits 2 state)
                    (#.+block-no-compress+
                       (transition-to uncompressed-block))
                    (#.+block-fixed-codes+
                       (setf (inflate-state-literal/length-table state)
                             *fixed-literal/length-table*
                             (inflate-state-distance-table state)
                             *fixed-distance-table*)
                       (transition-to literal/length))
                    (#.+block-dynamic-codes+
                       (transition-to dynamic-tables))
                    (#.+block-invalid+
                       (error 'reserved-block-type-error))))))

             ;; processing uncompressed blocks

             (uncompressed-block (state)
               (align-bits-bytewise state)
               (setf (inflate-state-length state) (ensure-and-read-bits 16 state))
               (transition-to uncompressed-block-checksum))

             (uncompressed-block-checksum (state)
               (let ((nlen (ensure-and-read-bits 16 state)))
                 (unless (zerop (logand (inflate-state-length state) nlen))
                   ;; Apparently Adobe's PDF generator(s) get this wrong, so let the
                   ;; user continue on if they choose to do so.
                   (cerror "Use the invalid stored block length."
                           'invalid-stored-block-length-error))
                 (transition-to copy-bytes)))

             (copy-bytes (state)
               (declare (type inflate-state state))
               (if (zerop (inflate-state-length state))
                   (setf (inflate-state-state state) #'block-type)
                   (let ((n-copied-bytes (min (inflate-state-length state)
                                              (- (inflate-state-input-end state)
                                                 (inflate-state-input-index state))
                                              (- (inflate-state-output-end state)
                                                 (inflate-state-output-index state)))))
                     (cond
                       ((zerop n-copied-bytes) (throw 'inflate-done nil))
                       (t
                        (replace (inflate-state-output state)
                                 (inflate-state-input state)
                                 :start1 (inflate-state-output-index state)
                                 :end1 (+ (inflate-state-output-index state)
                                          n-copied-bytes)
                                 :start2 (inflate-state-input-index state)
                                 :end2 (+ (inflate-state-input-index state)
                                          n-copied-bytes))
                        (incf (inflate-state-input-index state) n-copied-bytes)
                        (incf (inflate-state-output-index state) n-copied-bytes)
                        (decf (inflate-state-length state) n-copied-bytes)))))
               (values))

             ;; dynamic block compression tables

             (dynamic-tables (state)
               (declare (type inflate-state state))
               (ensure-bits 14 state)
               (setf (inflate-state-n-length-codes state) (+ (read-bits 5 state) 257)
                     (inflate-state-n-distance-codes state) (+ (read-bits 5 state) 1)
                     (inflate-state-n-codes state) (+ (read-bits 4 state) 4)
                     (inflate-state-n-values-read state) 0)
               (transition-to dynamic-code-lengths))

             (dynamic-code-lengths (state)
               (declare (type inflate-state state))
               (loop while (< (inflate-state-n-values-read state)
                              (inflate-state-n-codes state))
                     do (ensure-bits 3 state)
                        (record-code-length state (read-bits 3 state)))
               (loop while (< (inflate-state-n-values-read state) +max-n-code-lengths+)
                     do (record-code-length state 0))
               (setf (inflate-state-codes-table state)
                     (construct-huffman-decode-table (inflate-state-code-lengths state)
                                                     +max-n-code-lengths+)
                     (inflate-state-n-values-read state) 0)
               (transition-to dynamic-literal/length+distance-tables))

             (dynamic-literal/length+distance-tables (state)
               (declare (type inflate-state state))
               (read-dynamic-table state (inflate-state-codes-table state)
                                   (+ (inflate-state-n-length-codes state)
                                      (inflate-state-n-distance-codes state)))
               (setf (inflate-state-literal/length-table state)
                     (construct-huffman-decode-table (inflate-state-code-lengths state)
                                                     (inflate-state-n-length-codes state)))
               (setf (inflate-state-distance-table state)
                     (construct-huffman-decode-table (inflate-state-code-lengths state)
                                                     (inflate-state-n-distance-codes state)
                                                     (inflate-state-n-length-codes state)))
               (transition-to literal/length))

             ;; normal operation on compressed blocks

             (literal/length (state)
               (declare (type inflate-state state))
               (let ((value (decode-value (inflate-state-literal/length-table state)
                                          state)))
                 (declare (type (integer 0 288) value))
                 (cond
                   ((< value 256)
                    (setf (inflate-state-length state) value)
                    (transition-to literal))
                   ((> value 285)
                    (error 'illegal-length-code-error :code value))
                   ((> value 256)
                    (setf (inflate-state-length-code state) (- value 257))
                    (transition-to length-code))
                   (t #+nil (= value 256)
                    (transition-to block-type)))))

             (literal (state)
               (declare (type inflate-state state))
               (cond
                 ((= (inflate-state-output-index state)
                     (inflate-state-output-end state)) (throw 'inflate-done nil))
                 (t (setf (aref (inflate-state-output state)
                                (inflate-state-output-index state))
                          (inflate-state-length state))
                  (incf (inflate-state-output-index state))
                  (transition-to literal/length))))

             (length-code (state)
               (declare (type inflate-state state))
               (let* ((length-code (inflate-state-length-code state))
                      (length-extra (ensure-and-read-bits (n-length-extra-bits length-code) state)))
                 (setf (inflate-state-length state)
                       (+ (length-base length-code) length-extra))
                 (transition-to distance)))

             (distance (state)
               (declare (type inflate-state state))
               (let ((value (decode-value (inflate-state-distance-table state)
                                          state)))
                 (when (> value 29)
                   (error 'illegal-distance-code-error :code value))
                 (setf (inflate-state-distance state) value)
                 (transition-to distance-extra)))

             (distance-extra (state)
               (declare (type inflate-state state))
               (let* ((bits (n-distance-extra-bits (inflate-state-distance state)))
                      (distance-extra (if (zerop bits)
                                          0
                                          (ensure-and-read-bits bits state))))
                 (setf (inflate-state-distance state)
                       (+ (distance-base (inflate-state-distance state)) distance-extra))
                 (transition-to copy-match)))

             (copy-match (state)
               (declare (type inflate-state state))
               (let* ((distance (inflate-state-distance state))
                      (length (inflate-state-length state))
                      (start (inflate-state-output-start state))
                      (index (inflate-state-output-index state))
                      (end (inflate-state-output-end state))
                      (window-index (inflate-state-window-index state))
                      (n-bytes-to-copy (min length (- end index))))
                 (when (= index end)
                   (throw 'inflate-done nil))
                 (flet ((frob-by-copying-from (copy-source copy-index n-bytes-to-copy)
                          (declare (type (simple-array u8 (*)) copy-source))
                          (decf (inflate-state-length state) n-bytes-to-copy)
                          (incf (inflate-state-output-index state) n-bytes-to-copy)
                          (loop with output = (inflate-state-output state)
                                for i from index below (the fixnum (+ index n-bytes-to-copy))
                                for j from copy-index below (the fixnum (+ copy-index n-bytes-to-copy))
                                do (setf (aref output i) (aref copy-source j)))))
                   (cond
                     ((<= distance (- index start))
                      ;; we are within the output we have produced
                      (frob-by-copying-from (inflate-state-output state)
                                            (- index distance)
                                            n-bytes-to-copy))
                     (t
                      (let ((copy-index (+ (- window-index distance) (- index start))))
                        (cond
                          ((not (minusp copy-index))
                           ;; we are within the non-wraparound portion of the window
                           ;;
                           ;; can only copy up to the window's index, though
                           (let ((n-bytes-to-copy (min n-bytes-to-copy (- window-index copy-index))))
                             (frob-by-copying-from (inflate-state-window state)
                                                   copy-index
                                                   n-bytes-to-copy)))
                          (t
                           ;; we are within the wraparound portion of the window
                           (let* ((copy-index (+ copy-index
                                                 (length (inflate-state-window state))))
                                  (n-bytes-to-copy (min n-bytes-to-copy
                                                        (- (length (inflate-state-window state))
                                                           copy-index))))
                             (frob-by-copying-from (inflate-state-window state)
                                                   copy-index
                                                   n-bytes-to-copy)))))))
                   (when (zerop (inflate-state-length state))
                     (transition-to literal/length)))))

             ;; GZIP
             (gzip-header-id (state)
               (declare (type inflate-state state))
               (let ((header-field (ensure-and-read-bits 16 state)))
                 (unless (and (= (ldb (byte 8 0) header-field) #x1f)
                              (= (ldb (byte 8 8) header-field) #x8b))
                   (error 'invalid-gzip-header-error))
                 (transition-to gzip-cm)))

             (gzip-cm (state)
               (declare (type inflate-state state))
               (let ((cm-byte (ensure-and-read-bits 8 state)))
                 (setf (inflate-state-header state)
                       (make-instance 'gzip-header :compression-method cm-byte))
                 (transition-to gzip-flags)))

             (gzip-flags (state)
               (declare (type inflate-state state))
               (let ((flags-byte (ensure-and-read-bits 8 state)))
                 (setf (flags (inflate-state-header state)) flags-byte)
                 (transition-to gzip-mtime)))

             (gzip-mtime (state)
               (declare (type inflate-state state))
               (let ((mtime (ensure-and-read-bits 32 state)))
                 (setf (mtime (inflate-state-header state)) mtime)
                 (transition-to gzip-xfl)))

             (gzip-xfl (state)
               (declare (type inflate-state state))
               (let ((xfl-byte (ensure-and-read-bits 8 state)))
                 (setf (extra-flags (inflate-state-header state)) xfl-byte)
                 (transition-to gzip-os)))

             (gzip-os (state)
               (declare (type inflate-state state))
               (let ((os-byte (ensure-and-read-bits 8 state)))
                 (setf (os (inflate-state-header state)) os-byte)
                 (transition-to gzip-xlen-len)))

             (gzip-xlen-len (state)
               (declare (type inflate-state state))
               (let ((flags (flags (inflate-state-header state))))
                 (cond
                   ((logbitp +gzip-flag-extra+ flags)
                    (error "gzip extra field not supported yet"))
                   (t
                    (transition-to gzip-fname)))))

             (gzip-fname (state)
               (declare (type inflate-state state))
               (process-gzip-zero-terminated-field state +gzip-flag-name+
                                                   #'filename #'(setf filename)
                                                   #'gzip-fcomment))

             (gzip-fcomment (state)
               (declare (type inflate-state state))
               (process-gzip-zero-terminated-field state +gzip-flag-comment+
                                                   #'comment #'(setf comment)
                                                   #'gzip-crc16))

             (process-gzip-zero-terminated-field (state control-bit
                                                        slot set-slot
                                                        next-state)
               (let ((header (inflate-state-header state)))
                 (cond
                   ((logbitp control-bit (flags header))
                    (let ((byte (ensure-and-read-bits 8 state)))
                      (cond
                        ((zerop byte)
                         ;; the end, convert to sane form
                         (funcall set-slot
                                  (coerce (funcall slot header)
                                          '(vector u8))
                                  header)
                         (setf (inflate-state-state state) next-state))
                        (t
                         ;; wish we could use PUSH here
                         (funcall set-slot
                                  (cons byte (funcall slot header))
                                  header)))))
                   (t
                    (setf (inflate-state-state state) next-state)))
                 (values)))

             (gzip-crc16 (state)
               (declare (type inflate-state state))
               (let ((header (inflate-state-header state)))
                 (when (logbitp +gzip-flag-crc+ (flags header))
                   (let ((crc16 (ensure-and-read-bits 16 state)))
                     ;; FIXME: would be good to perform integrity checking here
                     (declare (ignore crc16))))
                 (transition-to block-type)))

             (gzip-crc32 (state)
               (declare (type inflate-state state))
               (let ((stored (ensure-and-read-bits 32 state))
                     (crc32 (inflate-state-checksum state)))
                 (update crc32
                         (inflate-state-output state)
                         (inflate-state-output-start state)
                         (- (inflate-state-output-index state)
                            (inflate-state-output-start state)))
                 (unless (= stored (checksum crc32))
                   (error 'invalid-checksum-error
                          :stored stored
                          :computed (checksum crc32)
                          :kind :crc32))
                 (transition-to gzip-isize)))

             (gzip-isize (state)
               (declare (type inflate-state state))
               (let ((isize (ensure-and-read-bits 32 state)))
                 (declare (ignore isize))
                 (setf (inflate-state-done state) t)
                 (transition-to done)))

             ;; ZLIB
             (zlib-cmf (state)
               (declare (type inflate-state state))
               (let ((cmf-byte (ensure-and-read-bits 8 state)))
                 (setf (inflate-state-header state)
                       (make-instance 'zlib-header :cmf cmf-byte))
                 (transition-to zlib-flags)))

             (zlib-flags (state)
               (declare (type inflate-state state))
               (let ((flags-byte (ensure-and-read-bits 8 state))
                     (header (inflate-state-header state)))
                 ;; check
                 (unless (zerop (mod (+ (* (cmf header) 256) flags-byte) 31))
                   (error 'invalid-zlib-header-error))
                 (setf (flags header) flags-byte)
                 (transition-to zlib-fdict)))

             (zlib-fdict (state)
               (declare (type inflate-state state))
               (let* ((header (inflate-state-header state))
                      (flags-byte (flags header)))
                 (when (logbitp +zlib-flag-fdict+ flags-byte)
                   (let ((fdict (ensure-and-read-bits 32 state)))
                     (setf (fdict header) fdict)))
                 (transition-to block-type)))

             (check-zlib-adler32 (state)
               (declare (type inflate-state state))
               (let ((stored (let ((x (ensure-and-read-bits 32 state)))
                               (logior (ash (ldb (byte 8 0) x) 24)
                                       (ash (ldb (byte 8 8) x) 16)
                                       (ash (ldb (byte 8 16) x) 8)
                                       (ldb (byte 8 24) x))))
                     (adler32 (inflate-state-checksum state)))
                 (update adler32
                         (inflate-state-output state)
                         (inflate-state-output-start state)
                         (- (inflate-state-output-index state)
                            (inflate-state-output-start state)))
                 (unless (= stored
                            (checksum adler32))
                   (error 'invalid-checksum-error
                          :stored stored
                          :computed (checksum adler32)
                          :kind :adler32))
                 (setf (inflate-state-done state) t)
                 (transition-to done)))
             )
      (unless (inflate-state-state state)
        (setf (inflate-state-state state)
              (ecase (inflate-state-data-format state)
                (deflate #'block-type)
                (zlib #'zlib-cmf)
                (gzip #'gzip-header-id))))
      (loop (funcall (inflate-state-state state) state)))))


;;; bzip2's decompress.c looks relatively simple, but a great deal of
;;; complexity and cleverness is hidden behind C preprocessor macro.
;;; The single biggest help in understand what is going on behind the
;;; macros is to read "Coroutines in C" by Simon Tatham:
;;;
;;;  http://www.chiark.greenend.org.uk/~sgtatham/coroutines.html
;;;
;;; decompress.c is using the same technique described in the paper,
;;; although with a slightly different implementation.
;;;
;;; Lisp, fortunately/alas, does not admit the same sort of techniques
;;; that C does--at least not expressed exactly the same way.  So our
;;; translation naturally differs in some places.  For example, to make
;;; it easier to figure out how much state we have to preserve, we
;;; choose to read more in at one time than decompress.c--the magic
;;; number header all at once or the bits for the mapping table in
;;; larger chunks than 1 bit at a time, for instance.

;;; Reading things in larger chunks than bits means that we have to do
;;; bit-reversal of various quantities.

(defun reverse-ub4 (x)
  (let ((table (load-time-value (make-array 16 :element-type 'fixnum
                                            :initial-contents '(0 8 4 12
                                                                2 10 6 14
                                                                1 9 5 13
                                                                3 11 7 15)))))
    (aref table x)))

(defun reverse-u8 (x)
  (logior (ash (reverse-ub4 (ldb (byte 4 0) x)) 4)
          (reverse-ub4 (ldb (byte 4 4) x))))

(defun reverse-ub16 (x)
  (logior (ash (reverse-u8 (ldb (byte 8 0) x)) 8)
          (reverse-u8 (ldb (byte 8 8) x))))

(defvar *dummy-vec* (make-array #.+bz-max-alpha-size+ :element-type 'u32))

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

  (limit #1=(let ((w (make-array +bz-n-groups+)))
           (dotimes (i +bz-n-groups+ w)
             (setf (aref w i) (make-array +bz-max-alpha-size+
                                          :element-type 'u32))))
         :type (simple-array t (#.+bz-n-groups+)))
  (base #1#
        :type (simple-array t (#.+bz-n-groups+)))
  (perm #1#
        :type (simple-array t (#.+bz-n-groups+)))
  (min-lengths (make-array #.+bz-n-groups+ :element-type 'u32)
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
  (g-limit *dummy-vec*
           :type (simple-array u32 (#.+bz-max-alpha-size+)))
  (g-base *dummy-vec*
          :type (simple-array u32 (#.+bz-max-alpha-size+)))
  (g-perm *dummy-vec*
          :type (simple-array u32 (#.+bz-max-alpha-size+))))

(defmethod print-object ((object bzip2-state) stream)
  (print-unreadable-object (object stream)
    (format stream "Bzip2 state bits: ~X/~D input: ~D/~D output ~D/~D"
            (bzip2-state-bits object)
            (bzip2-state-n-bits object)
            (bzip2-state-input-index object)
            (bzip2-state-input-end object)
            (bzip2-state-output-index object)
            (bzip2-state-output-end object))))

(defun make-maps (state)
  (declare (type bzip2-state state))
  (loop with n-in-use = 0
     with in-use-table = (bzip2-state-in-use state)
     with seq-to-unseq = (bzip2-state-seq-to-unseq state)
     for i from 0 below 256
     when (aref in-use-table i)
     do (setf (aref seq-to-unseq n-in-use) i
              n-in-use (1+ n-in-use))
     finally
       (return (setf (bzip2-state-n-in-use state) n-in-use))))

(defun make-decode-tables (state group min-len max-len alpha-size)
  (declare (type bzip2-state state))
  (let* ((limit (aref (bzip2-state-limit state) group))
         (base (aref (bzip2-state-base state) group))
         (perm (aref (bzip2-state-perm state) group))
         (len (bzip2-state-len state))
         (rmi (array-row-major-index len group 0)))
    (loop with pp = 0
       for i from min-len to max-len
       do (dotimes (j alpha-size)
            (when (= (row-major-aref len (+ rmi j)) i)
              (setf (aref perm pp) j)
              (incf pp))))
    (loop for i from 0 below +bz-max-code-len+
       do (setf (aref base i) 0
                (aref limit i) 0))
    (loop for i from 0 below alpha-size
       do (incf (aref base (1+ (row-major-aref len (+ i rmi))))))
    (loop for i from 1 below +bz-max-code-len+
       do (incf (aref base i)
                (aref base (1- i))))
    (loop with vec = 0
       for i from min-len to max-len
       do (incf vec (- (aref base (1+ i))
                       (aref base i)))
         (setf (aref limit i) (1- vec)
               vec (ash vec 1)))
    (loop for i from (+ min-len 1) to max-len
       do (setf (aref base i)
                (- (ash (1+ (aref limit (1- i))) 1)
                   (aref base i))))))

(defun undo-rle-obuf-to-output (state)
  (cond
    ((bzip2-state-block-randomized-p state)
     (error 'bzip2-randomized-blocks-unimplemented))
    (t
     (let ((calculated-block-crc (bzip2-state-calculated-block-crc state))
           (out-ch (bzip2-state-out-ch state))
           (out-len (bzip2-state-out-len state))
           (n-blocks-used (bzip2-state-n-blocks-used state))
           (k0 (bzip2-state-k0 state))
           (k1 0)
           (tt (bzip2-state-tt state))
           (t-position (bzip2-state-t-position state))
           (nblockpp (1+ (bzip2-state-nblock state)))
           (output (bzip2-state-output state))
           (index (bzip2-state-output-index state))
           (end (bzip2-state-output-end state)))
       (declare (type u32 calculated-block-crc))
       (declare (type (integer 0 260) out-len))
       (declare (type u8 k0 k1))
       (declare (type (integer 0 900001) n-blocks-used nblockpp))
       (declare (type u32 t-position))
       (macrolet ((get-fast ()
                    `(prog2
                         (setf t-position (aref tt t-position))
                         (logand t-position #xff)
                       (setf t-position (ash t-position -8)))))
         (tagbody
          START
            ;; "try to finish existing run"
            (when (zerop out-len)
              (go GRAB-MORE))
            (loop
               (when (= index end)
                 (go FINISH))
               (when (= out-len 1)
                 (go LEN-EQUAL-ONE))
               (setf (aref output index) out-ch)
               (setf calculated-block-crc
                     (logand #xffffffff
                             (logxor (ash calculated-block-crc 8)
                                     (aref +bzip2-crc32-table+
                                           (logxor (ash calculated-block-crc -24) out-ch)))))
               (decf out-len)
               (incf index))
          LEN-EQUAL-ONE
            (when (= index end)
              (setf out-len 1)
              (go FINISH))
            (setf (aref output index) out-ch)
            (setf calculated-block-crc
                  (logand #xffffffff
                          (logxor (ash calculated-block-crc 8)
                                  (aref +bzip2-crc32-table+
                                        (logxor (ash calculated-block-crc -24) out-ch)))))
            (incf index)
          GRAB-MORE
            ;; "Only caused by corrupt data stream?"
            (when (> n-blocks-used nblockpp)
              (return-from undo-rle-obuf-to-output t))
            (when (= n-blocks-used nblockpp)
              (setf out-len 0)
              (go FINISH))
            (setf out-ch k0)
               
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (unless (= k1 k0)
              (setf k0 k1)
              (go LEN-EQUAL-ONE))
            (when (= n-blocks-used nblockpp)
              (go LEN-EQUAL-ONE))

            (setf out-len 2)
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (when (= n-blocks-used nblockpp)
              (go CONTINUE))
            (unless (= k1 k0)
              (setf k0 k1)
              (go CONTINUE))

            (setf out-len 3)
            (setf k1 (get-fast))
            (incf n-blocks-used)
            (when (= n-blocks-used nblockpp)
              (go CONTINUE))
            (unless (= k1 k0)
              (setf k0 k1)
              (go CONTINUE))

            (setf k1 (get-fast))
            (incf n-blocks-used)
            (setf out-len (+ k1 4))
            (setf k0 (get-fast))
            (incf n-blocks-used)
          CONTINUE
            (go START)
          FINISH)
       
            #+nil
            (incf (bzip2-state-total-out state)
                  (- index (bzip2-state-output-index state) ))
            ;; Restore cached values.
            (setf (bzip2-state-calculated-block-crc state) calculated-block-crc
                  (bzip2-state-out-ch state) out-ch
                  (bzip2-state-out-len state) out-len
                  (bzip2-state-n-blocks-used state) n-blocks-used
                  (bzip2-state-k0 state) k0
                  (bzip2-state-t-position state) t-position
                  (bzip2-state-output-index state) index)
          nil)))))

;;; decompress.c has various logic relating to whether the user has
;;; chosen "small" decompression, which uses less memory.  We're just
;;; going to be memory-intensive and always pick the large option.  Maybe
;;; someday we can come back and add the small option.

(defun %bzip2-state-machine (state)
  (declare (type bzip2-state state))
  ;; See the enormous comment in %INFLATE-STATE-MACHINE for what's going
  ;; on here.
  (macrolet ((transition-to (next-state)
               `(progn
                  (setf (bzip2-state-state state) #',next-state)
                  #+(or sbcl cmu)
                  (,next-state state))))
    (labels (
             (read-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type bzip2-state state))
               (prog1
                   ;; We don't use (BYTE N (- ...)) here because doing it
                   ;; this way is ~10% faster on SBCL.
                   (ldb (byte n 0)
                        (ash (bzip2-state-bits state)
                             (the (integer -31 0)
                               (- n (bzip2-state-n-bits state)))))
                 (decf (bzip2-state-n-bits state) n)))

             (ensure-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type bzip2-state state))
               (let ((bits (bzip2-state-bits state))
                     (n-bits (bzip2-state-n-bits state))
                     (input-index (bzip2-state-input-index state)))
                 (declare (type u32 bits))
                 (loop while (< n-bits n)
                       when (>= input-index (bzip2-state-input-end state))
                         do (progn
                              (setf (bzip2-state-bits state) bits
                                    (bzip2-state-n-bits state) n-bits
                                    (bzip2-state-input-index state) input-index)
                              (throw 'bzip2-done nil))
                       do (let ((byte (aref (bzip2-state-input state) input-index)))
                            (declare (type u8 byte))
                            (setf bits
                                  (logand #xffffffff (logior (ash bits 8) byte)))
                            (incf n-bits 8)
                            (incf input-index))
                       finally (setf (bzip2-state-bits state) bits
                                     (bzip2-state-n-bits state) n-bits
                                     (bzip2-state-input-index state) input-index))))

             (ensure-and-read-bits (n state)
               (ensure-bits n state)
               (read-bits n state))

             (bzip2-header (state)
               (declare (type bzip2-state state))
               (let ((header-field (ensure-and-read-bits 32 state)))
                 (declare (type u32 header-field))
                 (unless (and (= (ldb (byte 8 24) header-field) +bz-header-b+)
                              (= (ldb (byte 8 16) header-field) +bz-header-z+)
                              (= (ldb (byte 8 8) header-field) +bz-header-h+))
                   (error 'invalid-bzip2-data))
                 (let ((block-size-magic-byte (ldb (byte 8 0) header-field)))
                   (unless (<= (+ +bz-header-0+ 1)
                               block-size-magic-byte
                               (+ +bz-header-0+ 9))
                     (error 'invalid-bzip2-data))
                   (setf (bzip2-state-100k-block-size state) (- block-size-magic-byte
                                                                +bz-header-0+))
                   ;; BZIP2 SMALL
                   (setf (bzip2-state-tt state)
                         (make-array (* (bzip2-state-100k-block-size state) +100k+)
                                     :element-type 'u32))
                   (transition-to bzip2-block-header1))))

             (bzip2-block-header1 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (case byte
                   (#x17 (transition-to bzip2-end-header2))
                   (#x31 (transition-to bzip2-block-header2))
                   (t (error 'invalid-bzip2-data)))))

             (bzip2-block-header2 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x41)
                     (transition-to bzip2-block-header3)
                     (error 'invalid-bzip2-data))))

             (bzip2-block-header3 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x59)
                     (transition-to bzip2-block-header4)
                     (error 'invalid-bzip2-data))))

             (bzip2-block-header4 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x26)
                     (transition-to bzip2-block-header5)
                     (error 'invalid-bzip2-data))))

             (bzip2-block-header5 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x53)
                     (transition-to bzip2-block-header6)
                     (error 'invalid-bzip2-data))))

             (bzip2-block-header6 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (unless (= byte #x59)
                   (error 'invalid-bzip2-data))
                 (incf (bzip2-state-current-block-number state))
                 (transition-to bzip2-block-crc32)))

             (bzip2-block-crc32 (state)
               (declare (type bzip2-state state))
               (let ((crc32-hi (ensure-and-read-bits 16 state))
                     (crc32-lo (ensure-and-read-bits 16 state)))
                 (setf (bzip2-state-stored-block-crc state)
                       (logior (ash crc32-hi 16) crc32-lo))
                 (transition-to bzip2-block-randombit)))

             (bzip2-block-randombit (state)
               (declare (type bzip2-state state))
               (let ((randomized-p (ensure-and-read-bits 1 state)))
                 (setf (bzip2-state-block-randomized-p state) (= randomized-p 1))
                 (transition-to bzip2-original-pointer)))

             (bzip2-original-pointer (state)
               (declare (type bzip2-state state))
               (let ((original-pointer (ensure-and-read-bits 24 state)))
                 (unless (<= 0 original-pointer
                             (+ 10 (* (bzip2-state-100k-block-size state) +100k+)))
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-original-pointer state) original-pointer)
                 (transition-to bzip2-mapping-table1)))

             (bzip2-mapping-table1 (state)
               (declare (type bzip2-state state))
               (let ((in-use-16 (reverse-ub16 (ensure-and-read-bits 16 state))))
                 (setf (bzip2-state-in-use-16 state) in-use-16)
                 (setf (bzip2-state-i state) 0)
                 (fill (bzip2-state-in-use state) nil)
                 (transition-to bzip2-mapping-table2)))

             (bzip2-mapping-table2 (state)
               (declare (type bzip2-state state))
               (loop with in-use-16 = (bzip2-state-in-use-16 state)
                     with in-use-table = (bzip2-state-in-use state)
                     while (< (bzip2-state-i state) 16)
                     when (logbitp (bzip2-state-i state) in-use-16)
                       do (let ((in-use (reverse-ub16 (ensure-and-read-bits 16 state))))
                            (dotimes (i 16)
                              (setf (aref in-use-table (+ (* (bzip2-state-i state) 16)
                                                          i))
                                    (logbitp i in-use))))
                     do
                  (incf (bzip2-state-i state)))
               (let ((n-in-use (make-maps state)))
                 (when (zerop n-in-use)
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-alpha-size state)
                       (+ n-in-use 2))
                 (transition-to bzip2-selector1)))

             (bzip2-selector1 (state)
               (declare (type bzip2-state state))
               (let ((n-groups (ensure-and-read-bits 3 state)))
                 (unless (<= 2 n-groups 6)
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-n-groups state) n-groups)
                 (transition-to bzip2-selector2)))

             (bzip2-selector2 (state)
               (declare (type bzip2-state state))
               (let ((n-selectors (ensure-and-read-bits 15 state)))
                 (unless (plusp n-selectors)
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-n-selectors state) n-selectors)
                 (setf (bzip2-state-i state) 0)
                 (transition-to bzip2-selector3a)))

             (bzip2-selector3a (state)
               (declare (type bzip2-state state))
               (setf (bzip2-state-j state) 0)
               (transition-to bzip2-selector3b))

             (bzip2-selector3b (state)
               (declare (type bzip2-state state))
               (loop
                 do (let ((bit (ensure-and-read-bits 1 state)))
                      (when (zerop bit) (loop-finish))
                      (when (>= (incf (bzip2-state-j state))
                                (bzip2-state-n-groups state))
                        (error 'invalid-bzip2-data)))
                 finally 
              (setf (aref (bzip2-state-selector-mtf state)
                          (bzip2-state-i state))
                    (bzip2-state-j state)))
               (if (< (incf (bzip2-state-i state))
                      (bzip2-state-n-selectors state))
                   (transition-to bzip2-selector3a)
                   (transition-to bzip2-selector-undo-mtf-values)))

             (bzip2-selector-undo-mtf-values (state)
               (declare (type bzip2-state state))
               (let ((pos (make-array +bz-n-groups+ 
                                      :element-type 'u8))
                     (n-groups (bzip2-state-n-groups state))
                     (n-selectors (bzip2-state-n-selectors state))
                     (selector-table (bzip2-state-selector state))
                     (selector-mtf (bzip2-state-selector-mtf state)))
                 (declare (dynamic-extent pos))
                 (dotimes (i n-groups)
                   (setf (aref pos i) i))
                 (dotimes (i n-selectors)
                   (let* ((v (aref selector-mtf i))
                          (tmp (aref pos v)))
                     (loop until (zerop v)
                           do (setf (aref pos v) (aref pos (1- v)))
                              (decf v))
                     (setf (aref pos 0) tmp)
                     (setf (aref selector-table i) tmp)))
                 (setf (bzip2-state-j state) 0)
                 (transition-to bzip2-coding-tables-groups-loop)))

             (bzip2-coding-tables-groups-loop (state)
               (declare (type bzip2-state state))
               (cond
                 ((< (bzip2-state-j state) (bzip2-state-n-groups state))
                  (setf (bzip2-state-curr state) (ensure-and-read-bits 5 state)
                        (bzip2-state-i state) 0)
                  (transition-to bzip2-coding-tables-alpha-loop))
                 (t
                  (transition-to bzip2-create-huffman-decode-tables))))

             (bzip2-coding-tables-alpha-loop (state)
               (declare (type bzip2-state state))
               (unless (<= 1 (bzip2-state-curr state) 20)
                 (error 'invalid-bzip2-data))
               (let ((uc (ensure-and-read-bits 1 state)))
                 (cond
                   ((zerop uc)
                    (setf (aref (bzip2-state-len state) (bzip2-state-j state) (bzip2-state-i state))
                          (bzip2-state-curr state))
                    (cond
                      ((< (incf (bzip2-state-i state))
                          (bzip2-state-alpha-size state))
                       (bzip2-coding-tables-alpha-loop state))
                      (t
                       (incf (bzip2-state-j state))
                       (transition-to bzip2-coding-tables-groups-loop))))
                   (t
                    (transition-to bzip2-coding-tables-alpha-loop2)))))

             (bzip2-coding-tables-alpha-loop2 (state)
               (declare (type bzip2-state state))
               (let ((uc (ensure-and-read-bits 1 state)))
                 (if (zerop uc)
                     (incf (bzip2-state-curr state))
                     (decf (bzip2-state-curr state)))
                 (transition-to bzip2-coding-tables-alpha-loop)))

             (bzip2-create-huffman-decode-tables (state)
               (declare (type bzip2-state state))
               (loop with n-groups = (bzip2-state-n-groups state)
                     with len = (bzip2-state-len state)
                     for x from 0 below n-groups
                     do (loop with minLen = 32
                              with maxLen = 0
                              with alpha-size = (bzip2-state-alpha-size state)
                              for y from 0 below alpha-size
                              do (let ((xy (aref len x y)))
                                   (setf maxLen (max maxLen xy)
                                         minLen (min minLen xy)))
                              finally
                           (make-decode-tables state x minLen maxLen alpha-size)
                           (setf (aref (bzip2-state-min-lengths state) x) minLen))
                     finally
                  ;; We're not 'returning' anything here, we're just
                  ;; forcing this call to be in tail position.
                  (return (transition-to bzip2-initialize-mtf-values))))

             (bzip2-initialize-mtf-values (state)
               (declare (type bzip2-state state))
               (loop
                 with kk = (1- +mtfa-size+)
                 with mtfa = (bzip2-state-mtfa state)
                 with mtfbase = (bzip2-state-mtfbase state)
                   initially
                (setf (bzip2-state-EOB state) (1+ (bzip2-state-n-in-use state))
                      (bzip2-state-nblockMAX state) (* 100000 (bzip2-state-100k-block-size state))
                      (bzip2-state-group-number state) -1
                      (bzip2-state-group-position state) 0)
                (fill (bzip2-state-unzftab state) 0)
                 for i from (1- (floor 256 +mtfl-size+)) downto 0
                 do (loop for j from (1- +mtfl-size+) downto 0
                          do
                       (setf (aref mtfa kk) (+ (* i +mtfl-size+) j))
                       (decf kk)
                          finally
                       (setf (aref mtfbase i) (1+ kk)))
                 finally
              (setf (bzip2-state-nblock state) 0
                    (bzip2-state-mtf-continuation state) #'bzip2-enter-mtf-decode-loop)
              ;; We're not 'returning' anything here, we're just
              ;; forcing this call to be in tail position.
              (return (transition-to bzip2-get-mtf-value))))

             (bzip2-get-mtf-value (state)
               (declare (type bzip2-state state))
               (when (zerop (bzip2-state-group-position state))
                 (when (>= (incf (bzip2-state-group-number state))
                           (bzip2-state-n-selectors state))
                   (error 'invalid-bzip2-data))
                 (let ((s (aref (bzip2-state-selector state)
                                (bzip2-state-group-number state))))
                   (setf (bzip2-state-group-position state) +bz-g-size+
                         (bzip2-state-g-minlen state) (aref (bzip2-state-min-lengths state) s)
                         (bzip2-state-g-limit state) (aref (bzip2-state-limit state) s)
                         (bzip2-state-g-perm state) (aref (bzip2-state-perm state) s)
                         (bzip2-state-g-base state) (aref (bzip2-state-base state) s))))
               (decf (bzip2-state-group-position state))
               (setf (bzip2-state-zn state) (bzip2-state-g-minlen state))
               (transition-to bzip2-get-mtf-value1))

             (bzip2-get-mtf-value1 (state)
               (declare (type bzip2-state state))
               (let ((zvec (ensure-and-read-bits (bzip2-state-zn state) state)))
                 (setf (bzip2-state-zvec state) zvec)
                 (transition-to bzip2-get-mtf-value2)))

             (bzip2-get-mtf-value2 (state)
               (declare (type bzip2-state state))
               (when (> (bzip2-state-zn state) 20)
                 (error 'invalid-bzip2-data))
               (cond
                 ((<= (bzip2-state-zvec state)
                      (aref (bzip2-state-g-limit state)
                            (bzip2-state-zn state)))
                  (transition-to bzip2-get-mtf-value-done))
                 (t
                  (incf (bzip2-state-zn state))
                  (transition-to bzip2-get-mtf-value3))))

             (bzip2-get-mtf-value3 (state)
               (declare (type bzip2-state state))
               (let ((zj (ensure-and-read-bits 1 state)))
                 (setf (bzip2-state-zvec state)
                       (logior (ash (bzip2-state-zvec state) 1) zj))
                 (transition-to bzip2-get-mtf-value2)))

             (bzip2-get-mtf-value-done (state)
               (declare (type bzip2-state state))
               (let* ((g-base (bzip2-state-g-base state))
                      (zn (bzip2-state-zn state))
                      (zvec (bzip2-state-zvec state))
                      (index (- zvec (aref g-base zn))))
                 (when (or (< index 0) (>= index +bz-max-alpha-size+))
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-lval state)
                       (aref (bzip2-state-g-perm state) index))
                 (let ((f (bzip2-state-mtf-continuation state)))
                   (declare (type function f))
                   (setf (bzip2-state-state state) f)
                   (funcall f state))))

             (bzip2-enter-mtf-decode-loop (state)
               (declare (type bzip2-state state))
               (let ((next-sym (bzip2-state-lval state)))
                 (cond
                   ((= next-sym (bzip2-state-EOB state))
                    (transition-to bzip2-prepare-cftab))
                   ((or (= next-sym +bz-runa+) (= next-sym +bz-runb+))
                    (setf (bzip2-state-es state) -1
                          (bzip2-state-N state) 1)
                    (transition-to bzip2-decode-rle-sequence))
                   (t
                    (transition-to bzip2-runc)))))

             (bzip2-decode-rle-sequence (state)
               (declare (type bzip2-state state))
               (let ((next-sym (bzip2-state-lval state)))
                 (cond
                   ((= next-sym +bz-runa+)
                    (incf (bzip2-state-es state) (bzip2-state-N state)))
                   ((= next-sym +bz-runb+)
                    (incf (bzip2-state-es state) (* (bzip2-state-N state) 2))))
                 (setf (bzip2-state-N state) (* (bzip2-state-N state) 2))
                 (setf (bzip2-state-mtf-continuation state) #'bzip2-maybe-finish-rle-sequence)
                 (transition-to bzip2-get-mtf-value)))

             (bzip2-maybe-finish-rle-sequence (state)
               (declare (type bzip2-state state))
               (let ((next-sym (bzip2-state-lval state)))
                 (if (or (= next-sym +bz-runa+) (= next-sym +bz-runb+))
                     (transition-to bzip2-decode-rle-sequence)
                     (transition-to bzip2-finish-rle-sequence))))

             (bzip2-finish-rle-sequence (state)
               (declare (type bzip2-state state))
               (let ((uc (aref (bzip2-state-seq-to-unseq state)
                               (aref (bzip2-state-mtfa state)
                                     (aref (bzip2-state-mtfbase state) 0)))))
                 (incf (aref (bzip2-state-unzftab state) uc)
                       (incf (bzip2-state-es state)))
                 (if (bzip2-state-small-decompression-p state)
                     (error 'bzip2-small-decompression-unimplemented)
                     (loop with nblock = (bzip2-state-nblock state)
                           with nblockMAX = (bzip2-state-nblockMAX state)
                           with tt = (bzip2-state-tt state)
                           repeat (bzip2-state-es state)
                           do
                        (when (>= nblock nblockMAX)
                          (error 'invalid-bzip2-data))
                        (setf (aref tt nblock) uc)
                        (incf nblock)
                           finally
                        (setf (bzip2-state-nblock state) nblock)
                        ;; We're not 'returning' anything here, we're
                        ;; just forcing this call to be in tail
                        ;; position.
                        (return (transition-to bzip2-enter-mtf-decode-loop))))))

             (bzip2-runc (state)
               (declare (type bzip2-state state))
               (let ((next-sym (bzip2-state-lval state))
                     (uc 0))
                 (when (>= (bzip2-state-nblock state)
                           (bzip2-state-nblockMAX state))
                   (error 'invalid-bzip2-data))
                 (let ((mtfbase (bzip2-state-mtfbase state))
                       (mtfa (bzip2-state-mtfa state))
                       (nn (1- next-sym)))
                   (cond
                     ((< nn +mtfl-size+)
                      ;; "avoid general-case expense"
                      (let ((pp (aref mtfbase 0)))
                        (setf uc (aref mtfa (+ pp nn)))
                        (replace mtfa mtfa :start1 (1+ pp) :end1 (+ pp nn 1)
                                 :start2 pp :end2 (+ pp nn))
                        (setf (aref mtfa pp) uc)))
                     (t
                      ;; "general case"
                      (let* ((lno (truncate nn +mtfl-size+))
                             (off (rem nn +mtfl-size+))
                             (pp (+ (aref mtfbase lno) off)))
                        (setf uc (aref mtfa pp))
                        (loop while (> pp (aref mtfbase lno))
                              do (setf (aref mtfa pp) (aref mtfa (1- pp)))
                                 (decf pp))
                        (incf (aref mtfbase lno))
                        (loop for x from lno above 0
                              do 
                           (setf (aref mtfa (decf (aref mtfbase x)))
                                 (aref mtfa (+ (aref mtfbase (1- x)) (1- +mtfl-size+)))))
                        (setf (aref mtfa (decf (aref mtfbase 0))) uc)
                        (when (zerop (aref mtfbase 0))
                          (loop with kk = (1- +mtfa-size+)
                                for ii from (1- (floor 256 +mtfl-size+)) downto 0
                                do (loop for jj from (1- +mtfl-size+) downto 0
                                         do (setf (aref mtfa kk)
                                                  (aref mtfa (+ (aref mtfbase ii) jj)))
                                            (decf kk))
                                   (setf (aref mtfbase ii) (1+ kk)))))))
                   (incf (aref (bzip2-state-unzftab state)
                               (aref (bzip2-state-seq-to-unseq state) uc)))
                   (if (bzip2-state-small-decompression-p state)
                       (error 'bzip2-small-decompression-unimplemented)
                       (setf (aref (bzip2-state-tt state) (bzip2-state-nblock state))
                             (aref (bzip2-state-seq-to-unseq state) uc)))
                   (incf (bzip2-state-nblock state))
                   (setf (bzip2-state-mtf-continuation state) #'bzip2-enter-mtf-decode-loop)
                   (transition-to bzip2-get-mtf-value))))

             (bzip2-prepare-cftab (state)
               (declare (type bzip2-state state))
               (when (or (minusp (bzip2-state-original-pointer state))
                         (>= (bzip2-state-original-pointer state)
                             (bzip2-state-nblock state)))
                 (error 'invalid-bzip2-data))
               (let ((cftab (bzip2-state-cftab state))
                     (unzftab (bzip2-state-unzftab state)))
                 (setf (aref cftab 0) 0)
                 (replace cftab unzftab :start1 1 :end1 257 :start2 0 :end2 256)
                 (loop for i from 1 to 256
                       do (incf (aref cftab i) (aref cftab (1- i))))
                 (loop with nblock = (bzip2-state-nblock state)
                       for i from 0 to 256
                       unless (<= 0 (aref cftab i) nblock)
                         do (error 'invalid-bzip2-data))
                 (setf (bzip2-state-out-len state) 0
                       (bzip2-state-out-ch state) 0
                       (bzip2-state-calculated-block-crc state) #xffffffff)
                 (loop with nblock = (bzip2-state-nblock state)
                       with tt = (bzip2-state-tt state)
                       for i from 0 below nblock
                       do (let ((uc (logand (aref tt i) #xff)))
                            (setf (aref tt (aref cftab uc))
                                  (logior (aref tt (aref cftab uc)) (ash i 8)))
                            (incf (aref cftab uc)))
                       finally
                    (setf (bzip2-state-t-position state)
                          (ash (aref tt (bzip2-state-original-pointer state)) -8))
                    (setf (bzip2-state-n-blocks-used state) 0)
                    (cond
                      ((bzip2-state-block-randomized-p state)
                       (error 'bzip2-randomized-blocks-unimplemented))
                      (t
                       ;; BZIP2-STATE-T-POSITION was sometimes set to
                       ;; a value outside its declared domain. Now
                       ;; TEMP is used to store this value instead.
                       (let ((temp (aref tt (bzip2-state-t-position state))))
                         (setf (bzip2-state-k0 state) (logand #xff temp)
                               (bzip2-state-t-position state) (ash temp -8)))
                       (incf (bzip2-state-n-blocks-used state))))
                    ;; We're not 'returning' anything here, we're just
                    ;; forcing this call to be in tail position.
                    (return (transition-to bzip2-output)))))

             (bzip2-output (state)
               (declare (type bzip2-state state))
               (let ((corruptp (undo-rle-obuf-to-output state)))
                 (when corruptp
                   (error 'invalid-bzip2-data))
                 (unless (and (= (bzip2-state-n-blocks-used state)
                                 (1+ (bzip2-state-nblock state)))
                              (zerop (bzip2-state-out-len state)))
                   (throw 'bzip2-done :ok))
                 (let ((stored (bzip2-state-stored-block-crc state))
                       (calculated (bzip2-state-calculated-block-crc state)))
                   (setf calculated (logand #xffffffff (lognot calculated)))
                   (setf (bzip2-state-calculated-block-crc state) calculated)
                   (unless (= calculated stored)
                     (error 'checksum-mismatch
                            :stored stored
                            :computed calculated
                            :kind :crc32))
                   (setf (bzip2-state-calculated-combined-crc state)
                         (logand #xffffffff
                                 (logior (ash (bzip2-state-calculated-combined-crc state) 1)
                                         (ash (bzip2-state-calculated-combined-crc state) -31))))
                   (setf (bzip2-state-calculated-combined-crc state)
                         (logand #xffffffff
                                 (logxor (bzip2-state-calculated-combined-crc state)
                                         calculated)))
                   (transition-to bzip2-block-header1))))

             (bzip2-end-header2 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x72)
                     (transition-to bzip2-end-header3)
                     (error 'invalid-bzip2-data))))

             (bzip2-end-header3 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x45)
                     (transition-to bzip2-end-header4)
                     (error 'invalid-bzip2-data))))

             (bzip2-end-header4 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x38)
                     (transition-to bzip2-end-header5)
                     (error 'invalid-bzip2-data))))

             (bzip2-end-header5 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (if (= byte #x50)
                     (transition-to bzip2-end-header6)
                     (error 'invalid-bzip2-data))))

             (bzip2-end-header6 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (unless (= byte #x90)
                   (error 'invalid-bzip2-data))
                 (setf (bzip2-state-stored-combined-crc state) 0)
                 (transition-to bzip2-stored-combined-crc32-1)))

             (bzip2-stored-combined-crc32-1 (state)
               (declare (type bzip2-state state))
               (setf (bzip2-state-stored-combined-crc state)
                     (ensure-and-read-bits 8 state))
               (transition-to bzip2-stored-combined-crc32-2))

             (bzip2-stored-combined-crc32-2 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (setf (bzip2-state-stored-combined-crc state)
                       (logand #xffffffff
                               (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                       byte)))
                 (transition-to bzip2-stored-combined-crc32-3)))

             (bzip2-stored-combined-crc32-3 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (setf (bzip2-state-stored-combined-crc state)
                       (logand #xffffffff
                               (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                       byte)))
                 (transition-to bzip2-stored-combined-crc32-4)))

             (bzip2-stored-combined-crc32-4 (state)
               (declare (type bzip2-state state))
               (let ((byte (ensure-and-read-bits 8 state)))
                 (setf (bzip2-state-stored-combined-crc state)
                       (logand #xffffffff
                               (logior (ash (bzip2-state-stored-combined-crc state) 8)
                                       byte)))
                 (unless (= (bzip2-state-stored-combined-crc state)
                            (bzip2-state-calculated-combined-crc state))
                   (error 'checksum-mismatch
                          :stored (bzip2-state-stored-combined-crc state)
                          :computed (bzip2-state-calculated-combined-crc state)
                          :kind :crc32))
                 (setf (bzip2-state-done state) t)
                 (transition-to bzip2-done)))

             (bzip2-done (state)
               (declare (ignore state))
               (throw 'bzip2-done t))
             )
      (unless (bzip2-state-state state)
        (setf (bzip2-state-state state) #'bzip2-header))
      (loop (funcall (the function (bzip2-state-state state)) state)))))

(defun %bzip2-decompress (state input output &key (input-start 0) input-end
                          (output-start 0) output-end)
  (declare (type bzip2-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (bzip2-state-input state) input
          (bzip2-state-input-start state) input-start
          (bzip2-state-input-index state) input-start
          (bzip2-state-input-end state) input-end
          (bzip2-state-output state) output
          (bzip2-state-output-start state) output-start
          (bzip2-state-output-index state) output-start
          (bzip2-state-output-end state) output-end)
    (catch 'bzip2-done
      (%bzip2-state-machine state))
    (values (- (bzip2-state-input-index state) input-start)
            (- (bzip2-state-output-index state) output-start))))

(defun make-bzip2-state ()
  (let ((state (%make-bzip2-state)))
    (setf (dstate-checksum state) (make-instance 'crc-32))
    state))

(defclass bzip2-codec ()
  ())

(defmethod encode ((codec bzip2-codec) in out)
  (error "unimplemented"))

(defmethod decode ((codec bzip2-codec) in out)
  (decompress out (make-dstate :bzip2) in))

(defclass decompressing-stream (fundamental-binary-input-stream)
  ((wrapped-stream :initarg :stream :reader wrapped-stream)

   (dstate :initarg :dstate :reader dstate)
   (dfun :initarg :dfun :reader dfun)

   (input-buffer :initform (->u8 4096)
                 :reader input-buffer)
   (input-buffer-index :initform 0 :accessor input-buffer-index)
   (input-buffer-n-bytes :initform 0 :accessor input-buffer-n-bytes)

   (output-buffer :initform (->u8 4096)
                  :reader output-buffer)
   (output-buffer-index :initform 0 :accessor output-buffer-index)
   (output-buffer-n-bytes :initform 0 :accessor output-buffer-n-bytes)))

(defun make-decompressing-stream (format stream)
  (multiple-value-bind (state dfun)
      (ecase format
        ((:deflate :zlib :gzip)
         (values (make-inflate-state format) #'%inflate))
        ((:bzip2)
         (values (make-bzip2-state) #'%bzip2-decompress)))
    (make-instance 'decompressing-stream
                   :stream stream
                   :dstate state
                   :dfun dfun)))

(defun output-available-p (stream)
  (/= (output-buffer-index stream) (output-buffer-n-bytes stream)))

(defun input-available-p (stream)
  (/= (input-buffer-index stream) (input-buffer-n-bytes stream)))

(defun refill-stream-input-buffer (stream)
  (with-slots (input-buffer wrapped-stream
                            input-buffer-index input-buffer-n-bytes)
      stream
    (let ((n-bytes-read (read-sequence input-buffer wrapped-stream)))
      (setf input-buffer-index 0 input-buffer-n-bytes n-bytes-read)
      (values))))

(defun refill-stream-output-buffer (stream)
  (unless (input-available-p stream)
    (refill-stream-input-buffer stream))
  (multiple-value-bind (bytes-read bytes-output)
      (funcall (the function (dfun stream))
               (dstate stream)
               (input-buffer stream)
               (output-buffer stream)
                :input-start (input-buffer-index stream)
                :input-end (input-buffer-n-bytes stream))
    (setf (output-buffer-index stream) 0
          (output-buffer-n-bytes stream) bytes-output
          (input-buffer-index stream) (+ (input-buffer-index stream) bytes-read))
    (assert (<= (input-buffer-index stream) (input-buffer-n-bytes stream)))))

;;; methods

(defun read-and-decompress-byte (stream)
  (flet ((maybe-done ()
           (when (output-available-p stream)
             (return-from read-and-decompress-byte
               (aref (output-buffer stream)
                     (prog1 (output-buffer-index stream)
                       (incf (output-buffer-index stream))))))))
    ;; several input buffers may be used up before output is available
    ;; => read-byte should refill "something" while at all possible,
    ;; like read-sequence already does.
    (loop initially (maybe-done)
          do (refill-stream-output-buffer stream)
             (maybe-done)
             (unless (input-available-p stream)
               (refill-stream-input-buffer stream))
             ;; If we didn't refill, then we must be all done.
             (unless (input-available-p stream)
               (finish-dstate (dstate stream))
               (return :eof)))))

(defun copy-existing-output (stream seq start end)
  (declare (type ->u8 seq))
  (let ((amount (min (- end start)
                     (- (output-buffer-n-bytes stream)
                        (output-buffer-index stream)))))
    (replace seq (output-buffer stream)
             :start1 start :end1 end
             :start2 (output-buffer-index stream)
             :end2 (output-buffer-n-bytes stream))
    (incf (output-buffer-index stream) amount)
    (+ start amount)))

(defmethod stream-read-sequence ((stream decompressing-stream) seq &optional (start 0) end)
  (unless (typep seq '->u8)
    (return-from stream-read-sequence (call-next-method)))
  (let ((end (or end (length seq))))
    (loop initially (when (output-available-p stream)
                      (setf start (copy-existing-output stream seq
                                                        start end)))
          while (< start end)
          do (unless (input-available-p stream)
               (refill-stream-input-buffer stream))
             ;; If we didn't refill, then we must be all done.
             (unless (input-available-p stream)
               (finish-dstate (dstate stream))
               (loop-finish))
             ;; Decompress directly into the user-provided buffer.
             (multiple-value-bind (bytes-read bytes-output)
                 (funcall (the function (dfun stream))
                          (dstate stream)
                          (input-buffer stream)
                          seq
                          :input-start (input-buffer-index stream)
                          :input-end (input-buffer-n-bytes stream)
                          :output-start start
                          :output-end end)
               (incf (input-buffer-index stream) bytes-read)
               (incf start bytes-output))
          finally (return start))))

(defmethod stream-read-byte ((stream decompressing-stream))
  (read-and-decompress-byte stream))

(defclass compressing-stream (fundamental-binary-output-stream)
  ((openp
    :initform t
    :accessor openp)
   (compressor
    :initarg :compressor
    :accessor compressor))
  (:documentation
   "A stream that transparently compresses its input and writes the
compressed data to another stream."))

(defun make-compressing-stream (compressor-type stream)
  "Return a COMPRESSING-STREAM that transparently compresses its input and
writes it to STREAM. COMPRESSOR-TYPE is a symbol naming the compressor class to
use.

Closing the returned COMPRESSING-STREAM merely finalizes the compression and
does not close STREAM."
  (make-instance
   'compressing-stream
   :compressor (make-instance
                compressor-type
                :callback (make-stream-output-callback stream))))

(defmethod stream-write-byte ((stream compressing-stream) byte)
  (unless (openp stream)
    (error 'stream-closed :stream stream))
  (compress-octet byte (compressor stream))
  byte)

(defmethod stream-write-sequence ((stream compressing-stream) sequence &optional start end)
  (unless (openp stream)
    (error 'stream-closed :stream stream))
  (let ((vector (if (typep sequence 'vector)
                    sequence
                    (coerce sequence 'vector))))
    (compress-u8-vector vector (compressor stream) :start start :end end))
  sequence)

(defmethod stream-element-type ((stream compressing-stream))
  'u8)

(defmethod close ((stream compressing-stream) &key abort)
  (declare (ignore abort))
  (when (openp stream)
    (finish-compression (compressor stream))
    (setf (openp stream) nil)
    t))

(defun bitstream-callback-missing (&rest args)
  (declare (ignore args))
  (error "No callback set in bitstream"))

(defun merge-bits (code size buffer bits callback)
  (declare (type u32 code)
           (type (integer 0 32) size)
           (type bitstream-buffer-bit-count bits)
           (type bitstream-buffer buffer)
           (type function callback))
  ;; BITS represents how many bits have been added to BUFFER so far,
  ;; so the FLOOR of it by 8 will give both the buffer byte index and
  ;; the bit index within that byte to where new bits should be
  ;; merged
  (let ((buffer-index (ash bits -3))
        (bit (logand #b111 bits)))
    ;; The first byte to which new bits are merged might have some
    ;; bits in it already, so pull it out for merging back in the
    ;; loop. This only has to be done for the first byte, since
    ;; subsequent bytes in the buffer will consist solely of bits from
    ;; CODE.
    ;;
    ;; The check (PLUSP BIT) is done to make sure that no garbage bits
    ;; from a previous write are re-used; if (PLUSP BIT) is zero, all
    ;; bits in the first output byte come from CODE.
    (let ((merge-byte (if (plusp bit) (aref buffer buffer-index) 0))
          (end #.+bitstream-buffer-size+)
          (result (+ bits size)))
      ;; (ceiling (+ bit size) 8) is the total number of bytes touched
      ;; in the buffer
      (dotimes (i (ceiling (+ bit size) 8))
        (let ((shift (+ bit (* i -8)))
              (j (+ buffer-index i)))
          ;; Buffer filled up in the middle of CODE
          (when (= j end)
            (funcall callback buffer j))
          ;; Merge part of CODE into the buffer
          (setf (aref buffer (logand #.+bitstream-buffer-mask+ j))
                (logior (logand #xFF (ash code shift)) merge-byte))
          (setf merge-byte 0)))
      ;; Writing is done, and the buffer is full, so call the callback
      (when (= result #.+bitstream-buffer-bits+)
        (funcall callback buffer #.+bitstream-buffer-size+))
      ;; Return only the low bits of the sum
      (logand #.+bitstream-buffer-bitmask+ result))))

(defun merge-octet (octet buffer bits callback)
  (declare (type u8 octet)
           (type bitstream-buffer buffer)
           (type bitstream-buffer-bit-count bits)
           (type function callback))
  (let ((offset (ceiling bits 8)))
    ;; End of the buffer beforehand
    (when (= offset #.+bitstream-buffer-size+)
      (funcall callback buffer #.+bitstream-buffer-size+)
      (setf offset 0
            bits 0))
    (setf (aref buffer offset) octet
          bits (+ bits 8))
    (when (= (1+ offset) #.+bitstream-buffer-size+)
      (funcall callback buffer #.+bitstream-buffer-size+)
      (setf bits 0))
    bits))

;;; Protocol

(defclass bitstream ()
  ((buffer
    :initarg :buffer
    :accessor buffer
    :documentation "Holds accumulated bits packed into octets.")
   (bits
    :initarg :bits
    :accessor bits
    :documentation "The number of bits written to the buffer so far.")
   (callback
    :initarg :callback
    :accessor callback
    :documentation "A function of two arguments, BUFFER and END,
    that should write out all the data in BUFFER up to END."))
   (:default-initargs
    :buffer (->u8 +bitstream-buffer-size+)
    :bits 0
    :callback #'bitstream-callback-missing))


(defmethod write-bits (code size (bitstream bitstream))
  (setf (bits bitstream)
        (merge-bits code size
                    (buffer bitstream)
                    (bits bitstream)
                    (callback bitstream))))

(defmethod write-u8 ((bitstream bitstream) octet)
  (setf (bits bitstream)
        (merge-octet octet
                     (buffer bitstream)
                     (bits bitstream)
                     (callback bitstream))))

(defmethod write-u8-vector (vector (bitstream bitstream) &key (start 0) end)
  ;;; Not efficient in the slightest, but not actually used internally.
  (let ((end (or end (length vector))))
    (loop for i from start below end
          do (write-u8 bitstream (aref vector i)))))

(defmethod flush ((bitstream bitstream))
  (let ((end (ceiling (bits bitstream) 8)))
    (funcall (callback bitstream) (buffer bitstream) end)
    (setf (bits bitstream) 0)))


(deftype code-vector ()
  '->u32)

(deftype size-vector ()
  '->u8)

(defclass huffman-codes ()
  ((codes
    :initarg :codes
    :accessor codes)
   (sizes
    :initarg :sizes
    :accessor sizes)))
                        
(defun code-vector (length)
  (->u32 length))

(defun size-vector (length)
  (->u8 length))

;;;
;;; Generate the fixed code/size vectors
;;;

(defun reverse-bits (word n)
  (let ((j 0))
    (dotimes (i n j)
      (setf j (logior (ash j 1) (logand #x1 word)))
      (setf word (ash word -1)))))

(defun fixed-huffman-codes ()
  "Generate the fixed Huffman codes specified by RFC1951."
  (let ((codes (code-vector 288))
        (sizes (size-vector 288))
        (i 0))
    (flet ((fill-range (length start end)
             (loop for j from start to end do
                   (setf (aref codes i) (reverse-bits j length)
                         (aref sizes i) length)
                   (incf i))))
      (fill-range 8 #b00110000  #b10111111)
      (fill-range 9 #b110010000 #b111111111)
      (fill-range 7 #b0000000   #b0010111)
      (fill-range 8 #b11000000  #b11000111)
      (make-instance 'huffman-codes :codes codes :sizes sizes))))

(defun length-codes (huffman-codes)
  "Compute a table of the (Huffman + extra bits) values for all
possible lengths for the given HUFFMAN-TABLE."
  (let ((codes (code-vector 259))
        (sizes (size-vector 259))
        (code 257)
        (length 3)
        (extra-bit-counts '(0 0 0 0 0 0 0 0
                            1 1 1 1
                            2 2 2 2
                            3 3 3 3
                            4 4 4 4
                            5 5 5 5
                            0)))
    (labels ((save-pair (i code size)
               (setf (aref codes i) code
                     (aref sizes i) size))
             (save-value (extra-bit-count extra-value)
               (let ((huffman-value (aref (codes huffman-codes) code))
                     (huffman-count (aref (sizes huffman-codes) code)))
                 (save-pair length 
                            (logior huffman-value
                                    (ash extra-value huffman-count))
                            (+ huffman-count extra-bit-count)))))
      (dolist (count extra-bit-counts)
        (dotimes (i (expt 2 count))
          (when (< length 258)
            (save-value count i)
            (incf length)))
        (incf code))
      (setf code 285)
      (save-value 0 0))
    (make-instance 'huffman-codes :codes codes :sizes sizes)))

(defun distance-codes ()
  "Compute a table of the (code + extra bits) values for all possible
distances as specified by RFC1951."
  (let ((codes (code-vector 32769))
        (sizes (size-vector 32769))
        (code 0)
        (distance 1)
        (extra-bit-counts '(0 0 0 0
                            1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9
                            10 10 11 11 12 12 13 13)))
    (flet ((save-value (extra-bit-count extra-value)
             (setf (aref codes distance)
                   (logior (ash extra-value 5) (reverse-bits code 5))
                   (aref sizes distance)
                   (+ 5 extra-bit-count))))
      (dolist (count extra-bit-counts)
        (dotimes (i (expt 2 count))
          (save-value count i)
          (incf distance))
        (incf code)))
    (make-instance 'huffman-codes :codes codes :sizes sizes)))

(defvar *fixed-huffman-codes* (fixed-huffman-codes))
(defvar *length-codes* (length-codes *fixed-huffman-codes*))
(defvar *distance-codes* (distance-codes))


(defconstant +maximum-match-length+ 258
  "The maximum match length allowed.")

(defconstant +maximum-match-distance+ 32768
  "The maximum distance for a match.")

(declaim (inline match-length))
(defun match-length (p1 p2 input end)
  "Returns the length of the match between positions p1 and p2 in
INPUT; END is a sentinel position that ends the match length
check if reached."
  (declare (type input-index p1 p2 end)
           (type input-buffer input))
  (let ((length 0))
    (loop
     (when (or (/= (aref input p1) (aref input p2))
               (= length +maximum-match-length+)
               (= p1 end))
       (return length))
     (setf p1 (logand (1+ p1) #xFFFF)
           p2 (logand (1+ p2) #xFFFF)
           length (logand #xFFF (1+ length))))))

(defun longest-match (p1 input chains end max-tests)
  (declare (type input-index p1 end)
           (type input-buffer input)
           (type chains-buffer chains)
           (type (integer 0 32) max-tests))
  (let ((match-length 0)
        (p2 (aref chains p1))
        (test-count 0)
        (distance 0))
    (declare (type (integer 0 258) match-length)
             (type (integer 0 32) test-count))
    (loop
     (when (or (= match-length +maximum-match-length+)
               (= test-count max-tests)
               (= p2 p1)
               (= p2 (aref chains p2)))
       (return (values match-length distance)))
     (let ((step (logand (- p1 p2) #xFFFF)))
       (when (< +maximum-match-distance+ step)
         (return (values match-length distance)))
       (let ((possible-length (match-length p1 p2 input end)))
         (when (and (< 2 possible-length)
                    (< match-length possible-length))
           (setf distance step
                 match-length possible-length))
         (setf p2 (aref chains p2)))
       (incf test-count)))))

(defun compress (input chains start end
                 literal-fun length-fun distance-fun)
  (declare (type input-buffer input)
           (type chains-buffer chains)
           (type input-index start end)
           (type function literal-fun length-fun distance-fun))
  (let ((p start))
    (loop
     (when (= p end)
       (return))
     (multiple-value-bind (length distance)
         (longest-match p input chains end 4)
       (declare (type (integer 0 258) length)
                (type (integer 0 32768) distance))
       (cond ((zerop length)
              (funcall literal-fun (aref input p))
              (setf p (logand (+ p 1) #xFFFF)))
             (t
              (funcall length-fun length)
              (funcall distance-fun distance)
              (setf p (logand (+ p length) #xFFFF))))))))

(defun make-input ()
  (make-array 65536 :element-type 'u8))

(defun make-chains ()
  (make-array 65536
              :element-type 'u16
              :initial-element 0))

(defun make-hashes ()
  (make-array +hashes-size+
              :element-type 'u16
              :initial-element 0))

(defun error-missing-callback (&rest args)
  (declare (ignore args))
  (error "No callback given for compression"))

;;; FIXME: MERGE-INPUT is pretty ugly. It's the product of incremental
;;; evolution and experimentation. It should be cleaned up.
;;;
;;; Its basic purpose is to use octets from INPUT to fill up 32k-octet
;;; halves of the 64k-octet OUTPUT buffer. Whenever a half fills up,
;;; the COMPRESS-FUN is invoked to compress that half. At the end, a
;;; partial half may remain uncompressed to be either filled by a
;;; future call to MERGE-INPUT or to get flushed out by a call to
;;; FINAL-COMPRESS.

(defun merge-input (input start count output offset compress-fun)
  "Merge COUNT octets from START of INPUT into OUTPUT at OFFSET;
on reaching 32k boundaries within OUTPUT, call the COMPRESS-FUN
with OUTPUT, a starting offset, and the count of pending data."
  (declare (type ->u8 input output))
  (let ((i start)
        (j (+ start (min count (- +input-limit+ (mod offset +input-limit+)))))
        (result (logand +buffer-size-mask+ (+ offset count))))
    (dotimes (k (ceiling (+ (logand offset +input-limit-mask+) count)
                         +input-limit+))
      (when (plusp k)
        (funcall compress-fun
                 output
                 (logxor offset #x8000)
                 +input-limit+))
      (replace output input :start1 offset :start2 i :end2 j)
      (setf offset (logand +input-limit+ (+ offset +input-limit+)))
      (setf i j
            j (min (+ start count) (+ j +input-limit+))))
    (when (zerop (logand result +input-limit-mask+))
      (funcall compress-fun output (logxor offset #x8000) +input-limit+))
    result))

(defun make-huffman-writer (huffman-codes bitstream)
  (let ((codes (codes huffman-codes))
        (sizes (sizes huffman-codes))
        (buffer (buffer bitstream))
        (callback (callback bitstream)))
    (lambda (value)
      (setf (bits bitstream)
            (merge-bits (aref codes value)
                        (aref sizes value)
                        buffer
                        (bits bitstream)
                        callback)))))

(defun reinitialize-bitstream-funs (compressor bitstream)
  (setf (literal-fun compressor)
        (make-huffman-writer *fixed-huffman-codes* bitstream)
        (length-fun compressor)
        (make-huffman-writer *length-codes* bitstream)
        (distance-fun compressor)
        (make-huffman-writer *distance-codes* bitstream)
        (compress-fun compressor)
        (make-compress-fun compressor)))


;;; Class & protocol

(defclass deflate-compressor ()
  ((input
    :initarg :input
    :accessor input)
   (chains
    :initarg :chains
    :accessor chains)
   (hashes
    :initarg :hashes
    :accessor hashes)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (counter
    :initarg :counter
    :accessor counter)
   (octet-buffer
    :initarg :octet-buffer
    :accessor octet-buffer)
   (bitstream
    :initarg :bitstream
    :accessor bitstream)
   (literal-fun
    :initarg :literal-fun
    :accessor literal-fun)
   (length-fun
    :initarg :length-fun
    :accessor length-fun)
   (distance-fun
    :initarg :distance-fun
    :accessor distance-fun)
   (byte-fun
    :initarg :byte-fun
    :accessor byte-fun)
   (compress-fun
    :initarg :compress-fun
    :accessor compress-fun))
  (:default-initargs
   :input (make-input)
   :chains (make-chains)
   :hashes (make-hashes)
   :start 0
   :end 0
   :counter 0
   :bitstream (make-instance 'bitstream)
   :octet-buffer (->u8 1)))

;;; Methods

(defmethod initialize-instance :after ((compressor deflate-compressor)
                                       &rest initargs
                                       &key
                                       literal-fun length-fun distance-fun
                                       compress-fun
                                       callback)
  (declare (ignore initargs))
  (let ((bitstream (bitstream compressor)))
    (setf (callback bitstream)
          (or callback #'error-missing-callback))
    (setf (literal-fun compressor)
          (or literal-fun (make-huffman-writer *fixed-huffman-codes*
                                               bitstream)))
    (setf (length-fun compressor)
          (or length-fun (make-huffman-writer *length-codes*
                                              bitstream)))
    (setf (distance-fun compressor)
          (or distance-fun (make-huffman-writer *distance-codes*
                                                bitstream)))
    (setf (compress-fun compressor)
          (or compress-fun (make-compress-fun compressor)))
    (start-data-format compressor)))

;;; A few methods delegate to the bitstream

(defmethod (setf callback) (new-fun (compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (prog1
        (setf (callback bitstream) new-fun)
      (reinitialize-bitstream-funs compressor bitstream))))

(defmethod write-bits (code size (compressor deflate-compressor))
  (write-bits code size (bitstream compressor)))

(defmethod write-u8 ((compressor deflate-compressor) octet)
  (write-u8 (bitstream compressor) octet))

(defmethod write-u8-vector (vector (compressor deflate-compressor)
                            &key (start 0) end)
  (write-u8-vector vector (bitstream compressor)
                   :start start
                   :end end))

(defmethod start-data-format ((compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (write-bits +final-block+ 1 bitstream)
    (write-bits +fixed-tables+ 2 bitstream)))

(defmethod compress-octet (octet compressor)
  (let ((vector (octet-buffer compressor)))
    (setf (aref vector 0) octet)
    (compress-u8-vector vector compressor)))

(defmethod compress-u8-vector (vector compressor &key (start 0) end)
  (let* ((closure (compress-fun compressor))
         (end (or end (length vector)))
         (count (- end start)))
    (let ((end
           (merge-input vector start count
                        (input compressor)
                        (end compressor)
                        closure)))
      (setf (end compressor) end
            (start compressor) (logand #x8000 end)
            (counter compressor) (logand #x7FFF end)))))

(defun hash-value (input position)
  (+ (* #.+rmax+ (aref input position))
     (* #.+radix+ (aref input (logand #.+input-mask+ (+ position 1))))
     (aref input (logand #.+input-mask+ (+ position 2)))))

(declaim (inline mod8191))
(defun mod8191 (z)
  (declare (type (integer 0 3057705) z))
  (let ((zz (+ (ash z -13) (logand #x1FFF z))))
    (if (< zz #x1FFF)
        zz
        (- zz #x1FFF))))

(defun update-chains (input hashes chains start count)
  (declare (type input-buffer input)
           (type hashes-buffer hashes)
           (type chains-buffer chains)
           (type input-index start)
           (type (integer 0 32768) count))
  (when (< count 3)
    (return-from update-chains))
  (let* ((hash (hash-value input start))
         (p0 start)
         (p1 (logand (+ start 2) #xFFFF)))
    (declare (type (integer 0 3057705) hash))
    (loop
     (let ((hash-index (mod8191 hash)))
       ;; Stuff the old hash index into chains at p0
       (setf (aref chains p0) (aref hashes hash-index))
       ;; Stuff p0 into the hashes
       (setf (aref hashes hash-index) p0)
       ;; Tentatively advance; if we hit the end, don't do the rest of
       ;; the hash update
       (setf p1 (logand (1+ p1) #xFFFF))
       (decf count)
       (when (= count 2)
         (return))
       ;; We're not at the end, so lop off the high, shift left, and
       ;; add the low to form a new hash value
       (setf hash (- hash (* (aref input p0) 11881)))
       (setf hash (* hash 109))
       (setf p0 (logand (1+ p0) #xFFFF))
       (setf hash (+ hash (aref input p1)))))))

(defmethod process-input ((compressor deflate-compressor) input start count)
  (update-chains input (hashes compressor) (chains compressor) start count))

(defmethod finish-data-format ((compressor deflate-compressor))
  (funcall (literal-fun compressor) 256))

(defmethod finish-compression ((compressor deflate-compressor))
  (final-compress compressor)
  (finish-data-format compressor)
  (flush (bitstream compressor)))

(defmethod final-compress ((compressor deflate-compressor))
  (let ((input (input compressor))
        (chains (chains compressor))
        (start (start compressor))
        (end (end compressor))
        (counter (counter compressor))
        (literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (process-input compressor input start counter)
    (compress input chains start end
              literal-fun
              length-fun
              distance-fun)))

(defmethod make-compress-fun ((compressor deflate-compressor))
  (let ((literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (lambda (input start count)
      (process-input compressor input start count)
      (let ((end (+ start count)))
        (compress input (chains compressor) start (logand #xFFFF end)
                  literal-fun
                  length-fun
                  distance-fun)))))

(defmacro with-compressor ((var class
                                &rest initargs
                                &key &allow-other-keys)
                           &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
     (multiple-value-prog1 
         (progn ,@body)
       (finish-compression ,var))))


(defun make-stream-output-callback (stream)
  "Return a function suitable for use as a compressor callback that
writes all compressed data to STREAM."
  (lambda (buffer end)
    (write-sequence buffer stream :end end)))

(defun compress-stream (compressor-name in out)
  (let ((callback (make-stream-output-callback out))
        (buffer (->u8 8192)))
    (with-compressor (compressor compressor-name
                                 :callback callback)
      (loop :for bytes-read := (read-sequence buffer in)
            :if (zerop bytes-read)
              :return
            :else
            :do (compress-u8-vector buffer compressor :end bytes-read)))))

(defun codec (format)
  (ecase format
    (:bzip2 (make-instance 'bzip2-codec))
    (:deflate (make-instance 'deflate-codec))
    (:zlib (make-instance 'zlib-codec))
    (:gzip (make-instance 'gzip-codec))))
