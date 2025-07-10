;;;;; DEFLATE/Inflate Decompression Module
;;;;; 
;;;;; This module implements RFC 1951 DEFLATE decompression with huffman decoding
;;;;; and sliding window support. Extracted from epsilon.lib.codec for modularity.

(defpackage :epsilon.lib.inflate
  (:use
   :cl
   :sb-gray
   :epsilon.lib.syntax
   :epsilon.lib.function
   :epsilon.lib.type
   :epsilon.lib.checksum.adler-32
   :epsilon.lib.checksum.crc-32
   :epsilon.lib.checksum.generic)
  (:export
   ;; Main decompression functions
   :make-inflate-state
   :finish-inflate-state
   :inflate-decompress
   :make-inflating-stream
   
   ;; Huffman decoding
   :huffman-decode-table
   :make-huffman-decode-table
   :construct-huffman-decode-table
   
   ;; State and structures
   :inflate-state
   :inflate-state-p
   :sliding-window
   
   ;; Constants
   :+block-no-compress+
   :+block-fixed-codes+
   :+block-dynamic-codes+
   :+block-invalid+
   :+max-code-length+
   :+max-codes+
   :+max-n-code-lengths+
   :+deflate-max-bits+
   
   ;; Errors
   :invalid-deflate-block
   :invalid-huffman-code
   :deflate-error
   :reserved-block-type-error
   :invalid-stored-block-length-error
   :code-lengths-start-with-repetition-error
   :code-lengths-bounds-error
   :illegal-length-code-error
   :illegal-distance-code-error
   :unassigned-huffman-code-error
   :invalid-checksum-error
   :invalid-gzip-header-error
   :invalid-zlib-header-error))

(in-package :epsilon.lib.inflate)

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

(define-constant +final-block+ #b1)
(define-constant +fixed-tables+ #b01)

;;;; GZIP/ZLIB constants

(defconstant +gzip-flag-extra+ 2)
(defconstant +gzip-flag-name+ 3)
(defconstant +gzip-flag-comment+ 4)
(defconstant +gzip-flag-crc+ 1)
(defconstant +zlib-flag-fdict+ 5)

;;;; Type definitions

(deftype deflate-code-length () '(integer 0 #.+max-code-length+))
(deftype deflate-code () '(unsigned-byte #.+max-code-length+))
(deftype deflate-code-value () '(integer 0 (#.+max-codes+)))
(deftype sliding-window () '(simple-array u8 (32768)))

;;;; Distance and length code tables

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

;;;; Huffman decoding structures

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
  (counts #1=(error "required parameter")
          :type (simple-array u16 (#.+max-code-length+))
          :read-only t)
  (offsets #1# :type (simple-array u16 (#.(1+ +max-code-length+)))
           :read-only t)
  (symbols nil :read-only t :type (simple-array fixnum (*)))
  (bits nil :read-only t))
) ; EVAL-WHEN

;;;; Huffman decode table construction

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

;;;; Fixed block tables

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

;;;; Error conditions

(define-condition deflate-error (simple-error) ())

(define-condition invalid-deflate-block (deflate-error)
  ((block-type :initarg :block-type :reader invalid-block-type))
  (:report (lambda (condition stream)
             (format stream "Invalid deflate block type: ~D"
                     (invalid-block-type condition)))))

(define-condition invalid-huffman-code (deflate-error)
  ((code :initarg :code :reader invalid-code))
  (:report (lambda (condition stream)
             (format stream "Invalid huffman code: ~D"
                     (invalid-code condition)))))

(define-condition reserved-block-type-error (deflate-error)
  () (:report "Reserved block type encountered"))

(define-condition invalid-stored-block-length-error (deflate-error)
  () (:report "Invalid stored block length"))

(define-condition code-lengths-start-with-repetition-error (deflate-error)
  () (:report "Code lengths start with repetition"))

(define-condition code-lengths-bounds-error (deflate-error)
  () (:report "Code lengths bounds error"))

(define-condition illegal-length-code-error (deflate-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal length code: ~D" (illegal-code condition)))))

(define-condition illegal-distance-code-error (deflate-error)
  ((code :initarg :code :reader illegal-code))
  (:report (lambda (condition stream)
             (format stream "Illegal distance code: ~D" (illegal-code condition)))))

(define-condition unassigned-huffman-code-error (deflate-error)
  () (:report "Unassigned huffman code"))

(define-condition invalid-checksum-error (deflate-error)
  ((stored :initarg :stored :reader stored-checksum)
   (computed :initarg :computed :reader computed-checksum)
   (kind :initarg :kind :reader checksum-kind))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A checksum: stored ~X, computed ~X"
                     (checksum-kind condition)
                     (stored-checksum condition)
                     (computed-checksum condition)))))

(define-condition invalid-gzip-header-error (deflate-error)
  () (:report "Invalid GZIP header"))

(define-condition invalid-zlib-header-error (deflate-error)
  () (:report "Invalid ZLIB header"))

;;;; Headers for gzip/zlib

(defclass gzip-header ()
  ((compression-method :initarg :compression-method :accessor compression-method)
   (flags :initarg :flags :accessor flags :initform 0)
   (mtime :initarg :mtime :accessor mtime :initform 0)
   (extra-flags :initarg :extra-flags :accessor extra-flags :initform 0)
   (os :initarg :os :accessor os :initform 0)
   (filename :initarg :filename :accessor filename :initform nil)
   (comment :initarg :comment :accessor comment :initform nil)))

(defclass zlib-header ()
  ((cmf :initarg :cmf :accessor cmf)
   (flags :initarg :flags :accessor flags :initform 0)
   (fdict :initarg :fdict :accessor fdict :initform nil)))

;;;; Base decompression state

(defstruct decompression-state
  (input nil)
  (input-start 0)
  (input-index 0)
  (input-end 0)
  (output nil)
  (output-start 0)
  (output-index 0)
  (output-end 0)
  (bits 0 :type u32)
  (n-bits 0 :type (integer 0 32))
  (state nil)
  (checksum nil)
  (done nil))

;;;; Inflate state structure

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

;;;; Constructor and destructor

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
               (error "Invalid format ~S" format))))
          (state (%make-inflate-state f)))
    (ecase f
      (gzip
       (setf (decompression-state-checksum state) (make-instance 'crc-32)))
      ((zlib deflate)
       (setf (decompression-state-checksum state) (make-instance 'adler-32))))
    state))

(defun finish-inflate-state (state)
  (unless (decompression-state-done state)
    (error "Stream not completely processed"))
  t)

(defmethod print-object ((object inflate-state) stream)
  (print-unreadable-object (object stream)
    (format stream "Inflate-State input ~D/~D; output ~D/~D"
            (- (decompression-state-input-index object)
               (decompression-state-input-start object))
            (- (decompression-state-input-end object)
               (decompression-state-input-index object))
            (- (decompression-state-output-index object)
               (decompression-state-output-start object))
            (- (decompression-state-output-end object)
               (decompression-state-output-index object)))))

;;;; Window management

(defun update-window (state)
  (declare (type inflate-state state))
  (let* ((output (decompression-state-output state))
         (start (decompression-state-output-start state))
         (index (decompression-state-output-index state))
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

;;;; Main decompression function

(defun inflate-decompress (state input output &key (input-start 0) input-end
                (output-start 0) output-end)
  "Decompresses data in INPUT between INPUT-START and INPUT-END
and places the result in OUTPUT between OUTPUT-START and
OUTPUT-END.  -START and -END arguments follow the convention of
the sequence functions.  Returns the number of bytes pulled from
the input and the number of bytes written to the output."
  (declare (type inflate-state state))
  (let* ((input-end (or input-end (length input)))
         (output-end (or output-end (length output))))
    (setf (decompression-state-input state) input
          (decompression-state-input-start state) input-start
          (decompression-state-input-index state) input-start
          (decompression-state-input-end state) input-end
          (decompression-state-output state) output
          (decompression-state-output-start state) output-start
          (decompression-state-output-index state) output-start
          (decompression-state-output-end state) output-end)
    (catch 'inflate-done
      (%inflate-state-machine state))
    (update-window state)

    (update (decompression-state-checksum state)
            output
            output-start
            (- (decompression-state-output-index state) output-start))
      
    (values (- (decompression-state-input-index state) input-start)
            (- (decompression-state-output-index state) output-start))))

(defun record-code-length (state value)
  (setf (aref (inflate-state-code-lengths state)
              (aref *code-length-code-order*
                    (inflate-state-n-values-read state))) value)
  (incf (inflate-state-n-values-read state)))

;;;; Main inflate state machine

(defun %inflate-state-machine (state)
  (declare (type inflate-state state))
  (macrolet ((transition-to (next-state)
               `(progn
                  (setf (decompression-state-state state) #',next-state)
                  #+(or sbcl cmu)
                  (,next-state state))))
    (labels ((read-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type inflate-state state))
               (prog1 (ldb (byte n 0) (decompression-state-bits state))
                 (setf (decompression-state-bits state)
                       (ash (decompression-state-bits state) (- n)))
                 (decf (decompression-state-n-bits state) n)))

             (ensure-bits (n state)
               (declare (type (integer 0 32) n))
               (declare (type inflate-state state))
               (let ((bits (decompression-state-bits state))
                     (n-bits (decompression-state-n-bits state))
                     (input-index (decompression-state-input-index state)))
                 (declare (type u32 bits))
                 (loop while (< n-bits n)
                       when (>= input-index (decompression-state-input-end state))
                         do (progn
                              (setf (decompression-state-bits state) bits
                                    (decompression-state-n-bits state) n-bits
                                    (decompression-state-input-index state) input-index)
                              (throw 'inflate-done nil))
                       do (let ((byte (aref (decompression-state-input state) input-index)))
                            (declare (type u8 byte))
                            (setf bits
                                  (logand #xffffffff (logior (ash byte n-bits) bits)))
                            (incf n-bits 8)
                            (incf input-index))
                       finally (setf (decompression-state-bits state) bits
                                     (decompression-state-n-bits state) n-bits
                                     (decompression-state-input-index state) input-index))))

             (ensure-and-read-bits (n state)
               (ensure-bits n state)
               (read-bits n state))

             (align-bits-bytewise (state)
               (declare (type inflate-state state))
               (let ((n-bits (decompression-state-n-bits state)))
                 (decf (decompression-state-n-bits state) (rem n-bits 8))
                 (setf (decompression-state-bits state)
                       (ash (decompression-state-bits state)
                            (- (rem n-bits 8))))
                 (values)))

             (decode-value (table state)
               (declare (type huffman-decode-table table))
               (declare (type inflate-state state))
               (ensure-bits (hdt-bits table) state)
               (let ((bits (decompression-state-bits state)))
                 (declare (type u32 bits))
                 (do ((counts (hdt-counts table))
                      (len 1 (1+ len))
                      (first 0 (sb-ext:truly-the fixnum (ash first 1)))
                      (code 0 (sb-ext:truly-the fixnum (ash code 1))))
                     ((>= len +max-code-length+)
                      (error 'unassigned-huffman-code-error))
                   (declare (type (and fixnum (integer 0 *)) first code))
                   (setf code (logior code (logand bits 1))
                         bits (ash bits -1))
                   (let ((count (aref counts len)))
                     (when (< (- code count) first)
                       (setf (decompression-state-bits state) bits)
                       (decf (decompression-state-n-bits state) len)
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
                  (setf (decompression-state-state state)
                        (ecase (inflate-state-data-format state)
                          (deflate
                              (setf (decompression-state-done state) t)
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
                   (cerror "Use the invalid stored block length."
                           'invalid-stored-block-length-error))
                 (transition-to copy-bytes)))

             (copy-bytes (state)
               (declare (type inflate-state state))
               (if (zerop (inflate-state-length state))
                   (setf (decompression-state-state state) #'block-type)
                   (let ((n-copied-bytes (min (inflate-state-length state)
                                              (- (decompression-state-input-end state)
                                                 (decompression-state-input-index state))
                                              (- (decompression-state-output-end state)
                                                 (decompression-state-output-index state)))))
                     (cond
                       ((zerop n-copied-bytes) (throw 'inflate-done nil))
                       (t
                        (replace (decompression-state-output state)
                                 (decompression-state-input state)
                                 :start1 (decompression-state-output-index state)
                                 :end1 (+ (decompression-state-output-index state)
                                          n-copied-bytes)
                                 :start2 (decompression-state-input-index state)
                                 :end2 (+ (decompression-state-input-index state)
                                          n-copied-bytes))
                        (incf (decompression-state-input-index state) n-copied-bytes)
                        (incf (decompression-state-output-index state) n-copied-bytes)
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
                   (t ; (= value 256)
                    (transition-to block-type)))))

             (literal (state)
               (declare (type inflate-state state))
               (cond
                 ((= (decompression-state-output-index state)
                     (decompression-state-output-end state)) (throw 'inflate-done nil))
                 (t (setf (aref (decompression-state-output state)
                                (decompression-state-output-index state))
                          (inflate-state-length state))
                  (incf (decompression-state-output-index state))
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
                      (start (decompression-state-output-start state))
                      (index (decompression-state-output-index state))
                      (end (decompression-state-output-end state))
                      (window-index (inflate-state-window-index state))
                      (n-bytes-to-copy (min length (- end index))))
                 (when (= index end)
                   (throw 'inflate-done nil))
                 (flet ((frob-by-copying-from (copy-source copy-index n-bytes-to-copy)
                          (declare (type (simple-array u8 (*)) copy-source))
                          (decf (inflate-state-length state) n-bytes-to-copy)
                          (incf (decompression-state-output-index state) n-bytes-to-copy)
                          (loop with output = (decompression-state-output state)
                                for i from index below (the fixnum (+ index n-bytes-to-copy))
                                for j from copy-index below (the fixnum (+ copy-index n-bytes-to-copy))
                                do (setf (aref output i) (aref copy-source j)))))
                   (cond
                     ((<= distance (- index start))
                      ;; we are within the output we have produced
                      (frob-by-copying-from (decompression-state-output state)
                                            (- index distance)
                                            n-bytes-to-copy))
                     (t
                      (let ((copy-index (+ (- window-index distance) (- index start))))
                        (cond
                          ((not (minusp copy-index))
                           ;; we are within the non-wraparound portion of the window
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

             ;; GZIP header processing functions would go here...
             ;; For now, stub them out

             (gzip-crc32 (state)
               (declare (ignore state))
               (error "GZIP format not fully implemented yet"))

             (check-zlib-adler32 (state)
               (declare (ignore state))
               (error "ZLIB format not fully implemented yet"))
             )
      (unless (decompression-state-state state)
        (setf (decompression-state-state state)
              (ecase (inflate-state-data-format state)
                (deflate #'block-type)
                (zlib #'check-zlib-adler32)
                (gzip #'gzip-crc32))))
      (loop (funcall (decompression-state-state state) state)))))

;;;; Stream interface

(defclass inflating-stream (fundamental-binary-input-stream)
  ((source :initarg :source :reader inflating-stream-source)
   (state :initarg :state :reader inflating-stream-state)
   (buffer :initform (make-array 8192 :element-type 'u8) :reader inflating-stream-buffer)
   (buffer-pos :initform 0 :accessor inflating-stream-buffer-pos)
   (buffer-end :initform 0 :accessor inflating-stream-buffer-end)))

(defun make-inflating-stream (format source)
  "Create a new inflating stream that decompresses data from SOURCE."
  (make-instance 'inflating-stream
                 :source source
                 :state (make-inflate-state format)))

(defmethod stream-read-byte ((stream inflating-stream))
  "Read a single byte from the inflating stream."
  (with-slots (buffer buffer-pos buffer-end) stream
    (when (>= buffer-pos buffer-end)
      ;; Need to decompress more data
      (error "Stream decompression not fully implemented yet"))
    (prog1 (aref buffer buffer-pos)
      (incf buffer-pos))))

(defmethod stream-element-type ((stream inflating-stream))
  '(unsigned-byte 8))

;;;; Convenience functions

(defun make-decompressing-stream (format source)
  "Create a decompressing stream for the given format."
  (when (member format '(:deflate :gzip :zlib))
    (make-inflating-stream format source)))

(defun make-huffman-decode-table (counts offsets symbols bits)
  "Create a huffman decode table from the given arrays."
  (make-hdt counts offsets symbols bits))

;; Alias for backward compatibility
(defun %inflate (state input output &rest args)
  "Backward compatibility alias for inflate-decompress."
  (apply #'inflate-decompress state input output args))