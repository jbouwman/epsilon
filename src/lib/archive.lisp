(defpackage :epsilon.lib.archive
  (:use
   :cl
   :epsilon.lib.type)
  (:local-nicknames
   (:char :epsilon.lib.char)
   (:codec :epsilon.lib.codec)
   (:crc-32 :epsilon.lib.checksum.crc-32)
   (:stream :epsilon.lib.stream)
   (:struct :epsilon.lib.struct))
  (:export
   :with-zip-file
   :entries
   :attributes
   :decode-entry))

(in-package #:epsilon.lib.archive)

(defvar *default-version-made*
  '(4 5))

(defvar *default-version-needed*
  '(2 0))

(defun version< (version1 version2)
  (destructuring-bind (major1 minor1) version1
    (destructuring-bind (major2 minor2) version2
      (or (< major1 major2)
          (and (= major1 major2) (< minor1 minor2))))))

(defvar *compatibility*
  #+windows :ntfs
  #+darwin :darwin
  #+(and unix (not darwin)) :unix)

(defvar *default-buffer-size* 4096)

(defun default-attributes-for (system)
  (case system
    ((:darwin :unix) #o644)
    (T 0)))

(defun ensure-buffer (buffer)
  (etypecase buffer
    (vector buffer)
    (integer (make-array buffer :element-type 'u8))
    (null (make-array *default-buffer-size* :element-type 'u8))))

(defun ensure-password (password)
  (etypecase password
    (string (char:string-to-bytes password :encoding :utf-8))
    ((vector u8) password)
    (null (restart-case (error 'password-required)
            (use-value (password)
              (ensure-password password))))))

(defun alist-vector (alist)
  (let* ((max (loop for cons in alist maximize (car cons)))
         (vec (make-array (1+ max) :initial-element :unknown)))
    (loop for (i . e) in alist
          do (setf (svref vec i) e))
    vec))

(defun alist-table (alist)
  (let ((table (make-hash-table)))
    (loop for (i . e) in alist
          do (setf (gethash i table) e))
    table))

(defun n-bit-p (bits &rest integers)
  (let ((max (1- (ash 1 bits))))
    (every (lambda (integer) (< integer max)) integers)))

(defun cap (value bits)
  (let ((max (1- (ash 1 bits))))
    (if (< max value)
        max
        value)))

(defun bitfield (&rest bits)
  (let ((int 0))
    (loop for i from 0
          for bit in bits
          do (when bit (setf (ldb (byte 1 i) int) 1)))
    int))

(defun enlist (thing &rest values)
  (if (listp thing) thing (list* thing values)))

(defun decode-string (octets flags)
  (char:bytes-to-string octets :encoding (if (logbitp 11 flags) :utf-8 :cp437)))

(defun encode-string (string)
  (if string
      (char:string-to-bytes string :encoding :utf-8)
      #()))

;;; TODO
;;;
;;; msdos timestamp is really a kind of thing. at core, a byte range
;;; may express one. there should be a simple, inlinable function that
;;; accomplishes the decoding. outside of that, a generaliation of how
;;; to decode things. there is some kind of monomorphizing, optimizing
;;; compiler in here somewhere.

;;; TODO
;;;
;;; The ldb stuff looks messy. it looks like there's a declarative,
;;; packed, c-like struct in there somewhere.

(defun decode-msdos-timestamp (date time)
  (let ((yy (ldb (byte 7 9) date))
        (mm (ldb (byte 4 5) date))
        (dd (ldb (byte 5 0) date))
        (h (ldb (byte 5 11) time))
        (m (ldb (byte 6 5) time))
        (s (ldb (byte 5 0) time)))
    (flet ((clamp (l x h)
             (min h (max l x))))
      (encode-universal-time (clamp 0 (1+ (* 2 s)) 59) (clamp 0 (1- m) 59) (clamp 0 (1- h) 23) (clamp 1 dd 31) (clamp 1 mm 12) (+ 1980 yy) NIL))))

(defun encode-msdos-timestamp (timestamp)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time timestamp NIL)
    (let ((date 0)
          (time 0))
      (setf (ldb (byte 7 9) date) (- yy 1980))
      (setf (ldb (byte 4 5) date) mm)
      (setf (ldb (byte 5 0) date) dd)
      (setf (ldb (byte 5 11) time) (1+ h))
      (setf (ldb (byte 6 5) time) (1+ m))
      (setf (ldb (byte 5 0) time) (floor s 2))
      (values date time))))

(defun decode-version (version)
  (multiple-value-bind (major minor) (floor (ldb (byte 8 0) version) 10)
    (list major minor)))

(defun encode-version (version &optional compatibility)
  (check-type version (cons (integer 0 9) (cons (integer 0 9) null)))
  (let ((idx (etypecase compatibility
               (null 0)
               (integer compatibility)
               (keyword (file-attribute-id compatibility))))
        (int (+ (* 10 (first version)) (second version))))
    (setf (ldb (byte 8 8) int) idx)
    int))

(defun decode-file-attribute (compat attr)
  (let ((compat (file-attribute-name compat))
        (msdos (ldb (byte 8 0) attr))
        (os-specific (ldb (byte 16 16) attr)))
    (list #+fixme (epsilon.sys.fs:decode-attributes msdos :windows) compat os-specific)))

(defun encode-file-attribute (thing)
  (destructuring-bind (msdos compat os-specific) thing
    (declare (ignore compat))
    (let ((i 0))
      (setf (ldb (byte 8 0) i) (logand #xFF (epsilon.sys.fs:encode-attributes msdos :windows)))
      (setf (ldb (byte 16 16) i) (logand #xFFFF os-specific))
      i)))

;; TODO review and remove

(deftype io ()
  `(or stream vector-input directory-input))

(defstruct (vector-input (:constructor make-vector-input (vector index start end)))
  (vector NIL :type (simple-array u8 (*)) :read-only T)
  (start 0 :type fixnum :read-only T)
  (end 0 :type fixnum :read-only T)
  (index 0 :type fixnum))

(defstruct directory-input)

(defun seek (io target)
  (etypecase io
    (vector-input
     (if (<= (vector-input-start io) target (1- (vector-input-end io)))
         (setf (vector-input-index io) target)
         (error 'out-of-bounds-seek :target target)))
    (stream
     (file-position io target))))

(defun has-more (io)
  (etypecase io
    (vector-input
     (< (vector-input-index io) (vector-input-end io)))
    (stream
     (< (file-position io) (file-length io)))))

(defun index (io)
  (etypecase io
    (vector-input
     (vector-input-index io))
    (stream
     (file-position io))))

(defun start (io)
  (etypecase io
    (vector-input
     (vector-input-start io))
    (stream
     0)))

(defun end (io)
  (etypecase io
    (vector-input
     (vector-input-end io))
    (stream
     (file-length io))))

(defmethod size ((io vector-input))
  (- (vector-input-end io) (vector-input-start io)))

(defmethod size ((io stream))
  (file-length io))

(defun u32 (io)
  (etypecase io
    (vector-input
     (prog1 (u32ref/le (vector-input-vector io) (vector-input-index io))
       (incf (vector-input-index io) 4)))
    (stream
     (read-u32/le io))))

(defun output (io array start end)
  (etypecase io
    (vector-input
     (when (<= (vector-input-end io) (+ (vector-input-index io) (- end start)))
       (error 'out-of-bounds-seek :target (+ (vector-input-index io) (- end start))))
     (loop with vector = (vector-input-vector io)
           for i from start below end
           for j from (vector-input-index io)
           do (setf (aref vector j) (aref array i)))
     (incf (vector-input-index io) (- end start)))
    (stream
     (write-sequence array io :start start :end end))))

(defun parse-structure* (io)
  (etypecase io
    (vector-input
     (multiple-value-bind (value index)
         (struct:decode-structure (vector-input-vector io) (vector-input-index io))
       (setf (vector-input-index io) index)
       value))
    (stream
     (struct:read-structure io))))

(defun write-structure* (structure io)
  (etypecase io
    (vector-input
     (setf (vector-input-index io)
           (struct:encode-structure structure (vector-input-vector io) (vector-input-index io))))
    (stream
     (struct:write-structure structure io)))
  io)

(defmacro parse-structure (structure-type io-var)
  (let ((io (gensym "IO")))
    `(let ((,io ,io-var))
       (etypecase ,io
         (vector-input
          (multiple-value-bind (value index)
              (,(intern (format NIL "~a-~a" 'decode structure-type))
               (vector-input-vector ,io) (vector-input-index ,io))
            (setf (vector-input-index ,io) index)
            value))
         (stream
          (,(intern (format NIL "~a-~a" 'read structure-type)) ,io))))))

(defun call-with-io (function io &key (start 0) end (if-exists :error) (direction :input))
  (etypecase io
    ((or string pathname)
     (if (epsilon.sys.fs:dir-p io)
         (funcall function (make-directory-input))
         (with-open-file (stream io :direction direction
                                    :element-type 'u8
                                    :if-exists if-exists)
           (funcall function stream))))
    (io
     (funcall function io))
    (vector
     (funcall function (make-vector-input io start start (or end (length io)))))))

(defmacro with-io ((io target &rest args) &body body)
  `(call-with-io (lambda (,io) ,@body) ,target ,@args))

(defparameter *file-attribute-compatibility-map*
  (alist-vector '((0 . :ms-dos)
                  (1 . :amiga)
                  (2 . :open-vms)
                  (3 . :unix)
                  (4 . :vm/cms)
                  (5 . :atari-st)
                  (6 . :os/2)
                  (7 . :macintosh)
                  (8 . :z-system)
                  (9 . :cp/m)
                  (10 . :ntfs)
                  (11 . :mvs)
                  (12 . :vse)
                  (13 . :acorn-risc)
                  (14 . :vfat)
                  (15 . :alternate-mvs)
                  (16 . :beos)
                  (17 . :tandem)
                  (18 . :os/400)
                  (19 . :darwin))))

(defun file-attribute-name (id)
  (if (<= 0 id (1- (length *file-attribute-compatibility-map*)))
      (aref *file-attribute-compatibility-map* id)
      (error 'unknown-enum-value :value id)))

(defun file-attribute-id (name)
  (or (position name *file-attribute-compatibility-map*)
      (error 'unknown-enum-value :value name)))

(defparameter *compression-method-map*
  (alist-vector '((0 . :store)
                  (1 . :shrink)
                  (2 . :reduce-1)
                  (3 . :reduce-2)
                  (4 . :reduce-3)
                  (5 . :reduce-4)
                  (6 . :implode)
                  (7 . :tokenizing)
                  (8 . :deflate)
                  (9 . :deflate64)
                  (10 . :pkware-implode)
                  (11 . :reserved)
                  (12 . :bzip2)
                  (13 . :reserved)
                  (14 . :lzma)
                  (15 . :reserved)
                  (16 . :cmpsc)
                  (17 . :reserved)
                  (18 . :terse)
                  (19 . :lz77)
                  (20 . :zstd)
                  (96 . :jpeg)
                  (97 . :wavpack)
                  (98 . :ppmd)
                  (99 . :ae-x))))

(defun compression-method-name (id)
  (if (<= 0 id (1- (length *compression-method-map*)))
      (aref *compression-method-map* id)
      (error 'unknown-enum-value :value id)))

(defun compression-method-id (name)
  (or (position name *compression-method-map*)
      (error 'unknown-enum-value :value name)))

(defparameter *encryption-method-map*
  (alist-table '((#x6601 . :des)
                 (#x6602 . :rc2)
                 (#x6603 . :3des-168)
                 (#x6609 . :3des-112)
                 (#x660e . :aes-128) 
                 (#x660f . :aes-192) 
                 (#x6610 . :aes-256) 
                 (#x6702 . :rc2)
                 (#x6720 . :blowfish)
                 (#x6721 . :twofish)
                 (#x6801 . :rc4)
                 (#xffff . :unknown))))

(defun encryption-method-name (id)
  (or (gethash id *encryption-method-map*)
      (error 'unknown-enum-value :value id)))

(defun encryption-method-id (name)
  (loop for id being the hash-keys of *encryption-method-map*
        for val being the hash-values of *encryption-method-map*
        do (when (eql name val) (return id))
        finally (error 'unsupported-encryption-method :encryption-method name)))

;; TODO merge with lib.codec

(defgeneric make-decompression-state (format))

(defgeneric call-with-decompressed-buffer (function vector start end state))

(defgeneric make-compression-state (format &key buffer))

(defgeneric call-with-compressed-buffer (function vector start end state))

(defgeneric call-with-completed-compressed-buffer (function state))

(defmethod make-decompression-state (format)
  (error 'unsupported-compression-method :compression-method format))

(defmethod make-decompression-state ((format (eql nil)))
  nil)

(defmethod make-decompression-state ((format (eql :store)))
  nil)

(defmethod call-with-decompressed-buffer (function input start end (state (eql nil)))
  (funcall function input start end))

(defclass deflate-state-2 ()
  ((output-buffer :initarg :buffer)))

(defmethod make-decompression-state ((format (eql :deflate)))
  (make-instance 'deflate-state-2 :buffer (stream:make-output-stream)))

(defmethod call-with-decompressed-buffer (function input start end (state deflate-state-2))
  (with-slots (output-buffer) state
    (codec::decompress output-buffer (codec::make-dstate :deflate) input
                           :input-state start
                           :input-end end)
    (let ((result (stream:buffer output-buffer)))
      (funcall function result 0 (length result)))
    (- end start)))

(defmethod make-compression-state ((format (eql nil)) &key buffer)
  (declare (ignore buffer))
  nil)

(defmethod make-compression-state ((format (eql :store)) &key buffer)
  (declare (ignore buffer))
  nil)

(defmethod call-with-compressed-buffer (function vector start end (state null))
  (funcall function vector start end))

(defmethod call-with-completed-compressed-buffer (function (state (eql nil)))
  (funcall function #() 0 0))

(defmethod make-compression-state ((format (eql :deflate)) &key buffer)
  (declare (ignore buffer))
  (make-instance 'codec::deflate-compressor))

(defmethod call-with-compressed-buffer (function vector start end (state codec::deflate-compressor))
  (setf (codec::callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (codec::compress-u8-vector vector state :start start :end end))

(defmethod call-with-completed-compressed-buffer (function (state codec::deflate-compressor))
  (setf (codec::callback state) (lambda (buffer end) (funcall function buffer 0 end)))
  (codec::finish-compression state))

(defgeneric make-decryption-state (format input password &key buffer &allow-other-keys))
(defgeneric call-with-decrypted-buffer (function input length state))

(defgeneric make-encryption-state (format password &key buffer))
(defgeneric call-with-encrypted-buffer (function vector start end state))
(defgeneric call-with-completed-encrypted-buffer (function state))

(defmethod make-decryption-state (format input password &rest args)
  (declare (ignore args))
  (error 'unsupported-encryption-method :encryption-method format))

(defstruct (vector-decryption-state
            (:constructor make-vector-decryption-state ()))
  (consumed 0 :type (unsigned-byte 32)))

(defmethod make-decryption-state ((format (eql NIL)) (input vector-input) password &key buffer)
  (declare (ignore buffer))
  (make-vector-decryption-state))

(defstruct (stream-decryption-state
            (:constructor make-stream-decryption-state (buffer)))
  (buffer NIL :type (simple-array u8 (*)))
  (index 0 :type (unsigned-byte 32))
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32)))

(defmethod make-decryption-state ((format (eql NIL)) (input stream) password &key buffer)
  (make-stream-decryption-state (ensure-buffer buffer)))

(defmethod call-with-decrypted-buffer (function (input stream) length (state stream-decryption-state))
  (let ((buffer (stream-decryption-state-buffer state))
        (index (stream-decryption-state-index state)))
    (flet ((output (start end)
             (let ((consumed (funcall function buffer start end)))
               (setf (stream-decryption-state-start state) consumed)
               (setf (stream-decryption-state-end state) end)
               (when (< consumed end)
                 (return-from call-with-decrypted-buffer consumed))
               consumed)))
      (when (< (stream-decryption-state-start state)
               (stream-decryption-state-end state))
        (output (stream-decryption-state-start state)
                (stream-decryption-state-end state)))
      (loop for read = (read-sequence buffer input :end (min (length buffer)
                                                             (- length index)))
            for consumed = (output 0 read)
            do (cond ((= 0 consumed)
                      (setf (stream-decryption-state-end state) 0)
                      (return))
                     (T
                      (incf index consumed))))
      (prog1 (- index (stream-decryption-state-index state))
        (setf (stream-decryption-state-index state) index)))))

(defmethod call-with-decrypted-buffer (function (input vector-input) length state)
  (let* ((start (vector-input-index input))
         (offset (+ start (vector-decryption-state-consumed state)))
         (read (funcall function (vector-input-vector input) offset (+ start length))))
    (prog1 (- read offset)
      (setf (vector-decryption-state-consumed state) (- read start)))))

(defmethod make-encryption-state ((format (eql NIL)) password &key buffer)
  (declare (ignore buffer))
  NIL)

(defmethod call-with-encrypted-buffer (function vector start end (state (eql NIL)))
  (funcall function vector start end))

(defmethod call-with-completed-encrypted-buffer (function (state (eql NIL)))
  (funcall function #() 0 0))

;; TODO: Support for AE-X https://www.winzip.com/win/en/aes_info.html
;; TODO: Support for other encryption methods
;; TODO: Support for central directory encryption

(defstruct (pkware-decrypt-state
            (:constructor %make-pkware-decrypt-state (buffer)))
  (buffer NIL :type (simple-array u8 (*)))
  (k0 305419896 :type (unsigned-byte 32))
  (k1 591751049 :type (unsigned-byte 32))
  (k2 878082192 :type (unsigned-byte 32)))

(defun crc-nth (n)
  (logior (ash (aref crc-32::*crc-32-table* (* 2 n)) 16)
          (aref crc-32::*crc-32-table* (1+ (* 2 n)))))

(defun crc32-rotate (crc byte)
  (logxor (ldb (byte 24 8) crc)
          (crc-nth (ldb (byte 8 0) (logxor crc byte)))))

(defun update-pkware-state (state byte)
  (setf (pkware-decrypt-state-k0 state) (crc32-rotate (pkware-decrypt-state-k0 state) byte))
  (setf (pkware-decrypt-state-k1 state) (logand #xFFFFFFFF (+ (pkware-decrypt-state-k1 state)
                                                              (logand (pkware-decrypt-state-k0 state) #xFF))))
  (setf (pkware-decrypt-state-k1 state) (logand #xFFFFFFFF (1+ (* (pkware-decrypt-state-k1 state) 134775813))))
  (setf (pkware-decrypt-state-k2 state) (crc32-rotate (pkware-decrypt-state-k2 state) (ash (pkware-decrypt-state-k1 state) -24))))

(defun pkware-decrypt-byte (state)
  (let ((temp (logand #xFFFF (logior 2 (pkware-decrypt-state-k2 state)))))
    (ash (* temp (logxor temp 1)) -8)))

(defun make-pkware-decrypt-state (buffer password initial-state index)
  (let ((state (%make-pkware-decrypt-state buffer)))
    (loop for byte across password
          do (update-pkware-state state byte))
    (loop for i from index below (+ index 12)
          for byte = (aref initial-state i)
          for c = (logxor byte (pkware-decrypt-byte state))
          do (update-pkware-state state c))
    state))

(defmethod make-decryption-state ((format (eql :pkware)) (input stream) password &key buffer)
  (let ((initial-state (make-array 12 :element-type 'u8)))
    (read-sequence initial-state input)
    (make-pkware-decrypt-state (ensure-buffer buffer) (ensure-password password) initial-state 0)))

(defmethod make-decryption-state ((format (eql :pkware)) (input vector-input) password &key buffer)
  (let ((state (make-pkware-decrypt-state (ensure-buffer buffer) (ensure-password password)
                                          (vector-input-vector input) (vector-input-index input))))
    (incf (vector-input-index input) 12)
    state))

(defmethod call-with-decrypted-buffer (function (input stream) length (state pkware-decrypt-state))
  (loop with buffer = (pkware-decrypt-state-buffer state)
        while (< 0 length)
        for read = (read-sequence buffer input :end length)
        do (loop for i from 0 below read
                 for byte = (aref buffer i)
                 for decrypted = (logxor byte (pkware-decrypt-byte state))
                 do (update-pkware-state state decrypted)
                    (setf (aref buffer i) (ldb (byte 8 0) decrypted)))
           (decf length read)
           ;; FIXME: does not work correctly.
           (let ((consumed (funcall function buffer 0 read)))
             (when (< consumed read)
               (return read)))))

(defmethod call-with-decrypted-buffer (function (input vector-input) length (state pkware-decrypt-state))
  (loop with inbuffer = (vector-input-vector input)
        with index = (vector-input-index input)
        with buffer = (pkware-decrypt-state-buffer state)
        for read = (min length (length buffer))
        while (< 0 length)
        do (loop for i from 0 below read
                 for byte = (aref inbuffer index)
                 for decrypted = (logxor byte (pkware-decrypt-byte state))
                 do (update-pkware-state state decrypted)
                    (setf (aref buffer i) decrypted)
                    (incf index))
           (decf length read)
           ;; FIXME: does not work correctly.
           (let ((consumed (funcall function buffer 0 read)))
             (when (< consumed read)
               (return read)))))

(struct:define-byte-structure (local-file #x04034B50)
  (version u16)
  (flags u16)
  (compression-method u16)
  (last-modified-time u16)
  (last-modified-date u16)
  (crc-32 u32)
  (compressed-size u32)
  (uncompressed-size u32)
  (file-name-length u16)
  (extra-field-length u16)
  (file-name u8 file-name-length)
  (extra u8 extra-field-length))

(struct:define-byte-structure (data-descriptor #x08074B50)
  (crc-32 u32)
  (compressed-size u32)
  (uncompressed-size u32))

(struct:define-byte-structure (data-descriptor/64 #x08074B50)
  (crc-32 u32)
  (compressed-size u64)
  (uncompressed-size u64))

(struct:define-byte-structure (extra-data #x08064B50)
  (extra-field-length u32)
  (extra u8 extra-field-length))

(struct:define-byte-structure (central-directory-entry #x02014B50)
  (version-made u16)
  (version-needed u16)
  (flags u16)
  (compression-method u16)
  (last-modified-time u16)
  (last-modified-date u16)
  (crc-32 u32)
  (compressed-size u32)
  (uncompressed-size u32)
  (file-name-length u16)
  (extra-field-length u16)
  (file-comment-length u16)
  (disk-number-start u16)
  (internal-file-attributes u16)
  (external-file-attributes u32)
  (local-header-offset u32)
  (file-name u8 file-name-length)
  (extra u8 extra-field-length)
  (file-comment u8 file-comment-length))

(struct:define-byte-structure (digital-signature #x05054B50)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (end-of-central-directory/64 #x06064B50)
  (size u64)
  (version-made u16)
  (version-needed u16)
  (number-of-disk u32)
  (central-directory-disk u32)
  (disk-entries u64)
  (central-directory-entries u64)
  (central-directory-size u64)
  (central-directory-start u64)
  (data-sector u8 (- size 44)))

(struct:define-byte-structure (end-of-central-directory-locator/64 #x07064B50)
  (central-directory-disk u32)
  (central-directory-start u64)
  (number-of-disks u32))

(struct:define-byte-structure (end-of-central-directory #x06054B50)
  (number-of-disk u16)
  (central-directory-disk u16)
  (disk-entries u16)
  (central-directory-entries u16)
  (central-directory-size u32)
  (central-directory-start u32)
  (file-comment-length u16)
  (file-comment u8 file-comment-length))

(struct:define-byte-structure decryption-header
  (iv-size u16)
  (iv u8 iv-size)
  (size u32)
  (format u16)
  (encryption-algorithm u16)
  (bit-length u16)
  (flags u16)
  (random-data-size u16)
  (random-data u8 random-data-size)
  (reserved-size u32)
  (reserved u8 reserved-size)
  (validation-size u16)
  (validation u8 validation-size)
  (crc u32))

;;; Extensible data fields
(struct:define-byte-structure (zip64-extended-information #x00001)
  (size u16)
  (original-size u64)
  (compressed-size u64)
  (header-offset u64)
  (starting-disk u32))

(struct:define-byte-structure (os/2 #x00009)
  (size u16)
  (uncompressed-size u32)
  (compression-type u16)
  (crc u32)
  (data u8 (- size 10)))

(struct:define-byte-structure (ntfs #x000A)
  (size u16)
  (reserved u32)
  (data u8 (- size 4)))

(struct:define-byte-structure (openvms #x000C)
  (size u16)
  (crc u32)
  (data u8 (- size 4)))

(struct:define-byte-structure (unix #x000D)
  (size u16)
  (atime u32)
  (mtime u32)
  (uid u16)
  (gid u16)
  (data u8 (- size 12)))

(struct:define-byte-structure (patch-descriptor #x000F)
  (size u16)
  (version u16)
  (flags u32)
  (old-size u32)
  (old-crc u32)
  (new-size u32)
  (new-crc u32))

(struct:define-byte-structure (pkcs7-store #x0014)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (x509-file #x0015)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (x509-central-directory #x0016)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (encryption-header #x0017)
  (size u16)
  (format u16)
  (encryption-algorithm u16)
  (bit-length u16)
  (flags u16)
  (certificate u8 (- size 8)))

(struct:define-byte-structure (record-management-controls #x0018)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (pkcs7-encryption-recipient-certificate-list #x0019)
  (size u16)
  (version u16)
  (store u8 (- size 2)))

(struct:define-byte-structure (mvs #x0065)
  (size u16)
  (id u32)
  (data u8 (- size 4)))

(struct:define-byte-structure (policy-decryption-key-record #x0021)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (key-provider-record #x0022)
  (size u16)
  (data u8 size))

(struct:define-byte-structure (policy-key-data-record #x0023)
  (size u16)
  (data u8 size))


;;; Third-Party Extra Fields
;;; Note: the APPNOTE.TXT mentions many more mappings as 'registered'
;;;       but does not note their internal structure. We just omit
;;;       them here.
(struct:define-byte-structure (zipit-macintosh-long #x2605)
  (size u16)
  (signature #x5A504954)
  (length u8)
  (file-name u8 length)
  (file-type u8 4)
  (creator u8 4))

(struct:define-byte-structure (zipit-macintosh-short-file #x2705)
  (size u16)
  (signature #x5A504954)
  (file-type u8 4)
  (creator u8 4)
  (flags u16))

(struct:define-byte-structure (zipit-macintosh-short-dir #x2805)
  (size u16)
  (signature #x5A504954)
  (flags u16)
  (view u16))

(struct:define-byte-structure (infozip-unicode-comment #x6375)
  (size u16)
  (version u8)
  (crc-32 u32)
  (comment u8 (- size 6)))

(struct:define-byte-structure (infozip-unicode-path #x7075)
  (size u16)
  (version u8)
  (crc-32 u32)
  (name u8 (- size 6)))

(struct:define-byte-structure (data-stream-alignment #xa11e)
  (size u16)
  (alignment u16)
  (padding u8 (- size 2)))

(struct:define-byte-structure (microsoft-open-packaging-growth-hint #xa220)
  (size u16)
  (signature u16)
  (padding-value u16)
  (padding u8 (- size 4)))

(struct:define-byte-structure (aes-extra-data #x9901)
  (size u16)
  (version u16)
  (vendor u16)
  (encryption-strength u8)
  (compression-method u16))

(defclass zip-file ()
  ((entries :initarg :entries
            :initform (make-array 0 :adjustable T :fill-pointer T) :accessor entries)
   (disks :initarg :disks
          :initform NIL
          :accessor disks)
   (comment :initform NIL
            :initarg :comment
            :accessor comment)))

(defmethod close ((file zip-file) &key abort)
  (when (disks file)
    (loop :for disk across (disks file)
          :do (when (streamp disk)
               (close disk :abort abort)))
    (setf (disks file) NIL)))

(defmethod print-object ((file zip-file) stream)
  (let ((disk (when (disks file) (aref (disks file) (1- (length (disks file)))))))
    (print-unreadable-object (file stream :type T)
      (etypecase disk
        (stream (if (open-stream-p disk)
                    (format stream "~s" (pathname disk))
                    (format stream "CLOSED")))
        (vector-input (format stream "[VECTOR]"))
        (null (format stream "CLOSED"))))))

(defun move-in-memory (file)
  (when (disks file)
    (loop :for i from 0 below (length (disks file))
          :for disk = (aref (disks file) i)
          :do (when (streamp disk)
               (unless (open-stream-p disk)
                 (error 'stream-closed))
               (file-position disk 0)
               (let ((buffer (make-array (file-length disk) :element-type 'u8)))
                 (read-sequence buffer disk)
                 (setf (aref (disks file) i) (make-vector-input buffer 0 0 (length buffer)))
                 (close disk))))))

(defclass zip-entry ()
  ((zip-file :initarg :zip-file
             :initform nil
             :accessor zip-file)
   (crc-32 :initform nil
           :accessor crc-32)
   (disk :initform nil
         :accessor disk)
   (offset :initform nil
           :accessor offset)
   (size :initform nil
         :accessor size)
   (uncompressed-size :initform nil
                      :accessor uncompressed-size)
   (extra-fields :initform nil
                 :accessor extra-fields)
   (version :initform nil
            :initarg :version
            :accessor version)
   (attributes :initform nil
               :initarg :attributes
               :accessor attributes)
   (encryption-method :initform nil
                      :initarg :encryption-method
                      :accessor encryption-method)
   (compression-method :initform nil
                       :initarg :compression-method
                       :accessor compression-method)
   (last-modified :initform (get-universal-time)
                  :initarg :last-modified
                  :accessor last-modified)
   (file-name :initform nil
              :initarg :file-name
              :accessor file-name)
   (comment :initform nil
            :initarg :comment
            :accessor comment)
   (content :initform nil
            :initarg :content
            :accessor content)))

(defmethod print-object ((entry zip-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (format stream "~s" (file-name entry))))

(defun entry-to-file (path entry &key (if-exists :error) password (restore-attributes T))
  (with-open-file (stream path :direction :output
                               :element-type 'u8
                               :if-exists if-exists)
    (flet ((output (buffer start end)
             (write-sequence buffer stream :start start :end end)
             end))
      (decode-entry #'output entry :password password)))
  (when (and restore-attributes
             (eql *compatibility* (second (attributes entry))))
    ;; TODO: restore other extended attributes from the extra blocks (uid/gid/etc)
    (setf (epsilon.sys.fs:attributes path) (third (attributes entry)))))

(defun entry-to-stream (stream entry &key password)
  (flet ((output (buffer start end)
           (write-sequence buffer stream :start start :end end)
           end))
    (decode-entry #'output entry :password password)))

(defun entry-to-vector (entry &key vector (start 0) password)
  (let ((vector (etypecase vector
                  ((vector u8) vector)
                  (null (make-array (uncompressed-size entry) :element-type 'u8))))
        (i start))
    (flet ((fast-copy (buffer start end)
             #+sbcl
             (sb-sys:with-pinned-objects (vector buffer)
               (sb-kernel:system-area-ub8-copy (sb-sys:vector-sap buffer) start (sb-sys:vector-sap vector) i (- end start))
               (incf i (- end start))
               end))
           (slow-copy (buffer start end)
             (loop :for j from start below end
                   :do (setf (aref vector i) (aref buffer j))
                      (incf i))
             end))
      (if (typep vector 'sb-kernel:simple-unboxed-array)
          (decode-entry #'fast-copy entry :password password)
          (decode-entry #'slow-copy entry :password password))
      vector)))

(defmacro with-zip-file ((file input &key (start 0) end) &body body)
  `(call-with-input-zip-file (lambda (,file) ,@body) ,input :start ,start :end ,end))

(defun extract-zip (file path &key (if-exists :error) password)
  (etypecase file
    (zip-file
     (loop :for entry across (entries file)
           :for full-path = (merge-pathnames (file-name entry) path)
           :do (ensure-directories-exist full-path)
              (unless (getf (first (attributes entry)) :directory)
                (entry-to-file full-path entry :if-exists if-exists :password password))))
    (T
     (with-zip-file (zip file)
       (extract-zip zip path :if-exists if-exists)))))

(defun ensure-zip-file (file)
  (etypecase file
    ((or pathname string list)
     (let ((entries (make-array 0 :adjustable T :fill-pointer T)))
       (flet ((process-file (file)
                (cond ((wild-pathname-p file)
                       (dolist (path (directory file))
                         (vector-push-extend (make-instance 'zip-entry :content path :file-name (enough-namestring path file)) entries)))
                      ((or (pathname-name file) (pathname-type file))
                       (vector-push-extend (make-instance 'zip-entry :content file) entries))
                      (t
                       (loop with base = (truename file)
                             :for path in (epsilon.sys.fs:list-dir file)
                             :for file-name = (enough-namestring path base)
                             :do (vector-push-extend (make-instance 'zip-entry :content path :file-name file-name) entries))))))
         (if (listp file)
             (mapc #'process-file file)
             (process-file file)))
       (make-instance 'zip-file :entries entries :comment "Created with Zippy")))
    ((or vector stream)
     (let ((entries (make-array 1)))
       (setf (aref entries 0) (make-instance 'zip-entry :content file :file-name "-"))
       (make-instance 'zip-file :entries entries :comment "Created with Zippy")))
    (zip-file
     file)))

(defun compress-zip (file target &key (start 0) end (if-exists :error) password)
  (let ((file (ensure-zip-file file)))
    (when password
      (loop :for entry across (entries file)
            :do (setf (encryption-method entry) :pkware)))
    (with-io (io target :direction :output :if-exists if-exists :start start :end end)
      (encode-file file io :password password))))

(define-condition archive-file-required (error)
  ((disk :initarg :disk :initform (error "DISK required.") :reader disk))
  (:report (lambda (c s) (format s "Disk ~a is required to continue reading the Zip file."
                                 (disk c)))))

(defun decode-extra-fields (vector)
  (let ((fields ()))
    (loop with index = 0
          while (< index (length vector))
          do (let* ((sig (u16ref/le vector index))
                    (dec (gethash sig struct:*structures*)))
               (incf index 2)
               (when dec
                 (push (funcall (first dec) vector index) fields))
               (if (< index (length vector))
                   (incf index (+ 2 (u16ref/le vector index)))
                   (return))))
    (nreverse fields)))

(defun process-extra-field (entry field)
  (typecase field
    (zip64-extended-information
     (setf (size entry) (zip64-extended-information-compressed-size field))
     (setf (uncompressed-size entry) (zip64-extended-information-original-size field))
     (setf (offset entry) (zip64-extended-information-header-offset field))
     (setf (disk entry) (zip64-extended-information-starting-disk field)))
    (encryption-header
     (setf (encryption-method entry)
           (list (encryption-method-name (encryption-header-encryption-algorithm field))
                 :bit-length (encryption-header-bit-length field))))
    (aes-extra-data
     (setf (encryption-method entry) (list (ecase (aes-extra-data-version field)
                                             (1 :AE-1)
                                             (2 :AE-2))
                                           :bit-length (ecase (aes-extra-data-encryption-strength field)
                                                         (1 128)
                                                         (2 192)
                                                         (3 256))))
     (setf (compression-method entry) (compression-method-name (aes-extra-data-compression-method field))))))

(defun lf-to-entry (lf entry)
  (macrolet ((maybe-set (field value)
               `(let ((value ,value))
                  (cond ((null (,field entry))
                         (setf (,field entry) value))
                        ((not (equal value (,field entry)))
                         (warn "Mismatch in ~a:~%  Central directory: ~a~%  Local file header: ~a"
                               ',field (,field entry) value))))))
    (maybe-set version (decode-version (local-file-version lf)))
    (setf (crc-32 entry) (local-file-crc-32 lf))
    ;; Ignore if size is not contained or in zip64
    (unless (or (logbitp 3 (local-file-flags lf))
                (= #xFFFFFFFF (local-file-compressed-size lf)))
      (maybe-set size (local-file-compressed-size lf))
      (maybe-set uncompressed-size (local-file-uncompressed-size lf)))
    (maybe-set compression-method (compression-method-name (local-file-compression-method lf)))
    (maybe-set encryption-method (cond ((logbitp 6 (local-file-flags lf)) '(:unknown))
                                       ((logbitp 0 (local-file-flags lf)) '(:pkware))))
    (maybe-set file-name (decode-string (local-file-file-name lf) (local-file-flags lf)))
    (setf (extra-fields entry) (append (extra-fields entry) (decode-extra-fields (local-file-extra lf))))
    (loop for field in (extra-fields entry)
          do (process-extra-field entry field))))

(defun cde-to-entry (cde entry)
  (setf (version entry) (decode-version (central-directory-entry-version-needed cde)))
  (setf (attributes entry) (decode-file-attribute (ldb (byte 8 8) (central-directory-entry-version-made cde))
                                                  (central-directory-entry-external-file-attributes cde)))
  (setf (crc-32 entry) (central-directory-entry-crc-32 cde))
  (setf (size entry) (central-directory-entry-compressed-size cde))
  (setf (uncompressed-size entry) (central-directory-entry-uncompressed-size cde))
  (setf (offset entry) (central-directory-entry-local-header-offset cde))
  (setf (disk entry) (central-directory-entry-disk-number-start cde))
  (setf (last-modified entry) (decode-msdos-timestamp (central-directory-entry-last-modified-date cde)
                                                      (central-directory-entry-last-modified-time cde)))
  (setf (compression-method entry) (compression-method-name (central-directory-entry-compression-method cde)))
  (setf (encryption-method entry) (cond ((logbitp 6 (central-directory-entry-flags cde)) '(:strong))
                                        ((logbitp 0 (central-directory-entry-flags cde)) '(:pkware))))
  (setf (comment entry) (decode-string (central-directory-entry-file-comment cde)
                                       (central-directory-entry-flags cde)))
  (setf (file-name entry) (decode-string (central-directory-entry-file-name cde)
                                         (central-directory-entry-flags cde)))
  (setf (extra-fields entry) (decode-extra-fields (central-directory-entry-extra cde)))
  (loop for field in (extra-fields entry)
        do (process-extra-field entry field)))

(defun decode-central-directory (input entries entry-offset)
  (let ((i entry-offset))
    (loop for structure = (parse-structure* input)
          for entry = (make-instance 'zip-entry)
          do (cde-to-entry structure entry)
             (setf (aref entries i) entry)
             (incf i)
          while (and (has-more input)
                     (< i (length entries))))
    i))

(defun decode-file (input)
  (let (entries disks)
    ;; First seek to end of file, then backtrack to find the end-of-central-directory signature.
    ;; We skip the bytes that are guaranteed to be part of the structure anyway. Thus, if the
    ;; comment is empty, we should immediately end up at the signature.
    (seek input (- (end input) (+ 4 2 2 2 2 4 4 2)))
    (loop for byte = (u32 input)
          until (= #x06054B50 byte)
          ;; Seek back the 4 bytes we read +1 extra byte.
          ;; TODO: This could be sped up by trying to match parts of the signature against what we
          ;;       read and then speculatively back up more bytes.
          do (if (<= (start input) (- (index input) 5))
                 (seek input (- (index input) 5))
                 (error 'malformed-file :message "No end of central directory marker could be found.")))
    ;; We should now be at the beginning (after the signature) of the end-of-central-directory.
    (let* ((eocd (parse-structure end-of-central-directory input))
           (cd-offset (end-of-central-directory-central-directory-start eocd))
           (cd-start-disk (end-of-central-directory-central-directory-disk eocd))
           (cd-end-disk (end-of-central-directory-number-of-disk eocd)))
      ;; OK, next we look for end-of-central-directory-locator/64, which should be
      ;; input - 4 (eocd sig) - 16 (ecod64 payload) - 4 (eocd64 sig)
      (seek input (- (index input) 4 16 4))
      (when (= #x07064B50 (u32 input))
        (let ((eocd-locator (parse-structure end-of-central-directory-locator/64 input))
              (eocd64-input input))
          (when (/= (end-of-central-directory-number-of-disk eocd)
                    (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
            (restart-case (error 'archive-file-required :disk (end-of-central-directory-locator/64-central-directory-disk eocd-locator))
              (use-value (new-input)
                (setf eocd64-input new-input))))
          (setf disks (make-array (end-of-central-directory-locator/64-number-of-disks eocd-locator) :initial-element NIL))
          (setf (aref disks (end-of-central-directory-locator/64-central-directory-disk eocd-locator)) eocd64-input)
          ;; Okey, header is on here, let's check it.
          (seek eocd64-input (end-of-central-directory-locator/64-central-directory-start eocd-locator))
          (if (= #x06064B50 (u32 eocd64-input))
              (let ((eocd (parse-structure end-of-central-directory/64 eocd64-input)))
                (setf cd-offset (end-of-central-directory/64-central-directory-start eocd))
                (setf cd-start-disk (end-of-central-directory/64-central-directory-disk eocd))
                (setf cd-end-disk (end-of-central-directory/64-number-of-disk eocd))
                (setf entries (make-array (end-of-central-directory/64-central-directory-entries eocd)
                                          :initial-element NIL :adjustable T :fill-pointer T)))
              (warn "File appears corrupted: 

Zip64 End of Central Directory Record was not at indicated position.
Will attempt to continue with 32 bit standard central directory."))))
      (cond ((and (null entries) (= #xFFFFFFFF (end-of-central-directory-central-directory-start eocd)))
             (error 'malformed-file :message "No Zip64 End of Central Directory record found, but End of Central
Directory contains a start marker that indicates there should be
one."))
            (T
             (let ((i 0))
               (unless entries
                 (setf entries (make-array (end-of-central-directory-central-directory-entries eocd)
                                           :initial-element NIL :adjustable T :fill-pointer T)))
               (unless disks
                 (setf disks (make-array (1+ (end-of-central-directory-number-of-disk eocd)) :initial-element NIL)))
               (unless (= #xFFFF (end-of-central-directory-number-of-disk eocd))
                 (setf (aref disks (end-of-central-directory-number-of-disk eocd)) input))
               (loop for disk from cd-start-disk to cd-end-disk
                     for input = (or (aref disks disk)
                                     (restart-case (error 'archive-file-required :disk disk)
                                       (use-value (new-input)
                                         (setf (aref disks disk) new-input))))
                     do (seek input cd-offset)
                        (setf cd-offset 0)
                        (setf i (decode-central-directory input entries i))))))
      (let ((zip-file (make-instance 'zip-file :comment (decode-string (end-of-central-directory-file-comment eocd) #b10000000000)
                                               :entries entries :disks disks)))
        (loop for entry across entries
              do (setf (zip-file entry) zip-file))
        zip-file))))

(defun open-zip-file (input &key (start 0) end)
  (etypecase input
    ((or pathname string)
     (let ((streams (list (open input :element-type 'u8)))
           (success NIL))
       (handler-bind ((archive-file-required
                        (lambda (c)
                          (let ((id (disk c)))
                            (let ((stream (open (make-pathname :type (format NIL "z~2,'0d" (1+ id)) :defaults input)
                                                :element-type 'u8)))
                              (push stream streams)
                              (use-value stream))))))
         (unwind-protect
              (let ((file (decode-file (first streams))))
                (setf success T)
                (values file streams))
           (unless success
             (mapc #'close streams))))))
    (stream
     (decode-file input))
    ((vector u8)
     (decode-file (make-vector-input input start start (or end (length input)))))))

(defun call-with-input-zip-file (function input &key (start 0) end)
  (multiple-value-bind (file streams) (open-zip-file input :start start :end end)
    (unwind-protect (funcall function file)
      (mapc #'close streams))))

(defun prepare-reading (entry)
  (let* ((disks (disks (zip-file entry)))
         (disk (disk entry))
         (input (or (aref disks disk)
                    (restart-case (error 'archive-file-required :disk disk)
                      (use-value (new-input)
                        (setf (aref disks disk) new-input))))))
    (seek input (offset entry))
    (lf-to-entry (parse-structure* input) entry)
    input))

(defun entry-raw-bytes (function entry)
  (let ((input (prepare-reading entry))
        (length (size entry)))
    (etypecase input
      (stream
       (loop with buffer = (ensure-buffer NIL)
             while (< 0 length)
             for read = (read-sequence buffer input :end (min (length buffer) length))
             do (funcall function buffer 0 read)
                (decf length read)))
      (vector-input
       (let ((start (vector-input-index input)))
         (funcall function (vector-input-vector input) start (+ start length))))
      (directory-input))))

(defun decode-entry (function entry &key password)
  (let* ((input (prepare-reading entry))
         (decryption-state (apply #'make-decryption-state (first (encryption-method entry)) input password (rest (encryption-method entry))))
         (decompression-state (make-decompression-state (compression-method entry))))
    (flet ((decompress (buffer start end)
             (call-with-decompressed-buffer function buffer start end decompression-state)))
      (call-with-decrypted-buffer #'decompress input (size entry) decryption-state))))

(defstruct (chunk-decoder
            (:constructor %make-chunk-decoder (input size decryption-state decompression-state buffer start end)))
  input
  size
  decryption-state
  decompression-state
  buffer
  start
  end)

(defun make-chunk-decoder (entry &key password)
  (let* ((input (prepare-reading entry))
         (decryption-state (apply #'make-decryption-state (first (encryption-method entry)) input password (rest (encryption-method entry))))
         (decompression-state (make-decompression-state (compression-method entry))))
    (%make-chunk-decoder input (size entry) decryption-state decompression-state NIL 0 0)))

(defun decode-chunk (decoder output start end)
  (let ((decompression-state (chunk-decoder-decompression-state decoder))
        (decryption-state (chunk-decoder-decryption-state decoder))
        (input (chunk-decoder-input decoder))
        (size (chunk-decoder-size decoder)))
    (labels ((decode (buffer bstart bend)
               (let ((copyable (min (- end start) (- bend bstart))))
                 (loop for i from 0 below copyable
                       do (setf (aref output (+ start i)) (aref buffer (+ bstart i))))
                 (incf start copyable)
                 (+ bstart copyable)))
             (decompress (buffer start end)
               (call-with-decompressed-buffer #'decode buffer start end decompression-state)))
      (loop until (= 0 (call-with-decrypted-buffer #'decompress input size decryption-state))))
    (min start end)))

(defvar *zip64-needed*)

(defun n-bit-and-note-zip64-p (bits &rest integers)
  (cond ((apply #'n-bit-p bits integers))
        (T
         (setf *zip64-needed* T)
         nil)))

(defun cap-and-note-zip64 (value bits)
  (let ((max (1- (ash 1 bits))))
    (cond ((< value max)
           value)
          (T
           (setf *zip64-needed* T)
           max))))

(defun entry-flags (entry)
  (bitfield (encryption-method entry)
            NIL
            NIL
            T ;; FIXME: Should only set this to T if the /output/ is non-seekable
            NIL
            NIL
            (and (encryption-method entry)
                 (not (eql :pkware (encryption-method entry))))
            NIL NIL NIL NIL T NIL NIL NIL NIL))

(defun backfill-from-content (entry)
  (let ((content (content entry)))
    (etypecase content
      ((or string pathname file-stream)
       (setf (last-modified entry) (file-write-date content))
       (unless (file-name entry)
         (setf (file-name entry) (file-namestring content)))
       (unless (attributes entry)
         (setf (attributes entry) (list `(,(if (epsilon.sys.fs:dir-p content)
                                               :directory
                                               :normal)
                                          T)
                                        *compatibility*
                                        (epsilon.sys.fs:attributes content))))
       (unless (epsilon.sys.fs:dir-p content)
         (typecase content
           (file-stream (setf (uncompressed-size entry) (file-length content)))
           (T
            (with-open-file (stream content :direction :input :element-type 'u8)
              (setf (uncompressed-size entry) (file-length stream)))))))
      (stream)
      (vector-input
       (setf (uncompressed-size entry) (size content)))
      (vector
       (setf (uncompressed-size entry) (length content)))
      (null
       (unless (attributes entry) ;; Assume directory.
         (setf (attributes entry) (list '(:directory T) *compatibility* (default-attributes-for *compatibility*))))))
    (unless (attributes entry)
      (setf (attributes entry) (list '(:normal T) *compatibility* (default-attributes-for *compatibility*))))
    (when (and content
               (null (compression-method entry)))
      (if (< 1024 (or (uncompressed-size entry) 1025))
          (setf (compression-method entry) :deflate)
          (setf (compression-method entry) :store)))))

(defun entry-version (entry)
  (encode-version (or (version entry) *default-version-needed*)
                  (if (consp (attributes entry))
                      (second (attributes entry))
                      *compatibility*)))

(defun entry-compression-id (entry)
  (compression-method-id
   (if (and (consp (encryption-method entry))
            (find (first (encryption-method entry)) '(:ae-1 :ae-2)))
       :ae-x
       (or (compression-method entry)
           :store))))

(defun add-extra-entry (extra entry)
  (let ((end (length extra)))
    (setf extra (adjust-array extra (+ end 4 (zip64-extended-information-size entry))))
    (encode-structure entry extra end)
    extra))

(defun entry-to-lf (entry)
  (let ((file-name (char:string-to-bytes (file-name entry) :encoding :utf-8))
        (extra (make-array 0 :adjustable T :element-type 'u8))
        (size (or (size entry) 0))
        (uncompressed-size (or (uncompressed-size entry) 0)))
    (when (not (n-bit-and-note-zip64-p 32 size))
      (add-extra-entry extra (make-zip64-extended-information
                              28 size uncompressed-size
                              (offset entry) 0)))
    (destructuring-bind (&optional method bittage) (enlist (encryption-method entry))
      (case method
        (:ae-1
         (add-extra-entry extra (make-aes-extra-data
                                 7 17729 1
                                 (ecase bittage
                                   (128 1)
                                   (192 2)
                                   (256 3))
                                 (compression-method-id (compression-method entry)))))
        (:ae-2
         (add-extra-entry extra (make-aes-extra-data
                                 7 17729 2
                                 (ecase bittage
                                   (128 1)
                                   (192 2)
                                   (256 3))
                                 (compression-method-id (compression-method entry)))))
        ((:pkware NIL))
        (T
         (add-extra-entry extra (make-encryption-header
                                 8 2 (encryption-method-id method)
                                 bittage 1 #())))))
    (multiple-value-bind (date time) (encode-msdos-timestamp (last-modified entry))
      (make-local-file (entry-version entry)
                       (entry-flags entry)
                       (entry-compression-id entry)
                       time date (or (crc-32 entry) 0)
                       (cap size 32) (cap uncompressed-size 32)
                       (length file-name) (length extra) file-name extra))))

(defun entry-to-dd (entry)
  (let ((uncompressed-size (uncompressed-size entry)))
    (if (n-bit-and-note-zip64-p 32 uncompressed-size)
        (make-data-descriptor (crc-32 entry) (size entry) uncompressed-size)
        (make-data-descriptor/64 (crc-32 entry) (size entry) uncompressed-size))))

(defun entry-to-cd (entry)
  (let ((file-name (char:string-to-bytes (file-name entry) :encoding :utf-8))
        (comment (encode-string (comment entry)))
        (extra (make-array 0 :adjustable T :element-type 'u8))
        (size (or (size entry) 0))
        (uncompressed-size (or (uncompressed-size entry) 0))
        (offset (offset entry)))
    (when (not (n-bit-and-note-zip64-p 32 size offset))
      (add-extra-entry extra (make-zip64-extended-information
                              28 size uncompressed-size
                              offset 0)))
    (multiple-value-bind (date time) (encode-msdos-timestamp (last-modified entry))
      (make-central-directory-entry
       (entry-version entry)
       (entry-version entry)
       (entry-flags entry)
       (entry-compression-id entry)
       time date (or (crc-32 entry) 0)
       (cap size 32) (cap uncompressed-size 32)
       (length file-name) (length extra) (length comment)
       0 0 (or (encode-file-attribute (attributes entry)) 0) (cap offset 32)
       file-name extra comment))))

(defun encode-entry-payload (entry output password)
  (cond ((content entry)
         (with-io (input (content entry))
           (let ((read 0)
                 (written 0)
                 (crc #xFFFFFFFF)
                 (encryption-state (make-encryption-state (encryption-method entry) password))
                 (compression-state (make-compression-state (compression-method entry))))
             (labels ((write-out (buffer start end)
                        (incf written (- end start))
                        (output output buffer start end))
                      (encrypt (buffer start end)
                        (call-with-encrypted-buffer #'write-out buffer start end encryption-state))
                      (compress (buffer start end)
                        (incf read (- end start))
                        (loop for i from start below end
                              do (setf crc (crc32-rotate crc (aref buffer i))))
                        (call-with-compressed-buffer #'encrypt buffer start end compression-state)))
               (etypecase input
                 (stream
                  (when (or (not (typep input 'file-stream))
                            (not (epsilon.sys.fs:dir-p input)))
                    (loop with buffer = (make-array 4096 :element-type 'u8)
                          for read = (read-sequence buffer input)
                          while (< 0 read)
                          do (compress buffer 0 read))))
                 (vector-input
                  (compress (vector-input-vector input) (vector-input-index input) (vector-input-end input)))
                 (directory-input))
               (call-with-completed-compressed-buffer #'encrypt compression-state)
               (call-with-completed-encrypted-buffer #'write-out encryption-state))
             (setf (crc-32 entry) (logxor #xFFFFFFFF crc))
             (setf (size entry) written)
             (setf (uncompressed-size entry) read))))
        ((and (offset entry) (size entry))
         ;; We are copying from source archive. Just transfer the bytes.
         (labels ((write-out (buffer start end)
                    (output output buffer start end)))
           (entry-raw-bytes #'write-out entry)))
        (T
         (setf (crc-32 entry) #xFFFFFFFF)
         (setf (size entry) 0)
         (setf (uncompressed-size entry) 0))))

(defun determine-min-version (zip64-used)
  ;; FIXME: be smarter about noting the min version based on other used features.
  (if zip64-used
      '(4 5)
      '(2 0)))

(defun encode-file (zip-file output &key password
                                         (version-made *default-version-made*)
                                         version-needed
                                         (zip64 :when-needed))
  (check-type zip64 (member NIL T :when-needed))
  (let ((*zip64-needed* NIL))
    (loop for i from 0
          for entry across (entries zip-file)
          for orig-offset = (offset entry)
          for offset = (index output)
          do (setf (offset entry) offset)
             (backfill-from-content entry)
             (write-structure* (entry-to-lf entry) output)
             ;; TODO: Decryption header and all that guff
             ;; KLUDGE: We temporarily reset the offset of the entry to
             ;;         ensure we can read it from the source archive should
             ;;         the entry be copyable from there.
             (progn
               (setf (offset entry) orig-offset)
               (encode-entry-payload entry output password)
               (setf (offset entry) offset))
             (write-structure* (entry-to-dd entry) output)
             ;; FIXME: If writing to a file-stream or vector, backtrack and
             ;;        Fixup the LF entry with size/crc/flag
          )
    (let* ((cd-start (index output))
           (entries (entries zip-file))
           (entry-count (length entries)))
      (loop for entry across entries
            do (write-structure* (entry-to-cd entry) output))
      (let* ((cd-end (index output))
             (comment (encode-string (comment zip-file)))
             ;; Create EOCD structure here and note overflows, so we
             ;; know whether to write the ZIP64 structures prior to
             ;; writing the EOCD structure.
             (eocd (make-end-of-central-directory
                    0 0
                    (cap-and-note-zip64 entry-count 16)
                    (cap-and-note-zip64 entry-count 16)
                    (cap (- cd-end cd-start) 32)
                    (cap cd-start 32)
                    (length comment) comment))
             (use-zip64-p (or (eq zip64 T)
                              (and (eq zip64 :when-needed) *zip64-needed*)))
             (min-version (determine-min-version use-zip64-p)))
        (cond ((not version-needed)
               (setf version-needed min-version))
              ((version< version-needed min-version)
               (error 'required-version-mismatched
                      :specified-version version-needed :required-version min-version)))
        (cond (use-zip64-p
               (write-structure* (make-end-of-central-directory/64
                                  44
                                  (encode-version version-made)
                                  (encode-version version-needed)
                                  0 0 entry-count entry-count
                                  (- cd-end cd-start) cd-start #())
                                 output)
               (write-structure* (make-end-of-central-directory-locator/64
                                  0 cd-end 1)
                                 output))
              (*zip64-needed*
               (error 'zip64-required :parameter zip64)))
        (write-structure* eocd output)))))

