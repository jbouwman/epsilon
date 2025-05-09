(in-package #:epsilon.lib.archive)

(define-byte-structure (local-file #x04034B50)
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

(define-byte-structure (data-descriptor #x08074B50)
  (crc-32 u32)
  (compressed-size u32)
  (uncompressed-size u32))

(define-byte-structure (data-descriptor/64 #x08074B50)
  (crc-32 u32)
  (compressed-size u64)
  (uncompressed-size u64))

(define-byte-structure (extra-data #x08064B50)
  (extra-field-length u32)
  (extra u8 extra-field-length))

(define-byte-structure (central-directory-entry #x02014B50)
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

(define-byte-structure (digital-signature #x05054B50)
  (size u16)
  (data u8 size))

(define-byte-structure (end-of-central-directory/64 #x06064B50)
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

(define-byte-structure (end-of-central-directory-locator/64 #x07064B50)
  (central-directory-disk u32)
  (central-directory-start u64)
  (number-of-disks u32))

(define-byte-structure (end-of-central-directory #x06054B50)
  (number-of-disk u16)
  (central-directory-disk u16)
  (disk-entries u16)
  (central-directory-entries u16)
  (central-directory-size u32)
  (central-directory-start u32)
  (file-comment-length u16)
  (file-comment u8 file-comment-length))

(define-byte-structure decryption-header
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
(define-byte-structure (zip64-extended-information #x00001)
  (size u16)
  (original-size u64)
  (compressed-size u64)
  (header-offset u64)
  (starting-disk u32))

(define-byte-structure (os/2 #x00009)
  (size u16)
  (uncompressed-size u32)
  (compression-type u16)
  (crc u32)
  (data u8 (- size 10)))

(define-byte-structure (ntfs #x000A)
  (size u16)
  (reserved u32)
  (data u8 (- size 4)))

(define-byte-structure (openvms #x000C)
  (size u16)
  (crc u32)
  (data u8 (- size 4)))

(define-byte-structure (unix #x000D)
  (size u16)
  (atime u32)
  (mtime u32)
  (uid u16)
  (gid u16)
  (data u8 (- size 12)))

(define-byte-structure (patch-descriptor #x000F)
  (size u16)
  (version u16)
  (flags u32)
  (old-size u32)
  (old-crc u32)
  (new-size u32)
  (new-crc u32))

(define-byte-structure (pkcs7-store #x0014)
  (size u16)
  (data u8 size))

(define-byte-structure (x509-file #x0015)
  (size u16)
  (data u8 size))

(define-byte-structure (x509-central-directory #x0016)
  (size u16)
  (data u8 size))

(define-byte-structure (encryption-header #x0017)
  (size u16)
  (format u16)
  (encryption-algorithm u16)
  (bit-length u16)
  (flags u16)
  (certificate u8 (- size 8)))

(define-byte-structure (record-management-controls #x0018)
  (size u16)
  (data u8 size))

(define-byte-structure (pkcs7-encryption-recipient-certificate-list #x0019)
  (size u16)
  (version u16)
  (store u8 (- size 2)))

(define-byte-structure (mvs #x0065)
  (size u16)
  (id u32)
  (data u8 (- size 4)))

(define-byte-structure (policy-decryption-key-record #x0021)
  (size u16)
  (data u8 size))

(define-byte-structure (key-provider-record #x0022)
  (size u16)
  (data u8 size))

(define-byte-structure (policy-key-data-record #x0023)
  (size u16)
  (data u8 size))


;;; Third-Party Extra Fields
;;; Note: the APPNOTE.TXT mentions many more mappings as 'registered'
;;;       but does not note their internal structure. We just omit
;;;       them here.
(define-byte-structure (zipit-macintosh-long #x2605)
  (size u16)
  (signature #x5A504954)
  (length u8)
  (file-name u8 length)
  (file-type u8 4)
  (creator u8 4))

(define-byte-structure (zipit-macintosh-short-file #x2705)
  (size u16)
  (signature #x5A504954)
  (file-type u8 4)
  (creator u8 4)
  (flags u16))

(define-byte-structure (zipit-macintosh-short-dir #x2805)
  (size u16)
  (signature #x5A504954)
  (flags u16)
  (view u16))

(define-byte-structure (infozip-unicode-comment #x6375)
  (size u16)
  (version u8)
  (crc-32 u32)
  (comment u8 (- size 6)))

(define-byte-structure (infozip-unicode-path #x7075)
  (size u16)
  (version u8)
  (crc-32 u32)
  (name u8 (- size 6)))

(define-byte-structure (data-stream-alignment #xa11e)
  (size u16)
  (alignment u16)
  (padding u8 (- size 2)))

(define-byte-structure (microsoft-open-packaging-growth-hint #xa220)
  (size u16)
  (signature u16)
  (padding-value u16)
  (padding u8 (- size 4)))

(define-byte-structure (aes-extra-data #x9901)
  (size u16)
  (version u16)
  (vendor u16)
  (encryption-strength u8)
  (compression-method u16))
