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

;; generic
;; constant
;; type
;; condition
;; hex
;; gzip
;; zlib
;; decompress
;; inflate
;; bzip
;; stream
;; bitstream
;; huffman
;; compress
;; public
