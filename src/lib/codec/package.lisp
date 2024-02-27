(defpackage #:lib.codec
  (:use
   #:cl
   #:sb-gray
   #:lib.binding
   #:lib.type
   #:lib.io
   #:lib.function
   #:lib.checksum.generic
   #:lib.checksum.adler-32
   #:lib.checksum.crc-32
   #:lib.stream)
  (:export
   #:encode
   #:encode-file
   #:encoding-error
   #:decode
   #:decode-file
   #:decoding-error
   #:make-decompressing-stream
   ))
