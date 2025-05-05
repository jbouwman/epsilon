(defpackage #:epsilon.lib.codec
  (:use
   #:cl
   #:sb-gray
   #:epsilon.lib.binding
   #:epsilon.lib.checksum.adler-32
   #:epsilon.lib.checksum.crc-32
   #:epsilon.lib.checksum.generic
   #:epsilon.lib.function
   #:epsilon.lib.type)
  (:export
   #:encode
   #:encode-file
   #:encoding-error
   #:decode
   #:decode-file
   #:decoding-error
   #:make-decompressing-stream))
