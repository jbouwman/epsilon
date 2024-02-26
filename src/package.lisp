(defpackage #:encode
  (:use
   #:cl
   #:sb-gray
   #:sys.type
   #:lib.io
   #:lib.eval
   #:lib.checksum.generic
   #:lib.checksum.adler-32)
  (:export
   #:encode
   #:encode-file
   #:encoding-error
   #:decode
   #:decode-file
   #:decoding-error))
