(defpackage #:encode
  (:use
   #:cl
   #:sb-gray
   #:encode.generic
   #:encode.type
   #:encode.checksum.adler-32)
  (:export

   ;; types
   #:u8
   #:u32
   
   #:codec
   
   #:encode
   #:encode-file
   #:encoding-error
   #:decode
   #:decode-file
   #:decoding-error))
