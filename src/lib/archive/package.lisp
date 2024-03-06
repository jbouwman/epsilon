(defpackage #:lib.archive
  (:use
   #:cl
   #:lib.stream
   #:lib.type)
  (:export
   #:with-zip-file
   #:entries
   #:attributes
   #:decode-entry))
