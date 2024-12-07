(defpackage #:epsilon.lib.archive
  (:use
   #:cl
   #:epsilon.lib.stream
   #:epsilon.lib.type)
  (:export
   #:with-zip-file
   #:entries
   #:attributes
   #:decode-entry))
