(defpackage #:epsilon.lib.archive
  (:use
   #:cl
   #:epsilon.lib.type)
  (:local-nicknames
   (#:stream #:epsilon.lib.stream))
  (:export
   #:with-zip-file
   #:entries
   #:attributes
   #:decode-entry))
