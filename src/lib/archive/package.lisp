(defpackage #:epsilon.lib.archive
  (:use
   #:cl
   #:epsilon.lib.type)
  (:local-nicknames
   (#:codec #:epsilon.lib.codec)
   (#:stream #:epsilon.lib.stream))
  (:export
   #:with-zip-file
   #:entries
   #:attributes
   #:decode-entry))
