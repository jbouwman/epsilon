(defpackage #:lib.archive
  (:use
   #:cl
   #:lib.type)
  (:local-nicknames
   (#:file-attributes #:org.shirakumo.file-attributes))
  (:export
   #:with-zip-file
   #:entries
   #:attributes
   #:decode-entry))
