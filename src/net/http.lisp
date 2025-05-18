(defpackage :epsilon.net.http
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:str :epsilon.lib.string)
   (:uri :epsilon.lib.uri))
  (:shadow
   :get
   :delete)
  (:export
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :fetch))
