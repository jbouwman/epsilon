;;;; Compatibility shim for epsilon.net.errors
;;;;
;;;; This package now re-exports everything from epsilon.net.core
;;;; to maintain backwards compatibility while errors have been consolidated.

(defpackage epsilon.net.errors
  (:use cl)
  (:import-from epsilon.net.core
   ;; Error conditions
   network-error
   connection-refused
   connection-reset
   connection-aborted
   timeout-error
   address-in-use
   would-block-error
   
   ;; Error utilities
   get-errno
   errno-to-string)
  (:export
   ;; Error conditions
   network-error
   connection-refused
   connection-reset
   connection-aborted
   timeout-error
   address-in-use
   would-block-error
   
   ;; Error utilities
   get-errno
   errno-to-string))

(in-package epsilon.net.errors)