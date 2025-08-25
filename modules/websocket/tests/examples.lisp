;;;; WebSocket Examples and Demo Applications
;;;;
;;;; This file contains example applications and usage patterns
;;;; for the WebSocket implementation.
;;;;
;;;; NOTE: This file is temporarily disabled as the high-level
;;;; server/client API is not yet implemented. These examples
;;;; show the intended API design for future implementation.

(defpackage epsilon.websocket.examples
  (:use cl)
  (:local-nicknames
   (ws epsilon.websocket)
   (str epsilon.string)
   (thread epsilon.sys.thread)
   (time epsilon.time))
  (:export
   ;; Placeholder - examples will be re-enabled when API is complete
   ))

(in-package epsilon.websocket.examples)

;; Examples temporarily disabled - the high-level websocket server/client
;; API needs to be implemented before these examples can work.