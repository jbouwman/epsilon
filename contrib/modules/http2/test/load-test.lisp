;;;; HTTP/2 Module Load Test
;;;;
;;;; Simple test to verify all modules can load without errors

(format t "Loading HTTP/2 modules...~%")

;; Load core modules in dependency order
(handler-case
    (progn
      (format t "Loading flow-control...~%")
      (load "modules/http2/src/flow-control.lisp")
      
      (format t "Loading frames...~%")
      (load "modules/http2/src/frames.lisp")
      
      (format t "Loading stream...~%")
      (load "modules/http2/src/stream.lisp")
      
      (format t "Loading error...~%")
      (load "modules/http2/src/error.lisp")
      
      (format t "Loading hpack...~%")
      (load "modules/http2/src/hpack.lisp")
      
      (format t "Loading priority...~%")
      (load "modules/http2/src/priority.lisp")
      
      (format t "Loading push...~%")
      (load "modules/http2/src/push.lisp")
      
      (format t "Loading main http2...~%")
      (load "modules/http2/src/http2.lisp")
      
      (format t "All modules loaded successfully!~%"))
  
  (error (e)
    (format t "Error loading modules: ~A~%" e)
    (quit 1)))

;; Test basic functionality
(format t "Testing basic functionality...~%")

(handler-case
    (progn
      ;; Test flow control
      (let ((controller (epsilon.http2.flow-control:make-connection-flow-controller)))
        (format t "✓ Flow control works~%"))
      
      ;; Test frame creation
      (let ((frame (epsilon.http2.frames:make-data-frame 1 "test")))
        (format t "✓ Frame creation works~%"))
      
      ;; Test stream creation
      (let ((stream (epsilon.http2.stream:initialize-stream 1 nil)))
        (format t "✓ Stream creation works~%"))
      
      ;; Test HPACK
      (let ((encoder (epsilon.http2.hpack:make-encoder))
            (decoder (epsilon.http2.hpack:make-decoder)))
        (format t "✓ HPACK creation works~%"))
      
      ;; Test priority tree
      (let ((tree (epsilon.http2.priority:make-priority-tree)))
        (format t "✓ Priority tree works~%"))
      
      ;; Test connection creation
      (let ((conn (epsilon.http2:make-http2-connection nil :client-p t)))
        (format t "✓ Connection creation works~%"))
      
      (format t "✓ All basic tests passed!~%"))
  
  (error (e)
    (format t "Error in basic tests: ~A~%" e)
    (quit 1)))