;;;; HTTP/2 Demonstration
;;;;
;;;; Shows basic HTTP/2 functionality

(defpackage :http2-demo
  (:use :cl)
  (:local-nicknames
   (#:h2 #:epsilon.http2)
   (#:hpack #:epsilon.http2.hpack)))

(in-package :http2-demo)

;;;; Frame Demonstration

(defun demo-frames ()
  "Demonstrate HTTP/2 frame creation and serialization"
  (format t "~%=== HTTP/2 Frame Demo ===~%~%")
  
  ;; Create a SETTINGS frame
  (let ((settings-frame (h2::make-settings-frame 
                        :initial-settings 
                        '((1 . 4096)   ; HEADER_TABLE_SIZE
                          (3 . 100)    ; MAX_CONCURRENT_STREAMS
                          (4 . 65535)  ; INITIAL_WINDOW_SIZE
                          (5 . 16384))))) ; MAX_FRAME_SIZE
    (format t "Created SETTINGS frame:~%")
    (format t "  Type: ~A~%" (h2:http2-frame-type settings-frame))
    (format t "  Flags: ~A~%" (h2:http2-frame-flags settings-frame))
    (format t "  Stream ID: ~A~%" (h2:http2-frame-stream-id settings-frame))
    (format t "  Payload size: ~A bytes~%~%" (h2:http2-frame-length settings-frame)))
  
  ;; Create a PING frame
  (let ((ping-frame (h2::make-ping-frame 
                    :data (make-array 8 :element-type '(unsigned-byte 8)
                                       :initial-contents '(1 2 3 4 5 6 7 8)))))
    (format t "Created PING frame:~%")
    (format t "  Type: ~A~%" (h2:http2-frame-type ping-frame))
    (format t "  Payload: ~A~%~%" (h2:http2-frame-payload ping-frame)))
  
  ;; Create a GOAWAY frame
  (let ((goaway-frame (h2::make-goaway-frame 
                      1000 
                      h2::+error-no-error+ 
                      "Server shutting down")))
    (format t "Created GOAWAY frame:~%")
    (format t "  Type: ~A~%" (h2:http2-frame-type goaway-frame))
    (format t "  Last Stream ID encoded in payload~%~%")))

;;;; HPACK Demonstration

(defun demo-hpack ()
  "Demonstrate HPACK header compression"
  (format t "~%=== HPACK Demo ===~%~%")
  
  (let ((encoder (hpack:create-encoder))
        (decoder (hpack:create-decoder)))
    
    ;; Test headers
    (let ((headers '((":method" . "GET")
                    (":path" . "/index.html")
                    (":scheme" . "https")
                    (":authority" . "www.example.com")
                    ("accept" . "text/html,application/xhtml+xml")
                    ("accept-encoding" . "gzip, deflate, br")
                    ("user-agent" . "Epsilon/1.0"))))
      
      (format t "Original headers (~D entries):~%" (length headers))
      (dolist (header headers)
        (format t "  ~A: ~A~%" (car header) (cdr header)))
      
      ;; Encode
      (let ((encoded (hpack:encode-headers encoder headers)))
        (format t "~%Encoded size: ~D bytes~%" (length encoded))
        (format t "Compression ratio: ~,1F%~%"
                (* 100 (- 1 (/ (length encoded)
                             (reduce #'+ headers 
                                    :key (lambda (h) 
                                          (+ (length (car h)) 
                                             (length (cdr h)) 
                                             2)))))))
        
        ;; Decode
        (let ((decoded (hpack:decode-headers decoder encoded)))
          (format t "~%Decoded headers (~D entries):~%" (length decoded))
          (dolist (header decoded)
            (format t "  ~A: ~A~%" (car header) (cdr header)))
          
          ;; Verify
          (format t "~%Headers match: ~A~%" 
                  (equal (sort (copy-list headers) #'string< :key #'car)
                         (sort (copy-list decoded) #'string< :key #'car))))))))

;;;; Flow Control Demonstration

(defun demo-flow-control ()
  "Demonstrate HTTP/2 flow control"
  (format t "~%=== Flow Control Demo ===~%~%")
  
  (let ((controller (h2::make-connection-flow-controller)))
    (format t "Initial window size: ~D~%" 
            (h2::flow-controller-send-window controller))
    
    ;; Consume some window
    (h2::consume-send-window controller 1000)
    (format t "After sending 1000 bytes: ~D~%" 
            (h2::flow-controller-send-window controller))
    
    ;; Update window
    (h2::update-send-window controller 500)
    (format t "After WINDOW_UPDATE (+500): ~D~%" 
            (h2::flow-controller-send-window controller))
    
    ;; Check if we can send
    (format t "Can send 65000 bytes? ~A~%" 
            (h2::can-send-p controller 65000))
    (format t "Can send 66000 bytes? ~A~%~%" 
            (h2::can-send-p controller 66000))))

;;;; Stream State Machine Demonstration

(defun demo-stream-states ()
  "Demonstrate HTTP/2 stream state transitions"
  (format t "~%=== Stream State Demo ===~%~%")
  
  (let* ((conn (make-instance 'h2::http2-connection
                              :socket nil
                              :client-p t))
         (stream (h2::create-stream conn)))
    
    (format t "Created stream #~D~%" (h2::stream-id stream))
    (format t "Initial state: ~A~%" (h2::http2-stream-state stream))
    
    ;; Transition through states
    (setf (h2::http2-stream-state stream) :open)
    (format t "After sending HEADERS: ~A~%" (h2::http2-stream-state stream))
    
    (setf (h2::http2-stream-state stream) :half-closed-local)
    (format t "After sending END_STREAM: ~A~%" (h2::http2-stream-state stream))
    
    (setf (h2::http2-stream-state stream) :closed)
    (format t "After receiving END_STREAM: ~A~%~%" (h2::http2-stream-state stream))))

;;;; Settings Negotiation Demonstration

(defun demo-settings ()
  "Demonstrate HTTP/2 settings negotiation"
  (format t "~%=== Settings Demo ===~%~%")
  
  (format t "Default settings:~%")
  (dolist (setting h2::+default-settings+)
    (format t "  ~A: ~D~%" 
            (case (car setting)
              (:header-table-size "HEADER_TABLE_SIZE")
              (:enable-push "ENABLE_PUSH")
              (:max-concurrent-streams "MAX_CONCURRENT_STREAMS")
              (:initial-window-size "INITIAL_WINDOW_SIZE")
              (:max-frame-size "MAX_FRAME_SIZE")
              (:max-header-list-size "MAX_HEADER_LIST_SIZE")
              (t (car setting)))
            (cdr setting)))
  
  ;; Create custom settings
  (let ((custom-settings '((:header-table-size . 8192)
                          (:enable-push . 0)
                          (:max-concurrent-streams . 200))))
    (format t "~%Custom settings:~%")
    (dolist (setting custom-settings)
      (format t "  ~A: ~D~%" (car setting) (cdr setting)))))

;;;; Main Demo Function

(defun run-demo ()
  "Run all HTTP/2 demonstrations"
  (format t "~%==============================~%")
  (format t "   HTTP/2 Protocol Demo~%")
  (format t "==============================~%")
  
  (demo-frames)
  (demo-hpack)
  (demo-flow-control)
  (demo-stream-states)
  (demo-settings)
  
  (format t "~%==============================~%")
  (format t "   Demo Complete~%")
  (format t "==============================~%~%"))

;; Export the main function
(export 'run-demo)