;;;; HTTP/2 Performance Benchmarks
;;;;
;;;; Benchmarks for measuring HTTP/2 implementation performance

(defpackage :epsilon.http2.benchmarks
  (:use :cl)
  (:local-nicknames
   (#:http2 #:epsilon.http2)
   (#:frames #:epsilon.http2.frames)
   (#:hpack #:epsilon.http2.hpack)
   (#:flow #:epsilon.http2.flow-control)
   (#:stream #:epsilon.http2.stream)
   (#:priority #:epsilon.http2.priority)))

(in-package :epsilon.http2.benchmarks)

;;;; Benchmark Utilities

(defmacro benchmark (name iterations &body body)
  "Run a benchmark and report timing"
  `(let ((start-time (get-internal-real-time)))
     (dotimes (i ,iterations)
       ,@body)
     (let* ((end-time (get-internal-real-time))
            (elapsed (/ (- end-time start-time) 
                       internal-time-units-per-second))
            (per-iteration (/ elapsed ,iterations)))
       (format t "~A: ~,3F seconds total, ~,6F seconds per iteration (~D iterations)~%"
               ,name elapsed per-iteration ,iterations)
       per-iteration)))

(defmacro with-memory-tracking (&body body)
  "Track memory allocation during execution"
  `(let ((before-bytes (sb-ext:get-bytes-consed)))
     (prog1 (progn ,@body)
       (let ((allocated (- (sb-ext:get-bytes-consed) before-bytes)))
         (format t "  Memory allocated: ~:D bytes~%" allocated)))))

;;;; Frame Processing Benchmarks

(defun benchmark-frame-parsing ()
  "Benchmark frame parsing performance"
  (format t "~%=== Frame Parsing Benchmarks ===~%")
  
  ;; Create test frames
  (let ((data-frame (frames:make-data-frame 1 (make-array 1024 :initial-element 42)))
        (headers-frame (frames:make-headers-frame 1 (make-array 256 :initial-element 0)))
        (settings-frame (frames:make-settings-frame))
        (window-update-frame (frames:make-window-update-frame 1 65535)))
    
    ;; Benchmark frame serialization
    (benchmark "DATA frame serialization" 100000
      (frames:write-frame nil data-frame))
    
    (benchmark "HEADERS frame serialization" 100000
      (frames:write-frame nil headers-frame))
    
    (benchmark "SETTINGS frame serialization" 100000
      (frames:write-frame nil settings-frame))
    
    (benchmark "WINDOW_UPDATE frame serialization" 100000
      (frames:write-frame nil window-update-frame))))

;;;; HPACK Benchmarks

(defun benchmark-hpack-compression ()
  "Benchmark HPACK header compression"
  (format t "~%=== HPACK Compression Benchmarks ===~%")
  
  (let ((encoder (hpack:make-encoder))
        (decoder (hpack:make-decoder))
        (small-headers '((":method" . "GET")
                        (":path" . "/")
                        (":scheme" . "https")))
        (large-headers (loop for i from 1 to 50
                           collect (cons (format nil "header-~D" i)
                                       (format nil "value-~D" i)))))
    
    ;; Small header list
    (benchmark "Small header encoding" 10000
      (hpack:encode-header-list encoder small-headers))
    
    (let ((encoded (hpack:encode-header-list encoder small-headers)))
      (benchmark "Small header decoding" 10000
        (hpack:decode-header-block decoder encoded)))
    
    ;; Large header list
    (with-memory-tracking
      (benchmark "Large header encoding" 1000
        (hpack:encode-header-list encoder large-headers)))
    
    ;; Huffman encoding
    (benchmark "Huffman encoding" 10000
      (hpack:huffman-encode "www.example.com"))
    
    (let ((encoded (hpack:huffman-encode "www.example.com")))
      (benchmark "Huffman decoding" 10000
        (hpack:huffman-decode encoded)))))

;;;; Flow Control Benchmarks

(defun benchmark-flow-control ()
  "Benchmark flow control operations"
  (format t "~%=== Flow Control Benchmarks ===~%")
  
  (let ((conn-controller (flow:make-connection-flow-controller))
        (stream-controllers (loop for i from 1 to 100
                                 collect (flow:make-stream-flow-controller))))
    
    ;; Window updates
    (benchmark "Window consumption" 100000
      (flow:consume-send-window conn-controller 100)
      (flow:update-send-window conn-controller 100))
    
    ;; Flow control checks
    (benchmark "Flow control validation" 100000
      (dolist (sc stream-controllers)
        (flow:can-send-data-p conn-controller sc 1000)))
    
    ;; DATA frame flow control
    (benchmark "DATA frame flow control" 10000
      (dolist (sc stream-controllers)
        (flow:handle-data-frame-flow-control conn-controller sc 1000)))))

;;;; Stream Management Benchmarks

(defun benchmark-stream-management ()
  "Benchmark stream state management"
  (format t "~%=== Stream Management Benchmarks ===~%")
  
  (let ((streams (loop for i from 1 to 1000 by 2
                      collect (stream:initialize-stream i nil))))
    
    ;; State transitions
    (benchmark "Stream state transitions" 10000
      (dolist (s streams)
        (stream:transition-stream-state s frames:+frame-headers+ 0)
        (stream:transition-stream-state s frames:+frame-data+ 
                                       frames:+flag-end-stream+)))
    
    ;; State validation
    (benchmark "Stream state validation" 100000
      (dolist (s streams)
        (stream:can-receive-frame-p s frames:+frame-data+)
        (stream:can-send-frame-p s frames:+frame-data+)))
    
    ;; Stream lifecycle
    (with-memory-tracking
      (benchmark "Stream creation/destruction" 1000
        (let ((temp-streams (loop for i from 1 to 100 by 2
                                 collect (stream:initialize-stream i nil))))
          (dolist (s temp-streams)
            (stream:close-stream s)))))))

;;;; Priority Tree Benchmarks

(defun benchmark-priority-tree ()
  "Benchmark priority tree operations"
  (format t "~%=== Priority Tree Benchmarks ===~%")
  
  (let ((tree (priority:make-priority-tree)))
    
    ;; Tree construction
    (benchmark "Priority tree construction" 100
      (let ((temp-tree (priority:make-priority-tree)))
        (loop for i from 1 to 1000 by 2
              do (priority:add-stream temp-tree i 
                                     :dependency (if (> i 1) (- i 2) 0)
                                     :weight (1+ (random 256))))))
    
    ;; Build a tree for other tests
    (loop for i from 1 to 1000 by 2
          do (priority:add-stream tree i 
                                 :dependency (if (> i 1) (- i 2) 0)
                                 :weight (1+ (random 256))))
    
    ;; Reprioritization
    (benchmark "Stream reprioritization" 1000
      (priority:reprioritize-stream tree 
                                   (1+ (* 2 (random 500)))
                                   0 
                                   (1+ (random 256))))
    
    ;; Scheduling
    (benchmark "Priority-based scheduling" 10000
      (priority:get-next-stream tree))
    
    ;; Tree traversal
    (benchmark "Priority tree traversal" 1000
      (loop for i from 1 to 1000 by 2
            do (priority:get-stream-children tree i)))))

;;;; Connection Benchmarks

(defun benchmark-connection-operations ()
  "Benchmark connection-level operations"
  (format t "~%=== Connection Operations Benchmarks ===~%")
  
  ;; Connection creation
  (with-memory-tracking
    (benchmark "Connection creation" 1000
      (http2:make-http2-connection nil :client-p t)))
  
  ;; Stream creation on connection
  (let ((conn (http2:make-http2-connection nil :client-p t)))
    (benchmark "Stream creation on connection" 10000
      (http2:create-stream conn))
    
    ;; Reset for next test
    (setf (slot-value conn 'http2::streams) (make-hash-table))
    (setf (slot-value conn 'http2::next-stream-id) 1))
  
  ;; Settings processing
  (let ((settings '((:header-table-size . 8192)
                   (:enable-push . 0)
                   (:max-concurrent-streams . 200)
                   (:initial-window-size . 131072)
                   (:max-frame-size . 32768))))
    (benchmark "Settings processing" 10000
      (flow:apply-settings-to-flow-control 
       (flow:make-connection-flow-controller)
       (mapcar (lambda (s) (cons (position (car s) 
                                          '(:header-table-size :enable-push
                                            :max-concurrent-streams 
                                            :initial-window-size
                                            :max-frame-size))
                                (cdr s)))
              settings)))))

;;;; Multiplexing Benchmark

(defun benchmark-stream-multiplexing ()
  "Benchmark concurrent stream handling"
  (format t "~%=== Stream Multiplexing Benchmarks ===~%")
  
  (let* ((conn (http2:make-http2-connection nil :client-p t))
         (num-streams 100)
         (streams (loop for i from 1 to (* 2 num-streams) by 2
                       collect (http2:create-stream conn))))
    
    ;; Simulate concurrent activity
    (benchmark "Concurrent stream operations" 100
      (dolist (s streams)
        ;; Simulate headers
        (stream:transition-stream-state s frames:+frame-headers+ 0)
        ;; Simulate data
        (when (stream:can-send-frame-p s frames:+frame-data+)
          (stream:transition-stream-state s frames:+frame-data+ 0))
        ;; Check flow control
        (flow:can-send-data-p 
         (http2:http2-connection-flow-controller conn)
         (stream:stream-flow-controller s)
         1000)))
    
    ;; Stream lookup performance
    (benchmark "Stream lookup" 100000
      (gethash (1+ (* 2 (random num-streams)))
               (http2:http2-connection-streams conn)))))

;;;; Comparison with HTTP/1.1

(defun benchmark-http2-vs-http1 ()
  "Compare HTTP/2 with HTTP/1.1 for various scenarios"
  (format t "~%=== HTTP/2 vs HTTP/1.1 Comparison ===~%")
  
  ;; Header compression benefit
  (let* ((http1-headers-text "GET /index.html HTTP/1.1\r\n\
Host: www.example.com\r\n\
User-Agent: Mozilla/5.0\r\n\
Accept: text/html,application/xhtml+xml\r\n\
Accept-Language: en-US,en;q=0.5\r\n\
Accept-Encoding: gzip, deflate, br\r\n\
Connection: keep-alive\r\n\r\n")
         (http2-headers '((":method" . "GET")
                        (":path" . "/index.html")
                        (":scheme" . "https")
                        (":authority" . "www.example.com")
                        ("user-agent" . "Mozilla/5.0")
                        ("accept" . "text/html,application/xhtml+xml")
                        ("accept-language" . "en-US,en;q=0.5")
                        ("accept-encoding" . "gzip, deflate, br")))
         (encoder (hpack:make-encoder))
         (http2-encoded (hpack:encode-header-list encoder http2-headers)))
    
    (format t "HTTP/1.1 header size: ~D bytes~%" (length http1-headers-text))
    (format t "HTTP/2 header size: ~D bytes~%" (length http2-encoded))
    (format t "Compression ratio: ~,1F%~%" 
            (* 100 (- 1 (/ (length http2-encoded) 
                          (length http1-headers-text))))))
  
  ;; Multiplexing benefit (simulated)
  (format t "~%Multiplexing benefit (100 requests):~%")
  (format t "HTTP/1.1 (sequential): ~D round trips~%" 100)
  (format t "HTTP/1.1 (6 connections): ~D round trips~%" (ceiling 100 6))
  (format t "HTTP/2 (multiplexed): 1 round trip~%"))

;;;; Main Benchmark Runner

(defun run-all-benchmarks ()
  "Run all HTTP/2 benchmarks"
  (format t "~%========================================~%")
  (format t "     HTTP/2 Performance Benchmarks~%")
  (format t "========================================~%")
  
  (benchmark-frame-parsing)
  (benchmark-hpack-compression)
  (benchmark-flow-control)
  (benchmark-stream-management)
  (benchmark-priority-tree)
  (benchmark-connection-operations)
  (benchmark-stream-multiplexing)
  (benchmark-http2-vs-http1)
  
  (format t "~%========================================~%")
  (format t "     Benchmarks Complete~%")
  (format t "========================================~%"))

;;;; Quick Performance Test

(defun quick-performance-test ()
  "Quick performance test for CI/CD"
  (format t "~%Running quick performance test...~%")
  
  (let ((results nil))
    ;; Test frame processing speed
    (push (cons "Frame processing"
                (benchmark "Frame test" 10000
                  (frames:make-data-frame 1 #(1 2 3 4 5))))
          results)
    
    ;; Test HPACK speed
    (let ((encoder (hpack:make-encoder)))
      (push (cons "HPACK encoding"
                  (benchmark "HPACK test" 1000
                    (hpack:encode-header-list 
                     encoder 
                     '((":method" . "GET") (":path" . "/")))))
            results))
    
    ;; Check performance thresholds
    (let ((slow-operations nil))
      (dolist (result results)
        (when (> (cdr result) 0.00001) ; 10 microseconds threshold
          (push (car result) slow-operations)))
      
      (if slow-operations
          (format t "~%Warning: Slow operations detected: ~{~A~^, ~}~%" 
                  slow-operations)
          (format t "~%All operations within performance thresholds.~%")))))