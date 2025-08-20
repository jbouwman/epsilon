;;;; HTTP Streaming Support
;;;;
;;;; Modern streaming HTTP implementation with backpressure support.
;;;; Handles chunked transfer encoding, server-sent events, and large file transfers.
;;;; Inspired by Rust's hyper streams and Go's io.Reader/Writer patterns.

(defpackage :epsilon.http.streaming
  (:use :cl)
  (:local-nicknames
   (#:channel #:epsilon.channel)
   (#:pool #:epsilon.http.pool)
   (#:client #:epsilon.http.client)
   (#:net #:epsilon.net)
   (#:str #:epsilon.string)
   (#:map #:epsilon.map))
  (:export
   ;; Streaming requests
   #:streaming-request
   #:stream-request-body
   #:finish-request-stream
   
   ;; Streaming responses
   #:streaming-response
   #:response-stream
   #:read-response-chunk
   #:response-stream-p
   #:close-response-stream
   
   ;; Chunked transfer encoding
   #:chunked-stream
   #:write-chunk
   #:finish-chunked-stream
   #:read-chunked-response
   
   ;; Server-sent events
   #:sse-stream
   #:sse-event
   #:make-sse-event
   #:read-sse-event
   #:write-sse-event
   
   ;; Large file handling
   #:stream-file-upload
   #:stream-file-download
   #:download-progress
   
   ;; Stream utilities
   #:copy-stream
   #:tee-stream
   #:limit-stream
   #:decompress-stream
   
   ;; Configuration
   #:streaming-config
   #:make-streaming-config
   #:*default-streaming-config*))

(in-package :epsilon.http.streaming)

;;;; Configuration

(defstruct streaming-config
  "Configuration for HTTP streaming operations"
  (chunk-size 8192 :type integer) ; Default chunk size for streaming
  (buffer-size 65536 :type integer) ; Buffer size for large transfers
  (max-chunk-size (* 1024 1024) :type integer) ; 1MB max chunk
  (timeout 30 :type integer) ; Stream timeout in seconds
  (enable-compression t :type boolean)
  (progress-callback nil :type (or null function))) ; Progress reporting

(defparameter *default-streaming-config* (make-streaming-config)
  "Default streaming configuration")

;;;; Streaming Request Support

(defstruct streaming-request
  "HTTP request with streaming body support"
  (connection nil)
  (method "POST" :type string)
  (path "/" :type string)
  (headers (map:make-map) :type map:map)
  (body-stream nil) ; Channel for streaming body data
  (finished-p nil :type boolean)
  (config (make-streaming-config) :type streaming-config))

(defun create-streaming-request (http-pool method url &key headers config)
  "Create a streaming HTTP request"
  (multiple-value-bind (scheme host port path query)
      (client::parse-url url)
    
    (let* ((ssl-p (string= scheme "https"))
           (connection (pool:get-connection http-pool host port :ssl-p ssl-p))
           (body-channel (channel:create-channel :capacity 10))
           (final-headers (map:merge (map:make-map 
                                     "Host" host
                                     "Transfer-Encoding" "chunked"
                                     "User-Agent" "epsilon.http/2.0")
                                    (or headers (map:make-map))))
           (config (or config *default-streaming-config*)))
      
      ;; Send request headers immediately
      (send-request-headers connection method path final-headers query)
      
      (make-streaming-request
       :connection connection
       :method method
       :path path
       :headers final-headers
       :body-stream body-channel
       :config config))))

(defun send-request-headers (connection method path headers query)
  "Send HTTP request headers for streaming request"
  (let ((stream (pool:http-connection-stream connection)))
    ;; Request line
    (format stream "~A ~A~@[?~A~] HTTP/1.1~C~C" 
            method path query #\Return #\Linefeed)
    
    ;; Headers
    (map:each (lambda (name value)
                (format stream "~A: ~A~C~C" name value #\Return #\Linefeed))
              headers)
    
    ;; End headers
    (format stream "~C~C" #\Return #\Linefeed)
    (force-output stream)))

(defun stream-request-body (streaming-req data)
  "Stream data as part of request body"
  (when (streaming-request-finished-p streaming-req)
    (error "Streaming request already finished"))
  
  (channel:send (streaming-request-body-stream streaming-req) data))

(defun finish-request-stream (streaming-req)
  "Finish streaming request and read response"
  (unless (streaming-request-finished-p streaming-req)
    (setf (streaming-request-finished-p streaming-req) t)
    
    ;; Send all queued chunks
    (let ((stream (pool:http-connection-stream (streaming-request-connection streaming-req))))
      (loop
       (multiple-value-bind (chunk received-p) 
           (channel:try-receive (streaming-request-body-stream streaming-req))
         (unless received-p (return))
         (write-http-chunk stream chunk)))
      
      ;; Send final chunk
      (write-http-chunk stream nil) ; nil = end of chunks
      (force-output stream))
    
    ;; Close body channel
    (channel:close-channel (streaming-request-body-stream streaming-req))
    
    ;; Read response
    (client::read-response-from-connection (streaming-request-connection streaming-req))))

;;;; Streaming Response Support

(defstruct streaming-response
  "HTTP response with streaming body support"
  (status 200 :type integer)
  (headers (map:make-map) :type map:map)
  (body-stream nil) ; Stream for response body
  (connection nil)
  (chunked-p nil :type boolean)
  (content-length nil :type (or null integer))
  (bytes-read 0 :type integer)
  (finished-p nil :type boolean)
  (config (make-streaming-config) :type streaming-config))

(defun create-streaming-response (connection response-headers)
  "Create streaming response from connection and headers"
  (let* ((transfer-encoding (map:get response-headers "Transfer-Encoding"))
         (content-length-str (map:get response-headers "Content-Length"))
         (chunked-p (and transfer-encoding 
                        (string-equal transfer-encoding "chunked")))
         (content-length (when content-length-str
                          (parse-integer content-length-str :junk-allowed t)))
         (body-stream (create-response-body-stream connection chunked-p content-length)))
    
    (make-streaming-response
     :status (parse-http-status connection)
     :headers response-headers
     :body-stream body-stream
     :connection connection
     :chunked-p chunked-p
     :content-length content-length)))

(defun create-response-body-stream (connection chunked-p content-length)
  "Create a stream for response body based on encoding"
  (cond
    (chunked-p
     (channel:make-data-stream
      :next-fn (lambda () (read-next-chunk connection))
      :close-fn (lambda () (close-connection connection))))
    
    (content-length
     (let ((bytes-read 0))
       (channel:make-data-stream
        :next-fn (lambda ()
                   (if (< bytes-read content-length)
                       (let* ((remaining (- content-length bytes-read))
                              (to-read (min remaining 8192))
                              (buffer (make-string to-read))
                              (actually-read (read-sequence buffer 
                                                          (pool:http-connection-stream connection))))
                         (incf bytes-read actually-read)
                         (values (subseq buffer 0 actually-read) 
                                 (< bytes-read content-length)))
                       (values nil nil)))
        :close-fn (lambda () (close-connection connection)))))
    
    ;; No content-length, read until connection closes
    (t
     (channel:make-data-stream
      :next-fn (lambda () (read-until-close connection))
      :close-fn (lambda () (close-connection connection))))))

(defun read-response-chunk (streaming-resp)
  "Read next chunk from streaming response"
  (when (streaming-response-finished-p streaming-resp)
    (return-from read-response-chunk (values nil nil)))
  
  (multiple-value-bind (chunk more-p) 
      (channel:stream-next (streaming-response-body-stream streaming-resp))
    
    (when chunk
      (incf (streaming-response-bytes-read streaming-resp) (length chunk)))
    
    (unless more-p
      (setf (streaming-response-finished-p streaming-resp) t))
    
    (values chunk more-p)))

(defun close-response-stream (streaming-resp)
  "Close streaming response and cleanup resources"
  (unless (streaming-response-finished-p streaming-resp)
    (setf (streaming-response-finished-p streaming-resp) t)
    (channel:stream-close (streaming-response-body-stream streaming-resp))))

;;;; Chunked Transfer Encoding

(defun write-http-chunk (stream data)
  "Write a chunk in HTTP chunked encoding format"
  (if data
      (progn
        ;; Chunk size in hex
        (format stream "~X~C~C" (length data) #\Return #\Linefeed)
        ;; Chunk data
        (write-string data stream)
        ;; Chunk terminator
        (format stream "~C~C" #\Return #\Linefeed))
      ;; Final chunk (size 0)
      (format stream "0~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed)))

(defun read-next-chunk (connection)
  "Read next chunk from chunked HTTP response"
  (let ((stream (pool:http-connection-stream connection)))
    (handler-case
        (let ((size-line (read-line stream)))
          ;; Parse chunk size (hex)
          (let ((chunk-size (parse-integer size-line :radix 16 :junk-allowed t)))
            (if (zerop chunk-size)
                ;; End of chunks
                (progn
                  ;; Read final CRLF
                  (read-line stream)
                  (values nil nil))
                ;; Read chunk data
                (let ((buffer (make-string chunk-size)))
                  (read-sequence buffer stream)
                  ;; Read trailing CRLF
                  (read-line stream)
                  (values buffer t)))))
      (end-of-file () (values nil nil)))))

;;;; Server-Sent Events (SSE)

(defstruct sse-event
  "Server-sent event"
  (type "message" :type string)
  (data "" :type string)
  (id nil :type (or null string))
  (retry nil :type (or null integer))) ; milliseconds

(defun write-sse-event (stream event)
  "Write SSE event to stream"
  (when (sse-event-type event)
    (format stream "event: ~A~C" (sse-event-type event) #\Linefeed))
  
  (when (sse-event-id event)
    (format stream "id: ~A~C" (sse-event-id event) #\Linefeed))
  
  (when (sse-event-retry event)
    (format stream "retry: ~D~C" (sse-event-retry event) #\Linefeed))
  
  ;; Data can be multi-line
  (dolist (line (str:split #\Newline (sse-event-data event)))
    (format stream "data: ~A~C" line #\Linefeed))
  
  ;; End event
  (format stream "~C" #\Linefeed)
  (force-output stream))

(defun read-sse-event (stream)
  "Read SSE event from stream"
  (let ((event (make-sse-event))
        (data-lines '())
        (line nil))
    
    (loop while (setf line (read-line stream nil nil))
          do (cond
               ;; Empty line = end of event
               ((string= line "")
                (return))
               
               ;; Comment line
               ((char= (char line 0) #\:)
                ;; Skip comments
                )
               
               ;; Field line
               (t
                (let ((colon-pos (position #\: line)))
                  (if colon-pos
                      (let ((field (subseq line 0 colon-pos))
                            (value (string-trim " " (subseq line (1+ colon-pos)))))
                        (cond
                          ((string= field "event")
                           (setf (sse-event-type event) value))
                          ((string= field "data")
                           (push value data-lines))
                          ((string= field "id")
                           (setf (sse-event-id event) value))
                          ((string= field "retry")
                           (setf (sse-event-retry event) 
                                 (parse-integer value :junk-allowed t)))))
                      ;; Field with no value
                      (when (string= line "data")
                        (push "" data-lines)))))))
    
    (when data-lines
      (setf (sse-event-data event) 
            (str:join #\Newline (nreverse data-lines))))
    
    (if (or (sse-event-data event) 
            (sse-event-type event) 
            (sse-event-id event))
        event
        nil))) ; No valid event

;;;; Large File Handling

(defstruct download-progress
  "Progress information for file downloads"
  (bytes-downloaded 0 :type integer)
  (total-bytes nil :type (or null integer))
  (percentage nil :type (or null float))
  (start-time 0 :type integer)
  (current-time 0 :type integer)
  (rate 0.0 :type float)) ; bytes per second

(defun stream-file-download (http-pool url output-stream &key config progress-callback)
  "Download large file with streaming and progress reporting"
  (let ((config (or config *default-streaming-config*))
        (start-time (get-universal-time)))
    
    (multiple-value-bind (scheme host port path query)
        (client::parse-url url)
      
      (let ((ssl-p (string= scheme "https")))
        (pool:with-http-connection (conn http-pool host port :ssl-p ssl-p)
          
          ;; Send request
          (client::send-request-on-connection conn "GET" path nil nil query)
          
          ;; Read response headers
          (let* ((response (client::read-response-headers (pool:http-connection-stream conn)))
                 (status (getf response :status))
                 (headers (getf response :headers))
                 (content-length-str (map:get headers "Content-Length"))
                 (content-length (when content-length-str
                                  (parse-integer content-length-str :junk-allowed t))))
            
            (unless (= status 200)
              (error "HTTP ~D downloading ~A" status url))
            
            ;; Stream response body
            (let ((buffer (make-array (streaming-config-buffer-size config) 
                                     :element-type 'character))
                  (bytes-downloaded 0)
                  (stream (pool:http-connection-stream conn)))
              
              (loop
               (let ((bytes-read (read-sequence buffer stream)))
                 (when (zerop bytes-read) (return))
                 
                 ;; Write to output
                 (write-sequence buffer output-stream :end bytes-read)
                 (incf bytes-downloaded bytes-read)
                 
                 ;; Report progress
                 (when progress-callback
                   (let* ((current-time (get-universal-time))
                          (elapsed (max 1 (- current-time start-time)))
                          (rate (/ bytes-downloaded elapsed))
                          (progress (make-download-progress
                                    :bytes-downloaded bytes-downloaded
                                    :total-bytes content-length
                                    :percentage (when content-length
                                                 (* 100.0 (/ bytes-downloaded content-length)))
                                    :start-time start-time
                                    :current-time current-time
                                    :rate rate)))
                     (funcall progress-callback progress)))))
              
              bytes-downloaded)))))))

(defun stream-file-upload (http-pool method url input-stream &key headers config progress-callback)
  "Upload large file with streaming and progress reporting"
  (let ((config (or config *default-streaming-config*))
        (start-time (get-universal-time)))
    
    (multiple-value-bind (scheme host port path query)
        (client::parse-url url)
      
      (let* ((ssl-p (string= scheme "https"))
             (upload-headers (map:merge (map:make-map "Transfer-Encoding" "chunked")
                                       (or headers (map:make-map)))))
        
        (pool:with-http-connection (conn http-pool host port :ssl-p ssl-p)
          
          ;; Send request headers
          (send-request-headers conn method path upload-headers query)
          
          ;; Stream request body
          (let ((buffer (make-array (streaming-config-chunk-size config)
                                   :element-type 'character))
                (bytes-uploaded 0)
                (stream (pool:http-connection-stream conn)))
            
            (loop
             (let ((bytes-read (read-sequence buffer input-stream)))
               (when (zerop bytes-read) 
                 ;; Send final chunk
                 (write-http-chunk stream nil)
                 (return))
               
               ;; Send chunk
               (write-http-chunk stream (subseq buffer 0 bytes-read))
               (incf bytes-uploaded bytes-read)
               
               ;; Report progress
               (when progress-callback
                 (let* ((current-time (get-universal-time))
                        (elapsed (max 1 (- current-time start-time)))
                        (rate (/ bytes-uploaded elapsed))
                        (progress (make-download-progress
                                  :bytes-downloaded bytes-uploaded
                                  :start-time start-time
                                  :current-time current-time
                                  :rate rate)))
                   (funcall progress-callback progress)))))
            
            ;; Read response
            (let ((response (client::read-response-from-connection conn)))
              (values response bytes-uploaded))))))))

;;;; Stream Utilities

(defun copy-stream (input-stream output-stream &key (buffer-size 8192))
  "Copy data from input stream to output stream"
  (let ((buffer (make-array buffer-size :element-type 'character))
        (total-bytes 0))
    (loop
     (let ((bytes-read (read-sequence buffer input-stream)))
       (when (zerop bytes-read) (return total-bytes))
       (write-sequence buffer output-stream :end bytes-read)
       (incf total-bytes bytes-read)))))

(defun tee-stream (input-stream output-streams)
  "Create a stream that writes to multiple outputs (like Unix tee)"
  (channel:make-data-stream
   :next-fn (lambda ()
              (multiple-value-bind (data more-p) (channel:stream-next input-stream)
                (when data
                  (dolist (out-stream output-streams)
                    (write-string data out-stream)
                    (force-output out-stream)))
                (values data more-p)))
   :close-fn (lambda ()
               (channel:stream-close input-stream)
               (dolist (out-stream output-streams)
                 (close out-stream)))))

;;;; Helper Functions

(defun parse-http-status (connection)
  "Parse HTTP status from response line"
  (let* ((stream (pool:http-connection-stream connection))
         (status-line (read-line stream)))
    (when (and status-line (>= (length status-line) 12))
      (parse-integer (subseq status-line 9 12) :junk-allowed t))))

(defun read-until-close (connection)
  "Read data until connection closes"
  (let ((stream (pool:http-connection-stream connection))
        (buffer (make-string 8192)))
    (handler-case
        (let ((bytes-read (read-sequence buffer stream)))
          (if (zerop bytes-read)
              (values nil nil)
              (values (subseq buffer 0 bytes-read) t)))
      (end-of-file () (values nil nil)))))

(defun close-connection (connection)
  "Close HTTP connection"
  (when connection
    (pool:destroy-http-connection connection)))