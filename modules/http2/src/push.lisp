;;;; HTTP/2 Server Push Implementation
;;;;
;;;; Implements server push functionality per RFC 7540 Section 8.2

(defpackage :epsilon.http2.push
  (:use :cl)
  (:local-nicknames
   (#:frames #:epsilon.http2.frames)
   (#:stream #:epsilon.http2.stream)
   (#:hpack #:epsilon.http2.hpack))
  (:export
   ;; Push promise management
   #:push-promise
   #:make-push-promise
   #:push-promise-p
   #:push-promise-stream-id
   #:push-promise-promised-stream-id
   #:push-promise-headers
   #:push-promise-state
   
   ;; Push operations
   #:can-push-p
   #:initiate-push
   #:send-push-promise
   #:reserve-push-stream
   #:fulfill-push-promise
   #:cancel-push-promise
   
   ;; Push cache
   #:push-cache
   #:make-push-cache
   #:cache-push-resource
   #:get-pushable-resources
   #:should-push-resource-p))

(in-package :epsilon.http2.push)

;;;; Push Promise Structure

(defstruct push-promise
  "Server push promise"
  (stream-id 0 :type (unsigned-byte 31))          ; Original request stream
  (promised-stream-id 0 :type (unsigned-byte 31)) ; New promised stream
  (headers nil :type list)                        ; Promised request headers
  (state :promised :type keyword)                 ; :promised, :sending, :sent, :cancelled
  (created-at (get-internal-real-time) :type integer)
  (response-headers nil :type list)               ; Response headers to send
  (response-data nil))                            ; Response data to send

;;;; Push Cache

(defstruct push-cache
  "Cache for managing pushable resources"
  (resources (make-hash-table :test 'equal))
  (push-history (make-hash-table :test 'equal))
  (max-age 3600 :type integer))

(defun cache-push-resource (cache path headers data &key dependencies)
  "Cache a resource that can be pushed"
  (setf (gethash path (push-cache-resources cache))
        (list :headers headers
              :data data
              :dependencies dependencies
              :cached-at (get-universal-time))))

(defun get-pushable-resources (cache request-path request-headers)
  "Get resources that should be pushed for a request"
  (let ((pushable nil)
        (now (get-universal-time)))
    
    ;; Check for resources associated with this path
    (maphash (lambda (path resource-info)
               (when (should-push-resource-p cache path request-headers now)
                 (push (cons path resource-info) pushable)))
             (push-cache-resources cache))
    
    ;; Sort by priority/dependency
    (sort pushable #'string< :key #'car)))

(defun should-push-resource-p (cache path request-headers now)
  "Determine if a resource should be pushed"
  ;; Check if recently pushed
  (let ((last-push (gethash path (push-cache-push-history cache))))
    (when (and last-push
               (< (- now last-push) (push-cache-max-age cache)))
      (return-from should-push-resource-p nil)))
  
  ;; Check client hints
  (let ((accept (assoc "accept" request-headers :test #'string-equal)))
    (when (and accept (search "no-push" (cdr accept)))
      (return-from should-push-resource-p nil)))
  
  ;; Default: push if not recently pushed
  t)

;;;; Push Operations

(defun can-push-p (connection)
  "Check if server push is enabled for connection"
  (let ((settings (slot-value connection 'remote-settings)))
    (not (zerop (or (cdr (assoc :enable-push settings)) 1)))))

(defun initiate-push (connection parent-stream-id request-headers)
  "Initiate a server push"
  (unless (can-push-p connection)
    (error "Server push is disabled by client"))
  
  ;; Reserve a new stream ID (must be even for server)
  (let* ((promised-stream-id (reserve-next-stream-id connection))
         (promise (make-push-promise
                  :stream-id parent-stream-id
                  :promised-stream-id promised-stream-id
                  :headers request-headers)))
    
    ;; Send PUSH_PROMISE frame
    (send-push-promise connection promise)
    
    ;; Reserve the stream
    (reserve-push-stream connection promised-stream-id)
    
    promise))

(defun reserve-next-stream-id (connection)
  "Reserve next available stream ID for push"
  (let ((next-id (slot-value connection 'next-stream-id)))
    ;; Ensure it's even (server-initiated)
    (when (oddp next-id)
      (incf next-id))
    (setf (slot-value connection 'next-stream-id) (+ next-id 2))
    next-id))

(defun send-push-promise (connection promise)
  "Send PUSH_PROMISE frame"
  (let* ((encoder (slot-value connection 'hpack-encoder))
         (encoded-headers (hpack:encode-header-list encoder 
                                                            (push-promise-headers promise)))
         (frame (make-push-promise-frame
                (push-promise-stream-id promise)
                (push-promise-promised-stream-id promise)
                encoded-headers)))
    
    ;; Send the frame
    (send-frame connection frame)
    
    ;; Update promise state
    (setf (push-promise-state promise) :sent)))

(defun make-push-promise-frame (stream-id promised-stream-id headers-block)
  "Create a PUSH_PROMISE frame"
  (let* ((promised-id-bytes 4)
         (payload-size (+ promised-id-bytes (length headers-block)))
         (payload (make-array payload-size :element-type '(unsigned-byte 8))))
    
    ;; Promised Stream ID (31 bits with reserved bit)
    (setf (aref payload 0) (logand #x7f (ash promised-stream-id -24)))
    (setf (aref payload 1) (logand #xff (ash promised-stream-id -16)))
    (setf (aref payload 2) (logand #xff (ash promised-stream-id -8)))
    (setf (aref payload 3) (logand #xff promised-stream-id))
    
    ;; Header block fragment
    (replace payload headers-block :start1 4)
    
    (frames:make-http2-frame
     :type frames:+frame-push-promise+
     :flags frames:+flag-end-headers+ ; Assuming complete headers
     :stream-id stream-id
     :length payload-size
     :payload payload)))

(defun reserve-push-stream (connection promised-stream-id)
  "Reserve a stream for server push"
  (let* ((initial-window-size (cdr (assoc :initial-window-size 
                                         (slot-value connection 'local-settings))))
         (new-stream (stream:initialize-stream promised-stream-id connection
                                              :initial-window-size 
                                              (or initial-window-size 65535))))
    
    ;; Set stream to reserved state
    (stream:reserve-stream new-stream nil) ; nil = reserved by server
    
    ;; Store in connection's stream table
    (setf (gethash promised-stream-id (slot-value connection 'streams))
          new-stream)
    
    new-stream))

(defun fulfill-push-promise (connection promise response-headers response-data)
  "Fulfill a push promise by sending the response"
  (let ((stream (gethash (push-promise-promised-stream-id promise)
                        (slot-value connection 'streams))))
    
    (unless stream
      (error "Push promise stream not found"))
    
    ;; Send response headers
    (send-headers connection stream response-headers :end-stream (null response-data))
    
    ;; Send response data if present
    (when response-data
      (send-data connection stream response-data :end-stream t))
    
    ;; Update promise state
    (setf (push-promise-state promise) :fulfilled)))

(defun cancel-push-promise (connection promise error-code)
  "Cancel a push promise"
  (let ((stream-id (push-promise-promised-stream-id promise)))
    
    ;; Send RST_STREAM for the promised stream
    (send-rst-stream connection stream-id error-code)
    
    ;; Remove from stream table
    (remhash stream-id (slot-value connection 'streams))
    
    ;; Update promise state
    (setf (push-promise-state promise) :cancelled)))

;;;; Helper Functions

(defun send-frame (connection frame)
  "Send a frame on the connection"
  ;; This would integrate with the connection's frame sending mechanism
  (funcall (slot-value connection 'send-frame-fn) frame))

(defun send-headers (connection stream headers &key end-stream)
  "Send headers on a stream"
  (let* ((encoder (slot-value connection 'hpack-encoder))
         (encoded (hpack:encode-header-list encoder headers))
         (frame (frames:make-headers-frame
                (stream:stream-id stream)
                encoded
                :end-stream end-stream
                :end-headers t)))
    (send-frame connection frame)))

(defun send-data (connection stream data &key end-stream)
  "Send data on a stream"
  (let ((frame (frames:make-data-frame
               (stream:stream-id stream)
               data
               :end-stream end-stream)))
    (send-frame connection frame)))

(defun send-rst-stream (connection stream-id error-code)
  "Send RST_STREAM frame"
  (let ((frame (frames:make-rst-stream-frame stream-id error-code)))
    (send-frame connection frame)))

;;;; Push Strategy

(defun analyze-request-for-push (request-path request-headers)
  "Analyze a request to determine what resources to push"
  (let ((pushable nil))
    
    ;; Common patterns for web applications
    (cond
      ;; Index page - push common assets
      ((or (string= request-path "/")
           (string= request-path "/index.html"))
       (setf pushable '(("/css/style.css" . "text/css")
                       ("/js/app.js" . "application/javascript")
                       ("/img/logo.png" . "image/png"))))
      
      ;; HTML pages - push associated resources
      ((search ".html" request-path)
       (let ((base (subseq request-path 0 (search ".html" request-path))))
         (setf pushable (list (cons (concatenate 'string base ".css") "text/css")
                            (cons (concatenate 'string base ".js") "application/javascript")))))
      
      ;; API endpoints - no push
      ((or (search "/api/" request-path)
           (search ".json" request-path))
       nil))
    
    ;; Filter based on Accept header
    (let ((accept (cdr (assoc "accept" request-headers :test #'string-equal))))
      (when accept
        (setf pushable
              (remove-if-not (lambda (resource)
                              (search (cdr resource) accept))
                            pushable))))
    
    pushable))

(defun create-push-headers (original-headers path)
  "Create headers for a pushed resource"
  (let ((push-headers (copy-list original-headers)))
    ;; Update :path pseudo-header
    (setf push-headers
          (cons (cons ":path" path)
                (remove ":path" push-headers :key #'car :test #'string=)))
    
    ;; Ensure required pseudo-headers are present
    (unless (assoc ":method" push-headers :test #'string=)
      (push (cons ":method" "GET") push-headers))
    (unless (assoc ":scheme" push-headers :test #'string=)
      (push (cons ":scheme" "https") push-headers))
    (unless (assoc ":authority" push-headers :test #'string=)
      (when-let ((host (assoc "host" original-headers :test #'string-equal)))
        (push (cons ":authority" (cdr host)) push-headers)))
    
    push-headers))