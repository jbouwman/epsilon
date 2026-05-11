;;;; HTTP/2 Protocol Implementation
;;;;
;;;; Main module for HTTP/2 protocol support
;;;; Part of the unified epsilon.http module
;;;;
;;;; This file is the entry point for HTTP/2 functionality under epsilon.http.h2

(defpackage :epsilon.http.h2
  (:use :cl)
  (:import
   (epsilon.http.h2.hpack hpack)
   (epsilon.http.h2.frames frames)
   (epsilon.http.h2.flow-control flow)
   (epsilon.http.h2.stream stream)
   (epsilon.http.h2.error error)
   (epsilon.http.h2.server h2-server)
   (epsilon.string str)
   (epsilon.net net)
   (epsilon.crypto crypto))
  (:import-from #:epsilon.http.h2.frames
   ;; Frame creation
   #:make-ping-frame
   #:make-settings-frame
   #:make-window-update-frame
   #:make-data-frame
   #:make-goaway-frame
   #:make-headers-frame
   #:make-http2-frame
   #:make-rst-stream-frame
   #:write-frame
   #:read-frame
   #:valid-frame-p
   ;; Frame accessors
   #:http2-frame-type
   #:http2-frame-flags
   #:http2-frame-length
   #:http2-frame-payload
   #:http2-frame-stream-id
   ;; Frame type constants
   #:+frame-data+
   #:+frame-headers+
   #:+frame-priority+
   #:+frame-rst-stream+
   #:+frame-settings+
   #:+frame-push-promise+
   #:+frame-ping+
   #:+frame-goaway+
   #:+frame-window-update+
   #:+frame-continuation+
   ;; Flag constants
   #:+flag-end-stream+
   #:+flag-end-headers+
   #:+flag-padded+
   #:+flag-priority+
   #:+flag-ack+
   ;; Error constants
   #:+error-no-error+
   #:+error-protocol-error+
   #:+error-internal-error+
   #:+error-flow-control-error+
   #:+error-settings-timeout+
   #:+error-stream-closed+
   #:+error-frame-size-error+
   #:+error-refused-stream+
   #:+error-cancel+
   #:+error-compression-error+
   #:+error-connect-error+
   #:+error-enhance-your-calm+
   #:+error-inadequate-security+
   #:+error-http-1-1-required+
   ;; Header decoding
   #:decode-headers-from-payload)
  (:import-from #:epsilon.http.h2.stream
   #:http2-stream-p
   #:stream-id
   #:stream-connection)
  (:export
   ;; Connection management
   #:make-http2-connection
   #:http2-connection-p
   #:http2-connection-flow-controller
   #:http2-connection-streams
   #:connection-tls-connection
   #:connection-send-frame
   #:connection-receive-frame
   #:connection-close
   #:connection-active-streams-count

   ;; Stream management
   #:create-stream
   #:stream-send-headers
   #:stream-send-data
   #:stream-receive-headers
   #:stream-receive-data
   #:stream-close

   ;; Client functions
   #:http2-request
   #:http2-get
   #:http2-post
   #:read-http2-response

   ;; Server functions
   #:start-server
   #:stop-server
   #:swap-tls-context!
   #:http2-server
   #:handle-http2-connection

   ;; Protocol negotiation
   #:upgrade-to-http2
   #:is-http2-connection

   ;; Settings
   #:make-settings
   #:apply-settings
   #:+default-settings+

   ;; Frame creation (re-exported from frames)
   #:make-ping-frame
   #:make-settings-frame
   #:make-window-update-frame
   #:make-data-frame
   #:make-goaway-frame
   #:make-headers-frame
   #:make-http2-frame
   #:make-rst-stream-frame
   #:write-frame
   #:read-frame
   #:valid-frame-p

   ;; Frame accessors (re-exported from frames)
   #:http2-frame-type
   #:http2-frame-flags
   #:http2-frame-length
   #:http2-frame-payload
   #:http2-frame-stream-id

   ;; Frame type constants (re-exported from frames)
   #:+frame-data+
   #:+frame-headers+
   #:+frame-priority+
   #:+frame-rst-stream+
   #:+frame-settings+
   #:+frame-push-promise+
   #:+frame-ping+
   #:+frame-goaway+
   #:+frame-window-update+
   #:+frame-continuation+

   ;; Flag constants (re-exported from frames)
   #:+flag-end-stream+
   #:+flag-end-headers+
   #:+flag-padded+
   #:+flag-priority+
   #:+flag-ack+

   ;; Error constants (re-exported from frames)
   #:+error-no-error+
   #:+error-protocol-error+
   #:+error-internal-error+
   #:+error-flow-control-error+
   #:+error-settings-timeout+
   #:+error-stream-closed+
   #:+error-frame-size-error+
   #:+error-refused-stream+
   #:+error-cancel+
   #:+error-compression-error+
   #:+error-connect-error+
   #:+error-enhance-your-calm+
   #:+error-inadequate-security+
   #:+error-http-1-1-required+

   ;; Header decoding (re-exported from frames)
   #:decode-headers-from-payload

   ;; Frame serialization
   #:serialize-frame
   #:deserialize-frame

   ;; Wrapper frame accessors
   #:frame-flag-set-p
   #:frame-type
   #:frame-stream-id
   #:frame-payload

   ;; Stream property accessors
   #:http2-stream-headers
   #:http2-stream-data
   #:http2-stream-state
   #:http2-stream-connection
   #:http2-stream-id

   ;; Stream functions (re-exported from stream)
   #:http2-stream-p
   #:stream-id
   #:stream-connection))

;;;; Constants

(defparameter +http2-preface+
  (format nil "PRI * HTTP/2.0~C~C~C~CSM~C~C~C~C"
          #\Return #\Newline #\Return #\Newline
          #\Return #\Newline #\Return #\Newline)
  "HTTP/2 connection preface")

(defparameter +default-settings+
  '((:header-table-size . 4096)
    (:enable-push . 1)
    (:max-concurrent-streams . 100)
    (:initial-window-size . 65535)
    (:max-frame-size . 16384)
    (:max-header-list-size . 8192))
  "Default HTTP/2 settings")

;;;; Connection Management

(defclass http2-connection ()
  ((socket :initarg :socket :accessor connection-socket :accessor http2-connection-socket)
   (tls-connection :initarg :tls-connection :accessor connection-tls-connection)
   (client-p :initarg :client-p :accessor connection-client-p :accessor http2-connection-client-p)
   (streams :initform (make-hash-table) :accessor connection-streams :accessor http2-connection-streams)
   (next-stream-id :initform 1 :accessor connection-next-stream-id)
   (local-settings :initform +default-settings+ :accessor connection-local-settings :accessor http2-connection-local-settings)
   (remote-settings :initform +default-settings+ :accessor connection-remote-settings)
   (hpack-encoder :accessor connection-hpack-encoder :accessor http2-connection-hpack-encoder)
   (hpack-decoder :accessor connection-hpack-decoder :accessor http2-connection-hpack-decoder)
   (flow-controller :accessor http2-connection-flow-controller :initform nil)
   (last-stream-id :initform 0 :accessor http2-connection-last-stream-id)
   (goaway-sent-p :initform nil :accessor connection-goaway-sent-p)
   (goaway-received-p :initform nil :accessor connection-goaway-received-p)
   (max-concurrent-streams :initform 100 :accessor connection-max-concurrent-streams)
   (active-streams-count :initform 0 :accessor connection-active-streams-count)))

(defun make-http2-connection (socket &key tls-connection client-p)
  "Create a new HTTP/2 connection"
  (let ((conn (make-instance 'http2-connection
                             :socket socket
                             :tls-connection tls-connection
                             :client-p client-p)))
    ;; Initialize flow control
    (setf (http2-connection-flow-controller conn)
          (flow:make-connection-flow-controller))

    ;; Initialize HPACK encoder/decoder
    (setf (connection-hpack-encoder conn) (hpack:make-encoder))
    (setf (connection-hpack-decoder conn) (hpack:make-decoder))

    ;; Set initial stream ID based on role
    (setf (connection-next-stream-id conn)
          (if client-p 1 2))

    ;; Send connection preface if client and socket exists
    (when (and client-p socket)
      (send-connection-preface conn))

    ;; Send initial SETTINGS frame if socket exists
    (when socket
      (send-settings-frame conn +default-settings+))

    conn))

(defun send-connection-preface (connection)
  "Send HTTP/2 connection preface (client only)"
  (let ((preface-bytes (string-to-bytes +http2-preface+)))
    (if (connection-tls-connection connection)
        (crypto:tls-write (connection-tls-connection connection) preface-bytes)
      (epsilon.net:tcp-write (connection-socket connection) preface-bytes))))

(defun setting-key-to-id (key)
  "Convert setting keyword to numeric identifier"
  (case key
    (:header-table-size frames:+settings-header-table-size+)
    (:enable-push frames:+settings-enable-push+)
    (:max-concurrent-streams frames:+settings-max-concurrent-streams+)
    (:initial-window-size frames:+settings-initial-window-size+)
    (:max-frame-size frames:+settings-max-frame-size+)
    (:max-header-list-size frames:+settings-max-header-list-size+)
    (t key)))

(defun convert-settings-to-numeric (settings)
  "Convert settings from keyword form to numeric form"
  (mapcar (lambda (setting)
            (cons (setting-key-to-id (car setting))
                  (cdr setting)))
          settings))

(defun send-settings-frame (connection settings)
  "Send SETTINGS frame"
  (let ((numeric-settings (convert-settings-to-numeric settings)))
    (let ((frame (make-settings-frame :initial-settings numeric-settings)))
      (connection-send-frame connection frame))))

(defun connection-send-frame (connection frame)
  "Send a frame over the connection"
  (let ((frame-bytes (serialize-frame frame)))
    (if (connection-tls-connection connection)
        (crypto:tls-write (connection-tls-connection connection) frame-bytes)
      (epsilon.net:tcp-write (connection-socket connection) frame-bytes))))

(defun connection-receive-frame (connection)
  "Receive and parse a frame from the connection"
  (let ((header-bytes (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Read frame header (9 bytes)
    (if (connection-tls-connection connection)
        (crypto:tls-read (connection-tls-connection connection) header-bytes :start 0 :end 9)
      (epsilon.net:tcp-read (connection-socket connection) header-bytes :start 0 :end 9))

    ;; Parse header to get payload length
    (let* ((payload-length (parse-frame-header header-bytes))
           (payload-bytes (when (> payload-length 0)
                           (make-array payload-length :element-type '(unsigned-byte 8)))))

      ;; Read payload if present
      (when payload-bytes
        (if (connection-tls-connection connection)
            (crypto:tls-read (connection-tls-connection connection) payload-bytes :start 0 :end payload-length)
          (epsilon.net:tcp-read (connection-socket connection) payload-bytes :start 0 :end payload-length)))

      ;; Parse complete frame
      (parse-frame header-bytes payload-bytes))))

(defun connection-close (connection)
  "Close HTTP/2 connection"
  ;; Send GOAWAY frame
  (let ((goaway-frame (frames:make-goaway-frame
                       (1- (connection-next-stream-id connection))
                       frames:+error-no-error+
                       "Connection closing")))
    (connection-send-frame connection goaway-frame))

  ;; Close underlying connection
  (when (connection-tls-connection connection)
    (crypto:tls-close (connection-tls-connection connection)))
  (when (connection-socket connection)
    (epsilon.net:tcp-shutdown (connection-socket connection) :how :both)))

;;;; Stream Management

(defun create-stream (connection)
  "Create a new stream on the connection"
  (let* ((stream-id (connection-next-stream-id connection))
         (initial-window-size (cdr (assoc :initial-window-size
                                         (connection-local-settings connection))))
         (stream (stream:initialize-stream stream-id connection
                                          :initial-window-size (or initial-window-size 65535))))
    ;; Increment next stream ID (odd for client, even for server)
    (incf (connection-next-stream-id connection) 2)
    ;; Store stream
    (setf (gethash stream-id (connection-streams connection)) stream)
    ;; Track active streams
    (incf (connection-active-streams-count connection))
    stream))

(defun stream-send-headers (stream headers &key end-stream)
  "Send headers on a stream"
  (let* ((connection (stream:stream-connection stream))
         (encoded-headers (hpack:encode-header-list
                          (connection-hpack-encoder connection)
                          headers))
         (frame (frames:make-headers-frame
                (stream:stream-id stream)
                encoded-headers
                :end-stream end-stream
                :end-headers t)))
    ;; Update stream state
    (stream:transition-stream-state stream frames:+frame-headers+
                                   (if end-stream frames:+flag-end-stream+ 0))
    (connection-send-frame connection frame)))

(defun stream-send-data (stream data &key end-stream)
  "Send data on a stream"
  (let* ((connection (stream:stream-connection stream))
         (stream-id (stream:stream-id stream))
         (data-bytes (if (stringp data)
                        (str:string-to-octets data)
                        data))
         (data-length (length data-bytes)))

    ;; Check flow control
    (unless (flow:can-send-data-p
             (http2-connection-flow-controller connection)
             (stream:stream-flow-controller stream)
             data-length)
      (error "Insufficient flow control window for ~D bytes" data-length))

    ;; Consume window
    (flow:consume-send-window (http2-connection-flow-controller connection) data-length)
    (flow:consume-send-window (stream:stream-flow-controller stream) data-length)

    ;; Create and send frame
    (let ((frame (frames:make-data-frame
                  stream-id
                  data-bytes
                  :end-stream end-stream)))
      ;; Update stream state
      (stream:transition-stream-state stream frames:+frame-data+
                                     (if end-stream frames:+flag-end-stream+ 0))
      (connection-send-frame connection frame))))

;;;; Client Functions

(defun parse-url (url)
  "Simple URL parser"
  (let* ((scheme-end (search "://" url))
         (scheme (if scheme-end (subseq url 0 scheme-end) "https"))
         (url-after-scheme (if scheme-end (subseq url (+ scheme-end 3)) url))
         (port-pos (position #\: url-after-scheme))
         (path-pos (position #\/ url-after-scheme))
         (host (if port-pos
                   (subseq url-after-scheme 0 port-pos)
                   (if path-pos
                       (subseq url-after-scheme 0 path-pos)
                       url-after-scheme)))
         (port (if port-pos
                   (parse-integer (subseq url-after-scheme (1+ port-pos)
                                         (or path-pos (length url-after-scheme))))
                   (if (string= scheme "https") 443 80)))
         (path (if path-pos
                   (subseq url-after-scheme path-pos)
                   "/")))
    (values scheme host port path nil)))

(defun http2-request (url &key method headers body)
  "Make an HTTP/2 request"
  ;; Parse URL
  (multiple-value-bind (scheme host port path query)
      (parse-url url)
    (declare (ignore scheme query))

    ;; Create TLS connection with ALPN
    (let* ((socket (epsilon.net:tcp-connect (epsilon.net:make-socket-address host port)))
           (tls-ctx (crypto:make-client-context))
           (_ (crypto:context-set-alpn-protocols tls-ctx '("h2")))
           (tls-conn (crypto:tls-connect socket tls-ctx
                                         :hostname host
                                         :alpn-protocols '("h2")))
           (h2-conn (make-http2-connection socket
                                           :tls-connection tls-conn
                                           :client-p t)))
      (declare (ignore _))

      ;; Verify we negotiated HTTP/2
      (let ((protocol (crypto:connection-alpn-protocol tls-conn)))
        (unless (string= protocol "h2")
          (error "Failed to negotiate HTTP/2, got: ~A" protocol)))

      ;; Create stream and send request
      (let ((stream (create-stream h2-conn)))
        ;; Prepare headers
        (let ((request-headers (list (cons ":method" (or method "GET"))
                                    (cons ":scheme" "https")
                                    (cons ":authority" host)
                                    (cons ":path" path))))
          ;; Add custom headers
          (when headers
            (setf request-headers (append request-headers headers)))

          ;; Send headers
          (stream-send-headers stream request-headers :end-stream (null body))

          ;; Send body if present
          (when body
            (stream-send-data stream body :end-stream t))

          ;; Read response
          (read-http2-response h2-conn stream))))))

(defun http2-get (url &key headers)
  "Make an HTTP/2 GET request"
  (http2-request url :method "GET" :headers headers))

(defun http2-post (url &key headers body)
  "Make an HTTP/2 POST request"
  (http2-request url :method "POST" :headers headers :body body))

;;;; Server Functions

(defun %printable-prefix (bytes)
  "Return a printable ASCII snippet of BYTES, escaping non-printables.
Used in error messages so operators can see what the peer actually sent
instead of staring at a raw octet mismatch."
  (with-output-to-string (s)
    (loop for b across bytes do
          (cond
            ((<= 32 b 126) (write-char (code-char b) s))
            ((= b 13) (write-string "\\r" s))
            ((= b 10) (write-string "\\n" s))
            ((= b 9)  (write-string "\\t" s))
            (t (format s "\\x~2,'0X" b))))))

(defun %preface-hint (received alpn)
  "Guess at what the peer was really doing. RECEIVED is the 24-byte buffer
from the wire, ALPN is the negotiated protocol string or NIL."
  (cond
    ((and alpn (not (string-equal alpn "h2")))
     (format nil "ALPN negotiated ~S, not h2 -- the client chose HTTP/1.1 but the server tried to read the HTTP/2 preface. Either advertise only \"h2\" or add an HTTP/1.1 fallback." alpn))
    ((and received (>= (length received) 4)
          (let ((head (map 'string #'code-char (subseq received 0 (min 4 (length received))))))
            (or (string-equal head "GET ")
                (string-equal head "POST")
                (string-equal head "HEAD")
                (string-equal head "PUT ")
                (string-equal head "DELE")
                (string-equal head "OPTI")
                (string-equal head "PATC"))))
     "Peer spoke HTTP/1.1 to an HTTP/2-only endpoint (no ALPN or ALPN ignored).")
    (t
     "Expected HTTP/2 preface \"PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n\".")))

(defun handle-http2-connection (connection handler)
  "Handle an HTTP/2 server connection"
  ;; First receive and validate client preface
  (unless (connection-client-p connection)
    (let ((preface (make-array 24 :element-type '(unsigned-byte 8))))
      (if (connection-tls-connection connection)
          (crypto:tls-read (connection-tls-connection connection) preface :start 0 :end 24)
          (net:tcp-read (connection-socket connection) preface :start 0 :end 24))

      ;; Validate preface
      (unless (equalp preface (string-to-bytes (subseq +http2-preface+ 0 24)))
        (let* ((tls (connection-tls-connection connection))
               (alpn (when tls
                       (ignore-errors (crypto:connection-alpn-protocol tls)))))
          (error 'error:http2-preface-error
                 :alpn alpn
                 :received (%printable-prefix preface)
                 :hint (%preface-hint preface alpn)
                 :message "Invalid HTTP/2 client preface")))))

  ;; Main frame processing loop
  (handler-case
      (loop
        (let ((frame (connection-receive-frame connection)))
          (handle-server-frame connection frame handler)))
    (end-of-file ()
      ;; Connection closed by client
      nil)
    (error (e)
      (format t "Error handling connection: ~A~%" e)
      (connection-close connection))))

(defun handle-server-frame (connection frame handler)
  "Handle a single frame in server mode"
  (let ((frame-type (frames:http2-frame-type frame))
        (stream-id (frames:http2-frame-stream-id frame)))

    (case frame-type
      ;; SETTINGS frame
      (#.frames:+frame-settings+
       (if (logtest (frames:http2-frame-flags frame) frames:+flag-ack+)
           ;; Settings ACK received
           nil
           ;; Apply settings and send ACK
           (progn
             (apply-remote-settings connection (frames:http2-frame-payload frame))
             (send-settings-ack connection))))

      ;; HEADERS frame
      (#.frames:+frame-headers+
       (let ((stream (or (gethash stream-id (connection-streams connection))
                        (let ((new-stream (stream:initialize-stream stream-id connection)))
                          (setf (gethash stream-id (connection-streams connection)) new-stream)
                          new-stream))))
         ;; Decode headers
         (let ((headers (decode-headers
                        (connection-hpack-decoder connection)
                        (frames:http2-frame-payload frame))))
           (setf (http2-stream-headers stream) headers)

           ;; If END_STREAM is set or we get END_HEADERS, process request
           (when (or (logtest (frames:http2-frame-flags frame) frames:+flag-end-stream+)
                    (logtest (frames:http2-frame-flags frame) frames:+flag-end-headers+))
             (process-server-request connection stream handler)))))

      ;; PING frame
      (#.frames:+frame-ping+
       (unless (logtest (frames:http2-frame-flags frame) frames:+flag-ack+)
         ;; Echo back PING with ACK flag
         (send-ping-ack connection (frames:http2-frame-payload frame))))

      ;; Other frames
      (t nil))))

(defun process-server-request (connection stream handler)
  "Process a complete HTTP/2 request and send response"
  (declare (ignore connection))
  (let* ((headers (http2-stream-headers stream))
         (body (when (> (length (http2-stream-data stream)) 0)
                 (bytes-to-string (http2-stream-data stream))))
         ;; Call handler to get response
         (response (funcall handler headers body)))

    ;; Send response
    (when response
      (let* ((status (or (getf response :status) 200))
             (response-headers (append
                               (list (cons ":status" (format nil "~D" status)))
                               (getf response :headers)))
             (response-body (getf response :body)))

        ;; Send HEADERS frame
        (stream-send-headers stream response-headers
                            :end-stream (null response-body))

        ;; Send DATA frame if body present
        (when response-body
          (stream-send-data stream
                           (if (stringp response-body)
                               (string-to-bytes response-body)
                               response-body)
                           :end-stream t))))))

(defun send-settings-ack (connection)
  "Send SETTINGS ACK frame"
  (let ((frame-bytes (make-array 9 :element-type '(unsigned-byte 8))))
    ;; Frame header: length=0, type=SETTINGS(4), flags=ACK(1), stream=0
    (setf (aref frame-bytes 0) 0)
    (setf (aref frame-bytes 1) 0)
    (setf (aref frame-bytes 2) 0)
    (setf (aref frame-bytes 3) frames:+frame-settings+)
    (setf (aref frame-bytes 4) frames:+flag-ack+)
    (setf (aref frame-bytes 5) 0)
    (setf (aref frame-bytes 6) 0)
    (setf (aref frame-bytes 7) 0)
    (setf (aref frame-bytes 8) 0)

    ;; Send directly
    (if (connection-tls-connection connection)
        (crypto:tls-write (connection-tls-connection connection) frame-bytes)
        (net:tcp-write (connection-socket connection) frame-bytes))))

(defun send-ping-ack (connection payload)
  "Send PING ACK frame"
  (let ((frame-bytes (make-array (+ 9 8) :element-type '(unsigned-byte 8))))
    (setf (aref frame-bytes 0) 0)
    (setf (aref frame-bytes 1) 0)
    (setf (aref frame-bytes 2) 8)
    (setf (aref frame-bytes 3) frames:+frame-ping+)
    (setf (aref frame-bytes 4) frames:+flag-ack+)
    (setf (aref frame-bytes 5) 0)
    (setf (aref frame-bytes 6) 0)
    (setf (aref frame-bytes 7) 0)
    (setf (aref frame-bytes 8) 0)

    ;; Copy payload (8 bytes)
    (when payload
      (loop for i from 0 below (min 8 (length payload))
            do (setf (aref frame-bytes (+ 9 i)) (aref payload i))))

    ;; Send directly
    (if (connection-tls-connection connection)
        (crypto:tls-write (connection-tls-connection connection) frame-bytes)
        (net:tcp-write (connection-socket connection) frame-bytes))))

(defun apply-remote-settings (connection settings-payload)
  "Apply settings from remote peer"
  (when (and settings-payload (> (length settings-payload) 0))
    (loop for i from 0 below (length settings-payload) by 6
          when (>= (+ i 6) (length settings-payload))
          do (return)
          do (let ((id (logior (ash (aref settings-payload i) 8)
                               (aref settings-payload (1+ i))))
                   (value (logior (ash (aref settings-payload (+ i 2)) 24)
                                  (ash (aref settings-payload (+ i 3)) 16)
                                  (ash (aref settings-payload (+ i 4)) 8)
                                  (aref settings-payload (+ i 5)))))
               (apply-single-setting connection id value)))))

(defun apply-single-setting (connection id value)
  "Apply a single setting to the connection"
  (case id
    (#.frames:+settings-header-table-size+
     (when (connection-hpack-decoder connection)
       (hpack:set-dynamic-table-size
        (hpack:hpack-decoder-dynamic-table (connection-hpack-decoder connection))
        value)))

    (#.frames:+settings-enable-push+
     (setf (connection-remote-settings connection)
           (acons :enable-push value
                  (remove :enable-push (connection-remote-settings connection)
                          :key #'car))))

    (#.frames:+settings-max-concurrent-streams+
     (setf (connection-max-concurrent-streams connection) value))

    (#.frames:+settings-initial-window-size+
     (let ((flow-ctrl (http2-connection-flow-controller connection)))
       (when flow-ctrl
         (flow:apply-settings-to-flow-control
          flow-ctrl
          (list (cons 4 value))))))

    (#.frames:+settings-max-frame-size+
     (setf (connection-remote-settings connection)
           (acons :max-frame-size value
                  (remove :max-frame-size (connection-remote-settings connection)
                          :key #'car))))

    (#.frames:+settings-max-header-list-size+
     (setf (connection-remote-settings connection)
           (acons :max-header-list-size value
                  (remove :max-header-list-size (connection-remote-settings connection)
                          :key #'car))))))

;;;; Frame Helper Functions

(defun serialize-frame (frame)
  "Serialize HTTP/2 frame to bytes"
  (let* ((payload (frames:http2-frame-payload frame))
         (length (if payload (length payload) 0))
         (header-bytes (make-array 9 :element-type '(unsigned-byte 8)))
         (total-bytes (make-array (+ 9 length) :element-type '(unsigned-byte 8))))

    ;; Frame header (9 bytes)
    (setf (aref header-bytes 0) (logand #xff (ash length -16)))
    (setf (aref header-bytes 1) (logand #xff (ash length -8)))
    (setf (aref header-bytes 2) (logand #xff length))
    (setf (aref header-bytes 3) (frames:http2-frame-type frame))
    (setf (aref header-bytes 4) (frames:http2-frame-flags frame))
    (setf (aref header-bytes 5) (logand #x7f (ash (frames:http2-frame-stream-id frame) -24)))
    (setf (aref header-bytes 6) (logand #xff (ash (frames:http2-frame-stream-id frame) -16)))
    (setf (aref header-bytes 7) (logand #xff (ash (frames:http2-frame-stream-id frame) -8)))
    (setf (aref header-bytes 8) (logand #xff (frames:http2-frame-stream-id frame)))

    ;; Copy header
    (replace total-bytes header-bytes :end1 9)

    ;; Copy payload if present
    (when payload
      (replace total-bytes payload :start1 9))

    total-bytes))

(defun parse-frame-header (header-bytes)
  "Parse frame header and return length"
  (logior (ash (aref header-bytes 0) 16)
          (ash (aref header-bytes 1) 8)
          (aref header-bytes 2)))

(defun parse-frame (header-bytes payload-bytes)
  "Parse complete frame from header and payload"
  (let* ((length (logior (ash (aref header-bytes 0) 16)
                         (ash (aref header-bytes 1) 8)
                         (aref header-bytes 2)))
         (type (aref header-bytes 3))
         (flags (aref header-bytes 4))
         (stream-id (logior (ash (logand #x7F (aref header-bytes 5)) 24)
                            (ash (aref header-bytes 6) 16)
                            (ash (aref header-bytes 7) 8)
                            (aref header-bytes 8))))
    (frames:make-http2-frame :length length
                              :type type
                              :flags flags
                              :stream-id stream-id
                              :payload payload-bytes)))

(defun deserialize-frame (bytes)
  "Deserialize bytes to a frame"
  (when (< (length bytes) 9)
    (error "Invalid frame: too short (~D bytes)" (length bytes)))

  (let* ((length (logior (ash (aref bytes 0) 16)
                        (ash (aref bytes 1) 8)
                        (aref bytes 2)))
         (type (aref bytes 3))
         (flags (aref bytes 4))
         (stream-id (logior (ash (logand #x7F (aref bytes 5)) 24)
                           (ash (aref bytes 6) 16)
                           (ash (aref bytes 7) 8)
                           (aref bytes 8)))
         (payload (when (> length 0)
                   (subseq bytes 9 (+ 9 length)))))

    (frames:make-http2-frame :length length
                             :type type
                             :flags flags
                             :stream-id stream-id
                             :payload payload)))

;;;; Frame Accessor Functions

(defun frame-flag-set-p (frame flag)
  "Check if a flag is set on the frame"
  (logtest (frames:http2-frame-flags frame) flag))

(defun frame-type (frame)
  "Get frame type"
  (frames:http2-frame-type frame))

(defun frame-stream-id (frame)
  "Get frame stream ID"
  (frames:http2-frame-stream-id frame))

(defun frame-payload (frame)
  "Get frame payload"
  (frames:http2-frame-payload frame))

;;;; Stream Property Wrappers

(defun http2-stream-headers (stream)
  "Get headers from stream"
  (stream:http2-stream-headers-buffer stream))

(defun (setf http2-stream-headers) (value stream)
  "Set headers on stream"
  (setf (stream:http2-stream-headers-buffer stream) value))

(defun http2-stream-data (stream)
  "Get data from stream"
  (stream:http2-stream-data-buffer stream))

(defun (setf http2-stream-data) (value stream)
  "Set data on stream"
  (setf (stream:http2-stream-data-buffer stream) value))

(defun http2-stream-state (stream)
  "Get stream state"
  (stream:http2-stream-state stream))

(defun (setf http2-stream-state) (value stream)
  "Set stream state"
  (setf (stream:http2-stream-state stream) value))

(defun http2-stream-connection (stream)
  "Get connection from stream"
  (stream:http2-stream-connection stream))

(defun http2-stream-id (stream)
  "Get stream ID"
  (stream:http2-stream-id stream))

;;;; Helper Functions

(defun string-to-bytes (string)
  "Convert string to byte array"
  (map 'vector #'char-code string))

(defun bytes-to-string (bytes)
  "Convert byte array to string"
  (map 'string #'code-char bytes))

(defun read-http2-response (connection stream)
  "Read HTTP/2 response from stream"
  (let ((response-headers nil)
        (response-data nil))

    (loop
      (let ((frame (connection-receive-frame connection)))
        (cond
          ;; HEADERS frame
          ((eq (frame-type frame) frames:+frame-headers+)
           (when (= (frame-stream-id frame) (stream-id stream))
             (setf response-headers
                   (decode-headers
                    (connection-hpack-decoder connection)
                    (frame-payload frame)))
             (when (frame-flag-set-p frame frames:+flag-end-stream+)
               (return))))

          ;; DATA frame
          ((eq (frame-type frame) frames:+frame-data+)
           (when (= (frame-stream-id frame) (stream-id stream))
             (setf response-data
                   (append response-data (coerce (frame-payload frame) 'list)))
             (when (frame-flag-set-p frame frames:+flag-end-stream+)
               (return))))

          ;; Other frames - handle as needed
          (t nil))))

    (list :headers response-headers
          :body (when response-data
                  (bytes-to-string (coerce response-data 'vector))))))

;;;; Stream Management Functions

(defun stream-receive-headers (stream)
  "Receive headers for a stream"
  (http2-stream-headers stream))

(defun stream-receive-data (stream)
  "Receive data for a stream"
  (let ((data (http2-stream-data stream)))
    (when (> (length data) 0)
      (bytes-to-string data))))

(defun stream-close (stream)
  "Close a stream"
  (setf (http2-stream-state stream) :closed)
  ;; Remove from connection's stream table
  (let ((connection (http2-stream-connection stream)))
    (remhash (http2-stream-id stream) (connection-streams connection))))

(defun http2-connection-p (obj)
  "Check if object is an HTTP/2 connection"
  (typep obj 'http2-connection))

(defun is-http2-connection (connection)
  "Check if connection is using HTTP/2"
  (http2-connection-p connection))

(defun upgrade-to-http2 (connection)
  "Upgrade an HTTP/1.1 connection to HTTP/2 via h2c.
   Delegates to the protocol negotiator's h2c upgrade mechanism.
   CONNECTION should be a protocol-connection struct from the negotiator.
   Returns the upgraded connection on success, or signals an error on failure."
  (let ((negotiator-pkg (find-package :epsilon.http.protocol.negotiator)))
    (if negotiator-pkg
        (let ((upgrade-fn (find-symbol "INITIATE-H2C-UPGRADE" negotiator-pkg)))
          (if upgrade-fn
              (let ((result (funcall upgrade-fn connection)))
                (or result
                    (error "h2c upgrade failed: server does not support HTTP/2 cleartext upgrade")))
              (error "h2c upgrade unavailable: INITIATE-H2C-UPGRADE not found in negotiator")))
        (error "h2c upgrade unavailable: negotiator package not loaded"))))

(defun http2-server (&rest args)
  "Start an HTTP/2 server (alias for start-server)"
  (apply #'start-server args))

(defun make-settings (alist)
  "Create settings from an alist"
  alist)

(defun apply-settings (connection settings)
  "Apply settings to a connection (alist format)"
  (when settings
    (dolist (setting settings)
      (let ((key (car setting))
            (value (cdr setting)))
        (apply-single-setting
         connection
         (case key
           (:header-table-size frames:+settings-header-table-size+)
           (:enable-push frames:+settings-enable-push+)
           (:max-concurrent-streams frames:+settings-max-concurrent-streams+)
           (:initial-window-size frames:+settings-initial-window-size+)
           (:max-frame-size frames:+settings-max-frame-size+)
           (:max-header-list-size frames:+settings-max-header-list-size+)
           (t key))
         value)))))

;;;; HPACK Helper Functions

(defun encode-headers (encoder headers)
  "Encode headers using HPACK"
  (hpack:encode-header-list encoder headers))

(defun decode-headers (decoder payload)
  "Decode headers using HPACK"
  (hpack:decode-header-block decoder payload))

;;;; Server Start/Stop Functions

(defun start-server (&key (host "0.0.0.0")
                          (port 8080)
                          cert-file
                          key-file
                          ca-file
                          require-client-cert
                          request-client-cert
                          handler
                          http1-application)
  "Start an HTTP/2 server.

HTTP1-APPLICATION, when supplied, is a (request)->response function used
to serve clients that negotiated ALPN \"http/1.1\" (or sent no ALPN on
TLS). This enables HTTP/1.1 fallback alongside HTTP/2.

REQUIRE-CLIENT-CERT (strict mTLS) and REQUEST-CLIENT-CERT (soft mTLS:
ask but don't require) are mutually exclusive. With soft mTLS the cert
(if any) reaches handlers via X-Client-Cert-* request headers."
  (h2-server:start-http2-server
   :host host
   :port port
   :ssl-p (and cert-file key-file t)
   :cert-file cert-file
   :key-file key-file
   :ca-file ca-file
   :require-client-cert require-client-cert
   :request-client-cert request-client-cert
   :handler handler
   :http1-application http1-application))

(defun stop-server (server)
  "Stop the HTTP/2 server."
  (h2-server:stop-http2-server server))

(defun swap-tls-context! (server new-context)
  "Hot-swap the TLS context on a running HTTP/2 server. Forwards to
the underlying `epsilon.http.h2.server:swap-tls-context!'. See that
docstring for atomicity guarantees."
  (h2-server:swap-tls-context! server new-context))
