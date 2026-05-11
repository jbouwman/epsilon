;;;; epsilon.http.client - HTTP Client Implementation
;;;;
;;;; HTTP/1.1 client with connection pooling and keep-alive

(defpackage epsilon.http.client
  (:use :cl)
  (:shadow #:get)
  (:import (epsilon.net net)
            (epsilon.string str)
            (epsilon.sequence seq)
            (epsilon.map map)
            (epsilon.crypto crypto)
            (epsilon.compression compress)
            (epsilon.encode enc)
            (epsilon.http.headers headers)
            (epsilon.http.response response)
            (epsilon.http.connection-pool pool)))

(in-package :epsilon.http.client)

(export '(response-too-large-error
          response-too-large-error-limit
          response-too-large-error-stage
          *max-response-headers-bytes*
          *max-response-body-bytes*
          *default-request-timeout*
          *default-tcp-read-timeout*))

;;; --------------------------------------------------------------------------
;;; Defensive response-size limits.
;;;
;;; The read loops below grow an adjustable byte vector as bytes arrive
;;; from the socket. Without a cap, a misbehaving (or hostile) server can
;;; force the loops to allocate until the heap is exhausted, producing a
;;; STORAGE-CONDITION inside a worker thread -- which on SBCL surfaces
;;; as a process-level "fatal error" that the per-worker handler-case
;;; cannot catch.  These two limits are deliberately conservative and
;;; can be widened by callers that legitimately need bigger responses
;;; (e.g. file downloads).
;;; --------------------------------------------------------------------------

(defvar *max-response-headers-bytes* (* 1 1024 1024)
  "Hard cap on the byte size of an HTTP response's headers (everything
   up to and including the CRLFCRLF terminator).  Exceeding this signals
   RESPONSE-TOO-LARGE-ERROR before any further allocation.")

(defvar *max-response-body-bytes* (* 64 1024 1024)
  "Hard cap on the byte size of an HTTP response body, applied to both
   Content-Length and chunked transfer-encoding paths.  Exceeding this
   signals RESPONSE-TOO-LARGE-ERROR rather than allocating until the
   heap is exhausted.")

(define-condition response-too-large-error (error)
  ((limit :initarg :limit :reader response-too-large-error-limit)
   (stage :initarg :stage :reader response-too-large-error-stage
          :documentation "One of :HEADERS, :CONTENT-LENGTH, :CHUNKED."))
  (:report (lambda (c s)
             (format s "HTTP response exceeded the ~A cap (~A bytes)"
                     (response-too-large-error-stage c)
                     (response-too-large-error-limit c)))))

(defun %check-headers-cap (n)
  (when (> n *max-response-headers-bytes*)
    (error 'response-too-large-error
           :limit *max-response-headers-bytes*
           :stage :headers)))

(defun %check-body-cap (n stage)
  (when (> n *max-response-body-bytes*)
    (error 'response-too-large-error
           :limit *max-response-body-bytes*
           :stage stage)))

;;; Content-Encoding decompression

(defun inflate-with-window-bits (data window-bits)
  "Decompress DATA using streaming inflater with specific WINDOW-BITS.
   This handles gzip (window-bits 31), raw deflate, and auto-detect (47)."
  (compress:with-inflater (ctx :window-bits window-bits)
    (let ((output (compress:inflate-update ctx data)))
      ;; inflate-update returns a byte vector
      output)))

(defun decompress-body (body content-encoding)
  "Decompress BODY bytes according to CONTENT-ENCODING header.
   Supports gzip, deflate, br (Brotli), and zstd.
   Returns decompressed bytes, or the original body if encoding is identity or unknown."
  (when (or (null body) (null content-encoding))
    (return-from decompress-body body))
  (let ((encoding (string-downcase (str:trim content-encoding))))
    (cond
      ((or (string= encoding "gzip") (string= encoding "x-gzip"))
       (inflate-with-window-bits body compress:+gzip-wbits+))
      ((string= encoding "deflate")
       ;; Use auto-detect window bits for deflate (handles both raw and zlib-wrapped)
       (inflate-with-window-bits body compress:+auto-wbits+))
      ((string= encoding "br")
       (if (compress:brotli-available-p)
           (compress:brotli-decompress body)
           (progn (warn "Brotli decompression requested but library not available")
                  body)))
      ((string= encoding "zstd")
       (if (compress:zstd-available-p)
           (compress:zstd-decompress body)
           (progn (warn "Zstd decompression requested but library not available")
                  body)))
      ((string= encoding "identity")
       body)
      (t body))))

(defclass http-connection ()
  ((socket :initarg :socket :accessor connection-socket)
   (host :initarg :host :accessor connection-host)
   (port :initarg :port :accessor connection-port)
   (ssl-p :initarg :ssl-p :accessor connection-ssl-p :initform nil)
   (tls-connection :initarg :tls-connection :accessor connection-tls-connection :initform nil)
   (tls-context :initarg :tls-context :accessor connection-tls-context :initform nil)
   (alpn-protocol :initarg :alpn-protocol
                  :accessor connection-alpn-protocol-name
                  :initform nil
                  :documentation "ALPN protocol negotiated during the TLS handshake,
or NIL for plain HTTP. Used by send-request/read-response to choose between the
HTTP/1.1 framing path and the HTTP/2 path.")))

(defun make-proxy-connection (proxy-host proxy-port target-host target-port
                              &key ssl-p tls-context cert-file key-file ca-file
                                   alpn-protocols proxy-username proxy-password)
  "Establish an HTTP connection through an HTTP proxy using CONNECT tunnel.
   First connects to the proxy, sends CONNECT to create a tunnel to target,
   then optionally upgrades to TLS.
   Returns an http-connection to the target through the proxy."
  (let* ((addresses (net:resolve-address proxy-host proxy-port))
         (address (first addresses))
         (socket (net:tcp-connect address))
         (proxy-stream (net:tcp-stream-byte-writer socket))
         (proxy-reader (net:tcp-stream-byte-reader socket)))
    ;; Send CONNECT request to proxy
    (let ((connect-request
            (with-output-to-string (s)
              (format s "CONNECT ~A:~D HTTP/1.1~C~C" target-host target-port
                      #\Return #\Linefeed)
              (format s "Host: ~A:~D~C~C" target-host target-port
                      #\Return #\Linefeed)
              ;; Proxy-Authorization if credentials provided
              (when (and proxy-username proxy-password)
                (let ((credentials (format nil "~A:~A" proxy-username proxy-password)))
                  (format s "Proxy-Authorization: Basic ~A~C~C"
                          (enc:base64-encode-string credentials)
                          #\Return #\Linefeed)))
              (format s "~C~C" #\Return #\Linefeed))))
      (let ((request-bytes (sb-ext:string-to-octets connect-request :external-format :utf-8)))
        (write-sequence request-bytes proxy-stream)
        (finish-output proxy-stream))
      ;; Read proxy response (looking for 200 Connection established)
      (let ((response-data (make-array 0 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0))
            (chunk (make-array 1 :element-type '(unsigned-byte 8))))
        ;; Read until CRLFCRLF
        (loop
          (let ((n (read-sequence chunk proxy-reader)))
            (when (zerop n) (error "Proxy closed connection during CONNECT"))
            (vector-push-extend (aref chunk 0) response-data)
            (let ((len (length response-data)))
              (when (and (>= len 4)
                         (= (aref response-data (- len 4)) 13)
                         (= (aref response-data (- len 3)) 10)
                         (= (aref response-data (- len 2)) 13)
                         (= (aref response-data (- len 1)) 10))
                (return)))))
        ;; Parse status line
        (let* ((response-str (sb-ext:octets-to-string
                              (coerce response-data '(simple-array (unsigned-byte 8) (*)))
                              :external-format :utf-8))
               (status-end (position #\Space response-str :start 9))
               (status-code (when status-end
                              (ignore-errors
                                (parse-integer (subseq response-str 9 status-end))))))
          (unless (eql status-code 200)
            (net:tcp-shutdown socket)
            (error "Proxy CONNECT failed with status ~A: ~A"
                   status-code (subseq response-str 0 (min 80 (length response-str)))))))
      ;; Tunnel is established. Now upgrade to TLS if needed.
      (let ((tls-conn nil)
            (tls-ctx nil)
            (effective-alpn (or alpn-protocols '("h2" "http/1.1"))))
        (when ssl-p
          (setf tls-ctx (or tls-context
                            (crypto:make-client-context
                             :cert-file cert-file
                             :key-file key-file
                             :ca-file ca-file
                             :verify-mode (if ca-file
                                              crypto:+verify-peer+
                                              crypto:+verify-none+))))
          (let ((sni-hostname (unless (every (lambda (c)
                                               (or (digit-char-p c) (char= c #\.)))
                                             target-host)
                                target-host)))
            (setf tls-conn (crypto:tls-connect socket tls-ctx
                                                :hostname sni-hostname
                                                :alpn-protocols effective-alpn))))
        (make-instance 'http-connection
                       :socket socket
                       :host target-host
                       :port target-port
                       :ssl-p ssl-p
                       :tls-connection tls-conn
                       :tls-context tls-ctx
                       :alpn-protocol (when tls-conn
                                        (handler-case
                                            (crypto:connection-alpn-protocol tls-conn)
                                          (error () nil))))))))

(defun make-http-connection (host port &key ssl-p tls-context cert-file key-file ca-file
                                        alpn-protocols verify-depth session-cache-p
                                        proxy-host proxy-port proxy-username proxy-password
                                        timeout)
  "Create an HTTP connection to HOST:PORT with full mTLS and HTTP/2 support.
   Parameters:
   - ssl-p: Enable SSL/TLS
   - tls-context: Pre-configured TLS context to use
   - cert-file: Client certificate file for mutual TLS
   - key-file: Client private key file for mutual TLS
   - ca-file: CA certificate file for server verification
   - alpn-protocols: List of ALPN protocols (default: '(\"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption
   - proxy-host: HTTP proxy hostname
   - proxy-port: HTTP proxy port
   - proxy-username: Proxy authentication username
   - proxy-password: Proxy authentication password
   - timeout: Request timeout in seconds (sets socket recv/send timeouts)
   Example: (make-http-connection \"example.com\" 443 :ssl-p t
                                  :cert-file \"client.pem\" :key-file \"key.pem\"
                                  :alpn-protocols '(\"h2\" \"http/1.1\"))"
  (declare (ignore verify-depth session-cache-p))
  ;; Route through proxy if configured
  (when proxy-host
    (return-from make-http-connection
      (make-proxy-connection proxy-host proxy-port host port
                             :ssl-p ssl-p
                             :tls-context tls-context
                             :cert-file cert-file
                             :key-file key-file
                             :ca-file ca-file
                             :alpn-protocols alpn-protocols
                             :proxy-username proxy-username
                             :proxy-password proxy-password)))
  (let* ((addresses (net:resolve-address host port))
         (address (first addresses))
         (socket (net:tcp-connect address))
         (tls-conn nil)
         (tls-ctx nil)
         (effective-alpn-protocols (or alpn-protocols '("http/1.1"))))
    ;; Set socket timeouts if requested
    (when timeout
      (let ((timeout-ms (round (* timeout 1000))))
        (handler-case
            (progn
              (net:set-socket-option socket :recv-timeout timeout-ms)
              (net:set-socket-option socket :send-timeout timeout-ms))
          (error () nil))))
    (when ssl-p
      (setf tls-ctx (or tls-context
                        (crypto:make-client-context
                         :cert-file cert-file
                         :key-file key-file
                         :ca-file ca-file
                         :verify-mode (if ca-file
                                          crypto:+verify-peer+
                                          crypto:+verify-none+))))
      ;; Only set SNI hostname for actual hostnames, not IP literals
      ;; (RFC 6066 specifies SNI is for DNS hostnames only)
      (let ((sni-hostname (unless (net:ip-literal-p host) host)))
        (setf tls-conn (crypto:tls-connect socket tls-ctx
                                            :hostname sni-hostname
                                            :alpn-protocols effective-alpn-protocols))))
    (let ((negotiated-alpn
            (when tls-conn
              (handler-case (crypto:connection-alpn-protocol tls-conn)
                (error () nil)))))
      (make-instance 'http-connection
                     :socket socket
                     :host host
                     :port port
                     :ssl-p ssl-p
                     :tls-connection tls-conn
                     :tls-context tls-ctx
                     :alpn-protocol negotiated-alpn))))

(defmacro with-connection ((conn host port &key ssl-p tls-context cert-file key-file ca-file
                                             alpn-protocols verify-depth session-cache-p
                                             proxy-host proxy-port
                                             proxy-username proxy-password
                                             timeout) &body body)
  "Execute body with an HTTP connection, supporting mutual TLS, HTTP/2, proxies, and timeouts"
  `(let ((,conn (make-http-connection ,host ,port
                                      :ssl-p ,ssl-p
                                      :tls-context ,tls-context
                                      :cert-file ,cert-file
                                      :key-file ,key-file
                                      :ca-file ,ca-file
                                      :alpn-protocols ,alpn-protocols
                                      :verify-depth ,verify-depth
                                      :session-cache-p ,session-cache-p
                                      :proxy-host ,proxy-host
                                      :proxy-port ,proxy-port
                                      :proxy-username ,proxy-username
                                      :proxy-password ,proxy-password
                                      :timeout ,timeout)))
     (unwind-protect
          (progn ,@body)
       (progn
         (when (connection-tls-connection ,conn)
           (crypto:tls-close (connection-tls-connection ,conn)))
         (when (and (connection-socket ,conn)
                    (not (connection-ssl-p ,conn)))
           (handler-case (net:tcp-shutdown (connection-socket ,conn))
             (error () nil))
           (net:tcp-close (connection-socket ,conn)))))))

(defun parse-url (url-string)
  "Parse URL-STRING into scheme, host, port, path, and query components.
   Example: (parse-url \"https://api.example.com:8080/data?id=123\")"
  (let* ((scheme-end (search "://" url-string))
         (scheme (if scheme-end
                     (subseq url-string 0 scheme-end)
                     "http"))
         (rest-url (if scheme-end
                       (subseq url-string (+ scheme-end 3))
                       url-string))
         (slash-pos (position #\/ rest-url))
         (question-pos (position #\? rest-url))
         (colon-pos (position #\: rest-url))
         ;; Find the end of the authority section (host:port)
         (authority-end (cond
                          (slash-pos slash-pos)
                          (question-pos question-pos)
                          (t (length rest-url))))
         ;; Only consider colon as port separator if it's before path/query
         (port-colon-pos (when (and colon-pos (< colon-pos authority-end))
                           colon-pos))
         (host-end (or port-colon-pos authority-end))
         (host (subseq rest-url 0 host-end))
         (port (cond
                 (port-colon-pos
                  (parse-integer (subseq rest-url (1+ port-colon-pos) authority-end)))
                 ((string= scheme "https") 443)
                 (t 80)))
         (path-start (or slash-pos (length rest-url)))
         (path-end (or question-pos (length rest-url)))
         (path (if (< path-start (length rest-url))
                   (subseq rest-url path-start path-end)
                   "/"))
         (query (when (and question-pos (< question-pos (length rest-url)))
                  (subseq rest-url (1+ question-pos)))))
    (values scheme host port path query)))

(defun format-request-line (method path query)
  "Format HTTP request line with METHOD, PATH and optional QUERY.
   Example: (format-request-line \"GET\" \"/api/users\" \"page=1\")"
  (format nil "~A ~A~@[?~A~] HTTP/1.1"
          method
          path
          query))

(defun format-headers (headers-arg)
  "Format HTTP headers for wire output. Accepts a header-map or a plain
   epsilon.map; both are iterated and rendered as `Name: Value` lines."
  (with-output-to-string (s)
    (cond
      ((null headers-arg) nil)
      ((headers:headers-p headers-arg)
       (headers:headers-each
        (lambda (k v)
          (format s "~A: ~A~C~C" k v #\Return #\Linefeed))
        headers-arg))
      (t
       (map:each
        (lambda (k v)
          (format s "~A: ~A~C~C" k v #\Return #\Linefeed))
        headers-arg)))))

(defun extract-content-length (headers-text)
  "Extract Content-Length from headers text"
  (let ((start (search "Content-Length:" headers-text :test #'char-equal)))
    (when start
      (let* ((line-start (+ start 15)) ; length of "Content-Length:"
             (line-end (or (position #\Linefeed headers-text :start line-start)
                           (length headers-text)))
             (length-str (str:trim (subseq headers-text line-start line-end))))
        (ignore-errors (parse-integer length-str))))))

(defun extract-transfer-encoding (headers-text)
  "Extract Transfer-Encoding from headers text. Returns lowercase string or nil."
  (let ((start (search "Transfer-Encoding:" headers-text :test #'char-equal)))
    (when start
      (let* ((line-start (+ start 18)) ; length of "Transfer-Encoding:"
             (line-end (or (position #\Linefeed headers-text :start line-start)
                           (length headers-text))))
        (string-downcase (str:trim (subseq headers-text line-start line-end)))))))

(defun chunked-encoding-p (headers-text)
  "Return T if headers indicate chunked transfer-encoding."
  (let ((te (extract-transfer-encoding headers-text)))
    (and te (search "chunked" te) t)))

;;; Chunked transfer-encoding decoding
;;;
;;; Chunked format (RFC 7230 Section 4.1):
;;;   chunk = chunk-size [chunk-ext] CRLF chunk-data CRLF
;;;   chunk-size = 1*HEXDIG
;;;   last-chunk = "0" [chunk-ext] CRLF
;;;   trailer-part = *( header-field CRLF )
;;;   chunked-body = *chunk last-chunk trailer-part CRLF

(defun decode-chunked-body (data start)
  "Decode chunked transfer-encoding from DATA starting at byte offset START.
   Returns the decoded body as a byte vector.
   DATA is a byte vector containing the raw chunked-encoded data."
  (let ((body (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (pos start)
        (len (length data)))
    (loop
      (when (>= pos len) (return))
      ;; Read chunk-size line: hex digits, optional extensions, CRLF
      (let ((line-end (find-crlf data pos)))
        (unless line-end (return))
        (let* ((line-bytes (subseq data pos line-end))
               (line-str (sb-ext:octets-to-string
                          (coerce line-bytes '(simple-array (unsigned-byte 8) (*)))
                          :external-format :utf-8))
               ;; Strip chunk extensions (everything after semicolon)
               (size-str (let ((semi (position #\; line-str)))
                           (if semi (subseq line-str 0 semi) line-str)))
               (chunk-size (ignore-errors (parse-integer (str:trim size-str) :radix 16))))
          (unless chunk-size (return))
          ;; Zero chunk-size means end of chunked data
          (when (zerop chunk-size) (return))
          ;; Skip past CRLF after size line
          (setf pos (+ line-end 2))
          ;; Copy chunk-size bytes of data
          (let ((chunk-end (min (+ pos chunk-size) len)))
            (loop for i from pos below chunk-end
                  do (vector-push-extend (aref data i) body))
            ;; Skip chunk data + trailing CRLF
            (setf pos (+ chunk-end 2))))))
    (coerce body '(simple-array (unsigned-byte 8) (*)))))

(defun find-crlf (data start)
  "Find the position of CR in a CRLF pair in DATA starting from START.
   Returns the position of CR, or nil if not found."
  (loop for i from start below (1- (length data))
        when (and (= (aref data i) 13)
                  (= (aref data (1+ i)) 10))
        return i))

(defun chunked-body-complete-p (data)
  "Return T if DATA (byte vector with fill-pointer) ends with the chunked
   body terminator CRLF CRLF. Per RFC 7230, a chunked body always ends with:
     last-chunk CRLF [trailer-part] CRLF
   The final four bytes are always 13 10 13 10 regardless of whether trailers
   or chunk extensions are present."
  (let ((len (length data)))
    (and (>= len 4)
         (= (aref data (- len 4)) 13)
         (= (aref data (- len 3)) 10)
         (= (aref data (- len 2)) 13)
         (= (aref data (- len 1)) 10))))

(defparameter *max-chunked-body-size* (* 16 1024 1024)
  "Maximum size in bytes for chunked response body (default 16 MB).
   Prevents unbounded memory growth from malicious or buggy servers.")

(defparameter *default-tcp-read-timeout* 30
  "Default per-call timeout in seconds for the plain-TCP read path.
   Sockets are non-blocking under the hood, so each net:tcp-read call must
   pass a timeout to wait for data instead of returning 0 immediately on
   EAGAIN.")

(defparameter *default-request-timeout* 30
  "Default request-level timeout (seconds) applied by REQUEST and the
   per-method wrappers (GET, HTTP-POST, ...) when callers do not pass an
   explicit :TIMEOUT.  Sets SO_RCVTIMEO/SO_SNDTIMEO on the underlying
   socket so a stalled server cannot wedge the caller forever.  Bind to
   NIL inside specific call sites that need to wait without a deadline
   (e.g. long-poll / SSE consumers).")

(defun read-remaining-chunked-data (connection response-bytes
                                    &key (body-start 0))
  "Read remaining chunked data from CONNECTION into RESPONSE-BYTES until the
   zero-length chunk terminator is found.
   BODY-START is the byte offset where the body begins (after headers).
   Checks if terminator is already present before reading more data.
   Signals an error if the accumulated data exceeds *max-chunked-body-size*."
  ;; Check if we already have the complete chunked body.
  ;; Only consider the response complete if there is actual body data
  ;; beyond the headers AND it ends with the chunked terminator.
  (when (and (> (length response-bytes) body-start)
             (chunked-body-complete-p response-bytes))
    (return-from read-remaining-chunked-data))
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop
      (when (> (length response-bytes) *max-chunked-body-size*)
        (error "Chunked response body exceeded maximum size of ~D bytes"
               *max-chunked-body-size*))
      (let ((bytes-read
              (if (connection-ssl-p connection)
                  (crypto:tls-read (connection-tls-connection connection)
                                   buf :start 0 :end 4096)
                  (net:tcp-read (connection-socket connection)
                                buf :start 0 :end 4096
                                :timeout *default-tcp-read-timeout*))))
        (when (or (null bytes-read) (zerop bytes-read)) (return))
        (loop for i from 0 below bytes-read
              do (vector-push-extend (aref buf i) response-bytes))
        (when (chunked-body-complete-p response-bytes)
          (return))))))

(defun normalize-caller-headers (headers)
  "Coerce HEADERS (a header-map, a plain epsilon.map, or NIL) to a plain
   epsilon.map for the on-wire writers."
  (cond
    ((null headers) map:+empty+)
    ((headers:headers-p headers)
     (let ((m map:+empty+))
       (headers:headers-each
        (lambda (k v) (setf m (map:assoc m k v)))
        headers)
       m))
    (t headers)))

(defun send-request (connection method path &key headers body query)
  "Send HTTP request over connection. HEADERS may be a header-map or a plain
   epsilon.map; both are normalized to plain map for wire output."
  (let* ((request-line (format-request-line method path query))
         (default-headers (map:make-map
                           "Host" (connection-host connection)
                           "User-Agent" "epsilon.http/1.0"
                           "Connection" "close"))
         (caller-headers (normalize-caller-headers headers))
         (all-headers (map:merge default-headers caller-headers))
         ;; Convert body to bytes if needed, compute Content-Length from bytes
         (body-bytes (when body
                       (if (stringp body)
                           (sb-ext:string-to-octets body :external-format :utf-8)
                           body)))
         (final-headers (if body-bytes
                            (map:assoc all-headers
                                       "Content-Length"
                                       (length body-bytes))
                            all-headers))
         ;; Build headers as string (body handled separately)
         (headers-str (format nil "~A~C~C~A~C~C"
                              request-line #\Return #\Linefeed
                              (format-headers final-headers) #\Return #\Linefeed)))
    (if (connection-ssl-p connection)
        ;; TLS connection - write headers then body separately
        (let ((tls-conn (connection-tls-connection connection)))
          (crypto:tls-write tls-conn headers-str)
          (when body-bytes
            (crypto:tls-write tls-conn body-bytes)))
        ;; Regular socket - write headers then body separately
        (let ((out-stream (net:tcp-stream-byte-writer (connection-socket connection)))
              (headers-bytes (sb-ext:string-to-octets headers-str :external-format :utf-8)))
          (write-sequence headers-bytes out-stream)
          (when body-bytes
            (write-sequence body-bytes out-stream))
          (finish-output out-stream)))))

(defun read-tcp-bytes (socket buffer end)
  "Read up to END bytes from SOCKET into BUFFER, blocking up to
   *default-tcp-read-timeout* seconds for data to become available.
   Returns the number of bytes read, or 0 on EOF/timeout."
  (or (net:tcp-read socket buffer
                    :start 0
                    :end end
                    :timeout *default-tcp-read-timeout*)
      0))

(defun read-response (connection &key method)
  "Read HTTP response from connection. The plain TCP path uses net:tcp-read
   (recv(2)) with a per-call timeout so small responses no longer hang
   waiting for a buffered read-sequence to fill its buffer. The TLS path
   uses crypto:tls-read for the same reason."
  (if (connection-ssl-p connection)
      ;; TLS connection - read directly from TLS
      (read-tls-response connection :method method)
      ;; Plain TCP - use net:tcp-read (recv(2)) so reads return as soon as
      ;; data is available, instead of blocking until the buffer is full.
      (let* ((socket (connection-socket connection))
             (response-bytes (make-array 8192 :element-type '(unsigned-byte 8)
                                               :adjustable t :fill-pointer 0))
             (chunk (make-array 4096 :element-type '(unsigned-byte 8)))
             (headers-complete nil)
             (content-length nil)
             (header-end-pos nil))
        ;; Read until we have complete headers
        (loop until headers-complete
              do (let ((bytes-read (read-tcp-bytes socket chunk 4096)))
                   (when (zerop bytes-read) (return))
                   (loop for i from 0 below bytes-read
                         do (vector-push-extend (aref chunk i) response-bytes))
                   (%check-headers-cap (length response-bytes))
                   ;; Check for CRLFCRLF (header end)
                   (let ((len (length response-bytes)))
                     (when (>= len 4)
                       (loop for i from 0 to (- len 4)
                             when (and (= (aref response-bytes i) 13)
                                       (= (aref response-bytes (+ i 1)) 10)
                                       (= (aref response-bytes (+ i 2)) 13)
                                       (= (aref response-bytes (+ i 3)) 10))
                             do (setf headers-complete t
                                      header-end-pos (+ i 4))
                                (return))))))
        ;; Parse headers to get Content-Length and Transfer-Encoding
        (let ((chunked-p nil))
          (when header-end-pos
            (let ((headers-str (sb-ext:octets-to-string
                                (subseq response-bytes 0 header-end-pos)
                                :external-format :utf-8)))
              (setf content-length (extract-content-length headers-str))
              (setf chunked-p (chunked-encoding-p headers-str))))
          (cond
            ;; Chunked transfer-encoding: read until zero-length chunk
            (chunked-p
             (read-remaining-chunked-data connection response-bytes
                                          :body-start (or header-end-pos 0))
             (let* ((raw (coerce response-bytes '(simple-array (unsigned-byte 8) (*))))
                    (decoded-body (decode-chunked-body raw header-end-pos))
                    ;; Rebuild response-bytes with headers + decoded body
                    (result (make-array (+ header-end-pos (length decoded-body))
                                        :element-type '(unsigned-byte 8))))
               (replace result raw :end1 header-end-pos)
               (replace result decoded-body :start1 header-end-pos)
               (parse-response-from-bytes result header-end-pos)))
            ;; Content-Length based reading
            ((and content-length (> content-length 0))
             (%check-body-cap content-length :content-length)
             (let* ((body-bytes-read (- (length response-bytes) (or header-end-pos 0)))
                    (bytes-remaining (- content-length body-bytes-read)))
               (loop while (> bytes-remaining 0)
                     do (let ((bytes-read (read-tcp-bytes socket chunk
                                                          (min bytes-remaining 4096))))
                          (when (zerop bytes-read) (return))
                          (loop for i from 0 below bytes-read
                                do (vector-push-extend (aref chunk i) response-bytes))
                          (decf bytes-remaining bytes-read))))
             (parse-response-from-bytes
              (coerce response-bytes '(simple-array (unsigned-byte 8) (*)))
              header-end-pos))
            ;; No Content-Length or chunked: parse what we have
            (t
             (parse-response-from-bytes
              (coerce response-bytes '(simple-array (unsigned-byte 8) (*)))
              header-end-pos)))))))

(defun read-tls-response (connection &key method)
  "Read HTTP response from TLS connection"
  (let* ((tls-conn (connection-tls-connection connection))
         (buffer-size 4096)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
         (response-bytes (make-array 0 :element-type '(unsigned-byte 8)
                                       :adjustable t :fill-pointer 0))
         (headers-complete nil)
         (content-length nil)
         ;; HEAD responses have Content-Length but no body
         (skip-body (string-equal method "HEAD")))

    ;; Read headers first
    (loop until headers-complete
          do (let ((bytes-read (crypto:tls-read tls-conn buffer :start 0 :end buffer-size)))
               (when (or (null bytes-read) (zerop bytes-read))
                 (return))
               (when (> bytes-read 0)
                 (loop for i from 0 below bytes-read
                       do (vector-push-extend (aref buffer i) response-bytes))
                 (%check-headers-cap (length response-bytes))
                 ;; Check if headers are complete (CRLFCRLF = 13 10 13 10)
                 (let ((len (length response-bytes)))
                   (when (>= len 4)
                     (loop for i from 0 to (- len 4)
                           when (and (= (aref response-bytes i) 13)
                                     (= (aref response-bytes (+ i 1)) 10)
                                     (= (aref response-bytes (+ i 2)) 13)
                                     (= (aref response-bytes (+ i 3)) 10))
                           do (setf headers-complete t)
                              (let ((headers-str (sb-ext:octets-to-string
                                                  (subseq response-bytes 0 (+ i 4))
                                                  :external-format :utf-8)))
                                (setf content-length (extract-content-length headers-str)))
                              (return)))))))

    ;; Find header-end-pos in bytes (scan for CRLFCRLF)
    (let ((header-end-pos nil))
      (let ((len (length response-bytes)))
        (when (>= len 4)
          (loop for i from 0 to (- len 4)
                when (and (= (aref response-bytes i) 13)
                          (= (aref response-bytes (+ i 1)) 10)
                          (= (aref response-bytes (+ i 2)) 13)
                          (= (aref response-bytes (+ i 3)) 10))
                do (setf header-end-pos (+ i 4))
                   (return))))

      ;; Detect chunked transfer-encoding
      (let ((chunked-p (when header-end-pos
                         (let ((hstr (sb-ext:octets-to-string
                                      (subseq response-bytes 0 header-end-pos)
                                      :external-format :utf-8)))
                           (chunked-encoding-p hstr)))))
        (cond
          ;; Chunked transfer-encoding
          ((and chunked-p (not skip-body) header-end-pos)
           (read-remaining-chunked-data connection response-bytes
                                        :body-start header-end-pos)
           (let* ((raw (coerce response-bytes '(simple-array (unsigned-byte 8) (*))))
                  (decoded-body (decode-chunked-body raw header-end-pos))
                  (result (make-array (+ header-end-pos (length decoded-body))
                                      :element-type '(unsigned-byte 8))))
             (replace result raw :end1 header-end-pos)
             (replace result decoded-body :start1 header-end-pos)
             (parse-response-from-bytes result header-end-pos)))

          ;; Content-Length based reading (but not for HEAD requests)
          ((and content-length (> content-length 0) (not skip-body) header-end-pos)
           (%check-body-cap content-length :content-length)
           (let* ((body-bytes-read (- (length response-bytes) header-end-pos))
                  (bytes-remaining (- content-length body-bytes-read)))
             (when (> bytes-remaining 0)
               (loop while (> bytes-remaining 0)
                     do (let* ((to-read (min bytes-remaining buffer-size))
                               (bytes-read (crypto:tls-read tls-conn buffer :start 0 :end to-read)))
                          (when (and bytes-read (> bytes-read 0))
                            (loop for i from 0 below bytes-read
                                  do (vector-push-extend (aref buffer i) response-bytes))
                            (decf bytes-remaining bytes-read))))))
           (parse-response-from-bytes
            (coerce response-bytes '(simple-array (unsigned-byte 8) (*)))
            header-end-pos))

          ;; No Content-Length or chunked
          (t
           (parse-response-from-bytes
            (coerce response-bytes '(simple-array (unsigned-byte 8) (*)))
            header-end-pos)))))))

(defun parse-response-from-bytes (response-bytes header-end-pos)
  "Parse HTTP response from raw bytes. Body is kept as a byte vector.
   Headers are decoded as UTF-8 (they are always ASCII-compatible).
   Header names are normalized to lowercase per RFC 7230 (HTTP headers are
   case-insensitive). Callers that need a specific wire casing should
   produce it at format time via epsilon.http.headers:canonicalize-name.
   If Content-Encoding is present, the body is decompressed automatically."
  (when (null header-end-pos)
    (return-from parse-response-from-bytes nil))
  (let* ((headers-str (sb-ext:octets-to-string
                       (subseq response-bytes 0 header-end-pos)
                       :external-format :utf-8))
         ;; Extract body as raw bytes
         (body-bytes (when (< header-end-pos (length response-bytes))
                       (subseq response-bytes header-end-pos)))
         ;; Parse headers from the string
         (lines (str:split #\Linefeed headers-str))
         (status-line (seq:first lines))
         (status-parts (seq:realize (str:split #\Space status-line)))
         (status-code (parse-integer (second status-parts)))
         (resp-headers map:+empty+))
    ;; Parse headers, normalizing names to lowercase
    (let ((lines-list (seq:realize lines)))
      (loop for i from 1 below (length lines-list)
            for line = (nth i lines-list)
            do (cond
                 ((or (string= line "") (string= line (string #\Return)))
                  (return))
                 (t
                  (let ((colon-pos (position #\: line)))
                    (when colon-pos
                      (let ((key (string-downcase
                                  (str:trim (subseq line 0 colon-pos))))
                            (value (str:trim (subseq line (1+ colon-pos)))))
                        (setf resp-headers (map:assoc resp-headers key value)))))))))
    ;; Decompress body if Content-Encoding is set
    (let ((content-encoding (map:get resp-headers "content-encoding")))
      (when (and body-bytes content-encoding)
        (handler-case
            (let ((decompressed (decompress-body body-bytes content-encoding)))
              (setf body-bytes decompressed)
              ;; Remove Content-Encoding since body is now decompressed
              (setf resp-headers (map:dissoc resp-headers "content-encoding"))
              ;; Update Content-Length to reflect decompressed size
              (setf resp-headers (map:assoc resp-headers "content-length"
                                            (format nil "~D" (length body-bytes)))))
          (error (e)
            ;; If decompression fails, keep original body
            (warn "Failed to decompress ~A response body: ~A" content-encoding e)))))
    (response:make-response :status status-code :headers resp-headers :body body-bytes)))

(defun parse-response (response-string)
  "Parse HTTP response into status, headers, and body. Header names are
   normalized to lowercase per RFC 7230."
  (let* ((lines (str:split #\Linefeed response-string))
         (status-line (seq:first lines))
         (status-parts (seq:realize (str:split #\Space status-line)))
         (status-code (parse-integer (second status-parts)))
         (resp-headers map:+empty+)
         (body-start nil))

    ;; Parse headers, normalizing names to lowercase
    (let ((lines-list (seq:realize lines)))
      (loop for i from 1 below (length lines-list)
            for line = (nth i lines-list)
          do (cond
               ((or (string= line "") (string= line (string #\Return)))
                (setf body-start (1+ i))
                (return))
               (t
                (let ((colon-pos (position #\: line)))
                  (when colon-pos
                    (let ((key (string-downcase
                               (str:trim (subseq line 0 colon-pos))))
                          (value (str:trim (subseq line (1+ colon-pos)))))
                      (setf resp-headers (map:assoc resp-headers key value))))))))

      ;; Extract body and return http-response object
      (let ((body (when body-start
                    (str:join #\Linefeed (seq:seq (subseq lines-list body-start))))))
        (response:make-response :status status-code :headers resp-headers :body body)))))

(defun request-with-pool (pool host port ssl-p method path query headers body
                          &key timeout)
  "Make an HTTP request using a connection pool for keep-alive reuse.
   Gets a connection from the pool, sends the request with Connection: keep-alive,
   reads the response, and returns the connection to the pool if reusable.
   TIMEOUT (seconds) sets socket recv/send timeouts on the pooled connection."
  (let ((conn (pool:get-connection host port :ssl-p ssl-p :pool pool))
        (force-close nil))
    (unwind-protect
         (let ((http-conn (make-instance 'http-connection
                                         :socket (pool:pooled-connection-socket conn)
                                         :host host
                                         :port port
                                         :ssl-p ssl-p
                                         :tls-connection (pool:pooled-connection-tls-connection conn))))
           ;; Set socket timeouts if requested
           (when (and timeout (pool:pooled-connection-socket conn))
             (let ((timeout-ms (round (* timeout 1000))))
               (handler-case
                   (progn
                     (net:set-socket-option (pool:pooled-connection-socket conn)
                                            :recv-timeout timeout-ms)
                     (net:set-socket-option (pool:pooled-connection-socket conn)
                                            :send-timeout timeout-ms))
                 (error () nil))))
           ;; Override Connection header to keep-alive for pooled requests
           (let* ((base-headers (normalize-caller-headers headers))
                  (keepalive-headers (map:assoc base-headers
                                                "Connection" "keep-alive")))
             (send-request http-conn method path
                           :headers keepalive-headers
                           :body body
                           :query query))
           (let ((resp (read-response http-conn :method method)))
             ;; Check if server wants to close the connection
             (when resp
               (let ((conn-header (map:get (response:response-headers resp)
                                           "connection")))
                 (when (and conn-header
                            (search "close" (string-downcase conn-header)))
                   (setf force-close t))))
             resp))
      ;; Return connection to pool (or close if server requested close)
      (pool:return-connection conn :pool pool :force-close force-close))))

(defun h2-symbol (name)
  "Look up a symbol exported from epsilon.http.h2 by NAME (a string).
   The h2 module is loaded as part of epsilon.http; if for some reason it
   isn't available, signal an error so the caller can fall back."
  (let ((sym (find-symbol name :epsilon.http.h2)))
    (unless sym
      (error "epsilon.http.h2:~A is not available; H2 client is not loaded" name))
    sym))

(defun headers-to-h2-alist (h)
  "Convert a header-map (or plain map) to the alist H2 expects, with
   lowercase header names per RFC 7540."
  (let ((acc nil))
    (cond
      ((null h) nil)
      ((headers:headers-p h)
       (headers:headers-each
        (lambda (k v)
          (push (cons (string-downcase (string k))
                      (if (stringp v) v (princ-to-string v)))
                acc))
        h))
      (t
       (map:each
        (lambda (k v)
          (push (cons (string-downcase (string k))
                      (if (stringp v) v (princ-to-string v)))
                acc))
        h)))
    (nreverse acc)))

(defun h2-response-to-http-response (h2-response)
  "Convert the plist returned by epsilon.http.h2:read-http2-response into a
   standard http-response so callers can't tell the difference. Header
   names are stored in lowercase canonical form, matching the HTTP/1.1
   parse path."
  (let* ((h2-headers (getf h2-response :headers))
         (body (getf h2-response :body))
         (status-pair (assoc ":status" h2-headers :test #'string=))
         (status (if status-pair
                     (parse-integer (cdr status-pair) :junk-allowed t)
                     200))
         (resp-headers map:+empty+))
    (dolist (pair h2-headers)
      (let ((name (car pair)))
        ;; Strip pseudo-headers (`:status`, `:method`, etc.); they aren't
        ;; HTTP headers, they're stream metadata. H2 already lowercases
        ;; on the wire so we just store as-is.
        (unless (and (> (length name) 0) (char= (char name 0) #\:))
          (setf resp-headers
                (map:assoc resp-headers (string-downcase name) (cdr pair))))))
    (response:make-response :status status
                            :headers resp-headers
                            :body body)))

(defun request-h2-on-connection (conn method path headers body)
  "Send an H2 request over an established TLS http-connection and read the
   response. Returns an http-response. Assumes the underlying TLS handshake
   already negotiated h2 via ALPN."
  (let* ((make-conn-fn (fdefinition (h2-symbol "MAKE-HTTP2-CONNECTION")))
         (create-stream-fn (fdefinition (h2-symbol "CREATE-STREAM")))
         (send-headers-fn (fdefinition (h2-symbol "STREAM-SEND-HEADERS")))
         (send-data-fn (fdefinition (h2-symbol "STREAM-SEND-DATA")))
         (read-resp-fn (fdefinition (h2-symbol "READ-HTTP2-RESPONSE")))
         (h2-conn (funcall make-conn-fn (connection-socket conn)
                           :tls-connection (connection-tls-connection conn)
                           :client-p t))
         (stream (funcall create-stream-fn h2-conn))
         (h2-headers (append `((":method" . ,method)
                               (":path" . ,path)
                               (":scheme" . "https")
                               (":authority" . ,(connection-host conn)))
                             (headers-to-h2-alist headers))))
    (funcall send-headers-fn stream h2-headers :end-stream (null body))
    (when body
      (let ((body-bytes (etypecase body
                          (string (sb-ext:string-to-octets body :external-format :utf-8))
                          ((vector (unsigned-byte 8)) body))))
        (funcall send-data-fn stream body-bytes :end-stream t)))
    (h2-response-to-http-response (funcall read-resp-fn h2-conn stream))))

(defun request (url &key (method "GET") headers body tls-context cert-file key-file ca-file
                    alpn-protocols verify-depth session-cache-p
                    proxy-host proxy-port proxy-username proxy-password
                    pool (timeout *default-request-timeout* timeout-supplied-p))
  "Make an HTTP request to URL with full mTLS, HTTP/2, proxy, pooling, and timeout support.

   When the underlying TLS handshake negotiates HTTP/2 via ALPN (i.e., the
   client supplied :alpn-protocols including \"h2\" and the server selected
   it), the request is automatically routed through the H2 client path and
   the response is wrapped in a standard http-response. Otherwise the
   HTTP/1.1 framing path is used.

   Parameters:
   - method: HTTP method (GET, POST, etc.)
   - headers: HTTP headers as a header-map or plain epsilon.map
   - body: Request body
   - tls-context: Pre-configured TLS context
   - cert-file: Client certificate for mutual TLS
   - key-file: Client private key for mutual TLS
   - ca-file: CA certificate for server verification
   - alpn-protocols: ALPN protocols to advertise (default: '(\"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption
   - proxy-host: HTTP proxy hostname
   - proxy-port: HTTP proxy port
   - proxy-username: Proxy authentication username
   - proxy-password: Proxy authentication password
   - pool: Connection pool (connection-pool struct) for keep-alive reuse
   - timeout: Request timeout in seconds.  Defaults to
     *DEFAULT-REQUEST-TIMEOUT* (30s).  Pass :TIMEOUT NIL explicitly to
     wait without a deadline (e.g. long-poll, SSE)."
  (declare (ignore timeout-supplied-p))
  (multiple-value-bind (scheme host port path query)
      (parse-url url)
    (let ((ssl-p (string= scheme "https")))
      (if pool
          ;; Pooled request: use keep-alive, return connection to pool after
          (request-with-pool pool host port ssl-p method path query headers body
                            :timeout timeout)
          ;; Non-pooled request: one-shot connection with Connection: close
          (with-connection (conn host port
                            :ssl-p ssl-p
                            :tls-context tls-context
                            :cert-file cert-file
                            :key-file key-file
                            :ca-file ca-file
                            :alpn-protocols alpn-protocols
                            :verify-depth verify-depth
                            :session-cache-p session-cache-p
                            :proxy-host proxy-host
                            :proxy-port proxy-port
                            :proxy-username proxy-username
                            :proxy-password proxy-password
                            :timeout timeout)
            (cond
              ;; HTTP/2 was negotiated: route through the H2 client path.
              ((and (connection-ssl-p conn)
                    (let ((alpn (connection-alpn-protocol-name conn)))
                      (and alpn (string= alpn "h2"))))
               (request-h2-on-connection conn method path headers body))
              ;; Default: HTTP/1.1 framing
              (t
               (send-request conn method path
                             :headers headers
                             :body body
                             :query query)
               (read-response conn :method method))))))))

(defun get (url &key headers tls-context cert-file key-file ca-file
                 alpn-protocols verify-depth session-cache-p pool
                 (timeout *default-request-timeout*))
  "Make GET request"
  (request url :method "GET" :headers headers
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))

(defun http-post (url &key headers body tls-context cert-file key-file ca-file
                       alpn-protocols verify-depth session-cache-p pool
                       (timeout *default-request-timeout*))
  "Make POST request"
  (request url :method "POST" :headers headers :body body
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))

(defun http-put (url &key headers body tls-context cert-file key-file ca-file
                      alpn-protocols verify-depth session-cache-p pool
                      (timeout *default-request-timeout*))
  "Make PUT request"
  (request url :method "PUT" :headers headers :body body
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))

(defun http-delete (url &key headers tls-context cert-file key-file ca-file
                         alpn-protocols verify-depth session-cache-p pool
                         (timeout *default-request-timeout*))
  "Make DELETE request"
  (request url :method "DELETE" :headers headers
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))

(defun http-head (url &key headers tls-context cert-file key-file ca-file
                       alpn-protocols verify-depth session-cache-p pool
                       (timeout *default-request-timeout*))
  "Make HEAD request"
  (request url :method "HEAD" :headers headers
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))

(defun http-options (url &key headers tls-context cert-file key-file ca-file
                          alpn-protocols verify-depth session-cache-p pool
                          (timeout *default-request-timeout*))
  "Make OPTIONS request"
  (request url :method "OPTIONS" :headers headers
           :tls-context tls-context :cert-file cert-file :key-file key-file
           :ca-file ca-file :alpn-protocols alpn-protocols
           :verify-depth verify-depth :session-cache-p session-cache-p
           :pool pool :timeout timeout))
