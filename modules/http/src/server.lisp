;;;; epsilon.http.server - HTTP Server Implementation

(defpackage epsilon.http.server
  (:use :cl)
  (:import (epsilon.net net)
            (epsilon.io io)
            (epsilon.string str)
            (epsilon.map map)
            (epsilon.time time)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.http.headers headers)
            (epsilon.crypto crypto)
            (epsilon.scheduler sched)
            (epsilon.scheduler.coroutine coro)
            (epsilon.scheduler.io-wait io-wait)
            (epsilon.log log)
            (epsilon.sys.env env)
            (epsilon.sys.thread thread)
            (epsilon.sys.lock lock)))

(defparameter *default-port* 8080)

(defvar *servers* map:+empty+)

;;; Server header
;;;
;;; Computed lazily on first response and cached thereafter. The platform
;;; segment prefers "NixOS" when /etc/NIXOS exists (the standard NixOS
;;; marker), otherwise falls back to the SBCL/epsilon platform feature.

(defvar *server-header-value* nil
  "Cached value of the 'Server' response header. Computed on first use.")

(defun %detect-server-platform ()
  (cond
    ((probe-file "/etc/NIXOS") "NixOS")
    (t (let ((p (ignore-errors (env:platform))))
         (cond
           ((null p) "Unknown")
           ((keywordp p) (string-capitalize (symbol-name p)))
           (t (princ-to-string p)))))))

(defun server-header-value ()
  "Return the 'Server' header value to advertise, e.g. \"Epsilon/0.15.0 (NixOS)\"."
  (or *server-header-value*
      (setf *server-header-value*
            (format nil "Epsilon/~A (~A)"
                    (or (ignore-errors (env:version)) "unknown")
                    (%detect-server-platform)))))

(defun %headers-have-server-p (resp-headers)
  "True if RESP-HEADERS already carries a Server header in any casing."
  (let ((found nil))
    (block search
      (map:each
       (lambda (k v)
         (declare (ignore v))
         (when (and (stringp k) (string-equal k "Server"))
           (setf found t)
           (return-from search)))
       resp-headers))
    found))

(defun %ensure-server-header (resp-headers)
  "Return RESP-HEADERS unchanged if it already has a Server header,
otherwise add the canonical Server entry."
  (if (%headers-have-server-p resp-headers)
      resp-headers
      (map:assoc resp-headers "Server" (server-header-value))))

(defstruct keepalive-policy
  "Per-server keep-alive tunables. See IMPL-331.

IDLE-TIMEOUT       seconds the server waits between requests on a
                   keepalive connection before closing it.
HEADER-TIMEOUT     seconds to wait for a complete request line +
                   headers once we're actively reading from a client.
                   Bounds slow-loris-style stalls where a client opens
                   a connection but trickles bytes; applied to every
                   request including the first.
TLS-HANDSHAKE-TIMEOUT  seconds to wait for the TLS handshake to
                   complete after TCP accept.  Without this, a client
                   that completes TCP and then never sends ClientHello
                   (or stalls mid-handshake) wedges the carrier for
                   the full kernel TCP timeout.
MAX-REQUESTS       hard cap per connection (nginx parity).
MAX-LIFETIME       seconds, regardless of activity."
  (enabled t :type boolean)
  (idle-timeout 30 :type (integer 1))
  (header-timeout 10 :type (integer 1))
  (tls-handshake-timeout 15 :type (integer 1))
  (max-requests 1000 :type (integer 1))
  (max-lifetime 300 :type (integer 1)))

(defparameter *default-keepalive-policy*
  (make-keepalive-policy :enabled t)
  "Default policy used when start-server is called without :keepalive.
HTTP/1.1 keepalive is on by default as of IMPL-331 step 3.")

(defclass http-server ()
  ((port :initarg :port :accessor server-port)
   (socket :initarg :socket :accessor server-socket)
   (thread :initarg :thread :accessor server-thread)
   (running-p :initform t :accessor server-running-p)
   (tls-context :initarg :tls-context :accessor server-tls-context :initform nil)
   ;; Mutex guarding tls-context reads/writes during hot-swap. Each
   ;; accept dereferences the slot once per connection; `swap-tls-context!`
   ;; replaces the slot under the mutex so readers never observe a
   ;; torn/partial context (IMPL-344 Stage 2).
   (tls-context-mutex :initform (lock:make-lock "http-tls-swap")
                      :accessor server-tls-context-mutex)
   (ssl-p :initarg :ssl-p :accessor server-ssl-p :initform nil)
   (require-client-cert :initarg :require-client-cert :accessor server-require-client-cert :initform nil)
   (alpn-protocols :initarg :alpn-protocols :accessor server-alpn-protocols :initform nil)
   (scheduler :initarg :scheduler :accessor server-scheduler :initform nil)
   (keepalive :initarg :keepalive :accessor server-keepalive
              :initform *default-keepalive-policy*)
   (application :initarg :application :accessor server-application
                :documentation "Middleware pipeline function to handle requests")))

(defun swap-tls-context! (server new-context)
  "Atomically replace SERVER's TLS context with NEW-CONTEXT. Safe to
call from any thread. The next accept() uses the new context; in-flight
TLS sessions keep the context they were handshaken with until they close
naturally. Returns NEW-CONTEXT.

Callers that want to dispose of the previous context must hold a
reference to it before calling swap! -- the swap does not return the
old value."
  (lock:with-lock ((server-tls-context-mutex server))
    (setf (server-tls-context server) new-context))
  new-context)

(defun read-http-headers (reader)
  "Read HTTP headers from buffered reader until double CRLF is found.
   Uses byte-oriented I/O with explicit UTF-8 decoding."
  (log:debug "read-http-headers called")
  (let ((header-buffer (make-array 8192 :element-type '(unsigned-byte 8)
                                        :adjustable t :fill-pointer 0))
        (max-bytes 8192)
        (prev-byte 0)
        (prev-prev-byte 0)
        (prev-prev-prev-byte 0))
    (loop
      (let ((byte (io:read-byte* reader)))
        (unless byte
          ;; EOF reached
          (log:debug "EOF reached while reading headers")
          (return-from read-http-headers
            (when (> (length header-buffer) 0)
              (sb-ext:octets-to-string
               (coerce header-buffer '(simple-array (unsigned-byte 8) (*)))
               :external-format :utf-8))))
        ;; Add byte to buffer
        (vector-push-extend byte header-buffer)
        ;; Check for CRLF CRLF sequence (13 10 13 10)
        (when (and (= byte 10)
                   (= prev-byte 13)
                   (= prev-prev-byte 10)
                   (= prev-prev-prev-byte 13))
          (log:debug "Found end of headers after ~A bytes" (length header-buffer))
          (return-from read-http-headers
            (sb-ext:octets-to-string
             (coerce header-buffer '(simple-array (unsigned-byte 8) (*)))
             :external-format :utf-8)))
        ;; Check size limit
        (when (>= (length header-buffer) max-bytes)
          (log:warn "Header size limit exceeded")
          (return-from read-http-headers nil))
        ;; Shift previous bytes
        (setf prev-prev-prev-byte prev-prev-byte
              prev-prev-byte prev-byte
              prev-byte byte)))))

(defun binary-content-type-p (content-type)
  "Return T if the content type indicates binary data."
  (let ((ct (string-downcase content-type)))
    (or (search "application/octet-stream" ct)
        (search "application/x-epkg" ct)
        (search "application/msgpack" ct)
        (search "application/x-tar" ct)
        (search "application/gzip" ct)
        (search "application/zip" ct)
        (search "image/" ct)
        (search "audio/" ct)
        (search "video/" ct)
        ;; Treat unrecognized application/* as binary, except known text types
        (and (search "application/" ct)
             (not (search "application/json" ct))
             (not (search "application/xml" ct))
             (not (search "application/x-www-form-urlencoded" ct))))))

(defun extract-http-version (request-text)
  "Pick the HTTP version off the request line of REQUEST-TEXT.
Returns a string like \"HTTP/1.1\" or NIL if it cannot be parsed."
  (when request-text
    (let* ((eol (or (position #\Newline request-text) (length request-text)))
           (line (string-right-trim '(#\Return) (subseq request-text 0 eol)))
           (last-space (position #\Space line :from-end t)))
      (when last-space
        (subseq line (1+ last-space))))))

(defun make-connection-source (connection ssl-p)
  "Create the underlying byte source for a connection. For TLS we wrap
the connection in a TLS stream; otherwise we use a TCP reader."
  (if ssl-p
      (crypto:make-tls-stream :connection connection)
      (io:make-tcp-reader connection)))

(defparameter *body-slurp-threshold* (* 64 1024)
  "Bodies whose declared Content-Length is at or below this threshold
are read fully into memory and exposed via REQUEST-BODY (the
historical behavior, kept so JSON / form-urlencoded handlers do not
need to opt in to streaming).  Bodies larger than this threshold are
exposed via REQUEST-BODY-SOURCE -- a thunk the handler must drain in
chunks, with REQUEST-BODY left NIL.

The threshold matters under near-OOM conditions: a 200 MB PUT slurped
into a 1 GB SBCL heap can exceed the live-data ceiling and tip the
runtime into 'Heap exhausted, game over.'  Streaming bodies write
straight through to disk in the handler instead.")

(defparameter *body-stream-chunk-size* (* 64 1024)
  "Chunk size for the streaming body-source thunk.  Each call reads up
to this many bytes off the connection via IO:READ-EXACT.  64 KiB
matches the response-side write buffer and is well above the typical
TLS record size, so we get one TLS record per pump on average.")

(defun %make-bounded-body-source (reader request total)
  "Build the thunk attached to a streaming HTTP request.  Drains
exactly TOTAL bytes from READER over successive calls, returning
fresh byte vectors of up to *BODY-STREAM-CHUNK-SIZE* each.  After
the final chunk the thunk returns NIL and flips
REQUEST's BODY-SOURCE-DRAINED-P flag so the keepalive loop knows
the reader is aligned for the next request."
  (let ((remaining total))
    (lambda ()
      (cond
        ((<= remaining 0)
         (setf (request:request-body-source-drained-p request) t)
         nil)
        (t
         (let* ((n (min remaining *body-stream-chunk-size*))
                (chunk (make-array n :element-type '(unsigned-byte 8))))
           (handler-case
               (io:read-exact reader chunk)
             (error (e)
               (log:error "streaming body-source read failed at remaining=~D: ~A"
                          remaining e)
               (error e)))
           (decf remaining n)
           (when (zerop remaining)
             (setf (request:request-body-source-drained-p request) t))
           chunk))))))

(defun read-http-request-using-reader (reader)
  "Read one HTTP request from READER (a buffered reader). Returns a
parsed http-request with its headers, body, and an internal
\"x-http-version\" header carrying the wire HTTP version.

Bodies up to *BODY-SLURP-THRESHOLD* are read fully into REQUEST-BODY
(byte vector for binary content types, string for text).  Larger
bodies are exposed as a streaming thunk via REQUEST-BODY-SOURCE; the
handler drains it chunk-by-chunk, and the keepalive loop forces a
connection close if the thunk is left undrained."
  (handler-case
      (let ((request-text (read-http-headers reader)))
            (log:debug "Request text length: ~A" (if request-text (length request-text) "NIL"))
              ;; Parse the complete request
              (when request-text
                ;; First parse headers to get content-length
                (let ((parsed-request (request:parse-http-request request-text))
                      (http-version (extract-http-version request-text)))
                  (when (and parsed-request http-version)
                    (request:add-header parsed-request "x-http-version" http-version))
                  (when parsed-request
                    ;; Read body if content-length is specified
                    (let ((content-length-str (map:get (request:request-headers parsed-request)
                                                       "content-length")))
                      (when content-length-str
                        (let ((content-length (ignore-errors (parse-integer content-length-str))))
                          (when (and content-length (> content-length 0))
                            (setf (request:request-content-length parsed-request)
                                  content-length)
                            (cond
                              ((> content-length *body-slurp-threshold*)
                               ;; Streaming path: install a chunked body-source.
                               ;; REQUEST-BODY stays NIL.  The handler must drain
                               ;; the thunk to keep the reader aligned for the
                               ;; next pipelined request; if it doesn't, the
                               ;; keepalive loop in HANDLE-CLIENT closes the
                               ;; connection.
                               (log:debug "Streaming body of ~A bytes (threshold ~A)"
                                          content-length *body-slurp-threshold*)
                               (setf (request:request-body-source parsed-request)
                                     (%make-bounded-body-source reader
                                                                parsed-request
                                                                content-length)))
                              (t
                               (log:debug "Reading body of ~A bytes" content-length)
                               (let ((body-bytes (make-array content-length
                                                             :element-type '(unsigned-byte 8))))
                                 (io:read-exact reader body-bytes)
                                 ;; Keep binary content types as byte vectors;
                                 ;; convert text content to strings
                                 (let* ((content-type (or (map:get
                                                           (request:request-headers parsed-request)
                                                           "content-type")
                                                          ""))
                                        (body (if (binary-content-type-p content-type)
                                                  (progn
                                                    (log:debug "Keeping binary body (~A bytes)" content-length)
                                                    body-bytes)
                                                  (let ((body-string (sb-ext:octets-to-string
                                                                      body-bytes
                                                                      :external-format :utf-8)))
                                                    (log:debug "Read text body: ~S" body-string)
                                                    body-string))))
                                   (setf (request:request-body parsed-request) body)))))))))
                    parsed-request))))
    (error (e)
      (log:error "Error reading HTTP request: ~A (type: ~A)" e (type-of e))
      nil)))  ; Return nil on any error

(defun read-http-request (connection ssl-p)
  "Read HTTP request from CONNECTION, creating a one-shot buffered reader.
Preserved for the proxy frontend, which calls it directly.  New callers
that want to reuse a reader across multiple requests (keepalive) should
use READ-HTTP-REQUEST-USING-READER directly."
  (log:debug "read-http-request called, ssl-p: ~A" ssl-p)
  (let ((source (make-connection-source connection ssl-p)))
    (unless source
      (log:error "Failed to create IO source!")
      (return-from read-http-request nil))
    (read-http-request-using-reader (io:make-buffered-reader source))))

(defun chunk-to-bytes (chunk)
  "Coerce a chunk value (string or byte vector) to a byte vector."
  (etypecase chunk
    (null nil)
    (string (sb-ext:string-to-octets chunk :external-format :utf-8))
    ((vector (unsigned-byte 8)) chunk)))

(defun call-for-each-chunk (body fn)
  "Invoke FN once per chunk in BODY. BODY is one of:
     - a function of zero arguments returning the next chunk or nil for end,
     - a list of chunks,
     - a vector of chunks (sequence)."
  (etypecase body
    (function
     (loop for chunk = (funcall body)
           while chunk
           do (funcall fn chunk)))
    (list
     (dolist (chunk body) (funcall fn chunk)))
    (vector
     ;; Treat string/byte-vector as a single-chunk body, not a sequence of bytes
     (if (or (stringp body) (typep body '(vector (unsigned-byte 8))))
         (funcall fn body)
         (loop for chunk across body do (funcall fn chunk))))))

(defun write-chunked-response (sink response-obj)
  "Write a Transfer-Encoding: chunked response to SINK.
   The headers are emitted with the chunked encoding header instead of
   Content-Length, and the body is iterated chunk-by-chunk."
  (let* ((resp-headers (response:response-headers response-obj))
         (status (response:response-status response-obj))
         ;; Drop any Content-Length the handler may have set, in either
         ;; case form, since chunked supersedes it.
         (resp-headers (map:dissoc resp-headers "Content-Length"))
         (resp-headers (map:dissoc resp-headers "content-length"))
         (resp-headers (map:assoc resp-headers
                                  "Transfer-Encoding" "chunked"))
         (resp-headers (%ensure-server-header resp-headers))
         (headers-text
          (with-output-to-string (out)
            (format out "HTTP/1.1 ~A ~A~C~C"
                    status (response:status-text status)
                    #\Return #\Newline)
            (map:each
             (lambda (key value)
               (format out "~A: ~A~C~C"
                       (headers:canonicalize-name key)
                       value #\Return #\Newline))
             resp-headers)
            (format out "~C~C" #\Return #\Newline)))
         (headers-bytes (sb-ext:string-to-octets headers-text
                                                 :external-format :utf-8)))
    (io:write-all sink headers-bytes)
    ;; Emit each chunk: hex size CRLF data CRLF
    (call-for-each-chunk
     (response:response-body response-obj)
     (lambda (chunk)
       (let ((bytes (chunk-to-bytes chunk)))
         (when (and bytes (> (length bytes) 0))
           (let ((size-line (sb-ext:string-to-octets
                             (format nil "~X~C~C" (length bytes)
                                     #\Return #\Newline)
                             :external-format :utf-8))
                 (crlf (sb-ext:string-to-octets
                        (format nil "~C~C" #\Return #\Newline)
                        :external-format :utf-8)))
             (io:write-all sink size-line)
             (io:write-all sink bytes)
             (io:write-all sink crlf))))))
    ;; Final zero-length chunk + trailing CRLF
    (let ((tail (sb-ext:string-to-octets
                 (format nil "0~C~C~C~C"
                         #\Return #\Newline #\Return #\Newline)
                 :external-format :utf-8)))
      (io:write-all sink tail))
    (io:flush sink)))

(defun make-connection-sink (connection ssl-p)
  "Create the byte sink used to write responses back to a connection."
  (if ssl-p
      (crypto:make-tls-stream :connection connection)
      (io:make-tcp-writer connection)))

(defun send-http-response-using-sink (sink response-obj)
  "Write RESPONSE-OBJ to SINK. Extracted so keepalive callers can reuse
one sink across multiple responses on the same connection.

String bodies are pre-encoded to UTF-8 octets and written through the
same headers-then-body path used for byte bodies; this avoids a second
copy of the body through `response-to-string` plus a second UTF-8
encoding of the combined payload, which on Darwin produced enough
foreign-buffer allocation thrash to wedge the runtime once a connection
delivered more than ~150 KB across multiple keepalive turns."
  (when sink
    (cond
        ;; Chunked response: stream body chunk-by-chunk
        ((response:response-chunked-p response-obj)
         (write-chunked-response sink response-obj))
        (t
         (let* ((raw-body (response:response-body response-obj))
                (body (etypecase raw-body
                        (null nil)
                        ((vector (unsigned-byte 8)) raw-body)
                        (string (sb-ext:string-to-octets
                                 raw-body :external-format :utf-8))))
                (resp-headers (response:response-headers response-obj))
                (status (response:response-status response-obj))
                (resp-headers (if (and body
                                       (not (or (map:get resp-headers "content-length")
                                                (map:get resp-headers "Content-Length"))))
                                  (map:assoc resp-headers "Content-Length"
                                             (format nil "~A" (length body)))
                                  resp-headers))
                (resp-headers (%ensure-server-header resp-headers))
                (headers-text
                 (with-output-to-string (out)
                   (format out "HTTP/1.1 ~A ~A~C~C"
                           status (response:status-text status)
                           #\Return #\Newline)
                   (map:each
                    (lambda (key value)
                      (format out "~A: ~A~C~C"
                              (headers:canonicalize-name key)
                              value #\Return #\Newline))
                    resp-headers)
                   (format out "~C~C" #\Return #\Newline)))
                (headers-bytes (sb-ext:string-to-octets headers-text
                                                        :external-format :utf-8))
                ;; Concatenate headers + body into a single buffer before
                ;; writing. Two separate write-all calls produce two TLS
                ;; records, and clients that issue one `tls-read` see only
                ;; the first. Preserving a one-shot write keeps the response
                ;; framing identical to the pre-fix string path.
                (combined (if body
                              (let ((buf (make-array (+ (length headers-bytes)
                                                        (length body))
                                                     :element-type '(unsigned-byte 8))))
                                (replace buf headers-bytes)
                                (replace buf body :start1 (length headers-bytes))
                                buf)
                              headers-bytes)))
           (io:write-all sink combined)
           (io:flush sink))))))

(defun send-http-response (connection response-obj ssl-p)
  "Send RESPONSE-OBJ on a one-shot sink built from CONNECTION. Preserved
for existing callers; the keepalive loop in handle-client uses
send-http-response-using-sink directly."
  (send-http-response-using-sink (make-connection-sink connection ssl-p)
                                 response-obj))

(defun header-token-has-p (header-value token)
  "Return T if the comma-separated HEADER-VALUE contains TOKEN
case-insensitively. HEADER-VALUE may be NIL."
  (when header-value
    (let ((hv (string-downcase header-value))
          (tok (string-downcase token)))
      (loop for start = 0 then (1+ comma)
            for comma = (position #\, hv :start start)
            for piece = (str:trim (subseq hv start (or comma (length hv))))
            thereis (string= piece tok)
            while comma))))

(defun client-wants-close-p (request)
  "True when the request's HTTP version + Connection header imply the
client wants the connection closed after this response. Defaults match
RFC 7230: HTTP/1.1 defaults to keepalive; earlier versions default to
close unless an explicit \"keep-alive\" token is present."
  (let* ((version (or (map:get (request:request-headers request)
                               "x-http-version")
                      "HTTP/1.1"))
         (conn (map:get (request:request-headers request) "connection")))
    (cond
      ((string= version "HTTP/1.1")
       (header-token-has-p conn "close"))
      (t
       (not (header-token-has-p conn "keep-alive"))))))

(defun server-wants-close-p (response)
  "True when the response already signals Connection: close."
  (header-token-has-p (response:response-header response "Connection")
                      "close"))

(defun mark-response-close (response)
  "Set Connection: close on RESPONSE so the peer knows not to reuse the
connection. Returns RESPONSE."
  (response:set-header response "Connection" "close")
  response)

(defun mark-response-keep-alive (response policy)
  "Advertise keep-alive on an HTTP/1.0 response. HTTP/1.1 clients assume
keep-alive by default so we don't bother there."
  (response:set-header response "Connection" "keep-alive")
  (response:set-header response "Keep-Alive"
                       (format nil "timeout=~D, max=~D"
                               (keepalive-policy-idle-timeout policy)
                               (keepalive-policy-max-requests policy)))
  response)

(defun handle-single-request (req connection ssl-p application)
  "Run APPLICATION against REQ and return the response. Annotates REQ with
mTLS client-cert / negotiated-protocol headers when ssl-p is true."
  (when ssl-p
    (let ((peer-cert (crypto:connection-peer-certificate connection)))
      (when peer-cert
        (let ((cert-subject (getf peer-cert :subject))
              (cert-issuer (getf peer-cert :issuer))
              (cert-fingerprint (getf peer-cert :fingerprint)))
          (setf req (request:add-header req "X-Client-Cert-Subject" cert-subject))
          (setf req (request:add-header req "X-Client-Cert-Issuer" cert-issuer))
          (when cert-fingerprint
            (setf req (request:add-header req "X-Client-Cert-Fingerprint"
                                          cert-fingerprint))))))
    (let ((protocol (crypto:connection-alpn-protocol connection)))
      (when protocol
        (setf req (request:add-header req "X-Negotiated-Protocol" protocol)))))
  (funcall application req))

(defun %connection-fd (connection ssl-p)
  "Best-effort: return CONNECTION's underlying kernel fd, or NIL if
the connection shape doesn't expose one. Used by the keepalive read
deadline to pre-park on fd readiness instead of pinning the carrier
inside an sb-ext:with-timeout protected blocking read."
  (handler-case
      (cond
        ;; TLS connection: the original socket is preserved on the
        ;; tls-connection record; tcp-stream-handle gives the fd.
        ;; Note: park-on-fd on this fd waits for *new TCP bytes*, not
        ;; for a complete TLS record. Mid-record waits still go
        ;; through the TLS read path's own loop.
        (ssl-p
         (let ((sock (and connection
                          (slot-boundp connection 'crypto::socket)
                          (slot-value connection 'crypto::socket))))
           (and sock (net:tcp-stream-handle sock))))
        (t (net:tcp-stream-handle connection)))
    (error () nil)))

(defun %wait-for-readable-or-deadline (connection ssl-p deadline)
  "Block (in coroutine context: park) until CONNECTION's fd is
readable or DEADLINE seconds elapse. Returns T if readable, NIL on
timeout. In non-coroutine context, OR when no platform reactor is
installed (no *fd-wait-register*), returns T and lets the inner
read enforce its own timeout (the legacy shape).

IMPL-398 stage 5: this replaces sb-ext:with-timeout around the
keepalive next-request read. with-timeout uses SIGALRM, which can't
preempt non-interruptible foreign calls (SSL_read, recv); pre-parking
on fd readiness with a real epoll/kqueue deadline gives us a
deadline that actually fires, and lets the carrier yield while we
wait. When no reactor is registered (today's default in production),
we degrade gracefully: the read still happens, the timeout just
isn't enforced at this layer, and the underlying read primitives
hit their own EAGAIN / connection-close handling."
  (let ((fd (%connection-fd connection ssl-p)))
    (cond
      ((and fd coro:*current-coroutine* io-wait:*fd-wait-register*)
       ;; Reactor is registered; park-on-fd is available.
       (handler-case
           (io-wait:park-on-fd fd '(:in) :timeout deadline)
         (error () t)))    ; on unexpected park error, fall through
      (t t))))

(defun handle-client (connection ssl-p application
                      &key require-client-cert
                           (keepalive *default-keepalive-policy*))
  "Handle a client connection using application pipeline with mTLS support.

When KEEPALIVE is enabled, loops reading HTTP/1.1 requests on the same
connection until the client closes, the idle-timeout fires, or a
per-connection limit is hit. When the application returns a 101 response
with a websocket-handler, the handler takes over the connection and
manages its own lifecycle."
  (log:debug "Handle-client called with connection: ~A, ssl-p: ~A, keepalive: ~A"
             connection ssl-p (keepalive-policy-enabled keepalive))
  (let ((ws-takeover nil)
        (reader nil)
        (sink nil)
        (start-time (get-universal-time)))
    (handler-case
        (progn
          ;; For mTLS, verify client certificate was provided if required
          (when (and ssl-p require-client-cert)
            (let ((peer-cert (crypto:connection-peer-certificate connection)))
              (unless peer-cert
                (error "Client certificate required but not provided"))
              (multiple-value-bind (valid-p error-msg)
                  (crypto:verify-peer-certificate connection)
                (unless valid-p
                  (error "Client certificate verification failed: ~A" error-msg)))))

          (let ((source (make-connection-source connection ssl-p)))
            (unless source
              (error "Failed to create IO source"))
            (setf reader (io:make-buffered-reader source)))
          (setf sink (make-connection-sink connection ssl-p))

          (block keepalive-loop
            (loop
              for request-index from 0
              ;; Read deadline:
              ;;   * Subsequent (keepalive) requests use IDLE-TIMEOUT,
              ;;     since most of the wait is between requests.
              ;;   * The first request and any non-keepalive case use
              ;;     HEADER-TIMEOUT, the slow-loris bound.
              ;; HEADER-TIMEOUT also caps the idle wait when keepalive
              ;; is enabled but IDLE-TIMEOUT < HEADER-TIMEOUT (rare).
              for read-deadline = (if (and (keepalive-policy-enabled keepalive)
                                           (> request-index 0))
                                      (keepalive-policy-idle-timeout keepalive)
                                      (keepalive-policy-header-timeout keepalive))
              for req = (cond
                          ;; Pre-park on fd readiness with the deadline
                          ;; before attempting the read. In coroutine
                          ;; context this yields the carrier back to its
                          ;; loop while we wait. See IMPL-398 stage 5
                          ;; and %wait-for-readable-or-deadline above.
                          ((not (%wait-for-readable-or-deadline
                                 connection ssl-p read-deadline))
                           (log:debug "Read timeout after ~Ds (request-index=~A)"
                                      read-deadline request-index)
                           nil)
                          (t
                           (handler-case
                               (read-http-request-using-reader reader)
                             (error (e)
                               (log:debug "Read failed: ~A" e)
                               nil))))
              do (cond
                   ;; No more requests: client closed cleanly or I/O failed.
                   ((null req)
                    (when (zerop request-index)
                      (log:debug "No request read on fresh connection"))
                    (return-from keepalive-loop))
                   (t
                    (let* ((response (handle-single-request
                                      req connection ssl-p application))
                           (streaming-body-leftover-p
                            ;; If the request had a streaming body-source
                            ;; and the handler did not fully drain it, the
                            ;; reader's position no longer aligns with the
                            ;; start of the next request and keepalive is
                            ;; unsafe.  Force a close.  S3 PUT always
                            ;; drains; this is the safety net.
                            (and (request:request-body-source req)
                                 (not (request:request-body-source-drained-p req))))
                           (last-p
                            (or (not (keepalive-policy-enabled keepalive))
                                (client-wants-close-p req)
                                (server-wants-close-p response)
                                streaming-body-leftover-p
                                (>= (1+ request-index)
                                    (keepalive-policy-max-requests keepalive))
                                (>= (- (get-universal-time) start-time)
                                    (keepalive-policy-max-lifetime keepalive)))))
                      (when streaming-body-leftover-p
                        (log:warn "Streaming request body not fully drained ~
                                   (handler returned with bytes remaining); ~
                                   forcing connection close to keep reader ~
                                   aligned"))
                      (cond
                        ((and (= (response:response-status response) 101)
                              (response:response-websocket-handler response))
                         (send-http-response-using-sink sink response)
                         (setf ws-takeover (response:response-websocket-handler response))
                         (return-from keepalive-loop))
                        (last-p
                         (mark-response-close response)
                         (send-http-response-using-sink sink response)
                         (return-from keepalive-loop))
                        (t
                         ;; HTTP/1.0 keep-alive: the peer wants an explicit
                         ;; Connection: keep-alive header on the response.
                         (let ((ver (map:get (request:request-headers req)
                                             "x-http-version")))
                           (when (and (stringp ver) (string= ver "HTTP/1.0"))
                             (mark-response-keep-alive response keepalive)))
                         (send-http-response-using-sink sink response)))))))))
      (error (e)
        (log:error "Error handling client: ~A" e)
        (ignore-errors
          (send-http-response connection
                              (response:html-response
                               (format nil "<h1>500 Internal Server Error</h1><p>~A</p>" e)
                               :status 500)
                              ssl-p))))

    ;; WebSocket takeover: handler owns the connection lifecycle
    (when ws-takeover
      (handler-case
          (funcall ws-takeover connection ssl-p)
        (error (e)
          (log:error "WebSocket handler error: ~A" e)))
      (return-from handle-client))

    ;; Close connection properly (normal HTTP path).  For TLS,
    ;; crypto:tls-close now also closes the underlying TCP fd (it didn't
    ;; before, which leaked one CLOSE-WAIT socket per served request --
    ;; eventually saturating the http server's accept loop and the
    ;; epoll reactor).  For plain TCP we need tcp-close, not
    ;; tcp-shutdown: tcp-shutdown sends FIN and unregisters from epoll
    ;; but leaves the fd open.
    (log:debug "Closing connection...")
    (handler-case
        (if ssl-p
            (crypto:tls-close connection)
            (net:tcp-close connection))
      (error (e)
        (log:debug "Connection close error (expected during teardown): ~A" e)))
    (log:debug "Connection closed")))

(defparameter *overflow-response-bytes*
  (sb-ext:string-to-octets
   (format nil
           "HTTP/1.1 503 Service Unavailable~C~CContent-Type: text/plain~C~CContent-Length: 21~C~CConnection: close~C~C~C~CServer overloaded~C~C"
           #\Return #\Newline
           #\Return #\Newline
           #\Return #\Newline
           #\Return #\Newline
           #\Return #\Newline
           #\Return #\Newline)
   :external-format :utf-8)
  "Pre-encoded plaintext 503 emitted on the accept thread when the
scheduler refuses a submission (carrier queue full).  We ship a body so
clients see why the connection terminated rather than a bare RST.")

(defun %shed-connection (raw-connection ssl-p)
  "Politely close RAW-CONNECTION when the scheduler is saturated.  For
plaintext we attempt a one-shot 503; for TLS the handshake hasn't
happened yet, so we just close the raw fd (the client retries or
times out).  All errors are swallowed -- shedding never raises."
  (handler-case
      (progn
        (unless ssl-p
          (handler-case
              (let ((sink (io:make-tcp-writer raw-connection)))
                (io:write-all sink *overflow-response-bytes*)
                (io:flush sink))
            (error () nil)))
        (handler-case (net:tcp-close raw-connection)
          (error () nil)))
    (error () nil)))

(defun server-loop (server)
  "Main server loop.  Blocks on tcp-accept until a connection arrives or the
listener is closed.  Each accepted connection is dispatched to the scheduler
as a coroutine; if the scheduler is saturated (per-carrier incoming queue at
its cap), the connection is shed with a 503 instead of being submitted.  Falls
back to a raw OS thread when no scheduler is present."
  (log:debug "Server loop started for port ~A" (server-port server))
  (let ((scheduler (server-scheduler server)))
    (loop while (server-running-p server)
          do (handler-case
                 (let ((raw-connection (net:tcp-accept (server-socket server))))
                   (when raw-connection
                     (log:debug "Accepted connection: ~A" raw-connection)
                     (let ((ssl-p (server-ssl-p server))
                           ;; Read the live context under the swap mutex
                           ;; so a concurrent swap! cannot tear the slot
                           ;; mid-deref. Each handler thread captures the
                           ;; context at accept time and keeps it for the
                           ;; lifetime of the connection.
                           (tls-ctx
                             (lock:with-lock
                                 ((server-tls-context-mutex server))
                               (server-tls-context server)))
                           (app (server-application server))
                           (require-cert (server-require-client-cert server))
                           (policy (server-keepalive server)))
                       (flet ((handle-connection ()
                                (let ((connection
                                        (cond
                                          ((not ssl-p) raw-connection)
                                          (t
                                           ;; Bound the handshake so a stalled
                                           ;; client cannot wedge the carrier
                                           ;; for the full kernel TCP timeout.
                                           (handler-case
                                               (sb-ext:with-timeout
                                                   (keepalive-policy-tls-handshake-timeout
                                                    policy)
                                                 (crypto:tls-accept raw-connection
                                                                    tls-ctx))
                                             (sb-ext:timeout ()
                                               (log:warn
                                                "TLS handshake timeout after ~Ds; closing"
                                                (keepalive-policy-tls-handshake-timeout
                                                 policy))
                                               (handler-case
                                                   (net:tcp-close raw-connection)
                                                 (error () nil))
                                               (return-from handle-connection)))))))
                                  (handle-client connection ssl-p app
                                                 :require-client-cert require-cert
                                                 :keepalive policy))))
                         (cond
                           ((null scheduler)
                            (thread:make-thread #'handle-connection
                                                :name "HTTP client handler"))
                           ((sched:scheduler-try-submit scheduler #'handle-connection))
                           (t
                            ;; Round-robin target carrier was at its cap.
                            ;; Shed the connection with a 503 (or close-only
                            ;; for TLS) and continue accepting.
                            (log:warn-with-fields
                             "shed connection (scheduler saturated)"
                             (log:fields :event "http.shed"
                                         :ssl-p ssl-p
                                         :overflow-count
                                         (sched:scheduler-overflow-count scheduler)
                                         :queue-depth
                                         (sched:scheduler-submit-queue-depth scheduler)))
                            (%shed-connection raw-connection ssl-p))))) ))
               (error (e)
                 (log:error "Error in server loop: ~A" e)
                 (unless (server-running-p server)
                   (return)))))))

(defun start-server (application &key (port *default-port*) (address "0.0.0.0")
                                 tls-context ssl-p cert-file key-file ca-file
                                 require-client-cert alpn-protocols verify-depth
                                 session-cache-p scheduler
                                 (keepalive *default-keepalive-policy*))
  "Start HTTP/HTTPS server with full mTLS and HTTP/2 support.
   Parameters:
   - application: Request handler function
   - port: Server port
   - address: Bind address
   - tls-context: Pre-configured TLS context
   - ssl-p: Enable SSL/TLS
   - cert-file: Server certificate file
   - key-file: Server private key file
   - ca-file: CA certificates for client verification (mTLS)
   - require-client-cert: Require client certificates (mTLS)
   - alpn-protocols: ALPN protocols to support (default: '(\"h2\" \"http/1.1\"))
   - verify-depth: Certificate chain verification depth
   - session-cache-p: Enable session resumption

   Hot-reloading a TLS context after startup is supported via
   `swap-tls-context!`. See epsilon.cert-manager:attach-to-server
   for the bridge that wires an in-process ACME renewal loop to this
   server's swap mutex."
  (declare (ignore verify-depth))
  (log:debug "Starting server on ~A:~A" address port)
  (when (map:get *servers* port)
    (error "Server already running on port ~D" port))
  ;; Note: *servers* is a regular epsilon.map keyed by port (integer);
  ;; case folding does not apply.

  ;; Create TLS context if SSL is requested
  (when (or ssl-p cert-file key-file)
    (unless tls-context
      (setf tls-context (crypto:make-server-context
                          :cert-file cert-file
                          :key-file key-file
                          :ca-file ca-file
                          :verify-mode (if require-client-cert
                                          (logior crypto:+verify-peer+
                                                 crypto:+verify-fail-if-no-peer-cert+)
                                        crypto:+verify-none+)
                          :require-client-cert require-client-cert
                          :session-cache-p session-cache-p))))

  (let* ((addr (net:make-socket-address address port))
         (listener (net:tcp-bind addr :reuse-addr t))
         ;; Get actual port (important when port=0 for dynamic allocation)
         (actual-port (net:socket-address-port (net:tcp-local-addr listener))))
    (let ((server (make-instance 'http-server
                                 :port actual-port
                                 :socket listener
                                 :tls-context tls-context
                                 :ssl-p (or ssl-p (not (null tls-context)))
                                 :require-client-cert require-client-cert
                                 :alpn-protocols alpn-protocols
                                 :scheduler scheduler
                                 :keepalive keepalive
                                 :application application)))
      (setf (server-thread server)
            (thread:make-thread
             (lambda ()
               (server-loop server))
             :name (format nil "HTTP server on port ~D" actual-port)))

      (setf *servers* (map:assoc *servers* actual-port server))
      server)))

(defun stop-server (port-or-server &key (timeout 5))
  "Stop HTTP server. TIMEOUT is the maximum seconds to wait for the
   accept thread to exit (default 5). If the thread does not exit
   within the timeout, it is abandoned to avoid blocking callers."
  (let ((server (if (typep port-or-server 'http-server)
                    port-or-server
                  (map:get *servers* port-or-server))))
    (when server
      (setf (server-running-p server) nil)
      (net:tcp-close (server-socket server))
      (handler-case
          (sb-ext:with-timeout timeout
            (thread:join-thread (server-thread server)))
        (sb-ext:timeout ()
          (log:warn "Server thread on port ~D did not exit within ~Ds"
                    (server-port server) timeout)))
      (setf *servers* (map:dissoc *servers* (server-port server)))
      t)))

(defmacro with-server ((server application &key (port *default-port*)) &body body)
  "Execute body with HTTP server running"
  `(let ((,server (start-server ,application :port ,port)))
     (unwind-protect
         (progn ,@body)
       (stop-server ,server))))

(defun wrap-middleware (handler &rest middlewares)
  "Wrap handler with multiple middleware functions"
  (if middlewares
      ;; Apply middleware functions in reverse order (innermost first)
      (reduce (lambda (h mw) (funcall mw h))
	      (reverse middlewares)
	      :initial-value handler)
    handler))
