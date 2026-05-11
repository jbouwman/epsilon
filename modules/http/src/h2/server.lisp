;;;; HTTP/2 Server Implementation
;;;;
;;;; Implements an HTTP/2 server that can pass h2spec conformance tests
;;;;
;;;; This file is part of the unified HTTP module (epsilon.http.h2)

(defpackage :epsilon.http.h2.server
  (:use :cl)
  (:import
   (epsilon.net net)
   (epsilon.crypto crypto)
   (epsilon.log log)
   (epsilon.scheduler sched)
   (epsilon.channel ch)
   (epsilon.http.server http-server)
   (epsilon.sys.thread thread)
   (epsilon.sys.lock lock))
  (:export
   ;; Server configuration
   #:http2-server-config
   #:make-http2-server-config
   #:http2-server-config-port
   #:http2-server-config-host
   #:http2-server-config-ssl-p
   #:http2-server-config-cert-file
   #:http2-server-config-key-file
   #:http2-server-config-handler
   #:http2-server-config-http1-application
   #:http2-server-config-max-concurrent-streams
   #:http2-server-config-initial-window-size
   #:http2-server-config-max-frame-size
   #:http2-server-config-max-header-list-size
   #:http2-server-config-enable-push
   #:http2-server-config-ca-file
   #:http2-server-config-require-client-cert
   #:http2-server-config-verify-depth
   #:http2-server-config-scheduler

   ;; Server state
   #:http2-server
   #:http2-server-running-p

   ;; Server functions
   #:start-http2-server
   #:stop-http2-server
   #:swap-tls-context!
   #:default-handler

   ;; Constants
   #:*client-preface*))

;;;; Server Configuration

(defstruct http2-server-config
  "HTTP/2 server configuration"
  (port 8080 :type integer)
  (host "0.0.0.0" :type string)
  (ssl-p nil :type boolean)
  (cert-file nil :type (or null string))
  (key-file nil :type (or null string))
  (ca-file nil :type (or null string))
  (require-client-cert nil :type boolean)
  ;; Soft mTLS: ask for a client cert during handshake but accept
  ;; connections without one. The cert (if any) reaches handlers via
  ;; the X-Client-Cert-* headers injected by handle-single-request.
  ;; Mutually exclusive with require-client-cert; prefer this when the
  ;; application authorises off the cert fingerprint rather than the
  ;; chain (e.g. an admin allowlist's allowlist gate).
  (request-client-cert nil :type boolean)
  (verify-depth 4 :type integer)
  (handler nil :type (or null function))
  ;; HTTP/1.1 fallback. When the client negotiates ALPN "http/1.1" (or
  ;; does not offer ALPN at all on a TLS connection), we route the
  ;; socket into epsilon.http.server:handle-client using this
  ;; application. Contract: (request) -> response, same shape as the
  ;; top-level HTTP server. NIL means reject non-h2 connections.
  (http1-application nil :type (or null function))
  (scheduler nil)
  (max-concurrent-streams 100 :type integer)
  (initial-window-size 65535 :type integer)
  (max-frame-size 16384 :type integer)
  (max-header-list-size 8192 :type integer)
  (enable-push t :type boolean))

;;;; Connection Preface

(defparameter *client-preface*
  #(80 82 73 32 42 32 72 84 84 80 47 50 46 48 13 10 13 10 83 77 13 10 13 10)
  "HTTP/2 client connection preface")

;;;; Forward references (these will be resolved at load time)
(declaim (ftype (function (t &key (:tls-connection t) (:client-p t)) t) h2-make-connection))
(declaim (ftype (function (t t) t) h2-handle-connection))

;;;; Server state

(defstruct http2-server
  "Running HTTP/2 server state."
  (config nil)
  (socket nil)
  (tls-context nil)
  ;; Mutex guarding tls-context reads/writes during hot-swap. The
  ;; accept loop dereferences the slot once per connection under this
  ;; mutex; `swap-tls-context!' replaces the slot under the same mutex
  ;; so readers never observe a torn/partial context. Mirrors the
  ;; HTTP/1 server's hot-swap shape (epsilon.http.server) so callers
  ;; like cert-manager can drive both with the same external API.
  (tls-context-mutex (lock:make-lock "h2-tls-swap"))
  (thread nil)
  (running-p nil :type boolean))

;;;; Server Implementation

(defun start-http2-server (&key (port 8080)
                               (host "0.0.0.0")
                               ssl-p
                               cert-file
                               key-file
                               ca-file
                               require-client-cert
                               request-client-cert
                               (verify-depth 4)
                               handler
                               http1-application
                               scheduler)
  "Start an HTTP/2 server.  Returns an http2-server struct for graceful shutdown.

HTTP1-APPLICATION, when provided, is a (request)->response handler used
for clients that negotiated ALPN \"http/1.1\" (or didn't negotiate ALPN
at all). The socket is dispatched to epsilon.http.server:handle-client
with this application. When HTTP1-APPLICATION is NIL, connections that
aren't HTTP/2 are closed."
  (let ((config (make-http2-server-config
                 :port port
                 :host host
                 :ssl-p ssl-p
                 :cert-file cert-file
                 :key-file key-file
                 :ca-file ca-file
                 :require-client-cert require-client-cert
                 :request-client-cert request-client-cert
                 :verify-depth verify-depth
                 :handler (or handler #'default-handler)
                 :http1-application http1-application
                 :scheduler scheduler)))

    (log:info "Starting HTTP/2 server on ~A:~D" host port)

    (let* ((server-socket (create-server-socket config))
           (tls-context (make-tls-context-from-config config))
           (server (make-http2-server
                    :config config
                    :socket server-socket
                    :tls-context tls-context
                    :running-p t)))
      (setf (http2-server-thread server)
            (thread:make-thread
             (lambda ()
               (h2-accept-loop server))
             :name (format nil "H2 server on port ~D" port)))
      server)))

(defun h2-accept-loop (server)
  "Accept loop for HTTP/2 server.  Blocks until a connection arrives or the
listener is closed.

The TLS context is re-read from the server slot on every iteration so
that `swap-tls-context!' (called by cert-manager when an external
agent rotates the cert files) takes effect on the next accepted
connection. In-flight TLS sessions keep the context they handshook
with until they close naturally."
  (let ((config (http2-server-config server))
        (scheduler (http2-server-config-scheduler (http2-server-config server))))
    (unwind-protect
         (loop while (http2-server-running-p server) do
           (handler-case
               (let ((client-socket (net:tcp-accept (http2-server-socket server))))
                 (when client-socket
                   ;; Capture the live context under the swap mutex so
                   ;; a concurrent swap! cannot tear the slot mid-deref.
                   ;; Each handler thread keeps its captured context
                   ;; for the lifetime of the connection.
                   (let* ((tls-context
                            (lock:with-lock
                                ((http2-server-tls-context-mutex server))
                              (http2-server-tls-context server)))
                          (connection (if tls-context
                                         (crypto:tls-accept client-socket tls-context)
                                         client-socket)))
                     (flet ((handle ()
                              (handle-connection connection config)))
                       (if scheduler
                           (sched:scheduler-submit scheduler #'handle)
                           (ch:spawn (handle)))))))
             (error (e)
               (when (http2-server-running-p server)
                 (log:error "H2 accept error: ~A" e)))))
      ;; Cleanup: the listener lives in http2-server-socket, so close
      ;; it via tcp-close (which handles both listener and stream).
      ;; Do NOT use close-socket here -- that helper is for client
      ;; streams and delegates to tcp-shutdown, which historically
      ;; did not accept listeners.
      (handler-case (net:tcp-close (http2-server-socket server))
        (error () nil)))))

(defun swap-tls-context! (server new-context)
  "Atomically replace SERVER's TLS context with NEW-CONTEXT. Safe to
call from any thread. The next accepted connection uses the new
context; in-flight TLS sessions keep the context they handshook with
until they close naturally. Returns NEW-CONTEXT.

Mirrors `epsilon.http.server:swap-tls-context!' so cert-manager can
drive both HTTP/1 and HTTP/2 servers through the same external API.
Callers that want to dispose of the previous context must hold a
reference to it before calling swap! -- the swap does not return the
old value."
  (lock:with-lock ((http2-server-tls-context-mutex server))
    (setf (http2-server-tls-context server) new-context))
  new-context)

(defun stop-http2-server (server)
  "Stop the HTTP/2 server gracefully."
  (when (http2-server-running-p server)
    (setf (http2-server-running-p server) nil)
    ;; Close the listener to wake any blocked tcp-accept via shutdown pipe
    (handler-case (net:tcp-close (http2-server-socket server))
      (error () nil))
    (when (http2-server-thread server)
      (handler-case
          (thread:join-thread (http2-server-thread server) :timeout 10)
        (error () nil))
      (setf (http2-server-thread server) nil))
    (log:info "HTTP/2 server stopped")))

(defun create-server-socket (config)
  "Create a server socket (plain TCP listener)"
  (let ((address (net:make-socket-address
                  (http2-server-config-host config)
                  (http2-server-config-port config))))
    (net:tcp-bind address :backlog 128)))

(defun make-tls-context-from-config (config)
  "Create a TLS server context from config if TLS is enabled.
   Returns the context, or NIL if TLS is not configured.

Advertises ALPN \"h2\" plus \"http/1.1\" iff a fallback application is
configured. Without a fallback there's no point offering http/1.1 --
the client would negotiate it, speak HTTP/1.1, and hit a preface
mismatch on the h2 path."
  (when (http2-server-config-ssl-p config)
    (let* ((tls-context (crypto:make-server-context
                         :cert-file (http2-server-config-cert-file config)
                         :key-file (http2-server-config-key-file config)
                         :ca-file (http2-server-config-ca-file config)
                         :require-client-cert (http2-server-config-require-client-cert config)
                         :request-client-cert (http2-server-config-request-client-cert config)))
           (alpn (if (http2-server-config-http1-application config)
                     '("h2" "http/1.1")
                     '("h2"))))
      (crypto:context-set-alpn-protocols tls-context alpn)
      tls-context)))

(defun accept-connection (server-socket tls-context)
  "Accept a new connection, optionally wrapping with TLS"
  (let ((client-socket (net:tcp-accept server-socket)))
    (if tls-context
        (crypto:tls-accept client-socket tls-context)
        client-socket)))

(defun close-socket (socket)
  "Close a socket"
  (net:tcp-shutdown socket :how :both))

(defun handle-connection (socket config)
  "Dispatch an accepted connection to HTTP/2 or HTTP/1.1 based on ALPN.

- TLS + ALPN \"h2\"       -> HTTP/2
- TLS + ALPN \"http/1.1\" -> HTTP/1.1 fallback (if configured)
- TLS + no ALPN          -> HTTP/1.1 fallback (if configured); many older
                            clients don't send ALPN. Falling back is more
                            forgiving than dropping them silently.
- plain TCP              -> HTTP/2 (h2c-style prior-knowledge)

When the HTTP/1.1 fallback path is taken, epsilon.http.server:handle-client
owns the socket lifecycle (it closes on the way out). The HTTP/2 path
closes the socket here after the frame loop ends."
  (let* ((is-tls (crypto:tls-connection-p socket))
         (alpn (when is-tls
                 (ignore-errors (crypto:connection-alpn-protocol socket))))
         (http1-app (http2-server-config-http1-application config))
         (prefer-http1 (and is-tls
                            http1-app
                            (or (null alpn)
                                (string-equal alpn "http/1.1")))))
    (log:info "connection established (~A~@[, ALPN=~A~])"
              (if is-tls "TLS" "plain") alpn)
    (cond
      (prefer-http1
       (log:debug "dispatching to HTTP/1.1 fallback handler")
       (handler-case
           (http-server::handle-client
            socket is-tls http1-app
            :require-client-cert (http2-server-config-require-client-cert config))
         (error (e)
           (log:error "HTTP/1.1 handler error: ~A" e))))
      ((and is-tls (null http1-app) (not (or (null alpn) (string-equal alpn "h2"))))
       (log:warn "peer negotiated ALPN ~S but no HTTP/1.1 fallback configured; closing" alpn)
       (handler-case (crypto:tls-close socket) (error () nil)))
      (t
       (%handle-h2-connection socket is-tls config)))))

(defun %handle-h2-connection (socket is-tls config)
  "Run the HTTP/2 state machine on SOCKET and close when it's done."
  (let* ((make-conn-fn (fdefinition (find-symbol "MAKE-HTTP2-CONNECTION" :epsilon.http.h2)))
         (handle-conn-fn (fdefinition (find-symbol "HANDLE-HTTP2-CONNECTION" :epsilon.http.h2)))
         (conn (funcall make-conn-fn
                        socket
                        :tls-connection (when is-tls socket)
                        :client-p nil))
         (handler (http2-server-config-handler config)))
    (handler-case
        (funcall handle-conn-fn conn handler)
      (error (e)
        (log:warn "HTTP/2 connection error: ~A" e)))
    (handler-case
        (if is-tls
            (crypto:tls-close socket)
            (close-socket socket))
      (error () nil))))

(defun default-handler (headers body)
  "Default request handler"
  (declare (ignore body))
  (format t "Received request with headers: ~A~%" headers)
  (list :status 200
        :headers (list (cons "content-type" "text/plain"))
        :body "Hello from HTTP/2 server!"))
