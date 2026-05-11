# epsilon.http

Unified HTTP client and server with HTTP/1.1, HTTP/2, and HTTP/3 support, connection pooling, TLS, and streaming.

## Installation

```lisp
;; In your module definition
(package my-server
  (import (epsilon.http http)))
```

## Features

- **HTTP/1.1, HTTP/2, and HTTP/3** - Automatic protocol negotiation
- **QUIC Transport** - HTTP/3 over UDP with 0-RTT support
- **TLS/SSL** - Certificate validation, client certificates
- **Connection Pooling** - Keep-alive with automatic management
- **Protocol Fallback** - Automatic H3 -> H2 -> H1 fallback
- **Streaming** - Chunked transfer, streaming bodies
- **Middleware** - Composable request/response processing
- **Retry Policies** - Configurable retry with backoff

## Quick Start

### Simple Client

```lisp
;; GET request
(http:get "https://api.example.com/users")

;; POST with JSON
(http:post "https://api.example.com/users"
  :json '(:name "Alice" :email "alice@example.com"))

;; With headers and timeout
(http:get "https://api.example.com/data"
  :headers '(("Authorization" . "Bearer token123"))
  :timeout 30)
```

### Simple Server

```lisp
(defun handler (request)
  (http:response 200
    :headers '(("Content-Type" . "application/json"))
    :body "{\"status\": \"ok\"}"))

(http:serve #'handler :port 8080)
```

## Client API

### Request Functions

Convenience functions for common HTTP methods:

```lisp
;; GET
(http:get url &key headers timeout verify-ssl)

;; POST
(http:post url &key headers body json form timeout verify-ssl)

;; PUT
(http:put url &key headers body json form timeout verify-ssl)

;; PATCH
(http:patch url &key headers body json form timeout verify-ssl)

;; DELETE
(http:delete url &key headers timeout verify-ssl)

;; HEAD
(http:head url &key headers timeout verify-ssl)

;; OPTIONS
(http:options url &key headers timeout verify-ssl)
```

### Full Request Control

```lisp
(http:request url
  :method :post
  :headers '(("Content-Type" . "application/json")
             ("X-Custom" . "value"))
  :body "{\"data\": 123}"
  :timeout 60
  :follow-redirects t
  :max-redirects 5
  :verify-ssl t)
```

### Response Object

```lisp
(let ((response (http:get "https://example.com")))
  (http:response-status response)      ; 200
  (http:response-headers response)     ; Map of headers
  (http:response-body response)        ; Body as string
  (http:response-body-bytes response)) ; Body as byte vector
```

### JSON Helpers

```lisp
;; Auto-encode body as JSON
(http:post url :json '(:key "value"))

;; Parse JSON response
(let ((response (http:get url)))
  (json:parse (http:response-body response)))
```

### Form Data

```lisp
;; URL-encoded form data
(http:post url :form '(:username "alice" :password "secret"))
```

### Streaming

```lisp
;; Stream large responses
(http:request url
  :on-data (lambda (chunk)
             (process-chunk chunk))
  :stream t)
```

### Connection Pooling

```lisp
;; Create a connection pool
(let ((pool (http:make-pool :max-connections 10
                            :max-connections-per-host 5
                            :idle-timeout 60)))
  ;; Use pool for requests
  (http:with-pool pool
    (http:get "https://api.example.com/1")
    (http:get "https://api.example.com/2")
    (http:get "https://api.example.com/3")))
```

### Retry Policies

```lisp
;; Automatic retry with exponential backoff
(http:request url
  :retry-policy (http:make-retry-policy
                  :max-attempts 3
                  :initial-delay 1.0
                  :max-delay 30.0
                  :backoff-multiplier 2.0
                  :retry-statuses '(502 503 504)))
```

## Server API

### Basic Server

```lisp
(defun my-handler (request)
  (let ((method (http:request-method request))
        (path (http:request-path request)))
    (cond
      ((and (eq method :get) (string= path "/"))
       (http:response 200 :body "Hello, World!"))
      ((and (eq method :get) (string= path "/api/status"))
       (http:response 200
         :headers '(("Content-Type" . "application/json"))
         :body "{\"status\": \"ok\"}"))
      (t
       (http:response 404 :body "Not Found")))))

(http:serve #'my-handler :port 8080)
```

### Request Object

```lisp
(defun handler (request)
  (http:request-method request)        ; :GET, :POST, etc.
  (http:request-path request)          ; "/api/users"
  (http:request-query request)         ; Map of query params
  (http:request-headers request)       ; Map of headers
  (http:request-body request)          ; Body string
  (http:request-body-bytes request)    ; Body bytes
  ...)
```

### Response Builder

```lisp
;; Simple response
(http:response 200 :body "OK")

;; With headers
(http:response 200
  :headers '(("Content-Type" . "text/html")
             ("Cache-Control" . "max-age=3600"))
  :body "<html>...</html>")

;; JSON response
(http:json-response '(:users (:alice :bob)))

;; Redirect
(http:redirect "/new-location")
(http:redirect "/new-location" :status 301)  ; Permanent
```

### Middleware

```lisp
;; Logging middleware
(defun logging-middleware (handler)
  (lambda (request)
    (log:info "~A ~A" (http:request-method request) (http:request-path request))
    (let ((response (funcall handler request)))
      (log:info "~D" (http:response-status response))
      response)))

;; CORS middleware
(defun cors-middleware (handler)
  (lambda (request)
    (let ((response (funcall handler request)))
      (http:add-headers response
        '(("Access-Control-Allow-Origin" . "*")
          ("Access-Control-Allow-Methods" . "GET, POST, PUT, DELETE"))))))

;; Compose middleware
(http:serve (-> #'my-handler
                logging-middleware
                cors-middleware)
            :port 8080)
```

### TLS Server

```lisp
(http:serve #'handler
  :port 8443
  :ssl t
  :cert-file "/path/to/cert.pem"
  :key-file "/path/to/key.pem")
```

### Server Options

```lisp
(http:serve #'handler
  :port 8080
  :host "0.0.0.0"              ; Bind address
  :workers 4                    ; Worker threads
  :backlog 128                  ; Connection backlog
  :keep-alive t                 ; Enable keep-alive
  :keep-alive-timeout 60        ; Keep-alive timeout
  :request-timeout 30           ; Request read timeout
  :max-body-size (* 10 1024 1024)) ; 10MB max body
```

### Graceful Shutdown

```lisp
(let ((server (http:serve #'handler :port 8080)))
  ;; ... server running ...
  (http:stop server :timeout 30))  ; Wait up to 30s for connections
```

## HTTP/2 Support

HTTP/2 is automatically negotiated via ALPN for TLS connections:

```lisp
;; Client: HTTP/2 auto-negotiated
(http:get "https://http2.example.com")

;; Server: HTTP/2 enabled by default with TLS
(http:serve #'handler :port 8443 :ssl t ...)
```

### HTTP/2 Specific Features

```lisp
;; Server push (server-side)
(defun handler (request)
  (http:push-promise request "/style.css")
  (http:response 200 :body "<html>...</html>"))

;; Stream priority (client-side)
(http:request url :priority 1)
```

## HTTP/3 Support

HTTP/3 runs over QUIC (UDP) and provides improved performance with 0-RTT connection establishment, no head-of-line blocking, and connection migration.

### Requirements

HTTP/3 requires the ngtcp2 and nghttp3 libraries:

**macOS (Homebrew)**:
```bash
brew install ngtcp2 nghttp3
```

**Linux (NixOS)**:
```nix
environment.systemPackages = with pkgs; [ ngtcp2 nghttp3 ];
```

### Client Usage

```lisp
;; Auto-negotiated (prefers H3 if available)
(http:get "https://cloudflare.com")

;; Force HTTP/3
(http:get "https://cloudflare.com" :protocol :h3)

;; Direct HTTP/3 API
(http.h3:http3-get "https://cloudflare.com")
(http.h3:http3-post "https://api.example.com" :body "{\"data\": 1}")
```

### Protocol Fallback

HTTP/3 automatically falls back to HTTP/2 or HTTP/1.1 on failure:

```lisp
;; Enable automatic fallback (default)
(setf http:*protocol-fallback-enabled* t)

;; Set H3 connection timeout before fallback
(setf http:*h3-connect-timeout* 5)  ; seconds

;; Make request with automatic fallback
(http:make-request-with-fallback "https://example.com"
  :method :get
  :headers '(("Accept" . "application/json")))
```

### Alt-Svc Discovery

The client automatically discovers HTTP/3 support via Alt-Svc headers:

```lisp
;; Parse Alt-Svc header
(http.h3:parse-alt-svc-value "h3=\":443\"; ma=86400")
;; => ((:protocol "h3" :host NIL :port 443 :max-age 86400))

;; Check if H3 is available for a host
(http.h3:h3-available-for-host-p "cloudflare.com")
```

### Connection Pooling

HTTP/3 connections are pooled with stream multiplexing:

```lisp
;; Use pooled connection
(http.h3:h3-pooled-request "https://api.example.com/endpoint"
  :method :get
  :headers '(("Accept" . "application/json")))

;; Get connection from pool
(http.h3:get-h3-connection "api.example.com" 443)

;; Use with-h3-pooled-connection for automatic management
(http.h3:with-h3-pooled-connection (conn "example.com" 443)
  (http.h3:http3-request-on-connection conn "/api/data" :method :get))
```

### Server Usage

```lisp
;; Create HTTP/3 server
(http.h3.server:make-http3-server-instance
  :host "0.0.0.0"
  :port 443
  :cert-file "/path/to/cert.pem"
  :key-file "/path/to/key.pem"
  :handler #'my-handler)

;; Start server
(http.h3.server:start-http3-server server)

;; Stop server
(http.h3.server:stop-http3-server server)
```

### Checking HTTP/3 Availability

```lisp
;; Check if HTTP/3 libraries are available
(http.h3:http3-available-p)  ; => T or NIL

;; Error if not available
(http.h3:ensure-http3-available)
```

## Configuration

### Client Defaults

```lisp
(setf http:*default-timeout* 30)           ; Seconds
(setf http:*default-user-agent* "my-app/1.0")
(setf http:*follow-redirects* t)
(setf http:*verify-ssl* t)
(setf http:*max-redirects* 5)
```

### TLS/SSL Options

```lisp
;; Client with custom CA
(http:get url :ca-file "/path/to/ca-bundle.crt")

;; Client with client certificate
(http:get url
  :cert-file "/path/to/client.crt"
  :key-file "/path/to/client.key")

;; Skip verification (not recommended for production)
(http:get url :verify-ssl nil)
```

## Error Handling

```lisp
(handler-case
    (http:get "https://example.com")
  (http:connection-error (e)
    (format t "Connection failed: ~A" e))
  (http:timeout-error (e)
    (format t "Request timed out: ~A" e))
  (http:ssl-error (e)
    (format t "SSL error: ~A" e)))
```

## See Also

- [epsilon.json](../json/) - JSON parsing for API responses
- [epsilon.crypto](../crypto/) - TLS certificates and encryption
- [examples/web-server.lisp](../../examples/web-server.lisp) - Server example
- [examples/api-client.lisp](../../examples/api-client.lisp) - Client example
