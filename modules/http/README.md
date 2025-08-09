# epsilon.http - HTTP Client and Server Library

The `epsilon.http` package provides comprehensive HTTP client and server functionality for epsilon applications. It offers modern features including request/response objects, JSON support, URL parsing, and query string handling.

## Architecture

The epsilon.http package is organized into several core modules:

- **client.lisp** - HTTP client functionality with support for GET, POST, PUT, DELETE, HEAD, and OPTIONS requests
- **server.lisp** - HTTP server with request routing and handler registration
- **request.lisp** - HTTP request parsing and objects
- **response.lisp** - HTTP response creation and formatting
- **net.lisp** - Simple network abstraction layer using SBCL's BSD sockets

## Quick Start

### HTTP Client

```lisp
(use-package :epsilon.http.client)

;; Simple GET request
(multiple-value-bind (status headers body)
    (http-get "http://example.com/api/data")
  (format t "Status: ~A~%" status)
  (format t "Body: ~A~%" body))

;; POST request with JSON data
(let ((json-data (with-output-to-string (s)
                   (epsilon.json:encode 
                    (epsilon.map:make-map "name" "test") s))))
  (multiple-value-bind (status headers body)
      (http-post "http://example.com/api/users"
                 :body json-data
                 :headers (epsilon.map:make-map 
                          "Content-Type" "application/json"))
    (format t "Created user, status: ~A~%" status)))
```

### HTTP Server

```lisp
(use-package :epsilon.http.server)
(use-package :epsilon.http.response)

;; Define request handlers
(define-handler (:get "/hello") (request)
  (text-response "Hello, World!"))

(define-handler (:post "/api/users") (request)
  (let* ((body (epsilon.http.request:request-body request))
         (user-data (when body (epsilon.json:parse body))))
    (json-response (epsilon.map:make-map 
                   "id" 123
                   "message" "User created")
                   :status 201)))

;; Start server
(defparameter *server* (start-server :port 8080))

;; Stop server when done
(stop-server *server*)
```

## HTTP Client API

### Core Functions

#### `(request url &key method headers body)`
Make an HTTP request to the specified URL.

- **url** - Target URL as string
- **method** - HTTP method as string (default: "GET")
- **headers** - Headers as epsilon.map
- **body** - Request body as string

Returns three values: `(status-code headers body)`

#### `(http-get url &key headers)`
Make a GET request.

#### `(http-post url &key headers body)`
Make a POST request.

#### `(http-put url &key headers body)`
Make a PUT request.

#### `(http-delete url &key headers)`
Make a DELETE request.

#### `(http-head url &key headers)`
Make a HEAD request.

#### `(http-options url &key headers)`
Make an OPTIONS request.

### URL Parsing

The client includes built-in URL parsing that handles:
- HTTP and HTTPS schemes
- Custom ports
- Path and query string extraction
- Proper encoding support

Example:
```lisp
(multiple-value-bind (scheme host port path query)
    (epsilon.http.client::parse-url "https://api.example.com:8080/users?active=true")
  ;; scheme: "https"
  ;; host: "api.example.com"  
  ;; port: 8080
  ;; path: "/users"
  ;; query: "active=true"
  )
```

## HTTP Server API

### Server Management

#### `(start-server &key port address)`
Start an HTTP server.

- **port** - Port to listen on (default: 8080)
- **address** - Address to bind to (default: "0.0.0.0")

Returns a server object.

#### `(stop-server port-or-server)`
Stop a running HTTP server.

- **port-or-server** - Either a port number or server object

#### `(with-server (server &key port) &body body)`
Execute body with a temporary HTTP server running.

### Request Routing

#### `(define-handler (method path) (request-var) &body handler-body)`
Define a request handler for a specific HTTP method and path.

- **method** - HTTP method keyword (`:get`, `:post`, etc.)
- **path** - URL path as string
- **request-var** - Variable name for the request object
- **handler-body** - Code to handle the request

Example:
```lisp
(define-handler (:get "/api/status") (req)
  (json-response (epsilon.map:make-map "status" "ok")))
```

## Request and Response Objects

### HTTP Request Object

Request objects provide access to:
- **method** - HTTP method as string
- **path** - URL path
- **headers** - Request headers as epsilon.map
- **body** - Request body as string
- **params** - Query parameters as epsilon.map

Access functions:
```lisp
(epsilon.http.request:request-method request)
(epsilon.http.request:request-path request)
(epsilon.http.request:request-headers request)
(epsilon.http.request:request-body request)
(epsilon.http.request:request-params request)
```

### HTTP Response Object

Response objects support:
- **status** - HTTP status code
- **headers** - Response headers as epsilon.map
- **body** - Response body as string

### Response Creation Functions

#### `(make-response &key status headers body)`
Create a basic HTTP response.

#### `(json-response data &key status)`
Create a JSON response from epsilon data structures.

#### `(html-response html &key status)`
Create an HTML response.

#### `(text-response text &key status)`
Create a plain text response.

#### `(redirect url &key status)`
Create a redirect response.

Example:
```lisp
;; JSON response
(json-response (epsilon.map:make-map "users" '("alice" "bob")) :status 200)

;; HTML response
(html-response "<h1>Welcome</h1><p>Hello, World!</p>")

;; Redirect
(redirect "/login" :status 302)
```

## Query String and Form Processing

### Query String Parsing

```lisp
(epsilon.http.request:parse-query-string "name=john&age=30&active=true")
;; Returns epsilon.map with parsed parameters
```

### URL Decoding

```lisp
(epsilon.http.request::url-decode "hello+world")    ;; => "hello world"
(epsilon.http.request::url-decode "test%40example.com") ;; => "test@example.com"
```

## Error Handling

The HTTP server automatically handles errors and returns appropriate HTTP status codes:

- **400 Bad Request** - For malformed requests
- **404 Not Found** - For unmatched routes
- **500 Internal Server Error** - For handler exceptions

Example error handler:
```lisp
(define-handler (:get "/error-demo") (request)
  (if (some-error-condition)
      (make-response :status 400 
                     :headers (epsilon.map:make-map "Content-Type" "text/plain")
                     :body "Bad Request")
      (text-response "Success")))
```

## Integration with epsilon.json

The epsilon.http package integrates seamlessly with epsilon.json for JSON processing:

```lisp
;; Parse JSON request body
(defun handle-json-post (request)
  (let* ((body (epsilon.http.request:request-body request))
         (data (when body (epsilon.json:parse body))))
    ;; Process data...
    (json-response (epsilon.map:make-map "received" data))))
```

## Limitations and Notes

1. **TLS/HTTPS Support**: Currently requires platform-specific modules that may not be available. HTTP connections work fully.

2. **Connection Pooling**: The current client creates new connections for each request. Future versions may add connection pooling.

3. **Async Operations**: All operations are currently synchronous. The server handles multiple clients using threads.

4. **Request Size Limits**: No built-in request size limits are enforced.

## Testing

The package includes a comprehensive test suite covering:
- URL parsing with various formats
- Request and response object creation
- Query string parsing and URL decoding
- JSON response generation
- HTTP message formatting

Run tests using:
```bash
epsilon test --package epsilon.http
```

## Dependencies

epsilon.http depends on:
- **epsilon.core** - Core epsilon functionality
- **epsilon.json** - JSON encoding/decoding
- **SBCL BSD sockets** - Network functionality

## Thread Safety

The HTTP server is thread-safe and handles multiple concurrent requests using SBCL's threading system. Each client connection is handled in a separate thread.

## Examples

See the `test-http.lisp` file in the epsilon root directory for comprehensive usage examples and working demonstrations of all major features.