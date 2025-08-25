# epsilon.http - HTTP Client and Server Library

The `epsilon.http` package provides comprehensive HTTP client and server functionality for epsilon applications. It offers modern features including a simple high-level API, request/response objects, JSON support, URL parsing, connection pooling, and full HTTP/HTTPS compliance testing.

## Architecture

The epsilon.http package is organized into several core modules:

- **client.lisp** - HTTP client functionality with support for GET, POST, PUT, DELETE, HEAD, and OPTIONS requests
- **server.lisp** - HTTP server with request routing and handler registration
- **request.lisp** - HTTP request parsing and objects
- **response.lisp** - HTTP response creation and formatting
- **net.lisp** - Simple network abstraction layer using SBCL's BSD sockets

## Quick Start

### Simple API (Recommended)

The simple API provides an easy-to-use interface for common HTTP operations:

```lisp
(use-package :epsilon.http)

;; Simple GET request
(let ((response (get "https://api.github.com/users/github")))
  (when (response-ok-p response)
    (let ((data (response-json response)))
      (format t "Name: ~A~%" (epsilon.map:get data "name")))))

;; POST with JSON
(post "https://api.example.com/users" 
      :json '(:name "Alice" :age 30))

;; POST with form data
(post "https://example.com/login"
      :form '(:username "alice" :password "secret"))

;; Custom headers
(get "https://api.example.com/protected"
     :headers '("Authorization" "Bearer my-token"))

;; Download a file
(download-file "https://example.com/document.pdf" 
               "/tmp/document.pdf")
```

### Traditional Client API

```lisp
(use-package :epsilon.http.client)

;; Traditional approach with more control
(multiple-value-bind (status headers body)
    (http-get "http://example.com/api/data")
  (format t "Status: ~A~%" status)
  (format t "Body: ~A~%" body))
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

## Simple API

The simple API provides the easiest way to make HTTP requests:

### One-Liner Functions

#### `(get url &key headers timeout)`
Make a GET request and return a response object.

```lisp
(get "https://api.example.com/data")
(get "https://api.example.com/data" :headers '("Authorization" "Bearer token"))
```

#### `(post url &key json form headers body)`
Make a POST request with automatic content encoding.

```lisp
;; JSON data
(post "https://api.example.com/users" :json '(:name "Alice" :age 30))

;; Form data
(post "https://example.com/login" :form '(:username "alice" :password "secret"))

;; Raw body
(post "https://api.example.com/data" :body "raw data" 
      :headers '("Content-Type" "text/plain"))
```

#### `(put url &key json form headers body)`
Make a PUT request.

```lisp
(put "https://api.example.com/users/1" :json '(:name "Alice Updated"))
```

#### `(delete url &key headers)`
Make a DELETE request.

```lisp
(delete "https://api.example.com/users/1")
```

### Response Helpers

#### `(response-ok-p response)`
Check if response status is 2xx.

```lisp
(let ((resp (get "https://api.example.com/data")))
  (when (response-ok-p resp)
    (process-data resp)))
```

#### `(response-json response)`
Parse response body as JSON, returns epsilon.map.

```lisp
(let* ((resp (get "https://api.github.com/users/github"))
       (data (response-json resp)))
  (epsilon.map:get data "name"))
```

#### `(response-text response)`
Get response body as string.

#### `(response-status response)`
Get HTTP status code.

#### `(response-header response header-name)`
Get specific response header.

### File Operations

#### `(download-file url filepath)`
Download file from URL.

```lisp
(download-file "https://example.com/document.pdf" "/tmp/document.pdf")
```

#### `(upload-file url filepath &key field-name additional-fields)`
Upload file using multipart/form-data.

```lisp
(upload-file "https://api.example.com/upload" "/tmp/document.pdf"
             :field-name "document"
             :additional-fields '(:description "Important file"))
```

## Traditional HTTP Client API

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

The package includes a comprehensive test suite with multiple test categories:

### Test Suites

1. **Basic Tests** - Core functionality tests
2. **Integration Tests** - Local client-server tests  
3. **External Server Tests** - Compliance testing against:
   - httpbin.org (HTTP methods, headers, auth)
   - postman-echo.com (protocols, compression)
   - badssl.com (TLS/SSL certificate validation)
4. **Performance Tests** - Latency and throughput benchmarks
5. **mTLS Tests** - Mutual TLS authentication
6. **Streaming Tests** - Large payload and streaming responses

### Running Tests

```bash
# Run all tests
epsilon --module epsilon.http.test-runner --exec run-all-tests

# Run specific test suites
epsilon --module epsilon.http.test-runner --exec run-basic-tests
epsilon --module epsilon.http.test-runner --exec run-integration-tests
epsilon --module epsilon.http.test-runner --exec run-external-tests

# Skip external tests (for CI/offline testing)
EPSILON_SKIP_EXTERNAL_TESTS=1 epsilon --module epsilon.http.test-runner --exec run-all-tests
```

## Dependencies

epsilon.http depends on:
- **epsilon.core** - Core epsilon functionality
- **epsilon.json** - JSON encoding/decoding
- **SBCL BSD sockets** - Network functionality

## Thread Safety

The HTTP server is thread-safe and handles multiple concurrent requests using SBCL's threading system. Each client connection is handled in a separate thread.

## Examples

See the `test-http.lisp` file in the epsilon root directory for  usage examples and working demonstrations of all major features.