# Epsilon Application Development Guide

This guide addresses real-world application development with Epsilon, covering multi-module systems, external protocols, and complex application patterns.

## Multi-Module System Structure

### Recommended Directory Layout

```
my-application/
├── epsilon                # Self-contained epsilon binary
├── scripts/
│   └── epsilon.lisp       # Bootstrap loader
├── module/
│   ├── core/              # Application core logic
│   │   ├── package.edn
│   │   ├── src/
│   │   │   ├── app/
│   │   │   │   ├── config.lisp
│   │   │   │   ├── main.lisp
│   │   │   │   └── routes.lisp
│   │   │   └── lib/
│   │   │       ├── auth.lisp
│   │   │       └── database.lisp
│   │   └── tests/
│   │       └── app/
│   ├── api/               # HTTP API module
│   │   ├── package.edn
│   │   ├── src/
│   │   │   └── api/
│   │   │       ├── handlers.lisp
│   │   │       ├── middleware.lisp
│   │   │       └── validation.lisp
│   │   └── tests/
│   └── workers/           # Background processing
│       ├── package.edn
│       ├── src/
│       │   └── workers/
│       │       ├── job-queue.lisp
│       │       └── schedulers.lisp
│       └── tests/
└── target/               # Build artifacts (auto-generated)
```

### Module Definition (package.edn)

**Core Module:**
```edn
{
  "name" "my-app.core"
  "version" "1.0.0" 
  "description" "Application core functionality"
  "sources" ["src"]
  "tests" ["tests"]
  "dependencies" ["epsilon.core"]
  "provides" ["my-app.config", "my-app.main", "my-app.lib.auth", "my-app.lib.database"]
}
```

**API Module:**
```edn
{
  "name" "my-app.api"
  "version" "1.0.0"
  "description" "HTTP API endpoints"
  "sources" ["src"] 
  "tests" ["tests"]
  "dependencies" ["epsilon.core", "epsilon.http", "my-app.core"]
  "provides" ["my-app.api.handlers", "my-app.api.middleware", "my-app.api.validation"]
}
```

### Load Order and Dependencies

Epsilon handles dependency resolution automatically:

1. **Automatic Discovery**: Scans `module/` directory for `package.edn` files
2. **Dependency Resolution**: Topological sort ensures correct load order
3. **Content-based Rebuilds**: Only recompiles changed modules and dependents

```bash
# Run your application
./epsilon main.lisp

# Run specific scripts
./epsilon setup.lisp

# For interactive development, start the REPL
./epsilon
```

### Module Communication Patterns

**Package Definitions:**
```lisp
;; my-app.core/src/app/config.lisp
(defpackage my-app.config
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (json epsilon.json))
  (:export
   #:load-config
   #:get-setting))

;; my-app.api/src/api/handlers.lisp
(defpackage my-app.api.handlers
  (:use cl)
  (:local-nicknames
   (config my-app.config)
   (http epsilon.http.server)
   (json epsilon.json))
  (:export
   #:setup-routes
   #:health-check-handler))
```

## External Protocols and Data Handling

### JSON Processing

Epsilon includes complete JSON support:

```lisp
(defpackage my-app.api.handlers
  (:use cl)
  (:local-nicknames (json epsilon.json)))

;; Parse JSON request body
(defun parse-request-body (request)
  (let ((body (http:request-body request)))
    (when body
      (json:decode (map:get body :content)))))

;; Generate JSON response
(defun json-response (data &optional (status 200))
  (http:make-response
   :status status
   :headers (map:make-map "Content-Type" "application/json")
   :body (json:encode data)))

;; Example API handler
(defun create-user-handler (request)
  (let ((user-data (parse-request-body request)))
    (if (validate-user-data user-data)
        (let ((user (create-user user-data)))
          (json-response (map:make-map "id" (user-id user) 
                                      "status" "created")
                        201))
        (json-response (map:make-map "error" "Invalid user data") 400))))
```

### MessagePack for Efficient Serialization

```lisp
(defpackage my-app.lib.serialization
  (:use cl)
  (:local-nicknames (msgpack epsilon.msgpack)))

;; Serialize data for caching or message queues
(defun serialize-job-data (job)
  (msgpack:encode (map:make-map 
                   "type" (job-type job)
                   "payload" (job-payload job)
                   "created" (job-timestamp job))))

;; Deserialize from external systems
(defun deserialize-job-data (bytes)
  (msgpack:decode bytes))
```

### HTTP Client for External APIs

```lisp
(defpackage my-app.lib.external
  (:use cl)
  (:local-nicknames 
   (http epsilon.http.client)
   (json epsilon.json)))

;; Call external REST API
(defun fetch-user-profile (user-id api-key)
  (let ((response (http:get 
                   (format nil "https://api.example.com/users/~A" user-id)
                   :headers (map:make-map "Authorization" 
                                         (format nil "Bearer ~A" api-key)))))
    (when (= (http:response-status response) 200)
      (json:decode (http:response-body response)))))

;; POST data to external service
(defun send-notification (user-id message)
  (let ((response (http:post "https://notifications.example.com/send"
                            :headers (map:make-map "Content-Type" "application/json")
                            :body (json:encode (map:make-map
                                               "user_id" user-id
                                               "message" message)))))
    (= (http:response-status response) 200)))
```

### HTTPS and TLS Support

```lisp
;; HTTPS client requests work automatically
(defun secure-api-call (endpoint data)
  (http:post endpoint
             :headers (map:make-map "Content-Type" "application/json")
             :body (json:encode data)))

;; HTTPS server setup
(defun start-secure-server (port cert-file key-file)
  (http:start-server 
   :port port
   :tls-cert cert-file
   :tls-key key-file
   :handler #'my-app-handler))
```

## Complex Application Patterns

### Configuration Management

**Environment-specific configuration:**
```lisp
(defpackage my-app.config
  (:use cl)
  (:local-nicknames 
   (map epsilon.map)
   (json epsilon.json)
   (env epsilon.sys.env)))

(defvar *config* nil "Application configuration")

(defun load-config (&optional (env "development"))
  "Load configuration based on environment"
  (let* ((config-file (format nil "config/~A.json" env))
         (config-data (json:decode-file config-file)))
    
    ;; Environment variable substitution
    (setf *config* 
          (map:map config-data
                   (lambda (value)
                     (if (and (stringp value) 
                              (starts-with value "${"))
                         (env:getenv (subseq value 2 (1- (length value))))
                         value))))))

(defun get-setting (key &optional default)
  "Get configuration setting with optional default"
  (map:get *config* key default))
```

**Configuration files:**
```json
// config/development.json
{
  "database_url": "postgresql://localhost:5432/myapp_dev",
  "redis_url": "redis://localhost:6379",
  "log_level": "debug",
  "external_api_key": "${EXTERNAL_API_KEY}"
}

// config/production.json  
{
  "database_url": "${DATABASE_URL}",
  "redis_url": "${REDIS_URL}",
  "log_level": "info",
  "external_api_key": "${EXTERNAL_API_KEY}"
}
```

### Application Initialization

**Main application bootstrap:**
```lisp
(defpackage my-app.main
  (:use cl)
  (:local-nicknames
   (config my-app.config)
   (api my-app.api.handlers)
   (workers my-app.workers.job-queue)
   (http epsilon.http.server)
   (log epsilon.log)))

(defun start-application (&key (env "development") (port 8080))
  "Start the complete application stack"
  
  ;; Load configuration
  (config:load-config env)
  (log:info "Loaded configuration for environment: ~A" env)
  
  ;; Initialize database connection
  (db:connect (config:get-setting "database_url"))
  (log:info "Connected to database")
  
  ;; Start background workers
  (workers:start-job-processor)
  (log:info "Started background job processor")
  
  ;; Start HTTP server
  (http:start-server 
   :port port
   :handler (api:setup-routes))
  (log:info "HTTP server started on port ~A" port))

(defun stop-application ()
  "Graceful shutdown"
  (http:stop-server)
  (workers:stop-job-processor)
  (db:disconnect)
  (log:info "Application stopped"))
```

### Error Handling and Middleware

**HTTP middleware pattern:**
```lisp
(defpackage my-app.api.middleware
  (:use cl)
  (:local-nicknames
   (http epsilon.http.server)
   (json epsilon.json)
   (log epsilon.log)))

(defun json-middleware (handler)
  "Wrap handler with JSON request/response processing"
  (lambda (request)
    (handler-case
        (let ((response (funcall handler request)))
          ;; Ensure JSON content type
          (http:add-header response "Content-Type" "application/json")
          response)
      (error (e)
        (log:error "Handler error: ~A" e)
        (http:make-response
         :status 500
         :headers (map:make-map "Content-Type" "application/json")
         :body (json:encode (map:make-map "error" "Internal server error")))))))

(defun auth-middleware (handler)
  "Require authentication for protected endpoints"
  (lambda (request)
    (let ((auth-header (http:get-header request "Authorization")))
      (if (and auth-header (valid-token-p auth-header))
          (funcall handler request)
          (http:make-response
           :status 401
           :headers (map:make-map "Content-Type" "application/json")
           :body (json:encode (map:make-map "error" "Unauthorized")))))))

;; Compose middleware
(defun setup-routes ()
  (-> #'my-routes
      (json-middleware)
      (auth-middleware)))
```

### Testing Patterns

**Integration tests:**
```lisp
(defpackage my-app.tests.integration
  (:use cl epsilon.tool.test)
  (:local-nicknames
   (http epsilon.http.client)
   (json epsilon.json)
   (main my-app.main)))

(deftest full-application-test
  "Test complete application workflow"
  ;; Start test server
  (main:start-application :env "test" :port 9090)
  
  (unwind-protect
      (progn
        ;; Test user creation
        (let ((response (http:post "http://localhost:9090/users"
                                   :headers (map:make-map "Content-Type" "application/json")
                                   :body (json:encode (map:make-map 
                                                      "name" "Test User"
                                                      "email" "test@example.com")))))
          (is-equal 201 (http:response-status response))
          (let ((user (json:decode (http:response-body response))))
            (is (map:get user "id"))
            
            ;; Test user retrieval
            (let ((get-response (http:get (format nil "http://localhost:9090/users/~A"
                                                 (map:get user "id")))))
              (is-equal 200 (http:response-status get-response))))))
    
    ;; Cleanup
    (main:stop-application)))
```

## Current Limitations and Workarounds

### Missing WebSocket Support

**Current state**: No native WebSocket implementation

**Workaround**: Use HTTP long polling or Server-Sent Events
```lisp
;; Server-Sent Events implementation
(defun events-handler (request)
  "Stream events to client using SSE"
  (http:make-response
   :status 200
   :headers (map:make-map 
             "Content-Type" "text/event-stream"
             "Cache-Control" "no-cache"
             "Connection" "keep-alive")
   :body (lambda (stream)
           (loop for event in (get-pending-events)
                 do (format stream "data: ~A~%~%" (json:encode event))
                    (force-output stream)))))
```

### Limited Async I/O (except Windows)

**Current state**: Full async support only on Windows

**Workaround**: Use threading for concurrent operations
```lisp
(defpackage my-app.lib.concurrent
  (:use cl)
  (:local-nicknames (thread epsilon.sys.thread)))

(defun parallel-api-calls (urls)
  "Make multiple API calls concurrently using threads"
  (let ((futures (mapcar (lambda (url)
                          (thread:future 
                           (lambda () (http:get url))))
                        urls)))
    (mapcar #'thread:force futures)))
```

### No Package Repository

**Current state**: Local modules only

**Workaround**: Git submodules or manual dependency management
```bash
# Add external module as git submodule
git submodule add https://github.com/example/epsilon-extension.git module/extension

# Update submodules
git submodule update --init --recursive
```

## Recommendations

### For New Projects
1. **Start Simple**: Use single module until complexity demands splitting
2. **Follow Conventions**: Use recommended directory structure
3. **Embrace Functional Style**: Leverage Epsilon's immutable data structures
4. **Test Early**: Use built-in test framework from the beginning

### For Complex Applications
1. **Module Boundaries**: Split by business domain, not technical layers
2. **Configuration**: Use environment-specific JSON configs with variable substitution
3. **Error Handling**: Implement middleware patterns for cross-cutting concerns
4. **Monitoring**: Use Epsilon's logging system extensively

### Future-Proofing
1. **Interfaces**: Design module APIs that can evolve
2. **Documentation**: Document module boundaries and contracts
3. **Testing**: Implement both unit and integration tests
4. **Performance**: Profile early if handling significant load

Epsilon provides a solid foundation for application development with modern patterns, though some features need workarounds until native implementations are available.
