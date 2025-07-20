# Building a Web Service with Epsilon

This tutorial demonstrates how to create a web service using Epsilon.

## Prerequisites

- Epsilon binary distribution for your platform (Mac, Linux, or Windows)

## Installation

1. Download the Epsilon distribution for your platform:
   ```bash
   # Mac
   curl -L https://epsilon-lang.org/releases/latest/epsilon-macos-x86_64.tar.gz | tar xz
   
   # Linux  
   curl -L https://epsilon-lang.org/releases/latest/epsilon-linux-x86_64.tar.gz | tar xz
   
   # Windows (PowerShell)
   Invoke-WebRequest -Uri https://epsilon-lang.org/releases/latest/epsilon-windows-x86_64.zip -OutFile epsilon.zip
   Expand-Archive epsilon.zip
   ```

2. Add epsilon to your PATH:
   ```bash
   export PATH=$PATH:$(pwd)/epsilon-*/
   ```

3. Verify installation:
   ```bash
   epsilon --version
   ```

## Step 1: Check Available Modules

Epsilon includes a bundled repository with all standard library modules:

```bash
# Start epsilon REPL
epsilon

# List available modules
(list-available-modules)

# You should see:
# epsilon.core (1.0.0) [LOADED]
# epsilon.http (1.0.0)
# epsilon.json (1.0.0)
# ... and more
```

## Step 2: Create a Simple Web Service

Create a file called `web-service.lisp`:

```lisp
;;;; Simple web service using Epsilon

;; Load required modules from bundled repository
(load-bundled-module "epsilon.http")
(load-bundled-module "epsilon.json")

(defpackage :my-web-service
  (:use :cl)
  (:local-nicknames
   (#:http #:epsilon.http.server)
   (#:response #:epsilon.http.response)
   (#:json #:epsilon.json)
   (#:map #:epsilon.map)))

(in-package :my-web-service)

;; Define handlers
(defun health-handler (request)
  (declare (ignore request))
  (response:json 
   (map:make "status" "healthy"
             "timestamp" (get-universal-time))))

(defun echo-handler (request)
  (let* ((body (http:request-body request))
         (data (when body (json:parse body))))
    (response:json 
     (map:make "echo" data
               "received_at" (get-universal-time)))))

(defun home-handler (request)
  (declare (ignore request))
  (response:html 
   "<h1>Epsilon Web Service</h1>
    <p>Endpoints:</p>
    <ul>
      <li>GET /health - Health check</li>
      <li>POST /echo - Echo JSON data</li>
    </ul>"))

;; Main server function
(defun start-server (&key (port 8080))
  (let ((server (http:create-server)))
    
    ;; Define routes
    (http:route server "GET" "/" #'home-handler)
    (http:route server "GET" "/health" #'health-handler)
    (http:route server "POST" "/echo" #'echo-handler)
    
    ;; Start server
    (http:start server :port port)
    (format t "Server running on http://localhost:~A~%" port)
    
    ;; Keep running
    (http:wait-for-shutdown server)))

;; Entry point for standalone execution
(defun main ()
  (handler-case
      (start-server)
    (condition (c)
      (format *error-output* "Server error: ~A~%" c)
      (sb-ext:exit :code 1))))
```

## Step 3: Run the Web Service

### Option 1: Interactive Development

```bash
# Start epsilon and load the file
epsilon

# In the REPL:
(load "web-service.lisp")
(in-package :my-web-service)
(start-server)
```

### Option 2: Direct Execution

```bash
# Run directly with epsilon
epsilon --load web-service.lisp --eval "(my-web-service:main)"
```

### Option 3: Create Standalone Executable

```bash
# Create an executable
epsilon --load web-service.lisp \
        --eval "(sb-ext:save-lisp-and-die \"my-web-service\" :toplevel #'my-web-service:main :executable t)"

# Run the executable
./my-web-service
```

## Step 4: Test the Service

Test your service with curl:

```bash
# Health check
curl http://localhost:8080/health

# Echo endpoint
curl -X POST http://localhost:8080/echo \
     -H "Content-Type: application/json" \
     -d '{"message": "Hello, Epsilon!"}'

# Home page
curl http://localhost:8080/
```

## Step 5: Add More Features

Let's enhance our service with an anagram generator:

```lisp
;; Add to web-service.lisp

(defun shuffle-word (word)
  "Randomly shuffle characters in a word"
  (coerce (alexandria:shuffle (coerce word 'list)) 'string))

(defun generate-anagram (text)
  "Generate anagram by shuffling each word"
  (format nil "~{~A~^ ~}" 
          (mapcar #'shuffle-word 
                  (uiop:split-string text))))

(defun anagram-handler (request)
  (let* ((body (http:request-body request))
         (data (when body (json:parse body)))
         (text (when data (map:get data "text"))))
    (if text
        (response:json 
         (map:make "original" text
                   "anagram" (generate-anagram text)))
        (response:json 
         (map:make "error" "No text provided")
         :status 400))))

;; Add route in start-server function:
(http:route server "POST" "/anagram" #'anagram-handler)
```

## Troubleshooting

### Module Loading Issues

If modules fail to load:

1. Check available modules:
   ```lisp
   (list-available-modules)
   ```

2. Load with verbose output:
   ```lisp
   (let ((*load-verbose* t))
     (load-bundled-module "epsilon.http"))
   ```

3. Check module dependencies:
   ```lisp
   ;; Each module lists its dependencies
   ;; epsilon.json depends on epsilon.parsing
   ;; epsilon.http depends on epsilon.core
   ```

### Platform-Specific Issues

**Mac**: If you get "cannot be opened because the developer cannot be verified":
```bash
xattr -d com.apple.quarantine epsilon
```

**Linux**: Ensure executable permissions:
```bash
chmod +x epsilon
```

**Windows**: Run from PowerShell or Command Prompt, not WSL.

### Server Issues

1. Port already in use:
   ```lisp
   (start-server :port 8081)  ; Try different port
   ```

2. Can't bind to address:
   - Check firewall settings
   - Try binding to localhost only

## Deployment

### Docker Container

Create a `Dockerfile`:

```dockerfile
FROM ubuntu:22.04

# Install curl for downloading epsilon
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Download and install epsilon
WORKDIR /opt
RUN curl -L https://epsilon-lang.org/releases/latest/epsilon-linux-x86_64.tar.gz | tar xz

# Add epsilon to PATH
ENV PATH="/opt/epsilon:$PATH"

# Copy application
WORKDIR /app
COPY web-service.lisp .

# Create startup script
RUN echo '#!/bin/bash\nepsilon --load web-service.lisp --eval "(my-web-service:main)"' > start.sh && \
    chmod +x start.sh

EXPOSE 8080

CMD ["./start.sh"]
```

Build and run:
```bash
docker build -t my-epsilon-service .
docker run -p 8080:8080 my-epsilon-service
```

## Key Concepts

1. **Self-Contained**: Epsilon includes everything needed - no external dependencies
2. **Bundled Modules**: Standard library modules are pre-compiled and bundled
3. **Module Loading**: Use `(load-bundled-module "name")` to load modules
4. **Cross-Platform**: Same code works on Mac, Linux, and Windows

## Next Steps

- Explore more epsilon modules: `epsilon.yaml`, `epsilon.msgpack`
- Add database connectivity with `epsilon.sql` (when available)
- Implement authentication and sessions
- Add WebSocket support for real-time features

This tutorial demonstrates that Epsilon works as a complete, self-contained Lisp environment for building web services without any external dependencies.
