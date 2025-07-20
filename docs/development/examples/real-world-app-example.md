# Real-World Application Example: Task Management API

This example demonstrates a complete multi-module application addressing the concerns raised about Epsilon's usability for complex systems.

## Project Structure

```
task-manager/
├── run.sh                     # Development interface  
├── scripts/
│   └── epsilon.lisp          # Bootstrap
├── config/
│   ├── development.json      # Dev configuration
│   ├── production.json       # Prod configuration
│   └── test.json            # Test configuration
├── module/
│   ├── core/                # Domain logic and configuration
│   │   ├── package.edn
│   │   ├── src/
│   │   │   ├── config.lisp
│   │   │   ├── models.lisp
│   │   │   └── services.lisp
│   │   └── tests/
│   ├── api/                 # REST API endpoints
│   │   ├── package.edn
│   │   ├── src/
│   │   │   ├── handlers.lisp
│   │   │   ├── middleware.lisp
│   │   │   └── validation.lisp
│   │   └── tests/
│   └── persistence/         # Data storage layer
│       ├── package.edn
│       ├── src/
│       │   ├── storage.lisp
│       │   └── migrations.lisp
│       └── tests/
└── target/                  # Build artifacts (auto-generated)
```

## Module Definitions

**Core Module (module/core/package.edn):**
```edn
{
  "name" "task-manager.core"
  "version" "1.0.0"
  "description" "Core domain logic and configuration"
  "sources" ["src"]
  "tests" ["tests"] 
  "dependencies" ["epsilon.core"]
  "provides" [
    "task-manager.config",
    "task-manager.models",
    "task-manager.services"
  ]
}
```

**API Module (module/api/package.edn):**
```edn
{
  "name" "task-manager.api"
  "version" "1.0.0"
  "description" "REST API endpoints and middleware"
  "sources" ["src"]
  "tests" ["tests"]
  "dependencies" ["epsilon.core", "epsilon.http", "task-manager.core"]
  "provides" [
    "task-manager.api.handlers",
    "task-manager.api.middleware", 
    "task-manager.api.validation"
  ]
}
```

**Persistence Module (module/persistence/package.edn):**
```edn
{
  "name" "task-manager.persistence"
  "version" "1.0.0"
  "description" "Data storage and migrations"
  "sources" ["src"]
  "tests" ["tests"]
  "dependencies" ["epsilon.core", "task-manager.core"]
  "provides" [
    "task-manager.storage",
    "task-manager.migrations"
  ]
}
```

## Configuration Management

**config/development.json:**
```json
{
  "server": {
    "port": 8080,
    "host": "localhost"
  },
  "database": {
    "url": "sqlite:task_manager_dev.db"
  },
  "logging": {
    "level": "debug",
    "format": "detailed"
  },
  "features": {
    "cors_enabled": true,
    "debug_routes": true
  }
}
```

**Core Configuration (module/core/src/config.lisp):**
```lisp
(defpackage task-manager.config
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (json epsilon.json)
   (env epsilon.sys.env)
   (log epsilon.log))
  (:export
   #:load-config
   #:get-setting
   #:*config*))

(in-package task-manager.config)

(defvar *config* nil "Application configuration")

(defun load-config (&optional (environment "development"))
  "Load configuration for specified environment"
  (let* ((config-file (format nil "config/~A.json" environment))
         (config-data (with-open-file (stream config-file :direction :input)
                        (json:decode (read-string-stream stream)))))
    
    ;; Environment variable substitution
    (setf *config* (substitute-env-vars config-data))
    (log:info "Loaded configuration for environment: ~A" environment)
    *config*))

(defun substitute-env-vars (data)
  "Recursively substitute environment variables in configuration"
  (cond
    ((stringp data)
     (if (and (> (length data) 3)
              (string= (subseq data 0 2) "${")
              (char= (char data (1- (length data))) #\}))
         (let ((var-name (subseq data 2 (1- (length data)))))
           (or (env:getenv var-name) data))
         data))
    ((map:map-p data)
     (map:map data #'substitute-env-vars))
    ((listp data)
     (mapcar #'substitute-env-vars data))
    (t data)))

(defun get-setting (path &optional default)
  "Get configuration setting using dot notation (e.g., 'server.port')"
  (let ((keys (split-string path ".")))
    (reduce (lambda (config key)
              (if (map:map-p config)
                  (map:get config key)
                  nil))
            keys
            :initial-value *config*)))

(defun split-string (string delimiter)
  "Split string by delimiter"
  (let ((parts '())
        (start 0))
    (loop for i from 0 below (length string)
          when (string= (string (char string i)) delimiter)
            do (push (subseq string start i) parts)
               (setf start (1+ i))
          finally (push (subseq string start) parts))
    (nreverse parts)))

(defun read-string-stream (stream)
  "Read entire stream as string"
  (let ((contents (make-string (file-length stream))))
    (read-sequence contents stream)
    contents))
```

## Domain Models

**module/core/src/models.lisp:**
```lisp
(defpackage task-manager.models
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (uuid epsilon.uuid)
   (time epsilon.time))
  (:export
   #:make-task
   #:task-id
   #:task-title
   #:task-description
   #:task-status
   #:task-created-at
   #:task-updated-at
   #:update-task
   #:validate-task))

(in-package task-manager.models)

(defun make-task (&key title description (status :pending))
  "Create a new task with generated ID and timestamps"
  (let ((now (time:now)))
    (map:make-map
     "id" (uuid:generate)
     "title" title
     "description" description
     "status" status
     "created_at" now
     "updated_at" now)))

(defun task-id (task) (map:get task "id"))
(defun task-title (task) (map:get task "title"))
(defun task-description (task) (map:get task "description"))
(defun task-status (task) (map:get task "status"))
(defun task-created-at (task) (map:get task "created_at"))
(defun task-updated-at (task) (map:get task "updated_at"))

(defun update-task (task updates)
  "Update task with new values and current timestamp"
  (map:assoc (map:merge task updates)
             "updated_at" (time:now)))

(defun validate-task (task-data)
  "Validate task data - returns (values valid-p errors)"
  (let ((errors '()))
    
    ;; Title validation
    (let ((title (map:get task-data "title")))
      (unless (and title (stringp title) (> (length title) 0))
        (push "Title is required and must be non-empty string" errors)))
    
    ;; Status validation  
    (let ((status (map:get task-data "status")))
      (unless (member status '(:pending :in-progress :completed))
        (push "Status must be one of: pending, in-progress, completed" errors)))
    
    (values (null errors) errors)))
```

## Service Layer

**module/core/src/services.lisp:**
```lisp
(defpackage task-manager.services
  (:use cl)
  (:local-nicknames
   (models task-manager.models)
   (storage task-manager.storage)
   (log epsilon.log))
  (:export
   #:create-task
   #:get-task
   #:list-tasks
   #:update-task
   #:delete-task))

(in-package task-manager.services)

(defun create-task (task-data)
  "Create a new task with validation"
  (multiple-value-bind (valid-p errors)
      (models:validate-task task-data)
    (if valid-p
        (let ((task (models:make-task 
                     :title (map:get task-data "title")
                     :description (map:get task-data "description")
                     :status (or (map:get task-data "status") :pending))))
          (storage:save-task task)
          (log:info "Created task" :task-id (models:task-id task))
          (values task nil))
        (values nil errors))))

(defun get-task (task-id)
  "Retrieve task by ID"
  (let ((task (storage:find-task task-id)))
    (if task
        (values task nil)
        (values nil "Task not found"))))

(defun list-tasks (&key (limit 50) (offset 0) status)
  "List tasks with optional filtering"
  (storage:list-tasks :limit limit :offset offset :status status))

(defun update-task (task-id updates)
  "Update existing task"
  (let ((existing-task (storage:find-task task-id)))
    (if existing-task
        (multiple-value-bind (valid-p errors)
            (models:validate-task (map:merge existing-task updates))
          (if valid-p
              (let ((updated-task (models:update-task existing-task updates)))
                (storage:save-task updated-task)
                (log:info "Updated task" :task-id task-id)
                (values updated-task nil))
              (values nil errors)))
        (values nil "Task not found"))))

(defun delete-task (task-id)
  "Delete task by ID"
  (if (storage:find-task task-id)
      (progn
        (storage:delete-task task-id)
        (log:info "Deleted task" :task-id task-id)
        (values t nil))
      (values nil "Task not found")))
```

## REST API Layer

**module/api/src/handlers.lisp:**
```lisp
(defpackage task-manager.api.handlers
  (:use cl)
  (:local-nicknames
   (http epsilon.http.server)
   (json epsilon.json)
   (map epsilon.map)
   (services task-manager.services)
   (validation task-manager.api.validation)
   (log epsilon.log))
  (:export
   #:setup-routes
   #:health-check
   #:create-task-handler
   #:get-task-handler
   #:list-tasks-handler
   #:update-task-handler
   #:delete-task-handler))

(in-package task-manager.api.handlers)

(defun setup-routes ()
  "Configure all API routes"
  (list
   (list :get "/health" #'health-check)
   (list :post "/tasks" #'create-task-handler)
   (list :get "/tasks/:id" #'get-task-handler)
   (list :get "/tasks" #'list-tasks-handler)
   (list :put "/tasks/:id" #'update-task-handler)
   (list :delete "/tasks/:id" #'delete-task-handler)))

(defun health-check (request)
  "Health check endpoint"
  (declare (ignore request))
  (json-response (map:make-map "status" "healthy" "timestamp" (get-universal-time))))

(defun create-task-handler (request)
  "POST /tasks - Create a new task"
  (let ((task-data (parse-json-body request)))
    (if task-data
        (multiple-value-bind (task errors)
            (services:create-task task-data)
          (if task
              (json-response task 201)
              (error-response "Validation failed" 400 :details errors)))
        (error-response "Invalid JSON body" 400))))

(defun get-task-handler (request)
  "GET /tasks/:id - Get task by ID"
  (let ((task-id (http:path-param request "id")))
    (multiple-value-bind (task error)
        (services:get-task task-id)
      (if task
          (json-response task)
          (error-response error 404)))))

(defun list-tasks-handler (request)
  "GET /tasks - List tasks with optional filtering"
  (let* ((query-params (http:query-params request))
         (limit (parse-integer (or (map:get query-params "limit") "50")))
         (offset (parse-integer (or (map:get query-params "offset") "0")))
         (status (when-let ((s (map:get query-params "status")))
                   (intern (string-upcase s) :keyword))))
    (let ((tasks (services:list-tasks :limit limit :offset offset :status status)))
      (json-response (map:make-map "tasks" tasks
                                  "limit" limit
                                  "offset" offset)))))

(defun update-task-handler (request)
  "PUT /tasks/:id - Update existing task"
  (let ((task-id (http:path-param request "id"))
        (updates (parse-json-body request)))
    (if updates
        (multiple-value-bind (task errors)
            (services:update-task task-id updates)
          (if task
              (json-response task)
              (error-response (if (stringp errors) errors "Validation failed") 
                            (if (stringp errors) 404 400)
                            :details (unless (stringp errors) errors))))
        (error-response "Invalid JSON body" 400))))

(defun delete-task-handler (request)
  "DELETE /tasks/:id - Delete task"
  (let ((task-id (http:path-param request "id")))
    (multiple-value-bind (success error)
        (services:delete-task task-id)
      (if success
          (http:make-response :status 204)
          (error-response error 404)))))

;; Helper functions
(defun parse-json-body (request)
  "Parse JSON request body"
  (handler-case
      (when-let ((body (http:request-body request)))
        (json:decode body))
    (error (e)
      (log:warn "Failed to parse JSON body" :error (princ-to-string e))
      nil)))

(defun json-response (data &optional (status 200))
  "Create JSON response with appropriate headers"
  (http:make-response
   :status status
   :headers (map:make-map "Content-Type" "application/json"
                         "Cache-Control" "no-cache")
   :body (json:encode data)))

(defun error-response (message &optional (status 500) &key details)
  "Create error response with consistent format"
  (json-response (map:make-map "error" message
                              "details" details
                              "timestamp" (get-universal-time))
                status))

(defmacro when-let ((var expr) &body body)
  "Execute body if expr is non-nil, binding it to var"
  `(let ((,var ,expr))
     (when ,var ,@body)))
```

## Persistence Layer

**module/persistence/src/storage.lisp:**
```lisp
(defpackage task-manager.storage
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (json epsilon.json)
   (config task-manager.config)
   (log epsilon.log))
  (:export
   #:initialize-storage
   #:save-task
   #:find-task
   #:list-tasks
   #:delete-task))

(in-package task-manager.storage)

;; Simple in-memory storage for demonstration
;; In production, this would use a proper database
(defvar *tasks* (make-hash-table :test 'equal) "Task storage")

(defun initialize-storage ()
  "Initialize storage system"
  (clrhash *tasks*)
  (log:info "Storage initialized"))

(defun save-task (task)
  "Save task to storage"
  (let ((id (map:get task "id")))
    (setf (gethash id *tasks*) task)
    task))

(defun find-task (task-id)
  "Find task by ID"
  (gethash task-id *tasks*))

(defun list-tasks (&key (limit 50) (offset 0) status)
  "List tasks with optional filtering and pagination"
  (let ((all-tasks (loop for task being the hash-values of *tasks*
                        when (or (null status) 
                                (eq (map:get task "status") status))
                        collect task)))
    ;; Sort by created_at descending
    (setf all-tasks (sort all-tasks 
                         (lambda (a b)
                           (> (map:get a "created_at") 
                              (map:get b "created_at")))))
    
    ;; Apply pagination
    (let ((end (min (+ offset limit) (length all-tasks))))
      (when (< offset (length all-tasks))
        (subseq all-tasks offset end)))))

(defun delete-task (task-id)
  "Delete task by ID"
  (remhash task-id *tasks*))
```

## Application Bootstrap

**scripts/epsilon.lisp (enhanced for application development):**
```lisp
#!/usr/bin/env sbcl --script

;; Enhanced bootstrap script for application development

(defvar *epsilon-root* 
  (truename (merge-pathnames "../" (directory-namestring *load-pathname*))))

(defun bootstrap-epsilon ()
  "Bootstrap Epsilon with development-friendly settings"
  
  ;; Set up paths
  (pushnew *epsilon-root* asdf:*central-registry*)
  
  ;; Load Epsilon core with enhanced logging
  (format t ";;; Bootstrapping Epsilon for development...~%")
  (time (load (merge-pathnames "scripts/epsilon-core.lisp" *epsilon-root*)))
  
  ;; Enable development features
  (setf *print-pretty* t)
  (setf *print-length* 50)
  (setf *print-level* 10)
  
  ;; Set up development REPL
  (when (find-package :swank)
    (funcall (intern "START-SERVER" :swank) :port 4005 :dont-close t))
  
  ;; Load application modules
  (epsilon.tool.build:build-modules '("task-manager.core" 
                                     "task-manager.persistence" 
                                     "task-manager.api"))
  
  (format t ";;; Epsilon ready for development~%"))

;; Development utilities
(defpackage task-manager.dev
  (:use cl)
  (:local-nicknames
   (config task-manager.config)
   (storage task-manager.storage)
   (api task-manager.api.handlers)
   (http epsilon.http.server)
   (log epsilon.log))
  (:export
   #:start-app
   #:stop-app
   #:restart-app
   #:load-sample-data))

(in-package task-manager.dev)

(defvar *server* nil "Development server instance")

(defun start-app (&key (env "development") (port nil))
  "Start the application with development settings"
  (config:load-config env)
  (storage:initialize-storage)
  
  (let ((server-port (or port (config:get-setting "server.port") 8080)))
    (setf *server* (http:start-server
                    :port server-port
                    :routes (api:setup-routes)))
    (log:info "Application started" :port server-port :environment env)
    (format t ";;; Task Manager API ready at http://localhost:~A~%" server-port)))

(defun stop-app ()
  "Stop the development server"
  (when *server*
    (http:stop-server *server*)
    (setf *server* nil)
    (log:info "Application stopped")))

(defun restart-app (&key (env "development"))
  "Restart the application (useful for development)"
  (stop-app)
  (start-app :env env))

(defun load-sample-data ()
  "Load sample data for development"
  (let ((sample-tasks '(
    (("title" "Set up development environment") 
     ("description" "Configure Epsilon and create project structure")
     ("status" :completed))
    (("title" "Implement user authentication")
     ("description" "Add JWT-based authentication system")
     ("status" :in-progress))
    (("title" "Add task prioritization")
     ("description" "Allow users to set task priorities")
     ("status" :pending))
    (("title" "Deploy to production")
     ("description" "Set up CI/CD pipeline and deploy application")
     ("status" :pending)))))
    
    (dolist (task-data sample-tasks)
      (task-manager.services:create-task (apply #'map:make-map task-data)))
    
    (log:info "Loaded sample data" :count (length sample-tasks))))

;; Auto-start in development
(when (string= (or (sb-ext:posix-getenv "EPSILON_ENV") "development") "development")
  (bootstrap-epsilon)
  (start-app)
  (load-sample-data))
```

## Usage Examples

**Starting Development:**
```bash
# Start the application in development mode
./run.sh

# Or manually with specific environment
EPSILON_ENV=development sbcl --load scripts/epsilon.lisp
```

**Building and Testing:**
```bash
# Build all modules
./run.sh build

# Build specific modules
./run.sh build --module task-manager.core,task-manager.api

# Run tests
./run.sh test --module task-manager.core
./run.sh test --module task-manager.api --test create-task*

# Run all tests
./run.sh test
```

**Testing the API:**
```bash
# Health check
curl http://localhost:8080/health

# Create a task
curl -X POST http://localhost:8080/tasks \
  -H "Content-Type: application/json" \
  -d '{"title": "Learn Epsilon", "description": "Build a real application"}'

# List tasks
curl http://localhost:8080/tasks

# Get specific task
curl http://localhost:8080/tasks/{task-id}

# Update task
curl -X PUT http://localhost:8080/tasks/{task-id} \
  -H "Content-Type: application/json" \
  -d '{"status": "completed"}'

# Delete task
curl -X DELETE http://localhost:8080/tasks/{task-id}
```

## Production Deployment

**config/production.json:**
```json
{
  "server": {
    "port": 8080,
    "host": "0.0.0.0"
  },
  "database": {
    "url": "${DATABASE_URL}"
  },
  "logging": {
    "level": "info",
    "format": "json"
  },
  "features": {
    "cors_enabled": false,
    "debug_routes": false
  }
}
```

**Production startup:**
```bash
EPSILON_ENV=production \
DATABASE_URL=postgresql://user:pass@db:5432/taskmanager \
sbcl --load scripts/epsilon.lisp
```

## Key Benefits Demonstrated

1. **Clear Module Structure**: Each module has well-defined responsibilities and dependencies
2. **Configuration Management**: Environment-specific configs with variable substitution
3. **Service Layer Architecture**: Clean separation between HTTP layer and business logic
4. **Built-in Protocols**: JSON serialization and HTTP client/server work out of the box
5. **Development Experience**: Hot-reloading friendly structure with development utilities
6. **Testing Support**: Module-based testing with Epsilon's built-in framework
7. **Production Ready**: Structured logging, error handling, and configuration management

This example shows that Epsilon can support real-world application development today, while highlighting areas where additional tooling would improve the experience.
