# Code Examples

Practical examples demonstrating Epsilon's capabilities in real-world scenarios.

## Data Processing Pipeline

Process user data with functional transformations:

```lisp
(defpackage #:example.data-processing
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)
    (#:json #:epsilon.lib.json)))

(in-package #:example.data-processing)

(defun process-user-data (json-file)
  "Process user data from JSON file with validation and aggregation."
  (->> (json:decode-file json-file)
       (seq:from-list)
       (seq:map #'normalize-user)
       (seq:filter #'valid-user-p)
       (seq:group-by (lambda (user) (map:get user :country)))
       (map:map (lambda (country users)
                  {:country country
                   :count (seq:count users)
                   :avg-age (average-age users)
                   :active-users (seq:count (seq:filter #'active-user-p users))}))))

(defun normalize-user (user-data)
  "Normalize user data structure."
  (-> user-data
      (map:update :email #'string-downcase)
      (map:update :name #'string-titlecase)
      (map:assoc :processed-at (get-universal-time))))

(defun valid-user-p (user)
  "Check if user data is valid."
  (and (map:contains-p user :email)
       (map:contains-p user :name)
       (> (length (map:get user :email)) 0)))
```

## Configuration Management

Manage application configuration with environment overrides:

```lisp
(defpackage #:example.config
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:env #:epsilon.sys.env)
    (#:yaml #:epsilon.lib.yaml)))

(in-package #:example.config)

(defparameter *default-config*
  {:database {:host "localhost"
              :port 5432
              :name "myapp"}
   :server {:port 8080
            :workers 4}
   :logging {:level :info
             :file "/var/log/myapp.log"}})

(defun load-config (&optional config-file)
  "Load configuration with environment variable overrides."
  (let ((config (if config-file
                    (map:merge *default-config* 
                               (yaml:decode-file config-file))
                    *default-config*)))
    (-> config
        (apply-env-overrides)
        (validate-config)
        (expand-paths))))

(defun apply-env-overrides (config)
  "Apply environment variable overrides to configuration."
  (-> config
      (map:update-in '(:database :host) 
                     (lambda (default) (env:get "DB_HOST" default)))
      (map:update-in '(:database :port)
                     (lambda (default) (parse-integer 
                                        (env:get "DB_PORT" (princ-to-string default)))))
      (map:update-in '(:server :port)
                     (lambda (default) (parse-integer
                                        (env:get "SERVER_PORT" (princ-to-string default)))))))
```

## HTTP API Server

Build a REST API server with routing and middleware:

```lisp
(defpackage #:example.api-server
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)
    (#:json #:epsilon.lib.json)
    (#:http #:epsilon.net.http)))

(in-package #:example.api-server)

(defparameter *users* (map:empty))

(defun start-server (&optional (port 8080))
  "Start the API server."
  (http:start-server
    :port port
    :routes (build-routes)
    :middleware (list #'json-middleware #'cors-middleware #'logging-middleware)))

(defun build-routes ()
  "Define API routes."
  (list
    (http:route :get "/users" #'list-users)
    (http:route :get "/users/:id" #'get-user)
    (http:route :post "/users" #'create-user)
    (http:route :put "/users/:id" #'update-user)
    (http:route :delete "/users/:id" #'delete-user)))

(defun list-users (request)
  "Return all users with optional filtering."
  (let ((country (http:query-param request "country"))
        (users (map:vals *users*)))
    (http:json-response
      (if country
          (seq:realize (seq:filter (lambda (user) 
                                    (equal (map:get user :country) country)) 
                                  users))
          (seq:realize users)))))

(defun create-user (request)
  "Create a new user."
  (let* ((user-data (http:json-body request))
         (user-id (generate-user-id))
         (user (map:assoc user-data 
                         :id user-id
                         :created-at (get-universal-time))))
    (setf *users* (map:assoc *users* user-id user))
    (http:json-response user :status 201)))
```

## Concurrent Processing

Process data concurrently using threading primitives:

```lisp
(defpackage #:example.concurrent
  (:use #:common-lisp)
  (:local-nicknames
    (#:seq #:epsilon.lib.sequence)
    (#:thread #:epsilon.sys.thread)
    (#:atomic #:epsilon.sys.atomic)))

(in-package #:example.concurrent)

(defun parallel-map (function sequence &key (workers 4))
  "Apply function to sequence elements in parallel."
  (let* ((input-queue (make-queue))
         (output-queue (make-queue))
         (workers-done (atomic:make-counter 0))
         (total-items (seq:count sequence)))
    
    ;; Fill input queue
    (seq:do (lambda (item) (queue-push input-queue item)) sequence)
    
    ;; Start worker threads
    (dotimes (i workers)
      (thread:spawn
        (lambda ()
          (loop
            (let ((item (queue-pop input-queue :timeout 1)))
              (if item
                  (queue-push output-queue (funcall function item))
                  (progn
                    (atomic:increment workers-done)
                    (return))))))))
    
    ;; Collect results
    (let ((results nil))
      (loop until (= (atomic:get workers-done) workers)
            do (let ((result (queue-pop output-queue :timeout 0.1)))
                 (when result
                   (push result results))))
      (reverse results))))

(defun process-large-dataset (data-source processor)
  "Process large dataset with parallel workers and progress tracking."
  (let ((progress (atomic:make-counter 0))
        (total (count-items data-source)))
    
    (thread:spawn
      (lambda ()
        (loop
          (let ((current (atomic:get progress)))
            (format t "Progress: ~A/~A (~,1F%)~%"
                    current total (* 100.0 (/ current total)))
            (sleep 1)
            (when (>= current total) (return))))))
    
    (parallel-map
      (lambda (item)
        (let ((result (funcall processor item)))
          (atomic:increment progress)
          result))
      data-source
      :workers 8)))
```

## Caching System

Implement a caching system with TTL and eviction policies:

```lisp
(defpackage #:example.cache
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:atomic #:epsilon.sys.atomic)
    (#:thread #:epsilon.sys.thread)))

(in-package #:example.cache)

(defstruct cache-entry
  value
  timestamp
  access-count)

(defclass ttl-cache ()
  ((data :initform (atomic:make-ref (map:empty)))
   (ttl :initarg :ttl :initform 3600) ; 1 hour default
   (max-size :initarg :max-size :initform 1000)
   (cleanup-thread :initform nil)))

(defmethod initialize-instance :after ((cache ttl-cache) &key)
  (start-cleanup-thread cache))

(defun cache-get (cache key &optional compute-fn)
  "Get value from cache, optionally computing if missing."
  (let* ((data (atomic:get (slot-value cache 'data)))
         (entry (map:get data key)))
    (cond
      ;; Cache hit and not expired
      ((and entry (not (expired-p entry (slot-value cache 'ttl))))
       (update-access-count cache key entry)
       (cache-entry-value entry))
      
      ;; Cache miss or expired, compute if function provided
      (compute-fn
       (let ((value (funcall compute-fn key)))
         (cache-put cache key value)
         value))
      
      ;; Cache miss, no compute function
      (t nil))))

(defun cache-put (cache key value)
  "Store value in cache."
  (let ((entry (make-cache-entry
                 :value value
                 :timestamp (get-universal-time)
                 :access-count 1)))
    (atomic:update (slot-value cache 'data)
                   (lambda (data)
                     (let ((updated (map:assoc data key entry)))
                       (if (> (map:count updated) (slot-value cache 'max-size))
                           (evict-lru updated cache)
                           updated))))))

(defun expired-p (entry ttl)
  "Check if cache entry has expired."
  (> (- (get-universal-time) (cache-entry-timestamp entry)) ttl))
```

## Data Validation & Transformation

Validate and transform data with composable functions:

```lisp
(defpackage #:example.validation
  (:use #:common-lisp)
  (:local-nicknames
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)))

(in-package #:example.validation)

(defun validate-and-transform (data schema)
  "Validate and transform data according to schema."
  (reduce (lambda (result field-spec)
            (let ((field-name (first field-spec))
                  (validators (rest field-spec)))
              (multiple-value-bind (value errors)
                  (validate-field data field-name validators)
                (if errors
                    (map:update result :errors 
                               (lambda (existing) (append existing errors)))
                    (map:assoc result field-name value)))))
          schema
          :initial-value {:valid t :errors nil}))

(defun validate-field (data field-name validators)
  "Validate a single field with a list of validators."
  (let ((value (map:get data field-name))
        (errors nil))
    (dolist (validator validators)
      (multiple-value-bind (valid-p error-msg transformed-value)
          (funcall validator value)
        (if valid-p
            (setf value transformed-value)
            (push error-msg errors))))
    (values value (reverse errors))))

;; Validator functions
(defun required ()
  "Validator that ensures field is present and not nil."
  (lambda (value)
    (if (and value (not (equal value "")))
        (values t nil value)
        (values nil "Field is required" nil))))

(defun string-length (min &optional max)
  "Validator for string length constraints."
  (lambda (value)
    (let ((len (length value)))
      (cond
        ((< len min)
         (values nil (format nil "Must be at least ~A characters" min) nil))
        ((and max (> len max))
         (values nil (format nil "Must be at most ~A characters" max) nil))
        (t (values t nil value))))))

(defun email-format ()
  "Validator for email format."
  (lambda (value)
    (if (and (stringp value) 
             (find #\@ value)
             (> (length value) 3))
        (values t nil (string-downcase value))
        (values nil "Invalid email format" nil))))

;; Usage example
(defparameter *user-schema*
  '((:name (required) (string-length 1 100))
    (:email (required) (email-format))
    (:age (required) (integer-range 13 120))))

(defun process-user-registration (form-data)
  "Process user registration with validation."
  (let ((result (validate-and-transform form-data *user-schema*)))
    (if (map:get result :valid)
        (create-user-account result)
        (signal-validation-errors (map:get result :errors)))))
```

## Testing Examples

Testing with Epsilon's test framework:

```lisp
(defpackage #:example.testing
  (:use #:common-lisp)
  (:local-nicknames
    (#:test #:epsilon.tool.test)
    (#:map #:epsilon.lib.map)
    (#:seq #:epsilon.lib.sequence)))

(in-package #:example.testing)

(test:define-test-suite user-management-tests
  "Test suite for user management functionality")

(test:define-test create-user-test (user-management-tests)
  "Test user creation with valid data"
  (let ((user-data {:name "Alice" :email "alice@example.com" :age 30}))
    (let ((user (create-user user-data)))
      (test:is (map:contains-p user :id))
      (test:is-equal "Alice" (map:get user :name))
      (test:is-equal "alice@example.com" (map:get user :email)))))

(test:define-test validate-user-data-test (user-management-tests)
  "Test user data validation"
  ;; Valid data
  (test:is (validate-user {:name "Bob" :email "bob@test.com" :age 25}))
  
  ;; Invalid data
  (test:is-not (validate-user {:name "" :email "invalid" :age -5}))
  (test:is-not (validate-user {:email "test@example.com"}))) ; missing name

(test:define-test user-search-test (user-management-tests)
  "Test user search functionality"
  (let ((users (list
                 {:name "Alice" :country "USA" :age 30}
                 {:name "Bob" :country "USA" :age 25}
                 {:name "Charlie" :country "UK" :age 35})))
    
    ;; Test country filter
    (let ((usa-users (search-users users :country "USA")))
      (test:is-equal 2 (length usa-users)))
    
    ;; Test age filter
    (let ((young-users (search-users users :min-age 30)))
      (test:is-equal 2 (length young-users)))))

;; Run tests
(defun run-all-tests ()
  "Run all test suites with reporting."
  (test:run-test-suite 'user-management-tests :verbose t))
```

These examples demonstrate Epsilon's capabilities across different domains while showcasing the functional programming patterns and API design.

---

*For more examples, see the source code and test suites in the Epsilon repository.*