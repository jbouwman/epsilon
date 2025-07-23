# JSON & YAML

Data serialization and parsing with Epsilon's encoding libraries.

## JSON Support

### Basic Usage

```lisp
(defpackage #:json-example
  (:use #:common-lisp)
  (:local-nicknames
    (#:json #:epsilon.json)
    (#:map #:epsilon.map)))

(in-package #:json-example)

;; Encoding data to JSON
(let ((data (map:make-map 
              :name "Alice"
              :age 30
              :hobbies '("reading" "hiking"))))
  (json:encode data))
;; => {"name":"Alice","age":30,"hobbies":["reading","hiking"]}

;; Parsing JSON strings
(json:decode "{\"x\": 42, \"y\": [1, 2, 3]}")
;; => #<MAP {:x 42, :y (1 2 3)}>
```

### Data Type Mapping

| Lisp Type | JSON Type | Notes |
|-----------|-----------|-------|
| Number | Number | Integers and floats |
| String | String | Unicode support |
| Boolean | Boolean | `t` → `true`, `nil` → `false` |
| List | Array | Preserves order |
| Map | Object | Key-value pairs |
| `nil` | `null` | Null value |

### Advanced JSON Features

#### Custom Encoding

```lisp
;; Custom object encoding
(defclass person ()
  ((name :initarg :name :reader person-name)
   (age :initarg :age :reader person-age)))

(defmethod json:encode-object ((person person))
  (map:make-map
    :type "Person"
    :name (person-name person)
    :age (person-age person)))

(let ((p (make-instance 'person :name "Bob" :age 25)))
  (json:encode p))
;; => {"type":"Person","name":"Bob","age":25}
```

#### Streaming Parser

For large JSON documents:

```lisp
(with-open-file (stream "large-data.json")
  (json:parse-stream stream
    :array-handler (lambda (items) 
                     (format t "Array with ~D items~%" (length items)))
    :object-handler (lambda (obj)
                      (format t "Object: ~A~%" obj))))
```

#### Configuration Options

```lisp
;; Pretty printing
(json:encode data :pretty t :indent 2)

;; Key transformation
(json:encode data :key-transform #'string-upcase)

;; Custom null handling
(json:decode "null" :null-value :null)
```

### Error Handling

```lisp
(handler-case
  (json:decode "invalid json")
  (json:parse-error (e)
    (format t "Parse error at position ~D: ~A~%" 
            (json:error-position e)
            (json:error-message e))))
```

## YAML Support

### Basic Usage

```lisp
(defpackage #:yaml-example
  (:use #:common-lisp)
  (:local-nicknames
    (#:yaml #:epsilon.yaml)
    (#:map #:epsilon.map)))

(in-package #:yaml-example)

;; Encoding to YAML
(let ((config (map:make-map
                :database (map:make-map
                            :host "localhost"
                            :port 5432)
                :features '("caching" "logging"))))
  (yaml:encode config))
```

Output:
```yaml
database:
  host: localhost
  port: 5432
features:
  - caching
  - logging
```

### Document Streams

YAML supports multiple documents in a single stream:

```lisp
;; Multiple documents
(yaml:encode-documents 
  (list (map:make-map :name "doc1" :value 1)
        (map:make-map :name "doc2" :value 2)))
```

Output:
```yaml
---
name: doc1
value: 1
---
name: doc2
value: 2
```

### Loading Documents

```lisp
;; Single document
(yaml:load "config.yaml")

;; Multiple documents
(yaml:load-all "multi-doc.yaml")
;; => ((#<MAP {:name "doc1", :value 1}>)
;;     (#<MAP {:name "doc2", :value 2}>))
```

### YAML-Specific Features

#### Anchors and References

```yaml
# Input YAML with anchors
default: &default
  timeout: 30
  retries: 3

development:
  <<: *default
  host: dev.example.com

production:
  <<: *default
  host: prod.example.com
```

```lisp
(yaml:load "config-with-anchors.yaml")
;; Automatically resolves references
```

#### Custom Tags

```lisp
;; Define custom tag handler
(yaml:define-tag "!person"
  (lambda (data)
    (make-instance 'person 
                   :name (map:get data :name)
                   :age (map:get data :age))))

;; Use in YAML
;; !person {name: "Charlie", age: 35}
```

### Configuration Files

Common pattern for application configuration:

```lisp
(defpackage #:app-config
  (:use #:common-lisp)
  (:local-nicknames
    (#:yaml #:epsilon.yaml)
    (#:map #:epsilon.map)))

(defclass config ()
  ((database-url :reader database-url)
   (port :reader port)
   (debug-mode :reader debug-mode)))

(defun load-config (filename)
  (let ((data (yaml:load filename)))
    (make-instance 'config
      :database-url (map:get-in data '(:database :url))
      :port (map:get data :port 8080)  ; default value
      :debug-mode (map:get data :debug nil))))

;; config.yaml:
;; database:
;;   url: "postgresql://localhost/myapp"
;; port: 3000
;; debug: true

(defparameter *config* (load-config "config.yaml"))
```

## Performance Considerations

### JSON Performance

- **Streaming parser** for large documents
- **Incremental parsing** reduces memory usage
- **Type-specific encoders** for performance-critical paths

```lisp
;; Fast path for simple data
(json:encode-fast simple-map)  ; Optimized for basic types

;; Memory-efficient parsing
(json:parse-lazy large-stream)  ; Lazy evaluation
```

### YAML Performance

- **Document caching** for frequently loaded files
- **Schema validation** can be disabled for speed
- **Binary mode** for faster I/O

```lisp
;; Performance options
(yaml:load "data.yaml" :validate nil :cache t)
```

## Integration Examples

### REST API Response

```lisp
(defun api-response (data &key (status 200))
  (map:make-map
    :status status
    :data data
    :timestamp (get-universal-time)))

(defun user-endpoint (user-id)
  (let ((user (find-user user-id)))
    (json:encode 
      (api-response 
        (map:make-map :id user-id :name (user-name user))))))
```

### Configuration Management

```lisp
(defclass environment-config ()
  ((name :initarg :name :reader env-name)
   (config :initarg :config :reader env-config)))

(defun load-environment-configs (directory)
  (mapcar (lambda (file)
            (make-instance 'environment-config
              :name (pathname-name file)
              :config (yaml:load file)))
          (directory (merge-pathnames "*.yaml" directory))))
```

### Data Pipeline

```lisp
(defun process-data-file (input-file output-file transform-fn)
  "Read JSON/YAML, transform, write back"
  (let* ((data (cond
                 ((string-suffix-p input-file ".json")
                  (json:load input-file))
                 ((string-suffix-p input-file ".yaml")
                  (yaml:load input-file))
                 (t (error "Unsupported file type"))))
         (transformed (funcall transform-fn data)))
    (cond
      ((string-suffix-p output-file ".json")
       (json:save transformed output-file :pretty t))
      ((string-suffix-p output-file ".yaml")
       (yaml:save transformed output-file)))))
```

## Error Recovery

### Robust Parsing

```lisp
(defun safe-parse-config (filename)
  "Parse config with fallback to defaults"
  (handler-case
    (cond
      ((string-suffix-p filename ".json")
       (json:load filename))
      ((string-suffix-p filename ".yaml")
       (yaml:load filename)))
    (error (e)
      (warn "Failed to parse ~A: ~A~%Using defaults." filename e)
      (map:make-map :debug t :port 8080))))
```

### Validation

```lisp
(defun validate-config (config required-keys)
  "Ensure required configuration keys are present"
  (dolist (key required-keys)
    (unless (map:contains-p config key)
      (error "Missing required config key: ~A" key)))
  config)

(defun load-and-validate-config (filename)
  (validate-config 
    (yaml:load filename)
    '(:database-url :secret-key :port)))
```

---

*Next: [Network Programming](../network/http.md) - HTTP client and server implementation*
