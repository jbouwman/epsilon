# Epsilon JSON Package

A comprehensive JSON parsing and encoding library built on parser combinators, providing high-performance JSON processing with detailed error reporting and full Unicode support.

## Overview

The Epsilon JSON package provides:

- **Complete JSON parsing** with full RFC 7159/8259 compliance
- **JSON encoding** from Epsilon data structures
- **Parser combinator foundation** for robust, composable parsing
- **Detailed error reporting** with line/column information
- **Unicode support** including escape sequences
- **Lazy tokenization** for memory-efficient processing
- **Integration** with Epsilon core data structures (maps, sequences)

## Quick Start

### Basic Parsing

```lisp
(use-package :epsilon.json)

;; Parse JSON strings
(parse "{\"name\": \"Alice\", \"age\": 30}")
;; => Epsilon map with :NAME "Alice" and :AGE 30

(parse "[1, 2, 3, \"hello\"]")
;; => Epsilon sequence with elements (1 2 3 "hello")

;; Parse JSON literals
(parse "true")     ; => T
(parse "false")    ; => NIL  
(parse "null")     ; => NIL
(parse "42")       ; => 42
(parse "3.14")     ; => 3.14
```

### JSON Encoding

```lisp
;; Encode Epsilon data structures to JSON
(encode (epsilon.map:make-map :name "Alice" :age 30))
;; => "{\"name\":\"Alice\",\"age\":30}"

(encode (epsilon.sequence:from-list '(1 2 3 "hello")))
;; => "[1,2,3,\"hello\"]"

;; Encode with pretty printing
(encode data :pretty t)
;; => Formatted JSON with indentation
```

### File Processing

```lisp
;; Parse JSON from file
(let ((json-string (epsilon.sys.fs:read-file "data.json")))
  (parse json-string))

;; Write JSON to file
(let ((data (epsilon.map:make-map :status "ok" :count 42)))
  (epsilon.sys.fs:write-file "output.json" (encode data :pretty t)))
```

## Core API

### Parsing Functions

**`parse`** `(json-string &key strict)`

Parse a JSON string into Epsilon data structures:

```lisp
;; Basic parsing
(parse "{\"key\": \"value\"}")

;; Strict mode (more validation)
(parse json-string :strict t)
```

**Return Values:**
- **Objects** → `epsilon.map:map`
- **Arrays** → `epsilon.sequence:sequence`  
- **Strings** → Common Lisp strings
- **Numbers** → Common Lisp numbers
- **true** → `T`
- **false/null** → `NIL`

### Encoding Functions

**`encode`** `(data &key pretty indent-size stream)`

Encode Epsilon data structures to JSON:

```lisp
;; Compact encoding
(encode data)

;; Pretty-printed encoding
(encode data :pretty t :indent-size 2)

;; Stream to output
(encode data :stream *standard-output*)
```

**Supported Input Types:**
- `epsilon.map:map` → JSON objects
- `epsilon.sequence:sequence` → JSON arrays
- `list` → JSON arrays
- `string` → JSON strings
- `number` → JSON numbers
- `T` → JSON true
- `NIL` → JSON null

### Tokenization

**`tokenize`** `(json-string)`

Low-level tokenization for advanced use cases:

```lisp
(tokenize "{\"key\": 42}")
;; => Sequence of tokens: (:LBRACE :STRING "key" :COLON :NUMBER 42 :RBRACE :EOF)
```

## Advanced Features

### Error Handling

The parser provides detailed error information:

```lisp
(handler-case
    (parse "{invalid json}")
  (json-parse-error (e)
    (format t "Parse error at line ~D, column ~D: ~A~%"
            (json-error-line e)
            (json-error-column e)
            (json-error-message e))))
```

### Unicode Support

Full Unicode support including escape sequences:

```lisp
;; Unicode literals
(parse "\"Hello, 世界\"")  ; => "Hello, 世界"

;; Unicode escapes
(parse "\"\\u4e16\\u754c\"")  ; => "世界"

;; Control character escapes
(parse "\"line1\\nline2\\ttabbed\"")  ; => "line1\nline2\ttabbed"
```

### Custom Object Construction

Control how JSON objects are constructed:

```lisp
;; Custom object builder
(defun custom-object-builder (pairs)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

;; Use with parser
(parse json-string :object-builder #'custom-object-builder)
```

### Streaming JSON

For large JSON documents:

```lisp
;; Stream encoding to avoid memory overhead
(with-open-file (stream "large-output.json" :direction :output)
  (encode large-data-structure :stream stream :pretty t))

;; Process JSON in chunks (for future versions)
(with-json-stream (stream "large-input.json")
  (loop for element = (read-json-element stream)
        while element
        do (process-element element)))
```

## Data Type Mapping

### JSON to Epsilon

| JSON Type | Epsilon Type | Example |
|-----------|--------------|---------|
| `{}` | `epsilon.map:map` | `(epsilon.map:make-map)` |
| `[]` | `epsilon.sequence:sequence` | `(epsilon.sequence:from-list '())` |
| `"string"` | `string` | `"hello"` |
| `42` | `integer` | `42` |
| `3.14` | `float` | `3.14` |
| `true` | `T` | `T` |
| `false` | `NIL` | `NIL` |
| `null` | `NIL` | `NIL` |

### Epsilon to JSON

| Epsilon Type | JSON Type | Example |
|--------------|-----------|---------|
| `epsilon.map:map` | `{}` | `{"key":"value"}` |
| `epsilon.sequence:sequence` | `[]` | `[1,2,3]` |
| `list` | `[]` | `[1,2,3]` |
| `string` | `"string"` | `"hello"` |
| `number` | number | `42` or `3.14` |
| `T` | `true` | `true` |
| `NIL` | `null` | `null` |

## Complex Examples

### Nested Data Structures

```lisp
;; Complex nested JSON
(let ((json "{
  \"users\": [
    {\"name\": \"Alice\", \"age\": 30, \"active\": true},
    {\"name\": \"Bob\", \"age\": 25, \"active\": false}
  ],
  \"metadata\": {
    \"total\": 2,
    \"page\": 1
  }
}"))
  (let ((data (parse json)))
    ;; Access nested data
    (let ((users (epsilon.map:get data "users"))
          (total (epsilon.map:get-in data '("metadata" "total"))))
      (format t "Found ~D users~%" total)
      (epsilon.sequence:each
        (lambda (user)
          (format t "User: ~A (age ~A)~%"
                  (epsilon.map:get user "name")
                  (epsilon.map:get user "age")))
        users))))
```

### Custom Data Processing

```lisp
;; Transform JSON during parsing
(defun process-json-api-response (json-string)
  (let ((data (parse json-string)))
    ;; Extract relevant fields
    (epsilon.map:make-map
      :items (epsilon.map:get data "data")
      :count (epsilon.map:get-in data '("meta" "total"))
      :has-more (epsilon.map:get-in data '("meta" "has_next_page")))))

;; Generate JSON API responses
(defun create-api-response (items &key total page)
  (encode
    (epsilon.map:make-map
      "data" items
      "meta" (epsilon.map:make-map
               "total" total
               "page" page
               "has_next_page" (> total (* page 20))))
    :pretty t))
```

### Configuration File Processing

```lisp
;; Load application configuration from JSON
(defun load-config (config-file)
  (let* ((json-string (epsilon.sys.fs:read-file config-file))
         (config (parse json-string)))
    ;; Validate required fields
    (unless (epsilon.map:get config "database")
      (error "Missing database configuration"))
    (unless (epsilon.map:get config "server")
      (error "Missing server configuration"))
    config))

;; Save configuration
(defun save-config (config config-file)
  (epsilon.sys.fs:write-file 
    config-file 
    (encode config :pretty t :indent-size 4)))
```

## Performance Considerations

### Memory Usage

```lisp
;; For large JSON documents, consider streaming
(defun process-large-json (filename)
  (let ((json-string (epsilon.sys.fs:read-file filename)))
    ;; Parse incrementally if possible
    (if (< (length json-string) 10000000)  ; 10MB
        (parse json-string)
        (warn "Large JSON file, consider streaming approach"))))
```

### Parsing Performance

```lisp
;; Reuse tokenizer for multiple small JSON strings
(let ((tokenizer (make-json-tokenizer)))
  (dolist (json-string json-strings)
    (let ((tokens (tokenize-with tokenizer json-string)))
      (process-tokens tokens))))
```

### Encoding Performance

```lisp
;; Pre-allocate output for known sizes
(defun fast-encode-array (items)
  (with-output-to-string (stream)
    (write-char #\[ stream)
    (loop for item in items
          for first = t then nil
          unless first do (write-char #\, stream)
          do (write-string (encode item) stream))
    (write-char #\] stream)))
```

## Integration Examples

### Web API Integration

```lisp
;; HTTP client JSON processing
(defun fetch-user-data (user-id)
  (multiple-value-bind (status headers body)
      (epsilon.http:http-get 
        (format nil "https://api.example.com/users/~A" user-id)
        :headers (epsilon.map:make-map "Accept" "application/json"))
    (when (= status 200)
      (parse body))))

;; HTTP server JSON responses
(epsilon.http:define-handler (:post "/api/users") (request)
  (let* ((body (epsilon.http.request:request-body request))
         (user-data (parse body)))
    ;; Process user data
    (let ((created-user (create-user user-data)))
      (epsilon.http.response:json-response 
        (epsilon.map:make-map 
          "id" (user-id created-user)
          "message" "User created successfully")
        :status 201))))
```

### Database Integration

```lisp
;; Store JSON data in database
(defun store-json-document (collection document)
  (let ((json-string (encode document)))
    (db:insert collection (list :data json-string))))

;; Retrieve and parse JSON data
(defun get-json-document (collection id)
  (let ((record (db:find-by-id collection id)))
    (when record
      (parse (getf record :data)))))
```

### Log Processing

```lisp
;; Parse JSON log entries
(defun process-json-logs (log-file)
  (with-open-file (stream log-file)
    (loop for line = (read-line stream nil)
          while line
          do (handler-case
                 (let ((log-entry (parse line)))
                   (process-log-entry log-entry))
               (json-parse-error (e)
                 (warn "Skipping malformed log entry: ~A" e))))))
```

## Error Handling

### Common Error Types

```lisp
;; Handle specific JSON errors
(handler-bind 
    ((json-parse-error 
       (lambda (e)
         (format t "JSON syntax error: ~A~%" e)))
     (json-unicode-error
       (lambda (e)
         (format t "Unicode encoding error: ~A~%" e)))
     (json-number-error
       (lambda (e)
         (format t "Invalid number format: ~A~%" e))))
  (parse potentially-malformed-json))
```

### Validation

```lisp
;; Validate JSON structure
(defun validate-user-json (json-string)
  (let ((data (parse json-string)))
    ;; Check required fields
    (unless (epsilon.map:contains-p data "name")
      (error "Missing required field: name"))
    (unless (epsilon.map:contains-p data "email")
      (error "Missing required field: email"))
    ;; Validate types
    (unless (stringp (epsilon.map:get data "name"))
      (error "Name must be a string"))
    data))
```

## Testing

### Test Data

The package includes comprehensive test data:
- `simple-object.json` - Basic object structure
- `simple-array.json` - Array with mixed types  
- `nested-object.json` - Complex nested structures
- `empty-structures.json` - Edge cases with empty objects/arrays
- `simple-string.json` - String with escape sequences
- `simple-number.json` - Various number formats

### Running Tests

```bash
# Run JSON package tests
./epsilon test --module epsilon.json

# Run specific test categories
./epsilon test --module epsilon.json --pattern "*parsing*"
./epsilon test --module epsilon.json --pattern "*encoding*"
```

## Best Practices

### Data Validation

```lisp
;; Always validate external JSON
(defun safe-parse-api-response (json-string)
  (handler-case
      (let ((data (parse json-string :strict t)))
        ;; Validate expected structure
        (validate-api-response-structure data)
        data)
    (json-parse-error (e)
      (error "Invalid JSON from API: ~A" e))))
```

### Memory Management

```lisp
;; For large datasets, process incrementally
(defun process-large-json-array (json-string)
  (let ((data (parse json-string)))
    (when (epsilon.sequence:sequence-p data)
      ;; Process in chunks to avoid memory pressure
      (loop for chunk in (epsilon.sequence:partition 1000 data)
            do (process-chunk chunk)))))
```

### Error Recovery

```lisp
;; Graceful error handling
(defun robust-json-processor (json-strings)
  (loop for json in json-strings
        for result = (handler-case
                         (parse json)
                       (json-parse-error (e)
                         (warn "Skipping invalid JSON: ~A" e)
                         nil))
        when result collect result))
```

## Dependencies

- **epsilon.core** - Core data structures and utilities
- **epsilon.parsing** - Parser combinator foundation
- **SBCL** - Steel Bank Common Lisp

The Epsilon JSON package provides a robust, high-performance JSON processing solution with excellent integration into the Epsilon ecosystem and comprehensive error handling suitable for production applications.