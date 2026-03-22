# epsilon.json

Fast JSON parser and encoder for Epsilon using parser combinators.

## Installation

```lisp
;; In your module definition
(package my-module
  (import (epsilon.json json)))
```

## Quick Start

```lisp
;; Parse JSON string
(json:parse "{\"name\": \"Epsilon\", \"version\": 14}")
;; => (:NAME "Epsilon" :VERSION 14)

;; Encode to JSON
(json:encode '(:name "Epsilon" :version 14))
;; => "{\"name\":\"Epsilon\",\"version\":14}"
```

## Parsing

### Basic Parsing

```lisp
;; Parse JSON string to Lisp data
(json:parse "{\"key\": \"value\"}")
;; => (:KEY "value")

(json:parse "[1, 2, 3]")
;; => (1 2 3)

(json:parse "true")
;; => T

(json:parse "null")
;; => NIL
```

### Type Mapping

| JSON | Lisp |
|------|------|
| `{}` object | Plist `(:key value ...)` |
| `[]` array | List `(...)` |
| `"string"` | String `"..."` |
| `123` | Integer `123` |
| `12.34` | Float `12.34` |
| `true` | `T` |
| `false` | `NIL` |
| `null` | `NIL` |

### Parsing Options

```lisp
;; Parse with keyword keys (default)
(json:parse "{\"name\": \"test\"}")
;; => (:NAME "test")

;; Parse with string keys
(json:parse "{\"name\": \"test\"}" :key-fn #'identity)
;; => ("name" "test")

;; Parse with custom key transformation
(json:parse "{\"user_name\": \"alice\"}"
  :key-fn (lambda (k) (intern (string-upcase (substitute #\- #\_ k)) :keyword)))
;; => (:USER-NAME "alice")
```

### Parsing from Streams

```lisp
;; Parse from file
(with-open-file (s "data.json")
  (json:parse-stream s))

;; Parse from string stream
(with-input-from-string (s "{\"a\": 1}")
  (json:parse-stream s))
```

### Error Handling

```lisp
(handler-case
    (json:parse invalid-json)
  (json:parse-error (e)
    (format t "Parse error at line ~D, column ~D: ~A"
            (json:error-line e)
            (json:error-column e)
            (json:error-message e))))
```

## Encoding

### Basic Encoding

```lisp
;; Encode plist as JSON object
(json:encode '(:name "Alice" :age 30))
;; => "{\"name\":\"Alice\",\"age\":30}"

;; Encode list as JSON array
(json:encode '(1 2 3))
;; => "[1,2,3]"

;; Encode nested structures
(json:encode '(:users ((:name "Alice") (:name "Bob"))))
;; => "{\"users\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"}]}"
```

### Encoding Options

```lisp
;; Pretty print with indentation
(json:encode data :pretty t)
;; => "{
;;      \"name\": \"Alice\",
;;      \"age\": 30
;;    }"

;; Custom indentation
(json:encode data :pretty t :indent 4)

;; Encode to stream
(with-open-file (s "output.json" :direction :output)
  (json:encode-to-stream data s))

;; Encode to string (default)
(json:encode-to-string data)
```

### Type Mapping

| Lisp | JSON |
|------|------|
| Plist `(:key value ...)` | Object `{}` |
| Hash table | Object `{}` |
| List `(...)` | Array `[]` |
| Vector `#(...)` | Array `[]` |
| String `"..."` | String `"..."` |
| Integer | Number |
| Float | Number |
| `T` | `true` |
| `NIL` | `null` or `false` |
| Symbol | String (symbol name) |

### Encoding NIL

By default, `NIL` encodes as `null`. To encode as `false`:

```lisp
;; Use explicit boolean
(json:encode '(:active :false))
;; => "{\"active\":false}"

;; Or use json:false
(json:encode `(:active ,json:false))
;; => "{\"active\":false}"
```

### Custom Encoders

```lisp
;; Define encoder for custom type
(defmethod json:encode-value ((obj my-object) stream)
  (json:encode-value
    (list :type "my-object"
          :data (my-object-data obj))
    stream))

;; Now custom objects can be encoded
(json:encode (make-my-object :data "test"))
;; => "{\"type\":\"my-object\",\"data\":\"test\"}"
```

## Streaming

### Incremental Parsing

```lisp
;; Parse large files incrementally
(json:with-json-stream (s "large-file.json")
  (json:read-array-start s)
  (loop while (json:has-more s)
        for item = (json:read-value s)
        do (process-item item)))
```

### Incremental Encoding

```lisp
;; Encode large arrays incrementally
(with-open-file (out "output.json" :direction :output)
  (json:with-json-writer (w out)
    (json:write-array-start w)
    (dolist (item items)
      (json:write-value w item))
    (json:write-array-end w)))
```

## Performance

The JSON module is optimized for performance:

- Single-pass tokenizer
- Direct output to streams (no intermediate strings)
- Minimal allocations during parsing

For very large files, use streaming APIs to avoid loading everything into memory.

## Examples

### Parse Configuration File

```lisp
(defun load-config (path)
  (with-open-file (s path)
    (json:parse-stream s)))

(let ((config (load-config "config.json")))
  (getf config :database-url))
```

### HTTP API Response

```lisp
(let* ((response (http:get "https://api.example.com/users"))
       (data (json:parse (http:response-body response))))
  (dolist (user (getf data :users))
    (format t "User: ~A~%" (getf user :name))))
```

### Write JSON Log

```lisp
(defun log-event (event)
  (let ((entry (list :timestamp (get-universal-time)
                     :event event)))
    (with-open-file (s "events.jsonl"
                       :direction :output
                       :if-exists :append)
      (json:encode-to-stream entry s)
      (terpri s))))
```

## See Also

- [epsilon.http](../http/) - HTTP client with JSON support
- [RFC 8259](https://tools.ietf.org/html/rfc8259) - JSON specification
