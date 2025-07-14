# Getting Started

This guide assumes you have already [installed Epsilon](installation.md).

## Running Epsilon

Start an interactive session:

```bash
./run.sh
```

This launches a REPL with Epsilon's core libraries loaded.

## Basic Usage

### Working with Maps

Epsilon provides immutable maps:

```lisp
;; Create a map
(defparameter *config* 
  (epsilon.lib.map:make-map :host "localhost" 
                            :port 8080))

;; Access values
(epsilon.lib.map:get *config* :host)
;=> "localhost"

;; Update (returns new map, original unchanged)
(epsilon.lib.map:assoc *config* :port 3000)
;=> #<MAP :host "localhost" :port 3000>
```

### Working with Sequences

Lazy sequences support functional operations:

```lisp
;; Transform data
(epsilon.lib.sequence:realize 
  (epsilon.lib.sequence:map #'1+ 
    (epsilon.lib.sequence:seq '(1 2 3 4 5))))
;=> (2 3 4 5 6)

;; Filter values  
(epsilon.lib.sequence:realize 
  (epsilon.lib.sequence:filter #'evenp 
    (epsilon.lib.sequence:seq '(1 2 3 4 5 6))))
;=> (2 4 6)

;; Reduce to single value
(epsilon.lib.sequence:reduce #'+ 
  (epsilon.lib.sequence:seq '(1 2 3 4 5)))
;=> 15
```

### JSON Handling

```lisp
;; Parse JSON
(epsilon.lib.json:decode "{\"status\": \"ok\", \"count\": 42}")
;=> #<MAP "status" "ok" "count" 42>

;; Generate JSON
(epsilon.lib.json:encode 
  (epsilon.lib.map:make-map :status "ok" :count 42))
;=> "{\"status\":\"ok\",\"count\":42}"
```

## Writing Programs

Create `hello.lisp`:

```lisp
#!/usr/bin/env sbcl --script

(load "scripts/boot.lisp")
(epsilon.tool.boot:boot)

(defpackage :hello
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:json :epsilon.lib.json)
   (:seq :epsilon.lib.sequence)))

(in-package :hello)

(defun main ()
  (let ((data (map:make-map 
                :message "Hello from Epsilon"
                :timestamp (get-universal-time))))
    (format t "~A~%" (json:encode data))))

(main)
```

Run it:

```bash
sbcl --script hello.lisp
```

## Building Applications

For larger projects, use Epsilon's module system:

1. Create a module directory with `package.edn`
2. Define your packages and dependencies
3. Build with `./run.sh build your-module`

See the [Application Development Guide](app-development-guide.md) for details.

## Next Steps

- Review [code examples](examples.md)
- Learn about [module structure](../architecture/module-management.md)
- Explore the [architecture documentation](../architecture/)