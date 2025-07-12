# Getting Started

This guide covers basic usage of Epsilon after [installation](installation.md).

## Interactive REPL

Start an interactive session:

```bash
epsilon
```

Load Epsilon libraries:

```lisp
;; The core libraries are already available
(map:make-map :a 1 :b 2)
;=> {:a 1, :b 2}
```

## Command Line Usage

```bash
# Evaluate expressions
epsilon --eval "(format t \"Hello, Epsilon!\")" --eval "(sb-ext:quit)"

# Run a script
epsilon --script my-program.lisp
```

## Your First Program

Create `hello.lisp`:

```lisp
(defpackage :hello
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:json :epsilon.lib.json)))

(in-package :hello)

(defun main ()
  (let ((data (map:make-map 
                :message "Hello, Epsilon!"
                :timestamp (get-universal-time))))
    (format t "~A~%" (json:encode data))))

(main)
```

Run it:

```bash
epsilon --script hello.lisp
```

## Working with Data Structures

### Maps

```lisp
;; Create a map
(defparameter *user* 
  (map:make-map :name "Alice" 
                :age 30
                :email "alice@example.com"))

;; Access values
(map:get *user* :name)
;=> "Alice"

;; Update (returns new map)
(map:assoc *user* :age 31)
;=> {:name "Alice", :age 31, :email "alice@example.com"}
```

### Sequences

```lisp
;; Map over sequences
(seq:map #'1+ '(1 2 3 4 5))
;=> (2 3 4 5 6)

;; Filter
(seq:filter #'evenp '(1 2 3 4 5 6))
;=> (2 4 6)

;; Reduce
(seq:reduce #'+ '(1 2 3 4 5))
;=> 15
```

## Working with JSON

```lisp
;; Parse JSON
(json:decode "{\"users\": [{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]}")
;=> (("users" . #((("name" . "Alice")) (("name" . "Bob")))))

;; Generate JSON
(json:encode '((:status . "ok") (:count . 42)))
;=> "{\"status\":\"ok\",\"count\":42}"
```

## Next Steps

- Explore the [API Reference](reference/api.md) for complete documentation
- See [Examples](reference/examples.md) for more code samples
- Learn about the [Build System](tools/build.md) for larger projects