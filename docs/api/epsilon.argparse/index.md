# epsilon.argparse

Argument parsing library for command-line interfaces with hierarchical command structure, automatic help generation, and type validation.

## Overview

The `epsilon.argparse` package parses command-line arguments in Common Lisp applications. It supports:

- Global options before commands
- Hierarchical command structure (subcommands)
- Automatic help generation
- Type validation and conversion
- Both positional and optional arguments
- Various argument actions (store, store-true, store-false, append)

## Quick Start

```text
(defpackage #:my-app
  (:use #:cl)
  (:local-nicknames (#:argparse #:epsilon.argparse)))

(in-package #:my-app)

;; Create a parser
(let ((parser (argparse:make-parser 
               :prog "my-app"
               :description "My awesome application")))
  
  ;; Add arguments
  (argparse:add-argument parser "--verbose" 
                        :action 'store-true
                        :help "Enable verbose output")
  
  (argparse:add-argument parser "input" 
                        :help "Input file to process")
  
  ;; Parse arguments
  (let* ((args '("--verbose" "data.txt"))
         (parsed (argparse:parse-args parser args)))
    
    ;; Access parsed values
    (when (argparse:get-option parsed "verbose")
      (format t "Verbose mode enabled~%"))
    
    (format t "Processing: ~A~%" 
            (first (argparse:parsed-positionals parsed)))))
```

## Main Functions

### Parser Creation

#### make-parser

Creates a new argument parser.

```common-lisp
(make-parser &key prog description epilog) → parser
```

**Arguments:**
- `prog` - Program name to display in help (optional)
- `description` - Program description for help text (optional)
- `epilog` - Text to display after help (optional)

**Example:**
```common-lisp
(make-parser :prog "epsilon"
             :description "Epsilon development tools"
             :epilog "For more info, see https://epsilon.dev")
```

### Adding Arguments

#### add-argument

Adds an argument to the parser.

```lisp
(add-argument parser name &rest args) → argument
```

**Arguments:**
- `parser` - The parser to add the argument to
- `name` - Argument name (e.g., "--verbose" or "filename")
- `&rest args` - Keyword arguments:
  - `:help` - Help text for this argument
  - `:type` - Type to convert to ('string, 'integer, 'boolean, or a function)
  - `:default` - Default value if not provided
  - `:required` - Whether this argument is required
  - `:action` - Action to take ('store, 'store-true, 'store-false, 'append)
  - `:choices` - List of valid choices
  - `:metavar` - Name to use in help messages
  - `:nargs` - Number of values (nil, '+, '*, '?, or an integer)
  - `:dest` - Destination key for storing the value

**Examples:**
```lisp
;; Optional flag
(add-argument parser "--verbose" 
              :action 'store-true
              :help "Enable verbose output")

;; Required positional argument
(add-argument parser "input" 
              :required t
              :help "Input file to process")

;; Optional with choices
(add-argument parser "--format" 
              :choices '("json" "yaml" "xml")
              :default "json"
              :help "Output format")

;; Multiple values
(add-argument parser "files" 
              :nargs '+
              :help "Files to process")

;; Integer type
(add-argument parser "--port" 
              :type 'integer
              :default 8080
              :help "Port to listen on")
```

### Subcommands

#### add-command

Adds a subcommand to the parser.

```lisp
(add-command parser name &key description help) → parser
```

**Arguments:**
- `parser` - The parent parser
- `name` - Command name
- `description` - Command description
- `help` - Help text (alias for description)

**Example:**
```lisp
(let* ((parser (make-parser :prog "git"))
       (add-cmd (add-command parser "add" 
                            :description "Add files to staging")))
  
  ;; Add arguments to the subcommand
  (add-argument add-cmd "files" 
                :nargs '+
                :help "Files to add"))
```

### Parsing

#### parse-args

Parses command line arguments.

```lisp
(parse-args parser args) → parsed-arguments
```

**Arguments:**
- `parser` - The parser to use
- `args` - List of argument strings to parse

**Returns:** A `parsed-arguments` object

**Example:**
```lisp
(let ((parsed (parse-args parser '("--verbose" "add" "file1.txt" "file2.txt"))))
  ;; Access results...
  )
```

### Accessing Results

#### parsed-command

Get the invoked subcommand name.

```lisp
(parsed-command parsed-arguments) → string or nil
```

#### parsed-options

Get the map of parsed options.

```lisp
(parsed-options parsed-arguments) → map
```

#### parsed-positionals

Get the list of positional argument values.

```lisp
(parsed-positionals parsed-arguments) → list
```

#### parsed-remaining

Get remaining unparsed arguments.

```lisp
(parsed-remaining parsed-arguments) → list
```

#### get-option

Convenience function to get an option value.

```lisp
(get-option parsed name &optional default) → value
```

**Example:**
```lisp
(let ((verbose (get-option parsed "verbose" nil))
      (format (get-option parsed "format" "json")))
  ...)
```

### Help Generation

#### print-help

Prints help message for a parser.

```lisp
(print-help parser &optional stream) → nil
```

**Arguments:**
- `parser` - The parser to print help for
- `stream` - Output stream (default: `*standard-output*`)

#### print-usage

Prints just the usage line.

```lisp
(print-usage parser &optional stream) → nil
```

## Argument Types

The `:type` parameter supports:

- `'string` - Default, no conversion
- `'integer` - Converts to integer using `parse-integer`
- `'boolean` - Converts "true", "yes", "1", "on" to `t`, others to `nil`
- Custom function - Called with the string value, should return converted value

**Example:**
```lisp
;; Custom type converter
(add-argument parser "--date" 
              :type (lambda (s) 
                      (local-time:parse-timestring s))
              :help "Date in ISO format")
```

## Argument Actions

The `:action` parameter controls how values are stored:

- `'store` - Store the value (default)
- `'store-true` - Store `t` when flag is present
- `'store-false` - Store `nil` when flag is present
- `'append` - Append values to a list

**Example:**
```lisp
;; Multiple --include options accumulate
(add-argument parser "--include" 
              :action 'append
              :help "Include directory (can be repeated)")

;; Usage: program --include /usr/include --include /usr/local/include
```

## Error Handling

The library provides several condition types for error handling:

- `argument-error` - Base condition for all argument errors
- `unknown-argument-error` - Unknown argument provided
- `missing-argument-error` - Required argument not provided
- `invalid-choice-error` - Value not in allowed choices
- `type-conversion-error` - Failed to convert value to specified type

**Example:**
```lisp
(handler-case
    (parse-args parser args)
  (argparse:argument-error (e)
    (format *error-output* "Error: ~A~%" 
            (argparse:error-message e))
    (argparse:print-help parser *error-output*)
    (sb-ext:exit :code 1)))
```

## Complete Example

Here's a complete example showing a CLI tool with subcommands:

```lisp
(defun create-parser ()
  (let ((parser (argparse:make-parser 
                 :prog "mytool"
                 :description "A tool with subcommands")))
    
    ;; Global options
    (argparse:add-argument parser "--config" 
                          :help "Config file path"
                          :default "~/.mytool.conf")
    
    ;; Subcommands
    (let ((serve-cmd (argparse:add-command parser "serve" 
                                          :description "Start server"))
          (build-cmd (argparse:add-command parser "build" 
                                          :description "Build project")))
      
      ;; serve command options
      (argparse:add-argument serve-cmd "--port" 
                            :type 'integer
                            :default 8080
                            :help "Port to listen on")
      (argparse:add-argument serve-cmd "--host" 
                            :default "localhost"
                            :help "Host to bind to")
      
      ;; build command options
      (argparse:add-argument build-cmd "target" 
                            :help "Build target")
      (argparse:add-argument build-cmd "--release" 
                            :action 'store-true
                            :help "Build in release mode"))
    
    parser))

(defun main (args)
  (let ((parser (create-parser)))
    (handler-case
        (let* ((parsed (argparse:parse-args parser args))
               (command (argparse:parsed-command parsed)))
          
          (cond
            ((string= command "serve")
             (let ((port (get-option parsed "port"))
                   (host (get-option parsed "host")))
               (format t "Starting server on ~A:~D~%" host port)))
            
            ((string= command "build")
             (let ((target (first (argparse:parsed-positionals parsed)))
                   (release (get-option parsed "release")))
               (format t "Building ~A~:[~; in release mode~]~%" 
                       target release)))
            
            (t
             (argparse:print-help parser))))
      
      (argparse:argument-error (e)
        (format *error-output* "Error: ~A~%~%" 
                (argparse:error-message e))
        (argparse:print-help parser *error-output*)
        (sb-ext:exit :code 1)))))
```

## Best Practices

1. **Use descriptive help texts** - Every argument should have a clear `:help` description
2. **Set appropriate defaults** - Provide sensible defaults where applicable
3. **Validate early** - Use `:choices` and `:type` to validate input during parsing
4. **Handle errors gracefully** - Always wrap `parse-args` in error handling
5. **Structure commands logically** - Group related functionality into subcommands
6. **Be consistent** - Follow common CLI conventions (e.g., `--help`, `--version`)

## Integration with Epsilon

The argparse library integrates seamlessly with other Epsilon components:

```lisp
;; With epsilon.sys.fs for path validation
(add-argument parser "--output" 
              :type (lambda (s) 
                      (let ((path (fs:path s)))
                        (unless (fs:writable-p (fs:parent path))
                          (error "Output directory not writable"))
                        path))
              :help "Output file path")

;; With epsilon.map for options storage
(let ((options (argparse:parsed-options parsed)))
  (map:each (lambda (k v)
              (format t "Option ~A = ~A~%" k v))
            options))
```

## See Also

- [epsilon.tool.dev](../epsilon.tool.dev/) - Example usage in Epsilon's CLI tools
- [Command-line Interface Guidelines](https://clig.dev/) - General CLI best practices
