;;;; Tests for Argument Parsing Library

(defpackage #:epsilon.argparse-tests
  (:use #:cl #:epsilon.argparse)
  (:local-nicknames
   (#:map #:epsilon.map)))

(in-package #:epsilon.argparse-tests)

(epsilon.test:deftest test-basic-parser-creation ()
  "Test creating a basic parser"
  (let ((parser (make-parser :prog "myapp" 
                            :description "A test application"
                            :epilog "See docs for more info")))
    (epsilon.test:is-equal "myapp" (parser-prog parser))
    (epsilon.test:is-equal "A test application" (parser-description parser))
    (epsilon.test:is-equal "See docs for more info" (parser-epilog parser))))

(epsilon.test:deftest test-add-arguments ()
  "Test adding various types of arguments"
  (let ((parser (make-parser :prog "test")))
    ;; Add option argument
    (let ((arg (add-argument parser "--verbose" 
                           :help "Enable verbose output"
                           :action 'store-true)))
      (epsilon.test:is-equal "--verbose" (argument-name arg))
      (epsilon.test:is-equal "Enable verbose output" (argument-help arg))
      (epsilon.test:is-equal 'store-true (argument-action arg)))
    
    ;; Add positional argument
    (let ((arg (add-argument parser "filename"
                           :help "Input file"
                           :type 'string
                           :required t)))
      (epsilon.test:is-equal "filename" (argument-name arg))
      (epsilon.test:is-equal t (argument-required arg)))
    
    ;; Add argument with choices
    (let ((arg (add-argument parser "--format"
                           :help "Output format"
                           :choices '("json" "xml" "yaml")
                           :default "json")))
      (epsilon.test:is-equal '("json" "xml" "yaml") (argument-choices arg))
      (epsilon.test:is-equal "json" (argument-default arg)))))

(epsilon.test:deftest test-parse-simple-args ()
  "Test parsing simple arguments"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--verbose" :action 'store-true)
    (add-argument parser "--output" :help "Output file")
    (add-argument parser "input" :help "Input file")
    
    ;; Test basic parsing
    (let ((result (parse-args parser '("--verbose" "--output" "out.txt" "in.txt"))))
      (epsilon.test:is-equal t (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal "out.txt" (map:get (parsed-options result) "output"))
      (epsilon.test:is-equal '("in.txt") (parsed-positionals result)))
    
    ;; Test with equals syntax
    (let ((result (parse-args parser '("--output=out.txt" "in.txt"))))
      (epsilon.test:is-equal "out.txt" (map:get (parsed-options result) "output"))
      (epsilon.test:is-equal '("in.txt") (parsed-positionals result)))
    
    ;; Test defaults
    (let ((result (parse-args parser '("in.txt"))))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "output")))))

(epsilon.test:deftest test-type-conversion ()
  "Test type conversion for arguments"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--count" :type 'integer :default 1)
    (add-argument parser "--enabled" :type 'boolean)
    (add-argument parser "port" :type 'integer)
    
    ;; Test integer conversion
    (let ((result (parse-args parser '("--count" "42" "8080"))))
      (epsilon.test:is-equal 42 (map:get (parsed-options result) "count"))
      (epsilon.test:is-equal '("8080") (parsed-positionals result)))
    
    ;; Test boolean conversion
    (let ((result (parse-args parser '("--enabled" "true" "8080"))))
      (epsilon.test:is-equal t (map:get (parsed-options result) "enabled")))
    
    (let ((result (parse-args parser '("--enabled" "false" "8080"))))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "enabled")))
    
    ;; Test default values
    (let ((result (parse-args parser '("8080"))))
      (epsilon.test:is-equal 1 (map:get (parsed-options result) "count")))))

(epsilon.test:deftest test-subcommands ()
  "Test parsing with subcommands"
  (let ((parser (make-parser :prog "git")))
    (add-argument parser "--verbose" :action 'store-true)
    
    ;; Add commit subcommand
    (let ((commit-parser (add-command parser "commit" :help "Record changes")))
      (add-argument commit-parser "-m" :help "Commit message" :required t)
      (add-argument commit-parser "--amend" :action 'store-true))
    
    ;; Add push subcommand
    (let ((push-parser (add-command parser "push" :help "Update remote refs")))
      (add-argument push-parser "--force" :action 'store-true)
      (add-argument push-parser "remote" :help "Remote name")
      (add-argument push-parser "branch" :help "Branch name"))
    
    ;; Test commit command
    (let ((result (parse-args parser '("--verbose" "commit" "-m" "Initial commit"))))
      (epsilon.test:is-equal "commit" (parsed-command result))
      (epsilon.test:is-equal t (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal "Initial commit" (map:get (parsed-options result) "m")))
    
    ;; Test push command
    (let ((result (parse-args parser '("push" "--force" "origin" "main"))))
      (epsilon.test:is-equal "push" (parsed-command result))
      (epsilon.test:is-equal t (map:get (parsed-options result) "force"))
      (epsilon.test:is-equal '("origin" "main") (parsed-positionals result)))
    
    ;; Test global options after command
    (let ((result (parse-args parser '("commit" "-m" "test" "--amend"))))
      (epsilon.test:is-equal "commit" (parsed-command result))
      (epsilon.test:is-equal t (map:get (parsed-options result) "amend")))))

(epsilon.test:deftest test-argument-validation ()
  "Test argument validation and error handling"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--format" 
                  :choices '("json" "xml" "yaml")
                  :required t)
    (add-argument parser "file" :required t)
    
    ;; Test missing required argument
    (epsilon.test:is-thrown (missing-argument-error)
      (parse-args parser '()))
    
    ;; Test invalid choice
    (epsilon.test:is-thrown (invalid-choice-error)
      (parse-args parser '("--format" "pdf" "file.txt")))
    
    ;; Test type conversion error
    (add-argument parser "--port" :type 'integer)
    (epsilon.test:is-thrown (type-conversion-error)
      (parse-args parser '("--format" "json" "--port" "abc" "file.txt")))))

(epsilon.test:deftest test-append-action ()
  "Test append action for collecting multiple values"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--include" :action 'append)
    (add-argument parser "--define" :action 'append)
    
    ;; Test multiple values
    (let ((result (parse-args parser '("--include" "foo" "--include" "bar" 
                                      "--define" "X=1" "--define" "Y=2"))))
      (epsilon.test:is-equal '("foo" "bar") (map:get (parsed-options result) "include"))
      (epsilon.test:is-equal '("X=1" "Y=2") (map:get (parsed-options result) "define")))
    
    ;; Test with equals syntax
    (let ((result (parse-args parser '("--include=foo" "--include=bar"))))
      (epsilon.test:is-equal '("foo" "bar") (map:get (parsed-options result) "include")))))

(epsilon.test:deftest test-store-true-false ()
  "Test store-true and store-false actions"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--verbose" :action 'store-true)
    (add-argument parser "--quiet" :action 'store-false :default t)
    (add-argument parser "--debug" :action 'store-true :default nil)
    
    ;; Test store-true
    (let ((result (parse-args parser '("--verbose"))))
      (epsilon.test:is-equal t (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal t (map:get (parsed-options result) "quiet"))  ; default
      (epsilon.test:is-equal nil (map:get (parsed-options result) "debug"))) ; default
    
    ;; Test store-false
    (let ((result (parse-args parser '("--quiet"))))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "quiet"))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "debug")))))

(epsilon.test:deftest test-help-generation ()
  "Test help message generation"
  (let ((parser (make-parser :prog "epsilon"
                            :description "Epsilon development tool"
                            :epilog "For more info, see docs")))
    (add-argument parser "--verbose" 
                  :action 'store-true
                  :help "Enable verbose output")
    (add-argument parser "--log"
                  :metavar "SPEC"
                  :help "Configure logging")
    (add-argument parser "command"
                  :help "Command to run")
    
    ;; Add subcommand
    (let ((build-parser (add-command parser "build" 
                                   :description "Build epsilon modules")))
      (add-argument build-parser "--force"
                    :action 'store-true
                    :help "Force rebuild")
      (add-argument build-parser "modules"
                    :help "Modules to build"))
    
    ;; Test usage generation
    (let ((output (with-output-to-string (s)
                    (print-usage parser s))))
      (epsilon.test:is (search "epsilon [options] <command> [arguments]" output)))
    
    ;; Test full help
    (let ((output (with-output-to-string (s)
                    (print-help parser s))))
      (epsilon.test:is (search "Epsilon development tool" output))
      (epsilon.test:is (search "Enable verbose output" output))
      (epsilon.test:is (search "Commands:" output))
      (epsilon.test:is (search "build" output))
      (epsilon.test:is (search "For more info, see docs" output)))))

(epsilon.test:deftest test-complex-parsing-scenario ()
  "Test complex real-world parsing scenario"
  (let ((parser (make-parser :prog "epsilon")))
    ;; Global options
    (add-argument parser "--log" :help "Configure logging")
    (add-argument parser "--verbose" :action 'store-true)
    (add-argument parser "--quiet" :action 'store-true)
    
    ;; Test command
    (let ((test-parser (add-command parser "test" :help "Run tests")))
      (add-argument test-parser "--module" :help "Modules to test")
      (add-argument test-parser "--format" 
                    :choices '("detailed" "brief" "junit")
                    :default "detailed")
      (add-argument test-parser "--test" :help "Test pattern"))
    
    ;; Build command
    (let ((build-parser (add-command parser "build" :help "Build modules")))
      (add-argument build-parser "--force" :action 'store-true)
      (add-argument build-parser "modules" :help "Modules to build"))
    
    ;; Test complex command line
    (let ((result (parse-args parser 
                             '("--log" "debug:epsilon.*" "--verbose" "test" 
                               "--module" "epsilon.core" "--test" "parse-*"))))
      (epsilon.test:is-equal "test" (parsed-command result))
      (epsilon.test:is-equal "debug:epsilon.*" (map:get (parsed-options result) "log"))
      (epsilon.test:is-equal t (map:get (parsed-options result) "verbose"))
      (epsilon.test:is-equal "epsilon.core" (map:get (parsed-options result) "module"))
      (epsilon.test:is-equal "parse-*" (map:get (parsed-options result) "test"))
      (epsilon.test:is-equal "detailed" (map:get (parsed-options result) "format")))))

(epsilon.test:deftest test-remaining-args ()
  "Test handling of remaining/unknown arguments"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--known" :help "A known option")
    (add-argument parser "file" :help "Input file")
    
    ;; Test with unknown options
    (let ((result (parse-args parser '("--known" "value" "input.txt" 
                                      "--unknown" "arg" "extra"))))
      (epsilon.test:is-equal "value" (map:get (parsed-options result) "known"))
      (epsilon.test:is-equal '("input.txt") (parsed-positionals result))
      (epsilon.test:is-equal '("--unknown" "arg" "extra") (parsed-remaining result)))
    
    ;; Test with -- separator
    (let ((result (parse-args parser '("--known" "value" "--" 
                                      "--not-an-option" "file.txt"))))
      (epsilon.test:is-equal "value" (map:get (parsed-options result) "known"))
      (epsilon.test:is-equal '() (parsed-positionals result))
      (epsilon.test:is-equal '("--not-an-option" "file.txt") (parsed-remaining result)))))

(epsilon.test:deftest test-custom-type-function ()
  "Test custom type conversion function"
  (let ((parser (make-parser :prog "test")))
    ;; Add custom type that parses key=value pairs
    (flet ((parse-pair (s)
             (let ((pos (position #\= s)))
               (if pos
                   (cons (subseq s 0 pos) (subseq s (1+ pos)))
                   (error "Invalid pair format: ~A" s)))))
      (add-argument parser "--define" 
                    :type #'parse-pair
                    :action 'append
                    :help "Define key=value pair"))
    
    ;; Test custom type
    (let ((result (parse-args parser '("--define" "FOO=bar" "--define" "X=42"))))
      (epsilon.test:is-equal '(("FOO" . "bar") ("X" . "42")) 
                (map:get (parsed-options result) "define")))
    
    ;; Test error handling
    (epsilon.test:is-thrown (type-conversion-error)
      (parse-args parser '("--define" "invalid")))))

(epsilon.test:deftest test-dest-parameter ()
  "Test dest parameter for storing under different name"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--input-file" 
                  :dest "input"
                  :help "Input file path")
    (add-argument parser "-o"
                  :dest "output"
                  :help "Output file")
    
    ;; Test dest mapping
    (let ((result (parse-args parser '("--input-file" "in.txt" "-o" "out.txt"))))
      (epsilon.test:is-equal "in.txt" (map:get (parsed-options result) "input"))
      (epsilon.test:is-equal "out.txt" (map:get (parsed-options result) "output"))
      ;; Original names should not be present
      (epsilon.test:is-equal nil (map:get (parsed-options result) "input_file"))
      (epsilon.test:is-equal nil (map:get (parsed-options result) "o")))))

(epsilon.test:deftest test-metavar-in-help ()
  "Test metavar parameter in help output"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--config"
                  :metavar "FILE"
                  :help "Configuration file")
    (add-argument parser "--level"
                  :metavar "N"
                  :type 'integer
                  :help "Verbosity level")
    
    ;; Test help output contains metavar
    (let ((output (with-output-to-string (s)
                    (print-help parser s))))
      (epsilon.test:is (search "--config FILE" output))
      (epsilon.test:is (search "--level N" output)))))

(epsilon.test:deftest test-nested-subcommands ()
  "Test multiple levels of subcommands"
  (let ((parser (make-parser :prog "app")))
    ;; Add top-level db command
    (let ((db-parser (add-command parser "db" :help "Database operations")))
      ;; Add db subcommands
      (let ((migrate-parser (add-command db-parser "migrate" :help "Run migrations")))
        (add-argument migrate-parser "--dry-run" :action 'store-true))
      (let ((seed-parser (add-command db-parser "seed" :help "Seed database")))
        (add-argument seed-parser "--file" :help "Seed file")))
    
    ;; Test parsing nested command
    (let ((result (parse-args parser '("db" "migrate" "--dry-run"))))
      (epsilon.test:is-equal "db" (parsed-command result))
      ;; Note: Current implementation only tracks one level of command
      ;; This is a limitation that could be enhanced
      (epsilon.test:is-equal t (map:get (parsed-options result) "dry_run")))))

(epsilon.test:deftest test-edge-cases ()
  "Test various edge cases"
  (let ((parser (make-parser :prog "test")))
    (add-argument parser "--flag" :action 'store-true)
    (add-argument parser "--value")
    
    ;; Empty args
    (let ((result (parse-args parser '())))
      (epsilon.test:is-equal nil (parsed-command result))
      (epsilon.test:is-equal 0 (map:count (parsed-options result))))
    
    ;; Only positional args
    (let ((result (parse-args parser '("foo" "bar" "baz"))))
      (epsilon.test:is-equal '("foo" "bar" "baz") (parsed-positionals result)))
    
    ;; Mixed ordering
    (let ((result (parse-args parser '("foo" "--flag" "bar" "--value" "x" "baz"))))
      (epsilon.test:is-equal t (map:get (parsed-options result) "flag"))
      (epsilon.test:is-equal "x" (map:get (parsed-options result) "value"))
      (epsilon.test:is-equal '("foo" "bar" "baz") (parsed-positionals result)))))

