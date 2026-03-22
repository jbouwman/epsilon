;;;; Tests for Argument Parsing Library

(defpackage epsilon.argparse-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.commands argparse)
            (epsilon.map map))
  (:enter t))

(deftest test-basic-parser-creation ()
  "Test creating a basic parser"
  (let ((parser (argparse:make-parser :command "myapp"
                            :description "A test application"
                            :epilog "See docs for more info")))
    (assert-equal "myapp" (argparse:parser-command parser))
    (assert-equal "A test application" (argparse:parser-description parser))
    (assert-equal "See docs for more info" (argparse:parser-epilog parser))))

(deftest test-add-arguments ()
  "Test adding various types of arguments"
  (let ((parser (argparse:make-parser :command "test")))
    ;; Add option argument
    (let ((arg (argparse:add-argument parser "--verbose"
                           :help "Enable verbose output"
                           :action 'store-true)))
      (assert-equal "--verbose" (argparse:argument-name arg))
      (assert-equal "Enable verbose output" (argparse:argument-help arg))
      (assert-equal 'store-true (argparse:argument-action arg)))

    ;; Add positional argument
    (let ((arg (argparse:add-argument parser "filename"
                           :help "Input file"
                           :type 'string
                           :required t)))
      (assert-equal "filename" (argparse:argument-name arg))
      (assert-equal t (argparse:argument-required arg)))

    ;; Add argument with choices
    (let ((arg (argparse:add-argument parser "--format"
                           :help "Output format"
                           :choices '("json" "xml" "yaml")
                           :default "json")))
      (assert-equal '("json" "xml" "yaml") (argparse:argument-choices arg))
      (assert-equal "json" (argparse:argument-default arg)))))

(deftest test-parse-simple-args ()
  "Test parsing simple arguments"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--verbose" :action 'store-true)
    (argparse:add-argument parser "--output" :help "Output file")
    (argparse:add-argument parser "input" :help "Input file")

    ;; Test basic parsing
    (let ((result (argparse:parse-args parser '("--verbose" "--output" "out.txt" "in.txt"))))
      (assert-equal t (map:get (argparse:parsed-options result) "verbose"))
      (assert-equal "out.txt" (map:get (argparse:parsed-options result) "output"))
      (assert-equal '("in.txt") (argparse:parsed-positionals result)))

    ;; Test with equals syntax
    (let ((result (argparse:parse-args parser '("--output=out.txt" "in.txt"))))
      (assert-equal "out.txt" (map:get (argparse:parsed-options result) "output"))
      (assert-equal '("in.txt") (argparse:parsed-positionals result)))

    ;; Test defaults
    (let ((result (argparse:parse-args parser '("in.txt"))))
      (assert-equal nil (map:get (argparse:parsed-options result) "verbose"))
      (assert-equal nil (map:get (argparse:parsed-options result) "output")))))

(deftest test-type-conversion ()
  "Test type conversion for arguments"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--count" :type 'integer :default 1)
    (argparse:add-argument parser "--enabled" :type 'boolean)
    (argparse:add-argument parser "port" :type 'integer)

    ;; Test integer conversion
    (let ((result (argparse:parse-args parser '("--count" "42" "8080"))))
      (assert-equal 42 (map:get (argparse:parsed-options result) "count"))
      (assert-equal '("8080") (argparse:parsed-positionals result)))

    ;; Test boolean conversion
    (let ((result (argparse:parse-args parser '("--enabled" "true" "8080"))))
      (assert-equal t (map:get (argparse:parsed-options result) "enabled")))

    (let ((result (argparse:parse-args parser '("--enabled" "false" "8080"))))
      (assert-equal nil (map:get (argparse:parsed-options result) "enabled")))

    ;; Test default values
    (let ((result (argparse:parse-args parser '("8080"))))
      (assert-equal 1 (map:get (argparse:parsed-options result) "count")))))

(deftest test-subcommands ()
  "Test parsing with subcommands"
  (let ((parser (argparse:make-parser :command "git")))
    (argparse:add-argument parser "--verbose" :action 'store-true)

    ;; Add commit subcommand
    (let ((commit-parser (argparse:add-command parser "commit" :help "Record changes")))
      (argparse:add-argument commit-parser "-m" :help "Commit message" :required t)
      (argparse:add-argument commit-parser "--amend" :action 'store-true))

    ;; Add push subcommand
    (let ((push-parser (argparse:add-command parser "push" :help "Update remote refs")))
      (argparse:add-argument push-parser "--force" :action 'store-true)
      (argparse:add-argument push-parser "remote" :help "Remote name")
      (argparse:add-argument push-parser "branch" :help "Branch name"))

    ;; Test commit command
    (let ((result (argparse:parse-args parser '("--verbose" "commit" "-m" "Initial commit"))))
      (assert-equal "commit" (argparse:parsed-command result))
      (assert-equal t (map:get (argparse:parsed-options result) "verbose"))
      (let ((sub (argparse:parsed-subresult result)))
        (assert-equal "Initial commit" (map:get (argparse:parsed-options sub) "m"))))

    ;; Test push command
    (let ((result (argparse:parse-args parser '("push" "--force" "origin" "main"))))
      (assert-equal "push" (argparse:parsed-command result))
      (let ((sub (argparse:parsed-subresult result)))
        (assert-equal t (map:get (argparse:parsed-options sub) "force"))
        (assert-equal '("origin" "main") (argparse:parsed-positionals sub))))

    ;; Test global options after command
    (let ((result (argparse:parse-args parser '("commit" "-m" "test" "--amend"))))
      (assert-equal "commit" (argparse:parsed-command result))
      (let ((sub (argparse:parsed-subresult result)))
        (assert-equal t (map:get (argparse:parsed-options sub) "amend"))))))

(deftest test-argument-validation ()
  "Test argument validation and error handling"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--format"
                  :choices '("json" "xml" "yaml")
                  :required t)
    (argparse:add-argument parser "file" :required t)

    ;; Test missing required argument
    (assert-condition (argparse:missing-argument-error)
      (argparse:parse-args parser '()))

    ;; Test invalid choice
    (assert-condition (argparse:invalid-choice-error)
      (argparse:parse-args parser '("--format" "pdf" "file.txt")))

    ;; Test type conversion error
    (argparse:add-argument parser "--port" :type 'integer)
    (assert-condition (argparse:type-conversion-error)
      (argparse:parse-args parser '("--format" "json" "--port" "abc" "file.txt")))))

(deftest test-append-action ()
  "Test append action for collecting multiple values"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--include" :action 'append)
    (argparse:add-argument parser "--define" :action 'append)

    ;; Test multiple values
    (let ((result (argparse:parse-args parser '("--include" "foo" "--include" "bar"
                                      "--define" "X=1" "--define" "Y=2"))))
      (assert-equal '("foo" "bar") (map:get (argparse:parsed-options result) "include"))
      (assert-equal '("X=1" "Y=2") (map:get (argparse:parsed-options result) "define")))

    ;; Test with equals syntax
    (let ((result (argparse:parse-args parser '("--include=foo" "--include=bar"))))
      (assert-equal '("foo" "bar") (map:get (argparse:parsed-options result) "include")))))

(deftest test-store-true-false ()
  "Test store-true and store-false actions"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--verbose" :action 'store-true)
    (argparse:add-argument parser "--quiet" :action 'store-false :default t)
    (argparse:add-argument parser "--debug" :action 'store-true :default nil)

    ;; Test store-true
    (let ((result (argparse:parse-args parser '("--verbose"))))
      (assert-equal t (map:get (argparse:parsed-options result) "verbose"))
      (assert-equal t (map:get (argparse:parsed-options result) "quiet"))  ; default
      (assert-equal nil (map:get (argparse:parsed-options result) "debug"))) ; default

    ;; Test store-false
    (let ((result (argparse:parse-args parser '("--quiet"))))
      (assert-equal nil (map:get (argparse:parsed-options result) "verbose"))
      (assert-equal nil (map:get (argparse:parsed-options result) "quiet"))
      (assert-equal nil (map:get (argparse:parsed-options result) "debug")))))


(deftest test-complex-parsing-scenario ()
  "Test complex real-world parsing scenario"
  (let ((parser (argparse:make-parser :command "epsilon")))
    ;; Global options
    (argparse:add-argument parser "--log" :help "Configure logging")
    (argparse:add-argument parser "--verbose" :action 'store-true)
    (argparse:add-argument parser "--quiet" :action 'store-true)

    ;; Test command
    (let ((test-parser (argparse:add-command parser "test" :help "Run tests")))
      (argparse:add-argument test-parser "--module" :help "Modules to test")
      (argparse:add-argument test-parser "--format"
                    :choices '("detailed" "brief" "junit")
                    :default "detailed")
      (argparse:add-argument test-parser "--test" :help "Test pattern"))

    ;; Build command
    (let ((build-parser (argparse:add-command parser "build" :help "Build modules")))
      (argparse:add-argument build-parser "--force" :action 'store-true)
      (argparse:add-argument build-parser "modules" :help "Modules to build"))

    ;; Test complex command line
    (let ((result (argparse:parse-args parser
                             '("--log" "debug:epsilon.*" "--verbose" "test"
                               "--module" "epsilon" "--test" "parse-*"))))
      (assert-equal "test" (argparse:parsed-command result))
      (assert-equal "debug:epsilon.*" (map:get (argparse:parsed-options result) "log"))
      (assert-equal t (map:get (argparse:parsed-options result) "verbose"))
      (let ((sub (argparse:parsed-subresult result)))
        (assert-equal "epsilon" (map:get (argparse:parsed-options sub) "module"))
        (assert-equal "parse-*" (map:get (argparse:parsed-options sub) "test"))
        (assert-equal "detailed" (map:get (argparse:parsed-options sub) "format"))))))

(deftest test-remaining-args ()
  "Test handling of remaining/unknown arguments"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--known" :help "A known option")
    (argparse:add-argument parser "file" :help "Input file")

    ;; Test that unknown options throw an error
    (assert-condition (argparse:unknown-argument-error)
      (argparse:parse-args parser '("--known" "value" "--unknown" "arg" "input.txt")))

    ;; Test with -- separator allows remaining args
    ;; TODO: Fix argparse to properly handle -- separator
    #+(or)
    (let ((result (argparse:parse-args parser '("--known" "value" "input.txt" "--"
                                      "--not-an-option" "file.txt"))))
      (assert-equal "value" (map:get (argparse:parsed-options result) "known"))
      (assert-equal '("input.txt") (argparse:parsed-positionals result))
      (assert-equal '("--not-an-option" "file.txt") (argparse:parsed-remaining result)))

    ;; Test that extra positional args are kept in remaining
    (let ((result (argparse:parse-args parser '("--known" "value" "input.txt" "extra1" "extra2"))))
      (assert-equal "value" (map:get (argparse:parsed-options result) "known"))
      (assert-equal '("input.txt") (argparse:parsed-positionals result))
      (assert-equal '("extra1" "extra2") (argparse:parsed-remaining result)))))

(deftest test-custom-type-function ()
  "Test custom type conversion function"
  (let ((parser (argparse:make-parser :command "test")))
    ;; Add custom type that parses key=value pairs
    (flet ((parse-pair (s)
             (let ((pos (position #\= s)))
               (if pos
                   (cons (subseq s 0 pos) (subseq s (1+ pos)))
                   (error "Invalid pair format: ~A" s)))))
      (argparse:add-argument parser "--define"
                    :type #'parse-pair
                    :action 'append
                    :help "Define key=value pair"))

    ;; Test custom type
    (let ((result (argparse:parse-args parser '("--define" "FOO=bar" "--define" "X=42"))))
      (assert-equal '(("FOO" . "bar") ("X" . "42"))
                (map:get (argparse:parsed-options result) "define")))

    ;; Test error handling
    (assert-condition (argparse:type-conversion-error)
      (argparse:parse-args parser '("--define" "invalid")))))

(deftest test-dest-parameter ()
  "Test dest parameter for storing under different name"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--input-file"
                  :dest "input"
                  :help "Input file path")
    (argparse:add-argument parser "-o"
                  :dest "output"
                  :help "Output file")

    ;; Test dest mapping
    (let ((result (argparse:parse-args parser '("--input-file" "in.txt" "-o" "out.txt"))))
      (assert-equal "in.txt" (map:get (argparse:parsed-options result) "input"))
      (assert-equal "out.txt" (map:get (argparse:parsed-options result) "output"))
      ;; Original names should not be present
      (assert-equal nil (map:get (argparse:parsed-options result) "input_file"))
      (assert-equal nil (map:get (argparse:parsed-options result) "o")))))

(deftest test-metavar-in-help ()
  "Test metavar parameter in help output"
  (let ((parser (argparse:make-parser :command "test")))
    (argparse:add-argument parser "--config"
                  :metavar "FILE"
                  :help "Configuration file")
    (argparse:add-argument parser "--level"
                  :metavar "N"
                  :type 'integer
                  :help "Verbosity level")

    ;; Test help output contains metavar
    (let ((output (with-output-to-string (s)
                    (argparse:print-help parser s))))
      (assert-true (search "--config FILE" output))
      (assert-true (search "--level N" output)))))

(deftest test-nested-subcommands ()
  "Test multiple levels of subcommands"
  (let ((parser (argparse:make-parser :command "app")))
    ;; Add top-level db command
    (let ((db-parser (argparse:add-command parser "db" :help "Database operations")))
      ;; Add db subcommands
      (let ((migrate-parser (argparse:add-command db-parser "migrate" :help "Run migrations")))
        (argparse:add-argument migrate-parser "--dry-run" :action 'store-true))
      (let ((seed-parser (argparse:add-command db-parser "seed" :help "Seed database")))
        (argparse:add-argument seed-parser "--file" :help "Seed file")))

    ;; Test parsing nested command
    (let ((result (argparse:parse-args parser '("db" "migrate" "--dry-run"))))
      (assert-equal "db" (argparse:parsed-command result))
      (let ((sub (argparse:parsed-subresult result)))
        (assert-true sub)))))
