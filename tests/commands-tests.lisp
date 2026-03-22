;;;; Tests for Declarative Command Argument Parsing

(defpackage epsilon.commands-test
  (:use :cl :epsilon.test)
  (:local-nicknames
   (commands epsilon.commands))
  (:enter t))

(deftest test-parse-declared-args-integer ()
  "Test parsing an integer flag"
  (let ((specs '((:name "port" :flag "--port" :type :integer :default 8080
                  :description "Port"))))
    (let ((result (commands:parse-declared-args specs '("--port" "7681"))))
      (assert-equal '(:port 7681) result))))

(deftest test-parse-declared-args-boolean ()
  "Test parsing a boolean flag"
  (let ((specs '((:name "debug" :flag "--debug" :type :boolean
                  :description "Debug mode"))))
    ;; Flag present
    (let ((result (commands:parse-declared-args specs '("--debug"))))
      (assert-equal '(:debug t) result))
    ;; Flag absent
    (let ((result (commands:parse-declared-args specs '())))
      (assert-equal '(:debug nil) result))))

(deftest test-parse-declared-args-string ()
  "Test parsing a string flag"
  (let ((specs '((:name "host" :flag "--host" :type :string :default "localhost"
                  :description "Hostname"))))
    (let ((result (commands:parse-declared-args specs '("--host" "0.0.0.0"))))
      (assert-equal '(:host "0.0.0.0") result))
    ;; Default when absent
    (let ((result (commands:parse-declared-args specs '())))
      (assert-equal '(:host "localhost") result))))

(deftest test-parse-declared-args-defaults ()
  "Test that defaults are applied when flags are omitted"
  (let ((specs '((:name "port" :flag "--port" :type :integer :default 8080
                  :description "Port")
                 (:name "debug" :flag "--debug" :type :boolean
                  :description "Debug"))))
    (let ((result (commands:parse-declared-args specs '())))
      (assert-equal '(:port 8080 :debug nil) result))))

(deftest test-parse-declared-args-multiple ()
  "Test parsing multiple flags together"
  (let ((specs '((:name "port" :flag "--port" :type :integer
                  :description "Port")
                 (:name "debug" :flag "--debug" :type :boolean
                  :description "Debug")
                 (:name "host" :flag "--host" :type :string :default "localhost"
                  :description "Hostname"))))
    (let ((result (commands:parse-declared-args specs
                    '("--port" "7681" "--debug" "--host" "0.0.0.0"))))
      (assert-equal '(:port 7681 :debug t :host "0.0.0.0") result))))

(deftest test-parse-declared-args-help ()
  "Test that --help returns :help sentinel"
  (let ((specs '((:name "port" :flag "--port" :type :integer :default 8080
                  :description "Port"))))
    (let ((result (commands:parse-declared-args specs '("--help"))))
      (assert-eq :help result))
    ;; --help with other flags
    (let ((result (commands:parse-declared-args specs '("--port" "9000" "--help"))))
      (assert-eq :help result))))

(deftest test-parse-declared-args-ignores-non-flags ()
  "Test that non-flag arguments (like module names) are ignored"
  (let ((specs '((:name "port" :flag "--port" :type :integer :default 8080
                  :description "Port"))))
    (let ((result (commands:parse-declared-args specs '("epsilon.lsp" "--port" "7681"))))
      (assert-equal '(:port 7681) result))))

(deftest test-parse-declared-args-nil-default ()
  "Test that missing optional args get nil default"
  (let ((specs '((:name "port" :flag "--port" :type :integer
                  :description "Port"))))
    (let ((result (commands:parse-declared-args specs '())))
      (assert-equal '(:port nil) result))))

(deftest test-command-struct-args-slot ()
  "Test that the command struct has an args slot"
  (let ((cmd (commands::make-command
              :name "test-cmd"
              :description "A test"
              :args '((:name "port" :flag "--port" :type :integer)))))
    (assert-equal "test-cmd" (commands:command-name cmd))
    (assert-not-null (commands:command-args cmd))
    (assert-equal "--port"
                  (getf (first (commands:command-args cmd)) :flag))))
