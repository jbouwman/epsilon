;;;; Enhanced Module System Example
;;;;
;;;; This file demonstrates the new Module System 2.0 syntax and features.

;; Load the Epsilon system
(load "/home/jbouwman/git/epsilon/scripts/epsilon.lisp")

;; Example 1: Basic module with clean syntax
(epsilon.lib.module:module my-app.config
  :use (cl epsilon.lib.syntax)
  :import ((map epsilon.lib.map)
           (json epsilon.lib.json))
  :export (load-config get-setting))

(defvar *config* nil "Global configuration map")

(defun load-config (file)
  "Load configuration from JSON file"
  (setf *config* 
        (json:decode-file file)))

(defun get-setting (key &optional default)
  "Get a configuration setting"
  (map:get *config* key default))

;; Example 2: Module with selective imports
(epsilon.lib.module:module my-app.utilities
  :use (cl)
  :import (((make-map get assoc) from epsilon.lib.map)
           ((split join trim) from epsilon.lib.string))
  :export (process-text normalize-data))

(defun process-text (text)
  "Process text using imported string functions"
  (-> text
      trim
      (split #\Space)
      (join ", ")))

(defun normalize-data (data)
  "Normalize data into a standard map format"
  (make-map "processed" t
            "data" data
            "timestamp" (get-universal-time)))

;; Example 3: Application main module
(epsilon.lib.module:module my-app.main
  :use (cl)
  :import ((config my-app.config)
           (utils my-app.utilities)
           (log epsilon.lib.log))
  :export (start-application))

(defun start-application (&optional (config-file "config.json"))
  "Start the application with the given configuration"
  (log:info "Starting application...")
  
  ;; Load configuration
  (config:load-config config-file)
  (log:info "Configuration loaded from ~A" config-file)
  
  ;; Process some sample data
  (let ((sample-text "  hello world from epsilon  "))
    (log:info "Original: ~S" sample-text)
    (log:info "Processed: ~S" (utils:process-text sample-text))
    (log:info "Normalized: ~S" (utils:normalize-data sample-text)))
  
  (log:info "Application started successfully"))

;; Demonstrate the functionality
(format t "~&=== Enhanced Module System Demo ===~%")
(format t "~&Creating sample config file...~%")

;; Create a sample config file
(with-open-file (stream "config.json" 
                        :direction :output 
                        :if-exists :supersede)
  (format stream "{\"app_name\": \"Epsilon Demo\", \"version\": \"1.0.0\", \"debug\": true}"))

;; Run the application
(start-application)

;; Show the configuration
(format t "~&~%Configuration loaded:~%")
(format t "App Name: ~A~%" (get-setting "app_name"))
(format t "Version: ~A~%" (get-setting "version"))
(format t "Debug Mode: ~A~%" (get-setting "debug"))

;; Clean up
(delete-file "config.json")

(format t "~&~%=== Demo Complete ===~%")