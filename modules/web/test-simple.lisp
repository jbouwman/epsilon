#!/usr/bin/env -S sbcl --script

(load "scripts/epsilon.lisp")
(require 'epsilon.core)
(require 'epsilon.parsing)
(require 'epsilon.json)
(require 'epsilon.http)
(require 'epsilon.web)

(format t "Testing epsilon.web package...~%")

;; Test JSON response
(let ((resp (epsilon.web:json (epsilon.map:make-map "status" "ok"))))
  (format t "JSON response created: ~A~%" 
          (epsilon.http.response:response-status resp)))

;; Test HTML response  
(let ((resp (epsilon.web:html "<h1>Test</h1>")))
  (format t "HTML response created: ~A~%"
          (epsilon.http.response:response-body resp)))

;; Test query parsing
(let ((params (epsilon.web:parse-query-string "foo=bar&baz=qux")))
  (format t "Query params parsed: foo=~A~%"
          (epsilon.map:get params "foo")))

(format t "Tests completed!~%")