;;;; YAML Shim for Core Module
;;;;
;;;; This file provides backward compatibility for YAML support in core
;;;; by delegating to the separate yaml module when available.

(defpackage :epsilon.lib.yaml
  (:use :cl)
  (:export #:parse-string
           #:parse-file
           #:parse
           #:node-value))

(in-package :epsilon.lib.yaml)

(defun yaml-available-p ()
  "Check if the yaml module is loaded and available."
  (find-package :epsilon.lib.yaml.impl))

(defun ensure-yaml-loaded ()
  "Ensure the yaml module is loaded, error if not available."
  (unless (yaml-available-p)
    (error "YAML support requires the epsilon.yaml module to be loaded")))

(defun parse-string (string)
  "Parse a YAML string into Lisp data structures."
  (ensure-yaml-loaded)
  (funcall (find-symbol "PARSE-STRING" :epsilon.lib.yaml.impl) string))

(defun parse-file (pathname)
  "Parse a YAML file into Lisp data structures."
  (ensure-yaml-loaded)
  (funcall (find-symbol "PARSE-FILE" :epsilon.lib.yaml.impl) pathname))

(defun parse (input)
  "Parse YAML from a string or stream into Lisp data structures."
  (ensure-yaml-loaded)
  (funcall (find-symbol "PARSE" :epsilon.lib.yaml.impl) input))

(defun node-value (node)
  "Extract the value from a YAML node."
  (ensure-yaml-loaded)
  (funcall (find-symbol "NODE-VALUE" :epsilon.lib.yaml.impl) node))

;;; Export a feature to indicate yaml shim is loaded
(pushnew :epsilon-yaml-shim *features*)