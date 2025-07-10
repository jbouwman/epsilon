;;;; JSON Shim for Core Module
;;;;
;;;; This file provides backward compatibility for JSON support in core
;;;; by delegating to the separate parsing module when available.

(defpackage :epsilon.lib.json
  (:use :cl)
  (:export #:tokenize
           #:parse))

(in-package :epsilon.lib.json)

(defun json-available-p ()
  "Check if the json module is loaded and available."
  (find-package :epsilon.lib.json.impl))

(defun ensure-json-loaded ()
  "Ensure the json module is loaded, error if not available."
  (unless (json-available-p)
    (error "JSON support requires the epsilon.parsing module to be loaded")))

(defun tokenize (input)
  "Tokenize JSON input."
  (ensure-json-loaded)
  (funcall (find-symbol "TOKENIZE" :epsilon.lib.json.impl) input))

(defun parse (input)
  "Parse JSON input into Lisp data structures."
  (ensure-json-loaded)
  (funcall (find-symbol "PARSE" :epsilon.lib.json.impl) input))

;;; Export a feature to indicate json shim is loaded
(pushnew :epsilon-json-shim *features*)