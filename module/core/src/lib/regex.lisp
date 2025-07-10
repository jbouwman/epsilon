;;;; Regex Shim for Core Module
;;;;
;;;; This file provides backward compatibility for regex support in core
;;;; by delegating to the separate regex module when available.

(defpackage :epsilon.lib.regex
  (:use :cl)
  (:shadow #:compile #:search)
  (:export #:scan
           #:scan-to-strings
           #:search
           #:match
           #:findall
           #:finditer
           #:sub
           #:subn
           #:compile))

(in-package :epsilon.lib.regex)

(defun regex-available-p ()
  "Check if the regex module is loaded and available."
  (find-package :epsilon.lib.regex.impl))

(defun ensure-regex-loaded ()
  "Ensure the regex module is loaded, error if not available."
  (unless (regex-available-p)
    (error "Regex support requires the epsilon.regex module to be loaded")))

(defun scan (regex target-string &rest args)
  "Scan for regex matches in target string."
  (ensure-regex-loaded)
  (apply (find-symbol "SCAN" :epsilon.lib.regex.impl) regex target-string args))

(defun scan-to-strings (regex target-string &rest args)
  "Scan for regex matches and return as strings."
  (ensure-regex-loaded)
  (apply (find-symbol "SCAN-TO-STRINGS" :epsilon.lib.regex.impl) regex target-string args))

(defun search (regex target-string &rest args)
  "Search for regex in target string."
  (ensure-regex-loaded)
  (apply (find-symbol "SEARCH" :epsilon.lib.regex.impl) regex target-string args))

(defun match (regex target-string &rest args)
  "Match regex against target string."
  (ensure-regex-loaded)
  (apply (find-symbol "MATCH" :epsilon.lib.regex.impl) regex target-string args))

(defun findall (regex target-string &rest args)
  "Find all matches of regex in target string."
  (ensure-regex-loaded)
  (apply (find-symbol "FINDALL" :epsilon.lib.regex.impl) regex target-string args))

(defun finditer (regex target-string &rest args)
  "Find all matches of regex in target string, returning an iterator."
  (ensure-regex-loaded)
  (apply (find-symbol "FINDITER" :epsilon.lib.regex.impl) regex target-string args))

(defun sub (regex replacement target-string &rest args)
  "Substitute regex matches with replacement."
  (ensure-regex-loaded)
  (apply (find-symbol "SUB" :epsilon.lib.regex.impl) regex replacement target-string args))

(defun subn (regex replacement target-string &rest args)
  "Substitute regex matches with replacement, returning count."
  (ensure-regex-loaded)
  (apply (find-symbol "SUBN" :epsilon.lib.regex.impl) regex replacement target-string args))

(defun compile (regex &rest args)
  "Compile a regex pattern."
  (ensure-regex-loaded)
  (apply (find-symbol "COMPILE" :epsilon.lib.regex.impl) regex args))

;;; Export a feature to indicate regex shim is loaded
(pushnew :epsilon-regex-shim *features*)