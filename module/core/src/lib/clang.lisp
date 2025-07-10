;;;; C Language Parser Shim for Core Module
;;;;
;;;; This file provides backward compatibility for clang support in core
;;;; by delegating to the separate foreign module when available.

(defpackage epsilon.lib.clang
  (:use cl)
  (:shadow #:keyword #:symbol)
  (:export #:tokenize
           #:parse))

(in-package epsilon.lib.clang)

(defun clang-available-p ()
  "Check if the clang module is loaded and available."
  (find-package :epsilon.lib.clang.impl))

(defun ensure-clang-loaded ()
  "Ensure the clang module is loaded, error if not available."
  (unless (clang-available-p)
    (error "C language parsing support requires the epsilon.foreign module to be loaded")))

(defun tokenize (input)
  "Tokenize C language input."
  (ensure-clang-loaded)
  (funcall (find-symbol "TOKENIZE" :epsilon.lib.clang.impl) input))

(defun parse (input)
  "Parse C language input into AST."
  (ensure-clang-loaded)
  (funcall (find-symbol "PARSE" :epsilon.lib.clang.impl) input))

;;; Export a feature to indicate clang shim is loaded
(pushnew :epsilon-clang-shim *features*)