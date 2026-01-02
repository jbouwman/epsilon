;;; epsilon-imenu.el --- Imenu backend for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides Imenu integration for Epsilon buffers.  It creates
;; a hierarchical outline of definitions in the current buffer.

;;; Code:

(require 'imenu)
(require 'epsilon-client)

;;; Customization

(defgroup epsilon-imenu nil
  "Epsilon Imenu settings."
  :group 'epsilon
  :prefix "epsilon-imenu-")

(defcustom epsilon-imenu-categories
  '(("Functions" . ("function" "defun" "defmethod" "defgeneric"))
    ("Macros" . ("macro" "defmacro"))
    ("Variables" . ("variable" "defvar" "defparameter" "defconstant"))
    ("Types" . ("class" "defclass" "defstruct" "deftype" "type"))
    ("Protocols" . ("protocol" "defprotocol"))
    ("Records" . ("record" "defrecord"))
    ("Constants" . ("constant")))
  "Category mapping for Imenu."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'epsilon-imenu)

(defcustom epsilon-imenu-use-server t
  "Use ELS server for Imenu index when available."
  :type 'boolean
  :group 'epsilon-imenu)

;;; Setup

(defun epsilon-imenu-setup ()
  "Set up Imenu for Epsilon buffers."
  (setq-local imenu-create-index-function #'epsilon-imenu-create-index)
  (setq-local imenu-auto-rescan t))

(defun epsilon-imenu-teardown ()
  "Tear down Imenu for Epsilon buffers."
  (setq-local imenu-create-index-function #'imenu-default-create-index-function))

;;; Index Creation

(defun epsilon-imenu-create-index ()
  "Create Imenu index for current buffer."
  (if (and epsilon-imenu-use-server (epsilon-connected-p))
      (epsilon-imenu--create-index-via-server)
    (epsilon-imenu--create-index-local)))

(defun epsilon-imenu--create-index-via-server ()
  "Create Imenu index via ELS server."
  (condition-case nil
      (let ((response (epsilon-request-sync
                       "outline" "document-symbols"
                       `((file . ,(buffer-file-name))
                         (contents . ,(buffer-substring-no-properties
                                       (point-min) (point-max)))))))
        (if (equal "ok" (alist-get 'status response))
            (epsilon-imenu--convert-symbols (alist-get 'symbols response))
          (epsilon-imenu--create-index-local)))
    (error (epsilon-imenu--create-index-local))))

(defun epsilon-imenu--create-index-local ()
  "Create Imenu index by parsing buffer locally."
  (let ((index '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward epsilon-imenu--def-regexp nil t)
        (let* ((kind (match-string 1))
               (name (match-string 2))
               (pos (match-beginning 0))
               (category (epsilon-imenu--get-category kind)))
          (when (and name category)
            (push (cons name pos)
                  (alist-get category index nil nil #'equal))))))
    ;; Reverse entries within each category
    (dolist (cat index)
      (setcdr cat (nreverse (cdr cat))))
    ;; Flatten single-category lists
    (if (= (length index) 1)
        (cdar index)
      index)))

(defvar epsilon-imenu--def-regexp
  (rx (group (or "defun" "defmacro" "defgeneric" "defmethod"
                 "defvar" "defparameter" "defconstant"
                 "defclass" "defstruct" "deftype"
                 "defprotocol" "defrecord"))
      (+ (any space))
      (group (+ (or word (syntax symbol)))))
  "Regexp for matching Epsilon definitions.")

;;; Symbol Conversion

(defun epsilon-imenu--convert-symbols (symbols)
  "Convert ELS SYMBOLS to Imenu format."
  (let ((index '()))
    (dolist (sym symbols)
      (let* ((name (alist-get 'name sym))
             (kind (alist-get 'kind sym))
             (line (alist-get 'line sym))
             (pos (epsilon-imenu--line-to-pos line))
             (category (epsilon-imenu--get-category kind)))
        (when (and name category)
          (push (cons name pos)
                (alist-get category index nil nil #'equal)))))
    ;; Reverse entries within each category
    (dolist (cat index)
      (setcdr cat (nreverse (cdr cat))))
    ;; Flatten single-category lists
    (if (= (length index) 1)
        (cdar index)
      (nreverse index))))

(defun epsilon-imenu--get-category (kind)
  "Get Imenu category for symbol KIND."
  (cl-loop for (category . kinds) in epsilon-imenu-categories
           when (member kind kinds)
           return category
           finally return "Other"))

(defun epsilon-imenu--line-to-pos (line)
  "Convert LINE number to buffer position."
  (when line
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (point))))

;;; Navigation

(defun epsilon-imenu-goto ()
  "Jump to a definition using Imenu with completion."
  (interactive)
  (let ((index (epsilon-imenu-create-index)))
    (if index
        (let* ((flat-index (epsilon-imenu--flatten-index index))
               (name (completing-read "Go to: " flat-index nil t))
               (pos (cdr (assoc name flat-index))))
          (when pos
            (push-mark)
            (goto-char pos)))
      (message "No definitions found"))))

(defun epsilon-imenu--flatten-index (index)
  "Flatten nested INDEX into flat alist."
  (if (and (consp (car index))
           (integerp (cdar index)))
      ;; Already flat
      index
    ;; Nested
    (apply #'append
           (mapcar (lambda (cat)
                     (if (integerp (cdr cat))
                         (list cat)
                       (mapcar (lambda (item)
                                 (cons (format "%s: %s"
                                               (car cat)
                                               (car item))
                                       (cdr item)))
                               (cdr cat))))
                   index))))

(provide 'epsilon-imenu)

;;; epsilon-imenu.el ends here
