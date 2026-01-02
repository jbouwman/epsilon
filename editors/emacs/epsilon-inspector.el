;;; epsilon-inspector.el --- Object inspector for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides an object inspector for Epsilon.  It allows drilling
;; down into data structures, viewing slots, and navigating object graphs.

;;; Code:

(require 'epsilon-client)

;;; Customization

(defgroup epsilon-inspector nil
  "Epsilon inspector settings."
  :group 'epsilon
  :prefix "epsilon-inspector-")

(defcustom epsilon-inspector-max-string-length 200
  "Maximum length for string display in inspector."
  :type 'integer
  :group 'epsilon-inspector)

;;; Faces

(defface epsilon-inspector-title-face
  '((t :inherit bold :height 1.1))
  "Face for inspector title."
  :group 'epsilon-inspector)

(defface epsilon-inspector-label-face
  '((t :inherit font-lock-variable-name-face))
  "Face for slot/field labels."
  :group 'epsilon-inspector)

(defface epsilon-inspector-value-face
  '((t :inherit default))
  "Face for values."
  :group 'epsilon-inspector)

(defface epsilon-inspector-action-face
  '((t :inherit link))
  "Face for clickable actions."
  :group 'epsilon-inspector)

(defface epsilon-inspector-type-face
  '((t :inherit font-lock-type-face))
  "Face for type information."
  :group 'epsilon-inspector)

;;; Mode Definition

(defvar epsilon-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'epsilon-inspector-action)
    (define-key map (kbd "TAB") #'epsilon-inspector-next-action)
    (define-key map (kbd "<backtab>") #'epsilon-inspector-prev-action)
    (define-key map (kbd "l") #'epsilon-inspector-back)
    (define-key map (kbd "r") #'epsilon-inspector-forward)
    (define-key map (kbd "n") #'epsilon-inspector-next)
    (define-key map (kbd "p") #'epsilon-inspector-previous)
    (define-key map (kbd "d") #'epsilon-inspector-describe)
    (define-key map (kbd "e") #'epsilon-inspector-eval)
    (define-key map (kbd "g") #'epsilon-inspector-refresh)
    (define-key map (kbd "q") #'epsilon-inspector-quit)
    (define-key map (kbd "?") #'epsilon-inspector-help)
    map)
  "Keymap for `epsilon-inspector-mode'.")

(define-derived-mode epsilon-inspector-mode special-mode "Epsilon Inspector"
  "Major mode for Epsilon object inspection.

Commands:
\\{epsilon-inspector-mode-map}"
  :group 'epsilon-inspector
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

;;; State Variables

(defvar-local epsilon-inspector--id nil
  "Current inspector ID.")

(defvar-local epsilon-inspector--history nil
  "Inspector navigation history (list of inspector IDs).")

(defvar-local epsilon-inspector--history-position 0
  "Current position in history.")

(defvar-local epsilon-inspector--content nil
  "Current inspector content.")

;;; Commands

(defun epsilon-inspect (expression)
  "Inspect result of evaluating EXPRESSION."
  (interactive
   (list (let ((sym (epsilon--symbol-at-point)))
           (read-string "Inspect: " sym))))
  (epsilon-request
   "inspect" "inspect"
   `((expression . ,expression)
     (package . ,(epsilon--current-package)))
   #'epsilon-inspector--display))

(defun epsilon-inspect-last-result ()
  "Inspect the last evaluation result."
  (interactive)
  (epsilon-request
   "inspect" "inspect-last-result"
   nil
   #'epsilon-inspector--display))

;;; Display

(defun epsilon-inspector--display (response)
  "Display inspector RESPONSE."
  (if (equal "ok" (alist-get 'status response))
      (let ((buffer (get-buffer-create "*epsilon-inspector*")))
        (with-current-buffer buffer
          (epsilon-inspector-mode)
          ;; Update history
          (when epsilon-inspector--id
            (push epsilon-inspector--id epsilon-inspector--history))
          (setq epsilon-inspector--id (alist-get 'inspector-id response))
          (setq epsilon-inspector--content response)
          (epsilon-inspector--render response))
        (pop-to-buffer buffer))
    (message "Inspect failed: %s"
             (alist-get 'message (alist-get 'error response)))))

(defun epsilon-inspector--render (response)
  "Render inspector RESPONSE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Title
    (insert (propertize (or (alist-get 'title response) "Inspector")
                        'face 'epsilon-inspector-title-face))
    (insert "\n")
    ;; Type info
    (when-let ((type (alist-get 'type response)))
      (insert (propertize (format "Type: %s" type)
                          'face 'epsilon-inspector-type-face))
      (insert "\n"))
    (insert "\n")
    ;; Content
    (dolist (part (alist-get 'content response))
      (epsilon-inspector--render-part part))
    (goto-char (point-min))))

(defun epsilon-inspector--render-part (part)
  "Render a single content PART."
  (let ((type (alist-get 'type part)))
    (pcase type
      ("text"
       (insert (alist-get 'value part)))
      ("label"
       (insert (propertize (alist-get 'value part)
                           'face 'epsilon-inspector-label-face)))
      ("value"
       (let ((value (alist-get 'value part)))
         (insert (propertize (epsilon-inspector--truncate-value value)
                             'face 'epsilon-inspector-value-face))))
      ("action"
       (epsilon-inspector--insert-action part))
      ("newline"
       (insert "\n"))
      ("separator"
       (insert (make-string 40 ?-) "\n"))
      ("slot"
       (epsilon-inspector--render-slot part))
      (_
       (insert (format "[unknown: %s]" type))))))

(defun epsilon-inspector--render-slot (slot)
  "Render a SLOT entry."
  (let ((name (alist-get 'name slot))
        (value (alist-get 'value slot))
        (action-index (alist-get 'action-index slot)))
    (insert "  ")
    (insert (propertize name 'face 'epsilon-inspector-label-face))
    (insert ": ")
    (if action-index
        (insert-button
         (epsilon-inspector--truncate-value value)
         'action-index action-index
         'action (lambda (_button) (epsilon-inspector--invoke-action action-index))
         'face 'epsilon-inspector-action-face
         'follow-link t)
      (insert (propertize (epsilon-inspector--truncate-value value)
                          'face 'epsilon-inspector-value-face)))
    (insert "\n")))

(defun epsilon-inspector--insert-action (part)
  "Insert an action button for PART."
  (let ((label (alist-get 'label part))
        (index (alist-get 'index part)))
    (insert-button
     label
     'action-index index
     'action (lambda (_button) (epsilon-inspector--invoke-action index))
     'face 'epsilon-inspector-action-face
     'follow-link t)))

(defun epsilon-inspector--truncate-value (value)
  "Truncate VALUE for display."
  (if (> (length value) epsilon-inspector-max-string-length)
      (concat (substring value 0 (- epsilon-inspector-max-string-length 3)) "...")
    value))

;;; Navigation

(defun epsilon-inspector-action ()
  "Invoke action at point."
  (interactive)
  (when-let ((index (get-text-property (point) 'action-index)))
    (epsilon-inspector--invoke-action index)))

(defun epsilon-inspector--invoke-action (index)
  "Invoke action at INDEX."
  (epsilon-request
   "inspect" "inspect-action"
   `((inspector-id . ,epsilon-inspector--id)
     (action-index . ,index))
   #'epsilon-inspector--display))

(defun epsilon-inspector-next-action ()
  "Move to next action button."
  (interactive)
  (let ((pos (next-single-property-change (point) 'action-index)))
    (when pos
      (goto-char pos)
      (when (not (get-text-property pos 'action-index))
        (let ((next (next-single-property-change pos 'action-index)))
          (when next (goto-char next)))))))

(defun epsilon-inspector-prev-action ()
  "Move to previous action button."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'action-index)))
    (when pos
      (goto-char pos)
      (when (not (get-text-property pos 'action-index))
        (let ((prev (previous-single-property-change pos 'action-index)))
          (when prev (goto-char prev)))))))

(defun epsilon-inspector-back ()
  "Go back in inspector history."
  (interactive)
  (when epsilon-inspector--history
    (let ((prev-id (pop epsilon-inspector--history)))
      (epsilon-request
       "inspect" "inspect-by-id"
       `((inspector-id . ,prev-id))
       (lambda (response)
         (when (equal "ok" (alist-get 'status response))
           (with-current-buffer "*epsilon-inspector*"
             (setq epsilon-inspector--id prev-id)
             (setq epsilon-inspector--content response)
             (epsilon-inspector--render response))))))))

(defun epsilon-inspector-forward ()
  "Go forward in inspector history (not yet implemented)."
  (interactive)
  (message "Forward navigation not yet implemented"))

(defun epsilon-inspector-next ()
  "Inspect next item in sequence (if applicable)."
  (interactive)
  (epsilon-request
   "inspect" "inspect-next"
   `((inspector-id . ,epsilon-inspector--id))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (epsilon-inspector--display response)
       (message "No next item")))))

(defun epsilon-inspector-previous ()
  "Inspect previous item in sequence (if applicable)."
  (interactive)
  (epsilon-request
   "inspect" "inspect-previous"
   `((inspector-id . ,epsilon-inspector--id))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (epsilon-inspector--display response)
       (message "No previous item")))))

;;; Other Commands

(defun epsilon-inspector-describe ()
  "Show full description of inspected object."
  (interactive)
  (epsilon-request
   "inspect" "describe"
   `((inspector-id . ,epsilon-inspector--id))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (with-help-window "*epsilon-describe*"
           (princ (alist-get 'description response)))
       (message "Describe failed")))))

(defun epsilon-inspector-eval (expression)
  "Evaluate EXPRESSION with * bound to inspected object."
  (interactive "sEval (* = inspected object): ")
  (epsilon-request
   "inspect" "eval-with-object"
   `((inspector-id . ,epsilon-inspector--id)
     (expression . ,expression))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "=> %s" (alist-get 'result response))
       (message "Error: %s"
                (alist-get 'message (alist-get 'error response)))))))

(defun epsilon-inspector-refresh ()
  "Refresh the inspector display."
  (interactive)
  (when epsilon-inspector--id
    (epsilon-request
     "inspect" "refresh"
     `((inspector-id . ,epsilon-inspector--id))
     (lambda (response)
       (when (equal "ok" (alist-get 'status response))
         (epsilon-inspector--render response))))))

(defun epsilon-inspector-quit ()
  "Quit the inspector."
  (interactive)
  (when epsilon-inspector--id
    (epsilon-request
     "inspect" "close"
     `((inspector-id . ,epsilon-inspector--id))
     #'ignore))
  (quit-window t))

(defun epsilon-inspector-help ()
  "Show inspector help."
  (interactive)
  (message "RET:action TAB:next l:back n/p:nav d:describe e:eval g:refresh q:quit"))

(provide 'epsilon-inspector)

;;; epsilon-inspector.el ends here
