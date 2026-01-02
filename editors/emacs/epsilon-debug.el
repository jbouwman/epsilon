;;; epsilon-debug.el --- Debugger interface for Epsilon -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module provides a debugger interface for Epsilon.  It handles
;; debug break notifications, displays the debugger buffer with restarts
;; and stack frames, and allows inspection and evaluation in frame context.

;;; Code:

(require 'epsilon-client)

;;; Customization

(defgroup epsilon-debug nil
  "Epsilon debugger settings."
  :group 'epsilon
  :prefix "epsilon-debug-")

(defcustom epsilon-debug-show-source t
  "Automatically show source for current frame."
  :type 'boolean
  :group 'epsilon-debug)

(defcustom epsilon-debug-window-height 20
  "Height of debugger window."
  :type 'integer
  :group 'epsilon-debug)

;;; Faces

(defface epsilon-debug-condition-face
  '((t :inherit error :weight bold))
  "Face for the error condition."
  :group 'epsilon-debug)

(defface epsilon-debug-restart-face
  '((t :inherit font-lock-function-name-face))
  "Face for restart names."
  :group 'epsilon-debug)

(defface epsilon-debug-frame-face
  '((t :inherit default))
  "Face for stack frame labels."
  :group 'epsilon-debug)

(defface epsilon-debug-current-frame-face
  '((t :inherit highlight))
  "Face for the current stack frame."
  :group 'epsilon-debug)

(defface epsilon-debug-local-face
  '((t :inherit font-lock-variable-name-face))
  "Face for local variable names."
  :group 'epsilon-debug)

;;; Mode Definition

(defvar epsilon-debug-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Restarts
    (define-key map (kbd "a") #'epsilon-debug-abort)
    (define-key map (kbd "c") #'epsilon-debug-continue)
    (define-key map (kbd "0") #'epsilon-debug-invoke-restart-0)
    (define-key map (kbd "1") #'epsilon-debug-invoke-restart-1)
    (define-key map (kbd "2") #'epsilon-debug-invoke-restart-2)
    (define-key map (kbd "3") #'epsilon-debug-invoke-restart-3)
    (define-key map (kbd "4") #'epsilon-debug-invoke-restart-4)
    (define-key map (kbd "5") #'epsilon-debug-invoke-restart-5)
    (define-key map (kbd "6") #'epsilon-debug-invoke-restart-6)
    (define-key map (kbd "7") #'epsilon-debug-invoke-restart-7)
    (define-key map (kbd "8") #'epsilon-debug-invoke-restart-8)
    (define-key map (kbd "9") #'epsilon-debug-invoke-restart-9)
    ;; Stepping
    (define-key map (kbd "s") #'epsilon-debug-step)
    (define-key map (kbd "n") #'epsilon-debug-next)
    (define-key map (kbd "o") #'epsilon-debug-step-out)
    ;; Frame navigation
    (define-key map (kbd "p") #'epsilon-debug-prev-frame)
    (define-key map (kbd "TAB") #'epsilon-debug-next-frame)
    (define-key map (kbd "<backtab>") #'epsilon-debug-prev-frame)
    (define-key map (kbd "RET") #'epsilon-debug-goto-source)
    ;; Inspection
    (define-key map (kbd "e") #'epsilon-debug-eval-in-frame)
    (define-key map (kbd "v") #'epsilon-debug-show-locals)
    (define-key map (kbd "i") #'epsilon-debug-inspect-frame)
    ;; Other
    (define-key map (kbd "q") #'epsilon-debug-quit)
    (define-key map (kbd "?") #'epsilon-debug-help)
    map)
  "Keymap for `epsilon-debug-mode'.")

(define-derived-mode epsilon-debug-mode special-mode "Epsilon Debug"
  "Major mode for Epsilon debugger.

Commands:
\\{epsilon-debug-mode-map}"
  :group 'epsilon-debug
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local cursor-type nil))

;;; State Variables

(defvar-local epsilon-debug--level nil
  "Current debugger level.")

(defvar-local epsilon-debug--condition nil
  "The error condition.")

(defvar-local epsilon-debug--restarts nil
  "Available restarts.")

(defvar-local epsilon-debug--frames nil
  "Stack frames.")

(defvar-local epsilon-debug--current-frame 0
  "Currently selected frame index.")

(defvar-local epsilon-debug--locals-visible nil
  "Whether locals are visible for current frame.")

;;; Setup

(defun epsilon-debug-setup ()
  "Set up debugger notification handler."
  (epsilon-register-notification-handler
   "debug" "break"
   #'epsilon-debug--handle-break)
  (epsilon-register-notification-handler
   "debug" "step"
   #'epsilon-debug--handle-step))

;;; Break Handler

(defun epsilon-debug--handle-break (payload)
  "Handle debug break PAYLOAD."
  (let ((buffer (get-buffer-create "*epsilon-debugger*")))
    (with-current-buffer buffer
      (epsilon-debug-mode)
      (setq epsilon-debug--level (alist-get 'level payload))
      (setq epsilon-debug--condition (alist-get 'condition payload))
      (setq epsilon-debug--restarts (alist-get 'restarts payload))
      (setq epsilon-debug--frames (alist-get 'frames payload))
      (setq epsilon-debug--current-frame 0)
      (setq epsilon-debug--locals-visible nil)
      (epsilon-debug--render))
    (epsilon-debug--show-buffer buffer)
    (when epsilon-debug-show-source
      (epsilon-debug--show-frame-source 0))))

(defun epsilon-debug--handle-step (payload)
  "Handle debug step PAYLOAD."
  ;; Update frames after stepping
  (when-let ((buffer (get-buffer "*epsilon-debugger*")))
    (with-current-buffer buffer
      (setq epsilon-debug--frames (alist-get 'frames payload))
      (setq epsilon-debug--current-frame 0)
      (epsilon-debug--render))
    (when epsilon-debug-show-source
      (epsilon-debug--show-frame-source 0))))

(defun epsilon-debug--show-buffer (buffer)
  "Show debugger BUFFER."
  (let ((window (display-buffer-in-side-window
                 buffer
                 `((side . bottom)
                   (window-height . ,epsilon-debug-window-height)))))
    (select-window window)))

;;; Rendering

(defun epsilon-debug--render ()
  "Render debugger buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize "Epsilon Debugger"
                        'face 'bold)
            (format " (level %d)\n\n" (or epsilon-debug--level 1)))
    ;; Condition
    (epsilon-debug--render-condition)
    (insert "\n")
    ;; Restarts
    (epsilon-debug--render-restarts)
    (insert "\n")
    ;; Backtrace
    (epsilon-debug--render-backtrace)
    ;; Help hint
    (insert "\n")
    (insert (propertize "Press ? for help" 'face 'shadow))
    (goto-char (point-min))))

(defun epsilon-debug--render-condition ()
  "Render the error condition."
  (insert (propertize "Condition:\n" 'face 'bold))
  (let ((type (alist-get 'type epsilon-debug--condition))
        (message (alist-get 'message epsilon-debug--condition)))
    (insert "  ")
    (insert (propertize (or type "Error") 'face 'epsilon-debug-condition-face))
    (insert ": ")
    (insert (or message "Unknown error"))
    (insert "\n")))

(defun epsilon-debug--render-restarts ()
  "Render available restarts."
  (insert (propertize "Restarts:\n" 'face 'bold))
  (let ((idx 0))
    (dolist (restart epsilon-debug--restarts)
      (let ((name (alist-get 'name restart))
            (description (alist-get 'description restart)))
        (insert (format "  [%d] " idx))
        (insert (propertize (upcase name) 'face 'epsilon-debug-restart-face))
        (insert " - ")
        (insert (or description ""))
        (insert "\n"))
      (cl-incf idx))))

(defun epsilon-debug--render-backtrace ()
  "Render the backtrace."
  (insert (propertize "Backtrace:\n" 'face 'bold))
  (let ((idx 0))
    (dolist (frame epsilon-debug--frames)
      (let ((label (alist-get 'label frame))
            (is-current (= idx epsilon-debug--current-frame)))
        (insert (if is-current
                    (propertize (format " > %2d: %s\n" idx label)
                                'face 'epsilon-debug-current-frame-face)
                  (format "   %2d: %s\n" idx label)))
        ;; Show locals if visible
        (when (and is-current epsilon-debug--locals-visible)
          (epsilon-debug--render-locals frame)))
      (cl-incf idx))))

(defun epsilon-debug--render-locals (frame)
  "Render local variables for FRAME."
  (when-let ((locals (alist-get 'locals frame)))
    (dolist (local locals)
      (let ((name (alist-get 'name local))
            (value (alist-get 'value local)))
        (insert "       ")
        (insert (propertize name 'face 'epsilon-debug-local-face))
        (insert " = ")
        (insert (or value "?"))
        (insert "\n")))))

;;; Frame Navigation

(defun epsilon-debug-next-frame ()
  "Move to next stack frame."
  (interactive)
  (when (< epsilon-debug--current-frame
           (1- (length epsilon-debug--frames)))
    (cl-incf epsilon-debug--current-frame)
    (epsilon-debug--render)
    (when epsilon-debug-show-source
      (epsilon-debug--show-frame-source epsilon-debug--current-frame))))

(defun epsilon-debug-prev-frame ()
  "Move to previous stack frame."
  (interactive)
  (when (> epsilon-debug--current-frame 0)
    (cl-decf epsilon-debug--current-frame)
    (epsilon-debug--render)
    (when epsilon-debug-show-source
      (epsilon-debug--show-frame-source epsilon-debug--current-frame))))

(defun epsilon-debug-goto-source ()
  "Go to source location of current frame."
  (interactive)
  (epsilon-debug--show-frame-source epsilon-debug--current-frame t))

(defun epsilon-debug--show-frame-source (frame-idx &optional select)
  "Show source for frame at FRAME-IDX.
If SELECT is non-nil, select the source window."
  (when-let ((frame (nth frame-idx epsilon-debug--frames)))
    (let ((file (alist-get 'file frame))
          (line (alist-get 'line frame)))
      (when (and file line (file-exists-p file))
        (let ((buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (goto-char (point-min))
            (forward-line (1- line))
            (pulse-momentary-highlight-one-line (point)))
          (if select
              (pop-to-buffer buffer)
            (display-buffer buffer)))))))

;;; Restart Commands

(defun epsilon-debug-invoke-restart (index)
  "Invoke restart at INDEX."
  (epsilon-request
   "debug" "invoke-restart"
   `((level . ,epsilon-debug--level)
     (restart-index . ,index))
   (lambda (response)
     (when (equal "ok" (alist-get 'status response))
       (epsilon-debug--close)))))

(defun epsilon-debug-abort ()
  "Abort to top level."
  (interactive)
  (epsilon-request
   "debug" "abort"
   `((level . ,epsilon-debug--level))
   (lambda (_) (epsilon-debug--close))))

(defun epsilon-debug-continue ()
  "Continue execution."
  (interactive)
  (epsilon-debug-invoke-restart 0))

(defun epsilon-debug-invoke-restart-0 () (interactive) (epsilon-debug-invoke-restart 0))
(defun epsilon-debug-invoke-restart-1 () (interactive) (epsilon-debug-invoke-restart 1))
(defun epsilon-debug-invoke-restart-2 () (interactive) (epsilon-debug-invoke-restart 2))
(defun epsilon-debug-invoke-restart-3 () (interactive) (epsilon-debug-invoke-restart 3))
(defun epsilon-debug-invoke-restart-4 () (interactive) (epsilon-debug-invoke-restart 4))
(defun epsilon-debug-invoke-restart-5 () (interactive) (epsilon-debug-invoke-restart 5))
(defun epsilon-debug-invoke-restart-6 () (interactive) (epsilon-debug-invoke-restart 6))
(defun epsilon-debug-invoke-restart-7 () (interactive) (epsilon-debug-invoke-restart 7))
(defun epsilon-debug-invoke-restart-8 () (interactive) (epsilon-debug-invoke-restart 8))
(defun epsilon-debug-invoke-restart-9 () (interactive) (epsilon-debug-invoke-restart 9))

;;; Stepping

(defun epsilon-debug-step ()
  "Step into next expression."
  (interactive)
  (epsilon-request
   "debug" "step"
   `((level . ,epsilon-debug--level)
     (mode . "into"))
   #'ignore))

(defun epsilon-debug-next ()
  "Step over next expression."
  (interactive)
  (epsilon-request
   "debug" "step"
   `((level . ,epsilon-debug--level)
     (mode . "over"))
   #'ignore))

(defun epsilon-debug-step-out ()
  "Step out of current function."
  (interactive)
  (epsilon-request
   "debug" "step"
   `((level . ,epsilon-debug--level)
     (mode . "out"))
   #'ignore))

;;; Inspection

(defun epsilon-debug-eval-in-frame (expression)
  "Evaluate EXPRESSION in context of current frame."
  (interactive "sEval in frame: ")
  (epsilon-request
   "debug" "eval-in-frame"
   `((level . ,epsilon-debug--level)
     (frame-index . ,epsilon-debug--current-frame)
     (expression . ,expression))
   (lambda (response)
     (if (equal "ok" (alist-get 'status response))
         (message "=> %s" (alist-get 'result response))
       (message "Error: %s"
                (alist-get 'message (alist-get 'error response)))))))

(defun epsilon-debug-show-locals ()
  "Toggle display of local variables for current frame."
  (interactive)
  (setq epsilon-debug--locals-visible (not epsilon-debug--locals-visible))
  ;; Fetch locals if needed
  (when epsilon-debug--locals-visible
    (epsilon-request
     "debug" "frame-locals"
     `((level . ,epsilon-debug--level)
       (frame-index . ,epsilon-debug--current-frame))
     (lambda (response)
       (when (equal "ok" (alist-get 'status response))
         (let ((frame (nth epsilon-debug--current-frame epsilon-debug--frames)))
           (setf (alist-get 'locals frame) (alist-get 'locals response)))
         (epsilon-debug--render)))))
  (epsilon-debug--render))

(defun epsilon-debug-inspect-frame ()
  "Inspect current frame in object inspector."
  (interactive)
  (epsilon-request
   "debug" "inspect-frame"
   `((level . ,epsilon-debug--level)
     (frame-index . ,epsilon-debug--current-frame))
   (lambda (response)
     (when (equal "ok" (alist-get 'status response))
       (epsilon-inspector--display response)))))

;;; Other Commands

(defun epsilon-debug-quit ()
  "Quit debugger."
  (interactive)
  (epsilon-debug-abort))

(defun epsilon-debug--close ()
  "Close debugger buffer."
  (when-let ((buffer (get-buffer "*epsilon-debugger*")))
    (kill-buffer buffer)))

(defun epsilon-debug-help ()
  "Show debugger help."
  (interactive)
  (message "a:abort c:continue 0-9:restart s:step n:next o:out p/TAB:frame e:eval v:locals q:quit"))

(provide 'epsilon-debug)

;;; epsilon-debug.el ends here
