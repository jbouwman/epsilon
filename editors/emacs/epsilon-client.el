;;; epsilon-client.el --- ELS protocol client -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jesse Bouwman
;; Author: Jesse Bouwman
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This module implements the core ELS (Epsilon Language Server) protocol
;; client for Emacs.  It handles TCP connection management, message framing
;; (4-byte big-endian length prefix), async request/response, and notification
;; dispatch.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup epsilon nil
  "Epsilon Language Server integration."
  :group 'languages
  :prefix "epsilon-")

(defcustom epsilon-server-host "127.0.0.1"
  "ELS server hostname."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-server-port 4141
  "ELS server port."
  :type 'integer
  :group 'epsilon)

(defcustom epsilon-auto-start-server t
  "Automatically start ELS server if not running."
  :type 'boolean
  :group 'epsilon)

(defcustom epsilon-server-executable "epsilon"
  "Path to the Epsilon executable."
  :type 'string
  :group 'epsilon)

(defcustom epsilon-connection-timeout 10
  "Timeout in seconds for establishing connection."
  :type 'integer
  :group 'epsilon)

(defcustom epsilon-request-timeout 30
  "Default timeout in seconds for synchronous requests."
  :type 'integer
  :group 'epsilon)

(defcustom epsilon-interactive-timeout 2
  "Timeout in seconds for interactive features (eldoc, completion).
Keep this short to avoid blocking Emacs."
  :type 'number
  :group 'epsilon)

(defcustom epsilon-max-connect-retries 3
  "Maximum number of connection retry attempts."
  :type 'integer
  :group 'epsilon)

(defcustom epsilon-connect-on-open t
  "Connect to ELS when opening Epsilon source files.
When non-nil, connection is initiated immediately when a file is opened.
When nil, connection is deferred until the first edit."
  :type 'boolean
  :group 'epsilon)

(defcustom epsilon-connect-retry-delay 2
  "Delay in seconds between connection retry attempts."
  :type 'number
  :group 'epsilon)

;;; State Variables

(defvar epsilon--connection nil
  "Current ELS connection process.")

(defvar epsilon--session-id nil
  "Current session ID.")

(defvar epsilon--capabilities nil
  "Server capabilities after hello handshake.")

(defvar epsilon--pending-requests (make-hash-table :test 'equal)
  "Hash table of pending request callbacks, keyed by request ID.")

(defvar epsilon--next-id 0
  "Counter for generating unique request IDs.")

(defvar epsilon--notification-handlers (make-hash-table :test 'equal)
  "Hash table of notification handlers, keyed by channel/op.")

(defvar epsilon--buffer-queue ""
  "Buffer for incomplete incoming data.")

(defvar epsilon--reconnect-timer nil
  "Timer for reconnection attempts.")

(defvar epsilon--server-process nil
  "Process for the ELS server if auto-started.")

(defvar epsilon--connecting nil
  "Non-nil when a connection attempt is in progress.")

(defvar epsilon--connect-retry-count 0
  "Current retry count for connection attempts.")

(defvar epsilon--startup-timer nil
  "Timer for async server startup.")

(defvar epsilon--last-connect-error nil
  "Last connection error message.")

(defvar epsilon--startup-failed-permanently nil
  "Non-nil if startup failed due to missing executable or fatal error.
Prevents infinite reconnection attempts.")

(defvar epsilon--current-workspace nil
  "Current workspace path after successful hello handshake.")

(defvar-local epsilon--buffer-workspace nil
  "Workspace root for the current buffer.")

;;; Mode-Line Status

(defvar-local epsilon-mode-line-string nil
  "Mode line string showing ELS connection status.")

(defun epsilon--update-mode-line ()
  "Update the mode-line to reflect ELS connection status."
  (setq epsilon-mode-line-string
        (cond
         (epsilon--startup-failed-permanently "[ELS:!]")
         (epsilon--connecting "[ELS:...]")
         ((not (epsilon-connected-p)) "[ELS:-]")
         ((not epsilon--session-id) "[ELS:...]")
         (epsilon--current-workspace
          (format "[ELS:%s]"
                  (file-name-nondirectory
                   (directory-file-name epsilon--current-workspace))))
         (t "[ELS:+]")))
  (force-mode-line-update t))

(defun epsilon-mode-line-setup ()
  "Set up ELS status in mode-line for the current buffer."
  (setq-local epsilon-mode-line-string "[ELS:-]")
  (unless (member '(epsilon-mode-line-string (" " epsilon-mode-line-string))
                  mode-line-misc-info)
    (push '(epsilon-mode-line-string (" " epsilon-mode-line-string))
          mode-line-misc-info))
  (epsilon--update-mode-line))

;;; Frame Codec

(defun epsilon--encode-frame (json-string)
  "Encode JSON-STRING as ELS frame (4-byte BE length prefix)."
  (let* ((bytes (encode-coding-string json-string 'utf-8))
         (len (length bytes)))
    (concat (unibyte-string
             (logand (ash len -24) #xff)
             (logand (ash len -16) #xff)
             (logand (ash len -8) #xff)
             (logand len #xff))
            bytes)))

(defun epsilon--decode-frame-length (bytes)
  "Decode 4-byte BE length from BYTES."
  (when (>= (length bytes) 4)
    (+ (ash (aref bytes 0) 24)
       (ash (aref bytes 1) 16)
       (ash (aref bytes 2) 8)
       (aref bytes 3))))

(defun epsilon--decode-frame (bytes)
  "Decode a complete ELS frame from BYTES.
Returns (PAYLOAD . REMAINING-BYTES) or nil if incomplete."
  (when (>= (length bytes) 4)
    (let ((len (epsilon--decode-frame-length bytes)))
      (when (>= (length bytes) (+ 4 len))
        (let ((payload (substring bytes 4 (+ 4 len)))
              (remaining (substring bytes (+ 4 len))))
          (cons (decode-coding-string payload 'utf-8) remaining))))))

;;; Connection Management

(defun epsilon-connected-p ()
  "Return non-nil if connected to ELS server."
  (and epsilon--connection
       (process-live-p epsilon--connection)))

(defun epsilon-ready-p ()
  "Return non-nil if connected and session established."
  (and (epsilon-connected-p) epsilon--session-id))

(defun epsilon-connect (&optional callback)
  "Connect to ELS server.
Optional CALLBACK is called with t on success, nil on failure.
Returns t if already connected, nil if connection attempt started."
  (interactive)
  (cond
   ;; Already connected
   ((epsilon-connected-p)
    (when (called-interactively-p 'any)
      (message "Already connected to ELS"))
    (when callback (funcall callback t))
    t)
   ;; Connection in progress
   (epsilon--connecting
    (when (called-interactively-p 'any)
      (message "Connection attempt already in progress"))
    nil)
   ;; Start new connection
   (t
    (setq epsilon--connecting t
          epsilon--last-connect-error nil)
    (epsilon--try-connect callback)
    nil)))

(defun epsilon--try-connect (&optional callback)
  "Internal: attempt to connect to ELS server."
  (epsilon--update-mode-line)
  (condition-case err
      (progn
        (setq epsilon--connection
              (make-network-process
               :name "epsilon-els"
               :host epsilon-server-host
               :service epsilon-server-port
               :coding 'binary
               :filter #'epsilon--connection-filter
               :sentinel #'epsilon--connection-sentinel
               :noquery t
               :nowait nil))  ; Sync connect is fast for localhost
        (setq epsilon--buffer-queue ""
              epsilon--connecting nil
              epsilon--connect-retry-count 0)
        (epsilon--update-mode-line)
        (message "Connected to ELS at %s:%d"
                 epsilon-server-host epsilon-server-port)
        ;; Async handshake
        (epsilon--hello-handshake-async callback)
        t)
    (file-error
     (setq epsilon--last-connect-error (error-message-string err))
     (epsilon--handle-connect-failure callback))))

(defun epsilon--handle-connect-failure (&optional callback)
  "Handle connection failure with optional retry or server start."
  (cond
   ;; Permanent failure - give up immediately
   (epsilon--startup-failed-permanently
    (setq epsilon--connecting nil
          epsilon--connect-retry-count 0)
    (message "ELS unavailable: %s. Use M-x epsilon-reset to retry."
             (or epsilon--last-connect-error "startup failed"))
    (when callback (funcall callback nil)))
   ;; Try auto-starting server on first failure
   ((and epsilon-auto-start-server
         (= epsilon--connect-retry-count 0)
         (not (epsilon--server-running-p)))
    (message "ELS not running, starting server...")
    (unless (epsilon--start-server)
      ;; Start failed immediately (e.g., missing executable)
      (setq epsilon--connecting nil
            epsilon--connect-retry-count 0)
      (message "ELS unavailable: %s. Use M-x epsilon-reset to retry."
               (or epsilon--last-connect-error "startup failed"))
      (when callback (funcall callback nil))
      (cl-return-from epsilon--handle-connect-failure nil))
    (cl-incf epsilon--connect-retry-count)
    ;; Schedule retry after delay (non-blocking)
    (setq epsilon--startup-timer
          (run-with-timer epsilon-connect-retry-delay nil
                          #'epsilon--retry-connect callback)))
   ;; Retry if under limit
   ((< epsilon--connect-retry-count epsilon-max-connect-retries)
    (cl-incf epsilon--connect-retry-count)
    (message "ELS connection failed, retry %d/%d..."
             epsilon--connect-retry-count epsilon-max-connect-retries)
    (setq epsilon--startup-timer
          (run-with-timer epsilon-connect-retry-delay nil
                          #'epsilon--retry-connect callback)))
   ;; Give up
   (t
    (setq epsilon--connecting nil
          epsilon--connect-retry-count 0)
    (message "Failed to connect to ELS: %s" epsilon--last-connect-error)
    (when callback (funcall callback nil)))))

(defun epsilon--retry-connect (&optional callback)
  "Timer callback to retry connection."
  (setq epsilon--startup-timer nil)
  (epsilon--try-connect callback))

(defun epsilon--server-running-p ()
  "Return non-nil if the server process is running."
  (and epsilon--server-process
       (process-live-p epsilon--server-process)))

(defun epsilon--executable-available-p ()
  "Return non-nil if epsilon executable is available.
Checks both PATH and direct file path."
  (or (executable-find epsilon-server-executable)
      (and (file-exists-p epsilon-server-executable)
           (file-executable-p epsilon-server-executable))))

(defun epsilon-disconnect ()
  "Disconnect from ELS server."
  (interactive)
  ;; Cancel pending timers
  (when epsilon--startup-timer
    (cancel-timer epsilon--startup-timer)
    (setq epsilon--startup-timer nil))
  (when epsilon--reconnect-timer
    (cancel-timer epsilon--reconnect-timer)
    (setq epsilon--reconnect-timer nil))
  ;; Clean up connection
  (when epsilon--connection
    (delete-process epsilon--connection))
  ;; Kill server process if we started it
  (when (and epsilon--server-process (process-live-p epsilon--server-process))
    (delete-process epsilon--server-process))
  (setq epsilon--connection nil
        epsilon--session-id nil
        epsilon--capabilities nil
        epsilon--current-workspace nil
        epsilon--buffer-queue ""
        epsilon--connecting nil
        epsilon--connect-retry-count 0
        epsilon--server-process nil)
  (clrhash epsilon--pending-requests)
  (epsilon--update-mode-line)
  (when (called-interactively-p 'any)
    (message "Disconnected from ELS")))

(defun epsilon-reset ()
  "Reset ELS state and allow retry after permanent failure.
Use this after fixing the epsilon installation."
  (interactive)
  (epsilon-disconnect)
  (setq epsilon--startup-failed-permanently nil
        epsilon--last-connect-error nil)
  (epsilon--update-mode-line)
  (message "ELS state reset. Use M-x epsilon-connect to retry."))

(defun epsilon-reconnect ()
  "Reconnect to ELS server."
  (interactive)
  (epsilon-disconnect)
  (epsilon-connect))

(defun epsilon-ensure-connected ()
  "Ensure connection to ELS server, connecting if needed.
Returns t if ready, nil if not connected (non-blocking)."
  (cond
   ;; Already ready
   ((epsilon-ready-p) t)
   ;; Connected but no session - start handshake
   ((epsilon-connected-p)
    (epsilon--hello-handshake-async)
    nil)
   ;; Not connected - start connection (async)
   (t
    (epsilon-connect)
    nil)))

(defun epsilon--start-server ()
  "Start the ELS server process.
Returns t if server was started, nil if failed."
  (unless (epsilon--server-running-p)
    (unless (epsilon--executable-available-p)
      (setq epsilon--startup-failed-permanently t
            epsilon--last-connect-error
            (format "Epsilon executable not found: %s" epsilon-server-executable))
      (cl-return-from epsilon--start-server nil))
    (let ((process-connection-type nil))
      (setq epsilon--server-process
            (start-process "epsilon-server"
                           "*epsilon-server*"
                           epsilon-server-executable
                           "--module" "epsilon.els"
                           "--eval" "(epsilon.els:main)"))
      ;; Monitor for immediate exit (crash on startup)
      (set-process-sentinel epsilon--server-process #'epsilon--server-sentinel)
      t)))

(defun epsilon--server-sentinel (process event)
  "Handle server process state changes.
Detects when server exits immediately after start (likely fatal error)."
  (when (string-match-p "\\(exited\\|finished\\|failed\\)" event)
    (let ((exit-code (process-exit-status process)))
      ;; If server exited with error shortly after start, mark as permanent failure
      (when (not (zerop exit-code))
        (setq epsilon--startup-failed-permanently t
              epsilon--last-connect-error
              (format "Epsilon server exited with code %d" exit-code))
        (message "Epsilon server failed to start (exit code %d). Use M-x epsilon-reset to retry."
                 exit-code)))))

(defun epsilon--connection-filter (process data)
  "Handle incoming DATA from ELS connection PROCESS."
  (setq epsilon--buffer-queue
        (concat epsilon--buffer-queue (string-to-unibyte data)))
  (let ((frame-result nil))
    (while (setq frame-result (epsilon--decode-frame epsilon--buffer-queue))
      (let ((payload (car frame-result))
            (remaining (cdr frame-result)))
        (setq epsilon--buffer-queue remaining)
        (epsilon--handle-message
         (json-parse-string payload :object-type 'alist))))))

(defun epsilon--connection-sentinel (process event)
  "Handle connection state change EVENT for PROCESS."
  (cond
   ((string-match-p "deleted\\|connection broken\\|failed" event)
    (setq epsilon--connection nil
          epsilon--session-id nil
          epsilon--current-workspace nil)
    (epsilon--update-mode-line)
    (message "ELS connection lost: %s" (string-trim event))
    (epsilon--schedule-reconnect))
   ((string-match-p "open" event)
    (epsilon--update-mode-line)
    (message "ELS connection established"))))

(defun epsilon--schedule-reconnect ()
  "Schedule a reconnection attempt."
  ;; Don't auto-reconnect after permanent failure
  (when epsilon--startup-failed-permanently
    (message "ELS permanently unavailable. Use M-x epsilon-reset to retry.")
    (cl-return-from epsilon--schedule-reconnect nil))
  (when epsilon--reconnect-timer
    (cancel-timer epsilon--reconnect-timer))
  (setq epsilon--reconnect-timer
        (run-with-timer 5 nil #'epsilon--try-reconnect)))

(defun epsilon--try-reconnect ()
  "Attempt to reconnect to ELS."
  (setq epsilon--reconnect-timer nil)
  (condition-case nil
      (epsilon-connect)
    (error (epsilon--schedule-reconnect))))

;;; Message Handling

(defun epsilon--handle-message (message)
  "Handle incoming MESSAGE from ELS."
  (let ((type (alist-get 'type message)))
    (pcase type
      ("response" (epsilon--handle-response message))
      ("notification" (epsilon--dispatch-notification message))
      ("error" (epsilon--handle-error message))
      (_ (message "Unknown message type: %s" type)))))

(defun epsilon--handle-response (message)
  "Handle response MESSAGE from ELS."
  (let* ((id (alist-get 'id message))
         (callback (gethash id epsilon--pending-requests)))
    (when callback
      (remhash id epsilon--pending-requests)
      (let ((payload (alist-get 'payload message)))
        (funcall callback payload)))))

(defun epsilon--handle-error (message)
  "Handle error MESSAGE from ELS."
  (let* ((id (alist-get 'id message))
         (callback (gethash id epsilon--pending-requests))
         (error-info (alist-get 'error message)))
    (when callback
      (remhash id epsilon--pending-requests)
      (funcall callback `((status . "error") (error . ,error-info))))
    (message "ELS error: %s" (alist-get 'message error-info))))

(defun epsilon--dispatch-notification (message)
  "Dispatch incoming notification MESSAGE to registered handler."
  (let* ((channel (alist-get 'channel message))
         (op (alist-get 'op message))
         (payload (alist-get 'payload message))
         (handler (gethash (cons channel op) epsilon--notification-handlers)))
    (when handler
      (funcall handler payload))))

;;; Request/Response API

(defun epsilon--send-message (message)
  "Send MESSAGE to ELS server.
Returns t on success, nil on failure."
  (condition-case nil
      (when (epsilon-connected-p)
        (let* ((json-str (json-encode message))
               (frame (epsilon--encode-frame json-str)))
          (process-send-string epsilon--connection frame)
          t))
    (error nil)))

(defun epsilon--request-internal (channel op payload callback)
  "Internal request function that doesn't check connection state.
Used by the hello handshake to avoid recursion.
Returns request ID on success, nil on failure."
  (when (epsilon-connected-p)
    (let* ((id (format "%d" (cl-incf epsilon--next-id)))
           (message `((id . ,id)
                      (type . "request")
                      (channel . ,channel)
                      (op . ,op)
                      (session . ,epsilon--session-id)
                      (payload . ,(or payload (make-hash-table))))))
      (when callback
        (puthash id callback epsilon--pending-requests))
      (if (epsilon--send-message message)
          id
        ;; Failed to send - clean up and return nil
        (when callback
          (remhash id epsilon--pending-requests))
        nil))))

(defun epsilon-request (channel op &optional payload callback)
  "Send async request to ELS server.
CHANNEL and OP identify the operation.
PAYLOAD is the request payload (plist or alist).
CALLBACK is called with the response payload.
Returns the request ID, or nil if not connected.
This is non-blocking and will not hang Emacs."
  (if (epsilon-ready-p)
      (epsilon--request-internal channel op payload callback)
    ;; Not ready - try to connect in background, call callback with nil
    (epsilon-ensure-connected)
    (when callback
      (funcall callback nil))
    nil))

(defun epsilon-request-sync (channel op &optional payload timeout)
  "Synchronous request to ELS server.
CHANNEL and OP identify the operation.
PAYLOAD is the request payload.
TIMEOUT defaults to `epsilon-request-timeout' seconds.
Returns response payload, or nil if not connected or timeout.
WARNING: This blocks Emacs. Prefer `epsilon-request' for interactive use."
  (unless (epsilon-ready-p)
    ;; Not ready - return nil immediately instead of blocking
    (epsilon-ensure-connected)
    (cl-return-from epsilon-request-sync nil))
  (let ((result nil)
        (done nil)
        (timeout-seconds (or timeout epsilon-request-timeout))
        (request-id nil))
    (setq request-id
          (epsilon--request-internal channel op payload
                                     (lambda (response)
                                       (setq result response
                                             done t))))
    (unless request-id
      (cl-return-from epsilon-request-sync nil))
    ;; Wait with timeout
    (let ((end-time (+ (float-time) timeout-seconds)))
      (while (and (not done)
                  (epsilon-connected-p)
                  (< (float-time) end-time))
        (accept-process-output epsilon--connection 0.1)))
    ;; Clean up if timed out
    (unless done
      (remhash request-id epsilon--pending-requests))
    result))

(defun epsilon-request-sync-fast (channel op &optional payload)
  "Fast synchronous request with short timeout for interactive use.
Uses `epsilon-interactive-timeout' (default 2 seconds).
Returns nil immediately if not connected."
  (epsilon-request-sync channel op payload epsilon-interactive-timeout))

(defun epsilon-notify (channel op &optional payload)
  "Send notification to ELS server (no response expected).
CHANNEL and OP identify the operation.
PAYLOAD is the notification payload.
Returns t if sent, nil if not connected."
  (when (epsilon-ready-p)
    (let ((message `((type . "notification")
                     (channel . ,channel)
                     (op . ,op)
                     (session . ,epsilon--session-id)
                     (payload . ,(or payload (make-hash-table))))))
      (epsilon--send-message message))))

;;; Notification Handlers

(defun epsilon-register-notification-handler (channel op handler)
  "Register HANDLER for notifications on CHANNEL/OP.
HANDLER is called with the notification payload."
  (puthash (cons channel op) handler epsilon--notification-handlers))

(defun epsilon-unregister-notification-handler (channel op)
  "Unregister notification handler for CHANNEL/OP."
  (remhash (cons channel op) epsilon--notification-handlers))

;;; Hello Handshake

(defun epsilon--hello-handshake-async (&optional callback)
  "Perform async hello handshake with ELS server.
CALLBACK is called with t on success, nil on failure."
  (let ((workspace (epsilon--active-workspace)))
    (epsilon--request-internal
     "control" "hello"
     `((protocol-version . "1.0")
       (client . ,(format "emacs-epsilon/%s" epsilon-version))
       (capabilities . ((watch-files . t)
                        (push-diagnostics . t)))
       ,@(when workspace
           `((workspace . ,workspace)
             (auto-load-workspace . t))))
     (lambda (response)
       (if (and response (equal "ok" (alist-get 'status response)))
           (progn
             (setq epsilon--session-id (alist-get 'session-id response))
             (setq epsilon--capabilities (alist-get 'capabilities response))
             (setq epsilon--current-workspace (alist-get 'workspace response))
             (epsilon--update-mode-line)
             (message "ELS session established: %s%s"
                      epsilon--session-id
                      (if epsilon--current-workspace
                          (format " (workspace: %s)" epsilon--current-workspace)
                        ""))
             (when callback (funcall callback t)))
         (epsilon--update-mode-line)
         (message "ELS hello failed")
         (when callback (funcall callback nil)))))))

(defun epsilon--hello-handshake ()
  "Perform synchronous hello handshake with ELS server.
Returns t on success, nil on failure."
  (unless (epsilon-connected-p)
    (cl-return-from epsilon--hello-handshake nil))
  (let ((result nil)
        (done nil)
        (workspace (epsilon--active-workspace)))
    (epsilon--request-internal
     "control" "hello"
     `((protocol-version . "1.0")
       (client . ,(format "emacs-epsilon/%s" epsilon-version))
       (capabilities . ((watch-files . t)
                        (push-diagnostics . t)))
       ,@(when workspace
           `((workspace . ,workspace)
             (auto-load-workspace . t))))
     (lambda (response)
       (if (and response (equal "ok" (alist-get 'status response)))
           (progn
             (setq epsilon--session-id (alist-get 'session-id response))
             (setq epsilon--capabilities (alist-get 'capabilities response))
             (setq epsilon--current-workspace (alist-get 'workspace response))
             (setq result t))
         (setq result nil))
       (setq done t)))
    ;; Short wait for handshake
    (let ((end-time (+ (float-time) epsilon-interactive-timeout)))
      (while (and (not done)
                  (epsilon-connected-p)
                  (< (float-time) end-time))
        (accept-process-output epsilon--connection 0.1)))
    (epsilon--update-mode-line)
    (when result
      (message "ELS session established: %s%s"
               epsilon--session-id
               (if epsilon--current-workspace
                   (format " (workspace: %s)" epsilon--current-workspace)
                 "")))
    result))

(defun epsilon-session-id ()
  "Return current ELS session ID."
  epsilon--session-id)

(defun epsilon-server-capabilities ()
  "Return ELS server capabilities."
  epsilon--capabilities)

;;; Utility Functions

(defun epsilon--symbol-at-point ()
  "Return the symbol at point as a string."
  (let ((sym (thing-at-point 'symbol t)))
    (when sym (substring-no-properties sym))))

(defun epsilon--sexp-before-point ()
  "Return the sexp before point as a string."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring-no-properties (point) end))))

(defun epsilon--enclosing-function ()
  "Return the name of the function enclosing point."
  (save-excursion
    (when (nth 1 (syntax-ppss))
      (goto-char (nth 1 (syntax-ppss)))
      (forward-char 1)
      (epsilon--symbol-at-point))))

(defun epsilon--current-package ()
  "Return the current package name."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "(in-package\\s-+[:#]?\\([^\")]+\\)" nil t)
        (match-string-no-properties 1)
      "epsilon.user")))

;;; Workspace Detection

(declare-function epsilon-project--find-workspace-root "epsilon-project")

(defun epsilon--active-workspace ()
  "Return the active workspace path, detecting if needed.
Uses the buffer-local workspace if set, otherwise detects from file path."
  (or epsilon--buffer-workspace
      (when-let ((root (and (fboundp 'epsilon-project--find-workspace-root)
                            (epsilon-project--find-workspace-root default-directory))))
        (setq epsilon--buffer-workspace root)
        root)))

(defun epsilon-connect-with-workspace (&optional callback)
  "Connect to ELS with workspace context.
Detects the workspace for the current buffer and initiates connection.
Optional CALLBACK is called with t on success, nil on failure."
  (let ((workspace (epsilon--active-workspace)))
    ;; Store for use during hello handshake
    (setq epsilon--buffer-workspace workspace)
    (epsilon-connect callback)))

;;; Version

(defconst epsilon-version "0.1.0"
  "Version of the Epsilon Emacs package.")

(provide 'epsilon-client)

;;; epsilon-client.el ends here
