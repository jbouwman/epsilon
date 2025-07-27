(defpackage epsilon.monitor.cli
  (:use 
   cl
   epsilon.type)
  (:local-nicknames
   (monitor epsilon.monitor)
   (time epsilon.time)
   (string epsilon.string))
  (:export
   cli-renderer
   make-cli-renderer
   
   ;; Internal functions exposed for testing
   cli-show-progress
   cli-progress-width
   make-progress-bar
   format-duration
   format-percentage
   get-indent-string
   get-context-depth))

(in-package epsilon.monitor.cli)

;;;; CLI Renderer for Human-Friendly Output

(defclass cli-renderer (monitor:process-renderer)
  ((show-progress :initarg :show-progress
                 :initform t
                 :accessor cli-show-progress
                 :documentation "Whether to show progress bars")
   (use-color :initarg :use-color
             :initform nil  ; Default to no color for now
             :accessor cli-use-color
             :documentation "Whether to use ANSI color codes")
   (progress-width :initarg :progress-width
                  :initform 20
                  :accessor cli-progress-width
                  :documentation "Width of progress bars in characters")
   (indent-level :initform 0
                :accessor cli-indent-level
                :documentation "Current indentation level for nested contexts")
   (context-states :initform (make-hash-table :test 'equal)
                  :accessor cli-context-states
                  :documentation "Track state of each context for updates"))
  (:documentation "CLI renderer that outputs human-friendly progress and status"))

(defun make-cli-renderer (&key (show-progress t) use-color (progress-width 20))
  "Create a new CLI renderer with specified options"
  (make-instance 'cli-renderer
                 :name "CLI Renderer"
                 :show-progress show-progress
                 :use-color use-color
                 :progress-width progress-width))

;;;; Utility Functions

(defun interactive-terminal-p ()
  "Check if we're running in an interactive terminal"
  ;; Simple heuristic - check if stdout is a TTY
  ;; TODO: More sophisticated detection
  (and (not (string:contains-p (or (sb-ext:posix-getenv "TERM") "") "dumb"))
       ;; Use a simple check for now - proper TTY detection would require more platform-specific code
       (not (sb-ext:posix-getenv "CI"))))

(defun make-progress-bar (progress width &optional (char #\█))
  "Create a text progress bar"
  (let* ((clamped-progress (max 0.0 (min 1.0 progress)))
         (filled (round (* clamped-progress width)))
         (empty (- width filled)))
    (concatenate 'string
                 (make-string filled :initial-element char)
                 (make-string empty :initial-element #\Space))))

(defun format-duration (duration)
  "Format duration in human-readable form"
  (let ((seconds (+ (time:duration-seconds duration) 
                   (/ (time:duration-nanoseconds duration) 1000000000.0))))
    (cond
      ((< seconds 1) (format nil "~,2fs" seconds))
      ((< seconds 60) (format nil "~,1fs" seconds))
      ((< seconds 3600) 
       (let ((mins (floor seconds 60))
             (secs (mod (floor seconds) 60)))
         (format nil "~dm ~ds" mins secs)))
      (t 
       (let ((hours (floor seconds 3600))
             (mins (floor (mod seconds 3600) 60)))
         (if (zerop mins)
             (format nil "~dh" hours)
             (format nil "~dh ~dm" hours mins)))))))

(defun format-percentage (progress)
  "Format progress as percentage"
  (if (numberp progress)
      (format nil "~3d%" (round (* progress 100)))
      "   "))

(defun get-indent-string (level)
  "Get indentation string for nested contexts"
  (if (zerop level)
      ""
      (concatenate 'string
                   (make-string (1- level) :initial-element #\│)
                   (string #\├)
                   (string #\─)
                   " ")))

(defun get-context-depth (context)
  "Calculate the nesting depth of a context"
  (if (and context (monitor:process-context-parent context))
      (1+ (get-context-depth (monitor:process-context-parent context)))
      0))

;;;; Event Rendering

(defmethod monitor:render-event ((renderer cli-renderer) event context)
  "Render an event to CLI output"
  (let ((depth (if context (get-context-depth context) 0)))
    (case (monitor:process-event-type event)
      (:start (render-start-event renderer event context depth))
      (:progress (render-progress-event renderer event context depth))
      (:log (render-log-event renderer event context depth))
      (:warning (render-warning-event renderer event context depth))
      (:error (render-error-event renderer event context depth))
      (:complete (render-complete-event renderer event context depth))
      (t (render-generic-event renderer event context depth)))))

(defun render-start-event (renderer event context depth)
  "Render a process start event"
  (let ((indent (get-indent-string depth))
        (operation (or (monitor:process-event-message event)
                      (and context (monitor:process-context-operation context))
                      "Starting process")))
    (format t "~A~A~%" indent operation)
    (force-output)))

(defun render-progress-event (renderer event context depth)
  "Render a progress update event"
  (when (and (cli-show-progress renderer) context)
    (let* ((indent (get-indent-string depth))
           (progress (monitor:process-context-progress context))
           (operation (monitor:process-context-operation context))
           (progress-bar (if (numberp progress)
                            (make-progress-bar progress (cli-progress-width renderer))
                            ""))
           (percentage (format-percentage progress))
           (elapsed (time:since (monitor:process-context-start-time context)))
           (duration-str (format-duration elapsed)))
      
      ;; For now, just print progress updates
      ;; TODO: Implement in-place updates using ANSI escape codes
      (when (monitor:process-event-message event)
        (format t "~A~A ~A ~A (~A)~%" 
                indent
                progress-bar
                percentage
                (monitor:process-event-message event)
                duration-str)
        (force-output)))))

(defun render-log-event (renderer event context depth)
  "Render a log event"
  (let ((indent (get-indent-string depth))
        (level (monitor:process-event-level event))
        (message (monitor:process-event-message event)))
    (case level
      (:trace (when (> *debug-level* 2)
                (format t "~ATRACE: ~A~%" indent message)))
      (:debug (when (> *debug-level* 1)
                (format t "~ADEBUG: ~A~%" indent message)))
      (:info (format t "~A~A~%" indent message))
      (t (format t "~A~A: ~A~%" indent (string-upcase level) message)))
    (force-output)))

(defun render-warning-event (renderer event context depth)
  "Render a warning event"
  (let ((indent (get-indent-string depth))
        (message (monitor:process-event-message event)))
    (format t "~AWARNING: ~A~%" indent message)
    (force-output)))

(defun render-error-event (renderer event context depth)
  "Render an error event with structured information"
  (let ((indent (get-indent-string depth))
        (message (monitor:process-event-message event))
        (data (monitor:process-event-data event)))
    
    (format t "~AERROR: ~A~%" indent message)
    
    ;; Show additional error context if available
    (when data
      (let ((error-type (getf data :error-type))
            (suggestions (getf data :suggestions))
            (related-contexts (getf data :related-contexts)))
        
        (when error-type
          (format t "~A  Type: ~A~%" indent error-type))
        
        (when suggestions
          (format t "~A  Suggestions:~%" indent)
          (dolist (suggestion suggestions)
            (format t "~A    - ~A~%" indent suggestion)))
        
        (when related-contexts
          (format t "~A  Related: ~{~A~^, ~}~%" indent related-contexts))))
    
    (force-output)))

(defun render-complete-event (renderer event context depth)
  "Render a completion event"
  (let ((indent (get-indent-string depth))
        (message (monitor:process-event-message event))
        (success (eq (monitor:process-context-state context) :completed))
        (elapsed (time:since (monitor:process-context-start-time context))))
    
    (format t "~A~A ~A (~A)~%" 
            indent
            (if success "✓" "✗")
            message
            (format-duration elapsed))
    (force-output)))

(defun render-generic-event (renderer event context depth)
  "Render any other type of event"
  (let ((indent (get-indent-string depth))
        (type (monitor:process-event-type event))
        (message (monitor:process-event-message event)))
    (when message
      (format t "~A[~A] ~A~%" indent (string-upcase type) message)
      (force-output))))

;;;; Debug Level Support

(defparameter *debug-level* 0
  "Debug output level: 0=info+, 1=debug+, 2=trace+")

(defun set-debug-level (level)
  "Set the debug output level"
  (setf *debug-level* level))