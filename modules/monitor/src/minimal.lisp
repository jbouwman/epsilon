(defpackage epsilon.monitor.minimal
  (:use 
   cl
   epsilon.type)
  (:local-nicknames
   (monitor epsilon.monitor)
   (time epsilon.time))
  (:export
   minimal-renderer
   make-minimal-renderer
   
   ;; Internal functions for testing
   minimal-error-only
   minimal-show-completion
   minimal-show-failures
   render-final-summary))

(in-package epsilon.monitor.minimal)

;;;; Minimal Renderer for Silent Operation

(defclass minimal-renderer (monitor:process-renderer)
  ((error-only :initarg :error-only
              :initform t
              :accessor minimal-error-only
              :documentation "Only show errors and warnings")
   (show-completion :initarg :show-completion
                   :initform nil
                   :accessor minimal-show-completion
                   :documentation "Show completion messages")
   (show-failures :initarg :show-failures
                 :initform t
                 :accessor minimal-show-failures
                 :documentation "Show failure summaries")
   (stream :initarg :stream
          :initform *error-output*
          :accessor minimal-stream
          :documentation "Output stream for messages"))
  (:documentation "Minimal renderer that stays silent unless there are problems"))

(defun make-minimal-renderer (&key 
                              (error-only t)
                              (show-completion nil)
                              (show-failures t)
                              (stream *error-output*))
  "Create a new minimal renderer with specified options"
  (make-instance 'minimal-renderer
                 :name "Minimal Renderer"
                 :error-only error-only
                 :show-completion show-completion
                 :show-failures show-failures
                 :stream stream))

;;;; Event Rendering

(defmethod monitor:render-event ((renderer minimal-renderer) event context)
  "Render only important events to minimize output noise"
  (case (monitor:process-event-type event)
    (:warning (render-warning renderer event context))
    (:error (render-error renderer event context))
    (:complete (when (or (minimal-show-completion renderer)
                        (and (minimal-show-failures renderer)
                             context
                             (eq (monitor:process-context-state context) :failed)))
                 (render-completion renderer event context)))
    (t nil))) ; Ignore all other events

(defun render-warning (renderer event context)
  "Render a warning message"
  (let ((message (monitor:process-event-message event))
        (operation (and context (monitor:process-context-operation context))))
    (if operation
        (format (minimal-stream renderer) "WARNING [~A]: ~A~%" operation message)
        (format (minimal-stream renderer) "WARNING: ~A~%" message))
    (force-output (minimal-stream renderer))))

(defun render-error (renderer event context)
  "Render an error message with minimal detail"
  (let ((message (monitor:process-event-message event))
        (operation (and context (monitor:process-context-operation context)))
        (data (monitor:process-event-data event)))
    
    ;; Basic error message
    (if operation
        (format (minimal-stream renderer) "ERROR [~A]: ~A~%" operation message)
        (format (minimal-stream renderer) "ERROR: ~A~%" message))
    
    ;; Show suggestions if available
    (when data
      (let ((suggestions (getf data :suggestions)))
        (when suggestions
          (format (minimal-stream renderer) "  Suggestions:~%")
          (dolist (suggestion suggestions)
            (format (minimal-stream renderer) "    - ~A~%" suggestion)))))
    
    (force-output (minimal-stream renderer))))

(defun render-completion (renderer event context)
  "Render completion status"
  (let ((message (monitor:process-event-message event))
        (success (and context (eq (monitor:process-context-state context) :completed)))
        (elapsed (and context (time:since (monitor:process-context-start-time context)))))
    
    (if success
        (when (minimal-show-completion renderer)
          (format (minimal-stream renderer) "COMPLETED: ~A" message)
          (when elapsed
            (format (minimal-stream renderer) " (~,2fs)" 
                    (+ (time:duration-seconds elapsed) 
                       (/ (time:duration-nanoseconds elapsed) 1000000000.0))))
          (format (minimal-stream renderer) "~%"))
        (when (minimal-show-failures renderer)
          (format (minimal-stream renderer) "FAILED: ~A" message)
          (when elapsed
            (format (minimal-stream renderer) " (after ~,2fs)" 
                    (+ (time:duration-seconds elapsed) 
                       (/ (time:duration-nanoseconds elapsed) 1000000000.0))))
          (format (minimal-stream renderer) "~%")))
    
    (force-output (minimal-stream renderer))))

;;;; Summary Functions

(defun render-final-summary (renderer failed-contexts total-contexts)
  "Render a final summary of operation results"
  (when (and (minimal-show-failures renderer) (> failed-contexts 0))
    (format (minimal-stream renderer) "~%SUMMARY: ~A of ~A operations failed~%" 
            failed-contexts total-contexts)
    (force-output (minimal-stream renderer))))