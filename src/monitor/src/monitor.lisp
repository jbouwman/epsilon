(defpackage epsilon.monitor
  (:use 
   cl
   epsilon.type)
  (:local-nicknames
   (time epsilon.time)
   (log epsilon.log))
  (:export
   ;; Core structures
   process-context
   process-event
   process-monitor
   
   ;; Context accessors
   process-context-id
   process-context-parent
   process-context-operation
   process-context-state
   process-context-progress
   process-context-metadata
   process-context-start-time
   process-context-estimated-duration
   process-context-tags
   
   ;; Event accessors
   make-process-event
   process-event-context-id
   process-event-timestamp
   process-event-type
   process-event-level
   process-event-data
   process-event-message
   process-event-tags
   
   ;; Context management
   create-process-context
   with-process-context
   with-nested-context
   current-context
   
   ;; Event emission
   process-start
   process-progress
   process-log
   process-warning
   process-error
   process-complete
   
   ;; Monitoring
   make-process-monitor
   monitor-active-contexts
   monitor-event-queue
   monitor-renderers
   monitor-running-p
   start-monitor
   stop-monitor
   emit-event
   emit-event-to-monitor
   
   ;; Renderers
   process-renderer
   render-event
   add-renderer
   remove-renderer
   
   ;; Default monitor management
   ensure-default-monitor
   set-default-monitor
   get-default-monitor))

(in-package epsilon.monitor)

;;;; Core Data Structures

(defstruct process-context
  "Represents the state and metadata of a running process or operation"
  id                    ; unique identifier (symbol or string)
  parent               ; parent context for hierarchical nesting
  operation            ; human-readable description of what's happening
  state               ; :starting :running :waiting :completed :failed :cancelled
  progress            ; progress information (number 0.0-1.0 or (:items completed total))
  metadata            ; extensible hash-table for additional data
  start-time          ; timestamp when context was created
  estimated-duration  ; estimated total time (seconds)
  tags)               ; list of tags for filtering/routing

(defstruct process-event
  "A single event emitted during process execution"
  context-id          ; id of the context that emitted this event
  timestamp           ; when the event occurred
  type               ; :start :progress :log :warning :error :complete :cancelled
  level             ; :trace :debug :info :warn :error :fatal
  data              ; event-specific payload (list or hash-table)
  message           ; human-readable message
  tags)             ; tags for filtering/routing

;;;; Global State

(defvar *current-context* nil
  "The currently active process context")

(defvar *default-monitor* nil
  "The default process monitor instance")

(defvar *context-counter* 0
  "Counter for generating unique context IDs")

;;;; Context Management

(defun generate-context-id ()
  "Generate a unique context identifier"
  (intern (format nil "CTX-~A" (incf *context-counter*)) :keyword))

(defun create-process-context (&key 
                               (id (generate-context-id))
                               parent
                               operation
                               (state :starting)
                               (progress 0.0)
                               metadata
                               estimated-duration
                               tags)
  "Create a new process context with optional parameters"
  (make-process-context
   :id id
   :parent parent
   :operation operation
   :state state
   :progress progress
   :metadata (or metadata (make-hash-table :test 'equal))
   :start-time (time:now)
   :estimated-duration estimated-duration
   :tags (if (listp tags) tags (list tags))))

(defun current-context ()
  "Return the currently active process context"
  *current-context*)

(defmacro with-process-context ((operation &key 
                                 id
                                 estimated-duration
                                 tags
                                 (auto-complete t)) &body body)
  "Execute body within a new process context"
  (let ((context-var (gensym "CONTEXT"))
        (result-var (gensym "RESULT"))
        (error-var (gensym "ERROR"))
        (monitor-var (gensym "MONITOR")))
    `(let* ((,context-var (create-process-context
                          :id ,id
                          :parent *current-context*
                          :operation ,operation
                          :estimated-duration ,estimated-duration
                          :tags ,tags))
            (*current-context* ,context-var)
            (,monitor-var *default-monitor*)
            ,result-var)
       ;; Register context with monitor
       (when ,monitor-var
         (setf (gethash (process-context-id ,context-var)
                       (monitor-active-contexts ,monitor-var))
               ,context-var))
       (unwind-protect
         (progn
           (process-start :context ,context-var)
           (handler-case
             (setf ,result-var (progn ,@body))
             (error (,error-var)
               (process-error (format nil "~A" ,error-var)
                            :context ,context-var
                            :data (list :error-type (type-of ,error-var)))
               (setf (process-context-state ,context-var) :failed)
               (signal ,error-var))))
         (progn
           (when ,auto-complete
             (unless (member (process-context-state ,context-var) '(:completed :failed :cancelled))
               (setf (process-context-state ,context-var) :completed)
               (process-complete :context ,context-var)))
           ;; Remove context from monitor
           (when ,monitor-var
             (remhash (process-context-id ,context-var)
                     (monitor-active-contexts ,monitor-var)))))
       ,result-var)))

(defmacro with-nested-context ((operation &key id estimated-duration tags) &body body)
  "Execute body within a nested process context"
  `(with-process-context (,operation :id ,id 
                                    :estimated-duration ,estimated-duration
                                    :tags ,tags)
     ,@body))

;;;; Event Emission

(defun emit-event (&key
                   (context *current-context*)
                   (monitor *default-monitor*)
                   type
                   (level :info)
                   message
                   data
                   tags)
  "Emit a process event to the monitor"
  (when (and context monitor)
    (let ((event (make-process-event
                  :context-id (process-context-id context)
                  :timestamp (time:now)
                  :type type
                  :level level
                  :data data
                  :message message
                  :tags tags)))
      (emit-event-to-monitor monitor event context))))

(defun process-start (&key (context *current-context*) message data)
  "Emit a process start event"
  (when context
    (setf (process-context-state context) :running))
  (emit-event :context context
              :type :start
              :level :info
              :message (or message 
                          (and context (process-context-operation context))
                          "Process started")
              :data data))

(defun process-progress (&key 
                         (context *current-context*)
                         completed
                         total
                         percentage
                         items
                         message
                         data)
  "Emit a progress update event"
  (when context
    (let ((progress-value 
           (cond
             (percentage (/ percentage 100.0))
             ((and completed total (plusp total)) (/ completed total))
             ((and completed total) 0.0) ; Division by zero case
             (items (list :items (first items) (second items)))
             (t (process-context-progress context)))))
      (setf (process-context-progress context) progress-value)))
  
  (emit-event :context context
              :type :progress
              :level :debug
              :message message
              :data (append data
                           (when completed (list :completed completed))
                           (when total (list :total total))
                           (when items (list :items items)))))

(defun process-log (level message &key (context *current-context*) data tags)
  "Emit a log event"
  (emit-event :context context
              :type :log
              :level level
              :message message
              :data data
              :tags tags))

(defun process-warning (message &key (context *current-context*) data tags)
  "Emit a warning event"
  (emit-event :context context
              :type :warning
              :level :warn
              :message message
              :data data
              :tags tags))

(defun process-error (message &key 
                      (context *current-context*)
                      data
                      tags
                      error-type
                      suggestions
                      related-contexts)
  "Emit an error event with structured error information"
  (when context
    (setf (process-context-state context) :failed))
  (emit-event :context context
              :type :error
              :level :error
              :message message
              :data (append data
                           (when error-type (list :error-type error-type))
                           (when suggestions (list :suggestions suggestions))
                           (when related-contexts (list :related-contexts related-contexts)))
              :tags tags))

(defun process-complete (&key 
                         (context *current-context*)
                         message
                         data
                         (success t))
  "Emit a completion event"
  (when context
    (setf (process-context-state context) (if success :completed :failed)))
  (emit-event :context context
              :type :complete
              :level :info
              :message (or message "Process completed")
              :data data))

;;;; Process Monitor

(defclass process-monitor ()
  ((active-contexts :initform (make-hash-table :test 'equal)
                   :accessor monitor-active-contexts
                   :documentation "Hash table of active process contexts")
   (event-queue :initform nil
               :accessor monitor-event-queue
               :documentation "Queue of pending events")
   (renderers :initform '()
             :accessor monitor-renderers
             :documentation "List of active renderers")
   (running :initform nil
           :accessor monitor-running-p
           :documentation "Whether the monitor is actively processing events")
   (background-thread :initform nil
                     :accessor monitor-background-thread
                     :documentation "Background thread for async event processing"))
  (:documentation "Central coordinator for process monitoring and event handling"))

(defun make-process-monitor (&key renderers)
  "Create a new process monitor with optional initial renderers"
  (let ((monitor (make-instance 'process-monitor)))
    (when renderers
      (dolist (renderer renderers)
        (add-renderer monitor renderer)))
    monitor))

(defgeneric emit-event-to-monitor (monitor event context)
  (:documentation "Emit an event to the monitor for processing"))

(defmethod emit-event-to-monitor ((monitor process-monitor) event context)
  "Add event to monitor's processing queue"
  (push event (monitor-event-queue monitor))
  (when (monitor-running-p monitor)
    (process-events monitor)))

(defgeneric process-events (monitor)
  (:documentation "Process pending events in the monitor"))

(defmethod process-events ((monitor process-monitor))
  "Process all pending events by sending them to renderers"
  (let ((events (reverse (monitor-event-queue monitor))))
    (setf (monitor-event-queue monitor) nil)
    (dolist (event events)
      (let ((context (gethash (process-event-context-id event)
                             (monitor-active-contexts monitor))))
        (dolist (renderer (monitor-renderers monitor))
          (handler-case
            (render-event renderer event context)
            (error (e)
              (log:warn "Renderer ~A failed: ~A" renderer e))))))))

(defgeneric start-monitor (monitor)
  (:documentation "Start the monitor's background processing"))

(defmethod start-monitor ((monitor process-monitor))
  "Start background event processing"
  (unless (monitor-running-p monitor)
    (setf (monitor-running-p monitor) t)))

(defgeneric stop-monitor (monitor)
  (:documentation "Stop the monitor's background processing"))

(defmethod stop-monitor ((monitor process-monitor))
  "Stop background event processing"
  (setf (monitor-running-p monitor) nil)
  (when (monitor-background-thread monitor)
    ;; TODO: Properly terminate background thread
    (setf (monitor-background-thread monitor) nil)))

(defgeneric add-renderer (monitor renderer)
  (:documentation "Add a renderer to the monitor"))

(defmethod add-renderer ((monitor process-monitor) renderer)
  "Add a renderer to process events"
  (push renderer (monitor-renderers monitor)))

(defgeneric remove-renderer (monitor renderer)
  (:documentation "Remove a renderer from the monitor"))

(defmethod remove-renderer ((monitor process-monitor) renderer)
  "Remove a renderer from event processing"
  (setf (monitor-renderers monitor)
        (remove renderer (monitor-renderers monitor))))

;;;; Renderer Base Class

(defclass process-renderer ()
  ((name :initarg :name
         :accessor renderer-name
         :documentation "Human-readable name for this renderer"))
  (:documentation "Base class for process event renderers"))

(defgeneric render-event (renderer event context)
  (:documentation "Render a single event using this renderer"))

(defmethod render-event ((renderer process-renderer) event context)
  "Default implementation - subclasses should override"
  (declare (ignore event context))
  nil)

;;;; Default Monitor Management

(defun ensure-default-monitor ()
  "Ensure that a default monitor exists"
  (unless *default-monitor*
    (setf *default-monitor* (make-process-monitor))
    (start-monitor *default-monitor*))
  *default-monitor*)

(defun set-default-monitor (monitor)
  "Set the default monitor for the system"
  (setf *default-monitor* monitor))

(defun get-default-monitor ()
  "Get the current default monitor"
  *default-monitor*)

;; Initialize default monitor when loaded
(ensure-default-monitor)