;;;; Evaluation Session Management
;;;;
;;;; This module provides session management for stateful code evaluation
;;;; in isolated subprocesses. Supports both ephemeral and persistent sessions
;;;; with resource controls and security restrictions.

(defpackage #:epsilon.lsp.evaluation.session
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:uuid #:epsilon.lib.uuid)
   (#:json #:epsilon.lib.json)
   (#:thread #:epsilon.sys.thread)
   (#:fs #:epsilon.sys.fs)
   (#:process #:epsilon.lib.process)
   (#:time #:epsilon.lib.time))
  (:export
   ;; Session types
   #:evaluation-session
   #:session-id
   #:session-name
   #:session-state
   #:session-process
   #:session-owner
   #:session-created-at
   #:session-last-used
   #:session-restrictions
   
   ;; Session manager
   #:session-manager
   #:create-session
   #:get-session
   #:list-sessions
   #:terminate-session
   #:cleanup-idle-sessions
   
   ;; Session states
   #:+session-active+
   #:+session-idle+
   #:+session-terminated+
   
   ;; Restrictions
   #:make-restrictions
   #:restriction-max-memory
   #:restriction-max-cpu-time
   #:restriction-allow-file-read
   #:restriction-allow-file-write
   #:restriction-allow-network))

(in-package #:epsilon.lsp.evaluation.session)

;;; Constants

(defconstant +session-active+ :active
  "Session is actively processing or ready for evaluation")

(defconstant +session-idle+ :idle
  "Session is idle but still alive")

(defconstant +session-terminated+ :terminated
  "Session has been terminated")

;;; Security Restrictions

(defstruct (evaluation-restrictions
            (:constructor make-restrictions
                          (&key (max-memory "256M")
                                (max-cpu-time 30)
                                (allow-file-read nil)
                                (allow-file-write nil)
                                (allow-network nil)
                                (allowed-paths '())
                                (forbidden-functions '()))))
  "Security and resource restrictions for an evaluation session"
  (max-memory "256M" :type string)
  (max-cpu-time 30 :type integer)
  (allow-file-read nil :type boolean)
  (allow-file-write nil :type boolean)
  (allow-network nil :type boolean)
  (allowed-paths '() :type list)
  (forbidden-functions '() :type list))

;;; Evaluation Session

(defclass evaluation-session ()
  ((id :initform (uuid:make-v4)
       :reader session-id
       :documentation "Unique session identifier")
   (name :initarg :name
         :initform nil
         :reader session-name
         :documentation "Human-readable session name")
   (process :initarg :process
            :accessor session-process
            :documentation "The subprocess running this session")
   (state :initform +session-active+
          :accessor session-state
          :documentation "Current session state")
   (owner :initarg :owner
          :reader session-owner
          :documentation "Client/workspace that owns this session")
   (created-at :initform (get-universal-time)
               :reader session-created-at
               :documentation "Session creation timestamp")
   (last-used :initform (get-universal-time)
              :accessor session-last-used
              :documentation "Last activity timestamp")
   (idle-timeout :initarg :idle-timeout
                 :initform 300
                 :reader session-idle-timeout
                 :documentation "Seconds before idle termination")
   (restrictions :initarg :restrictions
                 :initform (make-restrictions)
                 :reader session-restrictions
                 :documentation "Security and resource restrictions")
   (input-stream :accessor session-input-stream
                 :documentation "Input stream to subprocess")
   (output-stream :accessor session-output-stream
                  :documentation "Output stream from subprocess")
   (error-stream :accessor session-error-stream
                 :documentation "Error stream from subprocess")
   (pending-requests :initform (map:make-map)
                     :accessor session-pending-requests
                     :documentation "Map of request-id to pending evaluation")
   (modules :initarg :modules
            :initform '("epsilon.core")
            :reader session-modules
            :documentation "Modules loaded in this session")))

(defmethod print-object ((session evaluation-session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (format stream "~A (~A)" 
            (or (session-name session) (session-id session))
            (session-state session))))

;;; Session Manager

(defclass session-manager ()
  ((sessions :initform (map:make-map)
             :accessor manager-sessions
             :documentation "Map of session-id to evaluation-session")
   (max-sessions-per-client :initarg :max-sessions-per-client
                            :initform 5
                            :reader max-sessions-per-client)
   (default-idle-timeout :initarg :default-idle-timeout
                         :initform 300
                         :reader default-idle-timeout)
   (cleanup-interval :initform 30
                     :reader cleanup-interval)
   (cleanup-thread :initform nil
                   :accessor cleanup-thread)
   (executable-path :initarg :executable-path
                    :initform (or (sb-ext:posix-getenv "SBCL_HOME")
                                  "/usr/bin/sbcl")
                    :reader executable-path
                    :documentation "Path to SBCL executable")))

(defmethod initialize-instance :after ((manager session-manager) &key)
  "Start the cleanup thread after initialization"
  (start-cleanup-thread manager))

;;; Session Lifecycle

(defmethod create-session ((manager session-manager) 
                           &key name owner modules restrictions idle-timeout)
  "Create a new evaluation session with the specified configuration"
  ;; Check session limit for owner
  (let ((owner-sessions (count-owner-sessions manager owner)))
    (when (>= owner-sessions (max-sessions-per-client manager))
      (error "Session limit exceeded for client: ~A" owner)))
  
  ;; Launch subprocess
  (let* ((process (launch-evaluation-process 
                   :executable (executable-path manager)
                   :modules modules
                   :restrictions restrictions))
         (session (make-instance 'evaluation-session
                                 :name name
                                 :owner owner
                                 :process process
                                 :modules modules
                                 :restrictions (or restrictions (make-restrictions))
                                 :idle-timeout (or idle-timeout 
                                                   (default-idle-timeout manager)))))
    
    ;; Set up process streams
    (setf (session-input-stream session) (sb-ext:process-input process)
          (session-output-stream session) (sb-ext:process-output process)
          (session-error-stream session) (sb-ext:process-error process))
    
    ;; Initialize the session
    (handler-case
        (initialize-session session)
      (error (e)
        ;; Clean up on initialization failure
        (sb-ext:process-kill process 9)
        (error "Failed to initialize session: ~A" e)))
    
    ;; Register session
    (setf (map:get (manager-sessions manager) (session-id session)) session)
    
    ;; Return session
    session))

(defmethod get-session ((manager session-manager) session-id)
  "Retrieve a session by ID"
  (map:get (manager-sessions manager) session-id))

(defmethod list-sessions ((manager session-manager) &key owner)
  "List all sessions, optionally filtered by owner"
  (let ((all-sessions (map:vals (manager-sessions manager))))
    (if owner
        (remove-if-not (lambda (session)
                         (equal (session-owner session) owner))
                       all-sessions)
        all-sessions)))

(defmethod terminate-session ((manager session-manager) session-id)
  "Terminate a session and clean up resources"
  (let ((session (get-session manager session-id)))
    (when session
      ;; Update state
      (setf (session-state session) +session-terminated+)
      
      ;; Send shutdown command
      (ignore-errors
        (send-shutdown-command session))
      
      ;; Kill process
      (let ((process (session-process session)))
        (when (and process (sb-ext:process-alive-p process))
          ;; Give it a chance to shutdown gracefully
          (sleep 0.5)
          ;; Force kill if still alive
          (when (sb-ext:process-alive-p process)
            (sb-ext:process-kill process 9))))
      
      ;; Remove from manager
      (setf (manager-sessions manager) 
            (map:dissoc (manager-sessions manager) session-id))
      
      t)))

;;; Helper Functions

(defun count-owner-sessions (manager owner)
  "Count active sessions for a given owner"
  (count-if (lambda (session)
              (and (equal (session-owner session) owner)
                   (not (eq (session-state session) +session-terminated+))))
            (map:vals (manager-sessions manager))))

(defun launch-evaluation-process (&key executable modules restrictions)
  "Launch a new SBCL subprocess for evaluation"
  (let ((args (list "--noinform"
                    "--disable-ldb"
                    "--lose-on-corruption"
                    "--disable-debugger"
                    "--no-sysinit"
                    "--no-userinit")))
    
    ;; Add memory limit if specified
    (when (restriction-max-memory restrictions)
      (push (format nil "--dynamic-space-size=~A" 
                    (restriction-max-memory restrictions))
            args))
    
    ;; Add evaluation runner
    (push "--eval" args)
    (push "(require :epsilon.lsp.evaluation.runner)" args)
    (push "--eval" args)
    (push "(epsilon.lsp.evaluation.runner:main)" args)
    
    ;; Launch process
    (sb-ext:run-program executable (nreverse args)
                        :input :stream
                        :output :stream
                        :error :stream
                        :wait nil)))

(defun initialize-session (session)
  "Initialize a newly created session"
  ;; Send initialization message with modules and restrictions
  (let ((init-message (map:make-map
                       "type" "init"
                       "modules" (session-modules session)
                       "restrictions" (restrictions-to-map 
                                       (session-restrictions session)))))
    (write-message-to-session session init-message)
    
    ;; Wait for acknowledgment
    (let ((response (read-message-from-session session)))
      (unless (and (map:get response "type")
                   (string= (map:get response "type") "init-ack"))
        (error "Session initialization failed: ~A" response)))))

(defun send-shutdown-command (session)
  "Send shutdown command to session"
  (write-message-to-session session 
                            (map:make-map "type" "shutdown")))

(defun restrictions-to-map (restrictions)
  "Convert restrictions struct to map for serialization"
  (map:make-map
   "max-memory" (restriction-max-memory restrictions)
   "max-cpu-time" (restriction-max-cpu-time restrictions)
   "allow-file-read" (restriction-allow-file-read restrictions)
   "allow-file-write" (restriction-allow-file-write restrictions)
   "allow-network" (restriction-allow-network restrictions)
   "allowed-paths" (restriction-allowed-paths restrictions)
   "forbidden-functions" (restriction-forbidden-functions restrictions)))

;;; Session I/O

(defun write-message-to-session (session message)
  "Write a message to the session subprocess"
  (let ((stream (session-input-stream session)))
    (write-line (json:encode message) stream)
    (force-output stream)))

(defun read-message-from-session (session &key (timeout 30))
  "Read a message from the session subprocess"
  (let ((stream (session-output-stream session)))
    ;; TODO: Implement timeout
    (let ((line (read-line stream nil nil)))
      (when line
        (json:decode line)))))

;;; Cleanup Thread

(defun start-cleanup-thread (manager)
  "Start the background thread that cleans up idle sessions"
  (setf (cleanup-thread manager)
        (sb-thread:make-thread
         (lambda ()
           (loop
             (sleep (cleanup-interval manager))
             (handler-case
                 (cleanup-idle-sessions manager)
               (error (e)
                 (format *error-output* 
                         "Error in cleanup thread: ~A~%" e)))))
         :name "session-cleanup")))

(defmethod cleanup-idle-sessions ((manager session-manager))
  "Clean up sessions that have been idle too long"
  (let ((now (get-universal-time))
        (to-terminate '()))
    
    ;; Find idle sessions
    (map:each (lambda (id session)
                (when (and (eq (session-state session) +session-active+)
                           (> (- now (session-last-used session))
                              (session-idle-timeout session)))
                  ;; Mark as idle first
                  (setf (session-state session) +session-idle+)
                  ;; Schedule for termination
                  (push id to-terminate)))
              (manager-sessions manager))
    
    ;; Terminate idle sessions
    (dolist (session-id to-terminate)
      (terminate-session manager session-id))))