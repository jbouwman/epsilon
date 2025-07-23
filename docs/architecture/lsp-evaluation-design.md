# LSP Code Evaluation Architecture

## Overview

The Epsilon LSP server should use subprocess isolation for all code evaluation requests to maintain stability, security, and predictable behavior.

## Design Principles

1. **Complete Isolation**: Each evaluation runs in a separate process
2. **Resource Limits**: CPU time, memory, and I/O can be restricted
3. **Clean Environment**: Each evaluation starts fresh
4. **Non-blocking**: Evaluations don't block the LSP server
5. **Cancellable**: Long-running evaluations can be terminated

## Architecture

### Evaluation Manager

```lisp
(defclass evaluation-manager ()
  ((active-evaluations :initform (map:make-map)
                       :documentation "Map of request-id to evaluation-process")
   (max-concurrent :initform 5
                   :documentation "Maximum concurrent evaluations")
   (timeout :initform 30
            :documentation "Default timeout in seconds")))
```

### Evaluation Process

Each evaluation spawns a new SBCL process with:
- Limited memory (configurable, default 256MB)
- CPU time limit (configurable, default 30s)
- Restricted file system access
- No network access
- Clean environment with only epsilon.core loaded

### Communication Protocol

1. **Request**: LSP server sends evaluation request via stdin
2. **Response**: Subprocess returns results via stdout
3. **Errors**: Subprocess sends errors via stderr
4. **Status**: Process exit code indicates success/failure

### Message Format

```lisp
;; Request
{:id "unique-request-id"
 :code "(+ 1 2)"
 :context {:file-uri "file:///path/to/file.lisp"
           :position {:line 10 :character 5}
           :modules ["epsilon.core" "epsilon.http"]}
 :timeout 10}

;; Response
{:id "unique-request-id"
 :status :success
 :result "3"
 :output ""
 :duration 0.002}

;; Error Response
{:id "unique-request-id"
 :status :error
 :error {:type "DIVISION-BY-ZERO"
         :message "Division by zero"
         :backtrace ["frame1" "frame2"]}
 :output ""}
```

## Implementation Sketch

```lisp
(defun evaluate-in-subprocess (manager code &key context timeout)
  "Evaluate CODE in an isolated subprocess"
  (let* ((request-id (generate-request-id))
         (process (launch-evaluation-process))
         (request (make-evaluation-request 
                   :id request-id
                   :code code
                   :context context
                   :timeout (or timeout (timeout manager)))))
    
    ;; Track active evaluation
    (setf (map:get (active-evaluations manager) request-id) process)
    
    ;; Send request
    (write-to-process process request)
    
    ;; Set up timeout
    (schedule-timeout request-id timeout)
    
    ;; Return promise/future for async handling
    (make-evaluation-future request-id process)))

(defun launch-evaluation-process ()
  "Launch a new SBCL subprocess for evaluation"
  (sb-ext:run-program 
   (sb-ext:native-namestring #P"/usr/bin/sbcl")
   (list "--noinform"
         "--disable-ldb"
         "--lose-on-corruption"
         "--disable-debugger"
         "--eval" "(require :epsilon.lsp.evaluator)"
         "--eval" "(epsilon.lsp.evaluator:main)")
   :input :stream
   :output :stream
   :error :stream
   :wait nil))
```

## Security Considerations

1. **Sandboxing**: Use OS-level sandboxing (AppArmor/SELinux on Linux, sandbox-exec on macOS)
2. **Resource Limits**: Set ulimits for CPU, memory, file descriptors
3. **Restricted Packages**: Don't load packages that provide file/network access
4. **Code Sanitization**: Validate code before evaluation
5. **Audit Logging**: Log all evaluation requests

## Benefits

1. **Stability**: LSP server remains stable even if evaluation crashes
2. **Security**: Evaluated code cannot access LSP server internals
3. **Performance**: Multiple evaluations can run concurrently
4. **Debugging**: Each evaluation can be debugged independently
5. **Consistency**: Each evaluation has a predictable environment

## Stateful Evaluation Sessions

### Overview

While isolated evaluation is the default, many use cases require maintaining state across evaluations (REPL-style interaction). We support this through **Evaluation Sessions**.

### Session Management

```lisp
(defclass evaluation-session ()
  ((id :initform (generate-session-id)
       :reader session-id)
   (process :initarg :process
            :documentation "Long-lived subprocess for this session")
   (created-at :initform (get-universal-time))
   (last-used :initform (get-universal-time)
              :accessor last-used)
   (timeout :initform 300
            :documentation "Idle timeout in seconds")
   (state :initform :active
          :accessor session-state
          :documentation "One of :active, :idle, :terminated")
   (owner :initarg :owner
          :documentation "Client/workspace that owns this session")
   (restrictions :initarg :restrictions
                 :documentation "Security restrictions for this session")))

(defclass session-manager ()
  ((sessions :initform (map:make-map)
             :documentation "Map of session-id to evaluation-session")
   (max-sessions-per-client :initform 3)
   (session-idle-timeout :initform 300)
   (cleanup-thread :accessor cleanup-thread)))
```

### Session Lifecycle

1. **Create Session**: Client requests a new session with specific capabilities
2. **Evaluate in Session**: Sequential evaluations build on previous state
3. **Reset Session**: Clear all definitions but keep process alive
4. **Terminate Session**: Explicitly close or timeout after inactivity

### API Design

```lisp
;; Create a new session
{:method "epsilon/createEvaluationSession"
 :params {:name "my-repl"
          :modules ["epsilon.core" "epsilon.map"]
          :restrictions {:max-memory "512MB"
                        :allow-file-read true
                        :allow-file-write false
                        :allow-network false}}}

;; Response
{:session-id "sess-12345"
 :status "created"}

;; Evaluate in session
{:method "epsilon/evaluate"
 :params {:session-id "sess-12345"
          :code "(defparameter *counter* 0)"}}

;; Subsequent evaluation can reference previous state
{:method "epsilon/evaluate"
 :params {:session-id "sess-12345"
          :code "(incf *counter*)"}}
;; Returns 1

;; List active sessions
{:method "epsilon/listSessions"}

;; Terminate session
{:method "epsilon/terminateSession"
 :params {:session-id "sess-12345"}}
```

### Implementation Strategy

```lisp
(defmethod create-session ((manager session-manager) &key owner modules restrictions)
  "Create a new evaluation session"
  (let* ((process (launch-session-process modules restrictions))
         (session (make-instance 'evaluation-session
                                 :process process
                                 :owner owner
                                 :restrictions restrictions)))
    
    ;; Initialize the session
    (send-session-init session modules)
    
    ;; Register session
    (setf (map:get (sessions manager) (session-id session)) session)
    
    ;; Return session info
    session))

(defmethod evaluate-in-session ((manager session-manager) session-id code &key timeout)
  "Evaluate code in an existing session"
  (let ((session (map:get (sessions manager) session-id)))
    (unless session
      (error "Session not found: ~A" session-id))
    
    (when (eq (session-state session) :terminated)
      (error "Session terminated: ~A" session-id))
    
    ;; Update last-used timestamp
    (setf (last-used session) (get-universal-time))
    
    ;; Send evaluation request to session process
    (let ((request (make-session-request :code code :timeout timeout)))
      (write-to-process (slot-value session 'process) request)
      (read-session-response session timeout))))

(defmethod cleanup-idle-sessions ((manager session-manager))
  "Periodically clean up idle sessions"
  (let ((now (get-universal-time)))
    (map:each (lambda (id session)
                (when (> (- now (last-used session))
                         (slot-value session 'timeout))
                  (terminate-session manager id)))
              (sessions manager))))
```

### Safety Mechanisms

1. **Session Isolation**: Each session runs in its own process
2. **Resource Quotas**: Per-session memory/CPU limits
3. **Capability Model**: Sessions declare what access they need upfront
4. **Audit Trail**: All evaluations are logged with session context
5. **Automatic Cleanup**: Idle sessions are terminated

### Use Cases

1. **REPL Integration**: IDE terminal with persistent state
2. **Notebook Style**: Jupyter-like sequential evaluation
3. **Debugging Sessions**: Inspect and modify running state
4. **Teaching/Learning**: Step-through evaluation with state
5. **Testing**: Build up test scenarios incrementally

### Session Types

#### 1. Ephemeral Sessions (Default)
- Auto-created for single evaluation
- Terminated immediately after
- Maximum isolation

#### 2. Named Sessions
- Explicitly created and managed
- Persist across multiple evaluations
- Client must manage lifecycle

#### 3. Workspace Sessions
- One per workspace/project
- Automatically created on first use
- Shares project context

#### 4. Debug Sessions
- Created when debugging starts
- Has access to debugger APIs
- Enhanced introspection capabilities

### Best Practices

1. **Default to Isolation**: Use ephemeral sessions unless state is needed
2. **Explicit Sessions**: Require explicit session creation for stateful work
3. **Clear Naming**: Sessions should have descriptive names/purposes
4. **Resource Limits**: Always enforce resource limits, even for trusted code
5. **Session Hygiene**: Provide easy ways to reset/clear session state

## Future Enhancements

1. **Session Persistence**: Save/restore session state across restarts
2. **Session Sharing**: Multiple clients can share a session (with permissions)
3. **Remote Sessions**: Sessions can run on different machines
4. **Session Templates**: Pre-configured sessions for common tasks
5. **Time-Travel Debugging**: Record and replay session history
