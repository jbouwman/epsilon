# LSP Evaluation System Usage Guide

## Overview

The Epsilon LSP server provides a robust code evaluation system that allows IDE clients to execute Lisp code in isolated subprocesses. This enables REPL-like functionality while maintaining the stability and security of the LSP server.

## Key Features

- **Subprocess Isolation**: All code runs in separate processes
- **Session Management**: Stateful sessions for REPL-style interaction
- **Security Controls**: Resource limits and capability restrictions
- **Multiple Session Types**: Ephemeral, named, and workspace sessions
- **Async Evaluation**: Non-blocking code execution

## Client Integration

### Creating a Session

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "epsilon/evaluation/createSession",
  "params": {
    "name": "my-repl",
    "modules": ["epsilon.core", "epsilon.lib.map"],
    "restrictions": {
      "maxMemory": "512M",
      "maxCpuTime": 30,
      "allowFileRead": true,
      "allowFileWrite": false,
      "allowNetwork": false
    }
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "sessionId": "550e8400-e29b-41d4-a716-446655440000",
    "name": "my-repl",
    "state": "active",
    "modules": ["epsilon.core", "epsilon.lib.map"],
    "createdAt": 3909876543
  }
}
```

### Evaluating Code

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "epsilon/evaluation/evaluate",
  "params": {
    "sessionId": "550e8400-e29b-41d4-a716-446655440000",
    "code": "(defparameter *counter* 0)",
    "timeout": 10
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "value": "*COUNTER*",
    "output": "",
    "duration": 0.002
  }
}
```

### Ephemeral Evaluation

For one-off evaluations without creating a session:

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "epsilon/evaluation/evaluate",
  "params": {
    "code": "(+ 1 2 3)",
    "modules": ["epsilon.core"]
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "value": "6",
    "output": "",
    "duration": 0.001
  }
}
```

### Listing Sessions

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "epsilon/evaluation/listSessions",
  "params": {}
}

// Response
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": [
    {
      "sessionId": "550e8400-e29b-41d4-a716-446655440000",
      "name": "my-repl",
      "state": "active",
      "owner": "workspace-1",
      "lastUsed": 3909876600
    }
  ]
}
```

### Terminating a Session

```json
// Request
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "epsilon/evaluation/terminateSession",
  "params": {
    "sessionId": "550e8400-e29b-41d4-a716-446655440000"
  }
}

// Response
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "status": "terminated"
  }
}
```

## IDE Implementation Examples

### VS Code Extension

```typescript
// Create a REPL session
async function createREPLSession() {
  const result = await client.sendRequest('epsilon/evaluation/createSession', {
    name: 'REPL',
    modules: ['epsilon.core'],
    restrictions: {
      maxMemory: '256M',
      allowFileRead: true
    }
  });
  
  return result.sessionId;
}

// Evaluate selection
async function evaluateSelection() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;
  
  const selection = editor.selection;
  const code = editor.document.getText(selection);
  
  const result = await client.sendRequest('epsilon/evaluation/evaluate', {
    sessionId: currentSessionId,
    code: code
  });
  
  outputChannel.appendLine(`=> ${result.value}`);
  if (result.output) {
    outputChannel.appendLine(result.output);
  }
}
```

### Emacs LSP Client

```elisp
;; Create evaluation session
(defun epsilon-create-repl-session ()
  (interactive)
  (let ((result (lsp-request "epsilon/evaluation/createSession"
                             `(:name "Emacs REPL"
                               :modules ["epsilon.core"]))))
    (setq epsilon-session-id (gethash "sessionId" result))
    (message "Created session: %s" epsilon-session-id)))

;; Evaluate region
(defun epsilon-eval-region (start end)
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (result (lsp-request "epsilon/evaluation/evaluate"
                              `(:sessionId ,epsilon-session-id
                                :code ,code))))
    (epsilon-show-result result)))
```

## Best Practices

1. **Session Lifecycle**: Always terminate sessions when done
2. **Error Handling**: Wrap evaluations in try-catch blocks
3. **Resource Limits**: Set appropriate memory/CPU limits
4. **Security**: Use minimal permissions needed
5. **Timeout**: Set reasonable timeouts for evaluations

## Security Considerations

The evaluation system enforces several security measures:

- Process isolation prevents access to LSP server internals
- Resource limits prevent DoS attacks
- Capability-based permissions control file/network access
- Automatic cleanup of idle sessions
- Audit logging of all evaluations

## Performance Tips

1. Reuse sessions for related evaluations
2. Use ephemeral evaluation for one-off code
3. Set appropriate timeouts
4. Monitor session count and cleanup regularly
5. Pre-load commonly used modules in sessions