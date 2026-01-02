# Epsilon IO Stream Architecture

A modern, composable IO system designed for clarity and efficiency.

## Design Principles

1. **Protocol-Based Composition** - Small, focused protocols that compose naturally
2. **Explicit Over Implicit** - No hidden buffering, encoding, or threading
3. **Zero-Copy Where Possible** - Buffer views and slices avoid copying
4. **Structured Concurrency** - Async operations have clear ownership and cancellation
5. **Backpressure by Default** - Flow control is built into the design
6. **Resource Safety** - RAII-style cleanup via `with-` macros

## Core Protocols

Inspired by Go's io package and Rust's std::io traits, but adapted for Lisp idioms.

### Reader Protocol

```lisp
;; The fundamental read operation
(defgeneric read-into (reader buffer &key start end)
  "Read bytes into BUFFER[start:end]. Returns bytes read, 0 on EOF, or signals error.
   This is the ONLY method that must be implemented.")

;; Derived operations (have default implementations)
(defgeneric read-byte (reader)
  "Read a single byte. Returns byte or NIL on EOF.")

(defgeneric read-exact (reader buffer &key start end)
  "Read exactly (- end start) bytes or signal short-read-error.")

(defgeneric read-all (reader &key max-size)
  "Read until EOF into a fresh buffer.")
```

### Writer Protocol

```lisp
;; The fundamental write operation
(defgeneric write-from (writer buffer &key start end)
  "Write BUFFER[start:end] to writer. Returns bytes written.
   This is the ONLY method that must be implemented.")

;; Derived operations
(defgeneric write-byte (writer byte)
  "Write a single byte.")

(defgeneric write-all (writer buffer &key start end)
  "Write all bytes or signal error.")

(defgeneric flush (writer)
  "Ensure all buffered data is written. Default: no-op.")
```

### Additional Protocols

```lisp
;; Resource management
(defgeneric close (closable)
  "Release resources. Idempotent.")

;; Positioning (optional)
(defgeneric seek (seeker offset whence)
  "Set position. WHENCE is :start, :current, or :end. Returns new position.")

(defgeneric position (positionable)
  "Return current position.")

(defgeneric size (sizable)
  "Return total size if known, NIL otherwise.")
```

## Buffer Design

### Buffer Structure

```lisp
(defstruct buffer
  "A mutable view into a byte array with position tracking."
  (data nil :type (simple-array u8 (*)))  ; backing storage
  (start 0 :type fixnum)                   ; view start
  (end 0 :type fixnum)                     ; view end (exclusive)
  (position 0 :type fixnum)                ; read cursor
  (capacity 0 :type fixnum))               ; data length

;; Key invariants:
;; 0 <= start <= position <= end <= capacity
;; Readable bytes: [position, end)
;; Writable space: [end, capacity) or [position, capacity) after compact
```

### Buffer Operations

```lisp
;; Reading from buffer
(buffer-remaining buf)          ; => (- end position)
(buffer-get-byte buf)           ; read one byte, advance position
(buffer-get-bytes buf n)        ; read n bytes as new array
(buffer-peek-byte buf)          ; read without advancing

;; Writing to buffer
(buffer-space buf)              ; => (- capacity end)
(buffer-put-byte buf byte)      ; write one byte, advance end
(buffer-put-bytes buf data)     ; write bytes, advance end

;; Lifecycle
(buffer-compact buf)            ; move unread data to start
(buffer-clear buf)              ; reset position and end to 0
(buffer-flip buf)               ; prepare for reading after writing

;; Views (zero-copy)
(buffer-slice buf start end)    ; new buffer sharing same data
```

### Buffer Pool

```lisp
(defvar *buffer-pool* (make-buffer-pool :default-size 8192))

(with-buffer (buf &optional size)
  "Acquire buffer from pool, release on exit (normal or error)."
  body...)

(acquire-buffer pool &optional size)  ; manual acquire
(release-buffer pool buffer)          ; manual release
```

## Buffered Wrappers

### Buffered Reader

Provides efficient byte-at-a-time reading from an underlying source.

```lisp
(defstruct buffered-reader
  source      ; underlying reader
  buffer      ; internal buffer
  eof-p)      ; EOF flag

;; All reader protocol methods, plus:
(buffered-reader-read-line reader encoding)  ; => string
(buffered-reader-read-until reader delimiter) ; => bytes
(buffered-reader-peek reader n)              ; peek n bytes without consuming
(buffered-reader-skip reader n)              ; skip n bytes efficiently
```

### Buffered Writer

Reduces syscalls by batching writes.

```lisp
(defstruct buffered-writer
  sink        ; underlying writer
  buffer)     ; internal buffer

;; All writer protocol methods, with automatic batching
;; flush is meaningful - writes buffered data to sink
```

## Stream Combinators

### Limiting

```lisp
(limit-reader reader n)
  "Returns reader that yields at most N bytes, then EOF."

(limit-writer writer n)
  "Returns writer that accepts at most N bytes, then signals limit-exceeded."
```

### Teeing

```lisp
(tee-reader reader writer)
  "Returns reader that writes everything read to WRITER."

(multi-writer &rest writers)
  "Returns writer that writes to all WRITERS."
```

### Chaining

```lisp
(chain-readers &rest readers)
  "Returns reader that reads from each in sequence."

(pipe-reader reader transformer)
  "Returns reader that applies TRANSFORMER to bytes as they pass through."
```

### Counting

```lisp
(counting-reader reader)
  "Returns (values wrapped-reader count-fn) where (count-fn) returns bytes read."

(counting-writer writer)
  "Returns (values wrapped-writer count-fn) where (count-fn) returns bytes written."
```

## Async IO Model

### Design: Completion-Based with Structured Concurrency

Unlike callback spaghetti, we use a structured model inspired by:
- Rust's async/await with explicit runtimes
- Go's goroutines with channels
- Trio/anyio's structured concurrency

```lisp
;; Async context owns all operations
(with-io-context (ctx)
  ;; Spawn concurrent operations
  (let ((task1 (spawn ctx (async-read fd1 buf1)))
        (task2 (spawn ctx (async-read fd2 buf2))))
    ;; Wait for completion
    (await task1)
    (await task2)))
;; All tasks guaranteed complete or cancelled when context exits
```

### IO Context

```lisp
(defstruct io-context
  backend        ; platform-specific (kqueue/epoll/iocp)
  tasks          ; active task set
  completions    ; completion queue
  closed-p)      ; shutdown flag

(defgeneric context-poll (ctx timeout-ms)
  "Poll for completions. Returns list of completed tasks.")

(defgeneric context-spawn (ctx operation)
  "Submit operation, return task handle.")

(defgeneric context-cancel (ctx task)
  "Request cancellation of task.")
```

### Task Handle

```lisp
(defstruct task
  id
  operation      ; what we're doing
  state          ; :pending, :running, :completed, :cancelled, :failed
  result         ; completion value or error
  context)       ; owning context

(defgeneric task-await (task &optional timeout)
  "Block until task completes. Returns result or signals error.")

(defgeneric task-poll (task)
  "Non-blocking check. Returns T if complete.")
```

### Async Operations

```lisp
;; File/socket operations return tasks
(async-read ctx fd buffer &key offset count)
(async-write ctx fd buffer &key offset count)
(async-accept ctx listener-fd)
(async-connect ctx fd address)

;; Utilities
(async-timeout ctx duration-ms)
(async-race ctx &rest tasks)      ; first to complete
(async-all ctx &rest tasks)       ; all must complete
```

### Platform Abstraction

```lisp
;; Platform backends implement:
(defgeneric backend-create ()
  "Create platform-specific backend.")

(defgeneric backend-destroy (backend)
  "Cleanup backend resources.")

(defgeneric backend-register (backend fd events)
  "Register fd for events (:read, :write).")

(defgeneric backend-unregister (backend fd)
  "Remove fd from backend.")

(defgeneric backend-wait (backend timeout-ms max-events)
  "Wait for events. Returns list of (fd . event-type).")
```

## Error Handling

### Error Hierarchy

```lisp
io-error                          ; base class
  eof-error                       ; unexpected EOF
  short-read-error                ; read-exact got fewer bytes
  short-write-error               ; write-all couldn't complete
  would-block-error               ; non-blocking op would block
  closed-error                    ; operation on closed stream
  timeout-error                   ; async operation timed out
  cancelled-error                 ; task was cancelled
```

### Error Context

```lisp
(define-condition io-error (error)
  ((operation :initarg :operation :reader io-error-operation)
   (stream :initarg :stream :reader io-error-stream)
   (position :initarg :position :reader io-error-position)
   (cause :initarg :cause :reader io-error-cause)))
```

## Encoding Support

Encoding is NOT part of the stream protocol. It's handled by explicit wrappers:

```lisp
(defstruct text-reader
  source        ; buffered-reader
  encoding      ; :utf-8, :ascii, :latin-1, etc.
  decoder)      ; stateful decoder for streaming

(text-reader-read-char reader)     ; => character
(text-reader-read-line reader)     ; => string
(text-reader-read-string reader n) ; => string

(defstruct text-writer
  sink          ; buffered-writer
  encoding
  encoder)

(text-writer-write-char writer char)
(text-writer-write-string writer string)
(text-writer-write-line writer string)
```

## Standard Streams

```lisp
;; File streams
(open-file path &key direction if-exists if-does-not-exist)
  ; direction: :input, :output, :io
  ; Returns appropriate reader/writer/both

;; Memory streams
(make-byte-reader bytes)           ; read from byte vector
(make-byte-writer &key initial-size) ; write to growing buffer
(byte-writer-bytes writer)         ; extract written bytes

;; Null streams
*null-reader*                      ; always EOF
*null-writer*                      ; discards all writes

;; Standard IO (when available)
*stdin*                            ; reader for stdin
*stdout*                           ; writer for stdout
*stderr*                           ; writer for stderr
```

## Implementation Strategy

### Phase 1: Core Protocols
- Protocol definitions via CLOS generic functions
- Basic implementations: byte-reader, byte-writer
- Buffer and buffer-pool

### Phase 2: Buffered Wrappers
- buffered-reader with all convenience methods
- buffered-writer with flush semantics
- text-reader, text-writer

### Phase 3: Combinators
- limit, tee, chain, count wrappers
- Verification that composition works correctly

### Phase 4: Async Runtime
- io-context and task abstractions
- Platform backends (kqueue, epoll, iocp)
- Integration with sync wrappers

### Phase 5: Standard Streams
- File operations
- Standard IO (stdin/stdout/stderr)
- Memory streams

## Comparison with Prior Art

| Feature | Go | Rust | Epsilon (New) |
|---------|-----|------|---------------|
| Core trait | io.Reader/Writer | Read/Write traits | read-into/write-from |
| Buffering | bufio.Reader | BufReader | buffered-reader |
| Zero-copy | io.WriterTo | Cursor, Bytes | buffer-slice |
| Async model | goroutines | async/await + runtime | spawn/await + context |
| Cancellation | context.Context | Drop + AbortHandle | context scoping |
| Backpressure | channel blocking | Stream trait | explicit await |

## Code Organization

```
epsilon/modules/io/
  src/
    package.lisp           ; exports
    protocol.lisp          ; generic function definitions
    conditions.lisp        ; error hierarchy
    buffer.lisp            ; buffer and buffer-pool
    byte-stream.lisp       ; byte-reader, byte-writer
    buffered.lisp          ; buffered-reader, buffered-writer
    text.lisp              ; text-reader, text-writer
    combinators.lisp       ; limit, tee, chain, etc.
    file.lisp              ; file operations
    async/
      context.lisp         ; io-context, task
      operations.lisp      ; async-read, async-write, etc.
  tests/
    protocol-tests.lisp
    buffer-tests.lisp
    buffered-tests.lisp
    combinator-tests.lisp
    async-tests.lisp
  module.lisp
```

## Future Considerations

1. **Vectored IO** - readv/writev for scatter-gather
2. **Memory-mapped files** - mmap integration
3. **Zero-copy networking** - sendfile, splice
4. **Compression streams** - gzip, zstd wrappers
5. **TLS streams** - encryption layer
6. **Metrics** - bytes transferred, operation counts
