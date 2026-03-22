;;;; epsilon.io - Unified IO package
;;;;
;;;; Re-exports all symbols from the IO sub-packages for convenience.
;;;; Users can import this single package to get all IO functionality.

(defpackage epsilon.io
  (:use :cl)
  (:require (epsilon.symbol sym)
            (epsilon.io.protocol proto)
            (epsilon.io.conditions cond)
            (epsilon.io.buffer buf)
            (epsilon.io.byte-stream bs)
            (epsilon.io.buffered buffered)
            (epsilon.io.combinators comb)
            (epsilon.io.async async)
            (epsilon.io.net-adapters net-adapters)
            (epsilon.io.cl-stream-adapters cl-adapters))
  (:enter t))

;;; Re-export protocol (excluding macros)
(sym:reexport :epsilon.io.protocol
          '(read-into read-byte* read-exact read-all
            write-from write-byte* write-all flush
            close* open-p seek* position* size*
            reader-p writer-p closer-p seeker-p))

;;; Protocol macros
(defmacro with-open ((var opener &rest args) &body body)
  `(proto:with-open (,var ,opener ,@args) ,@body))

(defmacro with-reader ((var reader) &body body)
  `(proto:with-reader (,var ,reader) ,@body))

(defmacro with-writer ((var writer) &body body)
  `(proto:with-writer (,var ,writer) ,@body))

;;; Re-export condition types (import directly to preserve type association)
(sym:reexport-types :epsilon.io.conditions
                '("IO-ERROR" "EOF-ERROR" "SHORT-READ-ERROR" "SHORT-WRITE-ERROR"
                  "CLOSED-ERROR" "WOULD-BLOCK-ERROR" "TIMEOUT-ERROR" "CANCELLED-ERROR"
                  "SEEK-ERROR" "NOT-SEEKABLE-ERROR" "LIMIT-EXCEEDED-ERROR"
                  "BUFFER-ERROR" "BUFFER-OVERFLOW-ERROR" "BUFFER-UNDERFLOW-ERROR"))

;;; Re-export condition accessors and functions
(sym:reexport :epsilon.io.conditions
          '(io-error-operation io-error-stream io-error-position io-error-cause
            short-read-error-expected short-read-error-actual
            short-write-error-expected short-write-error-actual
            timeout-error-duration
            seek-error-offset seek-error-whence
            limit-exceeded-error-limit limit-exceeded-error-attempted
            buffer-overflow-error-capacity buffer-overflow-error-required
            buffer-underflow-error-available buffer-underflow-error-requested
            signal-would-block))

;;; Re-export buffer struct types
(sym:reexport-types :epsilon.io.buffer
                '("BUF" "BUF-POOL"))

;;; Re-export buffer functions (excluding macros)
(sym:reexport :epsilon.io.buffer
          '(make-buf buf-p buf-data buf-capacity buf-position buf-limit
            buf-remaining buf-space buf-empty-p buf-full-p
            buf-from-bytes buf-wrap buf-get-byte buf-peek-byte buf-put-byte
            buf-get-bytes buf-put-bytes buf-flip buf-clear buf-compact buf-slice buf-copy
            make-buf-pool buf-pool-p buf-pool-acquire buf-pool-release))

;;; Buffer macros
(defmacro with-buf ((var init pool) &body body)
  `(buf:with-buf (,var ,init ,pool) ,@body))

;;; Re-export byte-stream struct types
(sym:reexport-types :epsilon.io.byte-stream
                '("BYTE-READER" "BYTE-WRITER" "NULL-READER" "NULL-WRITER"))

;;; Re-export byte-stream functions
(sym:reexport :epsilon.io.byte-stream
          '(make-byte-reader byte-reader-p
            make-byte-writer byte-writer-p
            byte-writer-bytes byte-writer-string byte-writer-reset byte-writer-ensure-capacity
            make-null-reader null-reader-p *null-reader*
            make-null-writer null-writer-p *null-writer*
            bytes-to-reader string-to-reader collect-bytes copy-stream))

;;; Re-export buffered struct types
(sym:reexport-types :epsilon.io.buffered
                '("BUFFERED-READER" "BUFFERED-WRITER"))

;;; Re-export buffered functions
(sym:reexport :epsilon.io.buffered
          '(make-buffered-reader buffered-reader-p
            buffered-reader-peek buffered-reader-skip
            buffered-reader-read-until buffered-reader-read-line
            make-buffered-writer buffered-writer-p
            buffered-writer-write-string))

;;; Re-export combinator struct types
(sym:reexport-types :epsilon.io.combinators
                '("LIMIT-READER" "LIMIT-WRITER" "TEE-READER" "MULTI-WRITER" "CHAIN-READER"
                  "COUNTING-READER" "COUNTING-WRITER" "TRANSFORM-READER" "OFFSET-READER"))

;;; Re-export combinator functions
(sym:reexport :epsilon.io.combinators
          '(make-limit-reader limit-reader-p limit-reader-exhausted-p
            make-limit-writer limit-writer-p
            make-tee-reader tee-reader-p
            make-multi-writer multi-writer-p
            make-chain-reader chain-reader-p
            make-counting-reader counting-reader-p counting-reader-count
            make-counting-writer counting-writer-p counting-writer-count
            make-transform-reader transform-reader-p
            make-offset-reader offset-reader-p
            limit tee chain broadcast counting transform skip-bytes))

;;; Re-export async struct types
(sym:reexport-types :epsilon.io.async
                '("TASK" "IO-CONTEXT"))

;;; Re-export async functions (excluding macros)
(sym:reexport :epsilon.io.async
          '(make-task task-p task-state task-result task-complete-p
            task-complete task-fail task-cancel
            make-io-context io-context-p io-context-closed-p io-context-running-p
            io-context-close io-context-add-task io-context-remove-task
            spawn await await-any await-all poll-once run-until-complete
            async-read async-write async-accept async-connect
            async-sleep async-timeout
            set-nonblocking))

;;; Async macros
(defmacro with-io-context ((var &rest args) &body body)
  `(async:with-io-context (,var ,@args) ,@body))

(defmacro async-let (bindings &body body)
  `(async:async-let ,bindings ,@body))

;;; Re-export net-adapters struct types
(sym:reexport-types :epsilon.io.net-adapters
                '("TCP-READER" "TCP-WRITER"))

;;; Re-export net-adapters functions
(sym:reexport :epsilon.io.net-adapters
          '(make-tcp-reader tcp-reader-p tcp-reader-stream
            make-tcp-writer tcp-writer-p tcp-writer-stream
            tcp-stream-to-reader tcp-stream-to-writer))

;;; Re-export CL stream adapters struct types
(sym:reexport-types :epsilon.io.cl-stream-adapters
                '("CL-STREAM-READER" "CL-STREAM-WRITER"))

;;; Re-export CL stream adapters functions
(sym:reexport :epsilon.io.cl-stream-adapters
          '(make-cl-stream-reader cl-stream-reader-p cl-stream-reader-stream
            make-cl-stream-writer cl-stream-writer-p cl-stream-writer-stream
            cl-stream-to-reader cl-stream-to-writer))
