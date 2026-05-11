;;;; epsilon.io.net-adapters - TCP stream adapters for epsilon.io protocols
;;;;
;;;; Provides Reader/Writer protocol implementations for TCP streams.
;;;; These adapters wrap epsilon.net tcp-stream objects to enable
;;;; composition with epsilon.io abstractions (buffered readers, etc.)

(defpackage epsilon.io.net-adapters
  (:use :cl)
  (:import (epsilon.io.protocol proto)
            (epsilon.net net)
            (epsilon.typeclass tc)
            (epsilon.sys.semaphore sem)
            (epsilon.scheduler.coroutine coro)
            (epsilon.scheduler.io-wait io-wait)))

;;; ============================================================================
;;; TCP Reader - wraps tcp-stream for reading
;;; ============================================================================

(defstruct (tcp-reader (:constructor %make-tcp-reader))
  "Reader that reads from a TCP stream.

   Implements: Reader, Closer protocols.

   Note: The tcp-stream is shared; closing the reader closes the
   underlying stream."
  (stream nil)  ; tcp-stream object
  (closed-p nil :type boolean))

(defun make-tcp-reader (tcp-stream)
  "Create a tcp-reader wrapping TCP-STREAM.

   TCP-STREAM must be an epsilon.net tcp-stream object.
   The reader will delegate read operations to net:tcp-read."
  (%make-tcp-reader :stream tcp-stream))

(tc:definstance proto:closer tcp-reader
  (proto:close* (reader)
    "Close the TCP reader (and underlying stream)."
    (if (tcp-reader-closed-p reader)
        nil
        (progn
          (setf (tcp-reader-closed-p reader) t)
          (when (tcp-reader-stream reader)
            (net:tcp-close (tcp-reader-stream reader)))
          t)))
  (proto:open-p (reader)
    (not (tcp-reader-closed-p reader))))

(tc:definstance proto:reader tcp-reader
  (proto:read-into (reader buffer &key (start 0) (end (length buffer)))
    "Read bytes from the TCP stream into BUFFER.
     Blocks until data is available or connection is closed.

     When called from inside an epsilon.scheduler coroutine, parks the
     fiber on fd readiness via io-wait:park-on-fd, so the carrier
     thread returns to its loop and can run other coroutines while
     the wait is in flight. When called from a non-coroutine thread
     (REPL, tests, ad-hoc code), falls back to the legacy
     poll-and-semaphore path which blocks the calling thread.

     IMPL-398 stage 5 -- before this change, the :pending branch
     called sem:wait-on-semaphore with a 5-second timeout, blocking
     the carrier OS thread for the full timeout. Under HTTP keepalive
     idle-wait this pinned the carrier for the entire idle window,
     which the watchdog flagged as carrier-stall. Parking on the fd
     instead lets the carrier iterate freely."
    (when (tcp-reader-closed-p reader)
      (error "TCP reader is closed"))
    (when (null (tcp-reader-stream reader))
      (return-from proto:read-into 0))
    (let* ((stream (tcp-reader-stream reader)))
      (cond
        ;; Fiber-aware path: we're inside a coroutine AND the platform
        ;; reactor that backs park-on-fd is registered. Use the
        ;; existing tcp-poll-read for the readiness check (it does the
        ;; MSG_PEEK), but for the wait we park through the scheduler.
        ((and coro:*current-coroutine* io-wait:*fd-wait-register*)
         (loop
           (let* ((stale-waker (lambda () nil))
                  (result (net:tcp-poll-read stream stale-waker))
                  (fd (handler-case (net:tcp-stream-handle stream)
                        (error () nil))))
             (ecase result
               (:ready
                (let ((n (net:tcp-read stream buffer :start start :end end)))
                  (when (zerop n)
                    (setf (tcp-reader-closed-p reader) t))
                  (return n)))
               (:pending
                (cond
                  (fd
                   (handler-case
                       (io-wait:park-on-fd fd '(:in) :timeout 5)
                     (error () nil)))
                  (t
                   ;; Couldn't get an fd; degrade to a short coroutine
                   ;; sleep so we yield and try again.
                   (epsilon.scheduler:coroutine-sleep 0.05)))
                (unless (net:tcp-connected-p stream)
                  (setf (tcp-reader-closed-p reader) t)
                  (return 0)))))))
        ;; Non-coroutine fallback: legacy semaphore-wait path. Reached
        ;; only from tests / REPL; production carriers always run
        ;; through the fiber-aware branch above.
        (t
         (loop
           (let* ((s (sem:make-semaphore :name "poll-read" :count 0))
                  (result (net:tcp-poll-read stream
                                             (lambda ()
                                               (sem:signal-semaphore s)))))
             (ecase result
               (:ready
                (let ((n (net:tcp-read stream buffer :start start :end end)))
                  (when (zerop n)
                    (setf (tcp-reader-closed-p reader) t))
                  (return n)))
               (:pending
                (sem:wait-on-semaphore s :timeout 5)
                (unless (net:tcp-connected-p stream)
                  (setf (tcp-reader-closed-p reader) t)
                  (return 0)))))))))))

;;; ============================================================================
;;; TCP Writer - wraps tcp-stream for writing
;;; ============================================================================

(defstruct (tcp-writer (:constructor %make-tcp-writer))
  "Writer that writes to a TCP stream.

   Implements: Writer, Closer protocols.

   Note: The tcp-stream is shared; closing the writer closes the
   underlying stream."
  (stream nil)  ; tcp-stream object
  (closed-p nil :type boolean))

(defun make-tcp-writer (tcp-stream)
  "Create a tcp-writer wrapping TCP-STREAM.

   TCP-STREAM must be an epsilon.net tcp-stream object.
   The writer will delegate write operations to net:tcp-write."
  (%make-tcp-writer :stream tcp-stream))

(tc:definstance proto:closer tcp-writer
  (proto:close* (writer)
    "Close the TCP writer (and underlying stream)."
    (if (tcp-writer-closed-p writer)
        nil
        (progn
          (setf (tcp-writer-closed-p writer) t)
          (when (tcp-writer-stream writer)
            (net:tcp-close (tcp-writer-stream writer)))
          t)))
  (proto:open-p (writer)
    (not (tcp-writer-closed-p writer))))

(tc:definstance proto:writer tcp-writer
  (proto:write-from (writer buffer &key (start 0) (end (length buffer)))
    "Write bytes from BUFFER to the TCP stream."
    (when (tcp-writer-closed-p writer)
      (error "TCP writer is closed"))
    (when (null (tcp-writer-stream writer))
      (return-from proto:write-from 0))
    (net:tcp-write (tcp-writer-stream writer) buffer :start start :end end))
  (proto:flush (writer)
    "Flush any buffered data to the network."
    (when (tcp-writer-stream writer)
      (net:tcp-flush (tcp-writer-stream writer)))
    writer))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun tcp-stream-to-reader (tcp-stream)
  "Create a Reader from a TCP stream.
   Shorthand for make-tcp-reader."
  (make-tcp-reader tcp-stream))

(defun tcp-stream-to-writer (tcp-stream)
  "Create a Writer from a TCP stream.
   Shorthand for make-tcp-writer."
  (make-tcp-writer tcp-stream))
