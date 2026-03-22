;;;; io.lisp - Multiplexed I/O for subprocess streams
;;;;
;;;; Synchronous monitoring of subprocess stdout/stderr and process exit
;;;; using epsilon.async:process-monitor, which provides a platform-native
;;;; kqueue (Darwin) or epoll (Linux) backend without direct dependencies
;;;; on either.

(defpackage epsilon.process.io
  (:use :cl)
  (:local-nicknames
   (posix epsilon.process.posix)
   (spawn epsilon.process.spawn)
   (lib   epsilon.foreign)
   (async epsilon.async))
  (:export
   ;; Line buffer
   #:line-buffer #:make-line-buffer #:line-buffer-feed
   #:line-buffer-flush #:line-buffer-drain-fd

   ;; Primary API
   #:monitor-handle
   #:collect-output

   ;; Condition
   #:monitor-timeout)
  (:enter t))

;;; ============================================================================
;;; Condition
;;; ============================================================================

(define-condition monitor-timeout (condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Monitor timeout reached"))))

;;; ============================================================================
;;; Line Buffer
;;; ============================================================================

(defstruct (line-buffer (:constructor %make-line-buffer))
  "Accumulates bytes from non-blocking fd reads, emits complete lines."
  (fd       -1  :type integer)
  (buf      nil :type (or null (vector (unsigned-byte 8))))
  (tag      nil)
  (closed-p nil :type boolean))

(defun make-line-buffer (&key (fd -1) tag)
  "Create a new line buffer."
  (%make-line-buffer
   :fd fd
   :buf (make-array 0 :element-type '(unsigned-byte 8)
                      :adjustable t :fill-pointer 0)
   :tag tag))

(defun line-buffer-feed (lb octets callback)
  "Feed OCTETS (a byte vector) into line buffer LB. Call CALLBACK with each
   complete line (as a string). Partial lines are accumulated."
  (let ((buf (line-buffer-buf lb))
        (newline (char-code #\Newline)))
    (loop with start = 0
          for i from 0 below (length octets)
          when (= (aref octets i) newline)
            do (let* ((line-bytes (+ (length buf) (- i start)))
                      (combined (make-array line-bytes
                                            :element-type '(unsigned-byte 8))))
                 ;; Copy accumulated partial data
                 (when (> (length buf) 0)
                   (replace combined buf))
                 ;; Copy new data up to newline
                 (replace combined octets
                          :start1 (length buf)
                          :start2 start :end2 i)
                 ;; Emit line
                 (funcall callback
                          (sb-ext:octets-to-string combined
                                                   :external-format :utf-8))
                 ;; Reset buffer
                 (setf (fill-pointer buf) 0)
                 (setf start (1+ i)))
          finally
             ;; Accumulate remaining partial data
             (when (< start (length octets))
               (let ((old-len (length buf))
                     (new-len (- (length octets) start)))
                 (adjust-array buf (+ old-len new-len)
                               :fill-pointer (+ old-len new-len))
                 (replace buf octets
                          :start1 old-len
                          :start2 start))))))

(defun line-buffer-flush (lb callback)
  "Emit any remaining data in the buffer as a final line at EOF."
  (let ((buf (line-buffer-buf lb)))
    (when (> (length buf) 0)
      (funcall callback
               (sb-ext:octets-to-string
                (subseq buf 0 (length buf))
                :external-format :utf-8))
      (setf (fill-pointer buf) 0))))

(defun line-buffer-drain-fd (lb callback)
  "Non-blocking read loop on LB's fd, feeding data into the line buffer.
   Returns :EOF when fd is closed, :WOULD-BLOCK when no more data available."
  (let ((fd (line-buffer-fd lb)))
    (lib:with-foreign-memory ((read-buf 4096))
      (loop
        (let ((n (posix:fd-read fd read-buf 4096)))
          (cond
            ((= n 0)
             (setf (line-buffer-closed-p lb) t)
             (return :eof))
            ((< n 0)
             (let ((errno (sb-alien:get-errno)))
               (if (= errno async:+eagain+)
                   (return :would-block)
                   (error "read(~D) failed: errno ~D" fd errno))))
            (t
             (let ((octets (make-array n :element-type '(unsigned-byte 8))))
               (dotimes (i n)
                 (setf (aref octets i) (sb-sys:sap-ref-8 read-buf i)))
               (line-buffer-feed lb octets callback)))))))))

;;; ============================================================================
;;; I/O Multiplexer (thin wrapper over async:process-monitor)
;;; ============================================================================

(defstruct (io-mux (:constructor %make-io-mux))
  "Tracks fd-to-line-buffer associations alongside the platform process-monitor."
  monitor
  fd-map
  (pid-registered nil :type boolean))

(defun %mux-create ()
  (%make-io-mux
   :monitor (async:make-process-monitor)
   :fd-map  (make-hash-table :test 'eql)))

(defun %mux-close (mux)
  (async:process-monitor-close (io-mux-monitor mux)))

(defun %mux-add-read (mux fd line-buf)
  (setf (gethash fd (io-mux-fd-map mux)) line-buf)
  (async:process-monitor-add-read (io-mux-monitor mux) fd))

(defun %mux-remove (mux fd)
  (remhash fd (io-mux-fd-map mux))
  (async:process-monitor-remove-read (io-mux-monitor mux) fd))

(defun %mux-add-pid-exit (mux handle)
  (when (async:process-monitor-add-pid
         (io-mux-monitor mux)
         (spawn:process-handle-pid handle)
         (spawn:process-handle-pidfd handle))
    (setf (io-mux-pid-registered mux) t)))

(defun %mux-wait (mux timeout-ms)
  (async:process-monitor-wait (io-mux-monitor mux) timeout-ms))

;;; ============================================================================
;;; Event Loop
;;; ============================================================================

(defun monitor-handle (handle &key on-stdout on-stderr on-exit timeout)
  "Monitor a process handle with multiplexed I/O and callbacks.
   ON-STDOUT/ON-STDERR: (lambda (line)) for each complete line.
   ON-EXIT: (lambda (exit-code signal)) when process exits.
   TIMEOUT: seconds (nil = indefinite). Does NOT kill on timeout.
   Returns the handle (status updated)."
  (let* ((mux         (%mux-create))
         (stdout-fd   (spawn:process-handle-stdout-fd handle))
         (stderr-fd   (spawn:process-handle-stderr-fd handle))
         (stdout-lb   nil)
         (stderr-lb   nil)
         (fds-open    0)
         (process-reaped nil)
         (deadline    (when timeout
                        (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second)))))
    (unwind-protect
         (progn
           ;; Set up stdout monitoring
           (when (and stdout-fd on-stdout)
             (async:set-nonblocking stdout-fd)
             (setf stdout-lb (make-line-buffer :fd stdout-fd :tag :stdout))
             (%mux-add-read mux stdout-fd stdout-lb)
             (incf fds-open))

           ;; Set up stderr monitoring
           (when (and stderr-fd on-stderr)
             (async:set-nonblocking stderr-fd)
             (setf stderr-lb (make-line-buffer :fd stderr-fd :tag :stderr))
             (%mux-add-read mux stderr-fd stderr-lb)
             (incf fds-open))

           ;; Register for process exit notification
           (%mux-add-pid-exit mux handle)

           ;; Immediately check if process has already exited -- catches
           ;; the race where child exits before EVFILT_PROC registration.
           ;; With no background reaper, the zombie persists and waitpid
           ;; will see it.
           (spawn:handle-poll handle)
           (when (spawn:handle-exited-p handle)
             (setf process-reaped t))

           ;; If nothing to monitor, fall back to a plain wait
           (when (and (= fds-open 0) (not (io-mux-pid-registered mux))
                      (not process-reaped))
             (spawn:wait-for-handle handle timeout)
             (when (and on-exit (spawn:handle-exited-p handle))
               (funcall on-exit
                        (spawn:process-handle-exit-code handle)
                        (spawn:process-handle-term-signal handle)))
             (return-from monitor-handle handle))

           ;; Main event loop
           (loop
             (let* ((remaining-ms
                      (if deadline
                          (let ((ticks-left (- deadline (get-internal-real-time))))
                            (if (<= ticks-left 0)
                                (return)
                                (max 1 (truncate
                                        (* 1000 ticks-left)
                                        internal-time-units-per-second))))
                          ;; No deadline: poll at 100ms to enable periodic
                          ;; waitpid checks as a safety net for missed
                          ;; EVFILT_PROC / pidfd events
                          100))
                    (events (%mux-wait mux remaining-ms)))

               ;; Dispatch events
               (dolist (event events)
                 (let ((fd   (car event))
                       (type (cdr event)))
                   (case type
                     (:pid-exit
                      (setf process-reaped t))
                     ((:readable :eof)
                      (let ((lb (gethash fd (io-mux-fd-map mux))))
                        (when lb
                          (let* ((cb     (if (eq (line-buffer-tag lb) :stdout)
                                            on-stdout on-stderr))
                                 (status (line-buffer-drain-fd lb cb)))
                            (when (eq status :eof)
                              (line-buffer-flush lb cb)
                              (%mux-remove mux fd)
                              (decf fds-open)))))))))

               ;; Periodic waitpid safety net -- catches the race where
               ;; child exits before kqueue EVFILT_PROC registration,
               ;; missed pidfd events, and any other anomalies.
               (unless process-reaped
                 (spawn:handle-poll handle)
                 (when (spawn:handle-exited-p handle)
                   (setf process-reaped t)))

               ;; Terminate when all fds closed and process reaped
               (when (and (<= fds-open 0) process-reaped)
                 (return))))

           ;; Reap process if not yet done
           (unless (spawn:handle-exited-p handle)
             (spawn:handle-poll handle)
             (unless (spawn:handle-exited-p handle)
               (spawn:wait-for-handle handle 1)))

           ;; Call on-exit callback
           (when (and on-exit (spawn:handle-exited-p handle))
             (funcall on-exit
                      (spawn:process-handle-exit-code handle)
                      (spawn:process-handle-term-signal handle))))

      ;; Cleanup
      (%mux-close mux))
    handle))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun collect-output (handle &key timeout)
  "Collect all stdout and stderr from handle using multiplexed I/O.
   Returns (values stdout-string stderr-string exit-code)."
  (let ((stdout-lines nil)
        (stderr-lines nil)
        (exit-code    nil))
    (monitor-handle handle
                    :on-stdout (lambda (line) (push line stdout-lines))
                    :on-stderr (lambda (line) (push line stderr-lines))
                    :on-exit   (lambda (code sig)
                                 (declare (ignore sig))
                                 (setf exit-code code))
                    :timeout timeout)
    (values (if stdout-lines
                (format nil "~{~A~%~}" (nreverse stdout-lines))
                "")
            (if stderr-lines
                (format nil "~{~A~%~}" (nreverse stderr-lines))
                "")
            (or exit-code (spawn:process-handle-exit-code handle)))))
