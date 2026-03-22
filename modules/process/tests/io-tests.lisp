;;;; io-tests.lisp - Tests for epsilon.process.io
;;;;
;;;; Tests line buffer, monitor-handle with multiplexed I/O (epoll/kqueue),
;;;; and collect-output convenience function.

(defpackage epsilon.process.io-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process.io io)
            (epsilon.process.spawn spawn)
            (epsilon.process.posix posix))
  (:enter t))

;;; ============================================================================
;;; Line Buffer Unit Tests
;;; ============================================================================

(deftest test-line-buffer-single-line ()
  "Feed a single complete line, expect one callback"
  (let ((lb (io:make-line-buffer :tag :stdout))
        (lines nil))
    (io:line-buffer-feed lb
                         (sb-ext:string-to-octets "hello
" :external-format :utf-8)
                         (lambda (line) (push line lines)))
    (assert-equal 1 (length lines))
    (assert-equal "hello" (first lines))))

(deftest test-line-buffer-multiple-lines ()
  "Feed multiple lines at once, expect 3 callbacks"
  (let ((lb (io:make-line-buffer :tag :stdout))
        (lines nil))
    (io:line-buffer-feed lb
                         (sb-ext:string-to-octets (format nil "a~%b~%c~%")
                                                  :external-format :utf-8)
                         (lambda (line) (push line lines)))
    (setf lines (nreverse lines))
    (assert-equal 3 (length lines))
    (assert-equal "a" (first lines))
    (assert-equal "b" (second lines))
    (assert-equal "c" (third lines))))

(deftest test-line-buffer-partial-accumulation ()
  "Feed partial data, then rest with newline, expect one callback"
  (let ((lb (io:make-line-buffer :tag :stdout))
        (lines nil))
    (let ((cb (lambda (line) (push line lines))))
      (io:line-buffer-feed lb
                           (sb-ext:string-to-octets "hel" :external-format :utf-8)
                           cb)
      (assert-equal 0 (length lines))
      (io:line-buffer-feed lb
                           (sb-ext:string-to-octets (format nil "lo~%")
                                                    :external-format :utf-8)
                           cb)
      (assert-equal 1 (length lines))
      (assert-equal "hello" (first lines)))))

(deftest test-line-buffer-flush-partial ()
  "Feed data without newline, flush at EOF, expect callback"
  (let ((lb (io:make-line-buffer :tag :stdout))
        (lines nil))
    (let ((cb (lambda (line) (push line lines))))
      (io:line-buffer-feed lb
                           (sb-ext:string-to-octets "no-newline" :external-format :utf-8)
                           cb)
      (assert-equal 0 (length lines))
      (io:line-buffer-flush lb cb)
      (assert-equal 1 (length lines))
      (assert-equal "no-newline" (first lines)))))

(deftest test-line-buffer-empty-lines ()
  "Feed two empty lines, expect 2 empty-string callbacks"
  (let ((lb (io:make-line-buffer :tag :stdout))
        (lines nil))
    (io:line-buffer-feed lb
                         (sb-ext:string-to-octets (format nil "~%~%")
                                                  :external-format :utf-8)
                         (lambda (line) (push line lines)))
    (assert-equal 2 (length lines))
    (assert-equal "" (first lines))
    (assert-equal "" (second lines))))

;;; ============================================================================
;;; Monitor Integration Tests
;;; ============================================================================

(deftest test-monitor-stdout-lines ()
  "Echo 3 lines, verify all received in order"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "echo line1; echo line2; echo line3")
                :stdout :pipe :stderr :inherit))
        (lines nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line) (push line lines)))
      (setf lines (nreverse lines))
      (assert-equal 3 (length lines))
      (assert-equal "line1" (first lines))
      (assert-equal "line2" (second lines))
      (assert-equal "line3" (third lines))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-stderr-lines ()
  "Stderr output via callback"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "echo err1 >&2; echo err2 >&2")
                :stdout :inherit :stderr :pipe))
        (lines nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stderr (lambda (line) (push line lines)))
      (setf lines (nreverse lines))
      (assert-equal 2 (length lines))
      (assert-equal "err1" (first lines))
      (assert-equal "err2" (second lines))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-concurrent-stdout-stderr ()
  "Interleaved stdout+stderr"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c"
                        "echo out1; echo err1 >&2; echo out2; echo err2 >&2")
                :stdout :pipe :stderr :pipe))
        (out-lines nil)
        (err-lines nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line) (push line out-lines))
                         :on-stderr (lambda (line) (push line err-lines)))
      (setf out-lines (nreverse out-lines))
      (setf err-lines (nreverse err-lines))
      (assert-equal 2 (length out-lines))
      (assert-equal 2 (length err-lines))
      (assert-equal "out1" (first out-lines))
      (assert-equal "out2" (second out-lines))
      (assert-equal "err1" (first err-lines))
      (assert-equal "err2" (second err-lines))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-large-output ()
  "seq 1 100000 (~500KB), verify line count"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "seq 1 100000")
                :stdout :pipe :stderr :inherit))
        (count 0))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line)
                                      (declare (ignore line))
                                      (incf count)))
      (assert-equal 100000 count)
      (spawn:handle-close-streams handle))))

(deftest test-monitor-timeout ()
  "sleep 60 with timeout 0.5, verify returns quickly"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "sleep 60")
                :stdout :pipe :stderr :inherit))
        (start (get-internal-real-time)))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle :timeout 0.5)
      (let* ((elapsed-ticks (- (get-internal-real-time) start))
             (elapsed-secs (/ elapsed-ticks
                              internal-time-units-per-second)))
        ;; Should return within ~2 seconds (generous bound)
        (assert-true (< elapsed-secs 2.0)))
      ;; Process should still be running (timeout does NOT kill)
      (assert-equal :running (spawn:process-handle-status handle))
      ;; Clean up
      (spawn:handle-kill handle)
      (spawn:wait-for-handle handle 5)
      (spawn:handle-close-streams handle))))

(deftest test-monitor-exit-callback ()
  "exit 42, verify exit code in callback"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "exit 42")
                :stdout :pipe :stderr :inherit))
        (exit-code nil)
        (exit-signal nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-exit (lambda (code sig)
                                    (setf exit-code code
                                          exit-signal sig)))
      (assert-equal 42 exit-code)
      (assert-true (null exit-signal))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-signal-death ()
  "kill -9 $$, verify signal in callback"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "kill -9 $$")
                :stdout :pipe :stderr :inherit))
        (exit-code nil)
        (exit-signal nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-exit (lambda (code sig)
                                    (setf exit-code code
                                          exit-signal sig)))
      (assert-true (null exit-code))
      (assert-equal 9 exit-signal)
      (spawn:handle-close-streams handle))))

(deftest test-monitor-partial-line-at-eof ()
  "printf no-newline, verify delivered"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "printf 'no-newline'")
                :stdout :pipe :stderr :inherit))
        (lines nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line) (push line lines)))
      (assert-equal 1 (length lines))
      (assert-equal "no-newline" (first lines))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-no-output ()
  "true with stdout piped, no callback fired"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "true")
                :stdout :pipe :stderr :inherit))
        (called nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line)
                                      (declare (ignore line))
                                      (setf called t)))
      (assert-true (not called))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-stderr-only ()
  "Only stderr piped"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "echo stderr-only >&2")
                :stdout :inherit :stderr :pipe))
        (lines nil))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stderr (lambda (line) (push line lines)))
      (assert-equal 1 (length lines))
      (assert-equal "stderr-only" (first lines))
      (spawn:handle-close-streams handle))))

(deftest test-monitor-rapid-lines ()
  "yes | head -n 50000, stress test"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "yes | head -n 50000")
                :stdout :pipe :stderr :inherit))
        (count 0))
    (let ((handle (spawn:spawn spec)))
      (io:monitor-handle handle
                         :on-stdout (lambda (line)
                                      (declare (ignore line))
                                      (incf count)))
      (assert-equal 50000 count)
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; Convenience Function Tests
;;; ============================================================================

(deftest test-collect-output ()
  "Collect stdout string"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "echo hello; echo world")
                :stdout :pipe :stderr :inherit)))
    (let ((handle (spawn:spawn spec)))
      (multiple-value-bind (stdout stderr exit-code)
          (io:collect-output handle)
        (assert-equal (format nil "hello~%world~%") stdout)
        (assert-equal "" stderr)
        (assert-equal 0 exit-code))
      (spawn:handle-close-streams handle))))

(deftest test-collect-output-with-stderr ()
  "Collect both streams"
  (let ((spec (spawn:make-spawn-spec "/bin/sh"
                :args '("/bin/sh" "-c" "echo out; echo err >&2")
                :stdout :pipe :stderr :pipe)))
    (let ((handle (spawn:spawn spec)))
      (multiple-value-bind (stdout stderr exit-code)
          (io:collect-output handle)
        (assert-equal (format nil "out~%") stdout)
        (assert-equal (format nil "err~%") stderr)
        (assert-equal 0 exit-code))
      (spawn:handle-close-streams handle))))
