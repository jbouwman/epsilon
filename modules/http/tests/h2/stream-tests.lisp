;;;; HTTP/2 Stream Tests
;;;;
;;;; Unit tests for HTTP/2 stream lifecycle, state transitions,
;;;; and frame validation per RFC 7540 Section 5.1.

(defpackage :epsilon.http.h2.stream-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.stream stream)
   (epsilon.http.h2.frames frames)
   (epsilon.http.h2.flow-control flow)))

;;;; Stream State Constants

(deftest test-stream-state-constants ()
  "Test stream state constants have distinct values"
  (assert-true (= stream:+stream-idle+ 0))
  (assert-true (= stream:+stream-reserved-local+ 1))
  (assert-true (= stream:+stream-reserved-remote+ 2))
  (assert-true (= stream:+stream-open+ 3))
  (assert-true (= stream:+stream-half-closed-local+ 4))
  (assert-true (= stream:+stream-half-closed-remote+ 5))
  (assert-true (= stream:+stream-closed+ 6)))

;;;; Stream Initialization

(deftest test-initialize-stream ()
  "Test that initialize-stream creates a stream in idle state"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (stream:http2-stream-p s))
    (assert-true (= (stream:http2-stream-id s) 1))
    (assert-true (= (stream:http2-stream-state s) stream:+stream-idle+))
    (assert-true (not (null (stream:http2-stream-flow-controller s))))))

(deftest test-initialize-stream-custom-window ()
  "Test initialize-stream with custom window size"
  (let ((s (stream:initialize-stream 3 nil :initial-window-size 32768)))
    (assert-true (= (stream:http2-stream-id s) 3))
    (let ((fc (stream:http2-stream-flow-controller s)))
      (assert-true (= (flow:flow-controller-send-window fc) 32768)))))

;;;; Open Stream

(deftest test-open-stream-from-idle ()
  "Test opening a stream from idle state"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-open+))))

(deftest test-open-stream-from-reserved ()
  "Test opening a stream from reserved state goes to half-closed-remote"
  (let ((s (stream:initialize-stream 2 nil)))
    (stream:reserve-stream s t)  ; reserved-local
    (stream:open-stream s)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-half-closed-remote+))))

(deftest test-open-stream-from-open-errors ()
  "Test opening an already open stream signals an error"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (assert-true (handler-case
            (progn (stream:open-stream s) nil)
          (error () t)))))

;;;; Reserve Stream

(deftest test-reserve-stream-local ()
  "Test reserving a stream for local push"
  (let ((s (stream:initialize-stream 2 nil)))
    (stream:reserve-stream s t)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-reserved-local+))))

(deftest test-reserve-stream-remote ()
  "Test reserving a stream for remote push"
  (let ((s (stream:initialize-stream 2 nil)))
    (stream:reserve-stream s nil)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-reserved-remote+))))

;;;; HEADERS State Transitions

(deftest test-headers-idle-to-open ()
  "Test HEADERS without END_STREAM: idle -> open"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:transition-stream-state s frames:+frame-headers+ 0)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-open+))))

(deftest test-headers-idle-to-half-closed-remote ()
  "Test HEADERS with END_STREAM: idle -> half-closed-remote"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:transition-stream-state s frames:+frame-headers+ frames:+flag-end-stream+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-half-closed-remote+))))

(deftest test-headers-open-with-end-stream ()
  "Test HEADERS with END_STREAM: open -> half-closed-remote"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:transition-stream-state s frames:+frame-headers+ frames:+flag-end-stream+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-half-closed-remote+))))

(deftest test-headers-open-without-end-stream ()
  "Test HEADERS without END_STREAM: open stays open"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:transition-stream-state s frames:+frame-headers+ 0)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-open+))))

(deftest test-headers-half-closed-local-to-closed ()
  "Test HEADERS with END_STREAM: half-closed-local -> closed"
  (let ((s (stream:initialize-stream 1 nil)))
    (setf (stream:http2-stream-state s) stream:+stream-half-closed-local+)
    (stream:transition-stream-state s frames:+frame-headers+ frames:+flag-end-stream+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))))

;;;; DATA State Transitions

(deftest test-data-open-with-end-stream ()
  "Test DATA with END_STREAM: open -> half-closed-remote"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:transition-stream-state s frames:+frame-data+ frames:+flag-end-stream+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-half-closed-remote+))))

(deftest test-data-open-without-end-stream ()
  "Test DATA without END_STREAM: open stays open"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:transition-stream-state s frames:+frame-data+ 0)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-open+))))

(deftest test-data-half-closed-local-to-closed ()
  "Test DATA with END_STREAM: half-closed-local -> closed"
  (let ((s (stream:initialize-stream 1 nil)))
    (setf (stream:http2-stream-state s) stream:+stream-half-closed-local+)
    (stream:transition-stream-state s frames:+frame-data+ frames:+flag-end-stream+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))))

;;;; RST_STREAM Transitions

(deftest test-rst-stream-from-open ()
  "Test RST_STREAM: open -> closed"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:transition-stream-state s frames:+frame-rst-stream+ 0)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))
    (assert-true (stream::http2-stream-rst-received-p s))))

(deftest test-rst-stream-from-half-closed ()
  "Test RST_STREAM: half-closed -> closed"
  (let ((s (stream:initialize-stream 1 nil)))
    (setf (stream:http2-stream-state s) stream:+stream-half-closed-remote+)
    (stream:transition-stream-state s frames:+frame-rst-stream+ 0)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))))

;;;; can-receive-frame-p Tests

(deftest test-can-receive-headers-idle ()
  "Test that idle stream can receive HEADERS"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (stream:can-receive-frame-p s frames:+frame-headers+))))

(deftest test-can-receive-data-open ()
  "Test that open stream can receive DATA"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (assert-true (stream:can-receive-frame-p s frames:+frame-data+))))

(deftest test-cannot-receive-data-idle ()
  "Test that idle stream cannot receive DATA"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (not (stream:can-receive-frame-p s frames:+frame-data+)))))

(deftest test-can-receive-rst-stream-not-idle ()
  "Test that non-idle stream can receive RST_STREAM"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (assert-true (stream:can-receive-frame-p s frames:+frame-rst-stream+))))

(deftest test-cannot-receive-rst-stream-idle ()
  "Test that idle stream cannot receive RST_STREAM"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (not (stream:can-receive-frame-p s frames:+frame-rst-stream+)))))

(deftest test-can-receive-priority-any-state ()
  "Test that PRIORITY can be received in any state"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (stream:can-receive-frame-p s frames:+frame-priority+))
    (stream:open-stream s)
    (assert-true (stream:can-receive-frame-p s frames:+frame-priority+))
    (stream:close-stream s)
    (assert-true (stream:can-receive-frame-p s frames:+frame-priority+))))

;;;; can-send-frame-p Tests

(deftest test-can-send-headers-idle ()
  "Test that headers can be sent on idle stream"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (stream:can-send-frame-p s frames:+frame-headers+))))

(deftest test-can-send-data-open ()
  "Test that data can be sent on open stream"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (assert-true (stream:can-send-frame-p s frames:+frame-data+))))

(deftest test-cannot-send-data-idle ()
  "Test that data cannot be sent on idle stream"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (not (stream:can-send-frame-p s frames:+frame-data+)))))

(deftest test-cannot-send-rst-stream-closed ()
  "Test that RST_STREAM cannot be sent on closed stream"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:close-stream s)
    (assert-true (not (stream:can-send-frame-p s frames:+frame-rst-stream+)))))

;;;; Stream State Query Tests

(deftest test-stream-closed-p ()
  "Test stream-closed-p"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (not (stream:stream-closed-p s)))
    (stream:close-stream s)
    (assert-true (stream:stream-closed-p s))))

(deftest test-stream-open-p ()
  "Test stream-open-p"
  (let ((s (stream:initialize-stream 1 nil)))
    (assert-true (not (stream:stream-open-p s)))
    (stream:open-stream s)
    (assert-true (stream:stream-open-p s))
    (stream:close-stream s)
    (assert-true (not (stream:stream-open-p s)))))

;;;; Close and Reset Tests

(deftest test-close-stream ()
  "Test closing a stream"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:close-stream s)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))))

(deftest test-reset-stream ()
  "Test resetting a stream with error code"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (stream:reset-stream s frames:+error-cancel+)
    (assert-true (= (stream:http2-stream-state s) stream:+stream-closed+))
    (assert-true (= (stream::http2-stream-error-code s) frames:+error-cancel+))
    (assert-true (stream::http2-stream-rst-sent-p s))))

;;;; process-frame-for-stream Tests

(deftest test-process-headers-frame ()
  "Test processing a HEADERS frame stores it in headers buffer"
  (let ((s (stream:initialize-stream 1 nil))
        (frame (frames:make-headers-frame
                1
                (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0)
                :end-headers t)))
    (stream:process-frame-for-stream s frame)
    (assert-true (= (length (stream:http2-stream-headers-buffer s)) 1))
    (assert-true (= (stream:http2-stream-state s) stream:+stream-open+))))

(deftest test-process-data-frame ()
  "Test processing a DATA frame stores it in data buffer"
  (let ((s (stream:initialize-stream 1 nil)))
    (stream:open-stream s)
    (let ((frame (frames:make-data-frame 1 "hello")))
      (stream:process-frame-for-stream s frame)
      (assert-true (= (length (stream:http2-stream-data-buffer s)) 1)))))

(deftest test-process-frame-invalid-state ()
  "Test processing a frame in invalid state signals error"
  (let ((s (stream:initialize-stream 1 nil)))
    ;; Cannot receive DATA on idle stream
    (let ((frame (frames:make-data-frame 1 "hello")))
      (assert-true (handler-case
              (progn (stream:process-frame-for-stream s frame) nil)
            (error () t))))))

;;;; Alias Function Tests

(deftest test-stream-alias-functions ()
  "Test alias functions return correct values"
  (let ((s (stream:initialize-stream 5 :my-connection)))
    (assert-true (= (stream:stream-id s) 5))
    (assert-true (= (stream:stream-state s) stream:+stream-idle+))
    (assert-true (eq (stream:stream-connection s) :my-connection))
    (assert-true (not (null (stream:stream-flow-controller s))))
    (assert-true (= (stream:stream-dependency s) 0))
    (assert-true (= (stream:stream-weight s) 16))))

(deftest test-stream-priority-plist ()
  "Test stream-priority returns a plist"
  (let ((s (stream:initialize-stream 1 nil)))
    (let ((priority (stream:stream-priority s)))
      (assert-true (= (getf priority :dependency) 0))
      (assert-true (= (getf priority :weight) 16))
      (assert-true (not (getf priority :exclusive-p))))))
