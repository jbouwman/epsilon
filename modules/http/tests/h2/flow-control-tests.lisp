;;;; HTTP/2 Flow Control Tests
;;;;
;;;; Unit tests for HTTP/2 flow control including window management,
;;;; data frame handling, and settings application per RFC 7540.

(defpackage :epsilon.http.h2.flow-control-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.flow-control flow)))

;;;; Constants Tests

(deftest test-flow-control-constants ()
  "Test flow control constants"
  (assert-true (= flow:+default-initial-window-size+ 65535))
  (assert-true (= flow:+max-window-size+ (1- (ash 1 31)))))

;;;; Flow Controller Creation Tests

(deftest test-make-flow-controller-defaults ()
  "Test default flow controller creation"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (flow:flow-controller-p fc))
    (assert-true (= (flow:flow-controller-send-window fc) 65535))
    (assert-true (= (flow:flow-controller-recv-window fc) 65535))
    (assert-true (= (flow:flow-controller-initial-send-window fc) 65535))
    (assert-true (= (flow:flow-controller-initial-recv-window fc) 65535))))

(deftest test-make-connection-flow-controller ()
  "Test connection-level flow controller"
  (let ((fc (flow:make-connection-flow-controller)))
    (assert-true (flow:flow-controller-p fc))
    (assert-true (= (flow:flow-controller-send-window fc) 65535))))

(deftest test-make-stream-flow-controller-default ()
  "Test stream-level flow controller with default window"
  (let ((fc (flow:make-stream-flow-controller)))
    (assert-true (= (flow:flow-controller-send-window fc) 65535))
    (assert-true (= (flow:flow-controller-recv-window fc) 65535))))

(deftest test-make-stream-flow-controller-custom ()
  "Test stream-level flow controller with custom window"
  (let ((fc (flow:make-stream-flow-controller 32768)))
    (assert-true (= (flow:flow-controller-send-window fc) 32768))
    (assert-true (= (flow:flow-controller-recv-window fc) 32768))
    (assert-true (= (flow:flow-controller-initial-send-window fc) 32768))
    (assert-true (= (flow:flow-controller-initial-recv-window fc) 32768))))

;;;; can-send-p Tests

(deftest test-can-send-p-sufficient ()
  "Test can-send-p returns true when window is sufficient"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (flow:can-send-p fc 100))
    (assert-true (flow:can-send-p fc 65535))))

(deftest test-can-send-p-insufficient ()
  "Test can-send-p returns false when window is insufficient"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (not (flow:can-send-p fc 65536)))))

(deftest test-can-send-p-zero ()
  "Test can-send-p with zero bytes"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (flow:can-send-p fc 0))))

;;;; consume-send-window Tests

(deftest test-consume-send-window ()
  "Test consuming send window"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-send-window fc 1000)
    (assert-true (= (flow:flow-controller-send-window fc) (- 65535 1000)))))

(deftest test-consume-send-window-overflow ()
  "Test consuming more than available send window signals error"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (handler-case
            (progn (flow:consume-send-window fc 70000) nil)
          (error () t)))))

;;;; update-send-window Tests

(deftest test-update-send-window ()
  "Test updating send window with WINDOW_UPDATE"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-send-window fc 10000)
    (flow:update-send-window fc 5000)
    (assert-true (= (flow:flow-controller-send-window fc) (- 65535 10000 -5000)))))

(deftest test-update-send-window-overflow ()
  "Test that window overflow past max signals error"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (handler-case
            (progn (flow:update-send-window fc flow:+max-window-size+) nil)
          (error () t)))))

;;;; consume-recv-window Tests

(deftest test-consume-recv-window ()
  "Test consuming receive window"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-recv-window fc 2000)
    (assert-true (= (flow:flow-controller-recv-window fc) (- 65535 2000)))))

(deftest test-consume-recv-window-overflow ()
  "Test consuming more than available recv window signals error"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (handler-case
            (progn (flow:consume-recv-window fc 70000) nil)
          (error () t)))))

;;;; should-send-window-update-p Tests

(deftest test-should-send-window-update-not-yet ()
  "Test should-send-window-update-p when below threshold"
  (let ((fc (flow:make-flow-controller)))
    ;; Consume less than 50% of window
    (flow:consume-recv-window fc 10000)
    (assert-true (not (flow:should-send-window-update-p fc)))))

(deftest test-should-send-window-update-yes ()
  "Test should-send-window-update-p when above threshold"
  (let ((fc (flow:make-flow-controller)))
    ;; Consume more than 50% of window
    (flow:consume-recv-window fc 40000)
    (assert-true (flow:should-send-window-update-p fc))))

(deftest test-should-send-window-update-custom-threshold ()
  "Test should-send-window-update-p with custom threshold"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-recv-window fc 10000)
    ;; 10000/65535 ~ 15%, above a 10% threshold
    (assert-true (flow:should-send-window-update-p fc 0.1))))

;;;; calculate-window-update Tests

(deftest test-calculate-window-update ()
  "Test calculate-window-update returns consumed amount"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-recv-window fc 5000)
    (assert-true (= (flow:calculate-window-update fc) 5000))))

(deftest test-calculate-window-update-no-consumption ()
  "Test calculate-window-update when nothing consumed"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (= (flow:calculate-window-update fc) 0))))

;;;; update-recv-window Tests

(deftest test-update-recv-window ()
  "Test updating receive window"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-recv-window fc 10000)
    (flow:update-recv-window fc 10000)
    (assert-true (= (flow:flow-controller-recv-window fc) 65535))))

(deftest test-update-recv-window-overflow ()
  "Test receive window overflow signals error"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (handler-case
            (progn (flow:update-recv-window fc flow:+max-window-size+) nil)
          (error () t)))))

;;;; apply-settings-to-flow-control Tests

(deftest test-apply-settings-window-increase ()
  "Test applying SETTINGS that increases initial window size"
  (let ((fc (flow:make-flow-controller)))
    ;; Setting ID 4 = INITIAL_WINDOW_SIZE, increase from 65535 to 131070
    (flow:apply-settings-to-flow-control fc '((4 . 131070)))
    ;; Delta = 131070 - 65535 = 65535, applied to current send window
    (assert-true (= (flow:flow-controller-send-window fc) 131070))
    (assert-true (= (flow:flow-controller-initial-send-window fc) 131070))))

(deftest test-apply-settings-window-decrease ()
  "Test applying SETTINGS that decreases initial window size"
  (let ((fc (flow:make-flow-controller)))
    (flow:apply-settings-to-flow-control fc '((4 . 32768)))
    ;; Delta = 32768 - 65535 = -32767
    (assert-true (= (flow:flow-controller-send-window fc) 32768))
    (assert-true (= (flow:flow-controller-initial-send-window fc) 32768))))

(deftest test-apply-settings-no-window-setting ()
  "Test applying SETTINGS without window size has no effect"
  (let ((fc (flow:make-flow-controller)))
    (flow:apply-settings-to-flow-control fc '((1 . 8192)))  ; HEADER_TABLE_SIZE
    (assert-true (= (flow:flow-controller-send-window fc) 65535))))

;;;; handle-data-frame-flow-control Tests

(deftest test-handle-data-frame-both-controllers ()
  "Test handling DATA frame consumes both connection and stream windows"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller)))
    (flow:handle-data-frame-flow-control conn-fc stream-fc 1000)
    (assert-true (= (flow:flow-controller-recv-window conn-fc) (- 65535 1000)))
    (assert-true (= (flow:flow-controller-recv-window stream-fc) (- 65535 1000)))))

(deftest test-handle-data-frame-returns-updates ()
  "Test handle-data-frame returns window update recommendations"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller)))
    ;; Consume more than 50% to trigger update recommendation
    (multiple-value-bind (conn-update stream-update)
        (flow:handle-data-frame-flow-control conn-fc stream-fc 40000)
      (assert-true (not (null conn-update)))
      (assert-true (not (null stream-update)))
      (assert-true (= conn-update 40000))
      (assert-true (= stream-update 40000)))))

(deftest test-handle-data-frame-no-stream-controller ()
  "Test handle-data-frame works without stream controller"
  (let ((conn-fc (flow:make-connection-flow-controller)))
    (flow:handle-data-frame-flow-control conn-fc nil 1000)
    (assert-true (= (flow:flow-controller-recv-window conn-fc) (- 65535 1000)))))

;;;; handle-window-update-frame Tests

(deftest test-handle-window-update-normal ()
  "Test handling a normal WINDOW_UPDATE"
  (let ((fc (flow:make-flow-controller)))
    (flow:consume-send-window fc 10000)
    (flow:handle-window-update-frame fc 5000)
    (assert-true (= (flow:flow-controller-send-window fc) (- 65535 10000 -5000)))))

(deftest test-handle-window-update-zero-increment ()
  "Test that zero increment signals error"
  (let ((fc (flow:make-flow-controller)))
    (assert-true (handler-case
            (progn (flow:handle-window-update-frame fc 0) nil)
          (error () t)))))

;;;; can-send-data-p Tests

(deftest test-can-send-data-both-sufficient ()
  "Test can-send-data-p when both windows sufficient"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller)))
    (assert-true (flow:can-send-data-p conn-fc stream-fc 1000))))

(deftest test-can-send-data-conn-insufficient ()
  "Test can-send-data-p when connection window insufficient"
  (let ((conn-fc (flow:make-stream-flow-controller 100))
        (stream-fc (flow:make-stream-flow-controller)))
    (assert-true (not (flow:can-send-data-p conn-fc stream-fc 200)))))

(deftest test-can-send-data-stream-insufficient ()
  "Test can-send-data-p when stream window insufficient"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller 100)))
    (assert-true (not (flow:can-send-data-p conn-fc stream-fc 200)))))

(deftest test-can-send-data-nil-stream ()
  "Test can-send-data-p with nil stream controller"
  (let ((conn-fc (flow:make-connection-flow-controller)))
    (assert-true (flow:can-send-data-p conn-fc nil 1000))))

;;;; send-data-with-flow-control Tests

(deftest test-send-data-with-flow-control ()
  "Test send-data-with-flow-control consumes both windows"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller))
        (data (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (let ((result (flow:send-data-with-flow-control conn-fc stream-fc data)))
      (assert-true (= (length result) 100))
      (assert-true (= (flow:flow-controller-send-window conn-fc) (- 65535 100)))
      (assert-true (= (flow:flow-controller-send-window stream-fc) (- 65535 100))))))

(deftest test-send-data-insufficient-connection-window ()
  "Test send-data-with-flow-control errors on insufficient connection window"
  (let ((conn-fc (flow:make-stream-flow-controller 50))
        (stream-fc (flow:make-stream-flow-controller))
        (data (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-true (handler-case
            (progn (flow:send-data-with-flow-control conn-fc stream-fc data) nil)
          (error () t)))))

(deftest test-send-data-insufficient-stream-window ()
  "Test send-data-with-flow-control errors on insufficient stream window"
  (let ((conn-fc (flow:make-connection-flow-controller))
        (stream-fc (flow:make-stream-flow-controller 50))
        (data (make-array 100 :element-type '(unsigned-byte 8) :initial-element 0)))
    (assert-true (handler-case
            (progn (flow:send-data-with-flow-control conn-fc stream-fc data) nil)
          (error () t)))))
