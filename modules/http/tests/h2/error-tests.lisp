;;;; HTTP/2 Error Handling Tests
;;;;
;;;; Unit tests for HTTP/2 error conditions, code lookups,
;;;; and protocol violation checks per RFC 7540.

(defpackage :epsilon.http.h2.error-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.error err)
   (epsilon.http.h2.frames frames)))

;;;; Error Condition Tests

(deftest test-http2-error-signalable ()
  "Test that http2-error can be signaled and caught"
  (assert-true (handler-case
          (progn
            (error 'err:http2-error
                   :error-code frames:+error-protocol-error+
                   :message "test error")
            nil)
        (err:http2-error () t))))

(deftest test-http2-connection-error-inherits ()
  "Test that http2-connection-error inherits from http2-error"
  (assert-true (handler-case
          (progn
            (error 'err:http2-connection-error
                   :error-code frames:+error-protocol-error+
                   :message "connection error")
            nil)
        (err:http2-error () t))))

(deftest test-http2-stream-error-signalable ()
  "Test that http2-stream-error can be signaled"
  (assert-true (handler-case
          (progn
            (error 'err:http2-stream-error
                   :error-code frames:+error-cancel+
                   :message "stream error"
                   :stream-id 5)
            nil)
        (err:http2-stream-error () t))))

(deftest test-http2-compression-error-inherits ()
  "Test that http2-compression-error inherits from http2-connection-error"
  (assert-true (handler-case
          (progn
            (error 'err:http2-compression-error :message "bad compression")
            nil)
        (err:http2-connection-error () t))))

(deftest test-http2-compression-error-default-code ()
  "Test that http2-compression-error has default error code"
  (handler-case
      (error 'err:http2-compression-error :message "test")
    (err:http2-error (e)
      (assert-true (= (err::error-code e) frames:+error-compression-error+)))))

(deftest test-http2-flow-control-error-default-code ()
  "Test that http2-flow-control-error has default error code"
  (handler-case
      (error 'err:http2-flow-control-error :message "test")
    (err:http2-error (e)
      (assert-true (= (err::error-code e) frames:+error-flow-control-error+)))))

(deftest test-http2-protocol-error-default-code ()
  "Test that http2-protocol-error has default error code"
  (handler-case
      (error 'err:http2-protocol-error :message "test")
    (err:http2-error (e)
      (assert-true (= (err::error-code e) frames:+error-protocol-error+)))))

(deftest test-http2-frame-size-error-default-code ()
  "Test that http2-frame-size-error has default error code"
  (handler-case
      (error 'err:http2-frame-size-error :message "test")
    (err:http2-error (e)
      (assert-true (= (err::error-code e) frames:+error-frame-size-error+)))))

(deftest test-http2-settings-timeout-error-default-code ()
  "Test that http2-settings-timeout-error has default error code"
  (handler-case
      (error 'err:http2-settings-timeout-error :message "test")
    (err:http2-error (e)
      (assert-true (= (err::error-code e) frames:+error-settings-timeout+)))))

;;;; Error Code Name Tests

(deftest test-error-code-name-all-codes ()
  "Test error-code-name returns correct names for all 14 error codes"
  (assert-true (equal (err:error-code-name frames:+error-no-error+) "NO_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-protocol-error+) "PROTOCOL_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-internal-error+) "INTERNAL_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-flow-control-error+) "FLOW_CONTROL_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-settings-timeout+) "SETTINGS_TIMEOUT"))
  (assert-true (equal (err:error-code-name frames:+error-stream-closed+) "STREAM_CLOSED"))
  (assert-true (equal (err:error-code-name frames:+error-frame-size-error+) "FRAME_SIZE_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-refused-stream+) "REFUSED_STREAM"))
  (assert-true (equal (err:error-code-name frames:+error-cancel+) "CANCEL"))
  (assert-true (equal (err:error-code-name frames:+error-compression-error+) "COMPRESSION_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-connect-error+) "CONNECT_ERROR"))
  (assert-true (equal (err:error-code-name frames:+error-enhance-your-calm+) "ENHANCE_YOUR_CALM"))
  (assert-true (equal (err:error-code-name frames:+error-inadequate-security+) "INADEQUATE_SECURITY"))
  (assert-true (equal (err:error-code-name frames:+error-http-1-1-required+) "HTTP_1_1_REQUIRED")))

(deftest test-error-code-name-unknown ()
  "Test error-code-name for unknown error code"
  (let ((name (err:error-code-name 999)))
    (assert-true (stringp name))
    (assert-true (search "UNKNOWN" name))))

;;;; Error Code Description Tests

(deftest test-error-code-description-all-codes ()
  "Test error-code-description returns non-empty strings"
  (dolist (code (list frames:+error-no-error+
                     frames:+error-protocol-error+
                     frames:+error-internal-error+
                     frames:+error-flow-control-error+
                     frames:+error-settings-timeout+
                     frames:+error-stream-closed+
                     frames:+error-frame-size-error+
                     frames:+error-refused-stream+
                     frames:+error-cancel+
                     frames:+error-compression-error+
                     frames:+error-connect-error+
                     frames:+error-enhance-your-calm+
                     frames:+error-inadequate-security+
                     frames:+error-http-1-1-required+))
    (let ((desc (err:error-code-description code)))
      (assert-true (stringp desc))
      (assert-true (> (length desc) 0)))))

;;;; should-close-connection-p Tests

(deftest test-should-close-connection-protocol-error ()
  "Test that PROTOCOL_ERROR requires connection closure"
  (assert-true (err:should-close-connection-p frames:+error-protocol-error+)))

(deftest test-should-close-connection-internal-error ()
  "Test that INTERNAL_ERROR requires connection closure"
  (assert-true (err:should-close-connection-p frames:+error-internal-error+)))

(deftest test-should-close-connection-flow-control ()
  "Test that FLOW_CONTROL_ERROR requires connection closure"
  (assert-true (err:should-close-connection-p frames:+error-flow-control-error+)))

(deftest test-should-not-close-connection-cancel ()
  "Test that CANCEL does not require connection closure"
  (assert-true (not (err:should-close-connection-p frames:+error-cancel+))))

(deftest test-should-not-close-connection-refused ()
  "Test that REFUSED_STREAM does not require connection closure"
  (assert-true (not (err:should-close-connection-p frames:+error-refused-stream+))))

(deftest test-should-not-close-connection-no-error ()
  "Test that NO_ERROR does not require connection closure"
  (assert-true (not (err:should-close-connection-p frames:+error-no-error+))))

;;;; check-frame-size Tests

(deftest test-check-frame-size-valid ()
  "Test check-frame-size passes for valid frame"
  (let ((frame (frames:make-data-frame 1 "hello")))
    ;; Should not signal
    (err:check-frame-size frame 16384)
    (assert-true t)))

(deftest test-check-frame-size-oversized ()
  "Test check-frame-size signals for oversized frame"
  (let ((frame (frames:make-http2-frame :length 20000 :type frames:+frame-data+)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-frame-size-error () t)))))

(deftest test-check-frame-size-settings-not-multiple-of-6 ()
  "Test check-frame-size signals for SETTINGS not multiple of 6"
  (let ((frame (frames:make-http2-frame :length 7
                                        :type frames:+frame-settings+
                                        :stream-id 0)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-frame-size-error () t)))))

(deftest test-check-frame-size-window-update-not-4 ()
  "Test check-frame-size signals for WINDOW_UPDATE not 4 bytes"
  (let ((frame (frames:make-http2-frame :length 5
                                        :type frames:+frame-window-update+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-frame-size-error () t)))))

(deftest test-check-frame-size-priority-not-5 ()
  "Test check-frame-size signals for PRIORITY not 5 bytes"
  (let ((frame (frames:make-http2-frame :length 4
                                        :type frames:+frame-priority+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-stream-error () t)))))

(deftest test-check-frame-size-rst-stream-not-4 ()
  "Test check-frame-size signals for RST_STREAM not 4 bytes"
  (let ((frame (frames:make-http2-frame :length 3
                                        :type frames:+frame-rst-stream+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-connection-error () t)))))

(deftest test-check-frame-size-ping-not-8 ()
  "Test check-frame-size signals for PING not 8 bytes"
  (let ((frame (frames:make-http2-frame :length 6
                                        :type frames:+frame-ping+
                                        :stream-id 0)))
    (assert-true (handler-case
            (progn (err:check-frame-size frame 16384) nil)
          (err:http2-connection-error () t)))))

;;;; check-stream-id-validity Tests

(deftest test-check-stream-id-settings-must-be-zero ()
  "Test SETTINGS frame must have stream ID 0"
  (let ((frame (frames:make-http2-frame :type frames:+frame-settings+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-stream-id-validity frame) nil)
          (err:http2-protocol-error () t)))))

(deftest test-check-stream-id-ping-must-be-zero ()
  "Test PING frame must have stream ID 0"
  (let ((frame (frames:make-http2-frame :type frames:+frame-ping+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-stream-id-validity frame) nil)
          (err:http2-protocol-error () t)))))

(deftest test-check-stream-id-goaway-must-be-zero ()
  "Test GOAWAY frame must have stream ID 0"
  (let ((frame (frames:make-http2-frame :type frames:+frame-goaway+
                                        :stream-id 1)))
    (assert-true (handler-case
            (progn (err:check-stream-id-validity frame) nil)
          (err:http2-protocol-error () t)))))

(deftest test-check-stream-id-data-must-be-nonzero ()
  "Test DATA frame must have non-zero stream ID"
  (let ((frame (frames:make-http2-frame :type frames:+frame-data+
                                        :stream-id 0)))
    (assert-true (handler-case
            (progn (err:check-stream-id-validity frame) nil)
          (err:http2-protocol-error () t)))))

(deftest test-check-stream-id-headers-must-be-nonzero ()
  "Test HEADERS frame must have non-zero stream ID"
  (let ((frame (frames:make-http2-frame :type frames:+frame-headers+
                                        :stream-id 0)))
    (assert-true (handler-case
            (progn (err:check-stream-id-validity frame) nil)
          (err:http2-protocol-error () t)))))

(deftest test-check-stream-id-valid-settings ()
  "Test valid SETTINGS frame with stream ID 0"
  (let ((frame (frames:make-http2-frame :type frames:+frame-settings+
                                        :stream-id 0)))
    ;; Should not signal
    (err:check-stream-id-validity frame)
    (assert-true t)))

(deftest test-check-stream-id-valid-data ()
  "Test valid DATA frame with non-zero stream ID"
  (let ((frame (frames:make-http2-frame :type frames:+frame-data+
                                        :stream-id 1)))
    ;; Should not signal
    (err:check-stream-id-validity frame)
    (assert-true t)))

;;;; check-padding Tests

(deftest test-check-padding-valid ()
  "Test check-padding passes for valid padding"
  (let* ((payload (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame (frames:make-http2-frame :type frames:+frame-data+
                                         :flags frames:+flag-padded+
                                         :payload payload)))
    ;; Pad length 0 at byte 0 is valid
    (err:check-padding frame)
    (assert-true t)))

(deftest test-check-padding-exceeds-payload ()
  "Test check-padding signals when pad length >= payload length"
  (let* ((payload (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set pad length to 5 (>= payload length of 5)
    (setf (aref payload 0) 5)
    (let ((frame (frames:make-http2-frame :type frames:+frame-data+
                                          :flags frames:+flag-padded+
                                          :payload payload)))
      (assert-true (handler-case
              (progn (err:check-padding frame) nil)
            (err:http2-protocol-error () t))))))

(deftest test-check-padding-not-padded ()
  "Test check-padding does nothing for non-padded frames"
  (let ((frame (frames:make-http2-frame :type frames:+frame-data+
                                        :flags 0)))
    ;; Should not signal even without payload
    (err:check-padding frame)
    (assert-true t)))

;;;; check-settings-values Tests

(deftest test-check-settings-enable-push-valid ()
  "Test check-settings-values accepts ENABLE_PUSH 0 and 1"
  (err:check-settings-values (list (cons frames:+settings-enable-push+ 0)))
  (err:check-settings-values (list (cons frames:+settings-enable-push+ 1)))
  (assert-true t))

(deftest test-check-settings-enable-push-invalid ()
  "Test check-settings-values rejects ENABLE_PUSH not 0 or 1"
  (assert-true (handler-case
          (progn
            (err:check-settings-values
             (list (cons frames:+settings-enable-push+ 2)))
            nil)
        (err:http2-protocol-error () t))))

(deftest test-check-settings-initial-window-too-large ()
  "Test check-settings-values rejects oversized initial window"
  (assert-true (handler-case
          (progn
            (err:check-settings-values
             (list (cons frames:+settings-initial-window-size+ (ash 1 31))))
            nil)
        (err:http2-flow-control-error () t))))

(deftest test-check-settings-max-frame-size-out-of-range ()
  "Test check-settings-values rejects MAX_FRAME_SIZE out of range"
  ;; Below minimum (16384)
  (assert-true (handler-case
          (progn
            (err:check-settings-values
             (list (cons frames:+settings-max-frame-size+ 1000)))
            nil)
        (err:http2-protocol-error () t)))
  ;; Above maximum (16777215)
  (assert-true (handler-case
          (progn
            (err:check-settings-values
             (list (cons frames:+settings-max-frame-size+ 20000000)))
            nil)
        (err:http2-protocol-error () t))))

(deftest test-check-settings-max-frame-size-valid ()
  "Test check-settings-values accepts valid MAX_FRAME_SIZE"
  (err:check-settings-values
   (list (cons frames:+settings-max-frame-size+ 16384)))
  (err:check-settings-values
   (list (cons frames:+settings-max-frame-size+ 16777215)))
  (assert-true t))

;;;; check-window-update Tests

(deftest test-check-window-update-zero-connection ()
  "Test zero increment on connection signals protocol error"
  (assert-true (handler-case
          (progn (err:check-window-update 0 0) nil)
        (err:http2-protocol-error () t))))

(deftest test-check-window-update-zero-stream ()
  "Test zero increment on stream signals stream error"
  (assert-true (handler-case
          (progn (err:check-window-update 0 5) nil)
        (err:http2-stream-error () t))))

(deftest test-check-window-update-too-large ()
  "Test increment > 2^31-1 signals flow control error"
  (assert-true (handler-case
          (progn (err:check-window-update (ash 1 31) 0) nil)
        (err:http2-flow-control-error () t))))

(deftest test-check-window-update-valid ()
  "Test valid window update increment"
  (err:check-window-update 1000 0)
  (err:check-window-update 1000 5)
  (assert-true t))

;;;; check-priority-validity Tests

(deftest test-check-priority-self-dependency ()
  "Test stream depending on itself signals error"
  (assert-true (handler-case
          (progn (err:check-priority-validity 5 5) nil)
        (err:http2-stream-error () t))))

(deftest test-check-priority-valid ()
  "Test valid priority (different stream and dependency)"
  (err:check-priority-validity 5 3)
  (assert-true t))

;;;; check-header-block-validity Tests

(deftest test-check-headers-pseudo-after-regular ()
  "Test pseudo-headers after regular headers signals error"
  (assert-true (handler-case
          (progn
            (err:check-header-block-validity
             '(("content-type" . "text/html")
               (":method" . "GET")))
            nil)
        (err:http2-protocol-error () t))))

(deftest test-check-headers-missing-mandatory ()
  "Test missing mandatory pseudo-headers signals error"
  ;; Has :method but missing :scheme and :path
  (assert-true (handler-case
          (progn
            (err:check-header-block-validity
             '((":method" . "GET")))
            nil)
        (err:http2-protocol-error () t))))

(deftest test-check-headers-valid-request ()
  "Test valid request headers pass validation"
  (err:check-header-block-validity
   '((":method" . "GET")
     (":scheme" . "https")
     (":path" . "/")
     ("accept" . "text/html")))
  (assert-true t))

(deftest test-check-headers-valid-response ()
  "Test valid response headers (no :method) pass validation"
  (err:check-header-block-validity
   '((":status" . "200")
     ("content-type" . "text/html")))
  (assert-true t))

;;;; Error Frame Generation Tests

(deftest test-error-make-rst-stream-frame ()
  "Test RST_STREAM frame from error module"
  (let ((frame (err:make-rst-stream-frame 3 frames:+error-cancel+)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-rst-stream+))
    (assert-true (= (frames:http2-frame-stream-id frame) 3))
    (assert-true (= (frames:http2-frame-length frame) 4))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Error code = 8 (CANCEL)
      (assert-true (= (aref payload 3) 8)))))

(deftest test-error-make-goaway-frame ()
  "Test GOAWAY frame from error module"
  (let ((frame (err:make-goaway-frame 5 frames:+error-no-error+)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-goaway+))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    (assert-true (= (frames:http2-frame-length frame) 8))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Last stream ID = 5
      (assert-true (= (aref payload 3) 5))
      ;; Error code = 0
      (assert-true (= (aref payload 7) 0)))))

(deftest test-error-make-goaway-frame-with-debug ()
  "Test GOAWAY frame with debug data from error module"
  (let ((frame (err:make-goaway-frame 1 frames:+error-internal-error+ "err")))
    ;; 8 bytes header + 3 bytes debug
    (assert-true (= (frames:http2-frame-length frame) 11))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Debug data starts at offset 8
      (assert-true (= (aref payload 8) (char-code #\e))))))
