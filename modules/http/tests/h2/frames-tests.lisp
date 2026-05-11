;;;; HTTP/2 Frame Tests
;;;;
;;;; Unit tests for HTTP/2 frame creation, serialization, and validation
;;;; per RFC 7540.

(defpackage :epsilon.http.h2.frames-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.frames frames)))

;;;; Frame Structure Tests

(deftest test-frame-struct-creation ()
  "Test basic frame struct creation and field access"
  (let ((frame (frames:make-http2-frame
                :type frames:+frame-data+
                :flags frames:+flag-end-stream+
                :stream-id 1
                :length 5
                :payload (make-array 5 :element-type '(unsigned-byte 8)
                                       :initial-element 42))))
    (assert-true (frames:http2-frame-p frame))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-data+))
    (assert-true (= (frames:http2-frame-flags frame) frames:+flag-end-stream+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    (assert-true (= (frames:http2-frame-length frame) 5))
    (assert-true (= (length (frames:http2-frame-payload frame)) 5))
    (assert-true (= (aref (frames:http2-frame-payload frame) 0) 42))))

(deftest test-frame-default-values ()
  "Test frame struct defaults"
  (let ((frame (frames:make-http2-frame)))
    (assert-true (= (frames:http2-frame-type frame) 0))
    (assert-true (= (frames:http2-frame-flags frame) 0))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    (assert-true (= (frames:http2-frame-length frame) 0))
    (assert-true (null (frames:http2-frame-payload frame)))))

;;;; Constants Tests

(deftest test-frame-type-constants ()
  "Test frame type constants match RFC 7540"
  (assert-true (= frames:+frame-data+ #x0))
  (assert-true (= frames:+frame-headers+ #x1))
  (assert-true (= frames:+frame-priority+ #x2))
  (assert-true (= frames:+frame-rst-stream+ #x3))
  (assert-true (= frames:+frame-settings+ #x4))
  (assert-true (= frames:+frame-push-promise+ #x5))
  (assert-true (= frames:+frame-ping+ #x6))
  (assert-true (= frames:+frame-goaway+ #x7))
  (assert-true (= frames:+frame-window-update+ #x8))
  (assert-true (= frames:+frame-continuation+ #x9)))

(deftest test-flag-constants ()
  "Test frame flag constants"
  (assert-true (= frames:+flag-end-stream+ #x1))
  (assert-true (= frames:+flag-end-headers+ #x4))
  (assert-true (= frames:+flag-padded+ #x8))
  (assert-true (= frames:+flag-priority+ #x20))
  (assert-true (= frames:+flag-ack+ #x1)))

(deftest test-settings-constants ()
  "Test settings parameter constants"
  (assert-true (= frames:+settings-header-table-size+ #x1))
  (assert-true (= frames:+settings-enable-push+ #x2))
  (assert-true (= frames:+settings-max-concurrent-streams+ #x3))
  (assert-true (= frames:+settings-initial-window-size+ #x4))
  (assert-true (= frames:+settings-max-frame-size+ #x5))
  (assert-true (= frames:+settings-max-header-list-size+ #x6)))

(deftest test-error-code-constants ()
  "Test error code constants"
  (assert-true (= frames:+error-no-error+ #x0))
  (assert-true (= frames:+error-protocol-error+ #x1))
  (assert-true (= frames:+error-internal-error+ #x2))
  (assert-true (= frames:+error-flow-control-error+ #x3))
  (assert-true (= frames:+error-settings-timeout+ #x4))
  (assert-true (= frames:+error-stream-closed+ #x5))
  (assert-true (= frames:+error-frame-size-error+ #x6))
  (assert-true (= frames:+error-refused-stream+ #x7))
  (assert-true (= frames:+error-cancel+ #x8))
  (assert-true (= frames:+error-compression-error+ #x9))
  (assert-true (= frames:+error-connect-error+ #xa))
  (assert-true (= frames:+error-enhance-your-calm+ #xb))
  (assert-true (= frames:+error-inadequate-security+ #xc))
  (assert-true (= frames:+error-http-1-1-required+ #xd)))

;;;; SETTINGS Frame Tests

(deftest test-make-settings-frame-ack ()
  "Test SETTINGS frame with ACK flag"
  (let ((frame (frames:make-settings-frame :ack t)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-settings+))
    (assert-true (= (frames:http2-frame-flags frame) frames:+flag-ack+))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    (assert-true (= (frames:http2-frame-length frame) 0))))

(deftest test-make-settings-frame-defaults ()
  "Test SETTINGS frame with default settings"
  (let ((frame (frames:make-settings-frame)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-settings+))
    (assert-true (= (frames:http2-frame-flags frame) 0))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    ;; Default settings: 3 entries * 6 bytes = 18
    (assert-true (= (frames:http2-frame-length frame) 18))
    (assert-true (= (length (frames:http2-frame-payload frame)) 18))))

(deftest test-make-settings-frame-custom ()
  "Test SETTINGS frame with custom settings"
  (let* ((settings (list (cons frames:+settings-max-frame-size+ 32768)))
         (frame (frames:make-settings-frame :initial-settings settings)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-settings+))
    ;; 1 setting * 6 bytes = 6
    (assert-true (= (frames:http2-frame-length frame) 6))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Setting ID (16-bit): +settings-max-frame-size+ = 5
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 5))
      ;; Value (32-bit): 32768 = #x00008000
      (assert-true (= (aref payload 2) 0))
      (assert-true (= (aref payload 3) 0))
      (assert-true (= (aref payload 4) #x80))
      (assert-true (= (aref payload 5) 0)))))

;;;; PING Frame Tests

(deftest test-make-ping-frame-default ()
  "Test PING frame with no data"
  (let ((frame (frames:make-ping-frame)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-ping+))
    (assert-true (= (frames:http2-frame-flags frame) 0))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    (assert-true (= (frames:http2-frame-length frame) 8))
    (assert-true (= (length (frames:http2-frame-payload frame)) 8))))

(deftest test-make-ping-frame-ack ()
  "Test PING frame with ACK flag"
  (let ((frame (frames:make-ping-frame :ack t)))
    (assert-true (= (frames:http2-frame-flags frame) frames:+flag-ack+))))

(deftest test-make-ping-frame-custom-data ()
  "Test PING frame with custom opaque data"
  (let* ((data (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-contents '(1 2 3 4 5 6 7 8)))
         (frame (frames:make-ping-frame :data data)))
    (assert-true (= (frames:http2-frame-length frame) 8))
    (assert-true (= (aref (frames:http2-frame-payload frame) 0) 1))
    (assert-true (= (aref (frames:http2-frame-payload frame) 7) 8))))

;;;; GOAWAY Frame Tests

(deftest test-make-goaway-frame-basic ()
  "Test GOAWAY frame creation"
  (let ((frame (frames:make-goaway-frame 7 frames:+error-no-error+)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-goaway+))
    (assert-true (= (frames:http2-frame-flags frame) 0))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    ;; 4 bytes last-stream-id + 4 bytes error code = 8
    (assert-true (= (frames:http2-frame-length frame) 8))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Last stream ID = 7 (31-bit)
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) 0))
      (assert-true (= (aref payload 3) 7))
      ;; Error code = 0
      (assert-true (= (aref payload 4) 0))
      (assert-true (= (aref payload 5) 0))
      (assert-true (= (aref payload 6) 0))
      (assert-true (= (aref payload 7) 0)))))

(deftest test-make-goaway-frame-with-debug-data ()
  "Test GOAWAY frame with debug data"
  (let ((frame (frames:make-goaway-frame 1 frames:+error-internal-error+ "oops")))
    ;; 8 bytes header + 4 bytes debug data
    (assert-true (= (frames:http2-frame-length frame) 12))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Error code = 2 (INTERNAL_ERROR)
      (assert-true (= (aref payload 7) 2))
      ;; Debug data starts at offset 8
      (assert-true (= (aref payload 8) (char-code #\o))))))

(deftest test-make-goaway-frame-large-stream-id ()
  "Test GOAWAY frame with large last-stream-id"
  (let* ((large-id (1- (ash 1 31)))  ; 2^31 - 1
         (frame (frames:make-goaway-frame large-id frames:+error-no-error+)))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Verify 31-bit encoding (high bit reserved)
      (assert-true (= (logand (aref payload 0) #x7f) #x7f))
      (assert-true (= (aref payload 1) #xff))
      (assert-true (= (aref payload 2) #xff))
      (assert-true (= (aref payload 3) #xff)))))

;;;; WINDOW_UPDATE Frame Tests

(deftest test-make-window-update-frame ()
  "Test WINDOW_UPDATE frame creation"
  (let ((frame (frames:make-window-update-frame 1 1024)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-window-update+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    (assert-true (= (frames:http2-frame-length frame) 4))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; 1024 = #x00000400
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) 4))
      (assert-true (= (aref payload 3) 0)))))

(deftest test-make-window-update-frame-connection-level ()
  "Test WINDOW_UPDATE frame on stream 0 (connection level)"
  (let ((frame (frames:make-window-update-frame 0 65535)))
    (assert-true (= (frames:http2-frame-stream-id frame) 0))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; 65535 = #x0000FFFF
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) #xff))
      (assert-true (= (aref payload 3) #xff)))))

;;;; RST_STREAM Frame Tests

(deftest test-make-rst-stream-frame ()
  "Test RST_STREAM frame creation"
  (let ((frame (frames:make-rst-stream-frame 3 frames:+error-cancel+)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-rst-stream+))
    (assert-true (= (frames:http2-frame-stream-id frame) 3))
    (assert-true (= (frames:http2-frame-length frame) 4))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Error code = 8 (CANCEL)
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) 0))
      (assert-true (= (aref payload 3) 8)))))

;;;; DATA Frame Tests

(deftest test-make-data-frame-string ()
  "Test DATA frame with string input"
  (let ((frame (frames:make-data-frame 1 "hello")))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-data+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    (assert-true (= (frames:http2-frame-flags frame) 0))
    (assert-true (= (frames:http2-frame-length frame) 5))
    (assert-true (= (aref (frames:http2-frame-payload frame) 0) (char-code #\h)))))

(deftest test-make-data-frame-bytes ()
  "Test DATA frame with byte vector input"
  (let* ((data (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(10 20 30)))
         (frame (frames:make-data-frame 1 data)))
    (assert-true (= (frames:http2-frame-length frame) 3))
    (assert-true (= (aref (frames:http2-frame-payload frame) 0) 10))))

(deftest test-make-data-frame-end-stream ()
  "Test DATA frame with END_STREAM flag"
  (let ((frame (frames:make-data-frame 1 "x" :end-stream t)))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-end-stream+))))

(deftest test-make-data-frame-padded ()
  "Test DATA frame with PADDED flag"
  (let ((frame (frames:make-data-frame 1 "x" :padded t)))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-padded+))))

;;;; HEADERS Frame Tests

(deftest test-make-headers-frame-basic ()
  "Test HEADERS frame creation"
  (let* ((headers (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame (frames:make-headers-frame 1 headers)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-headers+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    (assert-true (= (frames:http2-frame-length frame) 10))
    (assert-true (= (frames:http2-frame-flags frame) 0))))

(deftest test-make-headers-frame-flags ()
  "Test HEADERS frame with all flags"
  (let* ((headers (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame (frames:make-headers-frame 1 headers
                                           :end-stream t
                                           :end-headers t
                                           :priority t
                                           :padded t)))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-end-stream+))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-end-headers+))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-priority+))
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-padded+))))

;;;; PRIORITY Frame Tests

(deftest test-make-priority-frame-defaults ()
  "Test PRIORITY frame with default values"
  (let ((frame (frames:make-priority-frame 3)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-priority+))
    (assert-true (= (frames:http2-frame-stream-id frame) 3))
    (assert-true (= (frames:http2-frame-length frame) 5))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Default dependency = 0
      (assert-true (= (logand (aref payload 0) #x7f) 0))
      ;; Default weight = 16, stored as weight-1 = 15
      (assert-true (= (aref payload 4) 15)))))

(deftest test-make-priority-frame-custom ()
  "Test PRIORITY frame with custom dependency and weight"
  (let ((frame (frames:make-priority-frame 5 :dependency 3 :weight 32)))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Dependency = 3
      (assert-true (= (aref payload 3) 3))
      ;; Weight = 32, stored as weight-1 = 31
      (assert-true (= (aref payload 4) 31)))))

(deftest test-make-priority-frame-exclusive ()
  "Test PRIORITY frame with exclusive flag"
  (let ((frame (frames:make-priority-frame 5 :dependency 3 :exclusive t)))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; Exclusive flag is high bit of first byte
      (assert-true (logtest (aref payload 0) #x80)))))

(deftest test-make-priority-frame-not-exclusive ()
  "Test PRIORITY frame without exclusive flag"
  (let ((frame (frames:make-priority-frame 5 :dependency 3)))
    (let ((payload (frames:http2-frame-payload frame)))
      (assert-true (not (logtest (aref payload 0) #x80))))))

;;;; CONTINUATION Frame Tests

(deftest test-make-continuation-frame ()
  "Test CONTINUATION frame creation"
  (let* ((headers (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame (frames:make-continuation-frame 1 headers)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-continuation+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    (assert-true (= (frames:http2-frame-length frame) 8))
    (assert-true (= (frames:http2-frame-flags frame) 0))))

(deftest test-make-continuation-frame-end-headers ()
  "Test CONTINUATION frame with END_HEADERS flag"
  (let* ((headers (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
         (frame (frames:make-continuation-frame 1 headers :end-headers t)))
    (assert-true (= (frames:http2-frame-flags frame) frames:+flag-end-headers+))))

;;;; Frame Validation Tests

(deftest test-valid-frame-p-data ()
  "Test valid-frame-p for a normal DATA frame"
  (let ((frame (frames:make-data-frame 1 "test")))
    (assert-true (frames:valid-frame-p frame))))

(deftest test-valid-frame-p-connection-frame ()
  "Test valid-frame-p for connection-level frame (stream-id 0)"
  (let ((frame (frames:make-settings-frame :ack t)))
    (assert-true (frames:valid-frame-p frame))))

(deftest test-valid-frame-p-max-length ()
  "Test valid-frame-p accepts maximum valid length"
  (let ((frame (frames:make-http2-frame :length #xffffff :type 0)))
    (assert-true (frames:valid-frame-p frame))))

(deftest test-valid-frame-p-unknown-type ()
  "Test valid-frame-p rejects unknown frame type"
  (let ((frame (frames:make-http2-frame :type 255 :stream-id 1)))
    (assert-true (not (frames:valid-frame-p frame)))))
