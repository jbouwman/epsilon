;;;; Tests for HTTP Streaming

(defpackage epsilon.http.streaming.tests
  (:use cl epsilon.syntax epsilon.test)
  (:import (epsilon.http.streaming streaming)
           (epsilon.channel channel)
           (epsilon.map map)
           (epsilon.string str)
           (epsilon.http.pool pool)))

;;;; Configuration Tests

(deftest test-streaming-config ()
  "Test streaming configuration"
  (let ((config (streaming:make-streaming-config
                 :chunk-size 4096
                 :buffer-size 32768
                 :timeout 60)))

    (assert-true (streaming:streaming-config-p config))
    (assert-= (streaming:streaming-config-chunk-size config) 4096)
    (assert-= (streaming:streaming-config-buffer-size config) 32768)
    (assert-= (streaming:streaming-config-timeout config) 60)))

;;;; Chunked Encoding Tests

(deftest test-chunk-encoding ()
  "Test HTTP chunked encoding format"
  (with-output-to-string (stream)
    ;; Test writing chunks
    (streaming::write-http-chunk stream "Hello")
    (streaming::write-http-chunk stream " World")
    (streaming::write-http-chunk stream nil) ; End chunk

    (let ((output (get-output-stream-string stream)))
      ;; Should contain hex length + CRLF + data + CRLF
      (assert-true (search "5" output)) ; Hex for 5 (length of "Hello")
      (assert-true (search "Hello" output))
      (assert-true (search "6" output)) ; Hex for 6 (length of " World")
      (assert-true (search " World" output))
      (assert-true (search "0" output))))) ; Final chunk

(deftest test-chunk-parsing ()
  "Test parsing chunked HTTP response"
  ;; Build a chunked response using write-http-chunk, then parse it back
  (let ((chunked-data (with-output-to-string (out)
                        (streaming::write-http-chunk out "Hello")
                        (streaming::write-http-chunk out " World")
                        (streaming::write-http-chunk out nil))))
    (with-input-from-string (stream chunked-data)
      (let ((conn (pool:make-http-connection :stream stream)))
        ;; First chunk
        (multiple-value-bind (chunk more-p)
            (streaming::read-next-chunk conn)
          (assert-true more-p)
          (assert-equal chunk "Hello"))
        ;; Second chunk
        (multiple-value-bind (chunk more-p)
            (streaming::read-next-chunk conn)
          (assert-true more-p)
          (assert-equal chunk " World"))
        ;; End of chunks
        (multiple-value-bind (chunk more-p)
            (streaming::read-next-chunk conn)
          (assert-true (null chunk))
          (assert-true (null more-p)))))))

;;;; Server-Sent Events Tests

(deftest test-sse-event-creation ()
  "Test SSE event structure"
  (let ((event (streaming:make-sse-event
                :type "update"
                :data "Hello SSE"
                :id "123"
                :retry 1000)))

    (assert-true (streaming:sse-event-p event))
    (assert-true (string= (streaming:sse-event-type event) "update"))
    (assert-true (string= (streaming:sse-event-data event) "Hello SSE"))
    (assert-true (string= (streaming:sse-event-id event) "123"))
    (assert-= (streaming:sse-event-retry event) 1000)))

(deftest test-sse-formatting ()
  "Test SSE event formatting"
  (let ((event (streaming:make-sse-event
                :type "update"
                :data "Hello SSE"
                :id "42"
                :retry 3000)))
    (let ((output (with-output-to-string (s)
                    (streaming:write-sse-event s event))))
      ;; Should contain all fields
      (assert-true (search "event: update" output))
      (assert-true (search "id: 42" output))
      (assert-true (search "retry: 3000" output))
      (assert-true (search "data: Hello SSE" output))
      ;; Should end with double newline (empty line terminates event)
      (let ((last-two (subseq output (- (length output) 2))))
        (assert-true (char= (char last-two 0) #\Linefeed))
        (assert-true (char= (char last-two 1) #\Linefeed))))))

(deftest test-sse-multiline-data ()
  "Test SSE events with multiline data"
  (let* ((multiline-data (format nil "line one~Cline two~Cline three" #\Newline #\Newline))
         (event (streaming:make-sse-event
                 :type "message"
                 :data multiline-data)))
    (let ((output (with-output-to-string (s)
                    (streaming:write-sse-event s event))))
      ;; Each line should be prefixed with "data: "
      (assert-true (search "data: line one" output))
      (assert-true (search "data: line two" output))
      (assert-true (search "data: line three" output))
      ;; Verify roundtrip: write then parse
      (let ((parsed (with-input-from-string (s (concatenate 'string output))
                      (streaming:read-sse-event s))))
        (assert-true parsed)
        (assert-true (string= (streaming:sse-event-type parsed) "message"))
        (assert-true (string= (streaming:sse-event-data parsed) multiline-data))))))

(deftest test-sse-parsing ()
  "Test parsing SSE events from stream"
  ;; Basic event parsing
  (let* ((sse-text (format nil "event: notify~Cid: 7~Cretry: 5000~Cdata: payload~C~C"
                           #\Linefeed #\Linefeed #\Linefeed #\Linefeed #\Linefeed))
         (event (with-input-from-string (s sse-text)
                  (streaming:read-sse-event s))))
    (assert-true event)
    (assert-true (string= (streaming:sse-event-type event) "notify"))
    (assert-true (string= (streaming:sse-event-id event) "7"))
    (assert-= (streaming:sse-event-retry event) 5000)
    (assert-true (string= (streaming:sse-event-data event) "payload")))

  ;; Comment lines should be skipped
  (let* ((sse-text (format nil ": this is a comment~Cdata: visible~C~C"
                           #\Linefeed #\Linefeed #\Linefeed))
         (event (with-input-from-string (s sse-text)
                  (streaming:read-sse-event s))))
    (assert-true event)
    (assert-true (string= (streaming:sse-event-data event) "visible")))

  ;; Empty stream returns event with defaults (no fields were parsed)
  (let ((event (with-input-from-string (s "")
                 (streaming:read-sse-event s))))
    ;; The event has default type "message" and empty data "",
    ;; so it's not nil, but no meaningful data was parsed
    (assert-true (string= "" (streaming:sse-event-data event)))
    (assert-true (null (streaming:sse-event-id event)))))

;;;; Download Progress Tests

(deftest test-download-progress ()
  "Test download progress tracking"
  (let ((progress (streaming:make-download-progress
                   :bytes-downloaded 1024
                   :total-bytes 4096
                   :start-time 1000
                   :current-time 1010
                   :rate 102.4)))

    (assert-true (streaming:download-progress-p progress))
    (assert-= (streaming:download-progress-bytes-downloaded progress) 1024)
    (assert-= (streaming:download-progress-total-bytes progress) 4096)
    (assert-true (= (streaming:download-progress-rate progress) 102.4))))

;;;; Stream Utilities Tests

(deftest test-copy-stream-mock ()
  "Test stream copying utility (mocked)"
  (let ((input-data "Hello, World!"))
    (with-input-from-string (input input-data)
      (with-output-to-string (output)
        (let ((bytes-copied (streaming:copy-stream input output :buffer-size 5)))
          (assert-= bytes-copied (length input-data))
          (assert-true (string= (get-output-stream-string output) input-data)))))))

;;;; Configuration and Error Tests

(deftest test-streaming-error-conditions ()
  "Test streaming error handling structures"
  ;; Test that error condition types exist
  (assert-true (subtypep 'streaming:streaming-config 'structure-object))
  (assert-true (subtypep 'streaming:sse-event 'structure-object))
  (assert-true (subtypep 'streaming:download-progress 'structure-object)))

;;;; Integration Structure Tests

(deftest test-streaming-integration-api ()
  "Test streaming integration API structure"
  ;; Test that required functions exist
  (assert-true (fboundp 'streaming:create-streaming-request))
  (assert-true (fboundp 'streaming:stream-request-body))
  (assert-true (fboundp 'streaming:finish-request-stream))
  (assert-true (fboundp 'streaming:read-response-chunk))
  (assert-true (fboundp 'streaming:close-response-stream))

  ;; Test file streaming functions
  (assert-true (fboundp 'streaming:stream-file-download))
  (assert-true (fboundp 'streaming:stream-file-upload))

  ;; Test utility functions
  (assert-true (fboundp 'streaming:copy-stream))
  (assert-true (fboundp 'streaming:tee-stream)))

;;;; Mock Streaming Request Tests

(deftest test-streaming-request-structure ()
  "Test streaming request structure"
  (let ((req (streaming:make-streaming-request
              :method "POST"
              :path "/upload"
              :headers (map:make-map "content-type" "application/octet-stream")
              :config (streaming:make-streaming-config :chunk-size 1024))))
    (assert-true (streaming:streaming-request-p req))
    (assert-equal (streaming:streaming-request-method req) "POST")
    (assert-equal (streaming:streaming-request-path req) "/upload")
    (assert-not (streaming:streaming-request-finished-p req))
    (assert-= (streaming:streaming-config-chunk-size
               (streaming:streaming-request-config req))
              1024)))

;;;; Mock Streaming Response Tests

(deftest test-streaming-response-structure ()
  "Test streaming response structure"
  (let ((resp (streaming:make-streaming-response
               :status 200
               :headers (map:make-map "transfer-encoding" "chunked")
               :chunked-p t
               :content-length nil)))
    (assert-true (streaming:streaming-response-p resp))
    (assert-= (streaming:streaming-response-status resp) 200)
    (assert-true (streaming:streaming-response-chunked-p resp))
    (assert-not (streaming:streaming-response-finished-p resp))
    (assert-= (streaming:streaming-response-bytes-read resp) 0)))

;;;; Backpressure and Flow Control Tests

(deftest test-streaming-config-backpressure ()
  "Test streaming configuration for backpressure scenarios"
  (let ((config (streaming:make-streaming-config
                 :chunk-size 1024
                 :buffer-size 65536
                 :max-chunk-size (* 1024 1024))))

    ;; Test reasonable defaults for backpressure
    (assert-true (> (streaming:streaming-config-buffer-size config)
           (streaming:streaming-config-chunk-size config)))
    (assert-true (> (streaming:streaming-config-max-chunk-size config)
           (streaming:streaming-config-buffer-size config)))))

;;;; Performance Tests (Structure)

(deftest test-streaming-performance-structure ()
  "Test streaming performance-related structures"
  (let ((large-config (streaming:make-streaming-config
                       :chunk-size (* 64 1024)      ; 64KB chunks
                       :buffer-size (* 1024 1024)   ; 1MB buffer
                       :max-chunk-size (* 10 1024 1024)))) ; 10MB max

    ;; Test configuration supports high-performance scenarios
    (assert-true (>= (streaming:streaming-config-chunk-size large-config) 65536))
    (assert-true (>= (streaming:streaming-config-buffer-size large-config) 1048576))
    (assert-true (>= (streaming:streaming-config-max-chunk-size large-config) 10485760))))

;;;; Channel Integration Tests

(deftest test-channel-stream-integration ()
  "Test integration with channel streams"
  ;; Test that streaming can work with channel-based streams
  (let ((ch (channel:make-channel :capacity 5)))

    ;; Send some data
    (channel:send ch "chunk1")
    (channel:send ch "chunk2")
    (channel:close-channel ch)

    ;; Create stream from channel
    (let ((stream (channel:from-channel ch)))

      ;; Should be able to read data
      (multiple-value-bind (chunk1 more1-p) (channel:stream-next stream)
        (assert-true more1-p)
        (assert-true (string= chunk1 "chunk1")))

      (multiple-value-bind (chunk2 more2-p) (channel:stream-next stream)
        (assert-true more2-p)
        (assert-true (string= chunk2 "chunk2")))

      ;; Should be finished
      (multiple-value-bind (chunk3 more3-p) (channel:stream-next stream)
        (assert-true (not more3-p))
        (assert-true (null chunk3))))))
