;;;; HTTP Performance Benchmarks
;;;;
;;;; Benchmarks for measuring HTTP/1.1 implementation performance.
;;;; Uses the epsilon.benchmark framework for consistent measurement.

(defpackage :epsilon.http.benchmarks
  (:use :cl)
  (:import
   (epsilon.http http)
   (epsilon.http.request request)
   (epsilon.http.response response)
   (epsilon.http.connection-pool pool)
   (epsilon.http.streaming streaming)
   (epsilon.http.security security)
   (epsilon.http.retry retry)
   (epsilon.map map)
   (epsilon.benchmark bench))
  (:export
   register-http-benchmarks
   run-all-benchmarks
   quick-performance-test))

;;;; Test Data Generators

(defun generate-test-request-string (body-size)
  "Generate a raw HTTP request string with specified body size"
  (let ((body (make-string body-size :initial-element #\a)))
    (format nil "POST /api/users HTTP/1.1~C~C\
Host: api.example.com~C~C\
Content-Type: application/json~C~C\
Content-Length: ~D~C~C\
Authorization: Bearer token123~C~C\
Accept: application/json~C~C\
User-Agent: epsilon.http/2.0~C~C\
~C~C~A"
            #\Return #\Newline
            #\Return #\Newline
            #\Return #\Newline
            body-size #\Return #\Newline
            #\Return #\Newline
            #\Return #\Newline
            #\Return #\Newline
            #\Return #\Newline
            body)))

(defun generate-test-headers (count)
  "Generate a map with specified number of headers"
  (let ((headers map:+empty+))
    (loop for i from 1 to count
          do (setf headers (map:assoc headers
                                      (format nil "X-Custom-Header-~D" i)
                                      (format nil "value-~D-with-some-content" i))))
    headers))

;;;; Request/Response Benchmarks

(defun register-request-benchmarks ()
  "Register request creation and parsing benchmarks"

  ;; Request object creation
  (bench:defbenchmark http/request/create ()
    (bench:consume
     (request:make-request "GET" "/api/users"
                           :headers (map:make-map "Host" "example.com"))))

  ;; Request with headers
  (bench:defbenchmark http/request/create-with-headers ()
    (let ((headers (map:make-map
                    "Host" "api.example.com"
                    "Content-Type" "application/json"
                    "Authorization" "Bearer token123"
                    "Accept" "application/json")))
      (bench:consume
       (request:make-request "POST" "/api/users"
                             :headers headers
                             :body "{\"name\":\"test\"}"))))

  ;; Parse simple GET request
  (bench:defbenchmark http/request/parse-simple ()
    (let ((req-string (format nil "GET /api/users HTTP/1.1~C~CHost: example.com~C~C~C~C"
                              #\Return #\Newline
                              #\Return #\Newline
                              #\Return #\Newline)))
      (bench:consume (request:parse-http-request req-string))))

  ;; Parse POST request with body
  (bench:defbenchmark http/request/parse-with-body ()
    (let ((req-string (generate-test-request-string 256)))
      (bench:consume (request:parse-http-request req-string)))))

(defun register-response-benchmarks ()
  "Register response creation and serialization benchmarks"

  ;; Response object creation
  (bench:defbenchmark http/response/create ()
    (bench:consume
     (response:make-response :status 200
                             :headers (map:make-map "Content-Type" "text/plain")
                             :body "Hello, World!")))

  ;; JSON response creation
  (bench:defbenchmark http/response/json ()
    (bench:consume
     (response:json-response '(:id 1 :name "test" :active t))))

  ;; HTML response creation
  (bench:defbenchmark http/response/html ()
    (bench:consume
     (response:html-response "<html><body><h1>Hello</h1></body></html>")))

  ;; Response to string serialization
  (bench:defbenchmark http/response/serialize ()
    (let ((resp (response:make-response
                 :status 200
                 :headers (map:make-map "Content-Type" "application/json")
                 :body "{\"id\":1,\"name\":\"test\"}")))
      (bench:consume (response:response-to-string resp))))

  ;; Status text lookup
  (bench:defbenchmark http/response/status-text ()
    (bench:consume (response:status-text 200))
    (bench:consume (response:status-text 404))
    (bench:consume (response:status-text 500))))

;;;; URL Benchmarks

(defun register-url-benchmarks ()
  "Register URL parsing and encoding benchmarks"

  ;; URL encoding
  (bench:defbenchmark http/url/encode ()
    (bench:consume
     (request:url-encode "hello world!@#$%^&*()=+[]{}")))

  ;; URL encoding long string
  (bench:defbenchmark http/url/encode-long ()
    (let ((long-string (make-string 100 :initial-element #\a)))
      (bench:consume (request:url-encode long-string))))

  ;; URL decoding
  (bench:defbenchmark http/url/decode ()
    (bench:consume
     (request:url-decode "hello+world%21%40%23%24%25%5E%26%2A")))

  ;; Query string parsing (small)
  (bench:defbenchmark http/url/query-parse-small ()
    (bench:consume
     (request:parse-query-string "page=1&limit=20")))

  ;; Query string parsing (large)
  (bench:defbenchmark http/url/query-parse-large ()
    (let ((query (format nil "~{~A=~A~^&~}"
                         (loop for i from 1 to 10
                               collect (format nil "param~D" i)
                               collect (format nil "value~D" i)))))
      (bench:consume (request:parse-query-string query)))))

;;;; Header Benchmarks

(defun register-header-benchmarks ()
  "Register header manipulation benchmarks"

  ;; Add single header
  (bench:defbenchmark http/headers/add-single ()
    (let ((req (request:make-request "GET" "/test")))
      (bench:consume (request:add-header req "X-Custom" "value"))))

  ;; Add multiple headers
  (bench:defbenchmark http/headers/add-multiple ()
    (let ((req (request:make-request "GET" "/test")))
      (loop for i from 1 to 10
            do (request:add-header req
                                   (format nil "X-Header-~D" i)
                                   (format nil "value-~D" i)))
      (bench:consume req)))

  ;; Set response header
  (bench:defbenchmark http/headers/set-response ()
    (let ((resp (response:make-response)))
      (bench:consume (response:set-header resp "X-Custom" "value")))))

;;;; Connection Pool Benchmarks

(defun register-pool-benchmarks ()
  "Register connection pool operation benchmarks"

  ;; Pool creation
  (bench:defbenchmark http/pool/create ()
    (bench:consume (pool:create-connection-pool)))

  ;; Pool key generation
  (bench:defbenchmark http/pool/key ()
    (bench:consume (pool:pool-key "api.example.com" 443)))

  ;; Pool stats creation
  (bench:defbenchmark http/pool/stats ()
    (bench:consume (pool:make-pool-stats)))

  ;; Pooled connection struct creation
  (bench:defbenchmark http/pool/connection-struct ()
    (bench:consume
     (pool:make-pooled-connection
      :host "api.example.com"
      :port 443
      :ssl-p t
      :created-time (get-universal-time)
      :last-used-time (get-universal-time)))))

;;;; Circuit Breaker Benchmarks

(defun register-circuit-breaker-benchmarks ()
  "Register circuit breaker operation benchmarks"

  ;; Circuit breaker creation
  (bench:defbenchmark http/circuit/create ()
    (bench:consume (retry:make-circuit-breaker)))

  ;; Check circuit state (closed)
  (bench:defbenchmark http/circuit/check-closed ()
    (let ((breaker (retry:make-circuit-breaker)))
      (bench:consume (retry:circuit-closed-p breaker))))

  ;; Check circuit state (open)
  (bench:defbenchmark http/circuit/check-open ()
    (let ((breaker (retry:make-circuit-breaker)))
      (bench:consume (retry:circuit-open-p breaker))))

  ;; Record success
  (bench:defbenchmark http/circuit/record-success ()
    (let ((breaker (retry:make-circuit-breaker)))
      (bench:consume (retry:record-success breaker))))

  ;; Record failure
  (bench:defbenchmark http/circuit/record-failure ()
    (let ((breaker (retry:make-circuit-breaker)))
      (bench:consume (retry:record-failure breaker)))))

;;;; Retry Policy Benchmarks

(defun register-retry-benchmarks ()
  "Register retry policy benchmarks"

  ;; Retry policy creation
  (bench:defbenchmark http/retry/create-policy ()
    (bench:consume (retry:make-retry-policy)))

  ;; Exponential backoff calculation
  (bench:defbenchmark http/retry/exponential-backoff ()
    (let ((policy (retry:make-retry-policy)))
      (bench:consume (retry:exponential-backoff 3 policy))))

  ;; Linear backoff calculation
  (bench:defbenchmark http/retry/linear-backoff ()
    (let ((policy (retry:make-retry-policy)))
      (bench:consume (retry:linear-backoff 3 policy))))

  ;; Fixed delay calculation
  (bench:defbenchmark http/retry/fixed-delay ()
    (let ((policy (retry:make-retry-policy)))
      (bench:consume (retry:fixed-delay 3 policy)))))

;;;; Middleware Benchmarks

(defun register-middleware-benchmarks ()
  "Register middleware chain benchmarks"

  ;; Security headers middleware creation
  (bench:defbenchmark http/middleware/security-headers-create ()
    (bench:consume
     (security:security-headers-middleware
      (lambda (req) (response:make-response)) :headers security:*default-security-headers*)))

  ;; CORS middleware creation
  (bench:defbenchmark http/middleware/cors-create ()
    (bench:consume
     (security:cors-middleware
      (lambda (req) (response:make-response)))))

  ;; Security headers middleware execution
  (bench:defbenchmark http/middleware/security-headers-exec ()
    (let ((handler (security:security-headers-middleware
                    (lambda (req) (response:make-response :status 200)))))
      (let ((req (request:make-request "GET" "/test")))
        (bench:consume (funcall handler req))))))

;;;; Streaming Configuration Benchmarks

(defun register-streaming-benchmarks ()
  "Register streaming configuration benchmarks"

  ;; Streaming config creation
  (bench:defbenchmark http/streaming/config-create ()
    (bench:consume (streaming:make-streaming-config)))

  ;; Streaming request struct creation
  (bench:defbenchmark http/streaming/request-struct ()
    (bench:consume
     (streaming:make-streaming-request
      :method "POST"
      :path "/upload"
      :headers (map:make-map "Content-Type" "application/octet-stream")))))

;;;; Parameterized Benchmark Groups

(defun run-request-size-benchmarks ()
  "Run request parsing with various body sizes"
  (bench:with-benchmark-group "http/request-sizes"
    (dolist (size '(0 256 1024 4096))
      (let ((req-string (generate-test-request-string size)))
        (bench:benchmark-with-input ((format nil "parse-~D" size)
                                     :input req-string
                                     :throughput (bench:throughput-bytes (+ size 200)))
          (bench:consume (request:parse-http-request bench::input)))))))

(defun run-header-count-benchmarks ()
  "Run header operations with various header counts"
  (bench:with-benchmark-group "http/header-counts"
    (dolist (count '(5 10 25 50))
      (let ((headers (generate-test-headers count)))
        (bench:benchmark-with-input ((format nil "serialize-~D-headers" count)
                                     :input headers
                                     :throughput (bench:throughput-elements count))
          (let ((resp (response:make-response :headers bench::input)))
            (bench:consume (response:response-to-string resp))))))))

(defun run-url-encoding-benchmarks ()
  "Run URL encoding with various string lengths"
  (bench:with-benchmark-group "http/url-encoding"
    (dolist (length '(10 50 100 500))
      (let ((str (make-string length :initial-element #\a)))
        (bench:benchmark-with-input ((format nil "encode-~D" length)
                                     :input str
                                     :throughput (bench:throughput-bytes length))
          (bench:consume (request:url-encode bench::input)))))))

;;;; Registration

(defun register-http-benchmarks ()
  "Register all HTTP benchmarks with the framework"
  (register-request-benchmarks)
  (register-response-benchmarks)
  (register-url-benchmarks)
  (register-header-benchmarks)
  (register-pool-benchmarks)
  (register-circuit-breaker-benchmarks)
  (register-retry-benchmarks)
  (register-middleware-benchmarks)
  (register-streaming-benchmarks))

;;;; Performance Budgets

(defun define-http-performance-budgets ()
  "Define performance budgets for CI gating"

  ;; Critical path operations - strict budgets
  (bench:defbudget "HTTP/REQUEST/CREATE" :max-time 0.000005)           ; 5us
  (bench:defbudget "HTTP/RESPONSE/CREATE" :max-time 0.000005)          ; 5us
  (bench:defbudget "HTTP/URL/ENCODE" :max-time 0.000010)               ; 10us
  (bench:defbudget "HTTP/URL/DECODE" :max-time 0.000010)               ; 10us
  (bench:defbudget "HTTP/POOL/KEY" :max-time 0.000002)                 ; 2us
  (bench:defbudget "HTTP/CIRCUIT/CHECK-CLOSED" :max-time 0.000002)     ; 2us

  ;; Parsing operations - moderate budgets
  (bench:defbudget "HTTP/REQUEST/PARSE-SIMPLE" :max-time 0.000020)     ; 20us
  (bench:defbudget "HTTP/REQUEST/PARSE-WITH-BODY" :max-time 0.000050)  ; 50us
  (bench:defbudget "HTTP/URL/QUERY-PARSE-SMALL" :max-time 0.000010)    ; 10us
  (bench:defbudget "HTTP/URL/QUERY-PARSE-LARGE" :max-time 0.000030)    ; 30us

  ;; Response operations
  (bench:defbudget "HTTP/RESPONSE/SERIALIZE" :max-time 0.000020)       ; 20us
  (bench:defbudget "HTTP/RESPONSE/JSON" :max-time 0.000050)            ; 50us

  ;; Middleware operations
  (bench:defbudget "HTTP/MIDDLEWARE/SECURITY-HEADERS-EXEC" :max-time 0.000020))  ; 20us

;;;; Main Benchmark Runner

(defun run-all-benchmarks ()
  "Run all HTTP benchmarks"
  (format t "~%========================================~%")
  (format t "     HTTP/1.1 Performance Benchmarks~%")
  (format t "========================================~%")

  ;; Register all benchmarks
  (register-http-benchmarks)

  ;; Run individual registered benchmarks
  (let ((benchmarks (bench:list-benchmarks)))
    (dolist (name benchmarks)
      (when (search "http/" (string name))
        (let ((fn (bench:get-benchmark name)))
          (when fn
            (format t "~%Running: ~A~%" name)
            (let ((result (bench:run-benchmark fn :name (string name))))
              (bench:format-benchmark-result result)))))))

  ;; Run parameterized groups
  (format t "~%--- Parameterized Benchmarks ---~%")
  (run-request-size-benchmarks)
  (run-header-count-benchmarks)
  (run-url-encoding-benchmarks)

  (format t "~%========================================~%")
  (format t "     Benchmarks Complete~%")
  (format t "========================================~%"))

;;;; Quick Performance Test

(defun quick-performance-test ()
  "Quick performance test for CI/CD with budget checking"
  (format t "~%Running quick HTTP/1.1 performance test...~%")

  ;; Define performance budgets
  (define-http-performance-budgets)

  ;; Register and run critical benchmarks
  (register-request-benchmarks)
  (register-response-benchmarks)
  (register-url-benchmarks)
  (register-pool-benchmarks)
  (register-circuit-breaker-benchmarks)

  (let ((results nil)
        (critical-benchmarks '(http/request/create
                               http/response/create
                               http/url/encode
                               http/pool/key
                               http/circuit/check-closed
                               http/request/parse-simple)))
    (dolist (name critical-benchmarks)
      (let ((fn (bench:get-benchmark name)))
        (when fn
          (format t "Running: ~A~%" name)
          (let ((result (bench:run-benchmark fn :name (string name))))
            (bench:format-benchmark-result result)
            (push result results)))))

    ;; Check against budgets
    (bench:check-budgets (nreverse results))))

;;;; HTTP/1.1 vs HTTP/2 Comparison (for later use)

(defun compare-http1-vs-http2 ()
  "Compare HTTP/1.1 with HTTP/2 header sizes (stub for integration)"
  (format t "~%=== HTTP/1.1 vs HTTP/2 Comparison ===~%")

  (let* ((http1-request "GET /index.html HTTP/1.1\r\n\
Host: www.example.com\r\n\
User-Agent: Mozilla/5.0\r\n\
Accept: text/html,application/xhtml+xml\r\n\
Accept-Language: en-US,en;q=0.5\r\n\
Accept-Encoding: gzip, deflate, br\r\n\
Connection: keep-alive\r\n\r\n"))

    (format t "HTTP/1.1 request size: ~D bytes~%" (length http1-request))
    (format t "(HTTP/2 comparison available when modules are unified)~%")))
