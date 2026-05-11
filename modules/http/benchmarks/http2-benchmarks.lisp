;;;; HTTP/2 Performance Benchmarks
;;;;
;;;; Benchmarks for measuring HTTP/2 protocol implementation performance.
;;;; Uses the epsilon.benchmark framework for consistent measurement.

(defpackage :epsilon.http.h2.benchmarks
  (:use :cl)
  (:import
   (epsilon.benchmark bench)
   (epsilon.http.h2.frames frames)
   (epsilon.http.h2.hpack hpack))
  (:export
   register-http2-benchmarks
   run-all-benchmarks
   quick-performance-test))

;;;; Frame Creation Benchmarks

(defun register-frame-benchmarks ()
  "Register HTTP/2 frame creation benchmarks"

  ;; DATA frame creation
  (bench:defbenchmark http2/frame/data-1kb ()
    (let ((payload (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (frames:make-data-frame 1 payload))))

  ;; HEADERS frame creation
  (bench:defbenchmark http2/frame/headers ()
    (let ((header-block (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
      (bench:consume (frames:make-headers-frame 1 header-block))))

  ;; SETTINGS frame creation
  (bench:defbenchmark http2/frame/settings ()
    (bench:consume (frames:make-settings-frame
                    `((,frames:+settings-max-concurrent-streams+ . 100)
                      (,frames:+settings-initial-window-size+ . 65535)))))

  ;; PING frame creation
  (bench:defbenchmark http2/frame/ping ()
    (let ((data (make-array 8 :element-type '(unsigned-byte 8) :initial-element 42)))
      (bench:consume (frames:make-ping-frame data))))

  ;; WINDOW_UPDATE frame creation
  (bench:defbenchmark http2/frame/window-update ()
    (bench:consume (frames:make-window-update-frame 1 65535)))

  ;; RST_STREAM frame creation
  (bench:defbenchmark http2/frame/rst-stream ()
    (bench:consume (frames:make-rst-stream-frame 1 frames:+error-cancel+)))

  ;; GOAWAY frame creation
  (bench:defbenchmark http2/frame/goaway ()
    (bench:consume (frames:make-goaway-frame 0 frames:+error-no-error+))))

;;;; HPACK Encoder/Decoder Benchmarks

(defun register-hpack-benchmarks ()
  "Register HPACK header compression benchmarks"

  ;; Encoder creation
  (bench:defbenchmark http2/hpack/create-encoder ()
    (bench:consume (hpack:make-encoder)))

  ;; Decoder creation
  (bench:defbenchmark http2/hpack/create-decoder ()
    (bench:consume (hpack:make-decoder)))

  ;; Encode simple request headers
  (bench:defbenchmark http2/hpack/encode-request ()
    (let ((encoder (hpack:make-encoder)))
      (bench:consume
       (hpack:encode-header-list
        encoder
        '((":method" . "GET")
          (":path" . "/")
          (":scheme" . "https")
          (":authority" . "www.example.com"))))))

  ;; Encode request with custom headers
  (bench:defbenchmark http2/hpack/encode-with-custom ()
    (let ((encoder (hpack:make-encoder)))
      (bench:consume
       (hpack:encode-header-list
        encoder
        '((":method" . "POST")
          (":path" . "/api/users")
          (":scheme" . "https")
          (":authority" . "api.example.com")
          ("content-type" . "application/json")
          ("authorization" . "Bearer token123456789")
          ("accept" . "application/json")
          ("user-agent" . "epsilon.http/2.0"))))))

  ;; Encode response headers
  (bench:defbenchmark http2/hpack/encode-response ()
    (let ((encoder (hpack:make-encoder)))
      (bench:consume
       (hpack:encode-header-list
        encoder
        '((":status" . "200")
          ("content-type" . "application/json")
          ("content-length" . "1234")
          ("cache-control" . "no-cache"))))))

  ;; Decode header block
  (bench:defbenchmark http2/hpack/decode ()
    (let* ((encoder (hpack:make-encoder))
           (decoder (hpack:make-decoder))
           (encoded (hpack:encode-header-list
                     encoder
                     '((":method" . "GET")
                       (":path" . "/")
                       (":scheme" . "https")
                       (":authority" . "www.example.com")))))
      (bench:consume (hpack:decode-header-block decoder encoded)))))

;;;; Huffman Coding Benchmarks

(defun register-huffman-benchmarks ()
  "Register Huffman encoding/decoding benchmarks"

  ;; Short string encoding
  (bench:defbenchmark http2/huffman/encode-short ()
    (bench:consume (hpack:huffman-encode "www.example.com")))

  ;; Medium string encoding
  (bench:defbenchmark http2/huffman/encode-medium ()
    (bench:consume (hpack:huffman-encode "application/json; charset=utf-8")))

  ;; Long string encoding
  (bench:defbenchmark http2/huffman/encode-long ()
    (bench:consume
     (hpack:huffman-encode
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")))

  ;; Huffman decoding
  (bench:defbenchmark http2/huffman/decode ()
    (let ((encoded (hpack:huffman-encode "www.example.com")))
      (bench:consume (hpack:huffman-decode encoded)))))

;;;; Performance Budgets

(defun define-http2-budgets ()
  "Define performance budgets for CI"
  ;; Frame creation should be fast (sub-microsecond)
  (bench:defbudget "http2/frame/data-1kb" :max-time 0.000005)      ; 5us
  (bench:defbudget "http2/frame/settings" :max-time 0.000002)      ; 2us
  (bench:defbudget "http2/frame/ping" :max-time 0.000001)          ; 1us

  ;; HPACK encoding should be efficient
  (bench:defbudget "http2/hpack/encode-request" :max-time 0.000010)  ; 10us
  (bench:defbudget "http2/hpack/decode" :max-time 0.000010)          ; 10us

  ;; Huffman encoding
  (bench:defbudget "http2/huffman/encode-short" :max-time 0.000005)) ; 5us

;;;; Main Registration

(defun register-http2-benchmarks ()
  "Register all HTTP/2 benchmarks with the framework"
  (register-frame-benchmarks)
  (register-hpack-benchmarks)
  (register-huffman-benchmarks)
  (define-http2-budgets))

;;;; Entry Points

(defun run-all-benchmarks ()
  "Run all HTTP/2 benchmarks"
  (format t "~%========================================~%")
  (format t "     HTTP/2 Performance Benchmarks~%")
  (format t "========================================~%")

  (register-http2-benchmarks)

  (let ((benchmarks (bench:list-benchmarks)))
    (dolist (name benchmarks)
      (when (search "http2/" (string name))
        (let ((fn (bench:get-benchmark name)))
          (when fn
            (format t "~%Running: ~A~%" name)
            (let ((result (bench:run-benchmark fn :name (string name))))
              (bench:format-benchmark-result result)))))))

  (format t "~%========================================~%")
  (format t "     Benchmarks Complete~%")
  (format t "========================================~%"))

(defun quick-performance-test ()
  "Quick performance test for CI/CD with budget checking"
  (format t "~%Running quick HTTP/2 performance test...~%")

  (register-http2-benchmarks)

  (let ((results nil)
        (critical-benchmarks '(http2/frame/data-1kb
                               http2/frame/settings
                               http2/hpack/encode-request
                               http2/hpack/decode
                               http2/huffman/encode-short)))
    (dolist (name critical-benchmarks)
      (let ((fn (bench:get-benchmark name)))
        (when fn
          (format t "Running: ~A~%" name)
          (let ((result (bench:run-benchmark fn :name (string name))))
            (bench:format-benchmark-result result)
            (push result results)))))

    (bench:check-budgets (nreverse results))))
