;;;; Chunked Transfer-Encoding Tests
;;;;
;;;; Tests for HTTP client chunked transfer-encoding support.
;;;; Uses in-process raw TCP servers that send chunked responses.

(defpackage :epsilon.http.chunked.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.client client)
   (epsilon.http.response response)
   (epsilon.http.test-helpers helpers)
   (epsilon.net net)
   (epsilon.map map)
   (epsilon.sys.thread thread)))

;;; Raw TCP server that sends chunked responses

(defun send-bytes-and-close (socket bytes)
  "Write raw bytes to a TCP socket and close it."
  (let ((writer (net:tcp-stream-byte-writer socket)))
    (write-sequence (if (stringp bytes)
                        (sb-ext:string-to-octets bytes :external-format :utf-8)
                        bytes)
                    writer)
    (finish-output writer))
  (handler-case (net:tcp-shutdown socket) (error () nil))
  (handler-case (net:tcp-close socket) (error () nil)))

(defun drain-request (socket)
  "Read and discard the HTTP request from SOCKET until CRLFCRLF is seen."
  (let ((reader (net:tcp-stream-byte-reader socket))
        (prev3 0) (prev2 0) (prev1 0)
        (buf (make-array 1 :element-type '(unsigned-byte 8))))
    (loop
      (let ((n (read-sequence buf reader)))
        (when (zerop n) (return))
        (let ((b (aref buf 0)))
          (when (and (= prev3 13) (= prev2 10) (= prev1 13) (= b 10))
            (return))
          (setf prev3 prev2 prev2 prev1 prev1 b))))))

(defun make-chunked-response (status chunks &key extra-headers trailers)
  "Build a raw HTTP/1.1 chunked response string.
   CHUNKS is a list of strings, each becomes one chunk.
   EXTRA-HEADERS is an alist of (name . value) pairs.
   TRAILERS is an alist of (name . value) pairs (sent after final chunk)."
  (with-output-to-string (s)
    (format s "HTTP/1.1 ~D OK~C~C" status #\Return #\Linefeed)
    (format s "Transfer-Encoding: chunked~C~C" #\Return #\Linefeed)
    (format s "Content-Type: text/plain~C~C" #\Return #\Linefeed)
    (dolist (h extra-headers)
      (format s "~A: ~A~C~C" (car h) (cdr h) #\Return #\Linefeed))
    (format s "~C~C" #\Return #\Linefeed)
    ;; Chunks: size in hex CRLF data CRLF
    (dolist (chunk chunks)
      (format s "~X~C~C~A~C~C" (length chunk) #\Return #\Linefeed
              chunk #\Return #\Linefeed))
    ;; Final zero-length chunk
    (format s "0~C~C" #\Return #\Linefeed)
    ;; Trailers (if any)
    (dolist (trailer trailers)
      (format s "~A: ~A~C~C" (car trailer) (cdr trailer) #\Return #\Linefeed))
    ;; Final CRLF after last chunk / trailers
    (format s "~C~C" #\Return #\Linefeed)))

(defun run-raw-tcp-server (port handler)
  "Start a raw TCP server on PORT that calls HANDLER with each accepted socket.
   HANDLER is responsible for closing the socket when done.
   Returns a list (socket thread) for cleanup."
  (let* ((listen-addr (net:make-socket-address "127.0.0.1" port))
         (listen-socket (net:tcp-bind listen-addr))
         (thread (thread:make-thread
                  (lambda ()
                    (loop
                      (handler-case
                          (let ((client-socket (net:tcp-accept listen-socket)))
                            (thread:make-thread
                             (lambda ()
                               (handler-case (funcall handler client-socket)
                                 (error () nil)))
                             :name "raw-tcp-client"))
                        (error () (return)))))
                  :name "raw-tcp-server")))
    (list listen-socket thread)))

(defun stop-raw-tcp-server (server)
  "Stop a raw TCP server returned by run-raw-tcp-server."
  (destructuring-bind (socket thread) server
    (handler-case (net:tcp-close socket) (error () nil))
    (handler-case (thread:destroy-thread thread) (error () nil))))

(defmacro with-raw-tcp-server ((port-var handler) &body body)
  "Execute BODY with a raw TCP server running HANDLER.
   PORT-VAR is bound to the port used."
  (let ((server-var (gensym "SERVER")))
    `(let* ((,port-var (helpers:find-available-port))
            (,server-var nil))
       (unwind-protect
            (progn
              (setf ,server-var (run-raw-tcp-server ,port-var ,handler))
              (sleep 0.05)
              ,@body)
         (when ,server-var
           (stop-raw-tcp-server ,server-var))))))

;;; Tests

(deftest test-chunked-simple ()
  "Test reading a simple chunked response with multiple chunks"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          (send-bytes-and-close sock
                                      (make-chunked-response
                                       200 '("Hello" ", " "World")))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/test" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (assert-equal "Hello, World" (response:response-body-string resp)))))

(deftest test-chunked-single-chunk ()
  "Test chunked response with a single chunk"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          (send-bytes-and-close sock
                                      (make-chunked-response
                                       200 '("Single chunk body")))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (assert-equal "Single chunk body" (response:response-body-string resp)))))

(deftest test-chunked-empty-body ()
  "Test chunked response with no data chunks (just the terminator)"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          (send-bytes-and-close sock
                                      (make-chunked-response 200 '()))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (let ((body (response:response-body-string resp)))
        (assert-true (or (null body) (string= body "")))))))

(deftest test-chunked-large-body ()
  "Test chunked response with a large body split across many chunks"
  (let* ((chunk-data (make-string 200 :initial-element #\X))
         (chunks (loop repeat 10 collect chunk-data))
         (expected (apply #'concatenate 'string chunks)))
    (with-raw-tcp-server (port
                          (lambda (sock)
                            (drain-request sock)
                            (send-bytes-and-close sock
                                        (make-chunked-response 200 chunks))))
      (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port))))
        (assert-true resp)
        (assert-equal 200 (response:response-status resp))
        (assert-equal (length expected)
                      (length (response:response-body-string resp)))))))

(deftest test-chunked-with-extensions ()
  "Test chunked response where chunk-size lines have extensions (ignored per RFC 7230)"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          ;; Manually build response with chunk extensions
                          (send-bytes-and-close sock
                                      (format nil "HTTP/1.1 200 OK~C~CTransfer-Encoding: chunked~C~C~C~C5;ext=val~C~CHello~C~C0~C~C~C~C"
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port))))
      (assert-true resp)
      (assert-equal 200 (response:response-status resp))
      (assert-equal "Hello" (response:response-body-string resp)))))

(deftest test-chunked-post-response ()
  "Test chunked response to a POST request"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          (send-bytes-and-close sock
                                      (make-chunked-response
                                       201 '("{\"id\":" "42}")))))
    (let ((resp (client:http-post (format nil "http://127.0.0.1:~D/items" port)
                                  :body "{\"name\":\"test\"}")))
      (assert-true resp)
      (assert-equal 201 (response:response-status resp))
      (assert-equal "{\"id\":42}" (response:response-body-string resp)))))

(deftest test-chunked-hex-uppercase ()
  "Test chunked response with uppercase hex chunk sizes"
  (with-raw-tcp-server (port
                        (lambda (sock)
                          (drain-request sock)
                          ;; Use uppercase hex manually
                          (send-bytes-and-close sock
                                      (format nil "HTTP/1.1 200 OK~C~CTransfer-Encoding: chunked~C~C~C~CA~C~C0123456789~C~C0~C~C~C~C"
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed
                                              #\Return #\Linefeed))))
    (let ((resp (client:get (format nil "http://127.0.0.1:~D/" port))))
      (assert-true resp)
      (assert-equal "0123456789" (response:response-body-string resp)))))

;;; Direct unit tests for decode-chunked-body
;;;
;;; These test the chunked decoder by feeding it raw byte vectors, no
;;; sockets involved. They cover edge cases that are awkward to provoke
;;; via a real HTTP server: truncated chunks, missing CRLFs, zero-byte
;;; chunk with trailers, mixed-case hex sizes, chunk extensions, and
;;; empty input.

(defun bytes (&rest values)
  "Build a (simple-array (unsigned-byte 8) (*)) from VALUES."
  (let ((arr (make-array (length values) :element-type '(unsigned-byte 8))))
    (loop for v in values
          for i from 0
          do (setf (aref arr i) v))
    arr))

(defun string->bytes (s)
  "Encode S as UTF-8 bytes."
  (sb-ext:string-to-octets s :external-format :utf-8))

(defun bytes->string (b)
  "Decode B as UTF-8 string."
  (sb-ext:octets-to-string
   (coerce b '(simple-array (unsigned-byte 8) (*)))
   :external-format :utf-8))

(deftest test-decode-chunked-empty-input ()
  "Decoding empty input should produce empty body, not error."
  (let* ((data (string->bytes ""))
         (decoded (client::decode-chunked-body data 0)))
    (assert-equal 0 (length decoded))))

(deftest test-decode-chunked-only-terminator ()
  "Decoding 0\\r\\n\\r\\n should produce an empty body."
  (let* ((data (string->bytes (format nil "0~C~C~C~C"
                                      #\Return #\Linefeed
                                      #\Return #\Linefeed)))
         (decoded (client::decode-chunked-body data 0)))
    (assert-equal 0 (length decoded))))

(deftest test-decode-chunked-single-chunk ()
  "A single chunk of size 5 followed by terminator."
  (let* ((data (string->bytes (format nil "5~C~CHello~C~C0~C~C~C~C"
                                      #\Return #\Linefeed
                                      #\Return #\Linefeed
                                      #\Return #\Linefeed
                                      #\Return #\Linefeed)))
         (decoded (client::decode-chunked-body data 0)))
    (assert-equal "Hello" (bytes->string decoded))))

(deftest test-decode-chunked-multiple-chunks ()
  "Several chunks should be concatenated in order."
  (let* ((wire (format nil "5~C~CHello~C~C2~C~C, ~C~C5~C~CWorld~C~C0~C~C~C~C"
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    (assert-equal "Hello, World" (bytes->string decoded))))

(deftest test-decode-chunked-uppercase-hex ()
  "Hex chunk sizes should be parsed case-insensitively (parse-integer handles this)."
  (let* ((wire (format nil "FF~C~C~AAB~C~C0~C~C~C~C"
                       #\Return #\Linefeed
                       (make-string 255 :initial-element #\X)
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    (assert-equal 255 (length decoded))))

(deftest test-decode-chunked-with-extensions ()
  "Chunk extensions (after ;) should be ignored per RFC 7230."
  (let* ((wire (format nil "5;ext=val~C~CHello~C~C0~C~C~C~C"
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    (assert-equal "Hello" (bytes->string decoded))))

(deftest test-decode-chunked-truncated-size-line ()
  "If the size line has no CRLF, decoder should stop cleanly without error."
  ;; "5" with no CRLF -- find-crlf returns nil
  (let* ((data (string->bytes "5"))
         (decoded (client::decode-chunked-body data 0)))
    (assert-equal 0 (length decoded))))

(deftest test-decode-chunked-truncated-data ()
  "If chunk data is shorter than the declared size, decoder should not
   over-read; it copies whatever bytes are available."
  ;; Declares size 10 but only has 5 bytes after the size line
  (let* ((wire (format nil "A~C~CHello"
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    ;; Should copy only the available bytes (5) without crashing
    (assert-equal 5 (length decoded))
    (assert-equal "Hello" (bytes->string decoded))))

(deftest test-decode-chunked-zero-with-trailers ()
  "Zero-length chunk followed by trailer headers should still produce empty body.
   The decoder stops at the first 0 chunk; trailer parsing is not its job."
  (let* ((wire (format nil "0~C~CX-Trailer-A: value~C~CX-Trailer-B: more~C~C~C~C"
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    (assert-equal 0 (length decoded))))

(deftest test-decode-chunked-non-hex-size ()
  "If the chunk size isn't valid hex, the decoder should bail without error."
  (let* ((wire (format nil "ZZZ~C~CHello~C~C0~C~C~C~C"
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed
                       #\Return #\Linefeed))
         (decoded (client::decode-chunked-body (string->bytes wire) 0)))
    (assert-equal 0 (length decoded))))

(deftest test-decode-chunked-with-start-offset ()
  "Decoder should honor the START offset, useful when body is concatenated
   to headers in the same buffer."
  (let* ((prefix (string->bytes "GARBAGE"))
         (chunked (string->bytes (format nil "5~C~CHello~C~C0~C~C~C~C"
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed
                                         #\Return #\Linefeed)))
         (combined (concatenate '(simple-array (unsigned-byte 8) (*)) prefix chunked))
         (decoded (client::decode-chunked-body combined (length prefix))))
    (assert-equal "Hello" (bytes->string decoded))))

(deftest test-find-crlf-found ()
  "find-crlf should locate CR position when CRLF is present."
  (let ((data (string->bytes (format nil "abc~C~Cdef" #\Return #\Linefeed))))
    (assert-equal 3 (client::find-crlf data 0))))

(deftest test-find-crlf-not-found ()
  "find-crlf should return nil when CRLF is absent."
  (let ((data (string->bytes "no crlf here")))
    (assert-nil (client::find-crlf data 0))))

(deftest test-chunked-body-complete-p-true ()
  "chunked-body-complete-p recognizes the chunked terminator CRLF CRLF."
  (let ((data (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents '(13 10 13 10)
                            :adjustable t :fill-pointer 4)))
    (assert-true (client::chunked-body-complete-p data))))

(deftest test-chunked-body-complete-p-false ()
  "chunked-body-complete-p returns nil for incomplete data."
  (let ((data (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents '(13 10 13)
                            :adjustable t :fill-pointer 3)))
    (assert-false (client::chunked-body-complete-p data))))

(deftest test-chunked-body-complete-p-too-short ()
  "chunked-body-complete-p returns nil when data is shorter than 4 bytes."
  (let ((data (make-array 2 :element-type '(unsigned-byte 8)
                            :initial-contents '(13 10)
                            :adjustable t :fill-pointer 2)))
    (assert-false (client::chunked-body-complete-p data))))
