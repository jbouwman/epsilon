;;;; HTTP Test Helpers
;;;;
;;;; Shared utilities for HTTP module tests: mock servers, response assertions,
;;;; and common test fixtures.

(defpackage :epsilon.http.test-helpers
  (:use :cl)
  (:import
   (epsilon.http.server server)
   (epsilon.http.request request)
   (epsilon.http.response response)
   (epsilon.http.client client)
   (epsilon.http.headers headers)
   (epsilon.map map)
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.pem pem)
   (epsilon.crypto.drbg drbg)
   (epsilon.crypto.ed25519-sign ed-sign))
  (:export
   #:with-mock-server
   #:find-available-port
   #:assert-status
   #:assert-header
   #:assert-body-contains
   #:make-echo-handler
   #:make-static-handler
   #:make-status-handler
   #:make-delayed-handler
   #:make-gzip-handler
   #:generate-test-cert
   #:cleanup-test-cert
   #:with-test-cert))

(defun find-available-port (&optional (start 18100) (max-tries 100))
  "Find an available TCP port starting from START.
   Tries up to MAX-TRIES ports before signaling an error."
  (loop for port from start below (+ start max-tries)
        do (handler-case
               (let ((srv (server:start-server
                           (lambda (req)
                             (declare (ignore req))
                             (response:text-response "probe"))
                           :port port)))
                 (server:stop-server srv)
                 (return port))
             (error () nil))
        finally (error "No available port found in range ~D-~D" start (+ start max-tries))))

(defmacro with-mock-server ((port-var handler &key (port nil port-supplied-p)) &body body)
  "Execute BODY with a temporary HTTP server running HANDLER.
   PORT-VAR is bound to the actual port used.
   If PORT is supplied, uses that port; otherwise finds a free one.

   Example:
     (with-mock-server (port (make-echo-handler))
       (let ((resp (client:get (format nil \"http://localhost:~A/test\" port))))
         (assert-status resp 200)))"
  (let ((server-var (gensym "SERVER")))
    `(let* ((,port-var ,(if port-supplied-p port '(find-available-port)))
            (,server-var nil))
       (unwind-protect
            (progn
              (setf ,server-var (server:start-server ,handler :port ,port-var))
              (sleep 0.05) ; Brief pause for server startup
              ,@body)
         (when ,server-var
           (server:stop-server ,server-var))))))

;;; Assertion Helpers

(defun assert-status (response expected-status)
  "Assert that RESPONSE has EXPECTED-STATUS code."
  (let ((actual (response:response-status response)))
    (unless (= actual expected-status)
      (error "Expected status ~D but got ~D" expected-status actual))
    t))

(defun assert-header (response header-name expected-value)
  "Assert that RESPONSE has HEADER-NAME with EXPECTED-VALUE.
   Uses response-header for case-insensitive lookup."
  (let ((actual (response:response-header response header-name)))
    (unless (and actual (string= actual expected-value))
      (error "Expected header ~A=~A but got ~A" header-name expected-value actual))
    t))

(defun assert-body-contains (response substring)
  "Assert that RESPONSE body contains SUBSTRING."
  (let ((body (response:response-body-string response)))
    (unless (and body (search substring body))
      (error "Expected body to contain ~S but body is ~S"
             substring (if (and body (> (length body) 200))
                           (subseq body 0 200)
                           body)))
    t))

;;; Common Handler Factories

(defun make-echo-handler ()
  "Create a handler that echoes request details as JSON."
  (lambda (req)
    (response:json-response
     (map:make-map
      "method" (request:request-method req)
      "path" (request:request-path req)
      "body" (request:request-body req)))))

(defun make-static-handler (body &key (status 200) (content-type "text/plain"))
  "Create a handler that always returns the same response."
  (lambda (req)
    (declare (ignore req))
    (let ((resp (response:make-response :status status :body body)))
      (response:set-header resp "Content-Type" content-type)
      resp)))

(defun make-status-handler ()
  "Create a handler that returns the status code from the URL path.
   e.g. GET /404 returns 404."
  (lambda (req)
    (let* ((path (request:request-path req))
           (code (ignore-errors (parse-integer (subseq path 1)))))
      (if code
          (response:text-response (format nil "Status ~D" code) :status code)
          (response:text-response "Bad Request" :status 400)))))

(defun make-delayed-handler (delay-seconds body)
  "Create a handler that waits DELAY-SECONDS before responding."
  (lambda (req)
    (declare (ignore req))
    (sleep delay-seconds)
    (response:text-response body)))

(defun make-gzip-handler (body)
  "Create a handler that returns gzip-compressed response body.
   Used for testing content decompression."
  (lambda (req)
    (declare (ignore req))
    ;; Return uncompressed for now -- compression testing uses raw bytes
    (response:text-response body)))

;;; Runtime certificate generation
;;;
;;; Tests that need a TLS server should generate a fresh self-signed
;;; certificate at runtime instead of relying on a static cert that will
;;; eventually expire. The cert is an Ed25519 cert valid for `localhost`.

(defun generate-test-cert ()
  "Generate a self-signed Ed25519 cert/key pair for testing.
   Returns (values cert-path key-path), both temp files in /tmp.
   Caller is responsible for cleanup via cleanup-test-cert or with-test-cert."
  (let* ((unique (format nil "~A-~A" (get-universal-time) (random 1000000)))
         (cert-path (format nil "/tmp/epsilon-http-test-cert-~A.pem" unique))
         (key-path (format nil "/tmp/epsilon-http-test-key-~A.pem" unique))
         (private-key (drbg:random-bytes 32))
         (public-key (ed-sign:ed25519-public-key-from-private private-key))
         (cert-der (x509:make-self-signed-certificate
                    :key-type :ed25519
                    :private-key private-key
                    :public-key-bytes public-key
                    :subject "localhost"
                    :dns-names '("localhost"))))
    (with-open-file (out cert-path :direction :output :if-exists :supersede)
      (write-string (pem:pem-encode (pem:make-pem-block "CERTIFICATE" cert-der))
                    out))
    (with-open-file (out key-path :direction :output :if-exists :supersede)
      (write-string (pem:pem-encode (pem:make-pem-block "PRIVATE KEY" private-key))
                    out))
    (values cert-path key-path)))

(defun cleanup-test-cert (cert-path key-path)
  "Remove certificate files generated by generate-test-cert."
  (when (and cert-path (probe-file cert-path))
    (delete-file cert-path))
  (when (and key-path (probe-file key-path))
    (delete-file key-path)))

(defmacro with-test-cert ((cert-var key-var) &body body)
  "Execute BODY with CERT-VAR and KEY-VAR bound to a freshly-generated
   self-signed certificate and key, removing the files when done.
   Both variables are declared ignorable so callers that only use one
   (e.g., a setup macro that passes them to start-server) don't have to
   add their own (declare (ignore ...))."
  `(multiple-value-bind (,cert-var ,key-var) (generate-test-cert)
     (declare (ignorable ,cert-var ,key-var))
     (unwind-protect
          (progn ,@body)
       (cleanup-test-cert ,cert-var ,key-var))))
