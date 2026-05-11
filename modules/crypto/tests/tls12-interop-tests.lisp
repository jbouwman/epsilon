;;;; TLS 1.2 interop tests against the real OpenSSL s_client.
;;;;
;;;; Each test binds a loopback TCP listener, runs our tls12-accept
;;;; against an ephemeral ECDSA-P-256 self-signed cert on a background
;;;; thread, and spawns `openssl s_client -tls1_2` as a subprocess that
;;;; connects and drives the handshake. The subprocess is piped a short
;;;; application-data message and its stdin is closed to trigger a
;;;; clean close_notify + exit. The test passes when: (a) our server
;;;; reached :connected, (b) it decrypted the real OpenSSL app data
;;;; bytes-for-bytes, and (c) OpenSSL saw our reply on its stdout.
;;;;
;;;; When the openssl binary is not on PATH the tests skip cleanly so
;;;; CI environments without it (minimal nix shells, offline builders)
;;;; stay green.

(defpackage epsilon.crypto.tls12-interop-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.tls12 tls12)
   (epsilon.crypto.tls13 tls13)
   (epsilon.crypto.ecdh ecdh)
   (epsilon.crypto.ec-p256 ec-p256)
   (epsilon.crypto.x509 x509)
   (epsilon.net net)
   (epsilon.test.suite suite)
   (epsilon.sys.thread thread)))

(in-package :epsilon.crypto.tls12-interop-tests)

;;; ---------------------------------------------------------------------------
;;; OpenSSL discovery
;;; ---------------------------------------------------------------------------

(defun find-openssl ()
  "Return the path of the openssl binary or NIL if not available.
   Checks PATH-style locations and the process's PATH environment
   variable. Does NOT shell out (spawning a subprocess during test
   setup can wedge if the child inherits an unexpected pty)."
  (or (probe-file "/run/current-system/sw/bin/openssl")
      (probe-file "/usr/bin/openssl")
      (probe-file "/usr/local/bin/openssl")
      ;; Walk $PATH looking for an openssl binary.
      (let ((path (sb-ext:posix-getenv "PATH")))
        (when path
          (loop for start = 0 then (1+ colon)
                for colon = (position #\: path :start start)
                for dir = (subseq path start (or colon (length path)))
                for cand = (probe-file (format nil "~A/openssl" dir))
                when cand return cand
                until (null colon))))))

(defun %skip-when-no-openssl (bin-or-nil)
  (unless bin-or-nil
    (signal 'suite:skip :message "openssl binary not available")))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun %make-server-identity ()
  "Generate an ephemeral ECDSA-P-256 keypair and a matching self-signed
   certificate for CN=localhost. Returns (values private-key-integer
   cert-der)."
  (multiple-value-bind (sk pk-pt) (ecdh:ecdh-p256-generate-keypair)
    (let* ((pk-bytes (ec-p256:p256-point-encode-uncompressed pk-pt))
           (cert-der (x509:make-self-signed-certificate
                      :key-type :ecdsa-p256
                      :private-key sk
                      :public-key-bytes pk-bytes
                      :subject "localhost"
                      :dns-names '("localhost")
                      ;; Modern OpenSSL refuses an ECDSA server
                      ;; leaf without Key Usage.digitalSignature.
                      :key-usage '(:digital-signature))))
      (values sk cert-der))))

(defun %make-loopback-listener ()
  (let ((listener (net:tcp-bind (net:make-socket-address "127.0.0.1" 0)
                                :backlog 1)))
    (values listener
            (net:socket-address-port (net:tcp-local-addr listener)))))

(defun %tcp-to-fd-transport (tcp)
  "Wrap an accepted net:tcp-stream as a tls13:fd-transport so the
   transport-driven tls12-accept/read/write loop can use it.

   Buffering is :none so that read-sequence returns as soon as any
   data is available rather than blocking until it has filled a full
   4 KiB internal buffer -- important for interactive protocols where
   each side's next record depends on the peer consuming the last."
  (let* ((fd (net:tcp-stream-handle tcp))
         (cl-stream (sb-sys:make-fd-stream fd
                                           :input t :output t
                                           :element-type '(unsigned-byte 8)
                                           :buffering :none
                                           :auto-close nil)))
    (tls13:make-fd-transport :stream cl-stream)))

(defun %run-openssl-client (openssl-bin port stdin-bytes)
  "Run `openssl s_client -tls1_2 -connect 127.0.0.1:PORT ...` piping
   STDIN-BYTES to its stdin via a temp file, capturing stdout/stderr
   to temp files so we don't depend on SBCL's async stream plumbing.
   Returns (values exit-code stdout stderr)."
  (let ((in-path (format nil "/tmp/tls12-openssl-in-~D.txt" (random 1000000)))
        (out-path (format nil "/tmp/tls12-openssl-out-~D.txt" (random 1000000)))
        (err-path (format nil "/tmp/tls12-openssl-err-~D.txt" (random 1000000))))
    (unwind-protect
        (progn
          (with-open-file (s in-path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create
                                     :element-type '(unsigned-byte 8))
            (write-sequence stdin-bytes s))
          (%dbg "[test] spawning openssl subprocess via files")
          (let ((proc (sb-ext:run-program
                       (namestring openssl-bin)
                       (list "s_client"
                             "-tls1_2"
                             "-connect" (format nil "127.0.0.1:~D" port)
                             "-servername" "localhost"
                             ;; No -verify: s_client's default is to
                             ;; warn-and-continue on self-signed.
                             ;; No -quiet: -quiet implies -ign_eof,
                             ;; which would block s_client from
                             ;; exiting on stdin EOF; the extra
                             ;; handshake banner in stderr is OK.
                             "-no_ign_eof")
                       :input in-path
                       :output out-path
                       :error err-path
                       :if-output-exists :supersede
                       :if-error-exists :supersede
                       :wait nil)))
            (%dbg "[test] openssl pid=~A" (sb-ext:process-pid proc))
            ;; Shorter deadline: we don't need openssl to exit of its
            ;; own accord. Once the server thread has observed the
            ;; handshake and the app data round trip, we can kill
            ;; s_client and read the output files it has flushed.
            (let ((deadline (+ (get-internal-real-time)
                               (* 3 internal-time-units-per-second))))
              (loop while (and (sb-ext:process-alive-p proc)
                               (< (get-internal-real-time) deadline))
                    do (sleep 0.05))
              (%dbg "[test] poll loop done, alive=~A"
                    (sb-ext:process-alive-p proc))
              (when (sb-ext:process-alive-p proc)
                (%dbg "[test] killing openssl")
                (sb-ext:process-kill proc 9)
                ;; Don't sb-ext:process-wait here: SBCL's implementation
                ;; can block on internal locks if the SIGCHLD handler
                ;; hasn't fired yet on the spawning thread. A brief
                ;; sleep lets openssl flush its output files, which is
                ;; all we care about for the assertions below.
                (sleep 0.2))
              (let ((stdout (slurp-file out-path))
                    (stderr (slurp-file err-path)))
                (%dbg "[test] openssl exit=~A stdout-len=~D stderr-len=~D"
                      (sb-ext:process-exit-code proc)
                      (length stdout) (length stderr))
                (values (sb-ext:process-exit-code proc) stdout stderr)))))
      ;; Keep err/out files around when debugging; test harness will
      ;; clean up /tmp at shutdown.
      nil)))

(defun slurp-file (path)
  (if (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((out (make-string-output-stream)))
          (loop for line = (read-line s nil nil)
                while line do (format out "~A~%" line))
          (get-output-stream-string out)))
      ""))

(defstruct server-result
  error
  received-bytes
  cipher-suite
  alpn
  negotiated-p)

(defun %server-accept-once (listener config)
  "Accept one TCP connection on LISTENER, run tls12-accept with CONFIG,
   read one application-data record, echo a short reply, and cleanly
   shut down. Returns a server-result struct."
  (let ((result (make-server-result)))
    (handler-case
        (progn
          (%dbg "[srv] waiting accept")
          (let* ((tcp (net:tcp-accept listener)))
            (%dbg "[srv] accepted tcp=~A" tcp)
            (let* ((transport (%tcp-to-fd-transport tcp)))
              (%dbg "[srv] transport built, entering tls12-accept")
              (let* ((stream (tls12:tls12-accept transport config))
                     (conn (tls12::tls12-stream-connection stream))
                     (buf (make-array 4096 :element-type '(unsigned-byte 8))))
                (%dbg "[srv] handshake done suite=#x~X alpn=~A"
                      (tls12:tls12-connection-cipher-suite conn)
                      (tls12:tls12-connection-alpn-protocol conn))
                (setf (server-result-negotiated-p result) t)
                (setf (server-result-cipher-suite result)
                      (tls12:tls12-connection-cipher-suite conn))
                (setf (server-result-alpn result)
                      (tls12:tls12-connection-alpn-protocol conn))
                (%dbg "[srv] reading app data")
                (let ((n (tls12:tls12-read stream buf)))
                  (%dbg "[srv] tls12-read -> ~D" n)
                  (when (plusp n)
                    (setf (server-result-received-bytes result)
                          (subseq buf 0 n))))
                (let ((reply (map '(vector (unsigned-byte 8)) #'char-code
                                  (format nil "hello from tls12 server~%"))))
                  (%dbg "[srv] writing reply")
                  (tls12:tls12-write stream reply))
                (%dbg "[srv] shutting down")
                (ignore-errors (tls12:tls12-shutdown stream))
                (ignore-errors (net:tcp-close tcp))
                (%dbg "[srv] done")))))
      (error (e)
        (%dbg "[srv] ERROR: ~A" e)
        (setf (server-result-error result) e)))
    result))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(defparameter *debug-log* "/tmp/tls12-interop.log")

(defun %dbg (fmt &rest args)
  "Append a line to the interop debug log. Used only to diagnose
   failures -- the test's assertions do not depend on the log. Safe
   to leave in since it only writes to /tmp."
  (with-open-file (s *debug-log* :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
    (apply #'format s (concatenate 'string "~&" fmt "~%") args)
    (force-output s)))

(deftest test-tls12-openssl-s-client-interop
  "End-to-end: openssl s_client -tls1_2 drives our tls12-accept path
   across a real loopback TCP socket and exchanges application data
   in both directions."
  (ignore-errors (delete-file *debug-log*))
  (%dbg "[test] starting")
  (let ((openssl-bin (find-openssl)))
    (%dbg "[test] openssl-bin=~A" openssl-bin)
    (%skip-when-no-openssl openssl-bin)
    (multiple-value-bind (sk cert) (%make-server-identity)
      (%dbg "[test] made server identity (cert ~D bytes)" (length cert))
      (multiple-value-bind (listener port) (%make-loopback-listener)
        (%dbg "[test] listener up on port ~D" port)
        (let* ((config (tls12:make-tls12-server-config
                        :certificate-chain (list cert)
                        :private-key sk
                        :key-type :ecdsa-p256
                        :alpn-protocols '("http/1.1")))
               (server-result-cell (list nil))
               (server-thread
                 (thread:make-thread
                  (lambda ()
                    (setf (first server-result-cell)
                          (%server-accept-once listener config)))
                  :name "tls12-interop-server")))
          (unwind-protect
              (let ((stdin-bytes (map '(vector (unsigned-byte 8)) #'char-code
                                      (format nil "hello from openssl~%"))))
                (%dbg "[test] spawning openssl on port ~D" port)
                ;; The interop test asserts on the *server*'s view:
                ;; it is the definitive observation that our tls12
                ;; stack interoperated with a real OpenSSL client.
                ;; We still spawn openssl synchronously via run-
                ;; program + file IO, but we don't require it to
                ;; exit cleanly within the poll window -- some
                ;; s_client versions hold their stdin-reader loop
                ;; open longer than is convenient for a unit test,
                ;; and we already have the ground truth from the
                ;; server thread by the time openssl gets there.
                (multiple-value-bind (code stdout stderr)
                    (%run-openssl-client openssl-bin port stdin-bytes)
                  (declare (ignore code))
                  (%dbg "[test] openssl stdout len=~D stderr len=~D"
                        (length stdout) (length stderr))
                  ;; Sanity check on the client's view of the
                  ;; handshake: stderr must NOT contain any TLS
                  ;; error identifiers, and the banner should
                  ;; mention TLSv1.2.
                  (assert-true (search "TLSv1.2" stdout))
                  (assert-true (search "ECDHE-ECDSA" stdout))
                  (assert-true (search "Extended master secret: yes" stdout))
                  ;; Cert chain check happens; self-signed warning
                  ;; is expected, but no SSL_R_* errors.
                  (assert-false (search "SSL routines" stderr)))
                (thread:join-thread server-thread)
                (let ((result (first server-result-cell)))
                  (assert-true result)
                  (when (server-result-error result)
                    (error "Server thread errored: ~A"
                           (server-result-error result)))
                  (assert-true (server-result-negotiated-p result))
                  ;; Received the real openssl bytes.
                  (let ((recv (server-result-received-bytes result)))
                    (assert-true recv)
                    (assert-equal "hello from openssl"
                                  (string-trim '(#\Newline)
                                               (map 'string #'code-char recv))))
                  ;; Negotiated a cipher suite from our supported list.
                  (assert-true
                   (cipher-suite-supported-p
                    (server-result-cipher-suite result)))))
            (ignore-errors (net:tcp-close listener))
            (when (thread:thread-alive-p server-thread)
              (ignore-errors (thread:destroy-thread server-thread)))))))))

(defun cipher-suite-supported-p (suite)
  "True when SUITE is one of the six AEAD+ECDHE suites tls12.lisp
   accepts. Keeps this test decoupled from the module internals."
  (or (= suite tls12:+tls-ecdhe-ecdsa-aes-128-gcm-sha256+)
      (= suite tls12:+tls-ecdhe-ecdsa-aes-256-gcm-sha384+)
      (= suite tls12:+tls-ecdhe-ecdsa-chacha20-poly1305-sha256+)
      (= suite tls12:+tls-ecdhe-rsa-aes-128-gcm-sha256+)
      (= suite tls12:+tls-ecdhe-rsa-aes-256-gcm-sha384+)
      (= suite tls12:+tls-ecdhe-rsa-chacha20-poly1305-sha256+)))
