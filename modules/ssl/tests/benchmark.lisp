;;;; Performance benchmark: native vs OpenSSL crypto primitives
;;;;
;;;; Measures throughput of common operations to compare the pure-Lisp
;;;; implementations against the OpenSSL-based ones.

(defpackage epsilon.ssl.benchmark
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:native-sha256 #:epsilon.ssl.sha256)
   (#:native-sha512 #:epsilon.ssl.sha512)
   (#:native-hmac #:epsilon.ssl.hmac)
   (#:native-hkdf #:epsilon.ssl.hkdf)
   (#:native-aes-gcm #:epsilon.ssl.aes-gcm)
   (#:native-chacha #:epsilon.ssl.chacha20-poly1305)
   (#:native-x25519 #:epsilon.ssl.curve25519)
   (#:native-ed-sign #:epsilon.ssl.ed25519-sign)
   (#:native-ecdsa #:epsilon.ssl.ecdsa)
   (#:native-drbg #:epsilon.ssl.drbg)
   (#:bridge #:epsilon.crypto.native)
   (#:mod-arith #:epsilon.ssl.modular)
   (#:rsa #:epsilon.ssl.rsa)
   (#:net #:epsilon.net))
  (:export #:run-benchmarks)
  (:enter t))

(in-package :epsilon.ssl.benchmark)

;;; ---------------------------------------------------------------------------
;;; Timing infrastructure
;;; ---------------------------------------------------------------------------

(defun benchmark (name iterations thunk)
  "Run THUNK for ITERATIONS, print timing results."
  ;; Warmup
  (dotimes (i (min 10 iterations))
    (funcall thunk))
  ;; Timed run
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall thunk))
    (let* ((end (get-internal-real-time))
           (elapsed-ms (/ (* 1000.0 (- end start))
                          internal-time-units-per-second))
           (ops-per-sec (if (> elapsed-ms 0)
                            (/ (* iterations 1000.0) elapsed-ms)
                            0)))
      (format t "~&  ~40A ~8,1Fms  (~:D ops/s)~%"
              name elapsed-ms (round ops-per-sec))
      (values elapsed-ms ops-per-sec))))

;;; ---------------------------------------------------------------------------
;;; Benchmark suites
;;; ---------------------------------------------------------------------------

(defun bench-sha256 ()
  (format t "~&--- SHA-256 ---~%")
  (let ((data (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42)))
    (benchmark "Native SHA-256 (64 bytes)" 10000
               (lambda () (native-sha256:sha256 data))))
  (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 42)))
    (benchmark "Native SHA-256 (1KB)" 5000
               (lambda () (native-sha256:sha256 data))))
  (let ((data (make-array 16384 :element-type '(unsigned-byte 8) :initial-element 42)))
    (benchmark "Native SHA-256 (16KB)" 500
               (lambda () (native-sha256:sha256 data)))))

(defun bench-sha512 ()
  (format t "~&--- SHA-512 ---~%")
  (let ((data (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42)))
    (benchmark "Native SHA-512 (64 bytes)" 10000
               (lambda () (native-sha512:sha512 data))))
  (let ((data (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 42)))
    (benchmark "Native SHA-512 (1KB)" 5000
               (lambda () (native-sha512:sha512 data)))))

(defun bench-hmac ()
  (format t "~&--- HMAC-SHA256 ---~%")
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
        (msg (make-array 64 :element-type '(unsigned-byte 8) :initial-element 2)))
    (benchmark "Native HMAC-SHA256 (64B msg)" 5000
               (lambda () (native-hmac:hmac :sha256 key msg)))))

(defun bench-hkdf ()
  (format t "~&--- HKDF ---~%")
  (let ((ikm (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
        (salt (make-array 32 :element-type '(unsigned-byte 8) :initial-element 2))
        (info (make-array 10 :element-type '(unsigned-byte 8) :initial-element 3)))
    (benchmark "Native HKDF-SHA256 (32B output)" 2000
               (lambda ()
                 (let ((prk (native-hkdf:hkdf-extract :sha256 salt ikm)))
                   (native-hkdf:hkdf-expand :sha256 prk info 32))))))

(defun bench-aes-gcm ()
  (format t "~&--- AES-256-GCM ---~%")
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
        (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 2))
        (aad (make-array 0 :element-type '(unsigned-byte 8))))
    (let ((pt (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42)))
      (benchmark "Native AES-256-GCM encrypt (64B)" 2000
                 (lambda () (native-aes-gcm:aes-gcm-encrypt pt key nonce :aad aad))))
    (let ((pt (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 42)))
      (benchmark "Native AES-256-GCM encrypt (1KB)" 1000
                 (lambda () (native-aes-gcm:aes-gcm-encrypt pt key nonce :aad aad))))
    (let ((pt (make-array 16384 :element-type '(unsigned-byte 8) :initial-element 42)))
      (benchmark "Native AES-256-GCM encrypt (16KB)" 100
                 (lambda () (native-aes-gcm:aes-gcm-encrypt pt key nonce :aad aad))))))

(defun bench-chacha20-poly1305 ()
  (format t "~&--- ChaCha20-Poly1305 ---~%")
  (let ((key (make-array 32 :element-type '(unsigned-byte 8) :initial-element 1))
        (nonce (make-array 12 :element-type '(unsigned-byte 8) :initial-element 2))
        (aad (make-array 0 :element-type '(unsigned-byte 8))))
    (let ((pt (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42)))
      (benchmark "Native ChaCha20-Poly1305 encrypt (64B)" 2000
                 (lambda () (native-chacha:chacha20-poly1305-encrypt pt key nonce :aad aad))))
    (let ((pt (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 42)))
      (benchmark "Native ChaCha20-Poly1305 encrypt (1KB)" 1000
                 (lambda () (native-chacha:chacha20-poly1305-encrypt pt key nonce :aad aad))))))

(defun bench-x25519 ()
  (format t "~&--- X25519 ---~%")
  (let ((sk (native-drbg:random-bytes 32))
        (pk (native-x25519:x25519-base (native-drbg:random-bytes 32))))
    (benchmark "Native X25519 scalar multiply" 200
               (lambda () (native-x25519:x25519 sk pk)))))

(defun bench-ed25519 ()
  (format t "~&--- Ed25519 ---~%")
  (let* ((msg (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42))
         (sk (native-drbg:random-bytes 32))
         (pk (native-ed-sign:ed25519-public-key-from-private sk)))
    (benchmark "Native Ed25519 sign" 100
               (lambda () (native-ed-sign:ed25519-sign sk msg)))
    (let ((sig (native-ed-sign:ed25519-sign sk msg)))
      (benchmark "Native Ed25519 verify" 100
                 (lambda () (native-ed-sign:ed25519-verify pk msg sig))))))

(defun bench-random ()
  (format t "~&--- Random ---~%")
  (benchmark "Native DRBG random 32 bytes" 10000
             (lambda () (native-drbg:random-bytes 32))))

(defun bench-montgomery ()
  (format t "~&--- Montgomery vs Naive mod-expt ---~%")
  ;; Use a 1024-bit prime (typical RSA CRT half)
  (let* ((p #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF)
         (base 314159265358979323)
         (exp (- p 2)))
    (benchmark "Naive mod-expt (2048-bit)" 5
               (lambda () (mod-arith:mod-expt base exp p)))
    (benchmark "Montgomery mod-expt (2048-bit)" 5
               (lambda () (mod-arith:mont-expt base exp p)))))

(defun bench-rsa ()
  (format t "~&--- RSA ---~%")
  (let* ((msg (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(104 101 108 108 111))))
    ;; Generate a small key for benchmarking (1024-bit is fast enough to generate)
    (multiple-value-bind (pub priv) (rsa:rsa-generate-key 1024)
      (let ((ct (rsa:rsa-encrypt 42 pub)))
        (benchmark "RSA-1024 encrypt (e=65537)" 200
                   (lambda () (rsa:rsa-encrypt 42 pub)))
        (benchmark "RSA-1024 decrypt (CRT+Montgomery)" 20
                   (lambda () (rsa:rsa-decrypt ct priv)))
        (benchmark "RSA-PSS-1024 sign (SHA-256)" 10
                   (lambda () (rsa:rsa-pss-sign priv msg :hash :sha256)))))))

(defun bench-tls-handshake ()
  (format t "~&--- TLS 1.3 Handshake ---~%")
  (let ((addrs (net:resolve-address "example.com" 443)))
    (benchmark "Native TLS handshake (example.com)" 3
               (lambda ()
                 (let* ((tcp (net:tcp-connect (first addrs)))
                        (fd (net:tcp-stream-handle tcp))
                        (ctx (bridge:make-client-context))
                        (conn (bridge:tls-connect fd ctx :hostname "example.com")))
                   (bridge:tls-close conn)
                   (net:tcp-close tcp))))))

;;; ---------------------------------------------------------------------------
;;; Main benchmark runner
;;; ---------------------------------------------------------------------------

(defun run-benchmarks (&key (include-tls t))
  "Run all benchmarks."
  (format t "~&~%========================================~%")
  (format t "  Crypto Native Benchmark~%")
  (format t "========================================~%~%")

  (bench-random)
  (bench-sha256)
  (bench-sha512)
  (bench-hmac)
  (bench-hkdf)
  (bench-aes-gcm)
  (bench-chacha20-poly1305)
  (bench-x25519)
  (bench-ed25519)
  (bench-montgomery)
  (bench-rsa)

  (when include-tls
    (bench-tls-handshake))

  (format t "~&~%========================================~%")
  (format t "  Benchmark complete~%")
  (format t "========================================~%"))

;;; ---------------------------------------------------------------------------
;;; Test wrapper
;;; ---------------------------------------------------------------------------

(deftest test-run-benchmarks
  "Run crypto primitive benchmarks (no network)"
  (run-benchmarks :include-tls nil)
  (assert-true t))
