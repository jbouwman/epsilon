;;;; Tests for the test-pki helper module.
;;;;
;;;; Self-test: the chains the helper produces must verify cleanly under
;;;; the same x509 path-validation code that real consumers rely on.

(defpackage epsilon.crypto.test-pki-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.x509 x509)
   (epsilon.crypto.test-pki tpki)))

(in-package :epsilon.crypto.test-pki-tests)

(defun %verify-chain (chain certs)
  "Verify CERTS as a chain anchored at CHAIN's root."
  (let ((trust-store (x509:make-trust-store)))
    (x509:trust-store-add trust-store
                          (x509:parse-x509-certificate
                           (tpki:test-chain-root-cert chain)))
    (multiple-value-bind (valid reason)
        (x509:verify-certificate-chain certs :trust-store trust-store)
      (declare (ignore reason))
      valid)))

;;; ---------------------------------------------------------------------------
;;; Two-cert chains: root -> leaf (no intermediate)
;;; ---------------------------------------------------------------------------

(deftest test-test-pki-two-cert-chain-ed25519
  "make-test-chain (Ed25519 throughout) produces a verifiable root->leaf chain"
  (let* ((chain (tpki:make-test-chain :leaf-key-type :ed25519
                                      :root-key-type :ed25519))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain)))
         (root (x509:parse-x509-certificate (tpki:test-chain-root-cert chain))))
    ;; Direct signature check leaf -> root
    (assert-true (x509:verify-certificate-signature leaf root))
    ;; Hostname verification through SAN
    (assert-true (x509:hostname-matches-p leaf "leaf.test.example"))
    ;; Full path validation
    (assert-true (%verify-chain chain (list leaf root)))))

(deftest test-test-pki-two-cert-chain-ec-p256
  "make-test-chain (EC P-256 throughout) verifies"
  (let* ((chain (tpki:make-test-chain :leaf-key-type :ecdsa-p256
                                      :root-key-type :ecdsa-p256))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain)))
         (root (x509:parse-x509-certificate (tpki:test-chain-root-cert chain))))
    (assert-true (x509:verify-certificate-signature leaf root))
    (assert-true (%verify-chain chain (list leaf root)))))

(deftest test-test-pki-two-cert-chain-mixed-algorithms
  "make-test-chain with mismatched root/leaf key types still verifies"
  (let* ((chain (tpki:make-test-chain :root-key-type :ecdsa-p256
                                      :leaf-key-type :ed25519))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain)))
         (root (x509:parse-x509-certificate (tpki:test-chain-root-cert chain))))
    (assert-true (x509:verify-certificate-signature leaf root))
    (assert-true (%verify-chain chain (list leaf root)))))

;;; ---------------------------------------------------------------------------
;;; Three-cert chains: root -> intermediate -> leaf
;;; ---------------------------------------------------------------------------

(deftest test-test-pki-intermediate-chain-ed25519
  "Intermediate-CA chains validate end-to-end"
  (let* ((chain (tpki:make-test-chain :root-key-type :ed25519
                                      :intermediate-key-type :ed25519
                                      :leaf-key-type :ed25519))
         (root (x509:parse-x509-certificate (tpki:test-chain-root-cert chain)))
         (int (x509:parse-x509-certificate
               (tpki:test-chain-intermediate-cert chain)))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain))))
    ;; Each link verifies independently
    (assert-true (x509:verify-certificate-signature int root))
    (assert-true (x509:verify-certificate-signature leaf int))
    ;; Full path validation through both layers
    (assert-true (%verify-chain chain (list leaf int root)))
    ;; Intermediate has cA=true
    (let ((bc (x509:x509-basic-constraints int)))
      (assert-true bc)
      (assert-true (x509:x509-basic-constraints-ca bc)))
    ;; Leaf has serverAuth EKU
    (let* ((eku-ext (x509:x509-get-extension
                     leaf x509:+oid-extended-key-usage+))
           (purposes (and eku-ext (x509:parse-extended-key-usage eku-ext))))
      (assert-true (member x509:+oid-server-auth+ purposes :test #'equal)))))

(deftest test-test-pki-intermediate-chain-cross-algorithm
  "Cross-algorithm intermediate chain (RSA root, EC int, Ed25519 leaf)"
  ;; Use a small RSA key for speed -- this test exercises the chain
  ;; *structure*, not RSA strength.
  (let* ((chain (tpki:make-test-chain :root-key-type :rsa
                                      :intermediate-key-type :ecdsa-p256
                                      :leaf-key-type :ed25519
                                      :rsa-bits 1024))
         (root (x509:parse-x509-certificate (tpki:test-chain-root-cert chain)))
         (int (x509:parse-x509-certificate
               (tpki:test-chain-intermediate-cert chain)))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain))))
    (assert-true (x509:verify-certificate-signature int root))
    (assert-true (x509:verify-certificate-signature leaf int))
    (assert-true (%verify-chain chain (list leaf int root)))))

;;; ---------------------------------------------------------------------------
;;; Independent calls produce independent chains. Root keys must NOT
;;; cross-validate, otherwise the helper's randomness is broken.
;;; ---------------------------------------------------------------------------

(deftest test-test-pki-chains-are-independent
  "Two consecutive calls produce different roots; one root's leaf must
   NOT verify against the other root."
  (let* ((c1 (tpki:make-test-chain))
         (c2 (tpki:make-test-chain))
         (leaf1 (x509:parse-x509-certificate (tpki:test-chain-leaf-cert c1)))
         (root2 (x509:parse-x509-certificate (tpki:test-chain-root-cert c2))))
    ;; Root certs differ
    (assert-not (equalp (tpki:test-chain-root-cert c1)
                        (tpki:test-chain-root-cert c2)))
    ;; And the chains do not cross-validate
    (assert-not (x509:verify-certificate-signature leaf1 root2))))

;;; ---------------------------------------------------------------------------
;;; Custom DNS SANs flow through to the leaf's SAN extension
;;; ---------------------------------------------------------------------------

(deftest test-test-pki-custom-leaf-dns-names
  "Leaf cert contains the requested DNS SANs"
  (let* ((chain (tpki:make-test-chain
                 :leaf-subject "alpha.example.test"
                 :leaf-dns-names '("alpha.example.test"
                                   "beta.example.test"
                                   "*.cdn.example.test")))
         (leaf (x509:parse-x509-certificate (tpki:test-chain-leaf-cert chain))))
    (assert-true (x509:hostname-matches-p leaf "alpha.example.test"))
    (assert-true (x509:hostname-matches-p leaf "beta.example.test"))
    ;; Wildcard SAN should match a subdomain of cdn.example.test
    (assert-true (x509:hostname-matches-p leaf "img.cdn.example.test"))
    ;; And not match unlisted hostnames
    (assert-not (x509:hostname-matches-p leaf "gamma.example.test"))))
