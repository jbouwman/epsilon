(:name "epsilon.crypto"
 :version "1.2.0"
 :description "Cryptographic operations, TLS/SSL support, and X.509 certificates using OpenSSL"
 :documentation "API.md"
 :requires ("epsilon.foreign" "epsilon.net")
 :provides ("crypto" "tls" "x509" "digital-signatures" "key-generation" "kdf" "blake2" "aead")
 :author "Jesse Bouwman"
 :license "MIT")