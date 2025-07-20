(:name "epsilon.crypto"
 :version "1.0.0"
 :author "Jesse Bouwman"
 :description "Cryptographic signatures and key management for Epsilon packages"
 :sources ("src/crypto/keys"
           "src/crypto/keyring"
           "src/crypto/signatures"
           "src/zzz-crypto")
 :tests ("tests")
 :dependencies ("epsilon.core")
 :provides ("epsilon.crypto.keys"
            "epsilon.crypto.keyring"
            "epsilon.crypto.signatures"
            "epsilon.crypto"))