(:name "epsilon.crypto"
 :module-set "epsilon-stdlib"
 :description "Cryptographic primitives, TLS 1.3, and PKI: hashes, AEAD, KDFs, asym keys, ASN.1/PEM/PKCS, X.509, OCSP."
 :stability :stable
 :requires ("epsilon.foreign"
            "epsilon.net"
            "epsilon.io"
            "epsilon.json"
            "epsilon.encode"
            "epsilon.scheduler"))
