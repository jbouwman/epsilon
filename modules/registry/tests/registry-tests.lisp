;;;; epsilon.registry tests

(defpackage epsilon.registry.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (reg epsilon.registry)
   (map epsilon.map)))

(in-package epsilon.registry.tests)

;;; Version parsing tests

(deftest test-parse-version
  "Test semantic version parsing"
  (let ((v (reg::parse-version "1.2.3")))
    (is-= (reg::semver-major v) 1)
    (is-= (reg::semver-minor v) 2)
    (is-= (reg::semver-patch v) 3))
  (let ((v (reg::parse-version "1.2.3-alpha")))
    (is-equal (reg::semver-prerelease v) "alpha"))
  (let ((v (reg::parse-version "1.2.3+build123")))
    (is-equal (reg::semver-build v) "build123")))

(deftest test-version-comparison
  "Test version comparison"
  (is (reg::version< "1.0.0" "1.0.1"))
  (is (reg::version< "1.0.0" "1.1.0"))
  (is (reg::version< "1.0.0" "2.0.0"))
  (is-not (reg::version< "1.0.1" "1.0.0"))
  (is (reg::version= "1.0.0" "1.0.0"))
  (is-not (reg::version= "1.0.0" "1.0.1")))

;;; Constraint parsing tests

(deftest test-parse-constraint
  "Test version constraint parsing"
  (let ((c (reg::parse-constraint "^1.0.0")))
    (is-eq (reg::version-constraint-operator c) :caret)
    (is-equal (reg::version-constraint-version c) "1.0.0"))
  (let ((c (reg::parse-constraint "~1.0.0")))
    (is-eq (reg::version-constraint-operator c) :tilde))
  (let ((c (reg::parse-constraint ">=1.0.0")))
    (is-eq (reg::version-constraint-operator c) :gte))
  (let ((c (reg::parse-constraint "1.0.0")))
    (is-eq (reg::version-constraint-operator c) :eq)))

(deftest test-version-satisfies
  "Test version constraint satisfaction"
  ;; Exact match
  (is (reg::version-satisfies-p "1.0.0" (reg::parse-constraint "1.0.0")))
  (is-not (reg::version-satisfies-p "1.0.1" (reg::parse-constraint "1.0.0")))
  ;; Greater than
  (is (reg::version-satisfies-p "1.0.1" (reg::parse-constraint ">1.0.0")))
  (is-not (reg::version-satisfies-p "1.0.0" (reg::parse-constraint ">1.0.0")))
  ;; Greater than or equal
  (is (reg::version-satisfies-p "1.0.0" (reg::parse-constraint ">=1.0.0")))
  (is (reg::version-satisfies-p "1.0.1" (reg::parse-constraint ">=1.0.0")))
  ;; Less than
  (is (reg::version-satisfies-p "0.9.0" (reg::parse-constraint "<1.0.0")))
  (is-not (reg::version-satisfies-p "1.0.0" (reg::parse-constraint "<1.0.0"))))

(deftest test-caret-compatibility
  "Test caret (^) version compatibility"
  ;; ^1.2.3 allows >=1.2.3 <2.0.0
  (is (reg::caret-compatible-p "1.2.3" "1.2.3"))
  (is (reg::caret-compatible-p "1.2.4" "1.2.3"))
  (is (reg::caret-compatible-p "1.9.0" "1.2.3"))
  (is-not (reg::caret-compatible-p "2.0.0" "1.2.3"))
  (is-not (reg::caret-compatible-p "1.2.2" "1.2.3"))
  ;; ^0.2.3 allows >=0.2.3 <0.3.0
  (is (reg::caret-compatible-p "0.2.3" "0.2.3"))
  (is (reg::caret-compatible-p "0.2.9" "0.2.3"))
  (is-not (reg::caret-compatible-p "0.3.0" "0.2.3")))

(deftest test-tilde-compatibility
  "Test tilde (~) version compatibility"
  ;; ~1.2.3 allows >=1.2.3 <1.3.0
  (is (reg::tilde-compatible-p "1.2.3" "1.2.3"))
  (is (reg::tilde-compatible-p "1.2.9" "1.2.3"))
  (is-not (reg::tilde-compatible-p "1.3.0" "1.2.3"))
  (is-not (reg::tilde-compatible-p "1.2.2" "1.2.3")))

;;; Checksum tests

(deftest test-compute-checksum
  "Test checksum computation"
  (let ((checksum (reg:compute-checksum "hello world")))
    (is (str:starts-with-p "sha256:" checksum))
    (is-= (length checksum) 71)))

(deftest test-verify-checksum
  "Test checksum verification"
  (let* ((content "hello world")
         (checksum (reg:compute-checksum content)))
    (is (reg:verify-checksum content checksum))
    (is-thrown (reg::checksum-mismatch)
      (reg:verify-checksum "different content" checksum))))

;;; Package name validation tests

(deftest test-valid-package-name
  "Test package name validation"
  (is (reg::valid-package-name-p "epsilon.core"))
  (is (reg::valid-package-name-p "my-package"))
  (is (reg::valid-package-name-p "package123"))
  (is-not (reg::valid-package-name-p "123package"))
  (is-not (reg::valid-package-name-p ""))
  (is-not (reg::valid-package-name-p "package with spaces")))

;;; Registry configuration tests

(deftest test-add-registry
  "Test registry configuration"
  (reg:add-registry "test" "https://test.example.com/api/v1"
                    :primary t)
  (let ((reg (reg::get-registry "test")))
    (is-not-null reg)
    (is-equal (reg::registry-url reg) "https://test.example.com/api/v1")
    (is (reg::registry-primary-p reg)))
  (reg:remove-registry "test")
  (is-not (reg::get-registry "test")))

;;; mTLS authentication tests

(deftest test-admin-certificate-management
  "Test admin certificate fingerprint management"
  ;; Clean up first
  (reg:clear-admin-certificates)
  (is-= (length (reg:list-admin-certificates)) 0)
  ;; Add a valid fingerprint
  (let ((fp "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"))
    (reg:add-admin-certificate fp)
    (is-= (length (reg:list-admin-certificates)) 1)
    (is (member (string-downcase fp) (reg:list-admin-certificates) :test #'string=))
    ;; Adding same fingerprint twice shouldn't duplicate
    (reg:add-admin-certificate fp)
    (is-= (length (reg:list-admin-certificates)) 1)
    ;; Remove the fingerprint
    (reg:remove-admin-certificate fp)
    (is-= (length (reg:list-admin-certificates)) 0)))

(deftest test-valid-fingerprint
  "Test fingerprint validation"
  ;; Valid 64-char hex fingerprint
  (is (reg::valid-fingerprint-p "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"))
  (is (reg::valid-fingerprint-p "AABBCCDD00112233445566778899AABBCCDD00112233445566778899AABBCCDD"))
  ;; Invalid fingerprints
  (is-not (reg::valid-fingerprint-p "short"))
  (is-not (reg::valid-fingerprint-p "not-hex-characters-here-need-64-chars-but-with-invalid-charactersx"))
  (is-not (reg::valid-fingerprint-p nil))
  (is-not (reg::valid-fingerprint-p "")))

(deftest test-invalid-fingerprint-error
  "Test that adding invalid fingerprint throws error"
  (is-thrown (simple-error)
    (reg:add-admin-certificate "invalid")))

;;; Version parsing edge cases

(deftest test-parse-version-edge-cases
  "Test version parsing edge cases"
  ;; Version with both prerelease and build
  (let ((v (reg::parse-version "1.2.3-beta.1+build.456")))
    (is-= (reg::semver-major v) 1)
    (is-= (reg::semver-minor v) 2)
    (is-= (reg::semver-patch v) 3)
    (is-equal (reg::semver-prerelease v) "beta.1")
    (is-equal (reg::semver-build v) "build.456"))
  ;; Zero versions
  (let ((v (reg::parse-version "0.0.0")))
    (is-= (reg::semver-major v) 0)
    (is-= (reg::semver-minor v) 0)
    (is-= (reg::semver-patch v) 0)))

(deftest test-version-comparison-edge-cases
  "Test version comparison edge cases"
  ;; Same major, different minor
  (is (reg::version< "1.0.0" "1.1.0"))
  (is-not (reg::version< "1.1.0" "1.0.0"))
  ;; Same major and minor, different patch
  (is (reg::version< "1.1.0" "1.1.1"))
  ;; Large version numbers
  (is (reg::version< "99.99.99" "100.0.0"))
  ;; Version equality
  (is (reg::version= "10.20.30" "10.20.30")))

;;; Constraint parsing edge cases

(deftest test-parse-constraint-all-operators
  "Test all constraint operators"
  (let ((c (reg::parse-constraint "<=1.0.0")))
    (is-eq (reg::version-constraint-operator c) :lte)
    (is-equal (reg::version-constraint-version c) "1.0.0"))
  (let ((c (reg::parse-constraint "<2.0.0")))
    (is-eq (reg::version-constraint-operator c) :lt))
  (let ((c (reg::parse-constraint "=1.5.0")))
    (is-eq (reg::version-constraint-operator c) :eq)))

(deftest test-version-satisfies-lte
  "Test less-than-or-equal constraint"
  (is (reg::version-satisfies-p "1.0.0" (reg::parse-constraint "<=1.0.0")))
  (is (reg::version-satisfies-p "0.9.9" (reg::parse-constraint "<=1.0.0")))
  (is-not (reg::version-satisfies-p "1.0.1" (reg::parse-constraint "<=1.0.0"))))

;;; Caret compatibility edge cases

(deftest test-caret-compatibility-zero-major
  "Test caret compatibility with 0.x versions"
  ;; ^0.0.3 should only match 0.0.3 (very strict)
  (is (reg::caret-compatible-p "0.0.3" "0.0.3"))
  (is-not (reg::caret-compatible-p "0.0.4" "0.0.3"))
  (is-not (reg::caret-compatible-p "0.1.0" "0.0.3")))

;;; Package name validation edge cases

(deftest test-valid-package-name-edge-cases
  "Test package name validation edge cases"
  ;; Valid names
  (is (reg::valid-package-name-p "a"))
  (is (reg::valid-package-name-p "A"))
  (is (reg::valid-package-name-p "abc-def.ghi"))
  (is (reg::valid-package-name-p "package-1.0"))
  ;; Invalid names
  (is-not (reg::valid-package-name-p "-package"))
  (is-not (reg::valid-package-name-p ".package"))
  (is-not (reg::valid-package-name-p "package!"))
  (is-not (reg::valid-package-name-p "package@name")))

;;; Checksum format validation

(deftest test-valid-checksum-format
  "Test checksum format validation"
  ;; Valid format
  (is (reg::valid-checksum-format-p "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"))
  ;; Invalid formats
  (is-not (reg::valid-checksum-format-p "md5:abc123"))
  (is-not (reg::valid-checksum-format-p "sha256:tooshort"))
  (is-not (reg::valid-checksum-format-p "noprefixhash"))
  (is-not (reg::valid-checksum-format-p nil)))

;;; Package info validation

(deftest test-validate-package-info
  "Test package info validation"
  ;; Valid package info
  (let ((info (map:make-map "name" "test-package"
                            "version" "1.0.0"
                            "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")))
    (is (reg::validate-package-info info)))
  ;; Missing name
  (is-thrown (simple-error)
    (reg::validate-package-info
     (map:make-map "version" "1.0.0"
                   "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")))
  ;; Invalid version
  (is-thrown (simple-error)
    (reg::validate-package-info
     (map:make-map "name" "test"
                   "version" "not-a-version"
                   "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2"))))

;;; Admin token verification

(deftest test-verify-admin-token
  "Test admin token verification"
  ;; Admin tokens start with 'admin:'
  (is (reg::verify-admin-token "admin:secret123"))
  (is (reg::verify-admin-token "admin:"))
  ;; Non-admin tokens
  (is-not (reg::verify-admin-token "user:token"))
  (is-not (reg::verify-admin-token "regular-token"))
  (is-not (reg::verify-admin-token nil))
  (is-not (reg::verify-admin-token "")))
