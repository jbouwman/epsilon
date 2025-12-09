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
