;;;; Tests for epsilon.update module

(defpackage epsilon.update.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (update epsilon.update)
   (str epsilon.string)))

(in-package epsilon.update.tests)

;;; Version Comparison Tests

(deftest version-parsing-simple
  (is (equal '(1 2 3) (update::parse-version "1.2.3")))
  (is (equal '(0 11 2) (update::parse-version "0.11.2")))
  (is (equal '(10 0 0) (update::parse-version "10.0.0"))))

(deftest version-parsing-with-v-prefix
  (is (equal '(1 2 3) (update::parse-version "v1.2.3")))
  (is (equal '(0 11 2) (update::parse-version "v0.11.2"))))

(deftest version-parsing-with-prerelease
  (is (equal '(1 0 0) (update::parse-version "1.0.0-alpha")))
  (is (equal '(1 0 0) (update::parse-version "1.0.0-beta.1")))
  (is (equal '(1 0 0) (update::parse-version "v1.0.0-rc.1"))))

(deftest version-comparison-equal
  (is (eq :equal (update:compare-versions "1.0.0" "1.0.0")))
  (is (eq :equal (update:compare-versions "v1.0.0" "1.0.0")))
  (is (eq :equal (update:compare-versions "0.11.2" "0.11.2"))))

(deftest version-comparison-greater
  (is (eq :greater (update:compare-versions "1.0.1" "1.0.0")))
  (is (eq :greater (update:compare-versions "1.1.0" "1.0.0")))
  (is (eq :greater (update:compare-versions "2.0.0" "1.9.9")))
  (is (eq :greater (update:compare-versions "0.12.0" "0.11.2"))))

(deftest version-comparison-less
  (is (eq :less (update:compare-versions "1.0.0" "1.0.1")))
  (is (eq :less (update:compare-versions "1.0.0" "1.1.0")))
  (is (eq :less (update:compare-versions "0.11.2" "0.12.0"))))

(deftest version-newer-p-test
  (is (update::version-newer-p "1.0.1" "1.0.0"))
  (is (update::version-newer-p "0.12.0" "0.11.2"))
  (is (not (update::version-newer-p "1.0.0" "1.0.0")))
  (is (not (update::version-newer-p "1.0.0" "1.0.1"))))

;;; Platform Detection Tests

(deftest platform-detection
  (let ((platform (update::detect-platform)))
    (is (stringp platform))
    (is (position #\- platform))  ; Should contain a dash
    ;; Should be in format OS-ARCH
    (let ((parts (epsilon.sequence:realize (str:split #\- platform))))
      (is (= 2 (length parts)))
      (is (member (first parts) '("linux" "macos" "darwin") :test #'string=))
      (is (member (second parts) '("x86_64" "arm64") :test #'string=)))))

;;; Installed Version Tests

(deftest get-installed-version-test
  (let ((version (update:get-installed-version)))
    (is (stringp version))
    ;; Should be parseable as version
    (is (listp (update::parse-version version)))))
