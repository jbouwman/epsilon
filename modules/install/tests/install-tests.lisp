;;;; epsilon.install tests
;;;;
;;;; Basic tests for the install module functionality.

(defpackage epsilon.install.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (manifest epsilon.install.manifest)
   (cache epsilon.install.cache)
   (str epsilon.string)))

(in-package epsilon.install.tests)

;;; Manifest parsing tests

(define-test manifest-parse-source-spec-github
  "Test parsing GitHub source specifications"
  (let ((spec (manifest:parse-source-spec "github:jbouwman/epsilon")))
    (assert-equal :github (manifest:source-type spec))
    (assert-equal "jbouwman" (manifest:source-owner spec))
    (assert-equal "epsilon" (manifest:source-repo spec))))

(define-test manifest-parse-source-spec-url
  "Test parsing URL source specifications"
  (let ((spec (manifest:parse-source-spec "https://example.com/module.tar.gz")))
    (assert-equal :url (manifest:source-type spec))
    (assert-equal "https://example.com/module.tar.gz" (manifest:source-url spec))))

(define-test manifest-parse-invalid-github-spec
  "Test that invalid GitHub specs raise errors"
  (assert-error 'manifest:manifest-error
                (manifest:parse-source-spec "github:invalid")))

;;; Dependency validation tests

(define-test manifest-validate-dependency-missing-name
  "Test that dependencies without names fail validation"
  (let* ((dep (make-instance 'manifest::dependency
                             :name nil
                             :source "github:user/repo"
                             :version "1.0.0"
                             :checksum "sha256:abc123"))
         (errors (manifest::validate-dependency dep)))
    (assert-true (> (length errors) 0))))

(define-test manifest-validate-dependency-missing-checksum
  "Test that dependencies without checksums fail validation"
  (let* ((dep (make-instance 'manifest::dependency
                             :name "test-module"
                             :source "github:user/repo"
                             :version "1.0.0"
                             :checksum nil))
         (errors (manifest::validate-dependency dep)))
    (assert-true (> (length errors) 0))))

(define-test manifest-validate-dependency-invalid-checksum-format
  "Test that checksums without sha256: prefix fail validation"
  (let* ((dep (make-instance 'manifest::dependency
                             :name "test-module"
                             :source "github:user/repo"
                             :version "1.0.0"
                             :checksum "abc123"))
         (errors (manifest::validate-dependency dep)))
    (assert-true (some (lambda (e) (str:contains-p "sha256:" e)) errors))))

;;; Cache tests

(define-test cache-normalize-module-name
  "Test module name normalization"
  (assert-equal "json-rpc" (cache::normalize-module-name "epsilon.json-rpc"))
  (assert-equal "json-rpc" (cache::normalize-module-name "json-rpc")))

(define-test cache-archive-filename
  "Test archive filename generation"
  (assert-equal "test-module-1.0.0.tar.gz"
                (cache::archive-filename "test-module" "1.0.0" :tar-gz))
  (assert-equal "test-module-1.0.0.zip"
                (cache::archive-filename "test-module" "1.0.0" :zip)))
