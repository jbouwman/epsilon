;;;; config-tests.lisp - Tests for compiler configuration

(defpackage :epsilon.compiler.config.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:config :epsilon.compiler.config)))

(in-package :epsilon.compiler.config.tests)

;;; Basic configuration tests

(deftest test-make-compiler-config
  (let ((cfg (config:make-compiler-config)))
    (is (config:compiler-config-p cfg))
    (is (equal "fasl" (config:compiler-config-fasl-extension cfg)))
    (is (eq t (config:compiler-config-inline-expansion cfg)))
    (is (eq nil (config:compiler-config-block-compilation cfg)))
    (is (= 100 (config:compiler-config-error-limit cfg)))
    (is (eq :file (config:compiler-config-progress-granularity cfg)))))

(deftest test-compiler-config-custom-values
  (let ((cfg (config:make-compiler-config
              :fasl-extension "fasl-custom"
              :error-limit 50
              :muffle-style-warnings t
              :parallel t
              :parallel-jobs 8)))
    (is (equal "fasl-custom" (config:compiler-config-fasl-extension cfg)))
    (is (= 50 (config:compiler-config-error-limit cfg)))
    (is (eq t (config:compiler-config-muffle-style-warnings cfg)))
    (is (eq t (config:compiler-config-parallel cfg)))
    (is (= 8 (config:compiler-config-parallel-jobs cfg)))))

(deftest test-copy-compiler-config
  (let* ((original (config:make-compiler-config
                    :error-limit 42
                    :fasl-extension "test"))
         (copy (config:copy-compiler-config original)))
    (is (config:compiler-config-p copy))
    (is (not (eq original copy)))
    (is (= 42 (config:compiler-config-error-limit copy)))
    (is (equal "test" (config:compiler-config-fasl-extension copy)))))

(deftest test-with-compiler-config
  (let ((outer-config config:*compiler-config*))
    (config:with-compiler-config (:error-limit 999
                                  :muffle-style-warnings t)
      (is (= 999 (config:compiler-config-error-limit config:*compiler-config*)))
      (is (eq t (config:compiler-config-muffle-style-warnings config:*compiler-config*)))
      ;; Original should be unchanged
      (is (not (eq outer-config config:*compiler-config*))))
    ;; After exiting, should be back to original
    (is (eq outer-config config:*compiler-config*))))

;;; Preset configuration tests

(deftest test-make-debug-config
  (let ((cfg (config:make-debug-config)))
    (is (config:compiler-config-p cfg))
    (is (eq t (config:compiler-config-preserve-source cfg)))
    (is (eq t (config:compiler-config-record-source-locations cfg)))
    (is (eq nil (config:compiler-config-muffle-style-warnings cfg)))
    ;; Check optimization qualities include debug 3
    (let ((qualities (config:compiler-config-optimize-qualities cfg)))
      (is (find 'debug qualities :key #'car))
      (is (= 3 (cadr (find 'debug qualities :key #'car)))))))

(deftest test-make-release-config
  (let ((cfg (config:make-release-config)))
    (is (config:compiler-config-p cfg))
    (is (eq nil (config:compiler-config-preserve-source cfg)))
    (is (eq t (config:compiler-config-muffle-style-warnings cfg)))
    (is (eq t (config:compiler-config-deterministic cfg)))
    (is (eq t (config:compiler-config-reproducible-builds cfg)))
    ;; Check optimization qualities include speed 3
    (let ((qualities (config:compiler-config-optimize-qualities cfg)))
      (is (find 'speed qualities :key #'car))
      (is (= 3 (cadr (find 'speed qualities :key #'car)))))))

(deftest test-make-development-config
  (let ((cfg (config:make-development-config)))
    (is (config:compiler-config-p cfg))
    (is (eq t (config:compiler-config-preserve-source cfg)))
    (is (eq t (config:compiler-config-record-source-locations cfg)))
    (is (eq nil (config:compiler-config-muffle-style-warnings cfg)))))
