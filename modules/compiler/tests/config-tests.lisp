;;;; config-tests.lisp - Tests for compiler configuration

(defpackage :epsilon.compiler.config.tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.compiler.config config)))

;;; Basic configuration tests

(deftest test-make-compiler-config
  (let ((cfg (config:make-compiler-config)))
    (assert-true (config:compiler-config-p cfg))
    (assert-true (equal "fasl" (config:compiler-config-fasl-extension cfg)))
    (assert-true (eq t (config:compiler-config-inline-expansion cfg)))
    (assert-true (eq nil (config:compiler-config-block-compilation cfg)))
    (assert-true (= 100 (config:compiler-config-error-limit cfg)))
    (assert-true (eq :file (config:compiler-config-progress-granularity cfg)))))

(deftest test-compiler-config-custom-values
  (let ((cfg (config:make-compiler-config
              :fasl-extension "fasl-custom"
              :error-limit 50
              :muffle-style-warnings t
              :parallel t
              :parallel-jobs 8)))
    (assert-true (equal "fasl-custom" (config:compiler-config-fasl-extension cfg)))
    (assert-true (= 50 (config:compiler-config-error-limit cfg)))
    (assert-true (eq t (config:compiler-config-muffle-style-warnings cfg)))
    (assert-true (eq t (config:compiler-config-parallel cfg)))
    (assert-true (= 8 (config:compiler-config-parallel-jobs cfg)))))

(deftest test-copy-compiler-config
  (let* ((original (config:make-compiler-config
                    :error-limit 42
                    :fasl-extension "test"))
         (copy (config:copy-compiler-config original)))
    (assert-true (config:compiler-config-p copy))
    (assert-true (not (eq original copy)))
    (assert-true (= 42 (config:compiler-config-error-limit copy)))
    (assert-true (equal "test" (config:compiler-config-fasl-extension copy)))))

(deftest test-with-compiler-config
  (let ((outer-config config:*compiler-config*))
    (config:with-compiler-config (:error-limit 999
                                  :muffle-style-warnings t)
      (assert-true (= 999 (config:compiler-config-error-limit config:*compiler-config*)))
      (assert-true (eq t (config:compiler-config-muffle-style-warnings config:*compiler-config*)))
      ;; Original should be unchanged
      (assert-true (not (eq outer-config config:*compiler-config*))))
    ;; After exiting, should be back to original
    (assert-true (eq outer-config config:*compiler-config*))))

;;; Preset configuration tests

(deftest test-make-debug-config
  (let ((cfg (config:make-debug-config)))
    (assert-true (config:compiler-config-p cfg))
    (assert-true (eq t (config:compiler-config-preserve-source cfg)))
    (assert-true (eq t (config:compiler-config-record-source-locations cfg)))
    (assert-true (eq nil (config:compiler-config-muffle-style-warnings cfg)))
    ;; Check optimization qualities include debug 3
    (let ((qualities (config:compiler-config-optimize-qualities cfg)))
      (assert-true (find 'debug qualities :key #'car))
      (assert-true (= 3 (cadr (find 'debug qualities :key #'car)))))))

(deftest test-make-release-config
  (let ((cfg (config:make-release-config)))
    (assert-true (config:compiler-config-p cfg))
    (assert-true (eq nil (config:compiler-config-preserve-source cfg)))
    (assert-true (eq t (config:compiler-config-muffle-style-warnings cfg)))
    (assert-true (eq t (config:compiler-config-deterministic cfg)))
    (assert-true (eq t (config:compiler-config-reproducible-builds cfg)))
    ;; Check optimization qualities include speed 3
    (let ((qualities (config:compiler-config-optimize-qualities cfg)))
      (assert-true (find 'speed qualities :key #'car))
      (assert-true (= 3 (cadr (find 'speed qualities :key #'car)))))))

(deftest test-make-development-config
  (let ((cfg (config:make-development-config)))
    (assert-true (config:compiler-config-p cfg))
    (assert-true (eq t (config:compiler-config-preserve-source cfg)))
    (assert-true (eq t (config:compiler-config-record-source-locations cfg)))
    (assert-true (eq nil (config:compiler-config-muffle-style-warnings cfg)))))
