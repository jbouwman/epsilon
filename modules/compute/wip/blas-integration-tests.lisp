(defpackage epsilon.compute.blas-integration-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (c epsilon.compute)
   (ffi epsilon.compute.blas-ffi)))

(in-package epsilon.compute.blas-integration-tests)

(deftest test-openblas-available
  "Test that OpenBLAS library can be loaded"
  ;; Check if we can find the OpenBLAS library
  (let ((lib-path (or (probe-file "/nix/store/q1qvrm5b4d8ra6n5n5kc3da9p5lnb3az-openblas-0.3.24/lib/libopenblas.so")
                      (probe-file "/usr/lib/libopenblas.so")
                      (probe-file "/usr/lib/x86_64-linux-gnu/libopenblas.so"))))
    (is (not (null lib-path)))))

(deftest test-blas-constants
  "Test BLAS constants are defined"
  (is (= ffi:+cblas-row-major+ 101))
  (is (= ffi:+cblas-col-major+ 102))
  (is (= ffi:+cblas-no-trans+ 111))
  (is (= ffi:+cblas-trans+ 112)))

(deftest test-foreign-module-available
  "Test that epsilon.foreign module is available"
  (is (find-package 'epsilon.foreign))
  (is (fboundp 'epsilon.foreign:defshared))
  (is (fboundp 'epsilon.foreign:foreign-alloc))
  (is (fboundp 'epsilon.foreign:foreign-free)))