;;;; Tests for epsilon.library module

(defpackage epsilon.library.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (lib epsilon.library)))

(in-package epsilon.library.tests)

; Tests for epsilon.library module

(deftest test-platform-detection
  "Test platform detection utilities"
  (let ((platform (lib:platform-string)))
    (assert (stringp platform))
    (assert (or (search "linux" platform)
		(search "darwin" platform)
		(search "windows" platform))))
  
  (let ((ext (lib:platform-library-extension)))
    (assert (member ext '(".so" ".dylib" ".dll") :test #'string=))))

(deftest test-platform-library-name
  "Test platform-specific library name generation"
  #+darwin
  (progn
    (assert (equal "libfoo.dylib" (lib:platform-library-name "foo")))
    (assert (equal "libfoo.3.dylib" (lib:platform-library-name "foo" "3"))))
  
  #+linux
  (progn
    (assert (equal "libfoo.so" (lib:platform-library-name "foo")))
    (assert (equal "libfoo.so.3" (lib:platform-library-name "foo" "3"))))
  
  #+windows
  (progn
    (assert (equal "foo.dll" (lib:platform-library-name "foo")))
    (assert (equal "foo-3.dll" (lib:platform-library-name "foo" "3")))))

(deftest test-library-registration
  "Test library registration"
  (lib:register-library 'test-lib
			:base-name "test"
			:version "1.0"
			:description "Test library")
  
  (let ((info (lib:library-info 'test-lib)))
    (assert info)
    (assert (equal "test" (getf info :base-name)))
    (assert (equal "1.0" (getf info :version)))
    (assert (equal "Test library" (getf info :description)))))

(deftest test-search-paths
  "Test search path management"
  (let ((original-paths (lib:get-search-paths)))
    (lib:add-search-path "/test/path" :prepend)
    (assert (equal "/test/path" (first (lib:get-search-paths))))
    
    (lib:add-search-path "/another/path" :append)
    (assert (member "/another/path" (lib:get-search-paths) :test #'string=))
    
    (lib:remove-search-path "/test/path")
    (assert (not (member "/test/path" (lib:get-search-paths) :test #'string=)))
    
    ;; Restore original paths
    (setf lib::*search-paths* original-paths)))

(deftest test-library-availability
  "Test checking library availability"
  ;; LibC should always be available
  (lib:define-library libc
		      :base-name "c"
		      :critical-p t
		      :search-names (list #+darwin "/usr/lib/libSystem.B.dylib"
					  #+linux "libc.so.6" #+linux "libc.so"
					  #+windows "msvcrt.dll"))
  
  ;; This might fail in some environments, but it's a reasonable test
  (when (probe-file #+darwin "/usr/lib/libSystem.B.dylib"
                    #+linux "/lib/x86_64-linux-gnu/libc.so.6"
                    #+windows "C:\\Windows\\System32\\msvcrt.dll"
                    #-(or darwin linux windows) "/usr/lib/libc.so")
    (assert (lib:library-available-p 'libc))))

(deftest test-diagnostics
  "Test library diagnostic functions"
  ;; Register a fake library that won't exist
  (lib:register-library 'fake-nonexistent-lib
			:base-name "totally-fake-lib-that-does-not-exist"
			:critical-p nil)
  
  ;; Check libraries should return status for all registered libs
  (let ((status (lib:check-libraries)))
    (assert (epsilon.map:map-p status)))
  
  ;; Available libraries should not include the fake one
  (assert (not (member 'fake-nonexistent-lib (lib:available-libraries))))
  
  ;; If we mark it as critical, it should appear in missing
  (lib:register-library 'fake-critical-lib
			:base-name "another-fake-lib"
			:critical-p t)
  (assert (member 'fake-critical-lib (lib:missing-libraries))))

(deftest test-manifest
  "Test library manifest creation"
  (let ((manifest (lib:get-library-manifest 'test-module)))
    (assert manifest)
    (assert (equal 'test-module (lib::library-manifest-module-name manifest)))
    (assert (null (lib::library-manifest-libraries manifest)))))

(deftest test-bundled-library-path
  "Test bundled library path generation"
  (let ((root (lib::get-bundled-library-root)))
    (assert (stringp root))
    (assert (search "library/lib" root)))
  
  ;; Test path generation (won't exist unless we bundle something)
  (let ((path (lib::get-bundled-library-path "test" "1.0")))
    ;; Path will be nil if not found, but we're testing the function works
    (assert (or (null path) (stringp path)))))

(deftest test-library-info-platforms
  "Test platform-specific library registration"
  (lib:register-library 'linux-only-lib
			:base-name "linux-specific"
			:platforms '("linux-x86_64"))
  
  #+linux
  (let ((info (lib:library-info 'linux-only-lib)))
    (assert info))
  
  ;; On non-Linux, finding this library should signal an error
  #-linux
  (handler-case
      (progn
        (lib:find-library 'linux-only-lib)
        (error "Should have signaled an error"))
    (error () t)))
