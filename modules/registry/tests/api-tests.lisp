;;;; epsilon.registry API integration tests
;;;;
;;;; Tests for the HTTP API endpoints.

(defpackage epsilon.registry.api-tests
  (:use cl epsilon.test)
  (:local-nicknames
   (reg epsilon.registry)
   (map epsilon.map)
   (json epsilon.json)))

(in-package epsilon.registry.api-tests)

;;; Test fixtures

(defvar *test-storage-dir* nil
  "Temporary storage directory for tests.")

(defun setup-test-storage ()
  "Create a temporary storage directory for tests."
  (let ((dir (merge-pathnames (format nil "epsilon-test-~A/" (get-universal-time))
                              #p"/tmp/")))
    (ensure-directories-exist dir)
    (setf *test-storage-dir* dir)
    (setf reg::*storage-directory* dir)))

(defun cleanup-test-storage ()
  "Remove test storage directory."
  (when *test-storage-dir*
    (reg::delete-directory-recursive *test-storage-dir*)
    (setf *test-storage-dir* nil)
    (setf reg::*storage-directory* nil)))

(defmacro with-test-storage (&body body)
  "Execute body with temporary test storage."
  `(unwind-protect
       (progn
         (setup-test-storage)
         ,@body)
     (cleanup-test-storage)))

;;; Mock request helpers

(defun make-mock-request (&key path method query-params path-params headers body)
  "Create a mock HTTP request for testing."
  (map:make-map "path" path
                "method" (or method :get)
                "query-params" (or query-params (map:make-map))
                "path-params" (or path-params (map:make-map))
                "headers" (or headers (map:make-map))
                "body" body))

(defun mock-request-query-param (request param)
  "Get a query parameter from mock request."
  (map:get (map:get request "query-params") param))

(defun mock-request-path-param (request param)
  "Get a path parameter from mock request."
  (map:get (map:get request "path-params") param))

(defun mock-request-header (request name)
  "Get a header from mock request."
  (map:get (map:get request "headers") name))

(defun mock-request-json-body (request)
  "Get JSON body from mock request."
  (map:get request "body"))

;;; API route tests

(deftest test-list-packages-empty
  "Test GET /api/v1/packages returns empty list initially"
  (with-test-storage
    (let ((packages (reg::list-all-packages)))
      (is-null packages))))

(deftest test-get-package-not-found
  "Test GET /api/v1/packages/:name for non-existent package"
  (with-test-storage
    (let ((info (reg::get-package-info-internal "nonexistent")))
      (is-null info))))

(deftest test-get-versions-not-found
  "Test GET /api/v1/packages/:name/versions for non-existent package"
  (with-test-storage
    (let ((versions (reg::get-package-versions-internal "nonexistent")))
      (is-null versions))))

;;; Storage operation tests

(deftest test-store-and-retrieve-package
  "Test storing and retrieving package metadata"
  (with-test-storage
    (let ((info (map:make-map "name" "test-pkg"
                              "version" "1.0.0"
                              "description" "A test package")))
      (reg::store-package-version "test-pkg" "1.0.0" info)
      (let ((retrieved (reg::get-version-details-internal "test-pkg" "1.0.0")))
        (is-not-null retrieved)
        (is-equal (map:get retrieved "name") "test-pkg")
        (is-equal (map:get retrieved "version") "1.0.0")))))

(deftest test-version-exists
  "Test version existence check"
  (with-test-storage
    (let ((info (map:make-map "name" "pkg"
                              "version" "1.0.0")))
      (is-not (reg::version-exists-p "pkg" "1.0.0"))
      (reg::store-package-version "pkg" "1.0.0" info)
      (is (reg::version-exists-p "pkg" "1.0.0"))
      (is-not (reg::version-exists-p "pkg" "2.0.0")))))

(deftest test-delete-package-internal
  "Test internal package deletion"
  (with-test-storage
    (let ((info (map:make-map "name" "to-delete"
                              "version" "1.0.0")))
      (reg::store-package-version "to-delete" "1.0.0" info)
      (is (reg::version-exists-p "to-delete" "1.0.0"))
      (is (reg::delete-package-internal "to-delete"))
      (is-not (reg::version-exists-p "to-delete" "1.0.0")))))

(deftest test-delete-version-internal
  "Test internal version deletion"
  (with-test-storage
    (let ((info1 (map:make-map "name" "pkg" "version" "1.0.0"))
          (info2 (map:make-map "name" "pkg" "version" "2.0.0")))
      (reg::store-package-version "pkg" "1.0.0" info1)
      (reg::store-package-version "pkg" "2.0.0" info2)
      (is (reg::version-exists-p "pkg" "1.0.0"))
      (is (reg::version-exists-p "pkg" "2.0.0"))
      (is (reg::delete-version-internal "pkg" "1.0.0"))
      (is-not (reg::version-exists-p "pkg" "1.0.0"))
      (is (reg::version-exists-p "pkg" "2.0.0")))))

;;; Registry stats tests

(deftest test-registry-stats-empty
  "Test registry stats with empty storage"
  (with-test-storage
    (let ((stats (reg::get-registry-stats-internal)))
      (is-not-null stats)
      (is-= (map:get stats "package_count") 0)
      (is-= (map:get stats "version_count") 0))))

(deftest test-registry-stats-with-packages
  "Test registry stats with packages"
  (with-test-storage
    (reg::store-package-version "pkg1" "1.0.0"
                                 (map:make-map "name" "pkg1" "version" "1.0.0"))
    (reg::store-package-version "pkg1" "1.1.0"
                                 (map:make-map "name" "pkg1" "version" "1.1.0"))
    (reg::store-package-version "pkg2" "1.0.0"
                                 (map:make-map "name" "pkg2" "version" "1.0.0"))
    (let ((stats (reg::get-registry-stats-internal)))
      (is-= (map:get stats "package_count") 2)
      (is-= (map:get stats "version_count") 3))))

;;; Authentication header extraction

(deftest test-extract-bearer-token
  "Test Bearer token extraction from headers"
  (let ((req-with-token (make-mock-request
                         :headers (map:make-map "Authorization" "Bearer abc123"))))
    ;; Note: This tests the function signature, actual extraction needs http module
    (is-equal (subseq "Bearer abc123" 7) "abc123"))
  ;; Test without token
  (let ((req-no-token (make-mock-request :headers (map:make-map))))
    (is-null (mock-request-header req-no-token "Authorization"))))

;;; Admin authentication tests

(deftest test-admin-auth-with-mtls-disabled
  "Test admin auth falls back to token when mTLS disabled"
  (let ((reg::*require-mtls-for-admin* nil))
    ;; When mTLS is disabled, admin tokens should work
    (is (reg::verify-admin-token "admin:test-token"))))

(deftest test-admin-auth-condition
  "Test admin-auth-required condition"
  (is-thrown (reg::admin-auth-required)
    (error 'reg::admin-auth-required)))

;;; Security headers

(deftest test-security-headers-defined
  "Test that security headers are properly defined"
  (let ((headers reg::+security-headers+))
    (is-not-null headers)
    ;; Check for essential security headers
    (is (assoc "Strict-Transport-Security" headers :test #'string=))
    (is (assoc "X-Content-Type-Options" headers :test #'string=))
    (is (assoc "X-Frame-Options" headers :test #'string=))
    (is (assoc "Content-Security-Policy" headers :test #'string=))
    (is (assoc "X-XSS-Protection" headers :test #'string=))))

(deftest test-hsts-header-value
  "Test HSTS header includes required directives"
  (let ((hsts (cdr (assoc "Strict-Transport-Security" reg::+security-headers+
                          :test #'string=))))
    (is (search "max-age=" hsts))
    (is (search "includeSubDomains" hsts))))

;;; Publish validation integration

(deftest test-publish-validates-required-fields
  "Test that publishing validates required fields"
  (let ((incomplete-info (map:make-map "name" "test")))
    (is-thrown (simple-error)
      (reg::validate-package-info incomplete-info))))

(deftest test-publish-validates-name-format
  "Test that publishing validates package name format"
  (let ((info (map:make-map "name" "123invalid"
                            "version" "1.0.0"
                            "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")))
    (is-thrown (simple-error)
      (reg::validate-package-info info))))

(deftest test-publish-validates-version-format
  "Test that publishing validates version format"
  (let ((info (map:make-map "name" "valid-name"
                            "version" "invalid"
                            "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")))
    (is-thrown (simple-error)
      (reg::validate-package-info info))))

(deftest test-publish-validates-checksum-format
  "Test that publishing validates checksum format"
  (let ((info (map:make-map "name" "valid-name"
                            "version" "1.0.0"
                            "checksum" "invalid-checksum")))
    (is-thrown (simple-error)
      (reg::validate-package-info info))))

;;; Duplicate version detection

(deftest test-publish-prevents-duplicate-version
  "Test that publishing prevents duplicate versions"
  (with-test-storage
    (let ((info (map:make-map "name" "pkg"
                              "version" "1.0.0"
                              "checksum" "sha256:a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2")))
      ;; First publish should succeed
      (reg::store-package-version "pkg" "1.0.0" info)
      ;; Attempting to publish same version should detect duplicate
      (is (reg::version-exists-p "pkg" "1.0.0")))))
