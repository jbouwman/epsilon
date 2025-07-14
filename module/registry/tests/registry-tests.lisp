;;;; Tests for Module Registry and Package Cache

(defpackage :epsilon.lib.registry.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:reg :epsilon.lib.registry)
   (:pkg :epsilon.lib.package)
   (:map :epsilon.lib.map)
   (:path :epsilon.lib.path)))

(in-package :epsilon.lib.registry.tests)

;;; Test data

(defparameter *test-package-info*
  (map:make-map
   "name" "test-package"
   "description" "A test package"
   "authors" '("Test Author")
   "versions" '("1.0.0" "1.1.0" "2.0.0")
   "homepage" "https://example.com"
   "license" "MIT"))

(defparameter *test-version-info*
  (map:make-map
   "version" "1.0.0"
   "checksum" "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4"
   "dependencies" (map:make-map "epsilon.core" "^1.0.0")
   "archive_url" "https://example.com/test-package-1.0.0.tar.gz"
   "published_at" (get-universal-time)
   "yanked" nil))

;;; Registry management tests

(deftest registry-management
  "Test registry add/remove/list operations"
  ;; Save original registries
  (let ((original-registries reg:*registries*))
    (unwind-protect
         (progn
           ;; Clear registries for testing
           (setf reg:*registries* nil)
           
           ;; Add registries
           (reg:add-registry "test1" "https://test1.example.com/api/v1" :primary t)
           (reg:add-registry "test2" "https://test2.example.com/api/v1" 
                             :auth-token "secret123")
           
           ;; Check they were added
           (is (= 2 (length (reg:list-registries))))
           
           ;; Check primary
           (let ((primary (reg:primary-registry)))
             (is (string= "test1" (map:get primary "name")))
             (is (map:get primary "primary")))
           
           ;; Change primary
           (reg:set-primary-registry "test2")
           (let ((primary (reg:primary-registry)))
             (is (string= "test2" (map:get primary "name"))))
           
           ;; Remove registry
           (reg:remove-registry "test1")
           (is (= 1 (length (reg:list-registries))))
           
           ;; Test auth
           (reg:registry-auth "test2" "new-token")
           (let ((test2 (reg:find-registry "test2")))
             (is (string= "new-token" (map:get test2 "auth-token")))))
      
      ;; Restore original registries
      (setf reg:*registries* original-registries))))

(deftest registry-duplicate-error
  "Test that adding duplicate registry throws error"
  (let ((original-registries reg:*registries*))
    (unwind-protect
         (progn
           (setf reg:*registries* nil)
           (reg:add-registry "test" "https://test.example.com")
           (is-thrown (error)
             (reg:add-registry "test" "https://other.example.com")))
      (setf reg:*registries* original-registries))))

;;; Package parsing tests

(deftest parse-package-info
  "Test parsing package info from registry response"
  (let ((info (reg:parse-package-info *test-package-info*)))
    (is (string= "test-package" (reg:package-info-name info)))
    (is (string= "A test package" (reg:package-info-description info)))
    (is (equal '("Test Author") (reg:package-info-authors info)))
    (is (equal '("1.0.0" "1.1.0" "2.0.0") (reg:package-info-versions info)))
    (is (string= "https://example.com" (reg:package-info-homepage info)))
    (is (string= "MIT" (reg:package-info-license info)))))

(deftest parse-version-info
  "Test parsing version info from registry response"
  (let ((version (reg:parse-package-version *test-version-info*)))
    (is (string= "1.0.0" (reg:package-version-version version)))
    (is (string= "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4" 
                 (reg:package-version-checksum version)))
    (is (not (reg:package-version-yanked version)))
    (let ((deps (reg:package-version-dependencies version)))
      (is (string= "^1.0.0" (map:get deps "epsilon.core"))))))

;;; Cache management tests

(deftest cache-paths
  "Test cache path generation"
  (let ((package-path (reg:cache-package-path "test-pkg" "1.0.0"))
        (metadata-path (reg:cache-metadata-path "test-pkg")))
    
    ;; Check package path structure
    (is (str:contains-p "packages/test-pkg/1.0.0" 
                        (path:path-string package-path)))
    
    ;; Check metadata path structure
    (is (str:contains-p "metadata/test-pkg.json" 
                        (path:path-string metadata-path)))))

(deftest cache-operations
  "Test cache save/load operations"
  (let ((test-content #(1 2 3 4 5))
        (test-name "cache-test")
        (test-version "1.0.0")
        (original-cache-dir reg:*cache-dir*))
    
    ;; Use temp directory for cache
    (unwind-protect
         (progn
           (setf reg:*cache-dir* (path:path-merge "/tmp" 
                                                   (format nil "epsilon-test-~A" 
                                                           (random 10000))))
           
           ;; Ensure not cached initially
           (is (not (reg:is-cached-p test-name test-version)))
           
           ;; Save to cache
           (reg:save-to-cache test-name test-version test-content)
           
           ;; Check it's cached
           (is (reg:is-cached-p test-name test-version))
           
           ;; Load from cache
           (let ((loaded (reg:load-from-cache test-name test-version)))
             (is (equalp test-content loaded)))
           
           ;; Clear cache
           (reg:clear-cache)
           (is (not (reg:is-cached-p test-name test-version))))
      
      ;; Clean up and restore
      (when (probe-file (path:path-string reg:*cache-dir*))
        (reg:clear-cache))
      (setf reg:*cache-dir* original-cache-dir))))

(deftest checksum-verification
  "Test checksum verification"
  (let* ((content (map "test data"))
         (digest (digest:make-digest :sha-256))
         (_ (digest:digest-sequence digest content))
         (checksum (format nil "sha256:~A" 
                           (hex:u8-to-hex (digest:get-digest digest)))))
    
    ;; Correct checksum should verify
    (is (reg:verify-checksum content checksum))
    
    ;; Wrong checksum should fail
    (is (not (reg:verify-checksum content "sha256:wrong")))))

;;; Resolution tests

(deftest resolution-context
  "Test resolution context operations"
  (let ((ctx (make-reg:resolution-context)))
    
    ;; Add constraints
    (reg:add-constraint ctx "pkg1" 
                        (pkg:parse-version-constraint "^1.0.0"))
    (reg:add-constraint ctx "pkg2" 
                        (pkg:parse-version-constraint "~2.1.0"))
    
    ;; Check constraints were added
    (is (= 2 (map:count (reg:resolution-context-constraints ctx))))
    
    ;; Check unresolved
    (is (reg:has-unresolved-p ctx))
    
    ;; Mark one as resolved
    (setf (reg:resolution-context-resolved ctx)
          (map:assoc (reg:resolution-context-resolved ctx)
                     "pkg1" "1.2.0"))
    
    ;; Still has unresolved
    (is (reg:has-unresolved-p ctx))
    
    ;; Mark all resolved
    (setf (reg:resolution-context-resolved ctx)
          (map:assoc (reg:resolution-context-resolved ctx)
                     "pkg2" "2.1.5"))
    
    ;; No more unresolved
    (is (not (reg:has-unresolved-p ctx)))))

(deftest constraint-merging
  "Test version constraint merging"
  (let ((any-constraint (pkg:parse-version-constraint "*"))
        (exact-constraint (pkg:parse-version-constraint "1.2.3"))
        (caret-constraint (pkg:parse-version-constraint "^1.2.0")))
    
    ;; Any merged with anything returns the other
    (is (eq exact-constraint 
            (reg:merge-constraints any-constraint exact-constraint)))
    (is (eq exact-constraint 
            (reg:merge-constraints exact-constraint any-constraint)))
    
    ;; Same exact versions merge
    (is (not (null (reg:merge-constraints exact-constraint exact-constraint))))
    
    ;; Different constraints may not merge (simplified)
    (is (null (reg:merge-constraints exact-constraint caret-constraint)))))

(deftest find-matching-version
  "Test finding best matching version"
  (let ((versions (list
                   (make-reg:package-version :version "1.0.0")
                   (make-reg:package-version :version "1.1.0")
                   (make-reg:package-version :version "1.2.0")
                   (make-reg:package-version :version "2.0.0")))
        (constraint (pkg:parse-version-constraint "^1.1.0")))
    
    ;; Should find 1.2.0 as best match
    (let ((match (reg:find-matching-version versions constraint)))
      (is (not (null match)))
      (is (string= "1.2.0" (reg:package-version-version match))))))

;;; Offline mode tests

(deftest offline-mode
  "Test offline mode behavior"
  (let ((original-offline reg:*offline-mode*))
    (unwind-protect
         (progn
           (setf reg:*offline-mode* t)
           
           ;; Search should use cache in offline mode
           ;; (Would need mock cache implementation)
           
           ;; Download should error in offline mode
           (is-thrown (error)
             (reg:download-package "not-cached" "1.0.0")))
      
      (setf reg:*offline-mode* original-offline))))

;;; Integration test helpers

(defun mock-registry-server ()
  "Create a mock registry server for testing"
  ;; This would start an HTTP server that responds to registry API calls
  ;; For now, just a placeholder
  nil)

(deftest registry-integration
  "Integration test with mock registry"
  ;; This would test the full flow:
  ;; 1. Start mock registry
  ;; 2. Add mock registry to configuration
  ;; 3. Search for packages
  ;; 4. Download a package
  ;; 5. Verify it's cached
  ;; 6. Test offline mode with cache
  (skip "Integration test requires mock HTTP server"))

;;; Performance tests

(deftest cache-gc-performance
  "Test cache garbage collection performance"
  (let ((original-cache-dir reg:*cache-dir*))
    (unwind-protect
         (progn
           (setf reg:*cache-dir* (path:path-merge "/tmp" 
                                                   (format nil "epsilon-gc-test-~A" 
                                                           (random 10000))))
           
           ;; Create many cache entries
           (dotimes (i 100)
             (reg:save-to-cache (format nil "pkg-~D" i) "1.0.0" 
                                (map:u8-encode (format nil "content-~D" i))))
           
           ;; Run GC and measure time
           (let ((start-time (get-internal-real-time)))
             (reg:gc-cache :max-age-days 0)  ; Remove all
             (let ((elapsed (/ (- (get-internal-real-time) start-time)
                               internal-time-units-per-second)))
               ;; Should complete quickly even with many files
               (is (< elapsed 1.0))
               (format t "~&Cache GC of 100 entries took ~,3F seconds~%" elapsed))))
      
      ;; Clean up
      (when (probe-file (path:path-string reg:*cache-dir*))
        (reg:clear-cache))
      (setf reg:*cache-dir* original-cache-dir))))