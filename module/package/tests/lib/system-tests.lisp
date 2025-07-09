(defpackage :epsilon.lib.package.system.tests
  (:use
   :cl
   :epsilon.tool.test)
  (:local-nicknames
   (:sys :epsilon.lib.package.system)
   (:repo :epsilon.lib.package.repository)
   (:dep :epsilon.lib.package.dependency)
   (:boot :epsilon.lib.package.boot)
   (:builder :epsilon.lib.package.builder)
   (:map :epsilon.lib.map)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.lib.uri)))

(in-package :epsilon.lib.package.system.tests)

;;;; ==========================================================================
;;;; Repository Tests
;;;; ==========================================================================

(deftest test-repository-initialization
  "Test repository initialization"
  (let ((temp-repo (uri:merge (fs:temp-dir) "test-repo/")))
    (unwind-protect
         (progn
           (repo:initialize-repository temp-repo)
           (is (fs:exists-p temp-repo))
           (is (fs:exists-p (uri:merge temp-repo "packages/")))
           (is (fs:exists-p (uri:merge temp-repo "cache/")))
           (is (fs:exists-p (uri:merge temp-repo "index.edn"))))
      (when (fs:exists-p temp-repo)
        (fs:delete-directory (uri:path temp-repo))))))

(deftest test-cache-management
  "Test package cache management"
  (let ((temp-repo (uri:merge (fs:temp-dir) "test-cache/")))
    (unwind-protect
         (progn
           (repo:initialize-repository temp-repo)
           ;; Test cache index operations
           (let ((index (repo:load-cache-index temp-repo)))
             (is (map:map= index index))
             (repo:save-cache-index index temp-repo)
             (is (map:map= index (repo:load-cache-index temp-repo)))))
      (when (fs:exists-p temp-repo)
        (fs:delete-directory (uri:path temp-repo))))))

;;;; ==========================================================================
;;;; Dependency Resolution Tests
;;;; ==========================================================================

(deftest test-dependency-graph
  "Test dependency graph building"
  (let ((modules (map:make-map
                  "core" (uri:make-uri :scheme "file" :path "/test/core")
                  "http" (uri:make-uri :scheme "file" :path "/test/http"))))
    ;; This test would need mock module files
    ;; For now, just test that the function exists
    (is (fboundp 'dep:build-dependency-graph))))

(deftest test-topological-sort
  "Test topological sorting"
  ;; Create a simple test graph
  (let ((graph (map:make-map)))
    ;; Mock simple dependency: A -> B -> C
    (setf graph (map:assoc graph "A" 
                          (dep:make-dep-node :name "A" 
                                           :dependencies '("B")
                                           :dependents '())))
    (setf graph (map:assoc graph "B"
                          (dep:make-dep-node :name "B"
                                           :dependencies '("C") 
                                           :dependents '("A"))))
    (setf graph (map:assoc graph "C"
                          (dep:make-dep-node :name "C"
                                           :dependencies '()
                                           :dependents '("B"))))
    
    (let ((order (dep:topological-sort graph)))
      (is (listp order))
      (is (= 3 (length order)))
      ;; C should come before B, B before A
      (is (< (position "C" order :test #'string=)
             (position "B" order :test #'string=)))
      (is (< (position "B" order :test #'string=)
             (position "A" order :test #'string=))))))

;;;; ==========================================================================
;;;; Boot System Tests
;;;; ==========================================================================

(deftest test-boot-cache-paths
  "Test boot cache path generation"
  (let ((boot:*boot-cache-dir* (uri:merge (fs:temp-dir) "boot-test/")))
    (let ((cache-path (boot:get-boot-cache-path "test-module"))
          (manifest-path (boot:get-boot-manifest-path "test-module")))
      (is (str:ends-with-p (uri:path cache-path) "test-module.boot.fasl"))
      (is (str:ends-with-p (uri:path manifest-path) "test-module.manifest")))))

(deftest test-boot-manifest
  "Test boot manifest operations"
  (let ((boot:*boot-cache-dir* (uri:merge (fs:temp-dir) "manifest-test/")))
    (unwind-protect
         (progn
           (boot:save-boot-manifest "test" 
                                   (uri:make-uri :scheme "file" :path "/test")
                                   (get-universal-time)
                                   "test-hash")
           (let ((manifest (boot:load-boot-manifest "test")))
             (is (not (null manifest)))
             (is (string= "test" (map:get manifest :module-name)))
             (is (string= "test-hash" (map:get manifest :source-hash)))))
      (when (fs:exists-p boot:*boot-cache-dir*)
        (fs:delete-directory (uri:path boot:*boot-cache-dir*))))))

;;;; ==========================================================================
;;;; Module Discovery Tests
;;;; ==========================================================================

(deftest test-module-discovery
  "Test module discovery functionality"
  ;; Test that module discovery function exists and returns a map
  (let ((modules (builder:discover-modules)))
    (is (map:map= modules modules))
    ;; Should find at least the package module itself
    (is (map:contains-p modules "epsilon.package"))))

;;;; ==========================================================================
;;;; System Integration Tests
;;;; ==========================================================================

(deftest test-system-initialization
  "Test package system initialization"
  (let ((sys:*package-system-initialized* nil)
        (temp-repo (uri:merge (fs:temp-dir) "system-test/")))
    (unwind-protect
         (progn
           (sys:initialize-package-system :repository temp-repo)
           (is sys:*package-system-initialized*)
           (is (fs:exists-p temp-repo)))
      (setf sys:*package-system-initialized* nil)
      (when (fs:exists-p temp-repo)
        (fs:delete-directory (uri:path temp-repo))))))

;;;; ==========================================================================
;;;; Function Availability Tests
;;;; ==========================================================================

(deftest test-exported-functions
  "Test that all expected functions are exported and available"
  ;; Repository functions
  (is (fboundp 'repo:initialize-repository))
  (is (fboundp 'repo:get-package-path))
  (is (fboundp 'repo:package-cache-valid-p))
  
  ;; Dependency functions
  (is (fboundp 'dep:resolve-dependencies))
  (is (fboundp 'dep:compute-build-order))
  (is (fboundp 'dep:load-module-info))
  
  ;; Boot functions
  (is (fboundp 'boot:boot-module))
  (is (fboundp 'boot:quick-boot))
  (is (fboundp 'boot:clear-boot-cache))
  
  ;; Builder functions
  (is (fboundp 'builder:build-modules))
  (is (fboundp 'builder:discover-modules))
  
  ;; System functions
  (is (fboundp 'sys:initialize-package-system))
  (is (fboundp 'sys:second-stage-boot))
  (is (fboundp 'sys:dev-build)))