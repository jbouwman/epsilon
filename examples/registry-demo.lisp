;;;; Package Registry and Caching System Demo
;;;;
;;;; This example demonstrates the complete package management workflow
;;;; including registry operations, dependency resolution, and caching.

;; Load Epsilon system
(load "/home/jbouwman/git/epsilon/scripts/epsilon.lisp")

;; For demo purposes, we'll simulate registry operations
(defpackage :registry-demo
  (:use :cl)
  (:local-nicknames
   (:reg :epsilon.lib.registry)
   (:pkg :epsilon.lib.package)
   (:cli :epsilon.tool.package-cli)
   (:map :epsilon.lib.map)
   (:path :epsilon.lib.path)))

(in-package :registry-demo)

(format t "~&=== Epsilon Package Registry Demo ===~%~%")

;; 1. Registry Configuration
(format t "1. Setting up registries...~%")

;; Add multiple registries (official, backup, private)
(reg:add-registry "official" 
                  "https://packages.epsilon-lang.org/api/v1"
                  :primary t)

(reg:add-registry "backup" 
                  "https://backup.epsilon-lang.org/api/v1"
                  :mirrors '("https://mirror1.epsilon.org" 
                            "https://mirror2.epsilon.org"))

(reg:add-registry "corporate" 
                  "https://packages.mycompany.com/api/v1"
                  :auth-token "secret-company-token")

(format t "   Configured ~D registries~%" (length (reg:list-registries)))

;; Show registry configuration
(cli:registry-list)

;; 2. Package Search and Discovery
(format t "~%2. Package discovery...~%")

;; This would normally make HTTP requests to registry
;; For demo, we'll simulate the responses
(let ((mock-search-results 
       (list
        (make-reg:package-info
         :name "epsilon.json"
         :description "Fast JSON parsing and generation"
         :authors '("Epsilon Team")
         :versions '("1.2.3" "1.2.2" "1.2.1")
         :license "MIT"
         :homepage "https://epsilon-lang.org/json")
        
        (make-reg:package-info
         :name "epsilon.crypto"
         :description "Cryptographic functions and utilities"
         :authors '("Security Team")
         :versions '("2.0.1" "2.0.0")
         :license "Apache-2.0"
         :homepage "https://epsilon-lang.org/crypto"))))
  
  (format t "   Found packages:~%")
  (dolist (pkg mock-search-results)
    (format t "     ~A v~A - ~A~%"
            (reg:package-info-name pkg)
            (first (reg:package-info-versions pkg))
            (reg:package-info-description pkg))))

;; 3. Create a sample project
(format t "~%3. Creating sample project...~%")

(let ((project-def 
       (pkg:make-package-definition
        :name "my-web-app"
        :version "1.0.0"
        :description "A web application using Epsilon"
        :authors '("Developer <dev@example.com>")
        
        ;; Dependencies with various constraint types
        :dependencies (map:from-list
                       '(("epsilon.core" 
                          ,(pkg:make-dependency
                            :name "epsilon.core"
                            :constraint (pkg:parse-version-constraint "^2.1.0")))
                         ("epsilon.http" 
                          ,(pkg:make-dependency
                            :name "epsilon.http"  
                            :constraint (pkg:parse-version-constraint "~1.5.2")))
                         ("epsilon.json"
                          ,(pkg:make-dependency
                            :name "epsilon.json"
                            :constraint (pkg:parse-version-constraint ">=1.2.0 <2.0.0")))
                         ("custom-auth"
                          ,(pkg:make-dependency
                            :name "custom-auth"
                            :constraint (pkg:parse-version-constraint 
                                        "git+https://github.com/company/auth.git@v1.5.0")))))
        
        :dev-dependencies (map:from-list
                          '(("epsilon.testing"
                             ,(pkg:make-dependency
                               :name "epsilon.testing"
                               :constraint (pkg:parse-version-constraint "*")))))
        
        :features (map:from-list
                   '(("default" '("json" "logging"))
                     ("full" '("json" "logging" "metrics" "auth"))
                     ("json" (map:make-map 
                              "description" "JSON support"
                              "dependencies" (map:make-map "epsilon.json" "^1.2.0")))
                     ("metrics" (map:make-map
                                 "description" "Performance metrics"
                                 "dependencies" (map:make-map "epsilon.metrics" "^1.0.0"))))))))

  (format t "   Project: ~A v~A~%"
          (pkg:package-definition-name project-def)
          (pkg:package-definition-version project-def))
  
  (format t "   Dependencies: ~D~%"
          (map:count (pkg:package-definition-dependencies project-def)))

  ;; 4. Dependency Resolution
  (format t "~%4. Resolving dependencies...~%")
  
  ;; Mock the resolution process
  (let ((mock-resolved 
         (map:from-list
          '(("epsilon.core" "2.1.5")
            ("epsilon.http" "1.5.8") 
            ("epsilon.json" "1.2.3")
            ("custom-auth" "1.5.0+git.abc123")
            ("epsilon.testing" "1.0.2")))))
    
    (format t "   Resolved versions:~%")
    (map:reduce (lambda (_ name version)
                  (declare (ignore _))
                  (format t "     ~A: ~A~%" name version))
                nil
                mock-resolved)
    
    ;; 5. Lock File Generation
    (format t "~%5. Generating lock file...~%")
    
    (let ((lock-data (pkg:generate-lock-file project-def mock-resolved)))
      (format t "   Generated epsilon-lock.edn with:~%")
      (format t "     Format version: ~D~%"
              (map:get lock-data "version"))
      (format t "     Dependencies: ~D~%"
              (map:count (map:get lock-data "dependencies")))
      (format t "     Build metadata: ~A~%"
              (map:get (map:get lock-data "build-metadata") "platform"))
      
      ;; Show sample lock entry
      (let ((core-lock (map:get (map:get lock-data "dependencies") "epsilon.core")))
        (format t "     Sample entry: epsilon.core@~A (~A)~%"
                (map:get core-lock "version")
                (subseq (map:get core-lock "checksum") 0 16))))))

;; 6. Cache Operations
(format t "~%6. Package caching...~%")

;; Show cache configuration
(format t "   Cache directory: ~A~%" 
        (path:path-string reg:*cache-dir*))

;; Simulate downloading and caching packages
(format t "   Simulating package downloads:~%")
(let ((packages '(("epsilon.core" "2.1.5")
                  ("epsilon.http" "1.5.8") 
                  ("epsilon.json" "1.2.3"))))
  
  (dolist (pkg packages)
    (destructuring-bind (name version) pkg
      ;; Simulate download by creating cache entry
      (let ((mock-content (map:u8-encode 
                           (format nil "Mock package content for ~A@~A" 
                                   name version))))
        (reg:save-to-cache name version mock-content)
        (format t "     ✓ ~A@~A (~D bytes)~%" 
                name version (length mock-content)))))

;; Verify cache
(format t "   Cache verification:~%")
(let ((cached-count 0))
  (dolist (pkg '(("epsilon.core" "2.1.5")
                 ("epsilon.http" "1.5.8") 
                 ("epsilon.json" "1.2.3")))
    (destructuring-bind (name version) pkg
      (when (reg:is-cached-p name version)
        (incf cached-count)
        (format t "     ✓ ~A@~A cached~%" name version))))
  
  (format t "   Total cached packages: ~D~%" cached-count))

;; 7. Version Constraint Examples
(format t "~%7. Version constraint examples...~%")

(let ((constraints '("^2.1.0"     ; Caret: >=2.1.0 <3.0.0
                     "~1.5.2"     ; Tilde: >=1.5.2 <1.6.0  
                     ">=1.0.0 <2.0.0"  ; Range
                     "*"          ; Any version
                     "git+https://github.com/user/repo.git@v1.0.0"))) ; Git
  
  (dolist (constraint-str constraints)
    (let ((constraint (pkg:parse-version-constraint constraint-str)))
      (format t "   ~A -> ~A constraint~%"
              constraint-str
              (pkg:version-constraint-type constraint)))))

;; 8. Offline Mode Demo
(format t "~%8. Offline mode demonstration...~%")

(let ((original-offline reg:*offline-mode*))
  (unwind-protect
       (progn
         (setf reg:*offline-mode* t)
         (format t "   Offline mode enabled~%")
         
         ;; Show that cached packages are available
         (format t "   Available offline:~%")
         (dolist (pkg '(("epsilon.core" "2.1.5")
                        ("epsilon.http" "1.5.8")))
           (destructuring-bind (name version) pkg
             (if (reg:is-cached-p name version)
                 (format t "     ✓ ~A@~A~%" name version)
                 (format t "     ✗ ~A@~A (not cached)~%" name version))))
         
         (format t "   Offline mode allows development without network access~%"))
    
    ;; Restore offline mode
    (setf reg:*offline-mode* original-offline)))

;; 9. CLI Command Examples
(format t "~%9. CLI command examples...~%")

(format t "   Example commands:~%")
(format t "     epsilon add epsilon.json --version '^1.2.0'~%")
(format t "     epsilon add custom-lib --git https://github.com/user/lib.git@main~%")
(format t "     epsilon update~%")
(format t "     epsilon install --frozen~%")
(format t "     epsilon publish~%")
(format t "     epsilon registry add corporate https://packages.company.com~%")

;; 10. Security Features
(format t "~%10. Security features...~%")

(format t "   Checksum verification: ~A~%" 
        (if reg:*verify-checksums* "ENABLED" "DISABLED"))

;; Test checksum verification
(let* ((test-content (map:u8-encode "test content"))
       (digest (digest:make-digest :sha-256))
       (_ (digest:digest-sequence digest test-content))
       (checksum (format nil "sha256:~A" 
                         (hex:u8-to-hex (digest:get-digest digest)))))
  
  (format t "   Example checksum verification: ~A~%"
          (if (reg:verify-checksum test-content checksum)
              "PASSED" "FAILED")))

;; Clean up demo
(format t "~%11. Cleanup...~%")
(reg:clear-cache)
(format t "   Cache cleared~%")

(format t "~%=== Demo Complete ===~%")
(format t "~%The Epsilon package registry system provides:~%")
(format t "  • Modern dependency management with semantic versioning~%")
(format t "  • Git hash pinning for reproducible builds~%") 
(format t "  • Local caching for offline development~%")
(format t "  • Multiple registry support with fallbacks~%")
(format t "  • Security through checksum verification~%")
(format t "  • Lock files for deployment reproducibility~%")
(format t "  • Feature flags for conditional dependencies~%")
(format t "~%This positions Epsilon with a world-class package ecosystem!~%")