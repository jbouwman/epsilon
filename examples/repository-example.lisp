;;;; Example of epsilon package system usage
;;;; Demonstrates index management, package creation, and dependency resolution

(require 'epsilon.core)
(require 'epsilon.package)

(defpackage #:epsilon.example.repository
  (:use #:cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:pkg #:epsilon.lib.package)
   (#:repo #:epsilon.lib.package.repository)
   (#:dep #:epsilon.lib.package.dependency)))

(in-package #:epsilon.example.repository)

;;; Example 1: Creating and managing a repository index

(defun example-create-index ()
  "Create a sample repository index with some packages"
  ;; Initialize repository
  (repo:initialize-repository)
  
  ;; Add epsilon.core package
  (repo:add-package-to-index 
   "epsilon.core" "1.0.0" "darwin-arm64" 
   "epsilon.core-1.0.0-darwin-arm64.epk" 1048576 "abc123..."
   '())
  
  ;; Add epsilon.http package with dependency on core
  (repo:add-package-to-index
   "epsilon.http" "1.0.0" "darwin-arm64"
   "epsilon.http-1.0.0-darwin-arm64.epk" 512000 "def456..."
   '(("epsilon.core" ">= 1.0.0")))
  
  ;; Add multiple versions of a package
  (repo:add-package-to-index
   "epsilon.json" "1.0.0" "darwin-arm64"
   "epsilon.json-1.0.0-darwin-arm64.epk" 256000 "ghi789..."
   '(("epsilon.core" ">= 1.0.0")))
  
  (repo:add-package-to-index
   "epsilon.json" "1.1.0" "darwin-arm64" 
   "epsilon.json-1.1.0-darwin-arm64.epk" 280000 "jkl012..."
   '(("epsilon.core" ">= 1.0.0")))
  
  ;; Add a commit-pinned package
  (repo:add-package-to-index
   "epsilon.experimental" "0.1.0" "darwin-arm64"
   "epsilon.experimental-0.1.0-darwin-arm64.epk" 128000 "mno345..."
   '(("epsilon.core" ">= 1.0.0"))
   :commit "abc123def456")
  
  (repo:read-repository-index))

;;; Example 2: Version specification parsing and resolution

(defun example-version-parsing ()
  "Demonstrate version specification parsing"
  (let ((specs '("1.0.0"                    ; Exact version
                 ">= 1.0.0"                 ; Minimum version
                 "> 1.0.0, < 2.0.0"        ; Range
                 "~> 1.0.0"                 ; Compatible version
                 "@abc123"                  ; Commit hash
                 "main")))                  ; Git ref
    
    (format t "~&Version Specification Parsing:~%")
    (dolist (spec specs)
      (let ((parsed (dep:parse-version-spec spec)))
        (format t "  ~A -> ~A~%" spec (map:get parsed :type))))
    
    ;; Test version comparisons
    (format t "~&Version Comparisons:~%")
    (let ((versions '(("1.0.0" "1.0.1" :less)
                      ("1.1.0" "1.0.0" :greater)  
                      ("2.0.0" "2.0.0" :equal))))
      (dolist (test versions)
        (let ((result (dep:compare-versions (first test) (second test))))
          (format t "  ~A vs ~A = ~A (expected ~A)~%" 
                  (first test) (second test) result (third test)))))))

;;; Example 3: Package filename operations

(defun example-package-filenames ()
  "Demonstrate package filename creation and parsing"
  (format t "~&Package Filename Operations:~%")
  
  ;; Create filenames
  (let ((filename1 (pkg:make-package-filename "epsilon.http" "1.0.0"))
        (filename2 (pkg:make-package-filename "epsilon.json" "2.1.0" "linux" "x86_64")))
    
    (format t "  Generated: ~A~%" filename1)
    (format t "  Generated: ~A~%" filename2)
    
    ;; Parse filenames
    (let ((parsed1 (pkg:parse-package-filename filename1))
          (parsed2 (pkg:parse-package-filename filename2)))
      
      (format t "  Parsed ~A:~%" filename1)
      (format t "    Name: ~A~%" (map:get parsed1 :name))
      (format t "    Version: ~A~%" (map:get parsed1 :version))
      (format t "    Platform: ~A~%" (map:get parsed1 :platform))
      
      (format t "  Parsed ~A:~%" filename2)
      (format t "    Name: ~A~%" (map:get parsed2 :name))
      (format t "    Version: ~A~%" (map:get parsed2 :version))
      (format t "    Platform: ~A~%" (map:get parsed2 :platform)))))

;;; Example 4: Dependency resolution

(defun example-dependency-resolution ()
  "Demonstrate dependency resolution with version constraints"
  (example-create-index)
  
  (format t "~&Dependency Resolution:~%")
  
  ;; Version resolution examples
  (let ((tests '(("epsilon.json" "1.0.0")
                 ("epsilon.json" ">= 1.0.0")
                 ("epsilon.json" "~> 1.0.0"))))
    
    (dolist (test tests)
      (let* ((package-name (first test))
             (version-spec (second test))
             (resolved (repo:resolve-version package-name version-spec "darwin-arm64")))
        (format t "  ~A ~A -> ~A~%" package-name version-spec 
                (or resolved "not found"))))))

;;; Example 5: Repository structure simulation

(defun example-repository-structure ()
  "Show how the repository would be organized on disk"
  (format t "~&Repository Structure Example:~%")
  (format t "~A~%" "~/.epsilon/")
  (format t "~A~%" "├── bin/")
  (format t "~A~%" "│   └── epsilon")
  (format t "~A~%" "├── repository/")
  (format t "~A~%" "│   ├── index.edn")
  (format t "~A~%" "│   └── packages/")
  (format t "~A~%" "│       ├── epsilon.core/")
  (format t "~A~%" "│       │   └── 1.0.0/")
  (format t "~A~%" "│       │       └── epsilon.core-1.0.0-darwin-arm64.epk")
  (format t "~A~%" "│       ├── epsilon.http/")
  (format t "~A~%" "│       │   ├── 1.0.0/")
  (format t "~A~%" "│       │   │   └── epsilon.http-1.0.0-darwin-arm64.epk")
  (format t "~A~%" "│       │   └── 1.1.0/")
  (format t "~A~%" "│       │       └── epsilon.http-1.1.0-darwin-arm64.epk")
  (format t "~A~%" "│       └── epsilon.json/")
  (format t "~A~%" "│           ├── 1.0.0/")
  (format t "~A~%" "│           │   └── epsilon.json-1.0.0-darwin-arm64.epk")
  (format t "~A~%" "│           └── 1.1.0/")
  (format t "~A~%" "│               └── epsilon.json-1.1.0-darwin-arm64.epk"))

;;; Main example runner

(defun run-all-examples ()
  "Run all repository system examples"
  (format t "~&========================================~%")
  (format t "Epsilon Repository System Examples~%")
  (format t "========================================~%")
  
  (example-version-parsing)
  (format t "~%")
  
  (example-package-filenames)
  (format t "~%")
  
  (example-dependency-resolution)
  (format t "~%")
  
  (example-repository-structure)
  (format t "~%")
  
  (format t "Examples completed!~%"))

;; Run examples when this file is loaded
;(run-all-examples) ; Uncomment to run automatically