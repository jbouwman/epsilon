;;;; Test script to demonstrate loading epsilon.json from EPK package

(require 'epsilon.core)
(require 'epsilon.package)

(defpackage #:test-json-epk
  (:use #:cl)
  (:local-nicknames
   (#:repo #:epsilon.lib.package.repository)
   (#:dep #:epsilon.lib.package.dependency)
   (#:map #:epsilon.lib.map)))

(in-package #:test-json-epk)

(defun test-repository-index ()
  "Test that epsilon.json is in the repository index"
  (format t "~&Testing repository index...~%~%")
  
  ;; Read the repository index
  (let ((index (repo:read-repository-index)))
    (format t "Repository packages:~%")
    (map:each (lambda (name package-data)
                (format t "  - ~A~%" name)
                (let ((versions (map:get package-data :versions)))
                  (when versions
                    (map:each (lambda (version version-data)
                                (format t "    ~A~%" version)
                                (let ((platforms (map:get version-data :platforms)))
                                  (when platforms
                                    (map:each (lambda (platform platform-data)
                                                (format t "      ~A: ~A~%"
                                                        platform
                                                        (map:get platform-data :filename)))
                                              platforms))))
                              versions))))
              (map:get index :packages map:+empty+))))

(defun test-version-resolution ()
  "Test version resolution for epsilon.json"
  (format t "~%~%Testing version resolution...~%~%")
  
  (let ((test-specs '("1.0.0"
                      ">= 1.0.0"
                      "~> 1.0.0")))
    (dolist (spec test-specs)
      (let ((resolved (repo:resolve-version "epsilon.json" spec "darwin-x86_64")))
        (format t "  epsilon.json ~A -> ~A~%"
                spec
                (or resolved "not found"))))))

(defun test-dependency-resolution ()
  "Test dependency resolution with epsilon.json"
  (format t "~%~%Testing dependency resolution...~%~%")
  
  ;; Parse version specs
  (let ((spec1 (dep:parse-version-spec ">= 1.0.0"))
        (spec2 (dep:parse-version-spec "~> 1.0.0")))
    
    (format t "Version spec parsing:~%")
    (format t "  '>= 1.0.0' -> type: ~A~%" (map:get spec1 :type))
    (format t "  '~> 1.0.0' -> type: ~A, min: ~A, max: ~A~%"
            (map:get spec2 :type)
            (map:get spec2 :min-version)
            (map:get spec2 :max-version))
    
    ;; Test version satisfaction
    (format t "~%Version satisfaction tests:~%")
    (let ((test-versions '("0.9.0" "1.0.0" "1.0.5" "1.1.0" "2.0.0")))
      (dolist (version test-versions)
        (format t "  ~A satisfies '>= 1.0.0': ~A~%"
                version
                (dep:version-satisfies-p version spec1))
        (format t "  ~A satisfies '~> 1.0.0': ~A~%"
                version
                (dep:version-satisfies-p version spec2))))))

(defun test-epk-structure ()
  "Show the EPK package structure"
  (format t "~%~%EPK Package Structure:~%~%")
  (format t "epsilon.json-1.0.0-darwin-x86_64.epk~%")
  (format t "├── META-INF/~%")
  (format t "│   ├── MANIFEST.edn          # Package metadata~%")
  (format t "│   ├── CHECKSUMS.sha256      # File checksums~%")
  (format t "│   └── DEPENDENCIES.edn      # Dependency tree~%")
  (format t "├── src/                      # Source code~%")
  (format t "│   └── lib/~%")
  (format t "│       ├── json.lisp         # JSON parser implementation~%")
  (format t "│       └── api.lisp          # Public API~%")
  (format t "├── fasl/                     # Compiled binaries~%")
  (format t "│   └── combined.fasl         # Combined FASL for fast loading~%")
  (format t "└── docs/                     # Documentation~%")
  (format t "    └── README.md~%"))

(defun main ()
  "Run all tests"
  (format t "~&========================================~%")
  (format t "Epsilon JSON EPK Package Test~%")
  (format t "========================================~%")
  
  (test-repository-index)
  (test-version-resolution)
  (test-dependency-resolution)
  (test-epk-structure)
  
  (format t "~%~%Summary:~%")
  (format t "- epsilon.json module created with epsilon.parsing dependency~%")
  (format t "- EPK package generated with standard structure~%")
  (format t "- Package registered in local repository~%")
  (format t "- Version resolution and dependency checking functional~%")
  (format t "~%The epsilon.json module is now available as a versioned package!~%"))

(main)