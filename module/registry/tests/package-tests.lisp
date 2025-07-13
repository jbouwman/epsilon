;;;; Tests for Advanced Package Definition Parser

(defpackage :epsilon.lib.package.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:pkg :epsilon.lib.package)
   (:map :epsilon.lib.map)
   (:edn :epsilon.lib.edn)))

(in-package :epsilon.lib.package.tests)

;;; Version parsing tests

(deftest parse-version-basic
  "Test basic version parsing"
  (let ((v (pkg:parse-version "1.2.3")))
    (is (= 1 (pkg:version-major v)))
    (is (= 2 (pkg:version-minor v)))
    (is (= 3 (pkg:version-patch v)))
    (is (null (pkg:version-prerelease v)))
    (is (null (pkg:version-build-metadata v)))))

(deftest parse-version-prerelease
  "Test version parsing with prerelease"
  (let ((v (pkg:parse-version "1.0.0-alpha.1")))
    (is (= 1 (pkg:version-major v)))
    (is (= 0 (pkg:version-minor v)))
    (is (= 0 (pkg:version-patch v)))
    (is (string= "alpha.1" (pkg:version-prerelease v)))))

(deftest parse-version-full
  "Test version parsing with prerelease and build metadata"
  (let ((v (pkg:parse-version "2.1.0-beta.2+build.456")))
    (is (= 2 (pkg:version-major v)))
    (is (= 1 (pkg:version-minor v)))
    (is (= 0 (pkg:version-patch v)))
    (is (string= "beta.2" (pkg:version-prerelease v)))
    (is (string= "build.456" (pkg:version-build-metadata v)))))

(deftest compare-versions-test
  "Test version comparison"
  (let ((v1 (pkg:parse-version "1.0.0"))
        (v2 (pkg:parse-version "1.0.1"))
        (v3 (pkg:parse-version "1.1.0"))
        (v4 (pkg:parse-version "2.0.0")))
    (is (= -1 (pkg:compare-versions v1 v2)))
    (is (= 1 (pkg:compare-versions v2 v1)))
    (is (= 0 (pkg:compare-versions v1 v1)))
    (is (= -1 (pkg:compare-versions v2 v3)))
    (is (= -1 (pkg:compare-versions v3 v4)))))

(deftest compare-versions-prerelease
  "Test version comparison with prerelease versions"
  (let ((v1 (pkg:parse-version "1.0.0"))
        (v2 (pkg:parse-version "1.0.0-alpha"))
        (v3 (pkg:parse-version "1.0.0-beta")))
    ;; Release version is greater than prerelease
    (is (= 1 (pkg:compare-versions v1 v2)))
    (is (= -1 (pkg:compare-versions v2 v1)))
    ;; Alpha comes before beta
    (is (= -1 (pkg:compare-versions v2 v3)))
    (is (= 1 (pkg:compare-versions v3 v2)))))

;;; Version constraint tests

(deftest parse-caret-constraint
  "Test caret version constraint parsing"
  (let ((c (pkg:parse-version-constraint "^1.2.3")))
    (is (eq :caret (pkg:version-constraint-type c)))
    (let ((min (pkg:version-constraint-min-version c))
          (max (pkg:version-constraint-max-version c)))
      (is (= 1 (pkg:version-major min)))
      (is (= 2 (pkg:version-minor min)))
      (is (= 3 (pkg:version-patch min)))
      (is (= 2 (pkg:version-major max)))
      (is (= 0 (pkg:version-minor max)))
      (is (= 0 (pkg:version-patch max))))))

(deftest parse-tilde-constraint
  "Test tilde version constraint parsing"
  (let ((c (pkg:parse-version-constraint "~1.2.3")))
    (is (eq :tilde (pkg:version-constraint-type c)))
    (let ((min (pkg:version-constraint-min-version c))
          (max (pkg:version-constraint-max-version c)))
      (is (= 1 (pkg:version-major min)))
      (is (= 2 (pkg:version-minor min)))
      (is (= 3 (pkg:version-patch min)))
      (is (= 1 (pkg:version-major max)))
      (is (= 3 (pkg:version-minor max)))
      (is (= 0 (pkg:version-patch max))))))

(deftest parse-range-constraint
  "Test range version constraint parsing"
  (let ((c (pkg:parse-version-constraint ">=1.0.0 <2.0.0")))
    (is (eq :range (pkg:version-constraint-type c)))
    (let ((min (pkg:version-constraint-min-version c))
          (max (pkg:version-constraint-max-version c)))
      (is (= 1 (pkg:version-major min)))
      (is (= 0 (pkg:version-minor min)))
      (is (= 0 (pkg:version-patch min)))
      (is (= 2 (pkg:version-major max)))
      (is (= 0 (pkg:version-minor max)))
      (is (= 0 (pkg:version-patch max))))))

(deftest parse-git-constraint
  "Test git dependency parsing"
  (let ((c1 (pkg:parse-version-constraint "git+https://github.com/user/repo.git@v1.2.3"))
        (c2 (pkg:parse-version-constraint "git+https://github.com/user/repo.git@abc123def"))
        (c3 (pkg:parse-version-constraint "git+https://github.com/user/repo.git")))
    ;; With tag
    (is (eq :git (pkg:version-constraint-type c1)))
    (is (string= "https://github.com/user/repo.git" (pkg:version-constraint-git-url c1)))
    (is (string= "v1.2.3" (pkg:version-constraint-git-ref c1)))
    
    ;; With commit hash
    (is (eq :git (pkg:version-constraint-type c2)))
    (is (string= "https://github.com/user/repo.git" (pkg:version-constraint-git-url c2)))
    (is (string= "abc123def" (pkg:version-constraint-git-ref c2)))
    
    ;; Default to main branch
    (is (eq :git (pkg:version-constraint-type c3)))
    (is (string= "https://github.com/user/repo.git" (pkg:version-constraint-git-url c3)))
    (is (string= "main" (pkg:version-constraint-git-ref c3)))))

(deftest version-satisfies-caret
  "Test caret constraint satisfaction"
  (let ((c (pkg:parse-version-constraint "^1.2.3")))
    ;; Should satisfy
    (is (pkg:version-satisfies-p (pkg:parse-version "1.2.3") c))
    (is (pkg:version-satisfies-p (pkg:parse-version "1.2.4") c))
    (is (pkg:version-satisfies-p (pkg:parse-version "1.3.0") c))
    (is (pkg:version-satisfies-p (pkg:parse-version "1.9.9") c))
    
    ;; Should not satisfy
    (is (not (pkg:version-satisfies-p (pkg:parse-version "1.2.2") c)))
    (is (not (pkg:version-satisfies-p (pkg:parse-version "2.0.0") c)))
    (is (not (pkg:version-satisfies-p (pkg:parse-version "0.9.9") c)))))

(deftest version-satisfies-tilde
  "Test tilde constraint satisfaction"
  (let ((c (pkg:parse-version-constraint "~1.2.3")))
    ;; Should satisfy
    (is (pkg:version-satisfies-p (pkg:parse-version "1.2.3") c))
    (is (pkg:version-satisfies-p (pkg:parse-version "1.2.4") c))
    (is (pkg:version-satisfies-p (pkg:parse-version "1.2.99") c))
    
    ;; Should not satisfy
    (is (not (pkg:version-satisfies-p (pkg:parse-version "1.2.2") c)))
    (is (not (pkg:version-satisfies-p (pkg:parse-version "1.3.0") c)))
    (is (not (pkg:version-satisfies-p (pkg:parse-version "2.0.0") c)))))

;;; Package definition parsing tests

(deftest parse-simple-package
  "Test parsing a simple package.edn"
  (let* ((edn-data (edn:read-edn-from-string
                    "{\"name\" \"my-package\"
                      \"version\" \"1.0.0\"
                      \"description\" \"A test package\"
                      \"author\" \"Test Author\"
                      \"sources\" [\"src\"]
                      \"tests\" [\"tests\"]}"))
         (pkg-def (pkg:parse-package-definition edn-data)))
    (is (string= "my-package" (pkg:package-definition-name pkg-def)))
    (is (string= "1.0.0" (pkg:package-definition-version pkg-def)))
    (is (string= "A test package" (pkg:package-definition-description pkg-def)))
    (is (equal '("Test Author") (pkg:package-definition-authors pkg-def)))
    (is (equal '("src") (pkg:package-definition-sources pkg-def)))
    (is (equal '("tests") (pkg:package-definition-tests pkg-def)))))

(deftest parse-dependencies
  "Test parsing dependencies with various formats"
  (let* ((edn-data (edn:read-edn-from-string
                    "{\"name\" \"my-package\"
                      \"version\" \"1.0.0\"
                      \"dependencies\" {
                        \"epsilon.core\" \"^2.1.0\"
                        \"epsilon.http\" \"~1.5.2\"
                        \"epsilon.database\" \">=1.0.0 <2.0.0\"
                        \"custom-lib\" {
                          \"version\" \"1.0.0\"
                          \"optional\" true
                        }
                        \"git-lib\" \"git+https://github.com/user/lib.git@v1.0.0\"
                      }}"))
         (pkg-def (pkg:parse-package-definition edn-data))
         (deps (pkg:package-definition-dependencies pkg-def)))
    
    ;; Check epsilon.core dependency
    (let ((core-dep (map:get deps "epsilon.core")))
      (is (not (null core-dep)))
      (is (string= "epsilon.core" (pkg:dependency-name core-dep)))
      (is (eq :caret (pkg:version-constraint-type (pkg:dependency-constraint core-dep)))))
    
    ;; Check optional dependency
    (let ((custom-dep (map:get deps "custom-lib")))
      (is (not (null custom-dep)))
      (is (pkg:dependency-optional custom-dep)))
    
    ;; Check git dependency
    (let ((git-dep (map:get deps "git-lib")))
      (is (not (null git-dep)))
      (is (eq :git (pkg:version-constraint-type (pkg:dependency-constraint git-dep)))))))

(deftest parse-features-and-profiles
  "Test parsing features and build profiles"
  (let* ((edn-data (edn:read-edn-from-string
                    "{\"name\" \"my-package\"
                      \"version\" \"1.0.0\"
                      \"features\" {
                        \"default\" [\"json\" \"logging\"]
                        \"full\" [\"json\" \"logging\" \"metrics\"]
                        \"json\" {
                          \"description\" \"JSON support\"
                          \"dependencies\" {\"epsilon.json\" \"^1.0.0\"}
                        }
                      }
                      \"build\" {
                        \"profiles\" {
                          \"dev\" {
                            \"features\" [\"default\" \"dev-tools\"]
                            \"optimization\" 0
                          }
                          \"release\" {
                            \"features\" [\"default\"]
                            \"optimization\" 3
                          }
                        }
                      }}"))
         (pkg-def (pkg:parse-package-definition edn-data))
         (features (pkg:package-definition-features pkg-def))
         (profiles (pkg:package-definition-build-profiles pkg-def)))
    
    ;; Check features
    (is (not (map:empty-p features)))
    (is (equal '("json" "logging") (map:get features "default")))
    (is (equal '("json" "logging" "metrics") (map:get features "full")))
    
    ;; Check build profiles
    (is (not (map:empty-p profiles)))
    (let ((dev-profile (map:get (map:get profiles "profiles") "dev")))
      (is (equal '("default" "dev-tools") (map:get dev-profile "features")))
      (is (= 0 (map:get dev-profile "optimization"))))))

;;; Lock file generation tests

(deftest generate-lock-file-test
  "Test lock file generation"
  (let* ((pkg-def (make-pkg:package-definition
                   :name "test-package"
                   :version "1.0.0"))
         (resolved-deps (map:make-map
                         "epsilon.core" '("epsilon.core" "2.1.5" "registry")
                         "git-lib" '("git-lib" "1.0.0" "git" "abc123def456")))
         (lock-data (pkg:generate-lock-file pkg-def resolved-deps)))
    
    (is (= 1 (map:get lock-data "version")))
    (is (not (null (map:get lock-data "dependencies"))))
    (is (not (null (map:get lock-data "build-metadata"))))
    
    ;; Check dependencies in lock file
    (let ((deps (map:get lock-data "dependencies")))
      (let ((core-lock (map:get deps "epsilon.core")))
        (is (string= "2.1.5" (map:get core-lock "version")))
        (is (string= "registry" (map:get core-lock "source")))
        (is (not (null (map:get core-lock "checksum")))))
      
      (let ((git-lock (map:get deps "git-lib")))
        (is (string= "1.0.0" (map:get git-lock "version")))
        (is (string= "git" (map:get git-lock "source")))
        (is (string= "abc123def456" (map:get git-lock "git-hash")))))))

;;; Utility tests

(deftest short-git-hash-test
  "Test git hash shortening"
  (is (string= "abc123d" (pkg:short-git-hash "abc123def456789")))
  (is (string= "short" (pkg:short-git-hash "short"))))

(deftest dependency-roundtrip
  "Test dependency parsing and serialization roundtrip"
  (let* ((dep-spec (map:make-map
                    "version" "^1.2.3"
                    "optional" t
                    "features" '("extra")))
         (dep (pkg:parse-dependency-spec "test-dep" dep-spec)))
    (is (string= "test-dep" (pkg:dependency-name dep)))
    (is (eq :caret (pkg:version-constraint-type (pkg:dependency-constraint dep))))
    (is (pkg:dependency-optional dep))
    (is (equal '("extra") (pkg:dependency-features dep)))))