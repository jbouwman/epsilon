(defpackage epsilon.update.tests
  (:use cl epsilon.tool.test)
  (:local-nicknames
   (update epsilon.update)))

(in-package epsilon.update.tests)

(deftest test-version-parsing
  "Test version string parsing"
  (is-equal '(1 2 3) (update::parse-version "1.2.3"))
  (is-equal '(0 1 0) (update::parse-version "0.1.0"))
  (is-equal '(2 0 0) (update::parse-version "2.0.0")))

(deftest test-version-comparison
  "Test version comparison logic"
  (is-equal :newer (update:version-compare "1.2.3" "1.2.2"))
  (is-equal :older (update:version-compare "1.2.1" "1.2.3"))
  (is-equal :equal (update:version-compare "1.2.3" "1.2.3"))
  (is-equal :newer (update:version-compare "2.0.0" "1.9.9"))
  (is-equal :older (update:version-compare "1.9.9" "2.0.0")))

(deftest test-version-newer-p
  "Test version newer predicate"
  (is-true (update:version-newer-p "1.2.3" "1.2.2"))
  (is-false (update:version-newer-p "1.2.1" "1.2.3"))
  (is-false (update:version-newer-p "1.2.3" "1.2.3")))

(deftest test-github-urls
  "Test GitHub API URL generation"
  (is-true (stringp (update::get-github-releases-url)))
  (is-true (stringp (update::get-latest-release-url)))
  (is-true (search "api.github.com" (update::get-github-releases-url)))
  (is-true (search "jbouwman/epsilon" (update::get-github-releases-url))))

(deftest test-version-extraction
  "Test version extraction from release data"
  (let ((release-data '(("tag_name" . "v1.2.3")
                        ("name" . "Release 1.2.3"))))
    (is-equal "1.2.3" (update::extract-version-from-release release-data)))
  (let ((release-data '(("tag_name" . "1.2.3")
                        ("name" . "Release 1.2.3"))))
    (is-equal "1.2.3" (update::extract-version-from-release release-data))))

(deftest test-current-version
  "Test current version retrieval"
  (is-true (stringp (update:get-current-version)))
  (is-true (> (length (update:get-current-version)) 0)))