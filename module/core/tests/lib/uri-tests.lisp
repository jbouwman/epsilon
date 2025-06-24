(defpackage #:epsilon.lib.uri.tests
  (:use
   #:cl
   #:epsilon.tool.test)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri)))

(in-package #:epsilon.lib.uri.tests)

(deftest file-uri
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///home") "two"))
                    "file:///home/two"))
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///home/") "two"))
                    "file:///home/two")))

(deftest path-utilities
  "Test URI path utility functions for consistent handling"
  ;; Test ensure-directory-path
  (is (string= (uri:ensure-directory-path "src/lib") "src/lib/"))
  (is (string= (uri:ensure-directory-path "src/lib/") "src/lib/"))
  (is (string= (uri:ensure-directory-path "") ""))
  
  ;; Test ensure-file-path  
  (is (string= (uri:ensure-file-path "src/lib/uri.lisp") "src/lib/uri.lisp"))
  (is (string= (uri:ensure-file-path "src/lib/") "src/lib"))
  (is (string= (uri:ensure-file-path "") ""))
  
  ;; Test path-join
  (is (string= (uri:path-join "src" "lib" "uri.lisp") "src/lib/uri.lisp"))
  (is (string= (uri:path-join "src/" "lib/" "uri.lisp") "src/lib/uri.lisp"))
  (is (string= (uri:path-join "src" "/lib" "uri.lisp") "src/lib/uri.lisp"))
  (is (string= (uri:path-join) nil))
  (is (string= (uri:path-join "single") "single")))

(deftest uri-merge-edge-cases
  "Test URI merge function with various path combinations"
  ;; No double slashes should be created
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///base/") "path"))
                    "file:///base/path"))
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///base") "/path"))
                    "file:///path"))  ; absolute path takes precedence
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///base") ""))
                    "file:///base"))  ; empty relative path
  ;; Build system style paths
  (is (string-equal (uri:to-string (uri:merge (uri:uri "file:///project/") "target/lisp/file.fasl"))
                    "file:///project/target/lisp/file.fasl")))
