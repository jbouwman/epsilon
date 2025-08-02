;;;; Tests for the protocol system and package sources

(in-package :cl-user)

(defpackage :epsilon.test.protocol
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:proto :epsilon.protocol)
   (:build-proto :epsilon.build.protocol)
   (:path :epsilon.path)
   (:map :epsilon.map)))

(in-package :epsilon.test.protocol)

(deftest test-protocol-definition
  "Test basic protocol definition"
  ;; Define a test protocol
  (proto:define-protocol test-protocol
    (:version "1.0")
    (:documentation "Test protocol")
    (:method test-method (x) "A test method"))
  
  ;; Check protocol was registered
  (is (proto:protocol-exists-p 'test-protocol))
  
  ;; Check protocol properties
  (let ((protocol (proto:find-protocol 'test-protocol)))
    (is-equal "1.0" (proto:protocol-version protocol))
    (is-equal "Test protocol" (proto:protocol-documentation protocol))
    (is (member 'test-method (proto:protocol-methods protocol))))
  
  ;; Check generic function was created
  (is (fboundp 'test-method)))

(deftest test-filesystem-source
  "Test filesystem package source"
  ;; Create a test package directory
  (let* ((test-dir (path:make-temp-path :prefix "test-package" :suffix ""))
         (package-file (path:path-join test-dir "package.lisp")))
    
    ;; Create directory
    (ensure-directories-exist (path:path-string package-file))
    
    ;; Write package.lisp
    (with-open-file (stream (path:path-string package-file)
                           :direction :output
                           :if-exists :supersede)
      (write '(:name "test-package"
               :version "1.0.0"
               :description "Test package"
               :dependencies ("epsilon.core"))
             :stream stream))
    
    ;; Create filesystem source
    (let ((source (build-proto:make-filesystem-source test-dir)))
      
      ;; Test package discovery
      (is-equal '("test-package") (build-proto:list-packages source))
      
      ;; Test package lookup
      (let ((pkg (build-proto:find-package source "test-package")))
        (is pkg)
        (is-equal "test-package" (build-proto:name pkg))
        (is-equal "1.0.0" (build-proto:version pkg)))
      
      ;; Test metadata loading
      (let ((metadata (build-proto:load-package-metadata source "test-package")))
        (is-equal "test-package" (getf metadata :name))
        (is-equal "Test package" (getf metadata :description)))
      
      ;; Test location resolution
      (let ((location (build-proto:resolve-package-location source "test-package")))
        (is (path:path-equal test-dir location))))
    
    ;; Cleanup
    (delete-file (path:path-string package-file))
    (sb-ext:delete-directory (path:path-string test-dir))))

(deftest test-directory-source
  "Test directory package source"
  ;; Create a test directory with multiple packages
  (let* ((test-dir (path:make-temp-path :prefix "test-packages" :suffix ""))
         (pkg1-dir (path:path-join test-dir "package1"))
         (pkg2-dir (path:path-join test-dir "package2")))
    
    ;; Create directories  
    (ensure-directories-exist (path:path-string (path:path-join pkg1-dir "package.lisp")))
    (ensure-directories-exist (path:path-string (path:path-join pkg2-dir "package.lisp")))
    
    ;; Write package files
    (with-open-file (stream (path:path-string (path:path-join pkg1-dir "package.lisp"))
                           :direction :output
                           :if-exists :supersede)
      (write '(:name "package-one" :version "1.0.0") :stream stream))
    
    (with-open-file (stream (path:path-string (path:path-join pkg2-dir "package.lisp"))
                           :direction :output
                           :if-exists :supersede)
      (write '(:name "package-two" :version "2.0.0") :stream stream))
    
    ;; Create directory source
    (let ((source (build-proto:make-directory-source test-dir)))
      
      ;; Test package discovery
      (let ((packages (build-proto:list-packages source)))
        (is (= 2 (length packages)))
        (is (member "package-one" packages :test #'string=))
        (is (member "package-two" packages :test #'string=)))
      
      ;; Test individual package lookup
      (let ((pkg1 (build-proto:find-package source "package-one"))
            (pkg2 (build-proto:find-package source "package-two")))
        (is pkg1)
        (is pkg2)
        (is-equal "1.0.0" (build-proto:version pkg1))
        (is-equal "2.0.0" (build-proto:version pkg2))))
    
    ;; Cleanup
    (delete-file (path:path-string (path:path-join pkg1-dir "package.lisp")))
    (delete-file (path:path-string (path:path-join pkg2-dir "package.lisp")))
    (sb-ext:delete-directory (path:path-string pkg1-dir))
    (sb-ext:delete-directory (path:path-string pkg2-dir))
    (sb-ext:delete-directory (path:path-string test-dir))))

(deftest test-composite-source
  "Test composite package source"
  ;; Create two test directories
  (let* ((base-dir (path:make-temp-path :prefix "test-composite" :suffix ""))
         (dir1 (path:path-join base-dir "source1"))
         (dir2 (path:path-join base-dir "source2"))
         (pkg1-file (path:path-join dir1 "package.lisp"))
         (pkg2-file (path:path-join dir2 "package.lisp")))
    
    ;; Create directories
    (ensure-directories-exist (path:path-string pkg1-file))
    (ensure-directories-exist (path:path-string pkg2-file))
    
    ;; Write package files
    (with-open-file (stream (path:path-string pkg1-file)
                           :direction :output
                           :if-exists :supersede)
      (write '(:name "source1-package" :version "1.0") :stream stream))
    
    (with-open-file (stream (path:path-string pkg2-file)
                           :direction :output
                           :if-exists :supersede)
      (write '(:name "source2-package" :version "2.0") :stream stream))
    
    ;; Create individual sources
    (let* ((source1 (build-proto:make-filesystem-source dir1))
           (source2 (build-proto:make-filesystem-source dir2))
           (composite (build-proto:make-composite-source (list source1 source2))))
      
      ;; Test combined package listing
      (let ((packages (build-proto:list-packages composite)))
        (is (= 2 (length packages)))
        (is (member "source1-package" packages :test #'string=))
        (is (member "source2-package" packages :test #'string=)))
      
      ;; Test package lookup across sources
      (is (build-proto:find-package composite "source1-package"))
      (is (build-proto:find-package composite "source2-package")))
    
    ;; Cleanup
    (delete-file (path:path-string pkg1-file))
    (delete-file (path:path-string pkg2-file))
    (sb-ext:delete-directory (path:path-string dir1))
    (sb-ext:delete-directory (path:path-string dir2))
    (sb-ext:delete-directory (path:path-string base-dir))))