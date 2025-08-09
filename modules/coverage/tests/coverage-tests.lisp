;;;; Comprehensive tests for epsilon.tool.coverage

(defpackage epsilon.tool.coverage.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (coverage epsilon.tool.coverage)
   (map epsilon.map)
   (fs epsilon.sys.fs)))

(in-package epsilon.tool.coverage.tests)

;;; Configuration Tests

(deftest default-configuration-test
  "Test default configuration values"
  (is (listp coverage:*coverage-exclude-patterns*))
  (is (stringp coverage:*coverage-output-directory*))
  (is (map:map-p coverage:*coverage-thresholds*))
  
  ;; Check threshold values are reasonable
  (let ((line-threshold (map:get coverage:*coverage-thresholds* :line-coverage))
        (form-threshold (map:get coverage:*coverage-thresholds* :form-coverage)))
    (is (and (numberp line-threshold) (>= line-threshold 0) (<= line-threshold 100)))
    (is (and (numberp form-threshold) (>= form-threshold 0) (<= form-threshold 100)))))

(deftest configuration-modification-test
  "Test that configuration can be modified"
  (let ((original-patterns coverage:*coverage-exclude-patterns*)
        (original-dir coverage:*coverage-output-directory*)
        (original-thresholds coverage:*coverage-thresholds*))
    
    (unwind-protect
        (progn
          ;; Modify configuration
          (setf coverage:*coverage-exclude-patterns* '("TEST-*"))
          (setf coverage:*coverage-output-directory* "test-coverage/")
          (setf coverage:*coverage-thresholds* 
                (map:make-map :line-coverage 90 :form-coverage 95))
          
          ;; Verify changes
          (is-equal coverage:*coverage-exclude-patterns* '("TEST-*"))
          (is-equal coverage:*coverage-output-directory* "test-coverage/")
          (is-= (map:get coverage:*coverage-thresholds* :line-coverage) 90))
      
      ;; Restore original values
      (setf coverage:*coverage-exclude-patterns* original-patterns
            coverage:*coverage-output-directory* original-dir
            coverage:*coverage-thresholds* original-thresholds))))

;;; Data Structure Tests

(deftest coverage-summary-structure-test
  "Test coverage summary data structure"
  (let ((summary (coverage:make-coverage-summary
                  :total-files 10
                  :covered-files 8
                  :total-lines 1000
                  :covered-lines 800
                  :total-forms 500
                  :covered-forms 400
                  :line-coverage-percent 80.0
                  :form-coverage-percent 80.0)))
    
    (is (coverage:coverage-summary-p summary))
    (is-= (coverage:coverage-summary-total-files summary) 10)
    (is-= (coverage:coverage-summary-covered-files summary) 8)
    (is-= (coverage:coverage-summary-total-lines summary) 1000)
    (is-= (coverage:coverage-summary-covered-lines summary) 800)
    (is-= (coverage:coverage-summary-line-coverage-percent summary) 80.0)))

(deftest coverage-file-structure-test
  "Test coverage file data structure"
  (let ((file-data (coverage:make-coverage-file
                    :pathname #P"test.lisp"
                    :total-lines 100
                    :covered-lines 80
                    :total-forms 50
                    :covered-forms 40
                    :line-coverage-percent 80.0
                    :form-coverage-percent 80.0)))
    
    (is (coverage:coverage-file-p file-data))
    (is (equal (coverage:coverage-file-pathname file-data) #P"test.lisp"))
    (is-= (coverage:coverage-file-total-lines file-data) 100)
    (is-= (coverage:coverage-file-covered-lines file-data) 80)
    (is-= (coverage:coverage-file-line-coverage-percent file-data) 80.0)))

(deftest coverage-line-structure-test
  "Test coverage line data structure"
  (let ((line-data (coverage:make-coverage-line
                    :number 42
                    :content "(defun test ())"
                    :hit-count 5
                    :covered-p t)))
    
    (is (coverage:coverage-line-p line-data))
    (is-= (coverage:coverage-line-number line-data) 42)
    (is-equal (coverage:coverage-line-content line-data) "(defun test ())")
    (is-= (coverage:coverage-line-hit-count line-data) 5)
    (is (coverage:coverage-line-covered-p line-data))))

(deftest coverage-form-structure-test
  "Test coverage form data structure"
  (let ((form-data (coverage:make-coverage-form
                    :start-line 10
                    :end-line 15
                    :start-column 2
                    :end-column 20
                    :form-text "(defun example ())"
                    :hit-count 3
                    :covered-p t)))
    
    (is (coverage:coverage-form-p form-data))
    (is-= (coverage:coverage-form-start-line form-data) 10)
    (is-= (coverage:coverage-form-end-line form-data) 15)
    (is-= (coverage:coverage-form-hit-count form-data) 3)
    (is (coverage:coverage-form-covered-p form-data))))

;;; Core Functionality Tests

(deftest coverage-control-basic-test
  "Test basic coverage control functions"
  ;; These tests verify the API exists and doesn't crash
  ;; Full functionality requires SB-COVER which may not be available in test environment
  
  ;; Test start-coverage doesn't crash
  (is-not (coverage:start-coverage :packages '("test-package")))
  
  ;; Test reset-coverage doesn't crash  
  (is-not (coverage:reset-coverage))
  
  ;; Test stop-coverage doesn't crash
  (is-not (coverage:stop-coverage)))

(deftest coverage-data-persistence-test
  "Test coverage data save/restore functionality"
  (let ((test-file (merge-pathnames (format nil "coverage-test-~A.dat" (random 100000)) 
                                      #P"/tmp/")))
    (unwind-protect
        (progn
          ;; Save some test data
          (setf (gethash "test-location" coverage:*coverage-data*) 42)
          (coverage:save-coverage-data test-file)
          
          ;; Verify file was created
          (is (probe-file test-file))
          
          ;; Clear data and restore
          (clrhash coverage:*coverage-data*)
          (coverage:restore-coverage-data test-file)
          
          ;; Verify data was restored
          (is-= (gethash "test-location" coverage:*coverage-data*) 42))
      
      ;; Clean up
      (when (probe-file test-file)
        (delete-file test-file)))))

;;; Utility Function Tests

(deftest simple-glob-match-test
  "Test simple glob pattern matching"
  ;; Test exact matches
  (is (coverage::simple-glob-match-p "exact" "exact"))
  (is-not (coverage::simple-glob-match-p "exact" "different"))
  
  ;; Test single wildcard
  (is (coverage::simple-glob-match-p "test*" "test123"))
  (is (coverage::simple-glob-match-p "*test" "mytest"))
  (is (coverage::simple-glob-match-p "test*ing" "testing"))
  (is-not (coverage::simple-glob-match-p "test*ing" "testing123"))
  
  ;; Test prefix/suffix patterns
  (is (coverage::simple-glob-match-p "SB-*" "SB-KERNEL"))
  (is (coverage::simple-glob-match-p "*-TESTS" "MY-PACKAGE-TESTS"))
  (is-not (coverage::simple-glob-match-p "SB-*" "CL-USER")))

(deftest hash-table-values-test
  "Test hash table values extraction utility"
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "key1" ht) "value1")
    (setf (gethash "key2" ht) "value2")
    (setf (gethash "key3" ht) "value3")
    
    (let ((values (coverage::hash-table-values ht)))
      (is (= (length values) 3))
      (is (member "value1" values :test #'equal))
      (is (member "value2" values :test #'equal))
      (is (member "value3" values :test #'equal)))))

(deftest should-include-file-test
  "Test file inclusion filtering"
  (let ((coverage:*coverage-packages* nil)
        (coverage:*coverage-exclude-patterns* '("SB-*" "SWANK*" "*-TESTS")))
    
    ;; Test with no package restrictions
    (is (coverage::should-include-file-p #P"src/my-package/file.lisp"))
    
    ;; Test exclusion patterns
    (is-not (coverage::should-include-file-p #P"src/sb-kernel/file.lisp"))
    (is-not (coverage::should-include-file-p #P"src/my-package-tests/file.lisp"))
    
    ;; Test with package restrictions
    (setf coverage:*coverage-packages* '("my-package"))
    (is (coverage::should-include-file-p #P"src/my-package/file.lisp"))
    (is-not (coverage::should-include-file-p #P"src/other-package/file.lisp"))))

;;; Report Generation Tests

(deftest html-escape-test
  "Test HTML character escaping"
  (is-equal (coverage::escape-html "normal text") "normal text")
  (is-equal (coverage::escape-html "Tom & Jerry") "Tom &amp; Jerry")
  (is-equal (coverage::escape-html "<script>") "&lt;script&gt;")
  (is-equal (coverage::escape-html "\"quoted\"") "&quot;quoted&quot;")
  (is-equal (coverage::escape-html "multiple < & > \"chars\"") 
            "multiple &lt; &amp; &gt; &quot;chars&quot;"))

(deftest substitute-string-test
  "Test string substitution utility"
  (is-equal (coverage::substitute-string "hello world" "world" "universe")
            "hello universe")
  (is-equal (coverage::substitute-string "test & test" "&" "&amp;")
            "test &amp; test")
  (is-equal (coverage::substitute-string "no matches" "xyz" "abc")
            "no matches"))

(deftest generate-html-report-test
  "Test HTML report generation"
  (let ((test-dir (fs:temp-dir "coverage-test")))
    (unwind-protect
        (progn
          ;; Create some test coverage data
          (let ((file-data (coverage:make-coverage-file
                           :pathname #P"test.lisp"
                           :total-lines 10
                           :covered-lines 8
                           :total-forms 5
                           :covered-forms 4
                           :line-coverage-percent 80.0
                           :form-coverage-percent 80.0)))
            (setf (gethash "test.lisp" coverage:*source-map*) file-data))
          
          ;; Generate report
          (let ((report-file (coverage:generate-html-report :output-dir test-dir)))
            (is (probe-file report-file))
            (is (search "coverage.html" (namestring report-file)))
            
            ;; Check that file contains expected content
            (let ((content (fs:read-file report-file)))
              (is (search "Coverage Report" content))
              (is (search "test.lisp" content)))))
      
      ;; Clean up
      (when (probe-file test-dir)
        (fs:delete-directory test-dir :recursive t))
      (clrhash coverage:*source-map*))))

(deftest generate-lcov-report-test
  "Test LCOV report generation"
  (let ((test-dir (fs:temp-dir "coverage-lcov-test")))
    (unwind-protect
        (progn
          ;; Create test data
          (let ((file-data (coverage:make-coverage-file
                           :pathname #P"test.lisp"
                           :total-lines 10
                           :covered-lines 8
                           :total-forms 5
                           :covered-forms 4
                           :lines (list (coverage:make-coverage-line
                                        :number 1
                                        :hit-count 3
                                        :covered-p t))
                           :forms (list (coverage:make-coverage-form
                                        :start-line 1
                                        :start-column 0
                                        :hit-count 2
                                        :covered-p t)))))
            (setf (gethash "test.lisp" coverage:*source-map*) file-data))
          
          ;; Generate report
          (let ((report-file (coverage:generate-lcov-report :output-dir test-dir)))
            (is (probe-file report-file))
            (is (search "coverage.lcov" (namestring report-file)))
            
            ;; Check LCOV format
            (let ((content (fs:read-file report-file)))
              (is (search "SF:" content))  ; Source file marker
              (is (search "DA:" content))  ; Line data marker
              (is (search "end_of_record" content)))))
      
      ;; Clean up
      (when (probe-file test-dir)
        (fs:delete-directory test-dir :recursive t))
      (clrhash coverage:*source-map*))))

(deftest generate-junit-report-test
  "Test JUnit XML report generation"
  (let ((test-dir (fs:temp-dir "coverage-junit-test")))
    (unwind-protect
        (progn
          ;; Create test data with low coverage to trigger failure
          (let ((file-data (coverage:make-coverage-file
                           :pathname #P"test.lisp"
                           :total-lines 10
                           :covered-lines 5
                           :total-forms 10
                           :covered-forms 5
                           :line-coverage-percent 50.0
                           :form-coverage-percent 50.0)))
            (setf (gethash "test.lisp" coverage:*source-map*) file-data))
          
          ;; Generate report
          (let ((report-file (coverage:generate-junit-report :output-dir test-dir)))
            (is (probe-file report-file))
            (is (search "junit.xml" (namestring report-file)))
            
            ;; Check XML format
            (let ((content (fs:read-file report-file)))
              (is (search "<?xml" content))
              (is (search "<testsuite" content))
              (is (search "</testsuite>" content)))))
      
      ;; Clean up
      (when (probe-file test-dir)
        (fs:delete-directory test-dir :recursive t))
      (clrhash coverage:*source-map*))))

(deftest generate-json-report-test
  "Test JSON report generation"
  (let ((test-dir (fs:temp-dir "coverage-json-test")))
    (unwind-protect
        (progn
          ;; Create test data
          (let ((file-data (coverage:make-coverage-file
                           :pathname #P"test.lisp"
                           :total-lines 10
                           :covered-lines 8
                           :total-forms 5
                           :covered-forms 4
                           :line-coverage-percent 80.0
                           :form-coverage-percent 80.0)))
            (setf (gethash "test.lisp" coverage:*source-map*) file-data))
          
          ;; Generate report
          (let ((report-file (coverage:generate-json-report :output-dir test-dir)))
            (is (probe-file report-file))
            (is (search "coverage.json" (namestring report-file)))
            
            ;; Check JSON format
            (let ((content (fs:read-file report-file)))
              (is (search "{" content))
              (is (search "\"summary\":" content))
              (is (search "\"files\":" content))
              (is (search "test.lisp" content)))))
      
      ;; Clean up
      (when (probe-file test-dir)
        (fs:delete-directory test-dir :recursive t))
      (clrhash coverage:*source-map*))))

;;; Analysis Function Tests

(deftest coverage-summary-computation-test
  "Test coverage summary computation"
  (unwind-protect
      (progn
        ;; Create test data
        (let ((file1 (coverage:make-coverage-file
                      :pathname #P"file1.lisp"
                      :total-lines 10
                      :covered-lines 8
                      :total-forms 5
                      :covered-forms 4
                      :line-coverage-percent 80.0
                      :form-coverage-percent 80.0))
              (file2 (coverage:make-coverage-file
                      :pathname #P"file2.lisp"
                      :total-lines 20
                      :covered-lines 15
                      :total-forms 10
                      :covered-forms 8
                      :line-coverage-percent 75.0
                      :form-coverage-percent 80.0)))
          (setf (gethash "file1.lisp" coverage:*source-map*) file1)
          (setf (gethash "file2.lisp" coverage:*source-map*) file2))
        
        ;; Test summary computation
        (let ((summary (coverage:coverage-summary)))
          (is (coverage:coverage-summary-p summary))
          (is-= (coverage:coverage-summary-total-files summary) 2)
          (is-= (coverage:coverage-summary-total-lines summary) 30)
          (is-= (coverage:coverage-summary-covered-lines summary) 23)
          (is-= (coverage:coverage-summary-total-forms summary) 15)
          (is-= (coverage:coverage-summary-covered-forms summary) 12)
          
          ;; Check computed percentages
          (is (< (abs (- (coverage:coverage-summary-line-coverage-percent summary) 76.67)) 0.1))
          (is (= (coverage:coverage-summary-form-coverage-percent summary) 80.0))))
    
    ;; Clean up
    (clrhash coverage:*source-map*)))

(deftest coverage-for-file-test
  "Test file-specific coverage retrieval"
  (unwind-protect
      (progn
        ;; Create test data
        (let ((file-data (coverage:make-coverage-file :pathname #P"test.lisp")))
          (setf (gethash "test.lisp" coverage:*source-map*) file-data))
        
        ;; Test retrieval
        (let ((result (coverage:coverage-for-file #P"test.lisp")))
          (is (coverage:coverage-file-p result))
          (is (equal (coverage:coverage-file-pathname result) #P"test.lisp")))
        
        ;; Test non-existent file
        (is (null (coverage:coverage-for-file #P"nonexistent.lisp"))))
    
    ;; Clean up
    (clrhash coverage:*source-map*)))

(deftest coverage-for-package-test
  "Test package-specific coverage retrieval"
  (unwind-protect
      (progn
        ;; Create test data
        (let ((file1 (coverage:make-coverage-file :pathname #P"my-package/file1.lisp"))
              (file2 (coverage:make-coverage-file :pathname #P"my-package/file2.lisp"))
              (file3 (coverage:make-coverage-file :pathname #P"other-package/file3.lisp")))
          (setf (gethash "my-package/file1.lisp" coverage:*source-map*) file1)
          (setf (gethash "my-package/file2.lisp" coverage:*source-map*) file2)
          (setf (gethash "other-package/file3.lisp" coverage:*source-map*) file3))
        
        ;; Test package filtering
        (let ((my-package-files (coverage:coverage-for-package "my-package")))
          (is (= (length my-package-files) 2))
          (is (every #'coverage:coverage-file-p my-package-files)))
        
        (let ((other-package-files (coverage:coverage-for-package "other-package")))
          (is (= (length other-package-files) 1)))
        
        (let ((nonexistent (coverage:coverage-for-package "nonexistent")))
          (is (null nonexistent))))
    
    ;; Clean up
    (clrhash coverage:*source-map*)))

(deftest get-uncovered-forms-test
  "Test uncovered forms retrieval"
  (unwind-protect
      (progn
        ;; Create test data with various hit counts
        (let ((form1 (coverage:make-coverage-form :hit-count 0 :covered-p nil))
              (form2 (coverage:make-coverage-form :hit-count 1 :covered-p t))
              (form3 (coverage:make-coverage-form :hit-count 5 :covered-p t))
              (file-data (coverage:make-coverage-file :pathname #P"test.lisp")))
          
          (setf (coverage:coverage-file-forms file-data) (list form1 form2 form3))
          (setf (gethash "test.lisp" coverage:*source-map*) file-data))
        
        ;; Test with threshold 0 (completely uncovered)
        (let ((uncovered-0 (coverage:get-uncovered-forms 0)))
          (is (= (length uncovered-0) 1)))
        
        ;; Test with threshold 2 (hit <= 2 times)
        (let ((uncovered-2 (coverage:get-uncovered-forms 2)))
          (is (= (length uncovered-2) 2))))
    
    ;; Clean up
    (clrhash coverage:*source-map*)))

;;; Integration Tests

(deftest with-coverage-macro-test
  "Test with-coverage macro"
  (let ((test-dir (fs:temp-dir "with-coverage-test")))
    (unwind-protect
        (progn
          ;; Test the macro doesn't crash
          (coverage:with-coverage (:packages '("test-package")
                                   :exclude-patterns '("*-TEST*")
                                   :output-dir test-dir)
            ;; Some dummy code to "cover"
            (+ 1 2 3)
            (list :a :b :c))
          
          ;; If we get here, the macro worked
          (is t))
      
      ;; Clean up
      (when (probe-file test-dir)
        (fs:delete-directory test-dir :recursive t)))))

;;; Error Handling Tests

(deftest coverage-error-handling-test
  "Test error handling in coverage functions"
  ;; Test that functions handle errors gracefully
  
  ;; Invalid file paths
  (is-not (coverage:save-coverage-data "/invalid/path/file.dat"))
  
  ;; Invalid output directories should be handled
  (let ((invalid-dir "/invalid/path/that/does/not/exist/"))
    ;; These should not crash, though they may warn
    (coverage:generate-html-report :output-dir invalid-dir)
    (coverage:generate-lcov-report :output-dir invalid-dir)
    (coverage:generate-junit-report :output-dir invalid-dir)
    (coverage:generate-json-report :output-dir invalid-dir)))

;;; Performance Tests

(deftest coverage-data-structures-performance-test
  "Test that coverage data structures perform reasonably"
  ;; Create many coverage items and verify no obvious performance issues
  (let ((start-time (get-internal-real-time)))
    
    ;; Create a large number of coverage entries
    (dotimes (i 1000)
      (let ((file-data (coverage:make-coverage-file
                        :pathname (make-pathname :name (format nil "file~D" i)
                                               :type "lisp"))))
        (setf (gethash (format nil "file~D.lisp" i) coverage:*source-map*) file-data)))
    
    ;; Generate summary (this should be reasonably fast)
    (let ((summary (coverage:coverage-summary)))
      (is (coverage:coverage-summary-p summary))
      (is-= (coverage:coverage-summary-total-files summary) 1000))
    
    ;; Check that this didn't take too long (more than 1 second would be concerning)
    (let ((elapsed (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second)))
      (is (< elapsed 1.0)))
    
    ;; Clean up
    (clrhash coverage:*source-map*)))