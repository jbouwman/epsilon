;;;; Code Coverage Tool for Epsilon
;;;;
;;;; This module provides comprehensive code coverage analysis for Epsilon projects,
;;;; building on SBCL's sb-cover facility with enhanced integration for epsilon's
;;;; test framework and module system. Supports both form-level and line-level
;;;; coverage tracking with multiple report formats.
;;;;
;;;; Key Features:
;;;; - Form-by-form and line-by-line coverage tracking
;;;; - Integration with epsilon.tool.test framework
;;;; - Multiple output formats (HTML, LCOV, JUnit XML, JSON)
;;;; - Module-aware filtering and reporting
;;;; - CI/CD integration support
;;;; - Configurable coverage thresholds
;;;;
;;;; Dependencies: sb-cover, epsilon.tool.test, epsilon.sys.fs, epsilon.lib.map

(defpackage epsilon.tool.coverage
  (:use cl)
  (:local-nicknames
   (test epsilon.tool.test)
   (fs epsilon.sys.fs)
   (map epsilon.lib.map)
   (str epsilon.lib.string)
   (seq epsilon.lib.sequence)
   (path epsilon.lib.path))
  (:export
   ;; Coverage control
   start-coverage
   stop-coverage
   reset-coverage
   save-coverage-data
   restore-coverage-data
   
   ;; Configuration
   *coverage-packages*
   *coverage-exclude-patterns*
   *coverage-output-directory*
   *coverage-thresholds*
   
   ;; Reporting
   generate-coverage-report
   generate-html-report
   generate-lcov-report
   generate-junit-report
   generate-json-report
   
   ;; Analysis
   coverage-summary
   coverage-for-file
   coverage-for-package
   get-uncovered-forms
   
   ;; Integration
   with-coverage
   run-tests-with-coverage))

(in-package :epsilon.tool.coverage)

;;;; Configuration

(defvar *coverage-packages* nil
  "List of package names to include in coverage analysis. 
   If NIL, covers all packages except those in exclude patterns.")

(defvar *coverage-exclude-patterns* '("SB-*" "SWANK*" "SLIME*" "*-TESTS")
  "List of glob patterns for packages to exclude from coverage.")

(defvar *coverage-output-directory* "target/coverage/"
  "Directory for coverage report output.")

(defvar *coverage-thresholds* 
  (map:make-map 
   :line-coverage 80
   :form-coverage 85
   :package-coverage 90)
  "Coverage thresholds for pass/fail reporting.")

(defvar *coverage-enabled* nil
  "Whether coverage tracking is currently enabled.")

(defvar *coverage-data* (make-hash-table :test 'equal)
  "Hash table storing coverage data: source-location -> hit-count")

(defvar *source-map* (make-hash-table :test 'equal)
  "Hash table mapping source locations to file info: location -> file-data")

;;;; Coverage Data Structures

(defstruct coverage-file
  "Coverage data for a single source file"
  pathname
  lines
  forms
  total-lines
  covered-lines
  total-forms
  covered-forms
  line-coverage-percent
  form-coverage-percent)

(defstruct coverage-line
  "Coverage data for a single line"
  number
  content
  hit-count
  covered-p)

(defstruct coverage-form
  "Coverage data for a single form"
  start-line
  end-line
  start-column
  end-column
  form-text
  hit-count
  covered-p)

(defstruct coverage-summary
  "Overall coverage summary"
  total-files
  covered-files
  total-lines
  covered-lines
  total-forms
  covered-forms
  line-coverage-percent
  form-coverage-percent
  package-summaries)

;;;; Core Coverage Control

(defun start-coverage (&key packages exclude-patterns)
  "Start coverage tracking with optional package filtering.
   PACKAGES - list of package names to track (nil for all)
   EXCLUDE-PATTERNS - list of glob patterns to exclude"
  (when *coverage-enabled*
    (warn "Coverage already enabled"))
  
  (when packages
    (setf *coverage-packages* packages))
  (when exclude-patterns
    (setf *coverage-exclude-patterns* exclude-patterns))
  
  ;; Enable SBCL's coverage instrumentation
  (handler-case
      (progn
        (require :sb-cover)
        (let ((sb-cover-pkg (find-package :sb-cover)))
          (unless sb-cover-pkg
            (error "SB-COVER package not available"))
          (eval `(declaim (optimize (,(find-symbol "STORE-COVERAGE-DATA" sb-cover-pkg) 1))))
          (funcall (find-symbol "RESET-COVERAGE" sb-cover-pkg)))
        (setf *coverage-enabled* t)
        (format t ";;; Coverage tracking enabled~%"))
    (error (e)
      (error "Failed to enable coverage: ~A" e))))

(defun stop-coverage ()
  "Stop coverage tracking and collect data"
  (unless *coverage-enabled*
    (warn "Coverage not enabled"))
  
  (handler-case
      (progn
        ;; Disable coverage optimization
        (let ((sb-cover-pkg (find-package :sb-cover)))
          (when sb-cover-pkg
            (eval `(declaim (optimize (,(find-symbol "STORE-COVERAGE-DATA" sb-cover-pkg) 0))))))
        (collect-coverage-data)
        (setf *coverage-enabled* nil)
        (format t ";;; Coverage tracking stopped~%"))
    (error (e)
      (error "Failed to stop coverage: ~A" e))))

(defun reset-coverage ()
  "Reset all coverage data"
  (let ((sb-cover-pkg (find-package :sb-cover)))
    (when sb-cover-pkg
      (funcall (find-symbol "RESET-COVERAGE" sb-cover-pkg))))
  (clrhash *coverage-data*)
  (clrhash *source-map*)
  (format t ";;; Coverage data reset~%"))

(defun save-coverage-data (file)
  "Save current coverage data to file"
  (with-open-file (stream file :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
    (write (list *coverage-data* *source-map*) :stream stream))
  (format t ";;; Coverage data saved to ~A~%" file))

(defun restore-coverage-data (file)
  "Restore coverage data from file"
  (with-open-file (stream file :direction :input)
    (let ((data (read stream)))
      (setf *coverage-data* (first data)
            *source-map* (second data))))
  (format t ";;; Coverage data restored from ~A~%" file))

;;;; Data Collection

(defun collect-coverage-data ()
  "Collect coverage data from SBCL's instrumentation"
  (clrhash *coverage-data*)
  (clrhash *source-map*)
  
  ;; Process SBCL's coverage data
  (let ((sb-cover-pkg (find-package :sb-cover)))
    (when sb-cover-pkg
      (let ((coverage-info-fn (find-symbol "COVERAGE-INFO" sb-cover-pkg)))
        (when coverage-info-fn
          (dolist (info (funcall coverage-info-fn))
            (destructuring-bind (pathname coverage-marks) info
              (when (should-include-file-p pathname)
                (process-file-coverage pathname coverage-marks)))))))))

(defun should-include-file-p (pathname)
  "Check if file should be included in coverage analysis"
  (let ((namestring (namestring pathname)))
    (and 
     ;; Include if no specific packages or file belongs to tracked package
     (or (null *coverage-packages*)
         (some (lambda (pkg) (search pkg namestring)) *coverage-packages*))
     ;; Exclude if matches exclude patterns  
     (not (some (lambda (pattern)
                  (simple-glob-match-p pattern namestring))
                *coverage-exclude-patterns*)))))

(defun process-file-coverage (pathname coverage-marks)
  "Process coverage data for a single file"
  (let ((file-data (make-coverage-file :pathname pathname))
        (line-map (make-hash-table))
        (form-map (make-hash-table)))
    
    ;; Process coverage marks from SBCL
    (loop for (start-pos end-pos hit-count) in coverage-marks
          do (let* ((location (format nil "~A:~A-~A" pathname start-pos end-pos))
                    (existing-count (gethash location *coverage-data* 0)))
               (setf (gethash location *coverage-data*) 
                     (+ existing-count hit-count))
               
               ;; Map to line/form data
               (multiple-value-bind (start-line start-col end-line end-col)
                   (position-to-line-column pathname start-pos end-pos)
                 (when start-line
                   (record-line-coverage line-map start-line end-line hit-count)
                   (record-form-coverage form-map start-line end-line 
                                       start-col end-col hit-count pathname
                                       start-pos end-pos)))))
    
    ;; Store processed data
    (setf (coverage-file-lines file-data) (hash-table-values line-map)
          (coverage-file-forms file-data) (hash-table-values form-map))
    
    (calculate-file-statistics file-data)
    (setf (gethash (namestring pathname) *source-map*) file-data)))

(defun position-to-line-column (pathname start-pos end-pos)
  "Convert character positions to line/column numbers"
  (with-open-file (stream pathname :direction :input)
    (let ((line 1)
          (column 0)
          (pos 0)
          (start-line nil)
          (start-col nil)
          (end-line nil)
          (end-col nil))
      
      (loop for char = (read-char stream nil nil)
            while char
            do (progn
                 (when (= pos start-pos)
                   (setf start-line line start-col column))
                 (when (= pos end-pos)
                   (setf end-line line end-col column)
                   (return))
                 
                 (if (char= char #\Newline)
                     (progn
                       (setf line (1+ line) column 0))
                     (incf column))
                 (incf pos)))
      
      (values start-line start-col 
              (or end-line start-line) 
              (or end-col start-col)))))

(defun record-line-coverage (line-map start-line end-line hit-count)
  "Record coverage for lines in the given range"
  (loop for line from start-line to end-line
        do (let ((line-data (gethash line line-map)))
             (if line-data
                 (incf (coverage-line-hit-count line-data) hit-count)
                 (setf (gethash line line-map)
                       (make-coverage-line 
                        :number line
                        :hit-count hit-count
                        :covered-p (> hit-count 0)))))))

(defun record-form-coverage (form-map start-line end-line start-col end-col 
                           hit-count pathname start-pos end-pos)
  "Record coverage for a form"
  (let ((form-key (format nil "~A:~A-~A" start-line start-col end-col)))
    (unless (gethash form-key form-map)
      (setf (gethash form-key form-map)
            (make-coverage-form
             :start-line start-line
             :end-line end-line
             :start-column start-col
             :end-column end-col
             :form-text (extract-form-text pathname start-pos end-pos)
             :hit-count hit-count
             :covered-p (> hit-count 0))))))

(defun extract-form-text (pathname start-pos end-pos)
  "Extract the source text for a form"
  (with-open-file (stream pathname :direction :input)
    (file-position stream start-pos)
    (let ((length (- end-pos start-pos)))
      (when (> length 0)
        (let ((buffer (make-string length)))
          (read-sequence buffer stream)
          (string-trim '(#\Space #\Tab #\Newline) buffer))))))

(defun calculate-file-statistics (file-data)
  "Calculate coverage statistics for a file"
  (let ((lines (coverage-file-lines file-data))
        (forms (coverage-file-forms file-data)))
    
    (setf (coverage-file-total-lines file-data) (length lines)
          (coverage-file-covered-lines file-data) 
          (count-if #'coverage-line-covered-p lines)
          
          (coverage-file-total-forms file-data) (length forms)
          (coverage-file-covered-forms file-data)
          (count-if #'coverage-form-covered-p forms))
    
    (setf (coverage-file-line-coverage-percent file-data)
          (if (zerop (coverage-file-total-lines file-data))
              100.0
              (* 100.0 (/ (coverage-file-covered-lines file-data)
                         (coverage-file-total-lines file-data))))
          
          (coverage-file-form-coverage-percent file-data)
          (if (zerop (coverage-file-total-forms file-data))
              100.0
              (* 100.0 (/ (coverage-file-covered-forms file-data)
                         (coverage-file-total-forms file-data)))))))

;;;; Analysis Functions

(defun coverage-summary ()
  "Generate overall coverage summary"
  (let ((files (hash-table-values *source-map*))
        (summary (make-coverage-summary)))
    
    (setf (coverage-summary-total-files summary) (length files)
          (coverage-summary-covered-files summary)
          (count-if (lambda (f) (> (coverage-file-line-coverage-percent f) 0)) files)
          
          (coverage-summary-total-lines summary)
          (reduce #'+ files :key #'coverage-file-total-lines :initial-value 0)
          (coverage-summary-covered-lines summary)
          (reduce #'+ files :key #'coverage-file-covered-lines :initial-value 0)
          
          (coverage-summary-total-forms summary)
          (reduce #'+ files :key #'coverage-file-total-forms :initial-value 0)
          (coverage-summary-covered-forms summary)
          (reduce #'+ files :key #'coverage-file-covered-forms :initial-value 0))
    
    (setf (coverage-summary-line-coverage-percent summary)
          (if (zerop (coverage-summary-total-lines summary))
              100.0
              (* 100.0 (/ (coverage-summary-covered-lines summary)
                         (coverage-summary-total-lines summary))))
          
          (coverage-summary-form-coverage-percent summary)
          (if (zerop (coverage-summary-total-forms summary))
              100.0
              (* 100.0 (/ (coverage-summary-covered-forms summary)
                         (coverage-summary-total-forms summary)))))
    
    summary))

(defun coverage-for-file (pathname)
  "Get coverage data for a specific file"
  (gethash (namestring pathname) *source-map*))

(defun coverage-for-package (package-name)
  "Get coverage data for all files in a package"
  (let ((files '()))
    (maphash (lambda (pathname file-data)
               (when (search package-name pathname)
                 (push file-data files)))
             *source-map*)
    files))

(defun get-uncovered-forms (&optional (threshold 0))
  "Get all forms with hit count <= threshold"
  (let ((uncovered '()))
    (maphash (lambda (pathname file-data)
               (dolist (form (coverage-file-forms file-data))
                 (when (<= (coverage-form-hit-count form) threshold)
                   (push (list pathname form) uncovered))))
             *source-map*)
    uncovered))

;;;; Utility Functions

(defun simple-glob-match-p (pattern string)
  "Simple glob pattern matching supporting * wildcards"
  (if (str:contains-p pattern "*")
      (let ((parts (str:split #\* pattern)))
        (cond
          ((= (length parts) 1)
           ;; No wildcards, exact match
           (string= pattern string))
          ((= (length parts) 2)
           ;; Single wildcard
           (let ((prefix (first parts))
                 (suffix (second parts)))
             (and (str:starts-with-p string prefix)
                  (str:ends-with-p string suffix)
                  (>= (length string) (+ (length prefix) (length suffix))))))
          (t
           ;; Multiple wildcards - simplified matching
           (and (str:starts-with-p string (first parts))
                (str:ends-with-p string (car (last parts)))))))
      ;; No wildcards, exact match
      (string= pattern string)))

(defun hash-table-values (hash-table)
  "Get all values from a hash table as a list"
  (let ((values '()))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value values))
             hash-table)
    values))

;;;; Report Generation

(defun generate-coverage-report (&key (format :html) (output-dir *coverage-output-directory*))
  "Generate coverage report in the specified format"
  (case format
    (:html (generate-html-report :output-dir output-dir))
    (:lcov (generate-lcov-report :output-dir output-dir))
    (:junit (generate-junit-report :output-dir output-dir))
    (:json (generate-json-report :output-dir output-dir))
    (t (error "Unsupported report format: ~A" format))))

(defun generate-html-report (&key (output-dir *coverage-output-directory*))
  "Generate HTML coverage report"
  (fs:make-dirs output-dir)
  (let ((summary (coverage-summary))
        (output-file (path:string-path-join output-dir "coverage.html")))
    
    (with-open-file (stream output-file :direction :output 
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (write-html-report stream summary))
    
    ;; Generate per-file reports
    (maphash (lambda (pathname file-data)
               (declare (ignore pathname))
               (generate-html-file-report file-data output-dir))
             *source-map*)
    
    (format t ";;; HTML coverage report generated: ~A~%" output-file)
    output-file))

(defun write-html-report (stream summary)
  "Write main HTML coverage report"
  ;; Write HTML header and summary
  (format stream "<!DOCTYPE html>~%<html>~%<head>~%    <title>Coverage Report</title>~%</head>~%<body>~%")
  (format stream "    <h1>Code Coverage Report</h1>~%")
  (format stream "    <h2>Summary</h2>~%")
  (format stream "    <p><strong>Total Files:</strong> ~D</p>~%" (coverage-summary-total-files summary))
  (format stream "    <p><strong>Covered Files:</strong> ~D</p>~%" (coverage-summary-covered-files summary))
  (format stream "    <p><strong>Line Coverage:</strong> ~,1F%% (~D/~D lines)</p>~%" 
          (coverage-summary-line-coverage-percent summary)
          (coverage-summary-covered-lines summary)
          (coverage-summary-total-lines summary))
  (format stream "    <p><strong>Form Coverage:</strong> ~,1F%% (~D/~D forms)</p>~%"
          (coverage-summary-form-coverage-percent summary)
          (coverage-summary-covered-forms summary)
          (coverage-summary-total-forms summary))
  (format stream "    <h2>File Coverage</h2>~%")
  (format stream "    <table>~%")
  (format stream "        <tr><th>File</th><th>Line Coverage</th><th>Form Coverage</th></tr>~%")
  
  ;; File rows
  (maphash (lambda (pathname file-data)
             (let ((line-pct (coverage-file-line-coverage-percent file-data))
                   (form-pct (coverage-file-form-coverage-percent file-data)))
               (format stream "        <tr>~%")
               (format stream "            <td>~A</td>~%" pathname)
               (format stream "            <td>~,1F%%</td>~%" line-pct)
               (format stream "            <td>~,1F%%</td>~%" form-pct)
               (format stream "        </tr>~%")))
           *source-map*)
  
  (format stream "    </table>
</body>
</html>"))

(defun generate-html-file-report (file-data output-dir)
  "Generate HTML report for a single file"
  (let ((output-file (path:string-path-join output-dir 
                                           (format nil "~A.html" 
                                                   (pathname-name (coverage-file-pathname file-data))))))
    (with-open-file (stream output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (write-html-file-report stream file-data))
    output-file))

(defun write-html-file-report (stream file-data)
  "Write HTML coverage report for a single file"
  (let ((pathname (coverage-file-pathname file-data)))
    (format stream "<!DOCTYPE html>~%<html>~%<head>~%    <title>Coverage: ~A</title>~%</head>~%<body>~%" pathname)
    (format stream "    <h1>~A</h1>~%" pathname)
    (format stream "    <p>Line Coverage: ~,1F%% (~D/~D)</p>~%" 
            (coverage-file-line-coverage-percent file-data)
            (coverage-file-covered-lines file-data)
            (coverage-file-total-lines file-data))
    (format stream "    <p>Form Coverage: ~,1F%% (~D/~D)</p>~%"
            (coverage-file-form-coverage-percent file-data)
            (coverage-file-covered-forms file-data)
            (coverage-file-total-forms file-data))
    (format stream "    <pre>~%")
    
    ;; Read and annotate source file
    (when (probe-file pathname)
      (with-open-file (source pathname :direction :input)
        (let ((line-num 1)
              (line-coverage-map (make-hash-table)))
          ;; Build line coverage map
          (dolist (line (coverage-file-lines file-data))
            (setf (gethash (coverage-line-number line) line-coverage-map)
                  (coverage-line-covered-p line)))
          
          ;; Output annotated source
          (loop for line = (read-line source nil nil)
                while line
                do (progn
                     (format stream "~3D: ~A~%" line-num (escape-html line))
                     (incf line-num)))))
    
    (format stream "    </pre>~%</body>~%</html>~%"))))

(defun escape-html (string)
  "Escape HTML special characters"
  (let ((result string))
    (setf result (substitute-string result "&" "&amp;"))
    (setf result (substitute-string result "<" "&lt;"))
    (setf result (substitute-string result ">" "&gt;"))
    (setf result (substitute-string result "\"" "&quot;"))
    result))

(defun substitute-string (string old new)
  "Replace all occurrences of OLD with NEW in STRING"
  (let ((old-len (length old))
        (result '())
        (i 0))
    (loop while (< i (length string))
          do (if (and (<= (+ i old-len) (length string))
                      (string= (subseq string i (+ i old-len)) old))
                 (progn
                   (setf result (nconc result (list new)))
                   (incf i old-len))
                 (progn
                   (setf result (nconc result (list (string (char string i)))))
                   (incf i))))
    (apply #'str:concat result)))

(defun generate-lcov-report (&key (output-dir *coverage-output-directory*))
  "Generate LCOV format coverage report"
  (fs:make-dirs output-dir)
  (let ((output-file (path:string-path-join output-dir "coverage.lcov")))
    (with-open-file (stream output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (write-lcov-report stream))
    (format t ";;; LCOV coverage report generated: ~A~%" output-file)
    output-file))

(defun write-lcov-report (stream)
  "Write LCOV format coverage data"
  (maphash (lambda (pathname file-data)
             (format stream "SF:~A~%" pathname)
             
             ;; Function records (using forms as functions)
             (dolist (form (coverage-file-forms file-data))
               (format stream "FN:~D,form_~D_~D~%" 
                       (coverage-form-start-line form)
                       (coverage-form-start-line form)
                       (coverage-form-start-column form))
               (format stream "FNDA:~D,form_~D_~D~%" 
                       (coverage-form-hit-count form)
                       (coverage-form-start-line form)
                       (coverage-form-start-column form)))
             
             ;; Function summary
             (format stream "FNF:~D~%" (coverage-file-total-forms file-data))
             (format stream "FNH:~D~%" (coverage-file-covered-forms file-data))
             
             ;; Line records
             (dolist (line (coverage-file-lines file-data))
               (format stream "DA:~D,~D~%" 
                       (coverage-line-number line)
                       (coverage-line-hit-count line)))
             
             ;; Line summary
             (format stream "LF:~D~%" (coverage-file-total-lines file-data))
             (format stream "LH:~D~%" (coverage-file-covered-lines file-data))
             (format stream "end_of_record~%"))
           *source-map*))

(defun generate-junit-report (&key (output-dir *coverage-output-directory*))
  "Generate JUnit XML format coverage report"
  (fs:make-dirs output-dir)
  (let ((output-file (path:string-path-join output-dir "coverage-junit.xml"))
        (summary (coverage-summary)))
    (with-open-file (stream output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (write-junit-report stream summary))
    (format t ";;; JUnit coverage report generated: ~A~%" output-file)
    output-file))

(defun write-junit-report (stream summary)
  "Write JUnit XML format coverage report"
  (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
  (format stream "<testsuite name=\"Coverage\" tests=\"~D\" failures=\"~D\" time=\"0\">~%"
          (coverage-summary-total-files summary)
          (- (coverage-summary-total-files summary)
             (coverage-summary-covered-files summary)))
  
  (maphash (lambda (pathname file-data)
             (let ((line-pct (coverage-file-line-coverage-percent file-data))
                   (form-pct (coverage-file-form-coverage-percent file-data))
                   (line-threshold (map:get *coverage-thresholds* :line-coverage))
                   (form-threshold (map:get *coverage-thresholds* :form-coverage)))
               (format stream "  <testcase classname=\"Coverage\" name=\"~A\">~%" pathname)
               (when (or (< line-pct line-threshold) (< form-pct form-threshold))
                 (format stream "    <failure message=\"Insufficient coverage\">~%")
                 (format stream "Line coverage: ~,1F%% (threshold: ~D%%)~%" line-pct line-threshold)
                 (format stream "Form coverage: ~,1F%% (threshold: ~D%%)~%" form-pct form-threshold)
                 (format stream "    </failure>~%"))
               (format stream "  </testcase>~%")))
           *source-map*)
  
  (format stream "</testsuite>~%"))

(defun generate-json-report (&key (output-dir *coverage-output-directory*))
  "Generate JSON format coverage report"
  (fs:make-dirs output-dir)
  (let ((output-file (path:string-path-join output-dir "coverage.json"))
        (summary (coverage-summary)))
    (with-open-file (stream output-file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (write-json-report stream summary))
    (format t ";;; JSON coverage report generated: ~A~%" output-file)
    output-file))

(defun write-json-report (stream summary)
  "Write JSON format coverage report"
  (format stream "{~%")
  (format stream "  \"summary\": {~%")
  (format stream "    \"total_files\": ~D,~%" (coverage-summary-total-files summary))
  (format stream "    \"covered_files\": ~D,~%" (coverage-summary-covered-files summary))
  (format stream "    \"line_coverage\": ~,2F,~%" (coverage-summary-line-coverage-percent summary))
  (format stream "    \"form_coverage\": ~,2F,~%" (coverage-summary-form-coverage-percent summary))
  (format stream "    \"total_lines\": ~D,~%" (coverage-summary-total-lines summary))
  (format stream "    \"covered_lines\": ~D,~%" (coverage-summary-covered-lines summary))
  (format stream "    \"total_forms\": ~D,~%" (coverage-summary-total-forms summary))
  (format stream "    \"covered_forms\": ~D~%" (coverage-summary-covered-forms summary))
  (format stream "  },~%")
  (format stream "  \"files\": {~%")
  
  (let ((first-file t))
    (maphash (lambda (pathname file-data)
               (unless first-file (format stream ",~%"))
               (setf first-file nil)
               (format stream "    \"~A\": {~%" pathname)
               (format stream "      \"line_coverage\": ~,2F,~%" (coverage-file-line-coverage-percent file-data))
               (format stream "      \"form_coverage\": ~,2F,~%" (coverage-file-form-coverage-percent file-data))
               (format stream "      \"total_lines\": ~D,~%" (coverage-file-total-lines file-data))
               (format stream "      \"covered_lines\": ~D,~%" (coverage-file-covered-lines file-data))
               (format stream "      \"total_forms\": ~D,~%" (coverage-file-total-forms file-data))
               (format stream "      \"covered_forms\": ~D~%" (coverage-file-covered-forms file-data))
               (format stream "    }"))
             *source-map*))
  
  (format stream "~%  }~%")
  (format stream "}~%"))

;;;; Integration

(defmacro with-coverage ((&key packages exclude-patterns output-dir) &body body)
  "Execute body with coverage tracking enabled"
  `(progn
     (start-coverage :packages ,packages :exclude-patterns ,exclude-patterns)
     (when ,output-dir
       (setf *coverage-output-directory* ,output-dir))
     (unwind-protect
          (progn ,@body)
       (stop-coverage))))

(defun run-tests-with-coverage (&key packages exclude-patterns 
                                    test-packages output-dir
                                    (report-formats '(:html :lcov)))
  "Run tests with coverage tracking and generate reports"
  (with-coverage (:packages packages 
                  :exclude-patterns exclude-patterns
                  :output-dir output-dir)
    ;; Run tests
    (if test-packages
        (dolist (pkg test-packages)
          (test:run :package pkg))
        (test:run))
    
    ;; Generate reports
    (fs:make-dirs *coverage-output-directory*)
    (dolist (format report-formats)
      (case format
        (:html (generate-html-report))
        (:lcov (generate-lcov-report))
        (:junit (generate-junit-report))
        (:json (generate-json-report))))
    
    ;; Return summary
    (coverage-summary)))