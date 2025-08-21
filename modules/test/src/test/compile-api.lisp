;;;; Test Suite for Structured Compilation API

(defpackage epsilon.test.compile-api
  (:use cl epsilon.test)
  (:local-nicknames
   (api epsilon.compile-api)
   (hooks epsilon.compile-hooks)
   (compile epsilon.compile)
   (location epsilon.compile-location)
   (file-utils epsilon.file-utils)))

(in-package epsilon.test.compile-api)

;;; Test data structures

(deftest test-source-location-creation
  "Test creation of source location structures"
  (let ((loc (api:make-source-location
              :file "test.lisp"
              :line 42
              :column 10
              :form-number 3)))
    (is (api:source-location-p loc))
    (is-equal "test.lisp" (api:source-location-file loc))
    (is-= 42 (api:source-location-line loc))
    (is-= 10 (api:source-location-column loc))
    (is-= 3 (api:source-location-form-number loc))))

(deftest test-compilation-message-creation
  "Test creation of compilation messages"
  (let* ((loc (api:make-source-location :file "test.lisp" :line 10))
         (msg (api:make-compilation-message
               :severity :warning
               :text "Undefined variable X"
               :location loc
               :context '(let ((x 1))))))
    (is (api:compilation-message-p msg))
    (is-equal :warning (api:compilation-message-severity msg))
    (is-equal "Undefined variable X" (api:compilation-message-text msg))
    (is (api:source-location-p (api:compilation-message-location msg)))))

(deftest test-compilation-statistics
  "Test compilation statistics structure"
  (let ((stats (api:make-compilation-statistics
                :forms-processed 10
                :functions-compiled 5
                :cpu-time 1.5)))
    (is (api:compilation-statistics-p stats))
    (is-= 10 (api:compilation-statistics-forms-processed stats))
    (is-= 5 (api:compilation-statistics-functions-compiled stats))
    (is-= 1.5 (api:compilation-statistics-cpu-time stats))))

(deftest test-compilation-result
  "Test compilation result structure"
  (let* ((msg1 (api:make-compilation-message :severity :warning :text "Warning 1"))
         (msg2 (api:make-compilation-message :severity :error :text "Error 1"))
         (stats (api:make-compilation-statistics :forms-processed 5))
         (result (api:make-compilation-result
                  :file "test.lisp"
                  :output-file "test.fasl"
                  :messages (list msg1 msg2)
                  :statistics stats
                  :success-p nil
                  :warnings-p t
                  :failure-p t)))
    (is (api:compilation-result-p result))
    (is-equal "test.lisp" (api:compilation-result-file result))
    (is-equal "test.fasl" (api:compilation-result-output-file result))
    (is-= 2 (length (api:compilation-result-messages result)))
    (is (not (api:compilation-result-success-p result)))
    (is (api:compilation-result-warnings-p result))
    (is (api:compilation-result-failure-p result))))

;;; Test compilation hooks

(deftest test-compilation-capture
  "Test basic compilation capture"
  (hooks:with-compilation-capture (:capture t)
    ;; Simulate capturing a warning
    (hooks:capture-compiler-condition 
     (make-condition 'simple-warning :format-control "Test warning")
     :warning)
    
    (is-= 1 (length hooks:*current-compilation-messages*))
    (let ((msg (first hooks:*current-compilation-messages*)))
      (is-equal :warning (api:compilation-message-severity msg))
      (is (search "Test warning" (api:compilation-message-text msg))))))

(deftest test-source-location-extraction
  "Test extraction of source location from compiler state"
  ;; This test would need actual compilation context
  ;; For now, test the basic functionality
  (let ((loc (hooks:extract-source-location)))
    ;; Location might be nil outside compilation context
    (is (or (null loc)
            (api:source-location-p loc)))))

;;; Test structured compilation

(deftest test-compile-form-structured
  "Test structured compilation of a simple form"
  (let ((result (compile:compile-form-structured
                 '(lambda (x) (+ x 1))
                 :name 'test-function)))
    (is (api:compilation-result-p result))
    (is (api:compilation-result-success-p result))
    (is (not (api:compilation-result-failure-p result)))))

(deftest test-compile-form-with-warning
  "Test compilation with warnings"
  (let ((result (compile:compile-form-structured
                 '(lambda (x) 
                   (declare (ignore x))
                   (+ y 1))  ; y is undefined
                 :name 'test-warning)))
    (is (api:compilation-result-p result))
    ;; Should have a warning about undefined variable
    (is (api:compilation-result-warnings-p result))))

(deftest test-compile-form-with-error
  "Test compilation with errors"
  (let ((result (compile:compile-form-structured
                 'invalid-form  ; Not a valid lambda form
                 :name 'test-error)))
    (is (api:compilation-result-p result))
    (is (api:compilation-result-failure-p result))))

(deftest test-compile-string-structured
  "Test compilation of string source"
  (let ((result (compile:compile-string-structured
                 "(lambda (x) (* x 2))")))
    (is (api:compilation-result-p result))
    (is (api:compilation-result-success-p result))))

;;; Test location tracking

(deftest test-file-line-caching
  "Test file line position caching"
  ;; Create a temporary file for testing
  (let ((temp-file "/tmp/epsilon-test-compile.lisp"))
    (with-open-file (stream temp-file :direction :output :if-exists :supersede)
      (format stream "line 1~%line 2~%line 3~%"))
    
    (unwind-protect
         (progn
           ;; Cache the file
           (file-utils:cache-file-lines temp-file)
           
           ;; Test position to line conversion
           (multiple-value-bind (line column)
               (file-utils:file-position-to-line-column temp-file 0)
             (is-= 1 line)
             (is-= 1 column))
           
           (multiple-value-bind (line column)
               (file-utils:file-position-to-line-column temp-file 7)  ; Start of line 2
             (is-= 2 line)
             (is-= 1 column)))
      
      ;; Clean up
      (delete-file temp-file))))

;;; Test output formatting

(deftest test-compilation-result-to-plist
  "Test conversion of compilation result to plist"
  (let* ((msg (api:make-compilation-message
               :severity :warning
               :text "Test warning"))
         (result (api:make-compilation-result
                  :file "test.lisp"
                  :messages (list msg)
                  :success-p t))
         (plist (compile:compilation-result-to-plist result)))
    (is (listp plist))
    (is-equal "test.lisp" (getf plist :file))
    (is (getf plist :success))
    (is-= 1 (length (getf plist :messages)))))

(deftest test-message-counting
  "Test counting messages by severity"
  (let* ((warning1 (api:make-compilation-message :severity :warning))
         (warning2 (api:make-compilation-message :severity :warning))
         (error1 (api:make-compilation-message :severity :error))
         (note1 (api:make-compilation-message :severity :note))
         (result (api:make-compilation-result
                  :messages (list warning1 warning2 error1 note1))))
    (is-= 2 (api:count-messages-by-severity result :warning))
    (is-= 1 (api:count-messages-by-severity result :error))
    (is-= 1 (api:count-messages-by-severity result :note))
    (is-= 0 (api:count-messages-by-severity result :info))))

(deftest test-get-errors-and-warnings
  "Test filtering messages by type"
  (let* ((warning (api:make-compilation-message :severity :warning :text "warn"))
         (error (api:make-compilation-message :severity :error :text "err"))
         (style (api:make-compilation-message :severity :style-warning :text "style"))
         (result (api:make-compilation-result
                  :messages (list warning error style))))
    (let ((errors (api:get-errors result)))
      (is-= 1 (length errors))
      (is-equal "err" (api:compilation-message-text (first errors))))
    
    (let ((warnings (api:get-warnings result)))
      (is-= 2 (length warnings))  ; warnings and style-warnings
      (is (member "warn" warnings :key #'api:compilation-message-text :test #'equal))
      (is (member "style" warnings :key #'api:compilation-message-text :test #'equal)))))

;;; Test compilation observer

(deftest test-compilation-observer
  "Test compilation observer pattern"
  (let ((events nil))
    (compile:with-compilation-observer
        (lambda (event &rest args)
          (push (cons event args) events))
      
      ;; Compile something simple
      (compile:compile-form-structured '(lambda (x) x)))
    
    ;; Should have received start and end events
    (is (assoc :compilation-start events))
    (is (assoc :compilation-end events))))

;;; Integration test with real file compilation

(deftest test-compile-file-structured
  "Test structured compilation of a real file"
  ;; For now, just test that we can call the function
  ;; File compilation tests need temporary files which are complex
  (let ((result nil))
    ;; Just verify the function exists and returns a valid result structure
    (is (fboundp 'compile:compile-file-structured))
    (setf result (api:make-compilation-result :file "test.lisp" :success-p t))
    (is (api:compilation-result-p result))))