;;;; macro-extraction-tests.lisp - Tests for C macro extraction via libclang
;;;;
;;;; Tests the macro visibility feature added as part of IMPL-154

(defpackage :epsilon.foreign.macro-extraction.tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:local-nicknames
   (#:lc #:epsilon.foreign.libclang)
   (#:ab #:epsilon.foreign.auto-binding)
   (#:fs #:epsilon.file))
  (:enter t))

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(defvar *test-header* nil
  "Path to temporary test header file.")

(defun create-test-header (content)
  "Create a temporary header file with given content.
   Returns the path to the file as a string."
  (let ((path (fs:join-paths
               (or (sb-ext:posix-getenv "TMPDIR")
                   (sb-ext:posix-getenv "TMP")
                   "/var/tmp/")
               (format nil "test-macros-~A.h" (random 100000)))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string content out))
    (setf *test-header* path)
    path))

(defun cleanup-test-header ()
  "Remove the test header file."
  (when (and *test-header* (probe-file *test-header*))
    (delete-file *test-header*)
    (setf *test-header* nil)))

;;; ============================================================================
;;; Macro Cursor Constants Tests
;;; ============================================================================

(deftest macro-cursor-constants-defined
  "Verify macro cursor constants are defined"
  (assert-true (boundp 'lc:+cxcursor-macro-definition+))
  (assert-true (boundp 'lc:+cxcursor-macro-expansion+))
  (assert-true (boundp 'lc:+cxcursor-preprocessing-directive+))
  (assert-true (boundp 'lc:+cxcursor-inclusion-directive+))
  ;; Check values match libclang
  (assert-= lc:+cxcursor-macro-definition+ 501)
  (assert-= lc:+cxcursor-macro-expansion+ 502))

(deftest token-kind-constants-defined
  "Verify token kind constants are defined"
  (assert-true (boundp 'lc:+cxtoken-punctuation+))
  (assert-true (boundp 'lc:+cxtoken-keyword+))
  (assert-true (boundp 'lc:+cxtoken-identifier+))
  (assert-true (boundp 'lc:+cxtoken-literal+))
  (assert-true (boundp 'lc:+cxtoken-comment+)))

;;; ============================================================================
;;; Macro Classification Tests
;;; ============================================================================

(deftest classify-constant-macro
  "Test classification of constant macros"
  (let ((macro-info (list :name "FOO"
                          :body-string "42"
                          :is-function-like nil)))
    (multiple-value-bind (kind metadata)
        (ab:classify-macro macro-info)
      (assert-eq kind :constant)
      (assert-= (getf metadata :value) 42))))

(deftest classify-hex-constant
  "Test classification of hex constant macros"
  (let ((macro-info (list :name "FLAGS"
                          :body-string "0xFF"
                          :is-function-like nil)))
    (multiple-value-bind (kind metadata)
        (ab:classify-macro macro-info)
      (assert-eq kind :constant)
      (assert-= (getf metadata :value) 255))))

(deftest classify-string-constant
  "Test classification of string constant macros"
  (let ((macro-info (list :name "VERSION"
                          :body-string "\"1.0.0\""
                          :is-function-like nil)))
    (multiple-value-bind (kind metadata)
        (ab:classify-macro macro-info)
      (assert-eq kind :constant)
      (assert-equal (getf metadata :value) "1.0.0"))))

(deftest classify-function-wrapper-macro
  "Test classification of function wrapper macros"
  ;; Use body without spaces around parens (as libclang actually tokenizes)
  (let ((macro-info (list :name "SET_MODE"
                          :body-string "ctrl(x, 1, y, NULL)"
                          :is-function-like t
                          :params '("x" "y"))))
    (multiple-value-bind (kind metadata)
        (ab:classify-macro macro-info)
      (assert-eq kind :function-wrapper)
      (assert-equal (getf metadata :target-function) "ctrl"))))

(deftest classify-complex-macro
  "Test classification of complex macros"
  (let ((macro-info (list :name "DO_STUFF"
                          :body-string "do { x++; y--; } while(0)"
                          :is-function-like t
                          :params '("x" "y"))))
    (multiple-value-bind (kind metadata)
        (ab:classify-macro macro-info)
      (assert-eq kind :complex)
      (assert-not-null (getf metadata :reason)))))

;;; ============================================================================
;;; Macro Name Conversion Tests
;;; ============================================================================

(deftest macro-name-to-constant-conversion
  "Test conversion of C macro names to Lisp constants"
  ;; Compare symbol-names since symbols are interned in different packages
  (assert-equal (symbol-name (ab:macro-name-to-constant "FOO")) "+FOO+")
  (assert-equal (symbol-name (ab:macro-name-to-constant "SSL_CTRL_MODE")) "+SSL-CTRL-MODE+")
  (assert-equal (symbol-name (ab:macro-name-to-constant "MAX_SIZE")) "+MAX-SIZE+"))

;;; ============================================================================
;;; Libclang Integration Tests (require libclang)
;;; ============================================================================

(deftest extract-macros-from-header
  "Test extracting macros from a test header file"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((header (create-test-header "
#define ANSWER 42
#define VERSION \"1.0\"
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define STATUS_OK 0
#define STATUS_ERR 1
")))
    (unwind-protect
        (lc:with-parsed-header
            (tu header :options lc:+cxtranslationunit-detailed-preprocessing-record+)
          (let ((macros (lc:extract-macro-definitions tu)))
            ;; Should find our defined macros
            (let ((answer (find "ANSWER" macros :key (lambda (m) (getf m :name)) :test #'string=)))
              (assert-not-null answer "ANSWER macro should be found")
              (assert-nil (getf answer :is-function-like)))
            (let ((max-macro (find "MAX" macros :key (lambda (m) (getf m :name)) :test #'string=)))
              (assert-not-null max-macro "MAX macro should be found")
              (assert-not-null (getf max-macro :is-function-like)))))
      (cleanup-test-header))))

(deftest extract-macros-with-prefix
  "Test extracting macros filtered by prefix"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((header (create-test-header "
#define SSL_OK 1
#define SSL_ERR 0
#define SSL_set_mode(s, m) ssl_ctrl((s), 1, (m))
#define OTHER_MACRO 42
")))
    (unwind-protect
        (lc:with-parsed-header
            (tu header :options lc:+cxtranslationunit-detailed-preprocessing-record+)
          (let ((ssl-macros (lc:extract-macro-definitions tu :prefix "SSL_")))
            ;; Should only find SSL_* macros
            (assert-true (>= (length ssl-macros) 3))
            (dolist (m ssl-macros)
              (assert-not-null (search "SSL_" (getf m :name))))))
      (cleanup-test-header))))

;;; ============================================================================
;;; OpenSSL Macro Extraction Test (if headers available)
;;; ============================================================================

(defun find-openssl-header ()
  "Try to find OpenSSL ssl.h header."
  (let ((candidates '("/usr/include/openssl/ssl.h"
                      "/opt/homebrew/include/openssl/ssl.h"
                      "/usr/local/include/openssl/ssl.h")))
    (find-if #'probe-file candidates)))

(deftest openssl-macro-extraction
  "Test extracting SSL_* macros from OpenSSL headers"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((ssl-header (find-openssl-header)))
    (unless ssl-header
      (skip "OpenSSL headers not found"))
    (lc:with-parsed-header
        (tu ssl-header :options lc:+cxtranslationunit-detailed-preprocessing-record+)
      (let ((macros (lc:extract-macro-definitions tu :prefix "SSL_")))
        ;; Should find various SSL_* macros
        (assert-true (> (length macros) 0) "Should find SSL_* macros")
        ;; Look for known macros
        (let ((ctrl-mode (find "SSL_CTRL_MODE" macros
                               :key (lambda (m) (getf m :name))
                               :test #'string=)))
          (when ctrl-mode
            (assert-nil (getf ctrl-mode :is-function-like)
                     "SSL_CTRL_MODE should be a constant")))))))

(deftest describe-header-macros-works
  "Test the describe-header-macros utility function"
  (unless (lc:libclang-available-p)
    (skip "libclang not available"))
  (let ((header (create-test-header "
#define CONST_A 1
#define CONST_B 2
#define FUNC_WRAP(x) other_func(x)
")))
    (unwind-protect
        ;; Should not error, and output to standard out
        (let ((output (with-output-to-string (*standard-output*)
                        (ab:describe-header-macros header))))
          (assert-true (> (length output) 0) "Should produce output"))
      (cleanup-test-header))))
