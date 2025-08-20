;;;; Debug test for macro expansion

(defpackage :test-macro-debug
  (:use :cl :epsilon.test)
  (:import-from :epsilon.crypto
                #:crypto-error))

(in-package :test-macro-debug)

(defun test-macro-expansion ()
  (format t "Testing macro expansion...~%")
  
  ;; Show the macro expansion
  (let ((expansion (macroexpand-1 
                    '(is-thrown crypto-error
                                (error 'crypto-error :code -1 :message "test")))))
    (format t "Macro expansion:~%~S~%" expansion))
  
  ;; Try the actual test
  (format t "~%Running actual test:~%")
  (handler-case
      (is-thrown crypto-error
                 (error 'crypto-error :code -1 :message "test"))
    (error (e)
      (format t "Error during test: ~A~%" e))))

(test-macro-expansion)