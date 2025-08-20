;;;; Debug test for error handling

(defpackage :test-error-debug
  (:use :cl)
  (:import-from :epsilon.crypto
                #:crypto-error))

(in-package :test-error-debug)

(defun test-error-condition ()
  (format t "Testing error condition...~%")
  
  ;; Check if we can signal and catch the error
  (handler-case
      (error 'crypto-error :code -1 :message "Test error")
    (crypto-error (e)
      (format t "Caught crypto-error: ~A~%" e)
      (format t "Error code: ~A~%" (epsilon.crypto:crypto-error-code e))
      (format t "Error message: ~A~%" (epsilon.crypto:crypto-error-string e)))
    (error (e)
      (format t "Caught other error: ~A~%" e)))
  
  ;; Check symbol package
  (format t "~%Symbol info:~%")
  (format t "crypto-error symbol: ~S~%" 'crypto-error)
  (format t "Symbol package: ~S~%" (symbol-package 'crypto-error))
  
  ;; Check class
  (let ((class (find-class 'crypto-error nil)))
    (if class
        (format t "Found class: ~S~%" class)
        (format t "No class found for crypto-error~%")))
  
  (format t "~%Test complete.~%"))

(test-error-condition)