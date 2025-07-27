;;;; Temporarily disabled due to package loading issues
;; (defpackage #:epsilon.checksum.tests
;;   (:use
;;    #:cl
;;    #:epsilon.test
;;    #:epsilon.checksum.adler-32
;;    #:epsilon.checksum.generic
;;    #:epsilon.type))

;; (in-package #:epsilon.checksum.tests)

;; (deftest adler-32
;;   (let ((adler (make-instance 'adler-32))
;;         (buffer (->u8 (loop :for i :from 1 :to 32768
;;                             :collect (mod i 256)))))
;;     (update adler buffer 0 32768)
;;     (is (= (checksum adler)
;;            60605362))))