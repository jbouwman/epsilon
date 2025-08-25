;;;; Tests for the protocol system and package sources

(defpackage :epsilon.test.protocol
  (:use :cl :epsilon.test)
  (:local-nicknames
   (:proto :epsilon.protocol)
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
