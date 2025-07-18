(defpackage #:epsilon.tool.catalog.tests
  (:use #:cl #:epsilon.tool.test)
  (:local-nicknames
   (#:catalog #:epsilon.tool.catalog)))

(in-package #:epsilon.tool.catalog.tests)

(deftest test-type-catalog-creation ()
  "Test creating a type catalog"
  (let ((cat (make-instance 'catalog:type-catalog)))
    (is (typep cat 'catalog:type-catalog))
    (is (arrayp (slot-value cat 'catalog::code)))
    (is (hash-table-p (slot-value cat 'catalog::name)))))

(deftest test-make-type-field ()
  "Test creating type fields"
  (let ((field (catalog:make-type-field 
                 '(:name "id" :type "integer" :description "User ID"))))
    (is (typep field 'catalog:type-definition))
    (is-equal "id" (slot-value field 'catalog::name))
    (is-equal "integer" (slot-value field 'catalog::type)))
  
  ;; Test error cases
  (is-thrown-p 'error
    (catalog:make-type-field '(:name 123 :type "string")))
  (is-thrown-p 'error
    (catalog:make-type-field '(:name "field" :type 456))))

(deftest test-make-type-definition ()
  "Test creating type definitions"
  (let ((typedef (catalog:make-type-definition
                  '(:name "User"
                    :description "A user record"
                    :fields ((:name "id" :type "integer")
                             (:name "name" :type "string"))))))
    (is (typep typedef 'catalog:type-definition))
    (is-equal "User" (slot-value typedef 'catalog::name))
    (is-equal 2 (length (slot-value typedef 'catalog::fields))))
  
  ;; Test error case
  (is-thrown-p 'error
    (catalog:make-type-definition '(:name 123 :fields ()))))

(deftest test-parse-reference ()
  "Test parsing type references"
  ;; Simple type
  (is-equalp '("string" nil nil) 
             (catalog:parse-reference "string"))
  
  ;; Array type
  (is-equalp '("string" nil t)
             (catalog:parse-reference "string*"))
  
  ;; Fixed size type
  (is-equalp '("byte" 16 nil)
             (catalog:parse-reference "byte(16)"))
  
  ;; Composite type
  (let ((result (catalog:parse-reference "string, integer")))
    (is-equal :type (first result))
    (is-equal 2 (length (second result)))))

(deftest test-sha-256 ()
  "Test SHA-256 hashing"
  (let ((data #(0 1 2 3 4 5 6 7 8 9)))
    (is (typep (catalog:sha-256 data) '(vector (unsigned-byte 8))))
    (is-equal 32 (length (catalog:sha-256 data)))))

(register-test-package :epsilon.tool.catalog.tests)