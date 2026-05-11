;;;; epsilon.foreign tests
;;;;
;;;; Tests for the FFI type system, type database, marshalling
;;;; infrastructure, and memory pool. These tests exercise pure logic
;;;; and internal data structures without requiring external shared libraries.

(defpackage epsilon.foreign-tests
  (:use :cl :epsilon.syntax :epsilon.test)
  (:import (epsilon.foreign.trampoline trampoline)
           (epsilon.foreign.type-database tdb)
           (epsilon.foreign.marshalling marshalling)
           (epsilon.map map)))

;;; =========================================================================
;;; C Type Registry (trampoline module)
;;; =========================================================================

(deftest get-c-type-int
  "Lookup :int returns a c-type with correct properties"
  (let ((ct (trampoline:get-c-type :int)))
    (assert-true (trampoline:c-type-p ct))
    (assert-= 4 (trampoline:c-type-size ct))
    (assert-= 4 (trampoline:c-type-alignment ct))
    (assert-true (trampoline:c-type-signed-p ct))))

(deftest get-c-type-unsigned-int
  ":unsigned-int is not signed"
  (let ((ct (trampoline:get-c-type :unsigned-int)))
    (assert-not (trampoline:c-type-signed-p ct))
    (assert-= 4 (trampoline:c-type-size ct))))

(deftest get-c-type-pointer
  ":pointer is 8 bytes on 64-bit"
  (let ((ct (trampoline:get-c-type :pointer)))
    (assert-= 8 (trampoline:c-type-size ct))
    (assert-= 8 (trampoline:c-type-alignment ct))))

(deftest get-c-type-double
  ":double is 8 bytes"
  (let ((ct (trampoline:get-c-type :double)))
    (assert-= 8 (trampoline:c-type-size ct))))

(deftest get-c-type-float
  ":float is 4 bytes"
  (let ((ct (trampoline:get-c-type :float)))
    (assert-= 4 (trampoline:c-type-size ct))))

(deftest get-c-type-void
  ":void has size 0"
  (let ((ct (trampoline:get-c-type :void)))
    (assert-= 0 (trampoline:c-type-size ct))))

(deftest get-c-type-char
  ":char is 1 byte, signed"
  (let ((ct (trampoline:get-c-type :char)))
    (assert-= 1 (trampoline:c-type-size ct))
    (assert-true (trampoline:c-type-signed-p ct))))

(deftest get-c-type-unknown-signals-error
  "Unknown type signals an error"
  (assert-condition (error) (trampoline:get-c-type :nonexistent-type)))

;;; POSIX type aliases

(deftest posix-type-aliases
  "POSIX types resolve with correct sizes"
  (assert-= 8 (trampoline:c-type-size (trampoline:get-c-type :size-t)))
  (assert-= 8 (trampoline:c-type-size (trampoline:get-c-type :ssize-t)))
  (assert-= 4 (trampoline:c-type-size (trampoline:get-c-type :pid-t)))
  (assert-= 8 (trampoline:c-type-size (trampoline:get-c-type :off-t))))

;;; FFI Signatures

(deftest ffi-signature-creation
  "FFI signatures can be created and queried"
  (let ((sig (trampoline:make-ffi-signature
              :return-type :int
              :arg-types '(:pointer :int))))
    (assert-true (trampoline:ffi-signature-p sig))
    (assert-eq :int (trampoline:ffi-signature-return-type sig))
    (assert-equal '(:pointer :int) (trampoline:ffi-signature-arg-types sig))))

;;; =========================================================================
;;; Type Database
;;; =========================================================================

(deftest type-database-register-and-lookup
  "Register a type and look it up by name"
  (let ((tdb:*type-database* (map:make-map)))
    (let ((ti (tdb:make-c-type-info :name "test_struct" :kind :struct
                                     :size 16 :alignment 8)))
      (tdb:register-type ti)
      (let ((found (tdb:lookup-type "test_struct")))
        (assert-not-null found)
        (assert-equal "test_struct" (tdb:c-type-info-name found))
        (assert-eq :struct (tdb:c-type-info-kind found))
        (assert-= 16 (tdb:c-type-info-size found))))))

(deftest type-database-lookup-missing
  "Looking up a non-existent type returns nil"
  (let ((tdb:*type-database* (map:make-map)))
    (assert-nil (tdb:lookup-type "no_such_type"))))

(deftest type-database-clear
  "clear-database removes all registered types"
  (let ((tdb:*type-database* (map:make-map)))
    (tdb:register-type (tdb:make-c-type-info :name "foo" :kind :struct))
    (tdb:clear-database)
    (assert-nil (tdb:lookup-type "foo"))))

(deftest type-database-all-types
  "all-types returns all registered types"
  (let ((tdb:*type-database* (map:make-map)))
    (tdb:register-type (tdb:make-c-type-info :name "a" :kind :struct))
    (tdb:register-type (tdb:make-c-type-info :name "b" :kind :enum))
    (assert-= 2 (length (tdb:all-types)))))

(deftest type-database-types-by-kind
  "types-by-kind filters by kind"
  (let ((tdb:*type-database* (map:make-map)))
    (tdb:register-type (tdb:make-c-type-info :name "s1" :kind :struct))
    (tdb:register-type (tdb:make-c-type-info :name "e1" :kind :enum))
    (tdb:register-type (tdb:make-c-type-info :name "s2" :kind :struct))
    (assert-= 2 (length (tdb:types-by-kind :struct)))
    (assert-= 1 (length (tdb:types-by-kind :enum)))))

;;; c-field-info

(deftest c-field-info-creation
  "Field info structs hold field metadata"
  (let ((fi (tdb:make-c-field-info :name "x" :type "int" :type-kind :primitive
                                    :offset 0 :size 4 :is-pointer nil)))
    (assert-equal "x" (tdb:c-field-info-name fi))
    (assert-equal "int" (tdb:c-field-info-type fi))
    (assert-= 0 (tdb:c-field-info-offset fi))
    (assert-= 4 (tdb:c-field-info-size fi))
    (assert-not (tdb:c-field-info-is-pointer fi))))

;;; c-enum-constant

(deftest c-enum-constant-creation
  "Enum constants hold name and integer value"
  (let ((ec (tdb:make-c-enum-constant :name "FOO" :value 42)))
    (assert-equal "FOO" (tdb:c-enum-constant-name ec))
    (assert-= 42 (tdb:c-enum-constant-value ec))))

;;; Memory pool tests live with the module: avalon/foreign-pool/tests/.
