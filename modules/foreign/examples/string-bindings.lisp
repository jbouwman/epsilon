;;;; string-bindings.lisp - Example: Binding C string functions
;;;;
;;;; This example demonstrates binding common C string functions
;;;; using epsilon's FFI system.

(defpackage :epsilon.foreign.examples.string-bindings
  (:use :cl)
  (:local-nicknames
   (:ffi :epsilon.foreign)
   (:bir :epsilon.foreign.binding-ir))
   (:enter t))

;;; ============================================================================
;;; Option 1: Manual Bindings (Simple Cases)
;;; ============================================================================

;; Direct defshared for simple functions
(ffi:defshared c-strlen "strlen" :libc :size-t
  (s :pointer))

(ffi:defshared c-strcmp "strcmp" :libc :int
  (s1 :pointer)
  (s2 :pointer))

;;; ============================================================================
;;; Option 2: JIT Discovery (Zero Setup)
;;; ============================================================================

;; These work without any declarations - signature discovered on first call
;; (ffi:call "strlen" :pointer my-string-ptr)
;; (ffi:call "memcpy" :pointer dest src n)

;;; ============================================================================
;;; Option 3: BIR-Based Bindings (Recommended for Libraries)
;;; ============================================================================

;; First, generate the BIR file (run once during development):
;;
;; (bir:grovel-to-file "/usr/include/string.h"
;;                     "bindings/string.bir.lisp"
;;                     :prefix "str")
;;
;; Then load at compile time:
;; (bir:define-library-from-ir :libc "bindings/string.bir.lisp")

;;; ============================================================================
;;; High-Level Wrappers
;;; ============================================================================

(defun lisp-strlen (string)
  "Get length of a Lisp string using C strlen.
   This is just for demonstration - use CL:LENGTH for real code!"
  (sb-sys:with-pinned-objects (string)
    (c-strlen (sb-sys:vector-sap string))))

(defun lisp-strcmp (s1 s2)
  "Compare two Lisp strings using C strcmp.
   Returns: negative if s1<s2, zero if equal, positive if s1>s2."
  (sb-sys:with-pinned-objects (s1 s2)
    (c-strcmp (sb-sys:vector-sap s1)
              (sb-sys:vector-sap s2))))

;;; ============================================================================
;;; Demo
;;; ============================================================================

(defun demo ()
  "Demonstrate the string bindings."
  (format t "~%=== String Bindings Demo ===~%")

  ;; strlen demo
  (let ((test-string "Hello, World!"))
    (format t "strlen(~S) = ~D~%" test-string (lisp-strlen test-string)))

  ;; strcmp demo
  (format t "strcmp(\"abc\", \"abd\") = ~D~%" (lisp-strcmp "abc" "abd"))
  (format t "strcmp(\"abc\", \"abc\") = ~D~%" (lisp-strcmp "abc" "abc"))
  (format t "strcmp(\"abd\", \"abc\") = ~D~%" (lisp-strcmp "abd" "abc")))
