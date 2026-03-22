;;;; libc-auto-binding.lisp - Example of auto-binding with libc
;;;;
;;;; This example demonstrates using the auto-binding system with the
;;;; standard C library. Most libc functions are simple enough for
;;;; auto-binding, with a few exceptions that need manual wrappers.

(defpackage epsilon.foreign.examples.libc
  (:use cl)
  (:local-nicknames
   (ffi epsilon.foreign)
   (auto epsilon.foreign.auto-binding)
   (sigs epsilon.foreign.signatures))
  (:export
   ;; Time functions
   #:libc-time
   #:libc-gettimeofday

   ;; String functions (auto-bound)
   #:libc-strlen
   #:libc-strcmp

   ;; Process functions
   #:libc-getpid
   #:libc-getuid

   ;; Math functions
   #:libc-sin
   #:libc-cos
   #:libc-sqrt

   ;; Utilities
   #:describe-libc-api)
  (:enter t))

;;; ============================================================================
;;; Example 1: Individual function auto-binding with defauto-jit
;;; ============================================================================
;;;
;;; For one-off function bindings, use defauto-jit which discovers the
;;; signature from headers at load time.

;; Math functions - pure auto-binding works well
(jit:defauto-jit libc-sin "sin" "math.h"
  :documentation "Compute sine of angle in radians")

(jit:defauto-jit libc-cos "cos" "math.h"
  :documentation "Compute cosine of angle in radians")

(jit:defauto-jit libc-sqrt "sqrt" "math.h"
  :documentation "Compute square root")

;; Process functions - simple return values, no args
(jit:defauto-jit libc-getpid "getpid" "unistd.h"
  :documentation "Get process ID")

(jit:defauto-jit libc-getuid "getuid" "unistd.h"
  :documentation "Get user ID")

;;; ============================================================================
;;; Example 2: Functions requiring manual string handling
;;; ============================================================================
;;;
;;; String functions need manual wrappers to handle C string <-> Lisp string
;;; conversion. The raw FFI call is auto-generated, but we wrap it.

;; Raw binding - takes pointer, returns size_t
(ffi:defshared %strlen-raw "strlen" "libc" :ulong
  (str :pointer)
  :documentation "Raw strlen binding")

;; Lisp wrapper with string conversion
(defun libc-strlen (string)
  "Return the length of STRING using C strlen.
   Demonstrates manual wrapper for string handling."
  (let ((c-str (sb-alien:make-alien-string string)))
    (unwind-protect
        (%strlen-raw (sb-alien:alien-sap c-str))
      (sb-alien:free-alien c-str))))

;; strcmp needs two string arguments
(ffi:defshared %strcmp-raw "strcmp" "libc" :int
  (s1 :pointer) (s2 :pointer)
  :documentation "Raw strcmp binding")

(defun libc-strcmp (s1 s2)
  "Compare two strings using C strcmp.
   Returns negative if s1 < s2, 0 if equal, positive if s1 > s2."
  (let ((c-s1 (sb-alien:make-alien-string s1))
        (c-s2 (sb-alien:make-alien-string s2)))
    (unwind-protect
        (%strcmp-raw (sb-alien:alien-sap c-s1)
                     (sb-alien:alien-sap c-s2))
      (sb-alien:free-alien c-s1)
      (sb-alien:free-alien c-s2))))

;;; ============================================================================
;;; Example 3: Functions requiring struct handling
;;; ============================================================================
;;;
;;; time() and gettimeofday() work with structs. We show both approaches:
;;; - Simple version that ignores struct output
;;; - Full version using JIT struct support

;; Simple time() - just returns time_t, ignore the output parameter
(ffi:defshared %time-raw "time" "libc" :long
  (tloc :pointer)
  :documentation "Raw time() - pass null to just get return value")

(defun libc-time ()
  "Get current Unix timestamp (seconds since epoch)."
  (%time-raw (sb-sys:int-sap 0)))

;; gettimeofday with struct - demonstrates struct-from-header
;; Note: This is a more advanced example showing struct handling
(ffi:defshared %gettimeofday-raw "gettimeofday" "libc" :int
  (tv :pointer) (tz :pointer)
  :documentation "Raw gettimeofday binding")

(defun libc-gettimeofday ()
  "Get current time with microsecond precision.
   Returns (values seconds microseconds)."
  ;; Allocate space for struct timeval (16 bytes on 64-bit)
  ;; struct timeval { time_t tv_sec; suseconds_t tv_usec; }
  (let ((tv-ptr (ffi:foreign-alloc 16)))
    (unwind-protect
        (progn
          (%gettimeofday-raw tv-ptr (sb-sys:int-sap 0))
          (values
           (sb-sys:sap-ref-64 tv-ptr 0)   ; tv_sec
           (sb-sys:sap-ref-64 tv-ptr 8))) ; tv_usec
      (ffi:foreign-free tv-ptr))))

;;; ============================================================================
;;; Example 4: Exploring the API
;;; ============================================================================

(defun describe-libc-api (&optional (header "stdlib.h"))
  "Describe functions available in a libc header.
   Useful for planning which functions to bind."
  (auto:describe-library-api
   (format nil "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/~A"
           header)))

;;; ============================================================================
;;; Usage Examples (in comments)
;;; ============================================================================

#|
;; Math functions
(libc-sin 0.0d0)        ; => 0.0d0
(libc-cos 0.0d0)        ; => 1.0d0
(libc-sqrt 4.0d0)       ; => 2.0d0

;; Process info
(libc-getpid)           ; => 12345 (your PID)
(libc-getuid)           ; => 501 (your UID)

;; String operations
(libc-strlen "hello")   ; => 5
(libc-strcmp "a" "b")   ; => -1 (negative, a < b)
(libc-strcmp "b" "a")   ; => 1 (positive, b > a)
(libc-strcmp "a" "a")   ; => 0 (equal)

;; Time
(libc-time)                         ; => 1705936800 (Unix timestamp)
(multiple-value-list (libc-gettimeofday))  ; => (1705936800 123456)

;; Explore API
(describe-libc-api "math.h")
(describe-libc-api "string.h")
|#
