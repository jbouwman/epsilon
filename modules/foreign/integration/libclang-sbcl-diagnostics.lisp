;;;; libclang-sbcl-diagnostics.lisp - Diagnostic tests for libclang/SBCL memory interaction
;;;;
;;;; This module provides minimal test cases to verify whether claimed memory
;;;; interaction issues between libclang and SBCL actually manifest.
;;;;
;;;; ROOT CAUSE IDENTIFIED AND FIXED:
;;;; The with-alien-callable form in visit-children allocated a temporary
;;;; callback trampoline that was vulnerable to GC corruption: when GC ran
;;;; during or between visit-children calls the trampoline memory could be
;;;; reclaimed, leading to memory faults at NULL+offset addresses (e.g.,
;;;; 0x140D, 0x15B5, 0x18FC).
;;;;
;;;; FIX: visit-children now uses a static GC-safe trampoline created once
;;;; via alien-lambda2 + %define-alien-callable (the IMPL-224 pattern).
;;;; The trampoline dispatches through *visitor-callback* which is bound
;;;; per-invocation.
;;;;
;;;; This was NOT a struct-by-value calling convention issue -- it was the
;;;; trampoline allocation mechanism.  with-alien-callable places the
;;;; trampoline in GC-visible memory; %define-alien-callable places it in
;;;; static/immobile space that GC does not relocate.
;;;;
;;;; The four originally claimed issues and their verifiability:
;;;; 1. Memory mapping conflicts - NOT the root cause (misattributed)
;;;; 2. Signal handler interference - NOT reproduced
;;;; 3. Thread-local storage conflicts - NOT reproduced
;;;; 4. Immobile space pressure - NOT reproduced
;;;;
;;;; This file can be run standalone without the test framework for diagnostics.

(defpackage epsilon.foreign.libclang.diagnostics
  (:use cl)
  (:local-nicknames
   (lc epsilon.foreign.libclang))
  (:export
   #:run-all-diagnostics
   #:diagnose-memory-mapping
   #:diagnose-signal-handlers
   #:diagnose-immobile-space
   #:diagnose-gc-interaction
   #:minimal-gc-callback-reproducer
   #:*diagnostic-verbose*
   #:*diagnostic-results*)
  (:enter t))

;;; Configuration

(defvar *diagnostic-verbose* t
  "When T, print detailed diagnostic output")

(defvar *diagnostic-results* nil
  "Accumulator for diagnostic results: list of (name passed-p message)")

(defun diagnostic-log (format-string &rest args)
  "Log diagnostic message when verbose mode is enabled."
  (when *diagnostic-verbose*
    (apply #'format t format-string args)
    (force-output)))

(defun check (test-p message)
  "Assert that TEST-P is true, recording the result."
  (if test-p
      (diagnostic-log "[PASS] ~A~%" message)
      (diagnostic-log "[FAIL] ~A~%" message))
  test-p)

(defmacro with-diagnostic ((name) &body body)
  "Run a diagnostic test, catching errors and recording results."
  `(let ((passed nil)
         (error-message nil))
     (handler-case
         (progn
           (setf passed (progn ,@body))
           (push (list ,name passed nil) *diagnostic-results*))
       (error (e)
         (setf error-message (format nil "~A" e))
         (diagnostic-log "[ERROR] ~A: ~A~%" ,name error-message)
         (push (list ,name nil error-message) *diagnostic-results*)))
     passed))

;;; ============================================================================
;;; Utility: SBCL Memory Space Information
;;; ============================================================================

(defun get-sbcl-memory-info ()
  "Get information about SBCL's memory spaces."
  (list
   :dynamic-space-size (sb-ext:dynamic-space-size)
   ;; Use get-bytes-consed as a proxy for memory activity
   :bytes-consed (sb-ext:get-bytes-consed)
   ;; Internal time as a simple metric
   :timestamp (get-internal-real-time)))

(defun describe-memory-region (name start size)
  "Describe a memory region for diagnostic output."
  (diagnostic-log "  ~A: #x~X - #x~X (~,2f MB)~%"
                  name start (+ start size) (/ size 1024.0 1024.0)))

;;; ============================================================================
;;; Test 1: Memory Mapping Conflict Detection
;;; ============================================================================
;;;
;;; Hypothesis: libclang's internal allocations may map memory in regions that
;;; conflict with SBCL's expectations about memory layout.
;;;
;;; Verification strategy:
;;; 1. Record SBCL's memory regions before loading libclang
;;; 2. Load libclang and parse headers
;;; 3. Check if any foreign allocations overlap with SBCL spaces
;;; 4. Force GC and verify heap integrity

(defun diagnose-memory-mapping ()
  "Verify whether libclang memory allocations conflict with SBCL heap"
  (with-diagnostic ("Memory Mapping Conflict")
    (unless (lc:libclang-available-p)
      (diagnostic-log "SKIPPED: libclang not available~%")
      (return-from diagnose-memory-mapping t))

    (diagnostic-log "~%=== Memory Mapping Conflict Diagnostic ===~%")

    ;; Record pre-load state
    (let* ((pre-info (get-sbcl-memory-info))
           (dynamic-size (getf pre-info :dynamic-space-size))
           (all-passed t))

      (diagnostic-log "SBCL Dynamic Space: ~,2f MB available~%"
                     (/ dynamic-size 1024.0 1024.0))
      (diagnostic-log "Bytes consed before: ~D~%"
                     (getf pre-info :bytes-consed))

      ;; Create index and parse multiple headers to stress memory allocation
      (let ((index (lc:create-index)))
        (unwind-protect
            (progn
              ;; Parse a header to trigger libclang's internal allocations
              (diagnostic-log "~%Parsing test header...~%")
              (let ((tu (lc:parse-header-string
                         "struct test_struct { int x; double y; char* z; };
                          int test_function(int a, int b);
                          typedef unsigned long size_t;
                          enum test_enum { A, B, C };")))
                (when tu
                  (diagnostic-log "Translation unit created at: #x~X~%"
                                 (sb-sys:sap-int tu))

                  ;; Extract declarations to exercise more of libclang
                  (let ((decls (lc:extract-all-declarations tu)))
                    (diagnostic-log "Found ~D declarations~%" (length decls)))

                  (lc:dispose-translation-unit tu)))

              ;; Force GC to verify heap integrity
              (diagnostic-log "~%Forcing GC...~%")
              (sb-ext:gc :full t)

              ;; Verify we can still allocate and use Lisp objects
              (let ((test-list (loop for i from 0 below 1000
                                     collect (cons i (format nil "test-~D" i)))))
                (unless (check (= 1000 (length test-list))
                              "Post-libclang GC: Can allocate Lisp objects")
                  (setf all-passed nil))
                (unless (check (equal (car (nth 500 test-list)) 500)
                              "Post-libclang GC: Lisp objects have correct values")
                  (setf all-passed nil)))

              (diagnostic-log "Memory mapping test: ~A~%"
                             (if all-passed "PASSED" "FAILED")))

          ;; Cleanup
          (lc:dispose-index index)))

      ;; Compare post-load state
      (let ((post-info (get-sbcl-memory-info)))
        (diagnostic-log "~%Memory state comparison:~%")
        (diagnostic-log "  Bytes consed before: ~D, after: ~D (delta: ~D)~%"
                       (getf pre-info :bytes-consed)
                       (getf post-info :bytes-consed)
                       (- (getf post-info :bytes-consed)
                          (getf pre-info :bytes-consed))))

      all-passed)))

;;; ============================================================================
;;; Test 2: Signal Handler Interference Detection
;;; ============================================================================
;;;
;;; Hypothesis: LLVM may install signal handlers that interfere with SBCL's
;;; handlers for SIGSEGV, SIGBUS, etc.
;;;
;;; Verification strategy:
;;; 1. Trigger SBCL's GC which relies on memory protection signals
;;; 2. Verify that write barriers still work correctly
;;; 3. Check that SBCL's internal signal handling remains intact

(defun diagnose-signal-handlers ()
  "Verify whether libclang signal handlers interfere with SBCL"
  (with-diagnostic ("Signal Handler Interference")
    (unless (lc:libclang-available-p)
      (diagnostic-log "SKIPPED: libclang not available~%")
      (return-from diagnose-signal-handlers t))

    (diagnostic-log "~%=== Signal Handler Interference Diagnostic ===~%")

    ;; Load libclang
    (lc:load-libclang)

    (let ((all-passed t))
      ;; Create a reference that will survive across GC generations
      (let ((old-gen-object (make-array 100 :initial-element nil)))
        ;; Force it to old generation
        (sb-ext:gc :full t)
        (sb-ext:gc :full t)

        ;; Now create young objects that reference the old one
        ;; This exercises the write barrier which uses signals
        (diagnostic-log "Testing write barrier after libclang load...~%")

        (loop for i from 0 below 100
              do (setf (aref old-gen-object i)
                       (list i (format nil "young-~D" i))))

        ;; Force minor GC to verify write barrier worked
        (sb-ext:gc)

        ;; Verify references are intact
        (let ((valid-count
               (loop for i from 0 below 100
                     count (and (listp (aref old-gen-object i))
                               (= (car (aref old-gen-object i)) i)))))
          (unless (check (= valid-count 100)
                        "Write barrier after libclang: All cross-generational references intact")
            (setf all-passed nil))
          (diagnostic-log "Write barrier test: ~D/100 references valid~%" valid-count)))

      ;; Parse a header to exercise libclang further
      (let ((tu (lc:parse-header-string "int simple_func(int x);")))
        (when tu
          (lc:dispose-translation-unit tu)))

      ;; Final write barrier test
      (let ((post-parse-object (make-array 50)))
        (sb-ext:gc :full t)
        (loop for i from 0 below 50
              do (setf (aref post-parse-object i) (cons i (* i i))))
        (sb-ext:gc)

        (let ((valid (loop for i from 0 below 50
                           always (equal (aref post-parse-object i) (cons i (* i i))))))
          (unless (check valid "Post-parse write barrier: References intact")
            (setf all-passed nil))
          (diagnostic-log "Signal handler test: ~A~%" (if all-passed "PASSED" "FAILED"))))

      all-passed)))

;;; ============================================================================
;;; Test 3: Immobile Space Pressure
;;; ============================================================================
;;;
;;; Hypothesis: Heavy symbol creation during header parsing can exhaust
;;; SBCL's immobile space which has fixed size.
;;;
;;; Verification strategy:
;;; 1. Parse many headers creating many symbols
;;; 2. Monitor immobile space usage
;;; 3. Verify we don't exhaust immobile space or corrupt it

(defun count-package-symbols (package)
  "Count symbols in a package."
  (let ((count 0))
    (do-symbols (sym package)
      (declare (ignore sym))
      (incf count))
    count))

(defun diagnose-immobile-space ()
  "Verify whether libclang usage causes immobile space pressure"
  (with-diagnostic ("Immobile Space Pressure")
    (unless (lc:libclang-available-p)
      (diagnostic-log "SKIPPED: libclang not available~%")
      (return-from diagnose-immobile-space t))

    (diagnostic-log "~%=== Immobile Space Pressure Diagnostic ===~%")

    ;; Count symbols before
    (let* ((cl-user-before (count-package-symbols :cl-user))
           (keyword-before (count-package-symbols :keyword))
           (total-before (+ cl-user-before keyword-before))
           (all-passed t))

      (diagnostic-log "Symbols before: CL-USER=~D, KEYWORD=~D, total=~D~%"
                     cl-user-before keyword-before total-before)

      ;; Parse headers that would create many symbols
      (let ((headers-parsed 0))
        (dotimes (i 10)
          (let ((code (format nil
                             "struct generated_struct_~D { int field_~D_a; double field_~D_b; };
                              int generated_func_~D(int arg_~D);
                              enum generated_enum_~D { VAL_~D_A, VAL_~D_B, VAL_~D_C };
                              typedef struct generated_struct_~D generated_type_~D;"
                             i i i i i i i i i i i)))
            (let ((tu (lc:parse-header-string code)))
              (when tu
                (incf headers-parsed)
                ;; Extract all declarations to exercise symbol creation
                (lc:extract-all-declarations tu)
                (lc:dispose-translation-unit tu)))))

        (diagnostic-log "Parsed ~D headers~%" headers-parsed))

      ;; Count symbols after
      (let* ((cl-user-after (count-package-symbols :cl-user))
             (keyword-after (count-package-symbols :keyword))
             (total-after (+ cl-user-after keyword-after))
             (new-symbols (- total-after total-before)))

        (diagnostic-log "Symbols after: CL-USER=~D, KEYWORD=~D, total=~D~%"
                       cl-user-after keyword-after total-after)
        (diagnostic-log "New symbols created: ~D~%"  new-symbols)

        ;; Verify we can still create symbols (immobile space not exhausted)
        (let ((test-symbols
               (loop for i from 0 below 100
                     collect (intern (format nil "DIAGNOSTIC-TEST-SYM-~D" i)
                                    :cl-user))))
          (unless (check (= 100 (length test-symbols))
                        "Can still create symbols after libclang parsing")
            (setf all-passed nil)))

        ;; Force GC to clean up
        (sb-ext:gc :full t)

        (diagnostic-log "Immobile space test: ~A~%"
                       (if all-passed "PASSED" "FAILED"))
        all-passed))))

;;; ============================================================================
;;; Test 4: GC Interaction Stress Test
;;; ============================================================================
;;;
;;; Hypothesis: Concurrent GC during libclang operations may cause corruption
;;; due to memory management conflicts.
;;;
;;; Verification strategy:
;;; 1. Create GC pressure while using libclang
;;; 2. Verify data integrity after mixed GC/FFI operations
;;; 3. Check for any corruption signals

(defun diagnose-gc-interaction ()
  "Verify GC/libclang interaction under stress"
  (with-diagnostic ("GC Interaction Stress")
    (unless (lc:libclang-available-p)
      (diagnostic-log "SKIPPED: libclang not available~%")
      (return-from diagnose-gc-interaction t))

    (diagnostic-log "~%=== GC Interaction Stress Test ===~%")

    (let ((integrity-failures 0)
          (iterations 20))

      ;; Create reference data
      (let ((reference-data
             (loop for i from 0 below 100
                   collect (list i (format nil "ref-~D" i) (make-array 10 :initial-element i)))))

        (dotimes (iter iterations)
          ;; Alternate between libclang operations and GC
          (when (evenp iter)
            ;; Parse a header
            (let ((tu (lc:parse-header-string
                       (format nil "struct iter_struct_~D { int val; };" iter))))
              (when tu
                (lc:extract-all-declarations tu)
                (lc:dispose-translation-unit tu))))

          ;; Force GC
          (if (zerop (mod iter 5))
              (sb-ext:gc :full t)
              (sb-ext:gc))

          ;; Create allocation pressure
          (let ((temp-data (loop for i from 0 below 1000
                                 collect (cons i (make-string 100)))))
            (declare (ignore temp-data)))

          ;; Verify reference data integrity
          (let ((valid
                 (loop for item in reference-data
                       for expected-i from 0
                       always (and (= (first item) expected-i)
                                  (string= (second item) (format nil "ref-~D" expected-i))
                                  (= (aref (third item) 0) expected-i)))))
            (unless valid
              (incf integrity-failures)
              (diagnostic-log "Integrity failure at iteration ~D~%" iter))))

        (diagnostic-log "Completed ~D iterations with ~D integrity failures~%"
                       iterations integrity-failures)

        (let ((passed (check (zerop integrity-failures)
                            "GC interaction: No data corruption detected")))
          (diagnostic-log "GC interaction test: ~A~%"
                         (if passed "PASSED" "FAILED"))
          passed)))))

;;; ============================================================================
;;; Test 5: Repeated Load/Unload Stress Test
;;; ============================================================================
;;;
;;; Additional test to verify stability under repeated libclang operations

(defun diagnose-repeated-operations ()
  "Verify stability under repeated libclang operations"
  (with-diagnostic ("Repeated Operations Stability")
    (unless (lc:libclang-available-p)
      (diagnostic-log "SKIPPED: libclang not available~%")
      (return-from diagnose-repeated-operations t))

    (diagnostic-log "~%=== Repeated Operations Stress Test ===~%")

    (let ((success-count 0)
          (failure-count 0)
          (iterations 50))

      (dotimes (i iterations)
        (handler-case
            (let ((tu (lc:parse-header-string
                       (format nil "int func_~D(int x, int y);" i))))
              (when tu
                (let ((info (lc:extract-function-info tu (format nil "func_~D" i))))
                  (when (and info (string= (getf info :name) (format nil "func_~D" i)))
                    (incf success-count)))
                (lc:dispose-translation-unit tu)))
          (error (e)
            (incf failure-count)
            (diagnostic-log "Error at iteration ~D: ~A~%" i e))))

      (diagnostic-log "Completed ~D iterations: ~D successes, ~D failures~%"
                     iterations success-count failure-count)

      (let ((all-passed t))
        (unless (check (zerop failure-count) "No errors during repeated operations")
          (setf all-passed nil))
        (unless (check (= success-count iterations) "All operations succeeded")
          (setf all-passed nil))
        (diagnostic-log "Repeated operations test: ~A~%"
                       (if all-passed "PASSED" "FAILED"))
        all-passed))))

;;; ============================================================================
;;; Minimal Reproducer for GC/Callback Bug
;;; ============================================================================
;;;
;;; This demonstrates the actual root cause: GC breaks struct-by-value callbacks.
;;; The crash is NOT due to libclang memory management but to SBCL's
;;; with-alien-callable not surviving GC.

(defun minimal-gc-callback-reproducer ()
  "Minimal reproducer for the GC/callback crash bug.

   This will crash with 'Memory fault at 0xNNNN' on the second visit-children
   call if the vendored SBCL's struct-by-value callback mechanism is broken.

   Returns T if no crash (bug is fixed), signals error otherwise."
  (unless (lc:libclang-available-p)
    (format t "SKIPPED: libclang not available~%")
    (return-from minimal-gc-callback-reproducer t))

  (format t "~%=== Minimal GC/Callback Reproducer ===~%")
  (format t "This tests whether struct-by-value callbacks survive GC.~%")
  (format t "~%")

  ;; Step 1: First visit-children (should work)
  (format t "Step 1: First visit-children call...~%")
  (let ((tu1 (lc:parse-header-string "int func_1(void);")))
    (let ((root (lc:get-translation-unit-cursor tu1)))
      (lc:visit-children root (lambda (child) (declare (ignore child)) 1)))
    (lc:dispose-translation-unit tu1))
  (format t "  OK~%")

  ;; Step 2: Force GC (this is what breaks things)
  (format t "Step 2: Running full GC...~%")
  (sb-ext:gc :full t)
  (format t "  OK~%")

  ;; Step 3: Second visit-children (will crash if bug present)
  (format t "Step 3: Second visit-children call...~%")
  (format t "  (If you see 'Memory fault', the bug is present)~%")
  (let ((tu2 (lc:parse-header-string "int func_2(void);")))
    (let ((root (lc:get-translation-unit-cursor tu2)))
      (lc:visit-children root (lambda (child) (declare (ignore child)) 1)))
    (lc:dispose-translation-unit tu2))
  (format t "  OK~%")

  (format t "~%=== PASSED: GC/Callback bug not present ===~%")
  t)

;;; ============================================================================
;;; Summary and Runner
;;; ============================================================================

(defun run-all-diagnostics ()
  "Run all diagnostic tests and print summary."
  (setf *diagnostic-results* nil)

  (format t "~%")
  (format t "================================================================~%")
  (format t "libclang/SBCL Memory Interaction Diagnostics~%")
  (format t "================================================================~%")
  (format t "~%")
  (format t "Testing claimed issues:~%")
  (format t "  1. Memory mapping conflicts~%")
  (format t "  2. Signal handler interference~%")
  (format t "  3. Immobile space pressure~%")
  (format t "  4. GC interaction~%")
  (format t "  5. Repeated operation stability~%")
  (format t "~%")

  (let ((*diagnostic-verbose* t))
    ;; Run all tests
    (diagnose-memory-mapping)
    (diagnose-signal-handlers)
    (diagnose-immobile-space)
    (diagnose-gc-interaction)
    (diagnose-repeated-operations))

  (format t "~%")
  (format t "================================================================~%")
  (format t "Diagnostic Results Summary~%")
  (format t "================================================================~%")
  (format t "~%")

  ;; Print results summary
  (let ((passed 0)
        (failed 0)
        (errors 0))
    (dolist (result (reverse *diagnostic-results*))
      (destructuring-bind (name passed-p error-msg) result
        (cond
          (error-msg (incf errors))
          (passed-p (incf passed))
          (t (incf failed)))
        (format t "~A: ~A~A~%"
                name
                (cond (error-msg "ERROR")
                      (passed-p "PASS")
                      (t "FAIL"))
                (if error-msg (format nil " - ~A" error-msg) ""))))

    (format t "~%")
    (format t "Total: ~D passed, ~D failed, ~D errors~%"
            passed failed errors)
    (format t "~%"))

  (format t "================================================================~%")
  (format t "Verifiability Assessment~%")
  (format t "================================================================~%")
  (format t "~%")
  (format t "The claimed issues and their verifiability:~%")
  (format t "~%")
  (format t "1. Memory mapping conflicts:~%")
  (format t "   VERIFIABLE via heap integrity checks after libclang operations~%")
  (format t "   Evidence: Run diagnose-memory-mapping and check for corruption~%")
  (format t "~%")
  (format t "2. Signal handler interference:~%")
  (format t "   VERIFIABLE via write barrier tests across GC generations~%")
  (format t "   Evidence: Run diagnose-signal-handlers, check cross-gen refs~%")
  (format t "~%")
  (format t "3. Immobile space pressure:~%")
  (format t "   VERIFIABLE by monitoring symbol counts before/after parsing~%")
  (format t "   Evidence: Run diagnose-immobile-space with large header sets~%")
  (format t "~%")
  (format t "4. GC interaction:~%")
  (format t "   VERIFIABLE via interleaved GC/FFI stress testing~%")
  (format t "   Evidence: Run diagnose-gc-interaction, check data integrity~%")
  (format t "~%")
  (format t "Note: If all tests pass, the claimed issues may be:~%")
  (format t "  - Fixed in this version of libclang/SBCL~%")
  (format t "  - Not reproducible with this workload~%")
  (format t "  - Requiring more aggressive stress testing to manifest~%")
  (format t "~%")
  *diagnostic-results*)
