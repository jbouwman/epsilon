;;;; smart-ffi.lisp - Smart FFI interface with auto-discovery
;;;;
;;;; This module provides user-friendly macros and functions that
;;;; automatically discover function signatures and provide optimal
;;;; FFI integration.

(in-package #:epsilon.foreign)

;;;; Auto-discovery function delegation

(defun auto-discover-signature (function-designator)
  "Automatically discover function signature using clang"
  (if (find-package :epsilon.clang.signatures)
      (funcall (find-symbol "AUTO-DISCOVER-SIGNATURE" :epsilon.clang.signatures) function-designator)
      (progn
        (warn "Clang signature extraction not available")
        nil)))

;;; Smart defshared macro with auto-discovery

(defmacro defshared-auto (lisp-name c-name &optional library)
  "Define foreign function with automatic signature detection"
  (let ((function-designator (if library
                                 (list c-name library)
                                 c-name)))
    `(progn
       ;; Try to discover signature at compile time
       (eval-when (:compile-toplevel :load-toplevel)
         (let ((signature (auto-discover-signature ',function-designator)))
           (when signature
             (format t "Auto-discovered signature for ~A: ~A~%" 
                     ',c-name signature))))
       
       ;; Define the function with runtime signature discovery
       (defun ,lisp-name (&rest args)
         ,(format nil "Automatically generated FFI binding for ~A" c-name)
         (let ((signature (auto-discover-signature ',function-designator)))
           (if signature
               (apply #'shared-call-unified ',function-designator
                      (getf signature :return-type)
                      (getf signature :arg-types)
                      args)
               (error "Could not determine signature for ~A" ',c-name)))))))

(defmacro defshared-smart (lisp-name c-name &optional library return-type arg-types)
  "Define foreign function with smart signature handling"
  (let ((function-designator (if library
                                 (list c-name library)
                                 c-name)))
    (if (and return-type arg-types)
        ;; Use provided signature
        `(defun ,lisp-name (&rest args)
           ,(format nil "FFI binding for ~A (~A ~A)" c-name return-type arg-types)
           (apply #'shared-call-unified ',function-designator
                  ',return-type ',arg-types args))
        ;; Auto-discover signature
        `(defshared-auto ,lisp-name ,c-name ,library))))

;;;; Enhanced function call interface

(defun ffi-call (function-name &rest args)
  "Smart FFI call with automatic signature detection"
  (let ((signature (auto-discover-signature function-name)))
    (if signature
        (apply #'shared-call-unified function-name 
               (getf signature :return-type)
               (getf signature :arg-types)
               args)
        (error "Could not determine signature for function ~A" function-name))))

(defun ffi-call-cached (function-name &rest args)
  "FFI call using cached signature (faster for repeated calls)"
  (let ((cached-sig (if (find-package :epsilon.clang.signatures)
                        (funcall (find-symbol "GET-CACHED-SIGNATURE" :epsilon.clang.signatures)
                                function-name)
                        nil)))
    (if cached-sig
        (let ((sig-types (if (find-package :epsilon.clang.signatures)
                            (funcall (find-symbol "SIGNATURE-TO-EPSILON-TYPES" :epsilon.clang.signatures)
                                    cached-sig)
                            nil)))
          (if sig-types
              (apply #'shared-call-unified function-name
                     (getf sig-types :return-type)
                     (getf sig-types :arg-types)
                     args)
              (apply #'ffi-call function-name args)))
        (apply #'ffi-call function-name args))))

;;;; Signature management utilities

(defun preload-common-signatures ()
  "Preload signatures for commonly used functions"
  (let ((common-functions 
         '(("strlen" "libc")
           ("malloc" "libc")
           ("free" "libc")
           ("printf" "libc")
           ("getpid" "libc")
           ("open" "libc")
           ("close" "libc")
           ("read" "libc")
           ("write" "libc"))))
    
    (format t "Preloading signatures for ~D common functions...~%" 
            (length common-functions))
    
    (dolist (func-spec common-functions)
      (handler-case
          (let ((signature (auto-discover-signature func-spec)))
            (when signature
              (format t "  ~A: OK~%" (first func-spec))))
        (error (e)
          (format t "  ~A: Failed (~A)~%" (first func-spec) e))))))

(defun validate-signature-database ()
  "Validate cached signatures against current system"
  (format t "Validating signature database...~%")
  (let ((validated 0)
        (failed 0))
    
    (when (find-package :epsilon.clang.signatures)
      (let ((db-symbol (find-symbol "*SIGNATURE-DATABASE*" :epsilon.clang.signatures)))
        (when db-symbol
          (maphash (lambda (key cached-entry)
                     (handler-case
                         (let ((signature (getf cached-entry :signature)))
                           (when signature
                             ;; Try to use the signature
                             (let ((fn-name (if (listp key) (first key) key)))
                               (resolve-function-address fn-name)
                               (incf validated))))
                       (error (e)
                         (format t "  Validation failed for ~A: ~A~%" key e)
                         (incf failed))))
                   (symbol-value db-symbol)))))
    
    (format t "Signature validation complete: ~D valid, ~D failed~%" 
            validated failed)))

;;;; Development and debugging utilities

(defmacro with-ffi-debugging (&body body)
  "Execute body with enhanced FFI debugging"
  `(let ((*track-call-performance* t)
         (*warn-about-legacy* t))
     (format t "FFI debugging enabled~%")
     (unwind-protect
          (progn ,@body)
       (format t "FFI call statistics:~%")
       (let ((stats (get-call-statistics)))
         (dolist (stat stats)
           (let ((func-name (car stat))
                 (call-info (cdr stat)))
             (format t "  ~A: ~D calls, ~,3F total time~%"
                     func-name
                     (call-stats-count call-info)
                     (call-stats-total-time call-info))))))))

(defun benchmark-ffi-approach (function-name args &key (iterations 1000))
  "Benchmark different FFI approaches for a function"
  (format t "Benchmarking FFI approaches for ~A (~D iterations)...~%" 
          function-name iterations)
  
  ;; Benchmark original shared-call (if available)
  (let ((original-time
         (handler-case
             (let ((start (get-internal-real-time)))
               (dotimes (i iterations)
                 (apply #'shared-call function-name :int '() args))
               (/ (- (get-internal-real-time) start) 
                  internal-time-units-per-second))
           (error (e)
             (format t "  Original shared-call failed: ~A~%" e)
             nil))))
    
    ;; Benchmark libffi unified call
    (let ((unified-time
           (handler-case
               (let ((start (get-internal-real-time)))
                 (dotimes (i iterations)
                   (apply #'shared-call-unified function-name :int '() args))
                 (/ (- (get-internal-real-time) start) 
                    internal-time-units-per-second))
             (error (e)
               (format t "  Unified call failed: ~A~%" e)
               nil))))
      
      ;; Benchmark auto-discovery call
      (let ((auto-time
             (handler-case
                 (let ((start (get-internal-real-time)))
                   (dotimes (i iterations)
                     (apply #'ffi-call function-name args))
                   (/ (- (get-internal-real-time) start) 
                      internal-time-units-per-second))
               (error (e)
                 (format t "  Auto-discovery call failed: ~A~%" e)
                 nil))))
        
        ;; Report results
        (when original-time
          (format t "  Original:      ~,6F seconds (~,2F μs/call)~%" 
                  original-time (* original-time 1000000 (/ iterations))))
        (when unified-time
          (format t "  Unified:       ~,6F seconds (~,2F μs/call)~%" 
                  unified-time (* unified-time 1000000 (/ iterations))))
        (when auto-time
          (format t "  Auto-discover: ~,6F seconds (~,2F μs/call)~%" 
                  auto-time (* auto-time 1000000 (/ iterations))))
        
        ;; Performance comparison
        (when (and original-time unified-time)
          (let ((speedup (/ original-time unified-time)))
            (format t "  Speedup: ~,2Fx~%" speedup)))))))

;;;; Convenience macros for common patterns

(defmacro with-c-library (library-name &body body)
  "Execute body with default library context"
  `(let ((*default-library* ,library-name))
     ,@body))

(defmacro defcfuns (library-name &body function-specs)
  "Define multiple C functions from the same library"
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (lisp-name c-name &optional return-type arg-types) spec
                   `(defshared-smart ,lisp-name ,c-name ,library-name ,return-type ,arg-types)))
               function-specs)))

;;;; Example usage and testing

(defun test-smart-ffi ()
  "Test smart FFI functionality"
  (format t "Testing smart FFI functionality...~%")
  
  ;; Test auto-discovery
  (handler-case
      (let ((pid (ffi-call "getpid")))
        (format t "getpid() via auto-discovery: ~A~%" pid))
    (error (e)
      (format t "Auto-discovery test failed: ~A~%" e)))
  
  ;; Test cached calls
  (handler-case
      (let ((len (ffi-call-cached "strlen" "hello world")))
        (format t "strlen() via cached call: ~A~%" len))
    (error (e)
      (format t "Cached call test failed: ~A~%" e)))
  
  ;; Test performance
  (when (> (hash-table-count *call-statistics*) 0)
    (format t "Call statistics: ~A~%" (get-call-statistics))))

;;;; Migration helpers

(defmacro migrate-defshared (lisp-name c-name library return-type arg-types)
  "Migrate old defshared to new smart version"
  `(progn
     ;; Emit deprecation warning
     (warn "migrate-defshared is deprecated. Use defshared-smart instead.")
     
     ;; Create new smart definition
     (defshared-smart ,lisp-name ,c-name ,library ,return-type ,arg-types)))

(defun audit-ffi-usage ()
  "Audit current FFI usage and suggest improvements"
  (format t "FFI Usage Audit:~%")
  
  ;; Check libffi availability
  (format t "  libffi available: ~A~%" (and (boundp '*libffi-library*) *libffi-library* t))
  (format t "  libffi for calls: ~A~%" (libffi-available-for-calls-p))
  
  ;; Check signature database
  (when (find-package :epsilon.clang.signatures)
    (let ((db-symbol (find-symbol "*SIGNATURE-DATABASE*" :epsilon.clang.signatures)))
      (when db-symbol
        (format t "  Cached signatures: ~D~%" 
                (hash-table-count (symbol-value db-symbol))))))
  
  ;; Check call statistics
  (format t "  Tracked calls: ~D~%" (hash-table-count *call-statistics*))
  
  ;; Suggestions
  (format t "  Suggestions:~%")
  (unless (and (boundp '*libffi-library*) *libffi-library*)
    (format t "    - Install libffi for better performance~%"))
  (unless *track-call-performance*
    (format t "    - Enable call tracking for optimization~%"))
  (when (< (hash-table-count *call-statistics*) 10)
    (format t "    - Consider preloading common signatures~%")))

;;;; Initialize smart FFI system

(defun initialize-smart-ffi ()
  "Initialize the smart FFI system"
  (format t "Initializing smart FFI system...~%")
  
  ;; Load signature database if available
  (when (find-package :epsilon.clang.signatures)
    (handler-case
        (funcall (find-symbol "LOAD-SIGNATURE-DATABASE" :epsilon.clang.signatures))
      (error (e)
        (format t "Could not load signature database: ~A~%" e))))
  
  ;; Preload common signatures
  (handler-case
      (preload-common-signatures)
    (error (e)
      (format t "Could not preload signatures: ~A~%" e)))
  
  (format t "Smart FFI initialization complete~%"))

;; Initialize when loaded
(eval-when (:load-toplevel :execute)
  (initialize-smart-ffi))