;;;; Benchmark Suites
;;;;
;;;; This module provides organized benchmark suites for different subsystems

(defpackage epsilon.tool.benchmark.suites
  (:use cl)
  (:local-nicknames
   (benchmark epsilon.tool.benchmark)
   (map epsilon.map))
  (:export
   ;; Suite management
   define-suite
   register-suite
   list-suites
   run-suite
   get-suite
   benchmark-suite-description
   
   ;; Standard suites
   register-core-suite
   register-functional-data-suite
   register-ffi-suite
   register-http2-suite
   register-crypto-suite
   register-quick-suite
   register-all-suites
   
   ;; Main entry point
   main
   
   ;; Configuration functions
   run-default-benchmarks
   run-ci-benchmarks
   run-performance-critical-benchmarks
   run-all-benchmarks
   save-baselines
   compare-baselines))

(in-package epsilon.tool.benchmark.suites)

;;; Suite infrastructure

(defvar *benchmark-suites* (map:make-map)
  "Registry of benchmark suites")

(defstruct benchmark-suite
  "A collection of related benchmarks"
  name
  description
  benchmarks
  setup-fn
  teardown-fn)

(defmacro define-suite (name (&key description setup teardown) &body benchmarks)
  "Define a benchmark suite"
  `(register-suite ',name
                   :description ,description
                   :setup-fn ,setup
                   :teardown-fn ,teardown
                   :benchmarks (list ,@(mapcar (lambda (b) `',b) benchmarks))))

(defun register-suite (name &key description benchmarks setup-fn teardown-fn)
  "Register a benchmark suite"
  (let ((suite (make-benchmark-suite
                :name name
                :description description
                :benchmarks benchmarks
                :setup-fn setup-fn
                :teardown-fn teardown-fn)))
    (setf *benchmark-suites* (map:assoc *benchmark-suites* name suite))
    suite))

(defun list-suites ()
  "List all registered benchmark suites"
  (map:keys *benchmark-suites*))

(defun get-suite (name)
  "Get a registered suite by name"
  (map:get *benchmark-suites* name))

(defun run-suite (suite-name &key (output-format :text))
  "Run all benchmarks in a suite"
  (let ((suite (get-suite suite-name)))
    (unless suite
      (error "Unknown benchmark suite: ~A" suite-name))
    
    (format t "~%Running benchmark suite: ~A~%" suite-name)
    (when (benchmark-suite-description suite)
      (format t "~A~%~%" (benchmark-suite-description suite)))
    
    ;; Run setup if provided
    (when (benchmark-suite-setup-fn suite)
      (funcall (benchmark-suite-setup-fn suite)))
    
    (unwind-protect
         (let ((results nil))
           (dolist (benchmark-name (benchmark-suite-benchmarks suite))
             (let ((benchmark-fn (benchmark:get-benchmark benchmark-name)))
               (if benchmark-fn
                   (progn
                     (format t "Running: ~A~%" benchmark-name)
                     (let ((result (benchmark:run-benchmark 
                                   benchmark-fn 
                                   :name (string benchmark-name))))
                       (push result results)))
                   (format t "Warning: Benchmark ~A not found~%" benchmark-name))))
           
           ;; Display results based on format
           (setf results (nreverse results))
           (case output-format
             (:json (benchmark:format-as-json results))
             (:csv (benchmark:format-as-csv results))
             (otherwise
              (dolist (result results)
                (benchmark:format-benchmark-result result))
              (when (> (length results) 1)
                (format t "~%")
                (benchmark:format-comparison 
                 (apply #'benchmark:compare-benchmarks results)))))
           
           results)
      
      ;; Run teardown if provided
      (when (benchmark-suite-teardown-fn suite)
        (funcall (benchmark-suite-teardown-fn suite))))))

;;; Core operations suite

(defun register-core-suite ()
  "Register core operations benchmark suite"
  
  ;; Try to load HAMT benchmarks from core module
  (handler-case
      (let ((loader (find-package "EPSILON.CORE.LOADER")))
        (when loader
          ;; Load benchmark resources for the core module
          (funcall (intern "LOAD-MODULE-RESOURCES" loader)
                   (symbol-value (intern "*ENVIRONMENT*" loader))
                   'epsilon.core
                   :benchmarks)
          ;; Now register the benchmarks after loading
          (when (find-package "EPSILON.CORE.BENCHMARKS.HAMT")
            (funcall (find-symbol "REGISTER-HAMT-BENCHMARKS-WITH-FRAMEWORK" 
                                 "EPSILON.CORE.BENCHMARKS.HAMT")))))
    (error (e)
      ;; Fall back to basic benchmarks if HAMT benchmarks aren't available
      (format t "Warning: Could not load HAMT benchmarks: ~A~%" e)
      
      ;; Define critical HAMT benchmarks inline as fallback
      (let ((map-pkg (find-package "EPSILON.MAP"))
            (set-pkg (find-package "EPSILON.SET")))
        
        (if (and map-pkg set-pkg)
            (progn
              ;; Core HAMT map benchmarks
              (benchmark:defbenchmark hamt-map-get-1000 ()
                (let ((large-map (loop with m = (symbol-value (intern "+EMPTY+" map-pkg))
                                      for i from 0 below 1000
                                      do (setf m (funcall (intern "ASSOC" map-pkg) m 
                                                         (intern (format nil "KEY~D" i)) i))
                                      finally (return m))))
                  (funcall (intern "GET" map-pkg) large-map 'key500)))
              
              (benchmark:defbenchmark hamt-set-contains-1000 ()
                (let ((large-set (loop with s = (symbol-value (intern "+EMPTY+" set-pkg))
                                      for i from 0 below 1000
                                      do (setf s (funcall (intern "ADD" set-pkg) s 
                                                         (intern (format nil "ELEM~D" i))))
                                      finally (return s))))
                  (funcall (intern "CONTAINS-P" set-pkg) large-set 'elem500)))
              
              (benchmark:defbenchmark hamt-map-build-100 ()
                (loop with m = (symbol-value (intern "+EMPTY+" map-pkg))
                      for i from 0 below 100
                      do (setf m (funcall (intern "ASSOC" map-pkg) m 
                                         (intern (format nil "KEY~D" i)) i))
                      finally (return m)))
              
              ;; Register the suite
              (define-suite core-operations
                  (:description "Core HAMT data structure benchmarks")
                hamt-map-get-1000
                hamt-set-contains-1000
                hamt-map-build-100))
            
            ;; If no HAMT packages available, minimal fallback
            (progn
              (benchmark:defbenchmark core-function-call ()
                (labels ((fib (n) 
                           (if (<= n 1) 
                               n 
                               (+ (fib (- n 1)) (fib (- n 2))))))
                  (fib 10)))
              
              (define-suite core-operations
                  (:description "Core operations benchmarks (minimal fallback)")
                core-function-call)))))))

;;; FFI suite

(defun register-ffi-suite ()
  "Register FFI benchmark suite"
  
  ;; Check if foreign module is available
  (when (find-package "EPSILON.FOREIGN")
    ;; Try to load module benchmarks if they exist
    (handler-case
        (let ((loader (find-package "EPSILON.CORE.LOADER")))
          (when loader
            ;; Load benchmark resources for the foreign module
            (funcall (intern "LOAD-MODULE-RESOURCES" loader)
                     (symbol-value (intern "*ENVIRONMENT*" loader))
                     'epsilon.foreign
                     :benchmarks)
            ;; Now register the benchmarks after loading
            (when (find-package "EPSILON.TOOL.BENCHMARK.FFI")
              (funcall (find-symbol "REGISTER-FFI-BENCHMARKS" 
                                   "EPSILON.TOOL.BENCHMARK.FFI")))))
      (error (e)
        ;; Fall back to simple FFI benchmarks
        (format t "Warning: Could not load enhanced FFI benchmarks: ~A~%" e)
        (let ((lib (find-package "EPSILON.FOREIGN")))
          
          ;; String operations via FFI
          (benchmark:defbenchmark ffi-strlen ()
            (funcall (intern "SHARED-CALL" lib)
                     '("strlen" "libc") :unsigned-long '(:string) 
                     "Hello, World!"))
          
          ;; Zero-arg FFI call
          (benchmark:defbenchmark ffi-getpid ()
            (funcall (intern "SHARED-CALL" lib)
                     '("getpid" "libc") :int nil))
          
          ;; Memory allocation
          (benchmark:defbenchmark ffi-malloc-free ()
            (let ((ptr (funcall (intern "FOREIGN-ALLOC" lib) 256)))
              (funcall (intern "FOREIGN-FREE" lib) ptr)))
          
          ;; Register the suite
          (define-suite ffi-operations
              (:description "Foreign Function Interface benchmarks")
            ffi-strlen
            ffi-getpid
            ffi-malloc-free))))))

;;; HTTP2 suite

(defun register-http2-suite ()
  "Register HTTP/2 benchmark suite"
  
  ;; Check if HTTP2 module is available
  (when (find-package "EPSILON.HTTP2")
    (let ((http2 (find-package "EPSILON.HTTP2"))
          (frames (find-package "EPSILON.HTTP2.FRAMES"))
          (hpack (find-package "EPSILON.HTTP2.HPACK")))
      
      ;; Frame creation
      (benchmark:defbenchmark http2-frame-creation ()
        (funcall (intern "MAKE-DATA-FRAME" frames) 
                 1 (make-array 1024 :initial-element 0)))
      
      ;; HPACK encoding
      (benchmark:defbenchmark http2-hpack-encode ()
        (let ((encoder (funcall (intern "MAKE-ENCODER" hpack))))
          (funcall (intern "ENCODE-HEADER-LIST" hpack)
                   encoder
                   '((":method" . "GET")
                     (":path" . "/")
                     (":scheme" . "https")))))
      
      ;; Huffman encoding
      (benchmark:defbenchmark http2-huffman-encode ()
        (funcall (intern "HUFFMAN-ENCODE" hpack) "www.example.com"))
      
      ;; Register the suite
      (define-suite http2-operations
          (:description "HTTP/2 protocol benchmarks")
        http2-frame-creation
        http2-hpack-encode
        http2-huffman-encode))))

;;; Quick benchmark suite for smoke testing

(defun register-quick-suite ()
  "Register quick benchmark suite for CI/CD smoke testing"
  
  ;; Essential HAMT operations for smoke testing
  (let ((map-pkg (find-package "EPSILON.MAP"))
        (set-pkg (find-package "EPSILON.SET")))
    
    (if (and map-pkg set-pkg)
        (progn
          ;; Quick HAMT smoke tests
          (benchmark:defbenchmark quick-map-basic ()
            (let ((m (funcall (intern "ASSOC" map-pkg) 
                             (symbol-value (intern "+EMPTY+" map-pkg)) :key "value")))
              (funcall (intern "GET" map-pkg) m :key)))
          
          (benchmark:defbenchmark quick-set-basic ()
            (let ((s (funcall (intern "ADD" set-pkg) 
                             (symbol-value (intern "+EMPTY+" set-pkg)) :element)))
              (funcall (intern "CONTAINS-P" set-pkg) s :element)))
          
          (benchmark:defbenchmark quick-map-build-10 ()
            (loop with m = (symbol-value (intern "+EMPTY+" map-pkg))
                  for i from 0 below 10
                  do (setf m (funcall (intern "ASSOC" map-pkg) m i i))
                  finally (return m)))
          
          (define-suite quick
              (:description "Quick HAMT smoke tests")
            quick-map-basic
            quick-set-basic
            quick-map-build-10))
        
        ;; Fallback if HAMT packages not available
        (progn
          (benchmark:defbenchmark quick-function-call ()
            (+ 1 2 3 4 5))
          
          (define-suite quick
              (:description "Quick smoke tests (fallback)")
            quick-function-call)))))

;;; Crypto suite

(defun register-crypto-suite ()
  "Register cryptography benchmark suite"
  
  ;; Check if crypto module is available
  (when (find-package "EPSILON.CRYPTO")
    ;; Try to load module benchmarks if they exist
    (handler-case
        (let ((loader (find-package "EPSILON.CORE.LOADER")))
          (when loader
            ;; Load benchmark resources for the crypto module
            (funcall (intern "LOAD-MODULE-RESOURCES" loader)
                     (symbol-value (intern "*ENVIRONMENT*" loader))
                     'epsilon.crypto
                     :benchmarks)
            ;; Now register the benchmarks after loading
            (when (find-package "EPSILON.TOOL.BENCHMARK.CRYPTO")
              (funcall (find-symbol "REGISTER-CRYPTO-BENCHMARKS" 
                                   "EPSILON.TOOL.BENCHMARK.CRYPTO")))))
      (error (e)
        (format t "Warning: Could not load crypto benchmarks: ~A~%" e)))))

;;; Functional Data Structures Suite

(defun register-functional-data-suite ()
  "Register functional data structures benchmark suite"
  
  ;; Try to load functional data benchmarks from core module
  (handler-case
      (let ((loader (find-package "EPSILON.CORE.LOADER")))
        (when loader
          ;; Load benchmark resources for the core module
          (funcall (intern "LOAD-MODULE-RESOURCES" loader)
                   (symbol-value (intern "*ENVIRONMENT*" loader))
                   'epsilon.core
                   :benchmarks)
          ;; Now register the benchmarks after loading
          (when (find-package "EPSILON.CORE.BENCHMARKS.HAMT")
            ;; Try the framework registration first
            (let ((register-fn (find-symbol "REGISTER-HAMT-BENCHMARKS-WITH-FRAMEWORK" 
                                           "EPSILON.CORE.BENCHMARKS.HAMT")))
              (when register-fn
                (funcall register-fn))))))
    (error (e)
      ;; Fall back to inline benchmarks if loading fails
      (format t "Warning: Could not load HAMT benchmarks from core module: ~A~%" e)
      
      (let ((map-pkg (find-package "EPSILON.MAP"))
            (set-pkg (find-package "EPSILON.SET")))
        
        (when (and map-pkg set-pkg)
          ;; Define key functional data benchmarks inline
          (benchmark:defbenchmark hamt-map-assoc-1000 ()
            (let ((large-map (loop with m = (symbol-value (intern "+EMPTY+" map-pkg))
                                  for i from 0 below 1000
                                  do (setf m (funcall (intern "ASSOC" map-pkg) m 
                                                     (intern (format nil "KEY~D" i)) i))
                                  finally (return m))))
              (funcall (intern "ASSOC" map-pkg) large-map :new-key "new")))
          
          (benchmark:defbenchmark hamt-map-get-10k ()
            (let ((xl-map (loop with m = (symbol-value (intern "+EMPTY+" map-pkg))
                               for i from 0 below 10000
                               do (setf m (funcall (intern "ASSOC" map-pkg) m 
                                                  (intern (format nil "KEY~D" i)) i))
                               finally (return m))))
              (funcall (intern "GET" map-pkg) xl-map 'key5000)))
          
          (benchmark:defbenchmark hamt-set-add-1000 ()
            (let ((large-set (loop with s = (symbol-value (intern "+EMPTY+" set-pkg))
                                  for i from 0 below 1000
                                  do (setf s (funcall (intern "ADD" set-pkg) s 
                                                     (intern (format nil "ELEM~D" i))))
                                  finally (return s))))
              (funcall (intern "ADD" set-pkg) large-set :new-element)))
          
          (benchmark:defbenchmark hamt-set-union-performance ()
            (let ((set1 (loop with s = (symbol-value (intern "+EMPTY+" set-pkg))
                             for i from 0 below 100
                             do (setf s (funcall (intern "ADD" set-pkg) s 
                                                (intern (format nil "ELEM~D" i))))
                             finally (return s)))
                  (set2 (loop with s = (symbol-value (intern "+EMPTY+" set-pkg))
                             for i from 50 below 150
                             do (setf s (funcall (intern "ADD" set-pkg) s 
                                                (intern (format nil "ELEM~D" i))))
                             finally (return s))))
              (funcall (intern "UNION" set-pkg) set1 set2)))
          
          ;; Register the suite
          (define-suite functional-data-structures
              (:description "HAMT-based functional data structures performance")
            hamt-map-assoc-1000
            hamt-map-get-10k
            hamt-set-add-1000
            hamt-set-union-performance))))))

;;; Registration function

(defun register-all-suites ()
  "Register all standard benchmark suites"
  (register-core-suite)
  (register-functional-data-suite)
  (register-ffi-suite)
  (register-http2-suite)
  (register-crypto-suite)
  (register-quick-suite)
  (format t "Registered benchmark suites: ~{~A~^, ~}~%" (list-suites)))