;;;; digest benchmarks
;;;;
;;;; Performance benchmarks for epsilon.digest hash algorithms.
;;;; Measures throughput at various data sizes.

(defpackage epsilon.digest.benchmarks
  (:use cl)
  (:local-nicknames
   (d2 epsilon.digest)
   (blake3 epsilon.digest.blake3)
   (xxhash epsilon.digest.xxhash3)
   (ssl epsilon.ssl))
  (:export
   run-all-benchmarks
   run-blake3-benchmarks
   run-xxhash-benchmarks
   run-comparison-benchmarks)
  (:enter t))

;;; ============================================================================
;;; Benchmark Infrastructure
;;; ============================================================================

(defun make-test-data (size)
  "Create test data of SIZE bytes."
  (let ((data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref data i) (mod i 256)))
    data))

(defun run-benchmark (name iterations fn data)
  "Run a benchmark and report timing."
  (format t "~%~A (~:D bytes, ~:D iterations)~%" name (length data) iterations)
  (force-output)

  ;; Warm up
  (dotimes (i (min 100 iterations))
    (funcall fn data))

  ;; Actual benchmark
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall fn data))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (total-bytes (* iterations (length data)))
           (throughput-mbps (/ total-bytes elapsed 1024 1024)))
      (format t "  Time: ~,3F seconds~%" elapsed)
      (format t "  Throughput: ~,2F MB/s~%" throughput-mbps)
      throughput-mbps)))

(defun format-size (bytes)
  "Format byte size for display."
  (cond
    ((>= bytes (* 1024 1024)) (format nil "~DMB" (/ bytes 1024 1024)))
    ((>= bytes 1024) (format nil "~DKB" (/ bytes 1024)))
    (t (format nil "~DB" bytes))))

;;; ============================================================================
;;; BLAKE3 Benchmarks
;;; ============================================================================

(defun run-blake3-benchmarks ()
  "Run BLAKE3 benchmarks at various sizes."
  (format t "~%========================================~%")
  (format t "     BLAKE3 Benchmarks~%")
  (format t "========================================~%")

  (let ((sizes '((64 100000)        ; 64B, 100K iterations
                 (256 50000)        ; 256B, 50K iterations
                 (1024 20000)       ; 1KB, 20K iterations
                 (4096 10000)       ; 4KB, 10K iterations
                 (16384 5000)       ; 16KB, 5K iterations
                 (65536 2000)       ; 64KB, 2K iterations
                 (262144 500)       ; 256KB, 500 iterations
                 (1048576 100))))   ; 1MB, 100 iterations
    (dolist (spec sizes)
      (destructuring-bind (size iterations) spec
        (let ((data (make-test-data size)))
          (run-benchmark (format nil "blake3-~A" (format-size size))
                         iterations
                         #'blake3:blake3
                         data))))))

(defun run-blake3-mode-benchmarks ()
  "Compare BLAKE3 modes (hash, keyed, derive)."
  (format t "~%========================================~%")
  (format t "     BLAKE3 Mode Comparison (4KB)~%")
  (format t "========================================~%")

  (let* ((size 4096)
         (iterations 10000)
         (data (make-test-data size))
         (key (make-test-data 32)))

    (run-benchmark "blake3-hash" iterations #'blake3:blake3 data)

    (run-benchmark "blake3-keyed" iterations
                   (lambda (d) (blake3:blake3-keyed key d))
                   data)

    (run-benchmark "blake3-derive" iterations
                   (lambda (d) (blake3:blake3-derive-key "benchmark" d))
                   data)))

(defun run-blake3-output-length-benchmarks ()
  "Benchmark BLAKE3 with different output lengths."
  (format t "~%========================================~%")
  (format t "     BLAKE3 Output Length (4KB input)~%")
  (format t "========================================~%")

  (let* ((size 4096)
         (iterations 10000)
         (data (make-test-data size)))

    (dolist (output-len '(32 64 128 256))
      (run-benchmark (format nil "blake3-out~D" output-len)
                     iterations
                     (lambda (d) (blake3:blake3 d :output-length output-len))
                     data))))

;;; ============================================================================
;;; xxHash Benchmarks
;;; ============================================================================

(defun run-xxhash-benchmarks ()
  "Run xxHash benchmarks at various sizes."
  (format t "~%========================================~%")
  (format t "     xxHash3 64-bit Benchmarks~%")
  (format t "========================================~%")

  (let ((sizes '((64 500000)        ; 64B, 500K iterations
                 (256 200000)       ; 256B, 200K iterations
                 (1024 100000)      ; 1KB, 100K iterations
                 (4096 50000)       ; 4KB, 50K iterations
                 (16384 20000)      ; 16KB, 20K iterations
                 (65536 5000)       ; 64KB, 5K iterations
                 (262144 2000)      ; 256KB, 2K iterations
                 (1048576 500))))   ; 1MB, 500 iterations
    (dolist (spec sizes)
      (destructuring-bind (size iterations) spec
        (let ((data (make-test-data size)))
          (run-benchmark (format nil "xxhash64-~A" (format-size size))
                         iterations
                         #'xxhash:xxhash64
                         data)))))

  (format t "~%========================================~%")
  (format t "     xxHash3 128-bit Benchmarks~%")
  (format t "========================================~%")

  (let ((sizes '((1024 50000)
                 (4096 20000)
                 (65536 2000)
                 (1048576 200))))
    (dolist (spec sizes)
      (destructuring-bind (size iterations) spec
        (let ((data (make-test-data size)))
          (run-benchmark (format nil "xxhash128-~A" (format-size size))
                         iterations
                         #'xxhash:xxhash128
                         data))))))

;;; ============================================================================
;;; Streaming vs One-Shot Benchmarks
;;; ============================================================================

(defun run-streaming-benchmarks ()
  "Compare streaming vs one-shot performance."
  (format t "~%========================================~%")
  (format t "     Streaming vs One-Shot (1MB)~%")
  (format t "========================================~%")

  (let* ((size (* 1024 1024))
         (iterations 50)
         (data (make-test-data size))
         (chunk-size 65536))

    ;; One-shot
    (run-benchmark "blake3-oneshot-1MB" iterations
                   #'blake3:blake3 data)

    ;; Streaming with 64KB chunks
    (run-benchmark "blake3-streaming-64KB-chunks" iterations
                   (lambda (d)
                     (let ((h (blake3:make-blake3-hasher)))
                       (loop for start from 0 below (length d) by chunk-size
                             for end = (min (+ start chunk-size) (length d))
                             do (d2:hasher-update h d :start start :end end))
                       (d2:hasher-finalize h)))
                   data)

    (run-benchmark "xxhash64-oneshot-1MB" iterations
                   #'xxhash:xxhash64 data)

    (run-benchmark "xxhash64-streaming-64KB-chunks" iterations
                   (lambda (d)
                     (let ((h (xxhash:make-xxhash64-hasher)))
                       (loop for start from 0 below (length d) by chunk-size
                             for end = (min (+ start chunk-size) (length d))
                             do (d2:hasher-update h d :start start :end end))
                       (d2:hasher-finalize h)))
                   data)))

;;; ============================================================================
;;; Comparison with epsilon.digest
;;; ============================================================================

(defun run-comparison-benchmarks ()
  "Compare different hash algorithms in digest."
  (format t "~%========================================~%")
  (format t "     Algorithm Comparison~%")
  (format t "========================================~%")

  (let ((sizes '((1024 20000)
                 (65536 1000)
                 (1048576 50))))
    (dolist (spec sizes)
      (destructuring-bind (size iterations) spec
        (let ((data (make-test-data size)))
          (format t "~%--- ~A ---~%" (format-size size))

          ;; OpenSSL SHA-256
          (run-benchmark (format nil "sha256-~A" (format-size size))
                         iterations
                         #'ssl:sha256
                         data)

          ;; BLAKE3
          (run-benchmark (format nil "blake3-~A" (format-size size))
                         iterations
                         #'blake3:blake3
                         data)

          ;; xxHash64
          (run-benchmark (format nil "xxhash64-~A" (format-size size))
                         iterations
                         #'xxhash:xxhash64
                         data))))))

;;; ============================================================================
;;; Small Input Benchmarks
;;; ============================================================================

(defun run-small-input-benchmarks ()
  "Benchmark performance on small inputs (latency-sensitive)."
  (format t "~%========================================~%")
  (format t "     Small Input Benchmarks~%")
  (format t "========================================~%")

  (let ((iterations 100000))
    (dolist (size '(1 4 8 16 32 64))
      (let ((data (make-test-data size)))
        (format t "~%--- ~D bytes ---~%" size)

        (run-benchmark "blake3" iterations #'blake3:blake3 data)

        (run-benchmark "xxhash64" iterations #'xxhash:xxhash64 data)

        (run-benchmark "sha256" iterations #'ssl:sha256 data)))))

;;; ============================================================================
;;; Main Entry Points
;;; ============================================================================

(defun run-all-benchmarks ()
  "Run all digest benchmarks."
  (format t "~%========================================~%")
  (format t "     epsilon.digest Benchmarks~%")
  (format t "========================================~%")

  (run-blake3-benchmarks)
  (run-blake3-mode-benchmarks)
  (run-blake3-output-length-benchmarks)
  (run-xxhash-benchmarks)
  (run-streaming-benchmarks)
  (run-small-input-benchmarks)
  (run-comparison-benchmarks)

  (format t "~%Benchmark complete.~%"))

(defun quick-benchmark ()
  "Run a quick benchmark for sanity checking."
  (let* ((size 65536)
         (iterations 1000)
         (data (make-test-data size)))

    (format t "~%Quick benchmark: 64KB, 1000 iterations~%")
    (format t "========================================~%")

    (run-benchmark "blake3" iterations #'blake3:blake3 data)
    (run-benchmark "xxhash64" iterations #'xxhash:xxhash64 data)
    (run-benchmark "sha256" iterations #'ssl:sha256 data)))
