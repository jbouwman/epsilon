# Binary Stream Transform Proposal

## Overview

This document proposes a binary stream transform system for epsilon.

The goal is to provide efficient, composable stream processing.

## Design Principles

1. **Zero-Copy Operations**: Minimize buffer copying through direct manipulation
2. **Compile-Time Composition**: Transform chains optimize to efficient code paths
3. **Explicit Buffer Management**: No hidden allocations or garbage collection pressure
4. **Functional Composition**: Transforms compose

## Phase 1: Core Transform Protocol

### Binary Transform Interface

```lisp
(defpackage epsilon.binary.transform
  (:use cl)
  (:local-nicknames
   (binary epsilon.binary))
  (:export
   ;; Core protocol
   binary-transform
   transform-process
   transform-reset
   transform-finish
   
   ;; Transform state
   transform-input-needed-p
   transform-output-available-p
   
   ;; Chain management
   transform-chain
   make-transform-chain))

(defgeneric transform-process (transform input-buffer input-start input-end 
                                        output-buffer output-start output-end)
  (:documentation "Process data through transform.
Returns (values input-consumed output-produced finished-p error-code)
- input-consumed: bytes consumed from input buffer
- output-produced: bytes written to output buffer  
- finished-p: t if transform is complete
- error-code: nil or error indicator"))

(defgeneric transform-reset (transform)
  (:documentation "Reset transform to initial state"))

(defgeneric transform-finish (transform output-buffer output-start output-end)
  (:documentation "Finalize transform, producing any remaining output"))
```

### Base Transform Class

```lisp
(defclass binary-transform ()
  ((state :initform :ready :accessor transform-state)
   (error-info :initform nil :accessor transform-error-info))
  (:documentation "Base class for all binary transforms"))

(defmethod transform-input-needed-p ((transform binary-transform))
  "Returns t if transform needs more input data"
  (eq (transform-state transform) :need-input))

(defmethod transform-output-available-p ((transform binary-transform))
  "Returns t if transform has output data ready"
  (eq (transform-state transform) :have-output))
```

### Transform Chain Implementation

```lisp
(defclass transform-chain (binary-transform)
  ((transforms :initarg :transforms :reader chain-transforms)
   (intermediate-buffers :initform nil :accessor chain-buffers)
   (buffer-size :initarg :buffer-size :initform 65536 :reader chain-buffer-size))
  (:documentation "Chains multiple transforms together"))

(defun make-transform-chain (&rest transforms)
  "Create a transform chain from the given transforms"
  (make-instance 'transform-chain :transforms transforms))

(defmethod transform-process ((chain transform-chain) input-buf in-start in-end
                                                      output-buf out-start out-end)
  "Process data through the entire transform chain"
  (let ((transforms (chain-transforms chain))
        (buffers (ensure-intermediate-buffers chain))
        (current-input input-buf)
        (current-start in-start)
        (current-end in-end)
        (total-consumed 0)
        (final-produced 0))
    
    ;; Process through each transform in sequence
    (loop for i from 0 below (length transforms)
          for transform = (nth i transforms)
          for output-buffer = (if (= i (1- (length transforms)))
                                  output-buf  ; Final output
                                  (nth i buffers))  ; Intermediate buffer
          for output-start = (if (= i (1- (length transforms))) out-start 0)
          for output-end = (if (= i (1- (length transforms))) out-end (length output-buffer))
          do (multiple-value-bind (consumed produced finished-p error)
                 (transform-process transform current-input current-start current-end
                                              output-buffer output-start output-end)
               (when error
                 (return-from transform-process (values 0 0 nil error)))
               
               ;; Update for next iteration
               (when (zerop i) (setf total-consumed consumed))
               (when (= i (1- (length transforms))) (setf final-produced produced))
               
               ;; Set up next transform's input
               (setf current-input output-buffer
                     current-start output-start
                     current-end (+ output-start produced))))
    
    (values total-consumed final-produced nil nil)))
```

## Phase 2: Core Transforms

### Identity Transform (Testing)

```lisp
(defclass identity-transform (binary-transform)
  ()
  (:documentation "Pass-through transform for testing"))

(defmethod transform-process ((transform identity-transform) input-buf in-start in-end
                                                             output-buf out-start out-end)
  "Copy input directly to output"
  (let* ((input-available (- in-end in-start))
         (output-space (- out-end out-start))
         (copy-amount (min input-available output-space)))
    
    ;; Direct memory copy
    (replace output-buf input-buf 
             :start1 out-start :end1 (+ out-start copy-amount)
             :start2 in-start :end2 (+ in-start copy-amount))
    
    (values copy-amount copy-amount (= copy-amount input-available) nil)))
```

### Buffer Transform (Accumulation)

```lisp
(defclass buffer-transform (binary-transform)
  ((internal-buffer :initform (make-array 0 :element-type '(unsigned-byte 8) 
                                           :adjustable t :fill-pointer 0)
                    :accessor transform-buffer)
   (output-position :initform 0 :accessor buffer-output-position))
  (:documentation "Accumulates input and provides buffered output"))

(defmethod transform-process ((transform buffer-transform) input-buf in-start in-end
                                                           output-buf out-start out-end)
  "Accumulate input and provide buffered output"
  (let ((buffer (transform-buffer transform))
        (input-size (- in-end in-start))
        (output-space (- out-end out-start)))
    
    ;; Accumulate input
    (when (> input-size 0)
      (let ((old-length (length buffer)))
        (adjust-array buffer (+ old-length input-size) :fill-pointer (+ old-length input-size))
        (replace buffer input-buf 
                 :start1 old-length
                 :start2 in-start :end2 in-end)))
    
    ;; Provide output
    (let* ((available (- (length buffer) (buffer-output-position transform)))
           (output-amount (min available output-space)))
      
      (when (> output-amount 0)
        (replace output-buf buffer
                 :start1 out-start :end1 (+ out-start output-amount)
                 :start2 (buffer-output-position transform)
                 :end2 (+ (buffer-output-position transform) output-amount))
        (incf (buffer-output-position transform) output-amount))
      
      (values input-size output-amount (zerop available) nil))))
```


## Phase 3: Stream Integration

### Transform-Aware Binary Streams

```lisp
(defpackage epsilon.binary.stream
  (:use cl)
  (:local-nicknames
   (binary epsilon.binary)
   (transform epsilon.binary.transform))
  (:export
   binary-transform-stream
   make-transform-stream
   transform-stream-source
   transform-stream-transforms))

(defclass binary-transform-stream ()
  ((source :initarg :source :reader transform-stream-source)
   (transforms :initarg :transforms :reader transform-stream-transforms)
   (read-buffer :initform (make-array 65536 :element-type '(unsigned-byte 8))
                :accessor stream-read-buffer)
   (read-position :initform 0 :accessor stream-read-position)
   (read-limit :initform 0 :accessor stream-read-limit)
   (eof-p :initform nil :accessor stream-eof-p))
  (:documentation "Binary stream with transform processing"))

(defun make-transform-stream (source &rest transforms)
  "Create a transform stream from source and transforms"
  (make-instance 'binary-transform-stream
    :source source
    :transforms (apply #'transform:make-transform-chain transforms)))
```

### Buffered Reading Implementation

```lisp
(defmethod ensure-buffer-data ((stream binary-transform-stream) minimum-bytes)
  "Ensure at least minimum-bytes are available in read buffer"
  (let ((available (- (stream-read-limit stream) (stream-read-position stream))))
    (when (and (< available minimum-bytes) (not (stream-eof-p stream)))
      ;; Compact buffer if needed
      (when (> (stream-read-position stream) 0)
        (replace (stream-read-buffer stream) (stream-read-buffer stream)
                 :start2 (stream-read-position stream) :end2 (stream-read-limit stream))
        (decf (stream-read-limit stream) (stream-read-position stream))
        (setf (stream-read-position stream) 0))
      
      ;; Read and transform more data
      (let ((source-data (make-array 32768 :element-type '(unsigned-byte 8)))
            (transforms (transform-stream-transforms stream)))
        (loop
          (let ((bytes-read (read-source-data (transform-stream-source stream) source-data)))
            (when (zerop bytes-read)
              (setf (stream-eof-p stream) t)
              (return))
            
            ;; Transform the data
            (multiple-value-bind (consumed produced finished-p error)
                (transform:transform-process transforms source-data 0 bytes-read
                                                      (stream-read-buffer stream)
                                                      (stream-read-limit stream)
                                                      (length (stream-read-buffer stream)))
              (when error
                (error "Transform error: ~A" error))
              
              (incf (stream-read-limit stream) produced)
              
              ;; Check if we have enough data now
              (when (>= (- (stream-read-limit stream) (stream-read-position stream))
                        minimum-bytes)
                (return))
              
              (when finished-p
                (setf (stream-eof-p stream) t)
                (return))))))))

(defmethod binary:read ((stream binary-transform-stream) type &optional (endian binary:*default-endian*))
  "Read a value of the specified type from the transform stream"
  (let ((size (binary:size type)))
    (ensure-buffer-data stream size)
    (let ((available (- (stream-read-limit stream) (stream-read-position stream))))
      (when (< available size)
        (error "Unexpected end of stream"))
      
      (prog1
          (binary:from-bytes (stream-read-buffer stream) type endian (stream-read-position stream))
        (incf (stream-read-position stream) size)))))

(defmethod binary:read-bytes ((stream binary-transform-stream) count)
  "Read count bytes from the transform stream"
  (ensure-buffer-data stream count)
  (let ((available (- (stream-read-limit stream) (stream-read-position stream)))
        (result (make-array count :element-type '(unsigned-byte 8))))
    (when (< available count)
      (error "Unexpected end of stream"))
    
    (replace result (stream-read-buffer stream)
             :start2 (stream-read-position stream)
             :end2 (+ (stream-read-position stream) count))
    (incf (stream-read-position stream) count)
    result))
```

## Phase 4: High-Level Integration

### ZIP Archive with Compression

```lisp
(defpackage epsilon.zip.compressed
  (:use cl)
  (:local-nicknames
   (zip epsilon.zip)
   (binary epsilon.binary)
   (stream epsilon.binary.stream)
   (deflate epsilon.binary.transform.deflate))
  (:export
   read-compressed-zip-entry
   write-compressed-zip-entry))

(defun read-compressed-zip-entry (zip-data entry)
  "Read and decompress a ZIP entry"
  (let ((compression-method (zip:zip-entry-compression-method entry)))
    (case compression-method
      (0 ; Stored - no compression
       (zip:zip-entry-data entry))
      (8 ; Deflate compression
       (let* ((compressed-data (zip:zip-entry-data entry))
              (decoder (deflate:deflate-decoder))
              (decompressed (make-array (zip:zip-entry-uncompressed-size entry)
                                        :element-type '(unsigned-byte 8))))
         (multiple-value-bind (consumed produced finished-p error)
             (transform:transform-process decoder 
                                          compressed-data 0 (length compressed-data)
                                          decompressed 0 (length decompressed))
           (declare (ignore consumed))
           (when error
             (error "Decompression failed: ~A" error))
           (when (not finished-p)
             (error "Incomplete decompression"))
           decompressed)))
      (t
       (error "Unsupported compression method: ~A" compression-method)))))
```

### Usage Examples

```lisp
;;; Reading compressed ZIP files
(let* ((zip-bytes (read-file-bytes "archive.zip"))
       (zip-file (zip:read-zip-file zip-bytes)))
  (dolist (entry (zip:zip-file-entries zip-file))
    (let ((decompressed-data (read-compressed-zip-entry zip-bytes entry)))
      (format t "Entry ~A: ~A bytes~%" 
              (zip:zip-entry-name entry)
              (length decompressed-data)))))

;;; Streaming decompression
(let ((compressed-stream (make-transform-stream 
                           (open "data.deflate" :element-type '(unsigned-byte 8))
                           (deflate:deflate-decoder))))
  ;; Read binary data through decompression
  (let ((magic (binary:read compressed-stream :u32))
        (version (binary:read compressed-stream :u16)))
    (format t "Magic: ~X, Version: ~D~%" magic version)))

;;; Transform composition
(let ((transform-chain 
        (transform:make-transform-chain
          (deflate:deflate-decoder)
          (make-instance 'crc32-validator)
          (make-instance 'base64-decoder))))
  ;; Process through multiple transforms efficiently
  (transform:transform-process transform-chain input-data 0 (length input-data)
                                              output-buffer 0 (length output-buffer)))
```

## Performance Characteristics

### Expected Performance Benefits

1. **Zero-Copy Chains**: Transform chains operate on shared buffers
2. **Vectorized Operations**: Bulk operations minimize per-byte overhead
3. **Compile-Time Optimization**: Transform chains can be specialized

### Benchmarking Framework

```lisp
(defpackage epsilon.binary.transform.benchmark
  (:use cl)
  (:local-nicknames
   (transform epsilon.binary.transform)
   (deflate epsilon.binary.transform.deflate)
   (benchmark epsilon.tool.benchmark))
  (:export
   benchmark-transform-throughput
   benchmark-compression-ratios))

(defun benchmark-transform-throughput ()
  "Benchmark transform processing throughput"
  (let ((test-data (make-array 1048576 :element-type '(unsigned-byte 8) 
                               :initial-contents (loop for i below 1048576 collect (mod i 256))))
        (output-buffer (make-array 2097152 :element-type '(unsigned-byte 8))))
    
    ;; Benchmark raw deflate compression
    (benchmark:with-timing (:iterations 100)
      (let ((encoder (deflate:deflate-encoder)))
        (transform:transform-process encoder test-data 0 (length test-data)
                                              output-buffer 0 (length output-buffer))))))
```
