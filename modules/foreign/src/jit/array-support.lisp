;;;; array-support.lisp - Array/Buffer passing support for JIT FFI
;;;;
;;;; Provides utilities for passing Lisp arrays to C functions:
;;;; - Array pinning macros for safe pointer access
;;;; - C-compatible array allocation
;;;; - Type mapping between Lisp and C array element types
;;;; - Output array handling for receiving C results

(defpackage epsilon.foreign.jit.array
  (:use cl)
  (:local-nicknames
   (jit epsilon.foreign.jit))
  (:export
   ;; Array pinning
   #:with-pinned-array
   #:with-pinned-arrays
   #:with-output-array

   ;; C-compatible arrays
   #:make-c-array
   #:c-array-pointer
   #:c-array-length

   ;; Type mapping
   #:c-type-to-lisp-element-type
   #:lisp-element-type-to-c-type
   #:element-size

   ;; Utilities
   #:copy-to-c-array
   #:copy-from-c-array
   #:zero-c-array)
  (:enter t))

;;; ============================================================================
;;; Type Mapping
;;; ============================================================================

(defparameter *c-to-lisp-element-types*
  '((:char . (signed-byte 8))
    (:uchar . (unsigned-byte 8))
    (:unsigned-char . (unsigned-byte 8))
    (:short . (signed-byte 16))
    (:ushort . (unsigned-byte 16))
    (:unsigned-short . (unsigned-byte 16))
    (:int . (signed-byte 32))
    (:uint . (unsigned-byte 32))
    (:unsigned-int . (unsigned-byte 32))
    (:long . (signed-byte 64))
    (:ulong . (unsigned-byte 64))
    (:unsigned-long . (unsigned-byte 64))
    (:float . single-float)
    (:double . double-float)
    (:pointer . (unsigned-byte 64)))
  "Mapping from C type keywords to Lisp array element types")

(defparameter *lisp-to-c-element-types*
  '(((signed-byte 8) . :char)
    ((unsigned-byte 8) . :uchar)
    ((signed-byte 16) . :short)
    ((unsigned-byte 16) . :ushort)
    ((signed-byte 32) . :int)
    ((unsigned-byte 32) . :uint)
    ((signed-byte 64) . :long)
    ((unsigned-byte 64) . :ulong)
    (single-float . :float)
    (double-float . :double))
  "Mapping from Lisp array element types to C type keywords")

(defparameter *element-sizes*
  '((:char . 1) (:uchar . 1) (:unsigned-char . 1)
    (:short . 2) (:ushort . 2) (:unsigned-short . 2)
    (:int . 4) (:uint . 4) (:unsigned-int . 4)
    (:long . 8) (:ulong . 8) (:unsigned-long . 8)
    (:float . 4) (:double . 8)
    (:pointer . 8))
  "Size in bytes for each C element type")

(defun c-type-to-lisp-element-type (c-type)
  "Convert C type keyword to Lisp array element type."
  (or (cdr (assoc c-type *c-to-lisp-element-types*))
      (error "Unknown C type: ~A" c-type)))

(defun lisp-element-type-to-c-type (lisp-type)
  "Convert Lisp array element type to C type keyword."
  (or (cdr (assoc lisp-type *lisp-to-c-element-types* :test #'equal))
      ;; Try to match upgaded types
      (cdr (assoc (upgraded-array-element-type lisp-type)
                  *lisp-to-c-element-types* :test #'equal))
      (error "Cannot map Lisp type to C: ~A" lisp-type)))

(defun element-size (c-type)
  "Get size in bytes for a C element type."
  (or (cdr (assoc c-type *element-sizes*))
      (error "Unknown C type: ~A" c-type)))

;;; ============================================================================
;;; Array Pinning
;;; ============================================================================

(defun coerce-to-simple-array (array element-type)
  "Coerce array to a simple array with the specified element type.
   Returns the array if already suitable, or a copy otherwise."
  (let ((target-type (if (keywordp element-type)
                         (c-type-to-lisp-element-type element-type)
                         element-type)))
    (if (and (typep array `(simple-array ,target-type (*)))
             (array-element-type array))
        array
        ;; Need to create a new array with correct element type
        (let ((new-array (make-array (length array)
                                     :element-type target-type)))
          (dotimes (i (length array))
            (setf (aref new-array i) (aref array i)))
          new-array))))

(defmacro with-pinned-array ((ptr-var array &key (element-type nil element-type-p))
                             &body body)
  "Pin a Lisp array and bind PTR-VAR to its System Area Pointer.

   The array is guaranteed to not move during BODY execution.
   PTR-VAR is bound to an integer address suitable for FFI calls.

   Parameters:
   - PTR-VAR: Variable to bind the pointer to
   - ARRAY: The Lisp array to pin
   - ELEMENT-TYPE: C type keyword (e.g., :double) for type coercion

   Example:
   (with-pinned-array (ptr my-doubles :element-type :double)
     (c-process-doubles ptr (length my-doubles)))"
  (let ((arr-var (gensym "ARRAY"))
        (coerced-var (gensym "COERCED")))
    `(let* ((,arr-var ,array)
            (,coerced-var ,(if element-type-p
                               `(coerce-to-simple-array ,arr-var ,element-type)
                               arr-var)))
       (sb-sys:with-pinned-objects (,coerced-var)
         (let ((,ptr-var (sb-sys:sap-int (sb-sys:vector-sap ,coerced-var))))
           ,@body)))))

(defmacro with-pinned-arrays (bindings &body body)
  "Pin multiple arrays simultaneously.

   BINDINGS is a list of (ptr-var array &key element-type) forms.

   Example:
   (with-pinned-arrays ((ptr1 array1 :element-type :double)
                        (ptr2 array2 :element-type :int))
     (c-function ptr1 ptr2 (length array1)))"
  (if (null bindings)
      `(progn ,@body)
      `(with-pinned-array ,(first bindings)
         (with-pinned-arrays ,(rest bindings)
           ,@body))))

(defmacro with-output-array ((ptr-var array-var count &key (element-type :double)
                                                           (initial-element 0))
                             &body body)
  "Create an output array, pass pointer to C, then access results.

   Creates a new array of COUNT elements, pins it, runs BODY with the
   pointer bound to PTR-VAR, then binds ARRAY-VAR to the array for
   accessing results after BODY completes.

   Example:
   (with-output-array (ptr results 100 :element-type :double)
     (c-compute-values ptr 100)
     ;; results now contains computed values
     (reduce #'+ results))"
  (let ((arr-var (gensym "OUTPUT-ARRAY")))
    `(let ((,arr-var (make-c-array ,element-type ,count
                                   :initial-element ,initial-element)))
       (with-pinned-array (,ptr-var ,arr-var :element-type ,element-type)
         (let ((,array-var ,arr-var))
           ,@body)))))

;;; ============================================================================
;;; C-Compatible Array Allocation
;;; ============================================================================

(defun make-c-array (element-type count &key initial-element)
  "Allocate a C-compatible Lisp array.

   The array is allocated with a specialized element type that matches
   C memory layout, making it suitable for pinning and passing to FFI.

   Parameters:
   - ELEMENT-TYPE: C type keyword (:double, :int, etc.)
   - COUNT: Number of elements
   - INITIAL-ELEMENT: Optional initial value for all elements

   Returns: A simple-array with appropriate element type

   Example:
   (make-c-array :double 100 :initial-element 0.0d0)"
  (let ((lisp-type (c-type-to-lisp-element-type element-type)))
    (if initial-element
        (make-array count
                    :element-type lisp-type
                    :initial-element (coerce initial-element lisp-type))
        (make-array count :element-type lisp-type))))

(defun c-array-pointer (array)
  "Get a pointer to the array's data.

   WARNING: This pointer is only valid while the array is pinned.
   Use with-pinned-array for safe access.

   Returns: Integer address of array data"
  (sb-sys:sap-int (sb-sys:vector-sap array)))

(defun c-array-length (array)
  "Get the length of a C-compatible array."
  (length array))

;;; ============================================================================
;;; Array Copy Utilities
;;; ============================================================================

(defun copy-to-c-array (source dest &key (start 0) (end nil))
  "Copy elements from SOURCE to C-compatible DEST array.

   Parameters:
   - SOURCE: Source sequence
   - DEST: Destination C-compatible array
   - START: Starting index in source (default 0)
   - END: Ending index in source (default: length of source)"
  (let ((actual-end (or end (length source))))
    (loop for i from start below actual-end
          for j from 0
          do (setf (aref dest j) (elt source i)))
    dest))

(defun copy-from-c-array (source dest &key (start 0) (end nil))
  "Copy elements from C-compatible SOURCE array to DEST.

   Parameters:
   - SOURCE: Source C-compatible array
   - DEST: Destination sequence
   - START: Starting index in source (default 0)
   - END: Ending index in source (default: length of source)"
  (let ((actual-end (or end (length source))))
    (loop for i from start below actual-end
          for j from 0
          do (setf (elt dest j) (aref source i)))
    dest))

(defun zero-c-array (array)
  "Zero out a C-compatible array.

   Parameters:
   - ARRAY: The array to zero

   Returns: The array (modified in place)"
  (let ((element-type (array-element-type array)))
    (cond
      ((subtypep element-type 'float)
       (fill array (coerce 0 element-type)))
      ((subtypep element-type 'integer)
       (fill array 0))
      (t (fill array 0))))
  array)

;;; ============================================================================
;;; SAP-based Memory Access (for raw pointer access)
;;; ============================================================================

(defun read-c-array-element (ptr index element-type)
  "Read an element from a C array through a pointer.

   Parameters:
   - PTR: Integer pointer to array start
   - INDEX: Element index
   - ELEMENT-TYPE: C type keyword

   Returns: The element value"
  (let ((sap (sb-sys:int-sap ptr))
        (offset (* index (element-size element-type))))
    (case element-type
      ((:char)
       (sb-sys:signed-sap-ref-8 sap offset))
      ((:uchar :unsigned-char)
       (sb-sys:sap-ref-8 sap offset))
      ((:short)
       (sb-sys:signed-sap-ref-16 sap offset))
      ((:ushort :unsigned-short)
       (sb-sys:sap-ref-16 sap offset))
      ((:int)
       (sb-sys:signed-sap-ref-32 sap offset))
      ((:uint :unsigned-int)
       (sb-sys:sap-ref-32 sap offset))
      ((:long)
       (sb-sys:signed-sap-ref-64 sap offset))
      ((:ulong :unsigned-long :pointer)
       (sb-sys:sap-ref-64 sap offset))
      ((:float)
       (sb-sys:sap-ref-single sap offset))
      ((:double)
       (sb-sys:sap-ref-double sap offset))
      (otherwise
       (error "Unknown element type: ~A" element-type)))))

(defun write-c-array-element (ptr index element-type value)
  "Write an element to a C array through a pointer.

   Parameters:
   - PTR: Integer pointer to array start
   - INDEX: Element index
   - ELEMENT-TYPE: C type keyword
   - VALUE: Value to write"
  (let ((sap (sb-sys:int-sap ptr))
        (offset (* index (element-size element-type))))
    (case element-type
      ((:char)
       (setf (sb-sys:signed-sap-ref-8 sap offset) value))
      ((:uchar :unsigned-char)
       (setf (sb-sys:sap-ref-8 sap offset) value))
      ((:short)
       (setf (sb-sys:signed-sap-ref-16 sap offset) value))
      ((:ushort :unsigned-short)
       (setf (sb-sys:sap-ref-16 sap offset) value))
      ((:int)
       (setf (sb-sys:signed-sap-ref-32 sap offset) value))
      ((:uint :unsigned-int)
       (setf (sb-sys:sap-ref-32 sap offset) value))
      ((:long)
       (setf (sb-sys:signed-sap-ref-64 sap offset) value))
      ((:ulong :unsigned-long :pointer)
       (setf (sb-sys:sap-ref-64 sap offset) value))
      ((:float)
       (setf (sb-sys:sap-ref-single sap offset) (float value 1.0)))
      ((:double)
       (setf (sb-sys:sap-ref-double sap offset) (float value 1.0d0)))
      (otherwise
       (error "Unknown element type: ~A" element-type))))
  value)

(defsetf read-c-array-element write-c-array-element)
