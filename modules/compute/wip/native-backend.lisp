;;;; Native Backend Implementation using OpenBLAS
;;;;
;;;; High-level interface to BLAS/LAPACK operations with memory management

(defpackage epsilon.compute.native-backend
  (:use :cl)
  (:local-nicknames
   (#:ffi #:epsilon.compute.blas-ffi)
   (#:foreign #:epsilon.foreign))
  (:export
   ;; Memory management
   #:with-pinned-array
   #:allocate-double-array
   #:free-double-array
   #:copy-to-foreign
   #:copy-from-foreign
   
   ;; Matrix/vector types
   #:native-array
   #:make-native-array
   #:native-array-data
   #:native-array-dimensions
   #:native-array-size
   #:native-array-ref
   #:native-array-set
   
   ;; BLAS Level 1
   #:blas-dot
   #:blas-norm
   #:blas-asum
   #:blas-axpy!
   #:blas-scal!
   #:blas-copy!
   #:blas-swap!
   
   ;; BLAS Level 2
   #:blas-gemv!
   #:blas-ger!
   
   ;; BLAS Level 3
   #:blas-gemm!
   #:blas-symm!
   
   ;; LAPACK
   #:lapack-lu!
   #:lapack-solve-lu!
   #:lapack-solve!
   #:lapack-eigen!
   #:lapack-svd!
   #:lapack-cholesky!
   #:lapack-solve-cholesky!
   
   ;; High-level operations
   #:matrix-multiply
   #:matrix-vector-multiply
   #:solve-linear-system
   #:matrix-inverse
   #:eigenvalues
   #:singular-values))

(in-package epsilon.compute.native-backend)

;;; Native array structure for efficient memory management

(defstruct native-array
  "Container for foreign memory with array metadata"
  (data nil :type (or null lib:foreign-pointer))
  (dimensions nil :type list)
  (size 0 :type fixnum)
  (owner-p t :type boolean)) ; whether we own the memory

(defun make-native-array (dimensions &key (initial-element 0.0d0) (element-type 'double-float))
  "Create a native array with given dimensions"
  (declare (ignore element-type)) ; for now, only support double-float
  (let* ((size (reduce #'* dimensions))
         (data (lib:foreign-alloc :double :count size)))
    ;; Initialize array
    (dotimes (i size)
      (setf (lib:mem-aref data :double i) (coerce initial-element 'double-float)))
    (make-native-array :data data
                       :dimensions dimensions
                       :size size
                       :owner-p t)))

(defun free-native-array (array)
  "Free memory associated with native array"
  (when (and (native-array-owner-p array)
             (native-array-data array))
    (lib:foreign-free (native-array-data array))
    (setf (native-array-data array) nil)))

(defun native-array-rank (array)
  "Return the rank (number of dimensions) of the array"
  (length (native-array-dimensions array)))

(defun native-array-ref (array &rest indices)
  "Get element from native array"
  (let ((index (apply #'array-row-major-index 
                     (native-array-dimensions array) 
                     indices)))
    (lib:mem-aref (native-array-data array) :double index)))

(defun (setf native-array-ref) (value array &rest indices)
  "Set element in native array"
  (let ((index (apply #'array-row-major-index
                     (native-array-dimensions array)
                     indices)))
    (setf (lib:mem-aref (native-array-data array) :double index)
          (coerce value 'double-float))))

(defun array-row-major-index (dimensions &rest indices)
  "Calculate row-major index for multi-dimensional array"
  (let ((index 0)
        (stride 1))
    (loop for i from (1- (length dimensions)) downto 0
          for idx in (reverse indices)
          do (incf index (* idx stride))
             (setf stride (* stride (nth i dimensions))))
    index))

;;; Memory management utilities

(defmacro with-foreign-doubles ((var count) &body body)
  "Allocate temporary foreign double array"
  `(let ((,var (lib:foreign-alloc :double :count ,count)))
     (unwind-protect
          (progn ,@body)
       (lib:foreign-free ,var))))

(defmacro with-foreign-ints ((var count) &body body)
  "Allocate temporary foreign int array"
  `(let ((,var (lib:foreign-alloc :int :count ,count)))
     (unwind-protect
          (progn ,@body)
       (lib:foreign-free ,var))))

(defmacro with-foreign-scalar ((var type value) &body body)
  "Allocate foreign scalar with initial value"
  `(lib:with-foreign-object (,var ,type)
     (setf (lib:mem-ref ,var ,type) ,value)
     ,@body))

(defun copy-to-foreign (lisp-array foreign-ptr)
  "Copy Lisp array to foreign memory"
  (let ((size (array-total-size lisp-array)))
    (dotimes (i size)
      (setf (lib:mem-aref foreign-ptr :double i)
            (coerce (row-major-aref lisp-array i) 'double-float)))))

(defun copy-from-foreign (foreign-ptr lisp-array)
  "Copy foreign memory to Lisp array"
  (let ((size (array-total-size lisp-array)))
    (dotimes (i size)
      (setf (row-major-aref lisp-array i)
            (lib:mem-aref foreign-ptr :double i)))))

;;; BLAS Level 1 Operations

(defun blas-dot (x y)
  "Compute dot product of vectors x and y"
  (assert (= (native-array-size x) (native-array-size y)))
  (ffi:%cblas-ddot (native-array-size x)
                   (native-array-data x) 1
                   (native-array-data y) 1))

(defun blas-norm (x)
  "Compute Euclidean norm of vector x"
  (ffi:%cblas-dnrm2 (native-array-size x)
                    (native-array-data x) 1))

(defun blas-asum (x)
  "Compute sum of absolute values of vector x"
  (ffi:%cblas-dasum (native-array-size x)
                    (native-array-data x) 1))

(defun blas-axpy! (alpha x y)
  "Compute y = alpha*x + y (destructive)"
  (assert (= (native-array-size x) (native-array-size y)))
  (ffi:%cblas-daxpy (native-array-size x)
                    (coerce alpha 'double-float)
                    (native-array-data x) 1
                    (native-array-data y) 1)
  y)

(defun blas-scal! (alpha x)
  "Compute x = alpha*x (destructive)"
  (ffi:%cblas-dscal (native-array-size x)
                    (coerce alpha 'double-float)
                    (native-array-data x) 1)
  x)

(defun blas-copy! (x y)
  "Copy vector x to y (destructive)"
  (assert (= (native-array-size x) (native-array-size y)))
  (ffi:%cblas-dcopy (native-array-size x)
                    (native-array-data x) 1
                    (native-array-data y) 1)
  y)

;;; BLAS Level 2 Operations

(defun blas-gemv! (alpha a x beta y &key (trans :no-trans))
  "Matrix-vector multiply: y = alpha*A*x + beta*y (destructive)"
  (let* ((dims-a (native-array-dimensions a))
         (m (first dims-a))
         (n (second dims-a))
         (trans-flag (ecase trans
                      (:no-trans ffi:+cblas-no-trans+)
                      (:trans ffi:+cblas-trans+))))
    (ffi:%cblas-dgemv ffi:+cblas-col-major+
                      trans-flag
                      m n
                      (coerce alpha 'double-float)
                      (native-array-data a) m
                      (native-array-data x) 1
                      (coerce beta 'double-float)
                      (native-array-data y) 1)
    y))

;;; BLAS Level 3 Operations

(defun blas-gemm! (alpha a b beta c &key (trans-a :no-trans) (trans-b :no-trans))
  "Matrix-matrix multiply: C = alpha*A*B + beta*C (destructive)"
  (let* ((dims-a (native-array-dimensions a))
         (dims-b (native-array-dimensions b))
         (dims-c (native-array-dimensions c))
         (m (first dims-c))
         (n (second dims-c))
         (k (if (eq trans-a :no-trans) (second dims-a) (first dims-a)))
         (trans-a-flag (ecase trans-a
                        (:no-trans ffi:+cblas-no-trans+)
                        (:trans ffi:+cblas-trans+)))
         (trans-b-flag (ecase trans-b
                        (:no-trans ffi:+cblas-no-trans+)
                        (:trans ffi:+cblas-trans+))))
    (ffi:%cblas-dgemm ffi:+cblas-col-major+
                      trans-a-flag trans-b-flag
                      m n k
                      (coerce alpha 'double-float)
                      (native-array-data a) (first dims-a)
                      (native-array-data b) (first dims-b)
                      (coerce beta 'double-float)
                      (native-array-data c) m)
    c))

;;; LAPACK Operations

(defun lapack-lu! (a)
  "Compute LU decomposition of matrix A (destructive)"
  (let* ((dims (native-array-dimensions a))
         (m (first dims))
         (n (second dims))
         (ipiv (make-native-array (list (min m n)))))
    (with-foreign-scalar (m-ptr :int m)
      (with-foreign-scalar (n-ptr :int n)
        (with-foreign-scalar (lda-ptr :int m)
          (with-foreign-scalar (info-ptr :int 0)
            (ffi:%dgetrf m-ptr n-ptr
                        (native-array-data a) lda-ptr
                        (native-array-data ipiv) info-ptr)
            (let ((info (lib:mem-ref info-ptr :int)))
              (unless (zerop info)
                (error "LAPACK dgetrf failed with info = ~A" info)))))))
    (values a ipiv)))

(defun lapack-solve! (a b)
  "Solve linear system A*X = B (destructive)"
  (let* ((dims-a (native-array-dimensions a))
         (dims-b (native-array-dimensions b))
         (n (first dims-a))
         (nrhs (if (= (length dims-b) 1) 1 (second dims-b)))
         (ipiv (make-native-array (list n))))
    (with-foreign-scalar (n-ptr :int n)
      (with-foreign-scalar (nrhs-ptr :int nrhs)
        (with-foreign-scalar (lda-ptr :int n)
          (with-foreign-scalar (ldb-ptr :int n)
            (with-foreign-scalar (info-ptr :int 0)
              (ffi:%dgesv n-ptr nrhs-ptr
                         (native-array-data a) lda-ptr
                         (native-array-data ipiv)
                         (native-array-data b) ldb-ptr
                         info-ptr)
              (let ((info (lib:mem-ref info-ptr :int)))
                (unless (zerop info)
                  (error "LAPACK dgesv failed with info = ~A" info))))))))
    b))

(defun lapack-eigen! (a)
  "Compute eigenvalues and eigenvectors of matrix A"
  (let* ((dims (native-array-dimensions a))
         (n (first dims))
         (wr (make-native-array (list n)))  ; real parts
         (wi (make-native-array (list n)))  ; imaginary parts
         (vr (make-native-array dims))      ; right eigenvectors
         (work-size (* 4 n))
         (work (make-native-array (list work-size))))
    (with-foreign-scalar (jobvl-ptr :char (char-code #\N))
      (with-foreign-scalar (jobvr-ptr :char (char-code #\V))
        (with-foreign-scalar (n-ptr :int n)
          (with-foreign-scalar (lda-ptr :int n)
            (with-foreign-scalar (ldvr-ptr :int n)
              (with-foreign-scalar (lwork-ptr :int work-size)
                (with-foreign-scalar (info-ptr :int 0)
                  (ffi:%dgeev jobvl-ptr jobvr-ptr
                             n-ptr
                             (native-array-data a) lda-ptr
                             (native-array-data wr)
                             (native-array-data wi)
                             (lib:null-pointer)  ; no left eigenvectors
                             n-ptr               ; ldvl (unused)
                             (native-array-data vr) ldvr-ptr
                             (native-array-data work) lwork-ptr
                             info-ptr)
                  (let ((info (lib:mem-ref info-ptr :int)))
                    (unless (zerop info)
                      (error "LAPACK dgeev failed with info = ~A" info))))))))))
    (values wr wi vr)))

;;; High-level operations

(defun matrix-multiply (a b)
  "High-level matrix multiplication"
  (let* ((dims-a (native-array-dimensions a))
         (dims-b (native-array-dimensions b))
         (m (first dims-a))
         (n (second dims-b))
         (c (make-native-array (list m n) :initial-element 0.0d0)))
    (blas-gemm! 1.0d0 a b 0.0d0 c)
    c))

(defun matrix-vector-multiply (a x)
  "High-level matrix-vector multiplication"
  (let* ((dims-a (native-array-dimensions a))
         (m (first dims-a))
         (y (make-native-array (list m) :initial-element 0.0d0)))
    (blas-gemv! 1.0d0 a x 0.0d0 y)
    y))

(defun solve-linear-system (a b)
  "Solve A*x = b without destroying inputs"
  (let ((a-copy (copy-native-array a))
        (b-copy (copy-native-array b)))
    (lapack-solve! a-copy b-copy)
    b-copy))

(defun copy-native-array (array)
  "Create a copy of a native array"
  (let ((copy (make-native-array (native-array-dimensions array))))
    (blas-copy! array copy)
    copy))