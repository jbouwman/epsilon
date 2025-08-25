;;;; FFI Bindings for OpenBLAS
;;;;
;;;; This file contains Foreign Function Interface bindings for BLAS/LAPACK operations

(defpackage epsilon.compute.blas-ffi
  (:use :cl)
  (:local-nicknames
   (#:foreign #:epsilon.foreign))
  (:export
   ;; BLAS Level 1 - Vector operations
   #:%cblas-ddot    ; dot product
   #:%cblas-dnrm2   ; Euclidean norm
   #:%cblas-dasum   ; sum of absolute values
   #:%cblas-daxpy   ; y = a*x + y
   #:%cblas-dscal   ; x = a*x
   #:%cblas-dcopy   ; copy vector
   #:%cblas-dswap   ; swap vectors
   
   ;; BLAS Level 2 - Matrix-vector operations
   #:%cblas-dgemv   ; y = alpha*A*x + beta*y
   #:%cblas-dger    ; A = alpha*x*y' + A (rank-1 update)
   #:%cblas-dsymv   ; symmetric matrix-vector multiply
   #:%cblas-dtrsv   ; solve triangular system
   
   ;; BLAS Level 3 - Matrix-matrix operations  
   #:%cblas-dgemm   ; C = alpha*A*B + beta*C
   #:%cblas-dsymm   ; symmetric matrix multiply
   #:%cblas-dtrsm   ; solve triangular matrix system
   #:%cblas-dsyrk   ; symmetric rank-k update
   
   ;; LAPACK routines
   #:%dgetrf        ; LU decomposition
   #:%dgetrs        ; solve using LU decomposition
   #:%dgetri        ; matrix inverse using LU
   #:%dgesv         ; solve linear system
   #:%dgels         ; least squares
   #:%dgeev         ; eigenvalues and eigenvectors
   #:%dgesvd        ; singular value decomposition
   #:%dpotrf        ; Cholesky decomposition
   #:%dpotrs        ; solve using Cholesky
   
   ;; Constants
   #:+cblas-row-major+
   #:+cblas-col-major+
   #:+cblas-no-trans+
   #:+cblas-trans+
   #:+cblas-conj-trans+
   #:+cblas-upper+
   #:+cblas-lower+
   #:+cblas-non-unit+
   #:+cblas-unit+
   #:+cblas-left+
   #:+cblas-right+))

(in-package epsilon.compute.blas-ffi)

;;; Constants for CBLAS
(defconstant +cblas-row-major+ 101)
(defconstant +cblas-col-major+ 102)
(defconstant +cblas-no-trans+ 111)
(defconstant +cblas-trans+ 112)
(defconstant +cblas-conj-trans+ 113)
(defconstant +cblas-upper+ 121)
(defconstant +cblas-lower+ 122)
(defconstant +cblas-non-unit+ 131)
(defconstant +cblas-unit+ 132)
(defconstant +cblas-left+ 141)
(defconstant +cblas-right+ 142)

;;; OpenBLAS library path (NixOS)
(defparameter *openblas-lib* "libopenblas")

;;; BLAS Level 1 Functions

(foreign:defshared %cblas-ddot "cblas_ddot" "libopenblas" :double
  (n :int)           ; number of elements
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  (y :pointer)       ; vector y
  (incy :int)        ; increment for y
  :documentation "Compute dot product of two vectors")

(foreign:defshared %cblas-dnrm2 "cblas_dnrm2" "libopenblas" :double
  (n :int)           ; number of elements
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  :documentation "Compute Euclidean norm of vector")

(foreign:defshared %cblas-dasum "cblas_dasum" "libopenblas" :double
  (n :int)           ; number of elements
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  :documentation "Compute sum of absolute values")

(foreign:defshared %cblas-daxpy "cblas_daxpy" "libopenblas" :void
  (n :int)           ; number of elements
  (alpha :double)    ; scalar alpha
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  (y :pointer)       ; vector y (input/output)
  (incy :int)        ; increment for y
  :documentation "Compute y = alpha*x + y")

(foreign:defshared %cblas-dscal "cblas_dscal" "libopenblas" :void
  (n :int)           ; number of elements
  (alpha :double)    ; scalar alpha
  (x :pointer)       ; vector x (input/output)
  (incx :int)        ; increment for x
  :documentation "Compute x = alpha*x")

(foreign:defshared %cblas-dcopy "cblas_dcopy" "libopenblas" :void
  (n :int)           ; number of elements
  (x :pointer)       ; source vector
  (incx :int)        ; increment for x
  (y :pointer)       ; destination vector
  (incy :int)        ; increment for y
  :documentation "Copy vector x to y")

(foreign:defshared %cblas-dswap "cblas_dswap" "libopenblas" :void
  (n :int)           ; number of elements
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  (y :pointer)       ; vector y
  (incy :int)        ; increment for y
  :documentation "Swap vectors x and y")

;;; BLAS Level 2 Functions

(foreign:defshared %cblas-dgemv "cblas_dgemv" "libopenblas" :void
  (order :int)       ; row/column major
  (trans :int)       ; transpose operation
  (m :int)           ; number of rows
  (n :int)           ; number of columns
  (alpha :double)    ; scalar alpha
  (a :pointer)       ; matrix A
  (lda :int)         ; leading dimension of A
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  (beta :double)     ; scalar beta
  (y :pointer)       ; vector y (input/output)
  (incy :int)        ; increment for y
  :documentation "Matrix-vector multiplication: y = alpha*A*x + beta*y")

(foreign:defshared %cblas-dger "cblas_dger" "libopenblas" :void
  (order :int)       ; row/column major
  (m :int)           ; number of rows
  (n :int)           ; number of columns
  (alpha :double)    ; scalar alpha
  (x :pointer)       ; vector x
  (incx :int)        ; increment for x
  (y :pointer)       ; vector y
  (incy :int)        ; increment for y
  (a :pointer)       ; matrix A (input/output)
  (lda :int)         ; leading dimension of A
  :documentation "Rank-1 update: A = alpha*x*y' + A")

;;; BLAS Level 3 Functions

(foreign:defshared %cblas-dgemm "cblas_dgemm" "libopenblas" :void
  (order :int)       ; row/column major
  (transa :int)      ; transpose A
  (transb :int)      ; transpose B
  (m :int)           ; rows of A (or A')
  (n :int)           ; columns of B (or B')
  (k :int)           ; columns of A (or A')
  (alpha :double)    ; scalar alpha
  (a :pointer)       ; matrix A
  (lda :int)         ; leading dimension of A
  (b :pointer)       ; matrix B
  (ldb :int)         ; leading dimension of B
  (beta :double)     ; scalar beta
  (c :pointer)       ; matrix C (input/output)
  (ldc :int)         ; leading dimension of C
  :documentation "Matrix-matrix multiplication: C = alpha*A*B + beta*C")

(foreign:defshared %cblas-dsymm "cblas_dsymm" "libopenblas" :void
  (order :int)       ; row/column major
  (side :int)        ; left or right
  (uplo :int)        ; upper or lower
  (m :int)           ; rows of C
  (n :int)           ; columns of C
  (alpha :double)    ; scalar alpha
  (a :pointer)       ; symmetric matrix A
  (lda :int)         ; leading dimension of A
  (b :pointer)       ; matrix B
  (ldb :int)         ; leading dimension of B
  (beta :double)     ; scalar beta
  (c :pointer)       ; matrix C (input/output)
  (ldc :int)         ; leading dimension of C
  :documentation "Symmetric matrix multiplication")

;;; LAPACK Functions

(foreign:defshared %dgetrf "dgetrf_" "libopenblas" :void
  (m :pointer)       ; number of rows (pointer to int)
  (n :pointer)       ; number of columns (pointer to int)
  (a :pointer)       ; matrix A (input/output)
  (lda :pointer)     ; leading dimension (pointer to int)
  (ipiv :pointer)    ; pivot indices (output)
  (info :pointer)    ; error code (output)
  :documentation "LU decomposition with partial pivoting")

(foreign:defshared %dgetrs "dgetrs_" "libopenblas" :void
  (trans :pointer)   ; transpose flag (pointer to char)
  (n :pointer)       ; order of matrix (pointer to int)
  (nrhs :pointer)    ; number of right-hand sides (pointer to int)
  (a :pointer)       ; LU factors from dgetrf
  (lda :pointer)     ; leading dimension (pointer to int)
  (ipiv :pointer)    ; pivot indices from dgetrf
  (b :pointer)       ; right-hand sides (input/output)
  (ldb :pointer)     ; leading dimension of B (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Solve linear system using LU decomposition")

(foreign:defshared %dgesv "dgesv_" "libopenblas" :void
  (n :pointer)       ; order of matrix (pointer to int)
  (nrhs :pointer)    ; number of right-hand sides (pointer to int)
  (a :pointer)       ; matrix A (input/output - contains LU on exit)
  (lda :pointer)     ; leading dimension of A (pointer to int)
  (ipiv :pointer)    ; pivot indices (output)
  (b :pointer)       ; right-hand sides (input/output - contains solution on exit)
  (ldb :pointer)     ; leading dimension of B (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Solve linear system A*X = B")

(foreign:defshared %dgeev "dgeev_" "libopenblas" :void
  (jobvl :pointer)   ; compute left eigenvectors (pointer to char)
  (jobvr :pointer)   ; compute right eigenvectors (pointer to char)
  (n :pointer)       ; order of matrix (pointer to int)
  (a :pointer)       ; matrix A (input/output)
  (lda :pointer)     ; leading dimension (pointer to int)
  (wr :pointer)      ; real parts of eigenvalues (output)
  (wi :pointer)      ; imaginary parts of eigenvalues (output)
  (vl :pointer)      ; left eigenvectors (output)
  (ldvl :pointer)    ; leading dimension of VL (pointer to int)
  (vr :pointer)      ; right eigenvectors (output)
  (ldvr :pointer)    ; leading dimension of VR (pointer to int)
  (work :pointer)    ; workspace
  (lwork :pointer)   ; size of workspace (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Compute eigenvalues and eigenvectors")

(foreign:defshared %dgesvd "dgesvd_" "libopenblas" :void
  (jobu :pointer)    ; compute U (pointer to char)
  (jobvt :pointer)   ; compute V^T (pointer to char)
  (m :pointer)       ; number of rows (pointer to int)
  (n :pointer)       ; number of columns (pointer to int)
  (a :pointer)       ; matrix A (input/output)
  (lda :pointer)     ; leading dimension of A (pointer to int)
  (s :pointer)       ; singular values (output)
  (u :pointer)       ; left singular vectors (output)
  (ldu :pointer)     ; leading dimension of U (pointer to int)
  (vt :pointer)      ; right singular vectors transposed (output)
  (ldvt :pointer)    ; leading dimension of VT (pointer to int)
  (work :pointer)    ; workspace
  (lwork :pointer)   ; size of workspace (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Singular value decomposition")

(foreign:defshared %dpotrf "dpotrf_" "libopenblas" :void
  (uplo :pointer)    ; upper or lower triangular (pointer to char)
  (n :pointer)       ; order of matrix (pointer to int)
  (a :pointer)       ; matrix A (input/output - contains Cholesky factor on exit)
  (lda :pointer)     ; leading dimension (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Cholesky decomposition")

(foreign:defshared %dpotrs "dpotrs_" "libopenblas" :void
  (uplo :pointer)    ; upper or lower triangular (pointer to char)
  (n :pointer)       ; order of matrix (pointer to int)
  (nrhs :pointer)    ; number of right-hand sides (pointer to int)
  (a :pointer)       ; Cholesky factor from dpotrf
  (lda :pointer)     ; leading dimension of A (pointer to int)
  (b :pointer)       ; right-hand sides (input/output - contains solution on exit)
  (ldb :pointer)     ; leading dimension of B (pointer to int)
  (info :pointer)    ; error code (output)
  :documentation "Solve using Cholesky decomposition")