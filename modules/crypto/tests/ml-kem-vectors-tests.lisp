;;;; ML-KEM canonical test-vector harness (IMPL-329 Phase A1 scaffold).
;;;;
;;;; This file walks `tests/vectors/ml-kem/<set>/<kind>/*.vec` and
;;;; runs each vector through the corresponding deterministic entry
;;;; point (`keygen-internal`, `encaps-internal`, or `decaps`),
;;;; asserting byte-level equality against the pinned reference
;;;; output. When the vector directory is empty (which it is until
;;;; canonical NIST ACVP or PQClean KAT vectors are imported) the
;;;; harness test passes trivially so it does not block CI.
;;;;
;;;; See `tests/vectors/ml-kem/README.md` for the file format and
;;;; for guidance on where to source canonical vectors.

(defpackage epsilon.crypto.ml-kem-vectors-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.crypto.ml-kem ml-kem)))

(in-package :epsilon.crypto.ml-kem-vectors-tests)

(defun %hex-to-bytes (hex)
  "Decode a whitespace-tolerant lowercase hex string to a byte vector."
  (let* ((clean (remove-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
                           hex))
         (len (floor (length clean) 2))
         (out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len out)
      (setf (aref out i)
            (parse-integer clean :start (* 2 i) :end (* 2 (1+ i)) :radix 16)))))

(defun %parse-vector-file (path)
  "Parse a .vec file into an alist of (key . value). Keys are lowercase
   strings; byte-valued values are decoded to byte vectors, numeric
   fields to integers, and textual fields to strings."
  (let ((result '()))
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            unless (or (zerop (length trimmed))
                       (char= (char trimmed 0) #\#))
            do (let ((colon (position #\: trimmed)))
                 (when colon
                   (let* ((key (string-downcase
                                (string-trim '(#\Space #\Tab)
                                             (subseq trimmed 0 colon))))
                          (raw (string-trim '(#\Space #\Tab)
                                            (subseq trimmed (1+ colon))))
                          (value (cond
                                   ((member key '("kind") :test #'string=) raw)
                                   (t (%hex-to-bytes raw)))))
                     (push (cons key value) result))))))
    (nreverse result)))

(defun %set-from-dir (name)
  (cond ((string= name "ml-kem-512")  :ml-kem-512)
        ((string= name "ml-kem-768")  :ml-kem-768)
        ((string= name "ml-kem-1024") :ml-kem-1024)
        (t (error "unknown parameter-set directory: ~A" name))))

(defparameter *source-dir*
  #.(make-pathname :defaults (or *compile-file-pathname* *load-pathname*)
                   :name nil :type nil)
  "Directory containing this source file, captured at compile time so
   we can find the vector tree regardless of how the file is loaded.")

(defun %vectors-root ()
  (merge-pathnames "vectors/ml-kem/" *source-dir*))

(defun %collect-vec-files (root kind)
  "Return a list of (set-keyword . pathname) for every .vec file
   under ROOT/<set>/<kind>/."
  (let ((out '()))
    (dolist (set-dir (directory (merge-pathnames "*/" root)))
      (let* ((set-name (car (last (pathname-directory set-dir))))
             (set (ignore-errors (%set-from-dir set-name)))
             (kind-dir (merge-pathnames (format nil "~A/" kind) set-dir)))
        (when set
          (dolist (file (directory (merge-pathnames "*.vec" kind-dir)))
            (push (cons set file) out)))))
    (nreverse out)))

(defun %assoc-bytes (vector key &key required)
  (let ((cell (assoc key vector :test #'string=)))
    (cond (cell (cdr cell))
          (required (error "vector missing required field ~A" key))
          (t nil))))

(defun %run-keygen-vector (set path)
  (let* ((v (%parse-vector-file path))
         (d  (%assoc-bytes v "d"  :required t))
         (z  (%assoc-bytes v "z"  :required t))
         (ek (%assoc-bytes v "ek" :required t))
         (dk (%assoc-bytes v "dk" :required t)))
    (multiple-value-bind (computed-ek computed-dk)
        (ml-kem:keygen-internal d z set)
      (unless (equalp computed-ek ek)
        (error "~A: ek mismatch" path))
      (unless (equalp computed-dk dk)
        (error "~A: dk mismatch" path)))))

(defun %run-encaps-vector (set path)
  (let* ((v (%parse-vector-file path))
         (ek (%assoc-bytes v "ek" :required t))
         (m  (%assoc-bytes v "m"  :required t))
         (k  (%assoc-bytes v "k"  :required t))
         (c  (%assoc-bytes v "c"  :required t)))
    (multiple-value-bind (computed-k computed-c)
        (ml-kem:encaps-internal ek m set)
      (unless (equalp computed-k k)
        (error "~A: shared secret mismatch" path))
      (unless (equalp computed-c c)
        (error "~A: ciphertext mismatch" path)))))

(defun %run-decaps-vector (set path)
  (let* ((v (%parse-vector-file path))
         (dk (%assoc-bytes v "dk" :required t))
         (c  (%assoc-bytes v "c"  :required t))
         (k  (%assoc-bytes v "k"  :required t)))
    (let ((computed (ml-kem:decaps dk c set)))
      (unless (equalp computed k)
        (error "~A: decaps mismatch" path)))))

(deftest test-ml-kem-canonical-vectors
  "Walk the tests/vectors/ml-kem tree and run every canonical vector
   through the corresponding deterministic entry point, asserting
   byte-level equality. Passes trivially (but is still exercised) if
   the vector tree is empty, so the harness does not block CI until
   canonical vectors are imported."
  (let ((root (%vectors-root)))
    (when (probe-file root)
      (dolist (cell (%collect-vec-files root "keygen"))
        (%run-keygen-vector (car cell) (cdr cell)))
      (dolist (cell (%collect-vec-files root "encaps"))
        (%run-encaps-vector (car cell) (cdr cell)))
      (dolist (cell (%collect-vec-files root "decaps"))
        (%run-decaps-vector (car cell) (cdr cell))))
    (assert-true t)))
