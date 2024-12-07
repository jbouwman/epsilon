;;;; vectors.lisp -- signed/unsigned byte accessors

(in-package #:epsilon.lib.type)

(declaim (inline array-data-and-offsets))
(defun array-data-and-offsets (v start end)
  "Like ARRAY-DISPLACEMENT, only more useful."
  (sb-kernel:with-array-data ((v v) (start start) (end end))
    (values v start end)))

(macrolet ((define-fetcher (bitsize signedp big-endian-p)
             (let ((ref-name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(defun ,ref-name (vector index)
                  (declare (type ->u8 vector))
                  (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                  (multiple-value-bind (vector start end)
                      (array-data-and-offsets vector index (+ index ,bytes))
                     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                     (declare (type (integer 0 ,(- array-dimension-limit bytes)) start))
                    (declare (ignore end))
                    ,(ref-form 'vector 'start bytes signedp big-endian-p)))))
           (define-storer (bitsize signedp big-endian-p)
             (let ((ref-name (byte-ref-fun-name bitsize signedp big-endian-p))
                   (set-name (byte-set-fun-name bitsize signedp big-endian-p))
                   (bytes (truncate bitsize 8)))
               `(progn
                 (defun ,set-name (vector index value)
                   (declare (type ->u8 vector))
                   (declare (type (integer 0 ,(- array-dimension-limit bytes)) index))
                   (declare (type (,(if signedp
                                        'signed-byte
                                        'unsigned-byte) ,bitsize) value))
                   (multiple-value-bind (vector start end)
                       (array-data-and-offsets vector index (+ index ,bytes))
                     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                     (declare (type (integer 0 ,(- array-dimension-limit bytes)) start))
                     (declare (ignore end))
                     ,(set-form 'vector 'start 'value bytes big-endian-p)))
                 (defsetf ,ref-name ,set-name))))
           (define-fetchers-and-storers (bitsize)
               (loop for i from 0 below 4
                     for signedp = (logbitp 1 i)
                     for big-endian-p = (logbitp 0 i)
                     collect `(define-fetcher ,bitsize ,signedp ,big-endian-p) into forms
                     collect `(define-storer ,bitsize ,signedp ,big-endian-p) into forms
                     finally (return `(progn ,@forms)))))
  (define-fetchers-and-storers 16)
  (define-fetchers-and-storers 32)
  (define-fetchers-and-storers 64))

(declaim (sb-ext:maybe-inline ieee-single-ref/be))
(defun ieee-single-ref/be (vector index)
  (sb-kernel:make-single-float (s32ref/be vector index)))

(declaim (sb-ext:maybe-inline ieee-single-sef/be))
(defun ieee-single-set/be (vector index value)
  (setf (s32ref/be vector index) (sb-kernel:single-float-bits value))
  value)
(defsetf ieee-single-ref/be ieee-single-set/be)

(declaim (sb-ext:maybe-inline ieee-single-ref/le))
(defun ieee-single-ref/le (vector index)
  (sb-kernel:make-single-float (s32ref/le vector index)))

(declaim (sb-ext:maybe-inline ieee-single-set/le))
(defun ieee-single-set/le (vector index value)
  (progn
    (setf (s32ref/le vector index) (sb-kernel:single-float-bits value))
    value))
(defsetf ieee-single-ref/le ieee-single-set/le)

(declaim (sb-ext:maybe-inline ieee-double-ref/be))
(defun ieee-double-ref/be (vector index)
  (let ((upper (s32ref/be vector index))
        (lower (u32ref/be vector (+ index 4))))
    (sb-kernel:make-double-float upper lower)))

(declaim (sb-ext:maybe-inline ieee-double-set/be))
(defun ieee-double-set/be (vector index value)
  (progn
    (setf (s32ref/be vector index) (sb-kernel:double-float-high-bits value)
          (u32ref/be vector (+ index 4)) (sb-kernel:double-float-low-bits value))
    value))
(defsetf ieee-double-ref/be ieee-double-set/be)

(declaim (sb-ext:maybe-inline ieee-double-ref/le))
(defun ieee-double-ref/le (vector index)
  (let ((lower (u32ref/le vector index))
        (upper (s32ref/le vector (+ index 4))))
    (sb-kernel:make-double-float upper lower)))

(declaim (sb-ext:maybe-inline ieee-double-set/le))
(defun ieee-double-set/le (vector index value)
  (progn
    (setf (u32ref/le vector index) (sb-kernel:double-float-low-bits value)
          (s32ref/le vector (+ index 4)) (sb-kernel:double-float-high-bits value))
    value))
(defsetf ieee-double-ref/le ieee-double-set/le)
