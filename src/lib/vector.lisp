(defpackage #:epsilon.lib.vector
  (:use
   #:cl
   #:epsilon.lib.symbol
   #:epsilon.lib.type)
  (:export
   #:check-bounds
   #:with-checked-bounds
   #:u8-get
   #:u8-set))
 
(in-package #:epsilon.lib.vector)

(defun check-bounds (vector start end)
  (let ((len (length vector)))
    (unless (<= 0 start end len)
      (error "Invalid vector start/end/length: ~A/~A/~A" start end len))))

(defmacro with-checked-bounds (((v vector) (s start) (e end)) &body body)
  "Like WITH-SIMPLE-VECTOR but bound-checks START and END."
  (once-only (vector start)
    `(let ((,e (or ,end (length ,vector))))
       (check-bounds ,vector ,start ,e)
       (sb-kernel:with-array-data ((,v ,vector) (,s ,start) (,e ,e)) ,@body))))

(defmacro u8-get (vector index &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness
                            '(:be #+big-endian :ne #+little-endian :re))))
    (once-only (vector index)
      `(logand
        ,(1- (ash 1 (* 8 bytes)))
        (logior
         ,@(loop for i from 0 below bytes
                 for offset = (if big-endian i (- bytes i 1))
                 for shift = (if big-endian
                                 (* (- bytes i 1) 8)
                                 (* offset 8))
                 collect `(ash (aref ,vector (+ ,index ,offset)) ,shift)))))))

(defmacro u8-set (value vector index &optional (bytes 1) (endianness :ne))
  (let ((big-endian (member endianness
                            '(:be #+big-endian :ne #+little-endian :re))))
    `(progn
       ,@(loop for i from 1 to bytes
               for offset = (if big-endian (- bytes i) (1- i)) collect
               `(setf (aref ,vector (+ ,index ,offset))
                      (ldb (byte 8 ,(* 8 (1- i))) ,value)))
       (values))))
