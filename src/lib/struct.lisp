(defpackage :epsilon.lib.struct
  (:use
   :cl
   :epsilon.lib.type)
  (:export
   :*structures*
   :define-byte-structure
   :read-structure))

(in-package :epsilon.lib.struct)

;; Zipfile structure support for lib.archive

(defvar *structures* (make-hash-table :test 'eql))

(defun decode-structure (vector index)
  (let* ((signature (u32ref/le vector index))
         (parser (or (gethash signature *structures*)
                     (error 'unknown-block-signature :signature signature))))
    (funcall (first parser) vector (+ index 4))))

(defun read-structure (stream)
  (let* ((signature (read-u32/le stream))
         (parser (or (gethash signature *structures*)
                     (error 'unknown-block-signature :signature signature))))
    (funcall (second parser) stream)))

(defun encode-structure (structure vector index)
  (let ((parser (or (gethash (type-of structure) *structures*)
                    (error 'unknown-structure-type :object structure))))
    (funcall (third parser) structure vector index)))

(defun write-structure (structure stream)
  (let ((parser (or (gethash (type-of structure) *structures*)
                    (error 'unknown-structure-type :object structure))))
    (funcall (fourth parser) structure stream)))

(defun integer-binary-type (integer)
  (cond ((<= integer #xFF) 'u8)
        ((<= integer #xFFFF) 'u16)
        ((<= integer #xFFFFFFFF) 'u32)
        ((<= integer #xFFFFFFFFFFFFFFFF) 'u64)
        (T (error 'integer-too-large))))

(defun binary-type-size (type)
  (ecase type
    (u8 1)
    (u16 2)
    (u32 4)
    (u64 8)))

(defun binary-type-type (type)
  (ecase type
    (u8 'u8)
    (u16 '(unsigned-byte 16))
    (u32 '(unsigned-byte 32))
    (u64 '(unsigned-byte 64))))

(defun binary-type-decoder (type)
  (ecase type
    (u8 'aref)
    (u16 'u16ref/le)
    (u32 'u32ref/le)
    (u64 'u64ref/le)))

(defun binary-type-reader (type)
  (ecase type
    (u8 'read-byte)
    (u16 'read-u16/le)
    (u32 'read-u32/le)
    (u64 'read-u64/le)))

(defun binary-type-encoder (type)
  (ecase type
    (u8 '(lambda (vector index value)
            (setf (aref vector index) value)))
    (u16 '(lambda (vector index value)
             (setf (u16ref/le vector index) value)))
    (u32 '(lambda (vector index value)
             (setf (u32ref/le vector index) value)))
    (u64 '(lambda (vector index value)
             (setf (u64ref/le vector index) value)))))

(defun binary-type-writer (type)
  (ecase type
    (u8 'write-byte)
    (u16 'write-u16/le)
    (u32 'write-u32/le)
    (u64 'write-u64/le)))

(defun generate-record-reader (record stream)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(progn
                (setf ,name (,(binary-type-reader btype) ,stream))
                (unless (= ,type ,name)
                  (error 'mismatched-type-signature :signature ,name)))))
          (count
           `(progn
              (setf ,name (make-array ,count :element-type ',(binary-type-type type)))
              (loop for i from 0 below (length ,name)
                    do (setf (aref ,name i) (,(binary-type-reader type) ,stream)))))
          (T
           `(setf ,name (,(binary-type-reader type) ,stream))))))

(defun generate-record-encoder (record vector index)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(progn (,(binary-type-encoder btype) ,vector ,index ,type)
                     (incf ,index ,(binary-type-size btype)))))
          (count
           (if (eql type 'character)
               `(let ((vec (epsilon.lib.char:string-to-u8 ,name :encoding :utf-8)))
                  (loop for char across vec
                        do (setf (aref ,vector ,index) char)
                           (incf ,index)
                        finally (return ,index)))
               `(loop for i from 0 below ,count
                      do (,(binary-type-encoder type) ,vector ,index (aref ,name i))
                         (incf ,index ,(binary-type-size type))
                      finally (return ,index))))
          (T
           `(progn (,(binary-type-encoder type) ,vector ,index ,name)
                   (incf ,index ,(binary-type-size type)))))))

(defun generate-record-writer (record stream)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(,(binary-type-writer btype) ,type ,stream)))
          (count
           (if (eql type 'character)
               `(let ((vec (epsilon.lib.char:string-to-u8 ,name :encoding :utf-8)))
                  (loop for char across vec
                        do (write-byte char ,stream)))
               `(loop for i from 0 below ,count
                      do (,(binary-type-writer type) (aref ,name i) ,stream))))
          (T
           `(,(binary-type-writer type) ,name ,stream)))))

(defun generate-record-decoder (record vector index)
  (destructuring-bind (name type &optional count) record
    (cond ((typep type 'integer)
           (let ((btype (integer-binary-type type)))
             `(progn
                (decf size ,(binary-type-size btype))
                (setf ,name (,(binary-type-decoder btype) ,vector ,index))
                (incf ,index ,(binary-type-size btype))
                (unless (= ,type ,name)
                  (error 'mismatched-type-signature :signature ,name)))))
          (count
           `(progn
              (setf ,name (make-array ,count :element-type ',(binary-type-type type)))
              (decf size (* (length ,name) ,(binary-type-size type)))
              (loop for i from 0 below (length ,name)
                    do (setf (aref ,name i) (,(binary-type-decoder type) ,vector ,index))
                       (incf ,index ,(binary-type-size type)))))
          (T
           `(progn
              (decf size ,(binary-type-size type))
              (setf ,name (,(binary-type-decoder type) ,vector ,index))
              (incf ,index ,(binary-type-size type)))))))

(defmacro define-byte-structure (name &body records)
  (destructuring-bind (name signature) (if (listp name) name (list name NIL))
    (let ((fields (mapcar #'first records))
          (constructor (intern (format NIL "~a-~a" 'make name)))
          (decode-name (intern (format NIL "~a-~a" 'decode name)))
          (read-name (intern (format NIL "~a-~a" 'read name)))
          (encode-name (intern (format NIL "~a-~a" 'encode name)))
          (write-name (intern (format NIL "~a-~a" 'write name))))
      `(progn
         (defstruct (,name (:constructor ,constructor ,fields))
           ,@fields)
         (defun ,decode-name (vector index &optional (size most-positive-fixnum))
           (let ,(remove 'size fields)
             (block NIL
               ,@(loop for record in records
                       collect `(when (<= size 0) (return))
                       collect (generate-record-decoder record 'vector 'index)))
             (values (,constructor ,@fields) index)))
         (defun ,read-name (stream)
           (let ,fields
             ,@(loop for record in records
                     collect (generate-record-reader record 'stream))
             (,constructor ,@fields)))
         (defun ,encode-name (structure vector index)
           ,@(typecase signature
               ((unsigned-byte 16)
                `((setf (u16ref/le vector index) ,signature)
                  (incf index 2)))
               ((unsigned-byte 32)
                `((setf (u32ref/le vector index) ,signature)
                  (incf index 4))))
           (with-slots ,fields structure
             ,@(loop for record in records
                     collect (generate-record-encoder record 'vector 'index))))
         (defun ,write-name (structure stream)
           ,@(typecase signature
               ((unsigned-byte 16)
                `((write-u16/le ,signature stream)))
               ((unsigned-byte 32)
                `((write-u32/le ,signature stream))))
           (with-slots ,fields structure
             ,@(loop for record in records
                     collect (generate-record-writer record 'stream))))
         ,@(when signature
             `((setf (gethash ',name *structures*)
                     (setf (gethash ,signature *structures*)
                           (list #',decode-name #',read-name #',encode-name #',write-name)))))))))
