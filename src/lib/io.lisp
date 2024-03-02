(defpackage #:lib.io
  (:use
   #:cl
   #:lib.binding
   #:lib.list
   #:lib.symbol
   #:lib.type
   #:sb-gray)
  (:export
   #:file=
   #:stream=
   #:stream-files
   #:copy-stream
   
   #:*default-output-buffer-size*

;;   #:index
;;   #:input-buffer #:output-buffer

   #:octets-from

   #:make-output-buffer #:finish-output-buffer
   #:buffer-position

   #:make-input-buffer #:input-buffer-vector #:input-buffer-stream

   #:fast-read-byte #:fast-write-byte
   #:fast-read-sequence #:fast-write-sequence
   #:with-fast-input #:with-fast-output

   ;; #:fast-seek

   #:write8 #:writeu8
   #:write8-le #:writeu8-le #:write8-be #:writeu8-be
   #:write16-le #:writeu16-le #:write16-be #:writeu16-be
   #:write24-le #:writeu24-le #:write24-be #:writeu24-be
   #:write32-le #:writeu32-le #:write32-be #:writeu32-be
   #:write64-le #:writeu64-le #:write64-be #:writeu64-be
   #:write128-le #:writeu128-le #:write128-be #:writeu128-be

   #:read8 #:readu8
   #:read8-le #:readu8-le #:read8-be #:readu8-be
   #:read16-le #:readu16-le #:read16-be #:readu16-be
   #:read32-le #:readu32-le #:read32-be #:readu32-be
   #:read64-le #:readu64-le #:read64-be #:readu64-be
   #:read128-le #:readu128-le #:read128-be #:readu128-be

   #:fast-output-stream #:fast-input-stream
   #:finish-output-stream))

(in-package #:lib.io)

(defmacro with-u8-in ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'u8)
     ,@body))

(defmacro with-u8-out ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'u8
                           :direction :output
                           :if-exists :supersede)
     ,@body))

(defun stream-files (f in out)
  (with-u8-in (in in)
    (with-u8-out (out out)
      (funcall f in out))))

(defun stream= (a b)
  (loop :for ab := (read-byte a nil nil)
        :for bb := (read-byte b nil nil)
        :unless (eql ab bb)
          :return nil
        :unless (and ab bb)
          :return t))

(defun file= (a b)
  (with-u8-in (a a)
    (with-u8-in (b b)
      (stream= a b))))

;; Vector buffer

(defvar *default-output-buffer-size* 16)

(declaim (inline output-buffer-vector output-buffer-fill output-buffer-len))
(defstruct output-buffer
  (vector (->u8 *default-output-buffer-size*)
   :type ->u8)
  (fill 0 :type array-index)
  (len 0 :type array-index)
  (queue nil :type list)
  (last nil :type list)
  (output nil))

(defstruct input-buffer
  (vector nil :type (or null ->u8))
  (pos 0 :type array-index)
  (stream nil))

(defun buffer-position (buffer)
  "Return the number of bytes read (for an INPUT-BUFFER) or written
   (for an OUTPUT-BUFFER)"
  (etypecase buffer
    (input-buffer (input-buffer-pos buffer))
    (output-buffer (output-buffer-len buffer))))

;; Sometimes it is usefull just to skip the buffer instead of reading from it.
(defun (setf buffer-position) (new-pos buffer)
  "Set the buffer position for input-buffer"
  (check-type buffer input-buffer)
  (let* ((pos (input-buffer-pos buffer))
         (vec (input-buffer-vector buffer))
         (vec-len (length vec)))
    (declare (optimize (speed 3) (safety 1))
             (type ->u8 vec)
             (type fixnum pos vec-len new-pos))
    ;; Only need to update if pos or new-pos is in stream range.
    (when-let ((stream-update-needed? (or (> pos vec-len)
                                          (> new-pos vec-len)))
               (stream (input-buffer-stream buffer)))
      (let* ((stream-file-pos (file-position stream))
             (pos-diff (- new-pos pos))
             (stream-diff (cond ((and (> pos vec-len)
                                      (< new-pos vec-len))
                                 ;; branch for pos in stream and new-pos
                                 ;; is in vector.
                                 (- vec-len pos))
                                ((and (< pos vec-len)
                                      (> new-pos vec-len))
                                 ;; branch for pos in vector. and new-pos
                                 ;; is in stream.
                                 (- pos-diff (- vec-len pos)))
                                ;; otherwise stream-diff = pos-diff.
                                (t pos-diff)))
             (new-stream-pos (+ stream-file-pos stream-diff)))
        (declare (type fixnum stream-file-pos new-stream-pos)
                 (type fixnum pos-diff stream-diff))
        (file-position stream new-stream-pos))))
  (setf (slot-value buffer 'pos) new-pos))

(defun octets-from (sequence)
  (let ((vec (->u8 (length sequence))))
    (replace vec sequence)
    vec))

(defun concat-buffer (buffer)
  (let* ((len (output-buffer-len buffer))
         (array
           #+fast-io-sv
           (if (eq :static (output-buffer-output buffer))
               (static-vectors:make-static-vector (the array-index len))
               (->u8 len))
           #-fast-io-sv
           (->u8 len)))
    (loop as i = 0 then (+ i (length a))
          for a in (output-buffer-queue buffer) do
            (replace (the ->u8 array)
                     (the ->u8 a) :start1 i)
          finally
             (replace (the ->u8 array)
                      (output-buffer-vector buffer)
                      :start1 i
                      :end2 (output-buffer-fill buffer)))
    array))

(defun flush (output-buffer)
  (when (> (output-buffer-fill output-buffer) 0)
    (write-sequence (output-buffer-vector output-buffer)
                    (output-buffer-output output-buffer)
                    :start 0 :end (output-buffer-fill output-buffer))
    (prog1 (output-buffer-fill output-buffer)
      (setf (output-buffer-fill output-buffer) 0))))

(defun extend (buffer &optional (min 1))
  (let ((vector (output-buffer-vector buffer)))
    (setf (output-buffer-last buffer)
          (nconc (output-buffer-last buffer)
                 (cons vector nil))
          (output-buffer-vector buffer)
          (->u8 (max min (1+ (* 2 (length vector)))))
          (output-buffer-fill buffer) 0)
    (unless (output-buffer-queue buffer)
      (setf (output-buffer-queue buffer)
            (output-buffer-last buffer)))))

(defun fast-write-byte (byte output-buffer)
  (declare (type u8 byte)
           (type output-buffer output-buffer)
           (optimize (speed 3) (safety 1)))
  (when (= (output-buffer-fill output-buffer)
           (array-dimension (output-buffer-vector output-buffer) 0))
    (if (streamp (output-buffer-output output-buffer))
        (flush output-buffer)
        (extend output-buffer)))
  (prog1
      (setf (aref (output-buffer-vector output-buffer)
                  (output-buffer-fill output-buffer))
            byte)
    (incf (output-buffer-fill output-buffer))
    (incf (output-buffer-len output-buffer))))

(defun fast-read-byte (input-buffer &optional (eof-error-p t) eof-value)
  (declare (type input-buffer input-buffer))
  (when-let ((vec (input-buffer-vector input-buffer))
             (pos (input-buffer-pos input-buffer)))
    (when (< pos (length vec))
      (incf (input-buffer-pos input-buffer))
      (return-from fast-read-byte (aref vec pos))))
  (when-let (stream (input-buffer-stream input-buffer))
    (let ((byte (read-byte stream eof-error-p eof-value)))
      (unless (equal byte eof-value)
        (incf (input-buffer-pos input-buffer)))
      (return-from fast-read-byte byte)))
  (if eof-error-p
      (error 'end-of-file :stream input-buffer)
      eof-value))

(defun fast-peek-byte (input-buffer &optional peek-type (eof-error-p t) eof-value)
  "This is like `peek-byte' only for fast-io input-buffers."
  (declare (type input-buffer input-buffer))
  (loop :for octet = (fast-read-byte input-buffer eof-error-p :eof)
     :for new-pos :from (input-buffer-pos input-buffer)
     :until (cond ((eq octet :eof)
                   (return eof-value))
                  ((null peek-type))
                  ((eq peek-type 't)
                   (plusp octet))
                  ((= octet peek-type)))
     :finally (setf (buffer-position input-buffer) new-pos)
       (return octet)))

(defun fast-write-sequence (sequence output-buffer &optional (start 0) end)
  (if (streamp (output-buffer-output output-buffer))
      (progn
        (flush output-buffer)
        (write-sequence sequence (output-buffer-output output-buffer) :start start :end end))
      (progn
        (let* ((start2 start)
               (len (if end
                        (- end start)
                        (- (length sequence) start)))
               (buffer-remaining
                 (- (length (output-buffer-vector output-buffer))
                    (output-buffer-fill output-buffer))))
          (when (> buffer-remaining 0)
            (replace (output-buffer-vector output-buffer)
                     (the ->u8 sequence)
                     :start1 (output-buffer-fill output-buffer)
                     :start2 start2
                     :end2 end)
            (incf start2 buffer-remaining)
            (incf (output-buffer-fill output-buffer)
                  (min buffer-remaining len)))
          (let ((sequence-remaining (- (or end (length sequence)) start2)))
            (when (> sequence-remaining 0)
              (extend output-buffer sequence-remaining)
              (replace (output-buffer-vector output-buffer)
                       (the ->u8 sequence)
                       :start2 start2
                       :end2 end)
              (incf (output-buffer-fill output-buffer) sequence-remaining)))
          (incf (output-buffer-len output-buffer) len)
          len))))

(defun fast-read-sequence (sequence input-buffer &optional (start 0) end)
  (declare (type ->u8 sequence)
           (type input-buffer input-buffer))
  (let ((start1 start)
        (total-len (if end
                       (- end start)
                       (- (length sequence) start))))
    (when-let ((vec (input-buffer-vector input-buffer))
               (pos (input-buffer-pos input-buffer)))
      (when (< pos (length vec))
        (let ((len (min total-len (- (length vec) pos))))
          (replace sequence vec
                   :start1 start1
                   :start2 pos
                   :end2 (+ pos len))
          (incf (input-buffer-pos input-buffer) len)
          (incf start1 len))))
    (when (< start1 total-len)
      (when-let (stream (input-buffer-stream input-buffer))
        (let ((bytes-read (read-sequence sequence stream
                                         :start start1
                                         :end (+ total-len start1))))
          (incf (input-buffer-pos input-buffer) bytes-read)
          (return-from fast-read-sequence bytes-read))))
    start1))

(defun finish-output-buffer (output-buffer)
  "Finish an output buffer. If it is backed by a vector (static or otherwise)
it returns the final octet vector. If it is backed by a stream it ensures that
all data has been flushed to the stream."
  (if (streamp (output-buffer-output output-buffer))
      (flush output-buffer)
      (concat-buffer output-buffer)))

(defmacro with-fast-output ((buffer &optional output) &body body)
  "Create `BUFFER`, optionally outputting to `OUTPUT`."
  `(let ((,buffer (make-output-buffer :output ,output)))
     ,@body
     (if (streamp (output-buffer-output ,buffer))
         (flush ,buffer)
         (finish-output-buffer ,buffer))))

(defmacro with-fast-input ((buffer vector &optional stream (offset 0)) &body body)
  `(let ((,buffer (make-input-buffer :vector ,vector :stream ,stream :pos ,offset)))
     ,@body))

 ;; READx and WRITEx
;;; WRITE-UNSIGNED-BE, READ-UNSIGNED-BE, etc taken from PACK, which is
;;; in the public domain.

(defmacro write-unsigned-be (value size buffer)
  (once-only (value buffer)
    `(progn
       ,@(loop for i from (* (1- size) 8) downto 0 by 8
               collect `(fast-write-byte (ldb (byte 8 ,i) ,value) ,buffer)))))

(defmacro read-unsigned-be (size buffer)
  (with-gensyms (value)
    (once-only (buffer)
      `(let ((,value 0))
         ,@(loop for i from (* (1- size) 8) downto 0 by 8
                 collect `(setf (ldb (byte 8 ,i) ,value) (fast-read-byte ,buffer)))
         ,value))))

(defmacro write-unsigned-le (value size buffer)
  (once-only (value buffer)
    `(progn
       ,@(loop for i from 0 below (* 8 size) by 8
               collect `(fast-write-byte (ldb (byte 8 ,i) ,value) ,buffer)))))

(defmacro read-unsigned-le (size buffer)
  (with-gensyms (value)
    (once-only (buffer)
      `(let ((,value 0))
         ,@(loop for i from 0 below (* 8 size) by 8
                 collect `(setf (ldb (byte 8 ,i) ,value) (fast-read-byte ,buffer)))
         ,value))))

(declaim (inline unsigned-to-signed))
(defun unsigned-to-signed (value size)
  (let ((max-signed (expt 2 (1- (* 8 size))))
        (to-subtract (expt 2 (* 8 size))))
    (if (>= value max-signed)
        (- value to-subtract)
        value)))

(declaim (inline signed-to-unsigned))
(defun signed-to-unsigned (value size)
  (if (minusp value)
      (+ value (expt 2 (* 8 size)))
      value))

(defmacro make-readers (&rest bitlens)
  (let ((names (mapcar (lambda (n)
                         (mapcar (lambda (m) (symbolicate (format nil m n)))
                                 '("READ~A-BE" "READU~A-BE"
                                   "READ~A-LE" "READU~A-LE")))
                       bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (inline ,@(flatten names)))
       ,@(loop for fun in names
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,(first fun) (buffer)
                    (unsigned-to-signed (read-unsigned-be ,bytes buffer) ,bytes))
                  (defun ,(second fun) (buffer)
                    (read-unsigned-be ,bytes buffer))
                  (defun ,(third fun) (buffer)
                    (unsigned-to-signed (read-unsigned-le ,bytes buffer) ,bytes))
                  (defun ,(fourth fun) (buffer)
                    (read-unsigned-le ,bytes buffer)))))))

(defmacro make-writers (&rest bitlens)
  (let ((names (mapcar (lambda (n)
                         (mapcar (lambda (m) (symbolicate (format nil m n)))
                                 '("WRITE~A-BE" "WRITEU~A-BE"
                                   "WRITE~A-LE" "WRITEU~A-LE")))
                       bitlens)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (notinline ,@(flatten names)))
       ,@(loop for fun in names
               for bits in bitlens
               as bytes = (truncate bits 8)
               collect
               `(progn
                  (defun ,(first fun) (value buffer)
                    (declare (type (signed-byte ,bits) value))
                    (write-unsigned-be (the (unsigned-byte ,bits)
                                            (signed-to-unsigned value ,bytes)) ,bytes buffer))
                  (defun ,(second fun) (value buffer)
                    (declare (type (unsigned-byte ,bits) value))
                    (write-unsigned-be (the (unsigned-byte ,bits) value)
                                       ,bytes buffer))
                  (defun ,(third fun) (value buffer)
                    (declare (type (signed-byte ,bits) value))
                    (write-unsigned-le (the (unsigned-byte ,bits)
                                            (signed-to-unsigned value ,bytes)) ,bytes buffer))
                  (defun ,(fourth fun) (value buffer)
                    (declare (type (unsigned-byte ,bits) value))
                    (write-unsigned-le (the (unsigned-byte ,bits) value)
                                       ,bytes buffer)))))))

(make-writers 16 24 32 64 128)
(make-readers 16 24 32 64 128)

(declaim (inline write8 writeu8 read8 readu8))
(defun write8 (value buffer)
  (declare (type (signed-byte 8) value))
  (fast-write-byte (signed-to-unsigned value 1) buffer))

(defun writeu8 (value buffer)
  (declare (type (unsigned-byte 8) value))
  (fast-write-byte value buffer))


(defun read8 (buffer)
  (unsigned-to-signed (fast-read-byte buffer) 1))

(defun readu8 (buffer)
  (fast-read-byte buffer))

(setf (symbol-function 'write8-le) #'write8)
(setf (symbol-function 'write8-be) #'write8)
(setf (symbol-function 'writeu8-le) #'writeu8)
(setf (symbol-function 'writeu8-be) #'writeu8)

(setf (symbol-function 'read8-le) #'read8)
(setf (symbol-function 'read8-be) #'read8)
(setf (symbol-function 'readu8-le) #'readu8)
(setf (symbol-function 'readu8-be) #'readu8)

;; fast-stream

(defclass fast-io-stream (fundamental-stream)
  ((openp :type boolean :initform t)))

(defmethod stream-file-position ((stream fast-io-stream) &optional position-spec)
  (with-slots (buffer) stream
    (cond (position-spec
           (setf (buffer-position buffer) position-spec))
          (t
           (buffer-position buffer)))))

(defmethod open-stream-p ((stream fast-io-stream))
  (slot-value stream 'openep))

 ;; fast-output-stream

(defclass fast-output-stream (fast-io-stream fundamental-output-stream)
  ((buffer :type output-buffer)))

(defmethod initialize-instance ((self fast-output-stream) &key stream
                                buffer-size &allow-other-keys)
  (call-next-method)
  (let ((*default-output-buffer-size* (or buffer-size *default-output-buffer-size*)))
    (with-slots (buffer) self
      (setf buffer (make-output-buffer :output stream)))))

(defmethod output-stream-p ((stream fast-output-stream))
  (with-slots (buffer) stream
    (and (typep buffer 'output-buffer))))

(defmethod stream-element-type ((stream fast-output-stream))
  "Return the underlying array element-type.
   Should always return '(unsigned-byte 8)."
  (with-slots (buffer) stream
    (array-element-type (output-buffer-vector buffer))))

(defmethod stream-write-byte ((stream fast-output-stream) byte)
  (with-slots (buffer) stream
    (fast-write-byte byte buffer)))

(defmethod stream-write-sequence ((stream fast-output-stream) sequence &optional start end)
  (with-slots (buffer) stream
    (fast-write-sequence sequence buffer start end))
  sequence)

(defun finish-output-stream (stream)
  (with-slots (buffer) stream
    (if (streamp (output-buffer-output buffer))
        (flush buffer)
        (finish-output-buffer buffer))))

(defmethod close ((stream fast-output-stream) &key abort)
  (declare (ignore abort))
  (finish-output-stream stream)
  (setf (slot-value stream 'openp) nil))

;; fast-input-stream ;; FIXME identical to vector stream

(defclass fast-input-stream (fast-io-stream fundamental-input-stream)
  ((buffer :type input-buffer)))

(defmethod initialize-instance ((self fast-input-stream) &key stream
                                vector &allow-other-keys)
  (call-next-method)
  (with-slots (buffer) self
    (setf buffer (make-input-buffer :vector vector :stream stream))))

(defmethod input-stream-p ((stream fast-input-stream))
  (with-slots (buffer) stream
    (and (typep buffer 'input-buffer))))

(defmethod stream-element-type ((stream fast-input-stream))
  "Return element-type of the underlying vector or stream.
   Return NIL if none are present."
  (with-slots (buffer) stream
    (if-let ((vec (input-buffer-vector buffer)))
      (array-element-type vec)
      (if-let ((stream (input-buffer-stream buffer)))
        (stream-element-type stream)))))

(defmethod peek-byte ((stream fast-input-stream) &optional peek-type (eof-error-p t) eof-value)
  (with-slots (buffer) stream
    (fast-peek-byte buffer peek-type eof-error-p eof-value)))

(defmethod stream-read-byte ((stream fast-input-stream))
  (with-slots (buffer) stream
    (fast-read-byte buffer)))

(defmethod stream-read-sequence ((stream fast-input-stream) sequence &optional start end)
  (with-slots (buffer) stream
    (fast-read-sequence sequence buffer start end)))

(defmethod close ((stream fast-input-stream) &key abort)
  (declare (ignore abort))
  (setf (slot-value stream 'openp) nil))



(defun copy-stream (input output &key (element-type (stream-element-type input))
                    (buffer-size 4096)
                    (buffer (make-array buffer-size :element-type element-type))
                    (start 0) end
                    finish-output)
  "Reads data from INPUT and writes it to OUTPUT. Both INPUT and OUTPUT must
be streams, they will be passed to READ-SEQUENCE and WRITE-SEQUENCE and must have
compatible element-types."
  (when (and end
             (< end start))
    (error "END is smaller than START in ~S" 'copy-stream))
  (let ((output-position 0)
        (input-position 0))
    (unless (zerop start)
      ;; FIXME add platform specific optimization to skip seekable streams
      (loop while (< input-position start)
            do (let ((n (read-sequence buffer input
                                       :end (min (length buffer)
                                                 (- start input-position)))))
                 (when (zerop n)
                   (error "~@<Could not read enough bytes from the input to fulfill ~
                           the :START ~S requirement in ~S.~:@>" 'copy-stream start))
                 (incf input-position n))))
    (assert (= input-position start))
    (loop while (or (null end) (< input-position end))
          do (let ((n (read-sequence buffer input
                                     :end (when end
                                            (min (length buffer)
                                                 (- end input-position))))))
               (when (zerop n)
                 (if end
                     (error "~@<Could not read enough bytes from the input to fulfill ~
                          the :END ~S requirement in ~S.~:@>" 'copy-stream end)
                     (return)))
               (incf input-position n)
               (write-sequence buffer output :end n)
               (incf output-position n)))
    (when finish-output
      (finish-output output))
    output-position))
