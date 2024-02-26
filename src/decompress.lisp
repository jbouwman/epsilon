(in-package :encode)

;;; This structure is never meant to be instantiated.  It exists only to
;;; provide common framework for other decompressors.

(defstruct (decompression-state
             (:constructor)
             (:conc-name dstate-))
  (state nil :type (or null function))
  (done nil)

  (input (->u8 1) :type ->u8)
  (input-start 0 :type (and fixnum (integer 0 *)))
  (input-index 0 :type (and fixnum (integer 0 *)))
  (input-end 0 :type (and fixnum (integer 0 *)))

  (output (->u8 1) :type ->u8)
  (output-start 0 :type (and fixnum (integer 0 *)))
  (output-index 0 :type (and fixnum (integer 0 *)))
  (output-end 0 :type (and fixnum (integer 0 *)))

  (checksum nil)

  ;; Bit buffer.
  (bits 0 :type u32)
  (n-bits 0 :type (integer 0 32)))

(defun make-dstate (format)
  "Return a structure suitable for uncompressing data in DATA-FORMAT;
DATA-FORMAT should be:

  :BZIP2    For decompressing data in the `bzip2' format;
  :GZIP     For decompressing data in the `gzip' format;
  :ZLIB     For decompressing data in the `zlib' format;
  :DEFLATE  For decompressing data in the `deflate' format.

The usual value of DATA-FORMAT will be one of :BZIP2 or :GZIP."
  (case format
    ((:deflate :zlib :gzip)
     (make-inflate-state format))
    ((:bzip2)
     (make-bzip2-state))
    (t
     (error 'invalid-format-error :format format))))

(defun finish-dstate (state)            ; FIXME dstate is jargony -- rename all -- is this generic?
  (unless (dstate-done state)
    (error 'stream-exhausted))
  t)

(defgeneric decompress (output state input &key &allow-other-keys)
  (:method (output format input &rest keys)
    (%decompress output format input keys))
  ;; Accommodate people who want to use lists as input, possibly for
  ;; experimenting with the API.
  (:method (output format (input list) &rest keys)
    (let ((vector (coerce input '(simple-array u8 (*)))))
      (%decompress output format vector keys))))

(defun %decompress (output format input keys)
  (let ((state (make-dstate format)))
    (multiple-value-prog1 (apply #'decompress output state input keys)
      (finish-dstate state))))

;;; SUBSEQ is specified to always make a copy.  But we don't want an
;;; exact copy of a freshly-consed vector; that'd be wasteful.
(defun maybe-subseq (v end)
  (if (= end (length v))
      v
      (subseq v 0 end)))

(defun decompress-fun-for-state (state)
  (typecase state
    (inflate-state #'%inflate)
    (bzip2-state #'%bzip2-decompress)))

;; For convenience.
(defun %decompress-from-pathname (output state pathname buffer-size)
  (with-open-file (stream pathname :element-type 'u8
                          :direction :input)
    (decompress output state stream
                :buffer-size (if (eq buffer-size :file-length)
                                 (file-length stream)
                                 buffer-size))))

(defmethod decompress ((output null) (state decompression-state) (input pathname)
                       &key)
  (%decompress-from-pathname output state input :file-length))

(defmethod decompress ((output pathname) (state decompression-state) (input pathname)
                       &key buffer-size)
  (check-type buffer-size (or null integer))
  (with-open-file (stream output :element-type 'u8
                          :direction :output)
    (%decompress-from-pathname stream state input buffer-size)))

(defmethod decompress ((output stream) (state decompression-state) (input pathname)
                       &key buffer-size)
  (check-type buffer-size (or null integer))
  (%decompress-from-pathname output state input buffer-size))

(defun %decompress/null-vector (state input fun
                                input-start input-end buffer-size)
  (declare (type function fun))
  (loop
    :with output = (->u8 buffer-size)
    :with output-start = 0
    do (cond
         ((= output-start (length output))
          ;; Reallocate the output buffer.
          (let ((new (->u8 (* 2 (length output)))))
            (setf output (replace new output))))
         (t
          (multiple-value-bind (consumed produced)
              (funcall fun state input output
                       :input-start input-start :input-end input-end
                       :output-start output-start :output-end (length output))
            (incf input-start consumed)
            (incf output-start produced)
            (when (or (dstate-done state)
                      (and (or (>= input-start input-end)
                               (zerop consumed))
                           (zerop produced)))
              (return-from %decompress/null-vector (maybe-subseq output output-start))))))))

(defmethod decompress ((output null) (state decompression-state) (input vector)
                       &key (input-start 0) input-end buffer-size)
  (%decompress/null-vector state input
                           (decompress-fun-for-state state)
                           input-start (or input-end (length input))
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/null-stream (state input fun buffer-size)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent input-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       with output = (make-array buffer-size :element-type 'u8)
       with output-start = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (cond
            ((= output-start (length output))
             ;; Reallocate the output buffer.
             (let ((new (make-array (* 2 (length output))
                                    :element-type 'u8)))
               (setf output (replace new output))))
            (t
             (multiple-value-bind (consumed produced)
                 (funcall fun state input-buffer output
                          :input-start input-start :input-end input-end
                          :output-start output-start)
               (incf input-start consumed)
               (incf output-start produced)
               (let ((input-consumed-p (>= input-start input-end)))
                 ;; Get more input if possible.
                 (when input-consumed-p
                   (setf input-start 0
                         input-end (read-sequence input-buffer input)))
                 (when (or (dstate-done state)
                           (and (or (and input-consumed-p (zerop input-end))
                                    (zerop consumed))
                                (zerop produced)))
                   (return-from %decompress/null-stream (maybe-subseq output output-start))))))))))

(defmethod decompress ((output null) (state decompression-state) (input stream)
                       &key buffer-size)
  (%decompress/null-stream state input
                           (decompress-fun-for-state state)
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/vector-vector (output state input fun
                                  input-start input-end
                                  output-start output-end)
  (declare (type ->u8 input output))
  (declare (type function fun))
  (loop
     with n-bytes-consumed = 0 and n-bytes-produced = 0
     do (multiple-value-bind (consumed produced)
            (funcall fun state input output
                     :input-start input-start :input-end input-end
                     :output-start output-start :output-end output-end)
          (incf input-start consumed)
          (incf output-start produced)
          (incf n-bytes-consumed consumed)
          (incf n-bytes-produced produced)
          (when (and (or (>= input-start input-end)
                         (zerop consumed))
                     (or (>= output-start output-end)
                         (zerop produced)))
            (return-from %decompress/vector-vector 
              (values n-bytes-consumed n-bytes-produced))))))

(defmethod decompress ((output vector) (state decompression-state) (input vector)
                       &key (input-start 0) input-end
                       (output-start 0) output-end)
  (%decompress/vector-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))
                             output-start (or output-end (length output))))

(defun %decompress/stream-vector (output state input fun input-start input-end)
  (declare (type function fun))
  (let ((buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent buffer))
    (loop (multiple-value-bind (consumed produced)
              (funcall fun state input buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence buffer output :end produced)
            (when (or (dstate-done state)
                      (and (or (>= input-start input-end)
                               (zerop consumed))
                           (zerop produced)))
              (return-from %decompress/stream-vector output))))))

(defmethod decompress ((output stream) (state decompression-state) (input vector)
                       &key (input-start 0) input-end)
  (%decompress/stream-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))))

(defun %decompress/stream-stream (output state input fun)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type 'u8))
        (output-buffer (make-array 8192 :element-type 'u8)))
    (declare (dynamic-extent input-buffer output-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (multiple-value-bind (consumed produced)
              (funcall fun state input-buffer output-buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence output-buffer output :end produced)
            (let ((input-consumed-p (>= input-start input-end)))
              (when input-consumed-p
                (setf input-start 0
                      input-end (read-sequence input-buffer input)))
              (when (or (dstate-done state)
                        (and (or (and input-consumed-p (zerop input-end))
                                 (zerop consumed))
                             (zerop produced)))
                (return-from %decompress/stream-stream output)))))))

(defmethod decompress ((output stream) (state decompression-state) (input stream)
                       &key)
  (%decompress/stream-stream output state input
                             (decompress-fun-for-state state)))
