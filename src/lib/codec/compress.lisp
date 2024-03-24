(in-package #:lib.codec)

(defconstant +maximum-match-length+ 258
  "The maximum match length allowed.")

(defconstant +maximum-match-distance+ 32768
  "The maximum distance for a match.")

(declaim (inline match-length))
(defun match-length (p1 p2 input end)
  "Returns the length of the match between positions p1 and p2 in
INPUT; END is a sentinel position that ends the match length
check if reached."
  (declare (type input-index p1 p2 end)
           (type input-buffer input))
  (let ((length 0))
    (loop
     (when (or (/= (aref input p1) (aref input p2))
               (= length +maximum-match-length+)
               (= p1 end))
       (return length))
     (setf p1 (logand (1+ p1) #xFFFF)
           p2 (logand (1+ p2) #xFFFF)
           length (logand #xFFF (1+ length))))))

(defun longest-match (p1 input chains end max-tests)
  (declare (type input-index p1 end)
           (type input-buffer input)
           (type chains-buffer chains)
           (type (integer 0 32) max-tests))
  (let ((match-length 0)
        (p2 (aref chains p1))
        (test-count 0)
        (distance 0))
    (declare (type (integer 0 258) match-length)
             (type (integer 0 32) test-count))
    (loop
     (when (or (= match-length +maximum-match-length+)
               (= test-count max-tests)
               (= p2 p1)
               (= p2 (aref chains p2)))
       (return (values match-length distance)))
     (let ((step (logand (- p1 p2) #xFFFF)))
       (when (< +maximum-match-distance+ step)
         (return (values match-length distance)))
       (let ((possible-length (match-length p1 p2 input end)))
         (when (and (< 2 possible-length)
                    (< match-length possible-length))
           (setf distance step
                 match-length possible-length))
         (setf p2 (aref chains p2)))
       (incf test-count)))))

(defun compress (input chains start end
                 literal-fun length-fun distance-fun)
  (declare (type input-buffer input)
           (type chains-buffer chains)
           (type input-index start end)
           (type function literal-fun length-fun distance-fun))
  (let ((p start))
    (loop
     (when (= p end)
       (return))
     (multiple-value-bind (length distance)
         (longest-match p input chains end 4)
       (declare (type (integer 0 258) length)
                (type (integer 0 32768) distance))
       (cond ((zerop length)
              (funcall literal-fun (aref input p))
              (setf p (logand (+ p 1) #xFFFF)))
             (t
              (funcall length-fun length)
              (funcall distance-fun distance)
              (setf p (logand (+ p length) #xFFFF))))))))

(defun make-input ()
  (make-array 65536 :element-type 'u8))

(defun make-chains ()
  (make-array 65536
              :element-type 'u16
              :initial-element 0))

(defun make-hashes ()
  (make-array +hashes-size+
              :element-type 'u16
              :initial-element 0))

(defun error-missing-callback (&rest args)
  (declare (ignore args))
  (error "No callback given for compression"))

;;; FIXME: MERGE-INPUT is pretty ugly. It's the product of incremental
;;; evolution and experimentation. It should be cleaned up.
;;;
;;; Its basic purpose is to use octets from INPUT to fill up 32k-octet
;;; halves of the 64k-octet OUTPUT buffer. Whenever a half fills up,
;;; the COMPRESS-FUN is invoked to compress that half. At the end, a
;;; partial half may remain uncompressed to be either filled by a
;;; future call to MERGE-INPUT or to get flushed out by a call to
;;; FINAL-COMPRESS.

(defun merge-input (input start count output offset compress-fun)
  "Merge COUNT octets from START of INPUT into OUTPUT at OFFSET;
on reaching 32k boundaries within OUTPUT, call the COMPRESS-FUN
with OUTPUT, a starting offset, and the count of pending data."
  (declare (type ->u8 input output))
  (let ((i start)
        (j (+ start (min count (- +input-limit+ (mod offset +input-limit+)))))
        (result (logand +buffer-size-mask+ (+ offset count))))
    (dotimes (k (ceiling (+ (logand offset +input-limit-mask+) count)
                         +input-limit+))
      (when (plusp k)
        (funcall compress-fun
                 output
                 (logxor offset #x8000)
                 +input-limit+))
      (replace output input :start1 offset :start2 i :end2 j)
      (setf offset (logand +input-limit+ (+ offset +input-limit+)))
      (setf i j
            j (min (+ start count) (+ j +input-limit+))))
    (when (zerop (logand result +input-limit-mask+))
      (funcall compress-fun output (logxor offset #x8000) +input-limit+))
    result))

(defun make-huffman-writer (huffman-codes bitstream)
  (let ((codes (codes huffman-codes))
        (sizes (sizes huffman-codes))
        (buffer (buffer bitstream))
        (callback (callback bitstream)))
    (lambda (value)
      (setf (bits bitstream)
            (merge-bits (aref codes value)
                        (aref sizes value)
                        buffer
                        (bits bitstream)
                        callback)))))

(defun reinitialize-bitstream-funs (compressor bitstream)
  (setf (literal-fun compressor)
        (make-huffman-writer *fixed-huffman-codes* bitstream)
        (length-fun compressor)
        (make-huffman-writer *length-codes* bitstream)
        (distance-fun compressor)
        (make-huffman-writer *distance-codes* bitstream)
        (compress-fun compressor)
        (make-compress-fun compressor)))


;;; Class & protocol

(defclass deflate-compressor ()
  ((input
    :initarg :input
    :accessor input)
   (chains
    :initarg :chains
    :accessor chains)
   (hashes
    :initarg :hashes
    :accessor hashes)
   (start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (counter
    :initarg :counter
    :accessor counter)
   (octet-buffer
    :initarg :octet-buffer
    :accessor octet-buffer)
   (bitstream
    :initarg :bitstream
    :accessor bitstream)
   (literal-fun
    :initarg :literal-fun
    :accessor literal-fun)
   (length-fun
    :initarg :length-fun
    :accessor length-fun)
   (distance-fun
    :initarg :distance-fun
    :accessor distance-fun)
   (byte-fun
    :initarg :byte-fun
    :accessor byte-fun)
   (compress-fun
    :initarg :compress-fun
    :accessor compress-fun))
  (:default-initargs
   :input (make-input)
   :chains (make-chains)
   :hashes (make-hashes)
   :start 0
   :end 0
   :counter 0
   :bitstream (make-instance 'bitstream)
   :octet-buffer (->u8 1)))

;;; Methods

(defmethod initialize-instance :after ((compressor deflate-compressor)
                                       &rest initargs
                                       &key
                                       literal-fun length-fun distance-fun
                                       compress-fun
                                       callback)
  (declare (ignore initargs))
  (let ((bitstream (bitstream compressor)))
    (setf (callback bitstream)
          (or callback #'error-missing-callback))
    (setf (literal-fun compressor)
          (or literal-fun (make-huffman-writer *fixed-huffman-codes*
                                               bitstream)))
    (setf (length-fun compressor)
          (or length-fun (make-huffman-writer *length-codes*
                                              bitstream)))
    (setf (distance-fun compressor)
          (or distance-fun (make-huffman-writer *distance-codes*
                                                bitstream)))
    (setf (compress-fun compressor)
          (or compress-fun (make-compress-fun compressor)))
    (start-data-format compressor)))

;;; A few methods delegate to the bitstream

(defmethod (setf callback) (new-fun (compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (prog1
        (setf (callback bitstream) new-fun)
      (reinitialize-bitstream-funs compressor bitstream))))

(defmethod write-bits (code size (compressor deflate-compressor))
  (write-bits code size (bitstream compressor)))

(defmethod write-u8 ((compressor deflate-compressor) octet)
  (write-u8 (bitstream compressor) octet))

(defmethod write-u8-vector (vector (compressor deflate-compressor)
                            &key (start 0) end)
  (write-u8-vector vector (bitstream compressor)
                   :start start
                   :end end))

(defmethod start-data-format ((compressor deflate-compressor))
  (let ((bitstream (bitstream compressor)))
    (write-bits +final-block+ 1 bitstream)
    (write-bits +fixed-tables+ 2 bitstream)))

(defmethod compress-octet (octet compressor)
  (let ((vector (octet-buffer compressor)))
    (setf (aref vector 0) octet)
    (compress-u8-vector vector compressor)))

(defmethod compress-u8-vector (vector compressor &key (start 0) end)
  (let* ((closure (compress-fun compressor))
         (end (or end (length vector)))
         (count (- end start)))
    (let ((end
           (merge-input vector start count
                        (input compressor)
                        (end compressor)
                        closure)))
      (setf (end compressor) end
            (start compressor) (logand #x8000 end)
            (counter compressor) (logand #x7FFF end)))))

(defun hash-value (input position)
  (+ (* #.+rmax+ (aref input position))
     (* #.+radix+ (aref input (logand #.+input-mask+ (+ position 1))))
     (aref input (logand #.+input-mask+ (+ position 2)))))

(declaim (inline mod8191))
(defun mod8191 (z)
  (declare (type (integer 0 3057705) z))
  (let ((zz (+ (ash z -13) (logand #x1FFF z))))
    (if (< zz #x1FFF)
        zz
        (- zz #x1FFF))))

(defun update-chains (input hashes chains start count)
  (declare (type input-buffer input)
           (type hashes-buffer hashes)
           (type chains-buffer chains)
           (type input-index start)
           (type (integer 0 32768) count))
  (when (< count 3)
    (return-from update-chains))
  (let* ((hash (hash-value input start))
         (p0 start)
         (p1 (logand (+ start 2) #xFFFF)))
    (declare (type (integer 0 3057705) hash))
    (loop
     (let ((hash-index (mod8191 hash)))
       ;; Stuff the old hash index into chains at p0
       (setf (aref chains p0) (aref hashes hash-index))
       ;; Stuff p0 into the hashes
       (setf (aref hashes hash-index) p0)
       ;; Tentatively advance; if we hit the end, don't do the rest of
       ;; the hash update
       (setf p1 (logand (1+ p1) #xFFFF))
       (decf count)
       (when (= count 2)
         (return))
       ;; We're not at the end, so lop off the high, shift left, and
       ;; add the low to form a new hash value
       (setf hash (- hash (* (aref input p0) 11881)))
       (setf hash (* hash 109))
       (setf p0 (logand (1+ p0) #xFFFF))
       (setf hash (+ hash (aref input p1)))))))

(defmethod process-input ((compressor deflate-compressor) input start count)
  (update-chains input (hashes compressor) (chains compressor) start count))

(defmethod finish-data-format ((compressor deflate-compressor))
  (funcall (literal-fun compressor) 256))

(defmethod finish-compression ((compressor deflate-compressor))
  (final-compress compressor)
  (finish-data-format compressor)
  (flush (bitstream compressor)))

(defmethod final-compress ((compressor deflate-compressor))
  (let ((input (input compressor))
        (chains (chains compressor))
        (start (start compressor))
        (end (end compressor))
        (counter (counter compressor))
        (literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (process-input compressor input start counter)
    (compress input chains start end
              literal-fun
              length-fun
              distance-fun)))

(defmethod make-compress-fun ((compressor deflate-compressor))
  (let ((literal-fun (literal-fun compressor))
        (length-fun (length-fun compressor))
        (distance-fun (distance-fun compressor)))
    (lambda (input start count)
      (process-input compressor input start count)
      (let ((end (+ start count)))
        (compress input (chains compressor) start (logand #xFFFF end)
                  literal-fun
                  length-fun
                  distance-fun)))))

(defmacro with-compressor ((var class
                                &rest initargs
                                &key &allow-other-keys)
                           &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
     (multiple-value-prog1 
         (progn ,@body)
       (finish-compression ,var))))
