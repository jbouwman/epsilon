(defpackage #:epsilon.lib.type
  (:use
   #:cl
   #:epsilon.lib.symbol)
  (:export

   #:array-index
   #:array-index+1
   
   ;; unsigned 8
   
   #:u8
   #:u8-get
   #:u8-set
   #:->u8
   #:random-u8

   ;; unsigned 16

   #:u16
   #:->u16
   #:write-u16-msb
   #:read-u16/be
   #:read-u16/be-into-sequence
   #:read-u16/be-sequence
   #:read-u16/le
   #:read-u16/le-into-sequence
   #:read-u16/le-sequence
   #:u16ref/be
   #:u16ref/le
   #:write-u16/be
   #:write-u16/be-sequence
   #:write-u16/le
   #:write-u16/le-sequence

   ;; signed 16
   
   #:read-s16/be
   #:read-s16/be-into-sequence
   #:read-s16/be-sequence
   #:read-s16/le
   #:read-s16/le-into-sequence
   #:read-s16/le-sequence
   #:write-s16/be
   #:write-s16/be-sequence
   #:write-s16/le
   #:write-s16/le-sequence
   #:s16ref/be
   #:s16ref/le

   ;; unsigned 32
   
   #:u32
   #:->u32
   #:write-u8
   #:write-u32
   #:write-u32-msb
   #:read-u32/be
   #:read-u32/be-into-sequence
   #:read-u32/be-sequence
   #:read-u32/le
   #:read-u32/le-into-sequence
   #:read-u32/le-sequence
   #:u32ref/be
   #:u32ref/le
   #:write-u32/be
   #:write-u32/be-sequence
   #:write-u32/le
   #:write-u32/le-sequence

   ;; signed 32

   #:read-s32/be
   #:read-s32/be-into-sequence
   #:read-s32/be-sequence
   #:read-s32/le
   #:read-s32/le-into-sequence
   #:read-s32/le-sequence
   #:s32ref/be
   #:s32ref/le
   #:write-s32/be
   #:write-s32/be-sequence
   #:write-s32/le
   #:write-s32/le-sequence

   ;; unsigned 64

   #:u64
   #:->u64
   #:read-u64/be
   #:read-u64/be-into-sequence
   #:read-u64/be-sequence
   #:read-u64/le
   #:read-u64/le-into-sequence
   #:read-u64/le-sequence
   #:write-u64/be
   #:write-u64/be-sequence
   #:write-u64/le
   #:write-u64/le-sequence
   #:u64ref/be
   #:u64ref/le

   ;; signed 64

   #:read-s64/be
   #:read-s64/be-into-sequence
   #:read-s64/be-sequence
   #:read-s64/le
   #:read-s64/le-into-sequence
   #:read-s64/le-sequence
   #:write-s64/be
   #:write-s64/be-sequence
   #:write-s64/le
   #:write-s64/le-sequence
   #:s64ref/be
   #:s64ref/le))

(in-package #:epsilon.lib.type)

(deftype array-index (&optional (length array-dimension-limit))
  `(mod ,length))

(deftype array-index+1 (&optional (length array-dimension-limit))
  `(mod ,(1+ length)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-unsigned-type (bits)
    (let ((base (symbolicate '#:u (princ-to-string bits))))
      `(progn
         (deftype ,base ()
           '(unsigned-byte ,bits))
         (deftype ,(symbolicate '#:-> base) (&optional length)
           (let ((length (or length '*)))
             `(simple-array ,',base (,length))))
         (defun ,(symbolicate '#:-> base) (arg)
           (etypecase arg
             (array-index (make-array arg :element-type ',base))
             (sequence (make-array (length arg)
                                   :element-type ',base
                                   :initial-contents arg))))))))

;; u8

(define-unsigned-type 8)

(defgeneric write-u8 (stream u8))

(defun random-u8 (size)
  (->u8 (loop :for i :from 1 :to size
              :collect (random 256))))

;; u16

(define-unsigned-type 16)

(defun write-u16-msb (stream u16)
  (write-u8 stream (ldb (byte 8 8) u16))
  (write-u8 stream (ldb (byte 8 0) u16)))

;; u32

(define-unsigned-type 32)

(defun write-u32 (stream u32)
  (write-u8 stream (ldb (byte 8 0) u32))
  (write-u8 stream (ldb (byte 8 8) u32))
  (write-u8 stream (ldb (byte 8 16) u32))
  (write-u8 stream (ldb (byte 8 24) u32)))

(defun write-u32-msb (stream u32)
  (write-u8 stream (ldb (byte 8 24) u32))
  (write-u8 stream (ldb (byte 8 16) u32))
  (write-u8 stream (ldb (byte 8 8) u32))
  (write-u8 stream (ldb (byte 8 0) u32)))

;; u64

(define-unsigned-type 64)

(deftype index ()
  '(mod #.array-dimension-limit))

(eval-when (:compile-toplevel :load-toplevel)
  
(defun byte-fun-name (bitsize signedp big-endian-p desc)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A~D~A/~A"
                    (symbol-name (if signedp :s :u))
                    bitsize
                    (symbol-name desc)
                    (symbol-name (if big-endian-p :be :le))))))

(defun float-fun-name (float-type big-endian-p desc)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A-~A/~A"
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name desc)
                    (symbol-name (if big-endian-p :be :le))))))

(defun byte-ref-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p :ref))

(defun float-ref-fun-name (float-type big-endian-p)
  (float-fun-name float-type big-endian-p :ref))

(defun byte-set-fun-name (bitsize signedp big-endian-p)
  (byte-fun-name bitsize signedp big-endian-p :set))

(defun float-set-fun-name (float-type big-endian-p)
  (float-fun-name float-type big-endian-p :set))

(defun stream-ref-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A~D/~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name (if signedp :s :u))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))))))

(defun stream-float-ref-fun-name (float-type readp big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A-~A/~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))))))

(defun stream-seq-fun-name (bitsize readp signedp big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A~D/~A-~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name (if signedp :s :u))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :sequence)))))

(defun stream-float-seq-fun-name (float-type readp big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A-~A/~A-~A"
                    (symbol-name (if readp :read :write))
                    (symbol-name :ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :sequence)))))

(defun stream-into-seq-fun-name (bitsize signedp big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A~D/~A-~A"
                    (symbol-name :read)
                    (symbol-name (if signedp :s :u))
                    bitsize
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :into-sequence)))))

(defun stream-float-into-seq-fun-name (float-type big-endian-p)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (format nil "~A-~A/~A-~A"
                    (symbol-name :read-ieee)
                    (symbol-name float-type)
                    (symbol-name (if big-endian-p :be :le))
                    (symbol-name :into-sequence)))))

(defun internalify (s)
  (let ((*package* (find-package :epsilon.lib.type)))
    (intern (concatenate 'string "%" (string s)))))

(defun format-docstring (&rest args)
  (loop with docstring = (apply #'format nil args)
	for start = 0 then (when pos (1+ pos))
	for pos = (and start (position #\Space docstring :start start))
	while start
	collect (subseq docstring start pos) into words
	finally (return (format nil "~{~<~%~1,76:;~A~>~^ ~}"
				words))))

(defun ref-form (vector-name index-name byte-size signedp big-endian-p)
  "Return a form that fetches a SIGNEDP BYTE-SIZE value from VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P."
  (multiple-value-bind (low high increment compare)
      (if big-endian-p
          (values 0 (1- byte-size) 1 #'>)
          (values (1- byte-size) 0 -1 #'<))
    (do ((i (+ low increment) (+ i increment))
         (shift (* (- byte-size 2) 8) (- shift 8))
         (forms nil))
        ((funcall compare i high)
         `(let* ((high-byte (aref , vector-name
                                    (+ ,index-name ,low)))
                 ;; Would be great if we could just sign-extend along
                 ;; with the load, but this is as good as it gets in
                 ;; portable Common Lisp.
                 (signed-high ,(if signedp
                                   `(if (logbitp 7 high-byte)
                                        (- high-byte 256)
                                        high-byte)
                                   'high-byte))
                 (shifted-into-place
                  (ash signed-high ,(* (1- byte-size) 8))))
            (declare (type u8 high-byte))
            (declare (type (,(if signedp 'signed-byte 'unsigned-byte) 8)
                           signed-high))
            (logior shifted-into-place ,@(nreverse forms))))
      (push `(ash (aref ,vector-name
                        (+ ,index-name ,i))
                  ,shift)
            forms))))

(defun set-form (vector-name index-name value-name byte-size big-endian-p)
  "Return a form that stores a BYTE-SIZE VALUE-NAME into VECTOR-NAME,
starting at INDEX-NAME.  The value is stored in the vector according to
BIG-ENDIAN-P.  The form returns VALUE-NAME."
  `(progn
     ,@(loop for i from 1 to byte-size
             collect (let ((offset (if big-endian-p
                                       (- byte-size i)
                                       (1- i))))
                       `(setf (aref ,vector-name
                                    (+ ,index-name ,offset))
                              (ldb (byte 8 ,(* 8 (1- i))) ,value-name))))
     ,value-name))

)                                       ; eval-when


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

(defun read-n-bytes-into (stream n-bytes v)
  (declare (type (integer 2 8) n-bytes))
  (dotimes (i n-bytes v)
    ;; READ-SEQUENCE would likely be more efficient here, but it does
    ;; not have the semantics we want--in particular, the blocking
    ;; semantics of READ-SEQUENCE are potentially bad.  It's not clear
    ;; that READ-BYTE is any better here, though...
    (setf (aref v i) (read-byte stream))))

(declaim (inline read-byte* write-byte*))
(defun read-byte* (stream n-bytes reffer)
  (declare (type (integer 2 8) n-bytes))
  (let ((v (->u8 n-bytes)))
    (declare (dynamic-extent v))
    (read-n-bytes-into stream n-bytes v)
    (funcall reffer v 0)))

(defun write-byte* (integer stream n-bytes setter)
  (declare (type (integer 2 8) n-bytes))
  (let ((v (->u8 n-bytes)))
    (declare (dynamic-extent v))
    (funcall setter v 0 integer)
    (write-sequence v stream)
    integer))

(declaim (inline read-into-vector*))
(defun read-into-vector* (stream vector start end n-bytes reffer)
  (declare (type (integer 2 8) n-bytes)
           (type function reffer))
  (let ((v (->u8 n-bytes)))
    (declare (dynamic-extent v))
    (loop for i from start below end
	  do (read-n-bytes-into stream n-bytes v)
	     (setf (aref vector i) (funcall reffer v 0))
	  finally (return vector))))

(defun read-into-list* (stream list start end n-bytes reffer)
  (declare (type (integer 2 8) n-bytes)
           (type function reffer))
  (do ((end (or end (length list)))
       (v (->u8 n-bytes))
       (rem (nthcdr start list) (rest rem))
       (i start (1+ i)))
      ((or (endp rem) (>= i end)) list)
    (declare (dynamic-extent v))
    (read-n-bytes-into stream n-bytes v)
    (setf (first rem) (funcall reffer v 0))))

(declaim (inline read-fresh-sequence))
(defun read-fresh-sequence (result-type stream count
			    element-type n-bytes reffer)
  (ecase result-type
    (list
     (let ((list (make-list count)))
       (read-into-list* stream list 0 count n-bytes reffer)))
    (vector
     (let ((vector (make-array count :element-type element-type)))
       (read-into-vector* stream vector 0 count n-bytes reffer)))))

(defun write-sequence-with-writer (seq stream start end writer)
  (declare (type function writer))
  (etypecase seq
    (list
     (mapc (lambda (e) (funcall writer e stream))
	   (subseq seq start end))
     seq)
    (vector
     (loop with end = (or end (length seq))
	   for i from start below end
	   do (funcall writer (aref seq i) stream)
	   finally (return seq)))))

(defun read-into-sequence (seq stream start end n-bytes reffer)
  (declare (type (integer 2 8) n-bytes))
  (etypecase seq
    (list
     (read-into-list* stream seq start end n-bytes reffer))
    (vector
     (let ((end (or end (length seq))))
       (read-into-vector* stream seq start end n-bytes reffer)))))

#.(loop for i from 0 upto #b10111
        for bitsize = (ecase (ldb (byte 2 3) i)
                        (0 16)
                        (1 32)
                        (2 64))
        for readp = (logbitp 2 i)
        for signedp = (logbitp 1 i)
        for big-endian-p = (logbitp 0 i)
	for name = (stream-ref-fun-name bitsize readp signedp big-endian-p)
	for n-bytes = (truncate bitsize 8)
	for byte-fun = (if readp
			   (byte-ref-fun-name bitsize signedp big-endian-p)
			   (byte-set-fun-name bitsize signedp big-endian-p))
	for byte-arglist = (if readp '(stream) '(integer stream))
	for subfun = (if readp 'read-byte* 'write-byte*)
	for element-type = `(,(if signedp 'signed-byte 'unsigned-byte) ,bitsize)
        collect `(progn
		   ,@(when readp
		       `((declaim (ftype (function (t) (values ,element-type &optional)) ,name))))
		   (defun ,name ,byte-arglist
		     (,subfun ,@byte-arglist ,n-bytes #',byte-fun))) into forms
	if readp
	  collect `(defun ,(stream-seq-fun-name bitsize t signedp big-endian-p)
		       (result-type stream count)
		     ,(format-docstring "Return a sequence of type RESULT-TYPE, containing COUNT elements read from STREAM.  Each element is a ~D-bit ~:[un~;~]signed integer read in ~:[little~;big~]-endian order.  RESULT-TYPE must be either CL:VECTOR or CL:LIST.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (read-fresh-sequence result-type stream count
					  ',element-type ,n-bytes #',byte-fun)) into forms
	else
	  collect `(defun ,(stream-seq-fun-name bitsize nil signedp big-endian-p)
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Write elements from SEQ between START and END as ~D-bit ~:[un~;~]signed integers in ~:[little~;big~]-endian order to STREAM.  SEQ may be either a vector or a list.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (write-sequence-with-writer seq stream start end #',name)) into forms
	if readp
	  collect `(defun ,(stream-into-seq-fun-name bitsize signedp big-endian-p)
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Destructively modify SEQ by replacing the elements of SEQ between START and END with elements read from STREAM.  Each element is a ~D-bit ~:[un~;~]signed integer read in ~:[little~;big~]-endian order.  SEQ may be either a vector or a list.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					bitsize signedp big-endian-p)
		     (read-into-sequence seq stream start end ,n-bytes #',byte-fun)) into forms
        finally (return `(progn ,@forms)))

#.(loop for i from 0 upto #b111
	for float-type = (if (logbitp 2 i) 'double 'single)
	for readp = (logbitp 1 i)
	for big-endian-p = (logbitp 0 i)
	for name = (stream-float-ref-fun-name float-type readp big-endian-p)
	for n-bytes = (ecase float-type (double 8) (single 4))
	for single-fun = (if readp
			     (float-ref-fun-name float-type big-endian-p)
			     (float-set-fun-name float-type big-endian-p))
	for arglist = (if readp '(stream) '(float stream))
	for subfun = (if readp 'read-byte* 'write-byte*)
	for element-type = (ecase float-type (double 'double-float) (single 'single-float))
	collect `(defun ,name ,arglist
		   (,subfun ,@arglist ,n-bytes #',single-fun)) into forms
	if readp
	  collect `(defun ,(stream-float-seq-fun-name float-type t big-endian-p)
		       (result-type stream count)
		     ,(format-docstring "Return a sequence of type RESULT-TYPE, containing COUNT elements read from STREAM.  Each element is a ~A read in ~:[little~;big~]-endian byte order.  RESULT-TYPE must be either CL:VECTOR or CL:LIST.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					element-type big-endian-p)
		     (read-fresh-sequence result-type stream count
					  ',element-type ,n-bytes #',single-fun)) into forms
	else
	  collect `(defun ,(stream-float-seq-fun-name float-type nil big-endian-p)
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Write elements from SEQ between START and END as ~As in ~:[little~;big~]-endian byte order to STREAM.  SEQ may be either a vector or a list.  STREAM must have an element type of (UNSIGNED-BYTE 8)."
					element-type big-endian-p)
		     (write-sequence-with-writer seq stream start end #',name)) into forms
	if readp
	  collect `(defun ,(stream-float-into-seq-fun-name float-type big-endian-p)
		       (seq stream &key (start 0) end)
		     ,(format-docstring "Destructively modify SEQ by replacing the elements of SEQ between START and END with elements read from STREAM.  Each element is a ~A read in ~:[little~;big~]-endian byte order.  SEQ may be either a vector or a list.  STREAM must have na element type of (UNSIGNED-BYTE 8)."
					element-type big-endian-p)
		     (read-into-sequence seq stream start end ,n-bytes #',single-fun)) into forms
	finally (return `(progn ,@forms)))

