(defpackage #:lib.digest.public
  (:use
   #:cl
   #:sb-gray
   #:lib.type
   #:lib.digest.common
   #:lib.digest.generic
   #:lib.digest.macro
   #:lib.symbol)
  (:export
   #:buffer-index
   #:defdigest
   #:define-digest-finalizer
   #:define-digest-registers
   #:define-digest-updater
   #:digest-file
   #:digest-length
   #:digest-sequence
   #:digest-stream
   #:digest-supported-p
   #:digestp
   #:finalize-registers
   #:list-all-digests
   #:make-digest
   #:maybe-expand-one-shot-call
   #:mdx
   #:mdx-updater
   #:start
   #:end                                ; fixme
   #:produce-digest
   #:state
   #:store-data-length
   #:update-digest
   #:update-digest-from-stream
   #:update-digest-from-vector))

;;; defining digest (hash) functions

(in-package #:lib.digest.public)

(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +buffer-size+ (* 128 1024))
  (defconstant +seq-copy-buffer-size+ 512)
) ; EVAL-WHEN

(deftype buffer-index () `(integer 0 (,+buffer-size+)))

(defun update-digest-from-stream (digest stream &key buffer (start 0) end)
  (cond
    ((subtypep (stream-element-type stream) 'u8)
     (flet ((frob (read-buffer start end)
              (loop for last-updated = (read-sequence read-buffer stream
                                                      :start start :end end)
                 do (update-digest digest read-buffer
                                   :start start :end last-updated)
                 until (< last-updated end)
                 finally (return digest))))
       (if buffer
           (frob buffer start (or end (length buffer)))
           (let ((buffer (->u8 +buffer-size+)))
             (declare (dynamic-extent buffer))
             (frob buffer 0 +buffer-size+)))))
    (t
     (error 'ironclad-error
            :format-control "Unsupported stream element-type ~S for stream ~S."
            :format-arguments (list (stream-element-type stream) stream)))))

(declaim (inline update-digest-from-vector))

(defun update-digest-from-vector (digest vector start end)
  ;; SBCL and CMUCL have with-array-data, so copying can be avoided even
  ;; for non-simple vectors.
  (declare (type (vector ->u8) vector)
           (type array-index start end))
  (sb-kernel:with-array-data ((data vector) (real-start start) (real-end end))
   (declare (ignore real-end))
   (update-digest digest data :start real-start :end (+ real-start (- end start)))))

;;; Storing a length at the end of the hashed data is very common and
;;; can be a small bottleneck when generating lots of hashes over small
;;; quantities of data.  We assume that the appropriate locations have
;;; already been zeroed if necessary.  LENGTH is also assumed to be an
;;; (effectively) 64-bit quantity.
(declaim (inline store-data-length))
(defun store-data-length (block length offset &optional big-endian-p)
  (let ((lo (if big-endian-p (1+ offset) offset))
        (hi (if big-endian-p offset (1+ offset))))
    (setf (aref block lo) (ldb (byte 32 0) length)
          (aref block hi) (ldb (byte 32 32) length))))

;;; macros for "mid-level" functions

(defmacro define-digest-registers ((digest-name &key (endian :big) (size 4) (digest-registers nil)) &rest registers)
  (let* ((struct-name (symbolicate digest-name '#:-regs))
         (constructor (symbolicate '#:initial- struct-name))
         (copier      (symbolicate '#:copy- struct-name))
         (digest-fun  (symbolicate digest-name '#:-regs-digest))
         (register-bit-size (* size 8))
         (digest-size (* size (or digest-registers
                                  (length registers))))
         (ref-fun (ubref-fun-name register-bit-size (eq endian :big))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,struct-name
                      (:type (vector (unsigned-byte ,register-bit-size)))
                      (:constructor ,constructor ())
                      (:copier ,copier))
           ,@registers)
         (deftype ,struct-name ()
           '(simple-array (unsigned-byte ,register-bit-size) (,(length registers)))))
       (defun ,digest-fun (regs buffer start)
         (declare (type ,struct-name regs)
                  (type ->u8 buffer)
                  (type (integer 0 ,(- array-dimension-limit digest-size)) start)
                  ,(burn-baby-burn))
         ,(let ((inlined-unpacking
                  `(setf ,@(loop for (reg value) in registers
                                 for index from 0 below digest-size by size
                                 nconc `((,ref-fun buffer (+ start ,index))
                                         (,(symbolicate digest-name '#:-regs- reg)
                                          regs))))))
               (cond
                 #+(and sbcl :little-endian)
                 ((eq endian :little)
                  `(if (and (= start 0) (<= ,register-bit-size sb-vm:n-word-bits))
                       (sb-kernel:ub8-bash-copy regs 0 buffer 0 ,digest-size)
                       ,inlined-unpacking))
                 #+(and sbcl :big-endian)
                 ((eq endian :big)
                  `(if (and (= start 0) (<= ,register-bit-size sb-vm:n-word-bits))
                       (sb-kernel:ub8-bash-copy regs 0 buffer 0 ,digest-size)
                       ,inlined-unpacking))
                 (t inlined-unpacking)))
         buffer))))

(defmacro define-digest-updater (digest-name &body body)
  (destructuring-bind (maybe-doc-string &rest rest) body
    `(defmethod update-digest ((state ,digest-name) (sequence vector) &key (start 0) (end (length sequence)))
       ,@(when (stringp maybe-doc-string)
               `(,maybe-doc-string))
       ,(hold-me-back)
       (check-type sequence ->u8)
       (check-type start array-index)
       (check-type end array-index)
       ,@(if (stringp maybe-doc-string)
             rest
             body))))

;;; SPECS is either (DIGEST-NAME DIGEST-BYTES) or a list of the same.
;;; The latter spelling is for digests that are related, but have
;;; different output sizes (e.g. SHA2-512 and SHA2-384).  In that case,
;;; the first list is expected to be for the "major" variant of the
;;; pair; its digest type is expected to be the supertype of the
;;; variants.
(defmacro define-digest-finalizer (specs &body body)
  (let* ((single-digest-p (not (consp (car specs))))
         (specs (if single-digest-p (list specs) specs))
         (inner-fun-name (symbolicate '#:finalize- (caar specs) '#:-state)))
    (destructuring-bind (maybe-doc-string &rest rest) body
      (let ((primary-digest (caar specs)))
        `(defmethod produce-digest ((state ,primary-digest)
                                    &key digest (digest-start 0))
           ,@(when (stringp maybe-doc-string)
               `(,maybe-doc-string))
           (flet ((,inner-fun-name (state digest digest-start)
                    (macrolet ((finalize-registers (state regs)
                                 (declare (ignorable state))
                                 (let ((clauses
                                         (loop for (digest-name digest-length) in ',specs
                                               collect `(,digest-name
                                                         (,(symbolicate digest-name '#:-regs-digest)
                                                          ,regs digest digest-start)))))
                                   (if ,single-digest-p
                                       (second (first clauses))
                                       (list* 'etypecase state
                                              (reverse clauses))))))
                      ,@(if (stringp maybe-doc-string)
                            rest
                            body))))
             (let ((digest-size ,(if single-digest-p
                                     (second (first specs))
                                     `(etypecase state
                                        ,@(reverse specs))))
                   (state-copy (copy-digest state)))
               (etypecase digest
                 (->u8
                  ;; verify that the buffer is large enough
                  (if (<= digest-size (- (length digest) digest-start))
                      (,inner-fun-name state-copy digest digest-start)
                      (error 'insufficient-buffer-space
                             :buffer digest :start digest-start
                             :length digest-size)))
                 (null
                  (,inner-fun-name state-copy
                                   (make-array digest-size
                                               :element-type 'u8)
                                   0))))))))))

;;; common superclass (superstructure?) for MD5-style digest functions

(defstruct (mdx
             (:constructor nil)
             (:copier nil))
  ;; This is technically an (UNSIGNED-BYTE 61).  But the type-checking
  ;; penalties that imposes on a good 32-bit implementation are
  ;; significant.  We've opted to omit the type declaration here.  If
  ;; you really need to digest exabytes of data, I'm sure we can work
  ;; something out.
  (amount 0)
  ;; Most "64-bit" digest functions (e.g. SHA512) will need to override
  ;; this initial value in an &AUX.
  (buffer (->u8 64) :read-only t :type ->u8)
  ;; This fixed type should be big enough for "64-bit" digest functions.
  (buffer-index 0 :type (integer 0 128)))

(declaim (inline mdx-updater))
(defun mdx-updater (state compressor seq start end)
  (declare (type mdx state))
  (declare (type function compressor))
  (declare (type array-index start end))
  (let* ((buffer (mdx-buffer state))
         (buffer-index (mdx-buffer-index state))
         (buffer-length (length buffer))
         (length (- end start)))
    (declare (type fixnum length))
    (unless (zerop buffer-index)
      (let ((amount (min (- buffer-length buffer-index)
                         length)))
        (copy-to-buffer seq start amount buffer buffer-index)
        (setq start (+ start amount))
        (let ((new-index (logand (+ buffer-index amount)
                                 (1- buffer-length))))
          (when (zerop new-index)
            (funcall compressor state buffer 0))
          (when (>= start end)
            (setf (mdx-buffer-index state) new-index)
            (incf (mdx-amount state) length)
            (return-from mdx-updater state)))))
    (loop until (< (- end start) buffer-length)
          do (funcall compressor state seq start)
             (setq start (the fixnum (+ start buffer-length)))
          finally (return
                    (let ((amount (- end start)))
                      (unless (zerop amount)
                        (copy-to-buffer seq start amount buffer 0))
                      (setf (mdx-buffer-index state) amount)
                      (incf (mdx-amount state) length)
                      state)))))

(declaim (notinline mdx-updater))

;;; high-level generic function drivers

;;; These three functions are intended to be one-shot ways to digest
;;; an object of some kind.  You could write these in terms of the more
;;; familiar digest interface below, but these are likely to be slightly
;;; more efficient, as well as more obvious about what you're trying to
;;; do.

(declaim (notinline make-digest))

(defmethod digest-file ((digest-name cons) pathname &rest kwargs)
  (apply #'digest-file (apply #'make-digest digest-name) pathname kwargs))
(defmethod digest-file ((digest-name symbol) pathname &rest kwargs)
  (apply #'digest-file (make-digest digest-name) pathname kwargs))

(defmethod digest-file (state pathname &key buffer (start 0) end
                        digest (digest-start 0))
  (with-open-file (stream pathname :element-type 'u8
                                   :direction :input
                                   :if-does-not-exist :error)
    (update-digest-from-stream state stream
                               :buffer buffer :start start :end end)
    (produce-digest state :digest digest :digest-start digest-start)))

(defmethod digest-stream ((digest-name cons) stream &rest kwargs)
  (apply #'digest-stream (apply #'make-digest digest-name) stream kwargs))
(defmethod digest-stream ((digest-name symbol) stream &rest kwargs)
  (apply #'digest-stream (make-digest digest-name) stream kwargs))

(defmethod digest-stream (state stream &key buffer (start 0) end
                          digest (digest-start 0))
  (update-digest-from-stream state stream
                               :buffer buffer :start start :end end)
  (produce-digest state :digest digest :digest-start digest-start))

(defmethod digest-sequence ((digest-name symbol) sequence &rest kwargs)
  (apply #'digest-sequence (make-digest digest-name) sequence kwargs))

(defmethod digest-sequence (state sequence &key (start 0) end
                                             digest (digest-start 0))
  (declare (type array-index start))
  (check-type sequence (vector ->u8))
  (let ((end (or end (length sequence))))
    (update-digest-from-vector state sequence start end))
  (produce-digest state :digest digest :digest-start digest-start))

;;; These four functions represent the common interface for digests in
;;; other crypto toolkits (OpenSSL, Botan, Python, etc.).  You obtain
;;; some state object for a particular digest, you update it with some
;;; data, and then you get the actual digest.

(defun make-digest (digest-name &rest keys &key &allow-other-keys)
  "Return a digest object which uses the algorithm DIGEST-NAME."
  (typecase digest-name
    (symbol
     (let ((name (massage-symbol digest-name)))
       (if (digestp name)
           (apply (the function (get name '%make-digest)) keys)
           (error 'unsupported-digest :name digest-name))))
    (t
     (error 'type-error :datum digest-name :expected-type 'symbol))))

;;; the digest-defining macro

(defun digestp (sym)
  (get sym '%digest-length))

(defun list-all-digests ()
  (loop for symbol being each external-symbol of (find-package :lib.digest.digest)
     if (digestp symbol)
     collect (intern (symbol-name symbol) :keyword) into digests
     finally (return (sort digests #'string<))))

(defun digest-supported-p (name)
  "Return T if the digest NAME is a valid digest name."
  (and (symbolp name)
       (not (null (digestp (massage-symbol name))))))

(defmethod digest-length ((digest-name symbol))
  (or (digestp (massage-symbol digest-name))
      (error 'unsupported-digest :name digest-name)))

(defmethod digest-length (digest-name)
  (error 'unsupported-digest :name digest-name))

(defmethod update-digest (digester (stream stream) &key buffer (start 0) end
                          &allow-other-keys)
  (update-digest-from-stream digester stream
                             :buffer buffer :start start :end end))

(defun optimized-maker-name (name)
  (symbolicate '#:%make- name '#:-digest))

(defmacro defdigest (name &key digest-length block-length)
  (let ((optimized-maker-name (optimized-maker-name name)))
    `(progn
       (setf (get ',name '%digest-length) ,digest-length)
       (setf (get ',name '%make-digest) (symbol-function ',optimized-maker-name))
       (defmethod digest-length ((digest ,name))
         ,digest-length)
       (defmethod block-length ((digest ,name))
         ,block-length))))

;;; If we pass a constant argument to MAKE-DIGEST, convert the
;;; MAKE-DIGEST call to a direct call to the state creation function.
(define-compiler-macro make-digest (&whole form &environment env
                                           name &rest keys &key &allow-other-keys)
  (declare (ignore env))
  (cond
    ((or (keywordp name)
         (and (quotationp name) (symbolp name)))
     (let ((name (massage-symbol (unquote name))))
       (if (digestp name)
           `(,(optimized-maker-name name) ,@keys)
           form)))
    (t form)))

;;; And do the same for various one-shot digest functions.
(defun maybe-expand-one-shot-call (form funname name 2nd-arg keys)
  (cond
    ((or (keywordp name)
         (and (quotationp name) (symbolp name)))
     (let ((name (massage-symbol (unquote name))))
       (if (digestp name)
           `(,funname (,(optimized-maker-name name)) ,2nd-arg ,@keys)
           form)))
    (t form)))

(define-compiler-macro digest-sequence (&whole form &environment env
                                               name sequence &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-sequence name sequence keys))

(define-compiler-macro digest-stream (&whole form &environment env
                                             name stream &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-stream name stream keys))

(define-compiler-macro digest-file (&whole form &environment env
                                           name file &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-file name file keys))



;;; digesting streams

(defclass digesting-stream (fundamental-binary-output-stream)
  ((digest :initarg :digest :reader stream-digest)
   (buffer :initform (->u8 64)
           :reader stream-buffer)
   (position :initform 0
             :reader stream-buffer-position)))

(defmethod sb-gray::stream-element-type ((stream digesting-stream))
  'u8)

(defun make-digesting-stream (digest &rest args)
  (make-instance 'digesting-stream :digest (apply #'make-digest digest args)))

(defmethod sb-gray::stream-write-byte ((stream digesting-stream) byte)
  (declare (type u8 byte))
  (with-slots (digest buffer position) stream
    (setf (aref buffer position) byte)
    (when (= (incf position) 64)
      (update-digest digest buffer :start 0 :end 64)
      (setf position 0))
    byte))

(defmethod stream-write-sequence
           ((stream digesting-stream) seq &optional (start 0) end)
  (typecase seq
    (->u8
     (let ((end (or end (length seq))))
       (unless (zerop (stream-buffer-position stream))
         (update-digest (stream-digest stream) (stream-buffer stream) :end
                        (stream-buffer-position stream))
         (setf (slot-value stream 'position) 0))
       (update-digest (stream-digest stream) seq :start start :end end)
       seq))
    (t (call-next-method))))

(defmethod sb-gray::stream-clear-output ((stream digesting-stream))
  (with-slots (digest position) stream
    (setf position 0)
    (reinitialize-instance digest)
    nil))

(defmethod produce-digest ((stream digesting-stream)
                           &key digest (digest-start 0))
  (with-slots ((%digest digest) buffer position) stream
    (unless (zerop position)
      (update-digest %digest buffer :start 0 :end position)
      (setf position 0))
    (produce-digest %digest :digest digest :digest-start digest-start)))

(defun execute-with-digesting-stream (digest fn)
  (with-open-stream (stream (make-digesting-stream digest))
    (funcall fn stream)
    (produce-digest stream)))

(defmacro with-digesting-stream ((var digest &rest args) &body body)
  `(with-open-stream (,var (make-digesting-stream ,digest ,@args))
     ,@body
     (produce-digest ,var)))
