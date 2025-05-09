(in-package #:epsilon.lib.char)

;;;; Character Encodings

(defclass character-encoding ()
  ((name :initarg :name :reader enc-name
         :initform (error "Must specify a NAME for this character encoding."))
   (documentation
    :initarg :documentation :reader enc-documentation :initform nil)
   (aliases :initarg :aliases :initform nil :reader enc-aliases)
   (code-unit-size
    :initarg :code-unit-size :reader enc-code-unit-size :initform 8)
   (max-units-per-char
    :initarg :max-units-per-char :reader enc-max-units-per-char :initform 1)
   (native-endianness
    :initarg :native-endianness :reader enc-native-endianness :initform t)
   (decode-literal-code-unit-limit
    :initarg :decode-literal-code-unit-limit :initform 0
    :reader enc-decode-literal-code-unit-limit)
   (encode-literal-code-unit-limit
    :initarg :encode-literal-code-unit-limit :initform 0
    :reader enc-encode-literal-code-unit-limit)
   (use-bom :initarg :use-bom :initform nil :reader enc-use-bom)
   (bom-encoding
    :initarg :bom-encoding :reader enc-bom-encoding :initform nil)
   (nul-encoding
    :initarg :nul-encoding :reader enc-nul-encoding :initform #(0))
   (default-replacement
    :initarg :default-replacement :reader enc-default-replacement
    :initform #x1a)
   (ambiguous
    :initarg :ambiguous :reader ambiguous-encoding-p :initform nil)))

(defmethod initialize-instance :after ((enc character-encoding)
                                       &key literal-char-code-limit)
  (when literal-char-code-limit
    (setf (slot-value enc 'encode-literal-code-unit-limit)
          literal-char-code-limit)
    (setf (slot-value enc 'decode-literal-code-unit-limit)
          literal-char-code-limit)))

(defvar *supported-character-encodings* nil)

(defun list-character-encodings ()
  "List of keyword symbols denoting supported character
encodings.  This list does not include aliases."
  *supported-character-encodings*)

(defvar *character-encodings* map:+empty+)

(defvar *default-character-encoding* :utf-8
  "Special variable used to determine the default character
encoding.")

(defun get-character-encoding (name)
  "Lookups the character encoding denoted by the keyword symbol
NAME.  Signals an error if one is not found.  If NAME is already
a CHARACTER-ENCONDING object, it is returned unmodified."
  (when (typep name 'character-encoding)
    (return-from get-character-encoding name))
  (when (eq name :default)
    (setq name *default-character-encoding*))
  (or (map:get *character-encodings* name)
      (error "Unknown character encoding: ~S" name)))

(defmethod ambiguous-encoding-p ((encoding symbol))
  (ambiguous-encoding-p (get-character-encoding encoding)))

(defun notice-character-encoding (enc)
  (pushnew (enc-name enc) *supported-character-encodings*)
  (dolist (kw (cons (enc-name enc) (enc-aliases enc)))
    (setf *character-encodings*
          (map:assoc *character-encodings* kw enc)))
  (enc-name enc))

(defmacro define-character-encoding (name docstring &body options)
  `(notice-character-encoding
    (make-instance 'character-encoding :name ,name ,@options
                   :documentation ,docstring)))

;;;; Mappings

;;; TODO: describe what mappings are

(defun make-fixed-width-counter (getter type &optional (unit-size-in-bits 8))
  (declare (ignore getter type))
  (check-type unit-size-in-bits fixnum)
  (let ((unit-size-in-bytes (/ unit-size-in-bits 8)))
    `(named-lambda fixed-width-counter (seq start end max)
       (declare (ignore seq) (fixnum start end max))
       ;; XXX: the result can be bigger than a fixnum when (> unit-size
       ;; 1) and we don't want that to happen. Possible solution: signal
       ;; a warning (hmm, make that an actual error) and truncate.
       (if (plusp max)
           (let ((count (the fixnum (min (floor max ,unit-size-in-bytes)
                                         (the fixnum (- end start))))))
             (values (the fixnum (* count ,unit-size-in-bytes))
                     (the fixnum (+ start count))))
           (values (the fixnum (* (the fixnum (- end start))
                                  ,unit-size-in-bytes))
                   (the fixnum end))))))

;;; Useful to develop new encodings incrementally starting with octet
;;; and code-unit counters.
(defun make-dummy-coder (sg st ds dt)
  (declare (ignore sg st ds dt))
  `(named-lambda dummy-coder (src s e dest i)
     (declare (ignore src s e dest i))
     (error "this encoder/decoder hasn't been implemented yet")))

(defclass abstract-mapping ()
  ((encoder-factory :accessor encoder-factory :initform 'make-dummy-coder)
   (decoder-factory :accessor decoder-factory :initform 'make-dummy-coder)
   (octet-counter-factory :accessor octet-counter-factory
                          :initform 'make-fixed-width-counter)
   (code-point-counter-factory :accessor code-point-counter-factory
                               :initform 'make-fixed-width-counter)))

(defclass concrete-mapping ()
  ((encoder :accessor encoder)
   (decoder :accessor decoder)
   (octet-counter :accessor octet-counter)
   (code-point-counter :accessor code-point-counter)))

(defparameter *abstract-mappings* map:+empty+)

(defun get-abstract-mapping (encoding)
  (map:get *abstract-mappings* encoding))

(defun (setf get-abstract-mapping) (value encoding)
  (setf *abstract-mappings*
        (map:assoc *abstract-mappings* encoding value)))

(defun %register-mapping-part (encoding slot-name fn)
  (let ((mapping (get-abstract-mapping encoding)))
    (unless mapping
      (setq mapping (make-instance 'abstract-mapping))
      (setf (get-abstract-mapping encoding) mapping))
    (setf (slot-value mapping slot-name) fn)))

(defmacro define-encoder (encoding (sa st da dt) &body body)
  `(%register-mapping-part ,encoding 'encoder-factory
                           (named-lambda encoder (,sa ,st ,da ,dt)
                             ,@body)))

(defmacro define-decoder (encoding (sa st da dt) &body body)
  `(%register-mapping-part ,encoding 'decoder-factory
                           (named-lambda decoder (,sa ,st ,da ,dt)
                             ,@body)))

(defmacro define-octet-counter (encoding (acc type) &body body)
  `(%register-mapping-part ,encoding 'octet-counter-factory
                           (named-lambda octet-counter-factory (,acc ,type)
                             ,@body)))

(defmacro define-code-point-counter (encoding (acc type) &body body)
  `(%register-mapping-part ,encoding 'code-point-counter-factory
                           (named-lambda code-point-counter (,acc ,type)
                             ,@body)))

(defun instantiate-encoder (encoding am octet-seq-getter octet-seq-type
                            code-point-seq-setter code-point-seq-type)
  (declare (ignore encoding))
  (funcall (encoder-factory am)
           octet-seq-getter
           octet-seq-type
           code-point-seq-setter
           code-point-seq-type))

(defun instantiate-decoder (encoding am octet-seq-getter octet-seq-type
                            code-point-seq-setter code-point-seq-type)
  (declare (ignore encoding))
  (funcall (decoder-factory am)
           octet-seq-getter
           octet-seq-type
           code-point-seq-setter
           code-point-seq-type))

(defun instantiate-code-point-counter (encoding am octet-seq-getter
                                       octet-seq-type)
  (declare (ignore encoding))
  (funcall (code-point-counter-factory am)
           octet-seq-getter
           octet-seq-type))

(defun instantiate-octet-counter (encoding am code-point-seq-getter
                                  code-point-seq-type)
  (if (= 1 (enc-max-units-per-char encoding))
      (make-fixed-width-counter code-point-seq-getter code-point-seq-type
                                (enc-code-unit-size encoding))
      (funcall (octet-counter-factory am)
               code-point-seq-getter
               code-point-seq-type)))

;;; Expands into code generated by the available abstract mappings
;;; that will be compiled into concrete mappings.  This is used in
;;; e.g. strings.lisp to define mappings between strings and
;;; u8 vectors.
;;;
;;; For each encoding funcall the abstract mappings at macro-expansion
;;; time with the src/dest accessors and types to generate the
;;; appropriate code for the concrete mappings. These functions are
;;; then saved in their respective slots of the CONCRETE-MAPPING
;;; object.
(defmacro instantiate-concrete-mappings
    (&key (encodings (map::keys *abstract-mappings*))
     (optimize '((speed 3) (debug 0) (compilation-speed 0)))
     octet-seq-getter octet-seq-setter octet-seq-type
     code-point-seq-getter code-point-seq-setter code-point-seq-type
     (instantiate-decoders t))
  `(let ((ht map:+empty+))
     (declare (optimize ,@optimize)
              #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
     (flet ((notice-mapping (encoding-name cm)
              (let* ((encoding (get-character-encoding encoding-name))
                     (aliases (enc-aliases encoding)))
                (dolist (kw (cons (enc-name encoding) aliases))
                  (setf ht (map:assoc ht kw cm))))))
       ,@(loop for encoding-name in encodings
               for encoding = (get-character-encoding encoding-name)
               for am = (map:get *abstract-mappings* encoding-name)
               collect
               `(let ((cm (make-instance 'concrete-mapping)))
                  (setf (encoder cm)
                        ,(instantiate-encoder encoding am
                                              code-point-seq-getter
                                              code-point-seq-type
                                              octet-seq-setter
                                              octet-seq-type))
                  ,(when instantiate-decoders
                     `(progn
                        (setf (decoder cm)
                              ,(instantiate-decoder encoding am
                                                    octet-seq-getter
                                                    octet-seq-type
                                                    code-point-seq-setter
                                                    code-point-seq-type))
                        (setf (code-point-counter cm)
                              ,(instantiate-code-point-counter
                                encoding am octet-seq-getter octet-seq-type))))
                  (setf (octet-counter cm)
                        ,(instantiate-octet-counter encoding am
                                                    code-point-seq-getter
                                                    code-point-seq-type))
                  (notice-mapping ,encoding-name cm))))
     ht))

;;;; Utilities used in enc-*.lisp

(defconstant +default-substitution-code-point+ #x1a
  "Default ASCII substitution character code point used in case of an encoding/decoding error.")

;;; We're converting between objects of the u8 and (MOD #x110000)
;;; types which are aliased here to U8 and CODE-POINT for convenience.

(deftype code-point () '(mod #x110000))

;;; Utility macro around DEFINE-ENCODER that takes care of most of the
;;; work need to deal with an 8-bit, fixed-width character encoding.
;;;
;;; BODY will be inside a loop and its return value will placed in the
;;; destination buffer.  BODY will be surounded by lexical BLOCK which
;;; will have the ENCODING's name, usually a keyword.  It handles all
;;; sorts of type declarations.
;;;
;;; See enc-ascii.lisp for a simple usage example.
(defmacro define-unibyte-encoder (encoding (code) &body body)
  (with-unique-names (s-getter s-type d-setter d-type
                      src start end dest d-start i di)
    `(define-encoder ,encoding (,s-getter ,s-type ,d-setter ,d-type)
       `(named-lambda ,',(symbolicate encoding '#:-unibyte-encoder)
            (,',src ,',start ,',end ,',dest ,',d-start)
          (declare (type ,,s-type ,',src)
                   (type ,,d-type ,',dest)
                   (fixnum ,',start ,',end ,',d-start))
          (loop for ,',i fixnum from ,',start below ,',end
                and ,',di fixnum from ,',d-start do
                (,,d-setter
                 (macrolet
                     ;; this should probably be a function...
                     ((handle-error (&optional (c ''character-encoding-error))
                        `(encoding-error
                          ,',',code ,',',encoding ,',',src ,',',i
                          +default-substitution-code-point+ ,c)))
                   (let ((,',code (,,s-getter ,',src ,',i)))
                     (declare (type code-point ,',code))
                     (block ,',encoding ,@',body)))
                 ,',dest ,',di)
                finally (return (the fixnum (- ,',di ,',d-start))))))))

;;; The decoder version of the above macro.
(defmacro define-unibyte-decoder (encoding (octet) &body body)
  (with-unique-names (s-getter s-type d-setter d-type
                      src start end dest d-start i di)
    `(define-decoder ,encoding (,s-getter ,s-type ,d-setter ,d-type)
       `(named-lambda ,',(symbolicate encoding '#:-unibyte-encoder)
            (,',src ,',start ,',end ,',dest ,',d-start)
          (declare (type ,,s-type ,',src)
                   (type ,,d-type ,',dest)
                   (fixnum ,',start ,',end ,',d-start))
          (loop for ,',i fixnum from ,',start below ,',end
                and ,',di fixnum from ,',d-start do
                (,,d-setter
                 (macrolet
                     ;; this should probably be a function...
                     ((handle-error (&optional (c ''character-decoding-error))
                        `(decoding-error
                          (vector ,',',octet) ,',',encoding ,',',src ,',',i
                          +default-substitution-code-point+ ,c)))
                   (let ((,',octet (,,s-getter ,',src ,',i)))
                     (declare (type u8 ,',octet))
                     (block ,',encoding ,@',body)))
                 ,',dest ,',di)
                finally (return (the fixnum (-  ,',di ,',d-start))))))))

;;;; Error Conditions
;;;
;;; For now, we don't define any actual restarts.  The only mechanism
;;; for "restarting" a coding error is the
;;; *SUPPRESS-CHARACTER-CODING-ERRORS* special variable which, when
;;; bound to T (the default), suppresses any error and uses a default
;;; replacement character instead.
;;;
;;; If it turns out that other more options are necessary, possible
;;; alternative approaches include:
;;;
;;;   a) use a *REPLACEMENT-CHARACTER* special variable that lets us
;;;      pick our own replacement character.  The encoder must do
;;;      additional work to check if this is character is encodable.
;;;
;;;   b) offer a restart to pick a replacement character.  Same
;;;      problem as above.
;;;
;;; Both approaches pose encoding problems when dealing with a
;;; variable-width encodings because different replacement characters
;;; will need different numbers of octets.  This is not a problem for
;;; UTF but will be a problem for the CJK charsets.  Approach (a) is
;;; nevertheless easier since the replacement character is known in
;;; advance and therefore the octet-counter can account for it.
;;;
;;; For more complex restarts like SBCL's -- that'll let you specify
;;; _several_ replacement characters for a single character error --
;;; will probably need extra support code outside the encoder/decoder
;;; (i.e. in the string-to-u8 function, for example) since the
;;; encoders/decoders deal with pre-allocated fixed-length buffers.
;;;
;;; SBCL has ASCII-specific (MALFORMED-ASCII) and UTF8-specific
;;; errors.  Why?  Do we want to add some of those too?

;;; FIXME: We used to deal with this with an extra ERRORP argument for
;;; encoders, decoders, etc...  Still undecided on the best way to do
;;; it.  We could also use a simple restart instead of this...
;;;
;;; In any case, this is not for the users to bind and it's not
;;; exported from the lib.char package.

(defvar *suppress-character-coding-errors* nil
  "If non-NIL, encoding or decoding errors are suppressed and the
the current character encoding's default replacement character is
used.")

;;; All error conditions are subtypes of
;;; CHARACTER-CODING-ERROR.  This error hierarchy is based on SBCL's.

(define-condition character-coding-error (error)
  ((buffer :initarg :buffer :reader character-coding-error-buffer)
   (position :initarg :position :reader character-coding-error-position)
   (encoding :initarg :encoding :reader character-coding-error-encoding)))

(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code))
  (:report (lambda (c s)
             (format s "Unable to encode character code point ~A as ~S."
                     (character-encoding-error-code c)
                     (character-coding-error-encoding c)))))

(declaim (inline encoding-error))
(defun encoding-error (code enc buf pos &optional
                       (sub +default-substitution-code-point+)
                       (e 'character-encoding-error))
  (unless *suppress-character-coding-errors*
    (error e :encoding enc :buffer buf :position pos :code code))
  sub)

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets))
  (:report (lambda (c s)
             (format s "Illegal ~S character starting at position ~D."
                     (character-coding-error-encoding c)
                     (character-coding-error-position c)))))

(define-condition end-of-input-in-character (character-decoding-error)
  ()
  (:documentation "Signalled by DECODERs or CODE-POINT-COUNTERs
of variable-width character encodings."))

(define-condition character-out-of-range (character-decoding-error)
  ()
  (:documentation
   "Signalled when the character being decoded is out of range."))

(declaim (inline decoding-error))
(defun decoding-error (octets enc buf pos &optional
                       (sub +default-substitution-code-point+)
                       (e 'character-decoding-error))
  (unless *suppress-character-coding-errors*
    (error e :octets octets :encoding enc :buffer buf :position pos))
  sub)
