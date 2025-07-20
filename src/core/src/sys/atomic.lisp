(defpackage #:epsilon.sys.atomic
  (:use
   #:cl
   #:epsilon.symbol)
  (:export
   #:make-atomic-integer
   #:atomic-integer-compare-and-swap
   #:atomic-integer-decf
   #:atomic-integer-incf
   #:atomic-integer-value))

(in-package #:epsilon.sys.atomic)

(defmacro atomic-cas (place old new)
  (with-gensyms (tmp)
    `(let ((,tmp ,old))
       (eql ,tmp (sb-ext:compare-and-swap ,place ,old ,new)))))

(defmacro atomic-decf (place &optional (delta 1))
  `(- (sb-ext:atomic-decf ,place ,delta) ,delta))

(defmacro atomic-incf (place &optional (delta 1))
`(+ (sb-ext:atomic-incf ,place ,delta) ,delta))

(deftype %atomic-integer-value ()
  '(unsigned-byte 64))

(defstruct (atomic-integer
             (:constructor %make-atomic-integer ()))
  "Wrapper for an UNSIGNED-BYTE that allows atomic
increment, decrement and swap.
The counter is a machine word: 32/64 bits depending on CPU."
  (cell 0 :type %atomic-integer-value))

(defmethod print-object ((aint atomic-integer) stream)
  (print-unreadable-object (aint stream :type t :identity t)
    (format stream "~S" (atomic-integer-value aint))))

(defun make-atomic-integer (&key (value 0))
  "Create an `ATOMIC-INTEGER` with initial value `VALUE`"
  (check-type value %atomic-integer-value)
  (let ((aint (%make-atomic-integer)))
    (setf (atomic-integer-value aint) value)
    aint))

(defun atomic-integer-compare-and-swap (atomic-integer old new)
  "If the current value of `ATOMIC-INTEGER` is equal to `OLD`, replace
it with `NEW`.

Returns T if the replacement was successful, otherwise NIL."
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value old new))
  (atomic-cas (atomic-integer-cell atomic-integer)
              old new))

(defun atomic-integer-decf (atomic-integer &optional (delta 1))
  "Decrements the value of `ATOMIC-INTEGER` by `DELTA`.

Returns the new value of `ATOMIC-INTEGER`."
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta))
  (atomic-decf (atomic-integer-cell atomic-integer)
               delta))

(defun atomic-integer-incf (atomic-integer &optional (delta 1))
  "Increments the value of `ATOMIC-INTEGER` by `DELTA`.

Returns the new value of `ATOMIC-INTEGER`."
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value delta))
  (atomic-incf (atomic-integer-cell atomic-integer)
      delta))

(defun atomic-integer-value (atomic-integer)
  "Returns the current value of `ATOMIC-INTEGER`."
  (declare (type atomic-integer atomic-integer))
  (atomic-integer-cell atomic-integer))

(defun (setf atomic-integer-value) (newval atomic-integer)
  (declare (type atomic-integer atomic-integer)
           (type %atomic-integer-value newval))
  (setf (atomic-integer-cell atomic-integer)
        newval))
