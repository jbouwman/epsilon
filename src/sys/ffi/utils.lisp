(in-package #:epsilon.sys.ffi)

(defun single-bit-p (integer)
  "Answer whether INTEGER, which must be an integer, is a single
set twos-complement bit."
  (if (<= integer 0)
      nil                            ; infinite set bits for negatives
      (loop until (logbitp 0 integer)
            do (setf integer (ash integer -1))
            finally (return (zerop (ash integer -1))))))

;;; This function is here because it needs to be defined early. It's
;;; used by DEFINE-PARSE-METHOD and DEFCTYPE to warn users when
;;; they're defining types whose names belongs to the KEYWORD or CL
;;; packages.  CFFI itself gets to use keywords without a warning.

(defun warn-if-kw-or-belongs-to-cl (name)
  (let ((package (symbol-package name)))
    (when (and (not (eq *package* (find-package '#:epsilon.sys.ffi)))
               (member package '(#:common-lisp #:keyword)
                       :key #'find-package))
      (warn "Defining a foreign type named ~S.  This symbol belongs to the ~A ~
             package and that may interfere with other code using FFI."
            name (package-name package)))))

(define-condition obsolete-argument-warning (style-warning)
  ((old-arg :initarg :old-arg :reader old-arg)
   (new-arg :initarg :new-arg :reader new-arg))
  (:report (lambda (c s)
             (format s "Keyword ~S is obsolete, please use ~S"
                     (old-arg c) (new-arg c)))))

(defun warn-obsolete-argument (old-arg new-arg)
  (warn 'obsolete-argument-warning
        :old-arg old-arg :new-arg new-arg))

(defun split-if (test seq &optional (dir :before))
  (remove-if #'(lambda (x) (equal x (subseq seq 0 0)))
             (loop for start fixnum = 0
                     then (if (eq dir :before)
                              stop
                              (the fixnum (1+ (the fixnum stop))))
                   while (< start (length seq))
                   for stop = (position-if test seq
                                           :start (if (eq dir :elide)
                                                      start
                                                      (the fixnum (1+ start))))
                   collect (subseq seq start
                                   (if (and stop (eq dir :after))
                                       (the fixnum (1+ (the fixnum stop)))
                                       stop))
                   while stop)))
