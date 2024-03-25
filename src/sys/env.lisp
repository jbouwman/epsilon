(defpackage #:sys.env
  (:use #:cl)
  (:export #:featurep
           #:getenv
           #:getenvp))

(in-package #:sys.env)

(defun featurep (x &optional (*features* *features*))
  "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
  (cond ((atom x)
         (and (member x *features*) t))
        ((eq :not (car x))
         (assert (null (cddr x))) (not (featurep (cadr x))))
        ((eq :or (car x))
         (some #'featurep (cdr x)))
        ((eq :and (car x))
         (every #'featurep (cdr x)))
        (t
         (error "~S: malformed feature specification ~S" 'featurep x))))

(defun getenv (x)
  "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
  (sb-ext:posix-getenv x))

(defun getenvp (x)
  "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
  (let ((g (getenv x)))
    (and (not (= 0 (length g)))
         g)))
