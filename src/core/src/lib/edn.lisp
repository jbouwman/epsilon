(defpackage epsilon.edn
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   read-edn
   read-edn-from-string
   enable-edn-syntax
   disable-edn-syntax
   *edn-readtable*))

(in-package epsilon.edn)

(defparameter *edn-readtable* nil)

(defun read-edn-map (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (unless (evenp (length forms))
      (error "Map literal must have an even number of forms"))
    (apply #'map:make-map forms)))

(defun read-edn-vector (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\] stream t)))
    (apply #'vector forms)))

(defun read-edn-set (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (apply #'map:make-map 
           (loop for form in forms
                 collect form
                 collect t))))

(defun read-edn-set-dispatch (stream char n)
  (declare (ignore char n))
  (read-edn-set stream #\{))

(defun enable-edn-syntax ()
  (setf *readtable* *edn-readtable*)
  (set-macro-character #\{ #'read-edn-map nil *edn-readtable*)
  (set-macro-character #\} (get-macro-character #\) nil) nil *edn-readtable*)
  (set-macro-character #\[ #'read-edn-vector nil *edn-readtable*)
  (set-macro-character #\] (get-macro-character #\) nil) nil *edn-readtable*)
  (set-dispatch-macro-character #\# #\{ #'read-edn-set-dispatch *edn-readtable*)
  t)

(defun disable-edn-syntax ()
  (setf *readtable* (copy-readtable nil))
  t)

(defun read-edn-from-string (string)
  (let ((*readtable* *edn-readtable*))
    (read-from-string string)))

(defun read-edn (stream)
  (let ((*readtable* *edn-readtable*))
    (read stream)))

(defun initialize-edn-readtable ()
  (unless *edn-readtable*
    (setf *edn-readtable* (copy-readtable nil))
    (set-macro-character #\{ #'read-edn-map nil *edn-readtable*)
    (set-macro-character #\} (get-macro-character #\) nil) nil *edn-readtable*)
    (set-macro-character #\[ #'read-edn-vector nil *edn-readtable*)
    (set-macro-character #\] (get-macro-character #\) nil) nil *edn-readtable*)
    (set-dispatch-macro-character #\# #\{ #'read-edn-set-dispatch *edn-readtable*)))

(initialize-edn-readtable)
