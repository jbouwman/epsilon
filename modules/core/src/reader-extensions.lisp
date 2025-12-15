;;;; reader-extensions.lisp - Reader macros for functional patterns
;;;;
;;;; Provides concise syntax for anonymous functions, reducing verbosity
;;;; in common functional patterns.

(defpackage :epsilon.reader
  (:use :cl)
  (:export
   #:install-reader-macros
   #:uninstall-reader-macros
   #:with-epsilon-reader))

(in-package :epsilon.reader)

;;; Argument symbol handling

(defun arg-symbol-p (sym)
  "Check if SYM is an argument placeholder (%, %1, %2, etc.)"
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 0)
              (char= (char name 0) #\%)
              (or (= (length name) 1)  ; Just %
                  (and (> (length name) 1)
                       (every #'digit-char-p (subseq name 1))))))))

(defun arg-number (sym)
  "Extract the argument number from an arg symbol.
   % returns 1, %1 returns 1, %2 returns 2, etc."
  (let ((name (symbol-name sym)))
    (if (= (length name) 1)
        1  ; % is equivalent to %1
        (parse-integer (subseq name 1)))))

(defun extract-arg-symbols (form)
  "Extract all % argument symbols from a form, walking nested structures."
  (let ((args '()))
    (labels ((walk (x)
               (cond
                 ((arg-symbol-p x)
                  (pushnew x args :test #'eq))
                 ((consp x)
                  (walk (car x))
                  (walk (cdr x))))))
      (walk form)
      args)))

(defun find-max-arg (args)
  "Find the highest numbered argument from a list of arg symbols."
  (if (null args)
      0
      (reduce #'max (mapcar #'arg-number args))))

(defun uses-single-percent-p (args)
  "Check if the args use only the single % form (not numbered)."
  (and (= (length args) 1)
       (= (length (symbol-name (first args))) 1)))

(defun make-arg-symbol-in-package (n package)
  "Create an argument symbol for number N in PACKAGE."
  (intern (format nil "%~D" n) package))

;;; Reader macro implementation

(defun parse-anon-fn (stream char arg)
  "Parse #f(...) anonymous function syntax.

   % is the single argument, %1 %2 etc for multiple args.

   Examples:
     #f(+ % 1)        => (lambda (%) (+ % 1))
     #f(+ %1 %2)      => (lambda (%1 %2) (+ %1 %2))
     #f(list)         => (lambda () (list))"
  (declare (ignore char arg))
  (let* ((body (read stream t nil t))
         (args (extract-arg-symbols body))
         (max-arg (find-max-arg args)))
    (cond
      ;; No args - nullary function
      ((null args)
       `(lambda () ,body))
      ;; Single % - unary function using the actual symbol from body
      ((uses-single-percent-p args)
       `(lambda (,(first args)) ,body))
      ;; Numbered args %1, %2, etc - generate in same package as found symbols
      (t
       (let* ((sample-pkg (symbol-package (first args)))
              (params (loop for i from 1 to max-arg
                            collect (make-arg-symbol-in-package i sample-pkg))))
         `(lambda ,params ,body))))))

;;; Readtable management

(defvar *original-readtable* nil
  "Saved readtable for uninstall")

(defvar *epsilon-readtable* nil
  "The readtable with epsilon extensions installed")

(defun install-reader-macros ()
  "Install epsilon reader extensions.
   Adds #f for anonymous function shorthand.

   Examples after installation:
     #f(+ % 1)           ; Single argument
     #f(* %1 %2)         ; Two arguments
     #f(identity)        ; No arguments

   Returns T on success."
  (unless *original-readtable*
    (setf *original-readtable* (copy-readtable *readtable*)))
  (let ((rt (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\f #'parse-anon-fn rt)
    (setf *epsilon-readtable* rt)
    (setf *readtable* rt))
  t)

(defun uninstall-reader-macros ()
  "Remove epsilon reader extensions, restoring original readtable.
   Returns T on success, NIL if no extensions were installed."
  (when *original-readtable*
    (setf *readtable* (copy-readtable *original-readtable*))
    (setf *original-readtable* nil)
    (setf *epsilon-readtable* nil)
    t))

(defmacro with-epsilon-reader (&body body)
  "Execute BODY with epsilon reader extensions enabled.
   Restores original readtable after BODY completes.

   Example:
     (with-epsilon-reader
       (read-from-string \"#f(+ % 1)\"))"
  (let ((saved (gensym "SAVED-READTABLE")))
    `(let ((,saved *readtable*))
       (unwind-protect
            (progn
              (install-reader-macros)
              ,@body)
         (setf *readtable* ,saved)
         (setf *original-readtable* nil)))))
