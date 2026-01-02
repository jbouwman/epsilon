;;;; reader-extensions.lisp - Epsilon reader syntax extensions
;;;;
;;;; Provides extended syntax for Epsilon Lisp:
;;;; - [1 2 3]      -> vector literal
;;;; - {:a 1 :b 2}  -> HAMT map literal
;;;; - #{1 2 3}     -> HAMT set literal
;;;; - #f(+ % 1)    -> anonymous function shorthand
;;;; - #~"..."      -> string interpolation (via epsilon.interpolation)
;;;;
;;;; These extensions are enabled by default at boot time via
;;;; (enable-epsilon-syntax).

(defpackage :epsilon.reader
  (:use :cl)
  (:local-nicknames
   (map epsilon.map)
   (set epsilon.set)
   (interp epsilon.interpolation))
  (:export
   ;; Readtable management
   #:*epsilon-readtable*
   #:enable-epsilon-syntax
   #:disable-epsilon-syntax
   #:with-epsilon-syntax
   ;; Reader functions (for epsilon.edn to use)
   #:read-vector
   #:read-map
   #:read-set
   #:read-set-dispatch
   #:parse-anon-fn
   ;; Legacy compatibility
   #:install-reader-macros
   #:uninstall-reader-macros
   #:with-epsilon-reader))

(in-package :epsilon.reader)

;;;; ==========================================================================
;;;; Literal Reader Functions
;;;; ==========================================================================

(defun read-vector (stream char)
  "Read a vector literal: [1 2 3] -> #(1 2 3)"
  (declare (ignore char))
  (let ((forms (read-delimited-list #\] stream t)))
    (apply #'vector forms)))

(defun read-map (stream char)
  "Read a map literal: {:a 1 :b 2} -> (map:make-map :a 1 :b 2)"
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (unless (evenp (length forms))
      (error "Map literal must have an even number of forms"))
    (apply #'map:make-map forms)))

(defun read-set (stream char)
  "Read a set literal (used by dispatch macro)"
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (apply #'set:make-set forms)))

(defun read-set-dispatch (stream char n)
  "Dispatch macro for set literals: #{1 2 3}"
  (declare (ignore char n))
  (read-set stream #\{))

;;;; ==========================================================================
;;;; Anonymous Function Syntax
;;;; ==========================================================================

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

;;;; ==========================================================================
;;;; Readtable Management
;;;; ==========================================================================

(defvar *epsilon-readtable* nil
  "The Epsilon readtable with extended syntax for vectors, maps, sets,
   anonymous functions, and string interpolation.")

(defvar *original-readtable* nil
  "Saved readtable for restoration when disabling epsilon syntax.")

(defun initialize-epsilon-readtable ()
  "Create the Epsilon readtable with all syntax extensions."
  (unless *epsilon-readtable*
    (setf *epsilon-readtable* (copy-readtable nil))

    ;; Vector literals: [1 2 3]
    (set-macro-character #\[ #'read-vector nil *epsilon-readtable*)
    (set-macro-character #\] (get-macro-character #\) nil) nil *epsilon-readtable*)

    ;; Map literals: {:a 1 :b 2}
    (set-macro-character #\{ #'read-map nil *epsilon-readtable*)
    (set-macro-character #\} (get-macro-character #\) nil) nil *epsilon-readtable*)

    ;; Set literals: #{1 2 3}
    (set-dispatch-macro-character #\# #\{ #'read-set-dispatch *epsilon-readtable*)

    ;; Anonymous function: #f(+ % 1)
    (set-dispatch-macro-character #\# #\f #'parse-anon-fn *epsilon-readtable*)

    ;; String interpolation: #~"Hello, ~{name}!"
    (set-dispatch-macro-character #\# #\~ #'interp:parse-interpolated-string *epsilon-readtable*))

  *epsilon-readtable*)

;; Initialize the readtable when the file is loaded
(initialize-epsilon-readtable)

;;;; ==========================================================================
;;;; Public API
;;;; ==========================================================================

(defun enable-epsilon-syntax ()
  "Enable Epsilon reader syntax extensions.

   After calling this, the following syntax is available:
   - [1 2 3]             -> Creates a vector #(1 2 3)
   - {:a 1 :b 2}         -> Creates a HAMT map
   - #{1 2 3}            -> Creates a HAMT set
   - #f(+ % 1)           -> Creates an anonymous function
   - #~\"Hello, ~{x}!\"  -> String interpolation

   Returns T on success."
  (unless *original-readtable*
    (setf *original-readtable* (copy-readtable *readtable*)))
  (setf *readtable* *epsilon-readtable*)
  t)

(defun disable-epsilon-syntax ()
  "Disable Epsilon reader syntax extensions, restoring the original readtable.
   Returns T on success, NIL if syntax was not enabled."
  (when *original-readtable*
    (setf *readtable* (copy-readtable *original-readtable*))
    (setf *original-readtable* nil)
    t))

(defmacro with-epsilon-syntax (&body body)
  "Execute BODY with Epsilon reader syntax extensions enabled.
   Restores the original readtable after BODY completes.

   Example:
     (with-epsilon-syntax
       (read-from-string \"[1 2 3]\"))"
  (let ((saved (gensym "SAVED-READTABLE")))
    `(let ((,saved *readtable*))
       (unwind-protect
            (progn
              (setf *readtable* *epsilon-readtable*)
              ,@body)
         (setf *readtable* ,saved)))))

;;;; ==========================================================================
;;;; Legacy Compatibility (for epsilon.reader)
;;;; ==========================================================================

(defun install-reader-macros ()
  "Install epsilon reader extensions.
   Adds #f for anonymous function shorthand and #~ for string interpolation.

   Examples after installation:
     #f(+ % 1)              ; Single argument function
     #f(* %1 %2)            ; Two argument function
     #f(identity)           ; No argument function
     #~\"Hello, ~{name}!\"  ; String interpolation
     [1 2 3]                ; Vector literal
     {:a 1 :b 2}            ; Map literal
     #{1 2 3}               ; Set literal

   Returns T on success.

   Note: This is now an alias for enable-epsilon-syntax which enables
   all syntax extensions."
  (enable-epsilon-syntax))

(defun uninstall-reader-macros ()
  "Remove epsilon reader extensions, restoring original readtable.
   Returns T on success, NIL if no extensions were installed."
  (disable-epsilon-syntax))

(defmacro with-epsilon-reader (&body body)
  "Execute BODY with epsilon reader extensions enabled.
   Restores original readtable after BODY completes.

   Example:
     (with-epsilon-reader
       (read-from-string \"#f(+ % 1)\"))

   Note: This is now an alias for with-epsilon-syntax."
  `(with-epsilon-syntax ,@body))

;;; Enable syntax extensions when this file is loaded
(enable-epsilon-syntax)
