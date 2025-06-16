(defpackage #:epsilon.sys.thread
  (:use
   #:cl
   #:epsilon.lib.function
   #:epsilon.sys.error)
  (:export
   #:all-threads
   #:current-thread
   #:destroy-thread
   #:error-in-thread
   #:interrupt-thread
   #:join-thread
   #:make-thread
   #:signal-in-thread
   #:thread
   #:thread-alive-p
   #:thread-name
   #:thread-p
   #:thread-yield
   #:warn-in-thread
   #:*default-special-bindings*
   #:*standard-io-bindings*))

(in-package #:epsilon.sys.thread)

(defun all-threads ()
  "Returns a sequence of all of the threads. This may not
  be freshly-allocated, so the caller should not modify it."
  (sb-thread:list-all-threads))

(defun %make-thread (function name)
  (sb-thread:make-thread function :name name))

(defvar *default-special-bindings* nil
  "This variable holds an alist associating special variable symbols
  to forms to evaluate. Special variables named in this list will
  be locally bound in the new thread before it begins executing user code.

  This variable may be rebound around calls to MAKE-THREAD to
  add/alter default bindings. The effect of mutating this list is
  undefined, but earlier forms take precedence over later forms for
  the same symbol, so defaults may be overridden by consing to the
  head of the list.")

(defmacro defbindings (name docstring &body initforms)
  (check-type docstring string)
  `(defparameter ,name
     (list
      ,@(loop for (special form) in initforms
              collect `(cons ',special ',form)))
     ,docstring))

;; Forms are evaluated in the new thread or in the calling thread?

(defbindings *standard-io-bindings*
  "Standard bindings of printer/reader control variables as per CL:WITH-STANDARD-IO-SYNTAX."
  (*package*                   (find-package :common-lisp-user))
  (*print-array*               t)
  (*print-base*                10)
  (*print-case*                :upcase)
  (*print-circle*              nil)
  (*print-escape*              t)
  (*print-gensym*              t)
  (*print-length*              nil)
  (*print-level*               nil)
  (*print-lines*               nil)
  (*print-miser-width*         nil)
  (*print-pprint-dispatch*     (copy-pprint-dispatch nil))
  (*print-pretty*              nil)
  (*print-radix*               nil)
  (*print-readably*            t)
  (*print-right-margin*        nil)
  (*random-state*              (make-random-state t))
  (*read-base*                 10)
  (*read-default-float-format* 'single-float)
  (*read-eval*                 t)
  (*read-suppress*             nil)
  (*readtable*                 (copy-readtable nil)))

(defun binding-default-specials (function special-bindings)
  "Return a closure that binds the symbols in SPECIAL-BINDINGS and calls
FUNCTION."
  (let ((specials (remove-duplicates special-bindings :from-end t :key #'car)))
    (named-lambda %binding-default-specials-wrapper ()
      (progv (mapcar #'car specials)
          (loop for (nil . form) in specials collect (eval form))
        (funcall function)))))

(defun make-thread (function
                    &key name (initial-bindings
                               *default-special-bindings*))
  "Creates and returns a thread named NAME, which will call the
  function FUNCTION with no arguments: when FUNCTION returns, the
  thread terminates. NAME defaults to \"Anonymous thread\" if unsupplied.

  On systems that do not support multi-threading, MAKE-THREAD will
  signal an error.

  The interaction between threads and dynamic variables is in some
  cases complex, and depends on whether the variable has only a global
  binding (as established by e.g. DEFVAR/DEFPARAMETER/top-level SETQ)
  or has been bound locally (e.g. with LET or LET*) in the calling
  thread.

  - Global bindings are shared between threads: the initial value of a
    global variable in the new thread will be the same as in the
    parent, and an assignment to such a variable in any thread will be
    visible to all threads in which the global binding is visible.

  - Local bindings, such as the ones introduced by INITIAL-BINDINGS,
    are local to the thread they are introduced in, except that

  - Local bindings in the the caller of MAKE-THREAD may or may not be
    shared with the new thread that it creates: this is
    implementation-defined. Portable code should not depend on
    particular behaviour in this case, nor should it assign to such
    variables without first rebinding them in the new thread."
  (%make-thread (binding-default-specials function initial-bindings)
                (or name "Anonymous thread")))

(defun join-thread (thread)
  "Wait until THREAD terminates. If THREAD has already terminated,
  return immediately. The return values of the thread function are
  returned."
  (sb-thread:join-thread thread))

(defun signal-error-if-current-thread (thread)
  (when (eq thread (current-thread))
    (error 'thread-error
           :message "Cannot destroy the current thread")))

(defun destroy-thread (thread)
  "Terminates the thread THREAD, which is an object
  as returned by MAKE-THREAD. This should be used with caution: it is
  implementation-defined whether the thread runs cleanup forms or
  releases its locks first.

  Destroying the calling thread is an error."
  (signal-error-if-current-thread thread)
  (sb-thread:terminate-thread thread))

(defun thread-yield ()
  "Allows other threads to run. It may be necessary or desirable to
  call this periodically in some implementations; others may schedule
  threads automatically."
  (sb-thread:thread-yield))

;; eof

(deftype thread ()
  'sb-thread:thread)

(defun thread-name (thread)
  (sb-thread:thread-name thread))

(defun interrupt-thread (thread function &rest args)
  "Interrupt THREAD and cause it to evaluate FUNCTION
  before continuing with the interrupted path of execution. This may
  not be a good idea if THREAD is holding locks or doing anything
  important. On systems that do not support multiple threads, this
  function signals an error."
  (flet ((apply-function ()
           (if args
               (named-lambda %interrupt-thread-wrapper ()
                 (apply function args))
               function)))
    (declare (dynamic-extent #'apply-function))
    (sb-thread:interrupt-thread thread (apply-function))))

(defun thread-alive-p (thread)
  "Returns true if THREAD is alive, that is, if
  DESTROY-THREAD has not been called on it."
  (sb-thread:thread-alive-p thread))

(defun current-thread ()
  "Returns the thread object for the calling
  thread. This is the same kind of object as would be returned by
  MAKE-THREAD."
  sb-thread:*current-thread*)

(defun thread-p (object)
  "Returns true if object is a thread, otherwise NIL."
  (typep object 'sb-thread:thread))
