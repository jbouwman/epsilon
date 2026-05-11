;;;; Generators -- yield/resume coroutines on sb-fiber
;;;;
;;;; A generator owns two fibers: a *producer* that runs the user
;;;; body and a *consumer* set on first GENERATOR-NEXT call.  YIELD
;;;; stashes a value and switches to the consumer; GENERATOR-NEXT
;;;; switches to the producer and reads the stashed value when control
;;;; returns.
;;;;
;;;; Independent of the scheduler -- generators run purely in response
;;;; to GENERATOR-NEXT.  They share the same sb-fiber primitive as
;;;; coroutines but no scheduler infrastructure.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-fiber))

(defpackage :epsilon.scheduler.generator
  (:use :cl)
  (:export
   #:generator
   #:generator-p
   #:make-generator
   #:generator-next
   #:generator-close
   #:generator-done-p
   #:defgenerator
   #:yield
   #:do-generator))

(in-package :epsilon.scheduler.generator)

;;; ---------------------------------------------------------------------------
;;; Generator state
;;; ---------------------------------------------------------------------------

(deftype generator-state () '(member :ready :running :done :failed))

(defstruct (generator (:constructor %make-generator))
  "PRODUCER  the fiber running the user body.
CONSUMER   the fiber that called GENERATOR-NEXT (set on first call).
SLOT       holds the most-recently yielded value (or the producer's
           error condition when state = :failed).
STATE      :ready (not yet started or paused inside YIELD)
           :running (control transferred to producer)
           :done (producer body returned normally)
           :failed (producer body errored)
ERROR      the condition object when state = :failed."
  (producer nil)
  (consumer nil)
  (slot nil)
  (state :ready :type generator-state)
  (error nil))

;;; ---------------------------------------------------------------------------
;;; Per-generator binding -- so YIELD knows which generator to switch on
;;; ---------------------------------------------------------------------------

(defvar *current-generator* nil
  "Bound inside a producer body so YIELD can find its host generator.")

;;; ---------------------------------------------------------------------------
;;; YIELD
;;; ---------------------------------------------------------------------------

(defun yield (value)
  "Inside a generator body: stash VALUE and return control to the
caller of GENERATOR-NEXT.  When GENERATOR-NEXT is invoked again, this
form returns NIL."
  (let ((gen *current-generator*))
    (unless gen
      (error "yield called outside a generator body"))
    (setf (generator-slot gen) value
          (generator-state gen) :ready)
    (sb-fiber:fiber-switch (generator-producer gen)
                           (generator-consumer gen))
    nil))

;;; ---------------------------------------------------------------------------
;;; make-generator
;;; ---------------------------------------------------------------------------

(defun make-generator (fn)
  "Build a generator that runs the zero-argument FN.  FN may call
YIELD to produce values."
  (let* ((gen (%make-generator))
         (fiber
          (sb-fiber:make-fiber
           (lambda ()
             (let ((*current-generator* gen))
               (handler-case
                   (progn (funcall fn)
                          (setf (generator-state gen) :done))
                 (error (e)
                   (setf (generator-error gen) e
                         (generator-state gen) :failed)))
               ;; Body completed -- switch back to consumer with done state.
               (when (generator-consumer gen)
                 (sb-fiber:fiber-switch (generator-producer gen)
                                        (generator-consumer gen))))))) )
    (setf (generator-producer gen) fiber)
    gen))

;;; ---------------------------------------------------------------------------
;;; generator-next
;;; ---------------------------------------------------------------------------

(defun generator-done-p (gen)
  (member (generator-state gen) '(:done :failed)))

(defun generator-next (gen)
  "Run the generator until the next YIELD or until it terminates.
Returns
  (values VALUE T)        on a successful yield
  (values NIL  :done)     on normal completion
Re-signals the underlying error if the producer body raised."
  (when (generator-done-p gen)
    (return-from generator-next (values nil :done)))
  ;; Record the consumer fiber on first entry so YIELD knows where to
  ;; return.  The same consumer-fiber is reused for all subsequent
  ;; calls; if the caller migrates threads (which sb-fiber pins
  ;; against), behaviour is undefined -- consistent with sb-fiber's
  ;; pinning rules.
  (unless (generator-consumer gen)
    (setf (generator-consumer gen) (sb-fiber:make-main-fiber)))
  (setf (generator-state gen) :running)
  (sb-fiber:fiber-switch (generator-consumer gen)
                         (generator-producer gen))
  (case (generator-state gen)
    (:ready (values (generator-slot gen) t))
    (:done  (values nil :done))
    (:failed (error (generator-error gen)))
    (t (error "Unexpected generator state: ~S" (generator-state gen)))))

;;; ---------------------------------------------------------------------------
;;; generator-close
;;; ---------------------------------------------------------------------------

(defun generator-close (gen)
  "Destroy the producer fiber if still alive.  Idempotent."
  (when (generator-producer gen)
    (handler-case (sb-fiber:destroy-fiber (generator-producer gen))
      (error () nil))
    (setf (generator-producer gen) nil
          (generator-state gen) :done))
  (when (generator-consumer gen)
    (handler-case (sb-fiber:destroy-fiber (generator-consumer gen))
      (error () nil))
    (setf (generator-consumer gen) nil)))

;;; ---------------------------------------------------------------------------
;;; defgenerator / do-generator sugar
;;; ---------------------------------------------------------------------------

(defmacro defgenerator (name lambda-list &body body)
  "Define a function NAME that, when called with LAMBDA-LIST, returns
a fresh generator running BODY (which may call YIELD)."
  `(defun ,name ,lambda-list
     (make-generator (lambda () ,@body))))

(defmacro do-generator ((var gen) &body body)
  "Iterate VAR over the values produced by GEN.  Closes GEN on exit
(normal or via unwind)."
  (let ((g (gensym "GEN")) (v (gensym "V")) (s (gensym "S")))
    `(let ((,g ,gen))
       (unwind-protect
            (loop
              (multiple-value-bind (,v ,s) (generator-next ,g)
                (when (eq ,s :done) (return))
                (let ((,var ,v)) ,@body)))
         (generator-close ,g)))))
