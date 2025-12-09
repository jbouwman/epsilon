;;;; effects.lisp - Lightweight effect tracking
;;;;
;;;; Provides explicit effect declaration and tracking for functions.
;;;; Effects make side effects visible in the type/declaration system.

(defpackage :epsilon.effects
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map))
  (:export
   ;; Effect declaration
   #:defeffect
   #:declare-effects
   #:defun-effects
   #:with-effects

   ;; Effect checking
   #:effects-of
   #:pure-p
   #:requires-effect-p

   ;; Effect contexts
   #:with-pure-context
   #:with-allowed-effects
   #:with-effect-handler
   #:perform

   ;; Pure function definition
   #:defpure

   ;; Common effects
   #:+io+
   #:+state+
   #:+exception+
   #:+async+
   #:+database+
   #:+network+
   #:+filesystem+
   #:+random+
   #:+time+

   ;; Effect info
   #:effect-info
   #:list-effects
   #:list-effectful-functions))

(in-package :epsilon.effects)

;;; Effect registry

(defvar *effects* map:+empty+
  "Registry of defined effects: name -> effect-info")

(defvar *function-effects* map:+empty+
  "Map of function-name -> list of effects")

(defvar *current-allowed-effects* nil
  "Dynamically bound list of allowed effects (nil = all allowed)")

(defvar *effect-handlers* nil
  "Stack of effect handlers: list of (effect . handler-fn)")

;;; Effect info structure

(defstruct effect-info
  "Information about a defined effect"
  (name nil :type keyword)
  (documentation nil :type (or null string))
  (parent nil :type (or null keyword)))

;;; Standard effects

(defconstant +io+ :io
  "General I/O effect")

(defconstant +state+ :state
  "Mutable state effect")

(defconstant +exception+ :exception
  "Exception throwing effect")

(defconstant +async+ :async
  "Asynchronous computation effect")

(defconstant +database+ :database
  "Database access effect")

(defconstant +network+ :network
  "Network I/O effect")

(defconstant +filesystem+ :filesystem
  "File system access effect")

(defconstant +random+ :random
  "Random number generation effect")

(defconstant +time+ :time
  "Time/clock access effect")

;;; Effect definition

(defmacro defeffect (name &key documentation parent)
  "Define a new effect type.

   Example:
   (defeffect :logging
     :documentation \"Logging side effect\"
     :parent :io)"
  `(progn
     (setf *effects*
           (map:assoc *effects* ,name
                      (make-effect-info
                       :name ,name
                       :documentation ,documentation
                       :parent ,parent)))
     ,name))

;; Register standard effects at load time
(defeffect :io :documentation "General I/O operations")
(defeffect :state :documentation "Mutable state modification")
(defeffect :exception :documentation "May throw exceptions")
(defeffect :async :documentation "Asynchronous operations")
(defeffect :database :documentation "Database operations" :parent :io)
(defeffect :network :documentation "Network operations" :parent :io)
(defeffect :filesystem :documentation "File system operations" :parent :io)
(defeffect :random :documentation "Random number generation")
(defeffect :time :documentation "Time/clock access")

;;; Effect declaration

(defmacro declare-effects (function-name effects)
  "Declare the effects of a function.

   Example:
   (declare-effects fetch-user (:database :network))"
  `(progn
     (setf *function-effects*
           (map:assoc *function-effects* ',function-name ',effects))
     ',function-name))

(defmacro defun-effects (name args effects &body body)
  "Define a function with explicit effect declaration.

   Example:
   (defun-effects fetch-user (id) (:database :network)
     (query-db (format nil \"SELECT * FROM users WHERE id = ~a\" id)))"
  `(progn
     (declare-effects ,name ,effects)
     (defun ,name ,args
       (with-effects ,effects
         ,@body))))

;;; Effect checking

(defun effects-of (function-name)
  "Get the declared effects of a function.

   Example:
   (effects-of 'fetch-user) => (:database :network)"
  (map:get *function-effects* function-name))

(defun pure-p (function-name)
  "Check if a function is declared pure (no effects).

   Example:
   (pure-p 'calculate-score) => T"
  (let ((effects (effects-of function-name)))
    (or (null effects) (equal effects '()))))

(defun requires-effect-p (function-name effect)
  "Check if a function requires a specific effect.

   Example:
   (requires-effect-p 'fetch-user :database) => T"
  (member effect (effects-of function-name)))

(defun effect-subsumes-p (parent child)
  "Check if PARENT effect subsumes CHILD effect.
   An effect subsumes itself and any child effects."
  (or (eq parent child)
      (let ((child-info (map:get *effects* child)))
        (and child-info
             (effect-info-parent child-info)
             (effect-subsumes-p parent (effect-info-parent child-info))))))

;;; Effect contexts

(defmacro with-effects (effects &body body)
  "Execute BODY with the given EFFECTS tracked.
   In a restricted context, will signal an error if effects are not allowed.

   Example:
   (with-effects (:io :database)
     (fetch-and-log-data))"
  `(progn
     (when *current-allowed-effects*
       (dolist (effect ',effects)
         (unless (some (lambda (allowed)
                         (effect-subsumes-p allowed effect))
                       *current-allowed-effects*)
           (error "Effect ~A not allowed in current context. Allowed: ~A"
                  effect *current-allowed-effects*))))
     ,@body))

(defmacro with-pure-context (&body body)
  "Execute BODY in a pure context where no effects are allowed.
   Any effectful operation will signal an error.

   Example:
   (with-pure-context
     (calculate-result data))  ; OK if calculate-result is pure"
  `(let ((*current-allowed-effects* '()))
     ,@body))

(defmacro with-allowed-effects (effects &body body)
  "Execute BODY allowing only the specified EFFECTS.

   Example:
   (with-allowed-effects (:database)
     ;; Only database effects allowed here
     (query-db ...))  ; OK
     ;; (send-email ...) would error if send-email has :network effect"
  `(let ((*current-allowed-effects* ',effects))
     ,@body))

;;; Effect handlers (algebraic effects lite)

(defmacro with-effect-handler (handlers &body body)
  "Execute BODY with effect handlers installed.
   Handlers are (effect (args...) body...) forms.

   Example:
   (with-effect-handler
       ((:log (msg) (push msg *log-buffer*))
        (:metric (key val) (record-metric key val)))
     (perform :log \"Starting process\")
     (perform :metric :requests 1)
     (do-work))"
  (let ((handler-alist (gensym "HANDLERS")))
    `(let ((,handler-alist
            (list ,@(loop for handler in handlers
                          for effect = (first handler)
                          for args = (second handler)
                          for handler-body = (cddr handler)
                          collect `(cons ,effect
                                        (lambda ,args ,@handler-body))))))
       (let ((*effect-handlers* (append ,handler-alist *effect-handlers*)))
         ,@body))))

(defun perform (effect &rest args)
  "Perform an effect, invoking the current handler.

   Example:
   (perform :log \"Something happened\")
   (perform :metric :counter 42)"
  (let ((handler (assoc effect *effect-handlers*)))
    (if handler
        (apply (cdr handler) args)
        (error "No handler for effect: ~A" effect))))

;;; Pure function definition

(defmacro defpure (name args &body body)
  "Define a pure function (no effects).
   Documents that the function has no side effects.

   Example:
   (defpure calculate-score (attempts correct)
     \"Calculate score from attempts - no side effects\"
     (* 100 (/ correct (max attempts 1))))"
  `(progn
     (declare-effects ,name ())
     (defun ,name ,args
       ,@body)))

;;; Introspection

(defun effect-info (effect)
  "Get information about a defined effect.

   Example:
   (effect-info :database)
   => #S(EFFECT-INFO :NAME :DATABASE :DOCUMENTATION \"Database operations\" :PARENT :IO)"
  (map:get *effects* effect))

(defun list-effects ()
  "List all defined effects.

   Example:
   (list-effects) => (:io :state :exception ...)"
  (let ((effects '()))
    (map:each (lambda (k v)
                (declare (ignore v))
                (push k effects))
              *effects*)
    (nreverse effects)))

(defun list-effectful-functions ()
  "List all functions with declared effects.

   Example:
   (list-effectful-functions)
   => ((FETCH-USER :DATABASE :NETWORK) (WRITE-LOG :IO :FILESYSTEM) ...)"
  (let ((functions '()))
    (map:each (lambda (fn effects)
                (when effects
                  (push (cons fn effects) functions)))
              *function-effects*)
    (nreverse functions)))
