;;;; epsilon.scheduler.fiber-discipline -- annotation surface for the
;;;; "what's safe to call from a coroutine" question.
;;;;
;;;; The scheduler runs M:N: a small carrier pool dispatches a large
;;;; pool of fiber-backed coroutines. Coroutine code MUST yield through
;;;; the scheduler's park primitives when waiting; if it instead blocks
;;;; the OS thread on a futex, semaphore, syscall, or SIGALRM-style
;;;; with-timeout, the carrier's iter-counter freezes and the watchdog
;;;; eventually flags or kills the process.
;;;;
;;;; Today nothing in the language, package surface, or compile output
;;;; distinguishes a fiber-safe wait from a thread-blocking wait. This
;;;; module provides the missing distinction as symbol-plist metadata
;;;; that downstream tools (epsilon.analyze.lint, the watchdog backtrace
;;;; classifier, code review) can consult.
;;;;
;;;; Taxonomy:
;;;;
;;;;   :fiber-aware              Yields control through epsilon.scheduler
;;;;                             (park-on-fd, coroutine-sleep, ...). Safe.
;;;;   :cpu-bound                Runs without yielding, but bounded and
;;;;                             algorithmic. Safe-ish; long ones should
;;;;                             use run-blocking to offload.
;;;;   :blocks-thread            Syscalls or native synchronization that
;;;;                             blocks the OS thread. UNSAFE inside a
;;;;                             coroutine. Calling this from
;;;;                             :fiber-aware code is the bug
;;;;                             IMPL-398 prevents.
;;;;   :fiber-aware-conditional  Behaviour depends on arguments
;;;;                             (e.g. an io:read-into that may park or
;;;;                             block depending on the underlying
;;;;                             reader). Discouraged; consider
;;;;                             splitting into two named functions.
;;;;
;;;; The annotations are additive: code that doesn't use them keeps
;;;; working. Value comes from the lint and runtime detectors that
;;;; consult the metadata.
;;;;
;;;; Storage: kind is stored under indicator
;;;; 'epsilon.scheduler.fiber-discipline::fiber-kind on the symbol's
;;;; plist; alternative is stored under
;;;; 'epsilon.scheduler.fiber-discipline::fiber-aware-alternative;
;;;; reason (a doc string) under
;;;; 'epsilon.scheduler.fiber-discipline::fiber-reason.
;;;; Symbol-plist keys are intentional: they survive across image
;;;; reloads, they don't add a runtime hash-table lookup at the call
;;;; site, and they're inspectable in the SLIME / SLY inspector.

(defpackage :epsilon.scheduler.fiber-discipline
  (:use :cl)
  (:export
   ;; Taxonomy keywords (re-exported as symbols for documentation
   ;; purposes; callers usually write them as keywords directly).
   #:+fiber-aware+
   #:+cpu-bound+
   #:+blocks-thread+
   #:+fiber-aware-conditional+
   #:+unknown+
   #:fiber-kinds

   ;; Annotation forms
   #:define-fiber-fn
   #:declaim-fiber-kind

   ;; Accessors
   #:fiber-kind
   #:fiber-aware-alternative
   #:fiber-reason
   #:annotated-symbols

   ;; Configuration
   #:*default-fiber-kind*

   ;; Stall classification
   #:classify-stall-reason
   #:*stall-reason-patterns*

   ;; Conditions
   #:fiber-discipline-error
   #:invalid-fiber-kind))

(in-package :epsilon.scheduler.fiber-discipline)

;;; ---------------------------------------------------------------------------
;;; Taxonomy
;;; ---------------------------------------------------------------------------

(defparameter +fiber-aware+ :fiber-aware
  "Function yields control through epsilon.scheduler when it waits.")

(defparameter +cpu-bound+ :cpu-bound
  "Function runs without yielding but is bounded and algorithmic.")

(defparameter +blocks-thread+ :blocks-thread
  "Function blocks the OS thread on a syscall or native primitive.
Unsafe inside a coroutine. Move to a fiber-aware variant or wrap
in run-blocking.")

(defparameter +fiber-aware-conditional+ :fiber-aware-conditional
  "Function's wait shape depends on its arguments. Discouraged; prefer
splitting into two distinctly-named entry points.")

(defparameter +unknown+ :unknown
  "Symbol has no fiber-kind annotation. Treated as
*DEFAULT-FIBER-KIND* for lint purposes.")

(defparameter fiber-kinds
  (list :fiber-aware :cpu-bound :blocks-thread
        :fiber-aware-conditional :unknown)
  "All recognised fiber-kind keywords.")

(defparameter *default-fiber-kind* :unknown
  "Kind reported for symbols that have no annotation. Lint policy
treats :unknown as 'warn'; once the fiber-aware migration is far
enough along, this can be flipped to :cpu-bound to default to
quiet-and-safe-ish.")

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition fiber-discipline-error (error)
  ()
  (:documentation "Parent of fiber-discipline annotation errors."))

(define-condition invalid-fiber-kind (fiber-discipline-error)
  ((kind :initarg :kind :reader invalid-fiber-kind-kind)
   (symbol :initarg :symbol :reader invalid-fiber-kind-symbol))
  (:report (lambda (c s)
             (format s "~S is not a valid fiber-kind for ~S; expected one of ~S"
                     (invalid-fiber-kind-kind c)
                     (invalid-fiber-kind-symbol c)
                     fiber-kinds))))

;;; ---------------------------------------------------------------------------
;;; Plist storage
;;; ---------------------------------------------------------------------------

(defun %check-kind (kind symbol)
  (unless (member kind fiber-kinds :test #'eq)
    (error 'invalid-fiber-kind :kind kind :symbol symbol))
  kind)

(defun fiber-kind (symbol)
  "Return the fiber-kind annotation for SYMBOL, or *DEFAULT-FIBER-KIND*
if SYMBOL has no annotation. SYMBOL must be a non-NIL symbol."
  (or (get symbol 'fiber-kind)
      *default-fiber-kind*))

(defun (setf fiber-kind) (kind symbol)
  (%check-kind kind symbol)
  (setf (get symbol 'fiber-kind) kind))

(defun fiber-aware-alternative (symbol)
  "Return the symbol that implements the :fiber-aware variant of
SYMBOL, or NIL if none is registered. The lint uses this to suggest
a fix when it flags a :blocks-thread call from fiber-aware code."
  (get symbol 'fiber-aware-alternative))

(defun (setf fiber-aware-alternative) (alt symbol)
  (setf (get symbol 'fiber-aware-alternative) alt))

(defun fiber-reason (symbol)
  "Return the documentation string explaining why SYMBOL has its
particular fiber-kind, or NIL."
  (get symbol 'fiber-reason))

(defun (setf fiber-reason) (reason symbol)
  (setf (get symbol 'fiber-reason) reason))

;;; ---------------------------------------------------------------------------
;;; Registry of annotated symbols
;;;
;;; Kept so tools (CLI, lint, /diagnostics endpoints) can enumerate
;;; what's annotated without scanning every package. Population
;;; happens automatically through DECLAIM-FIBER-KIND and
;;; DEFINE-FIBER-FN.
;;; ---------------------------------------------------------------------------

(defvar *annotated-symbols* nil
  "List of every symbol that has been annotated with a fiber-kind.
Updated under *ANNOTATIONS-LOCK*. Order is most-recently-annotated
first; lint and CLI sort it as they like.")

;; The lock is a plain SBCL mutex rather than epsilon.sys.lock to avoid
;; pulling that module into the bootstrap dependency closure of
;; epsilon.scheduler. The annotation registry is updated only at
;; load time; contention is negligible.
#+sbcl
(defvar *annotations-lock* (sb-thread:make-mutex
                            :name "fiber-discipline-annotations"))

(defmacro %with-annotations-lock (&body body)
  #+sbcl `(sb-thread:with-mutex (*annotations-lock*) ,@body)
  #-sbcl `(progn ,@body))

(defun %register-annotation (symbol)
  (%with-annotations-lock
    (pushnew symbol *annotated-symbols*)))

(defun annotated-symbols ()
  "Return a fresh list of every symbol that has been annotated."
  (%with-annotations-lock
    (copy-list *annotated-symbols*)))

;;; ---------------------------------------------------------------------------
;;; DECLAIM-FIBER-KIND -- annotate any symbol, including those in
;;; packages we don't own (sb-ext:*, sb-thread:*, sb-posix:*, ...).
;;; ---------------------------------------------------------------------------

(defmacro declaim-fiber-kind (symbol kind &key alternative reason)
  "Declare SYMBOL's fiber-kind. Used to annotate functions whose
definitions live outside our control (SBCL internals, user code we
don't want to touch). The annotation runs at load time.

  KIND          one of FIBER-KINDS.
  ALTERNATIVE   (optional) symbol naming the fiber-aware replacement
                the lint should suggest when it flags a :blocks-thread
                call from fiber-aware code.
  REASON        (optional) human-readable string explaining why this
                kind was chosen; surfaces in lint output.

Example:

  (declaim-fiber-kind sb-ext:with-timeout :blocks-thread
    :reason \"SIGALRM cannot preempt non-interruptible foreign calls\"
    :alternative epsilon.scheduler:coroutine-wait-until)"
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (fiber-kind ',symbol) ,kind)
       ,@(when alternative
           `((setf (fiber-aware-alternative ',symbol) ',alternative)))
       ,@(when reason
           `((setf (fiber-reason ',symbol) ,reason)))
       (%register-annotation ',symbol))
     ',symbol))

;;; ---------------------------------------------------------------------------
;;; DEFINE-FIBER-FN -- defun + annotation in one form. Use this in
;;; new code; existing defuns get DECLAIM-FIBER-KIND beside them.
;;; ---------------------------------------------------------------------------

(defmacro define-fiber-fn (name lambda-list &body body)
  "Define NAME as a function whose body is BODY, and stamp its
fiber-kind annotation in the same form. The first form of BODY may
be a docstring; the second may be a (DECLARE ...) block. Either may
contain a special declaration (FIBER-KIND :KIND ...) recognised here:

  (define-fiber-fn fiber-read (fd buf &key start end)
    \"Park the fiber until FD is readable, then read into BUF.\"
    (declare (fiber-kind :fiber-aware
                         :reason \"parks via park-on-fd\"))
    ...)

The (DECLARE (FIBER-KIND ...)) form is consumed here and removed
from the actual DEFUN body. If no FIBER-KIND declaration is given,
the function's kind defaults to :fiber-aware (the common case for
new code in fiber-discipline-aware modules); that default is
intentional so the discipline is opt-out, not opt-in."
  (multiple-value-bind (docstring decls real-body kind alternative reason)
      (%parse-fiber-fn-body body)
    `(progn
       (defun ,name ,lambda-list
         ,@(when docstring (list docstring))
         ,@decls
         ,@real-body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (fiber-kind ',name) ,kind)
         ,@(when alternative
             `((setf (fiber-aware-alternative ',name) ',alternative)))
         ,@(when reason
             `((setf (fiber-reason ',name) ,reason)))
         (%register-annotation ',name))
       ',name)))

(defun %parse-fiber-fn-body (body)
  "Split a DEFUN body into (VALUES DOCSTRING DECLS REAL-BODY KIND
ALTERNATIVE REASON), pulling out a (DECLARE (FIBER-KIND ...)) form
if present. Multiple FIBER-KIND declarations are an error."
  (let ((docstring nil)
        (decls nil)
        (real-body body)
        (kind :fiber-aware)
        (alternative nil)
        (reason nil)
        (saw-fiber-kind nil))
    (when (and (consp real-body) (stringp (first real-body))
               (rest real-body))
      (setf docstring (first real-body)
            real-body (rest real-body)))
    (loop while (and (consp real-body)
                     (consp (first real-body))
                     (eq (caar real-body) 'declare))
          do (let ((decl-forms (cdr (first real-body)))
                   (kept '()))
               (dolist (df decl-forms)
                 (cond
                   ;; Compare by symbol-name so user-side
                   ;; (declare (fiber-kind :blocks-thread)) works
                   ;; regardless of which package interned FIBER-KIND.
                   ;; Interning the user-side symbol directly into
                   ;; this package would be hostile to readers; the
                   ;; string compare is the conventional escape hatch.
                   ((and (consp df)
                         (symbolp (first df))
                         (string= (symbol-name (first df)) "FIBER-KIND"))
                    (when saw-fiber-kind
                      (error "DEFINE-FIBER-FN: more than one ~
                              (FIBER-KIND ...) declaration"))
                    (setf saw-fiber-kind t)
                    (let ((plist (rest df)))
                      ;; (fiber-kind :KIND :alternative SYM :reason STR)
                      (when plist
                        (setf kind (first plist))
                        (loop for (k v) on (rest plist) by #'cddr
                              do (case k
                                   (:alternative (setf alternative v))
                                   (:reason (setf reason v))
                                   (t (error "DEFINE-FIBER-FN: ~
                                              unknown FIBER-KIND ~
                                              keyword ~S" k)))))))
                   (t (push df kept))))
               (when kept
                 (push `(declare ,@(nreverse kept)) decls))
               (setf real-body (rest real-body))))
    (values docstring (nreverse decls) real-body
            kind alternative reason)))

;;; ---------------------------------------------------------------------------
;;; Stall reason classifier
;;;
;;; Given a backtrace (list of frame strings), guess what the carrier
;;; is stuck inside. The watchdog calls this when it logs a
;;; carrier-stall warn event so the operator gets a one-keyword
;;; summary alongside the full backtrace.
;;;
;;; Patterns are matched in order; first match wins. The match is on
;;; SUBSTRING within any of the top N frames, so a bare textual scan
;;; is enough -- no need to reflect on the actual frame structure.
;;; ---------------------------------------------------------------------------

(defparameter *stall-reason-patterns*
  '(;; Native condition variable / semaphore wait.
    ("CONDITION-WAIT"    . :thread-blocked-on-mutex)
    ("WAIT-ON-SEMAPHORE" . :thread-blocked-on-mutex)
    ("GRAB-MUTEX"        . :thread-blocked-on-mutex)
    ;; SIGALRM-style preemption that doesn't reach foreign frames.
    ("WITH-TIMEOUT"      . :with-timeout-non-preemptive)
    ;; TLS read in the native crypto stack.
    ("TLS-READ"          . :tls-read-blocked)
    ("TLS12-READ"        . :tls-read-blocked)
    ("SSL-READ"          . :tls-read-blocked)
    ;; Raw read syscalls outside fiber-aware loops.
    ("UNIX-READ"         . :thread-blocked-on-syscall-read)
    ;; Plain SLEEP.
    ("SLEEP"             . :thread-blocked-on-sleep))
  "Alist of (FRAME-PATTERN . REASON-KEYWORD) used by
CLASSIFY-STALL-REASON. Earlier entries win on first match. Patterns
are case-insensitive substrings; the matcher uppercases both sides
before comparing.")

(defun classify-stall-reason (backtrace &key (top 8))
  "Given BACKTRACE (a list of frame strings as returned by
SCHED:CARRIER-BACKTRACE), return a single keyword describing the
stall, or :OTHER if no pattern matches.

Only the top TOP frames are inspected; the carrier-loop tail is
the same on every stall and provides no diagnostic value."
  (when (null backtrace)
    (return-from classify-stall-reason :no-backtrace))
  (let ((frames (subseq backtrace 0 (min top (length backtrace)))))
    (dolist (entry *stall-reason-patterns* :other)
      (destructuring-bind (pattern . reason) entry
        (when (some (lambda (frame)
                      (search pattern frame :test #'char-equal))
                    frames)
          (return reason))))))

;;; ---------------------------------------------------------------------------
;;; Convenience: bulk annotation
;;; ---------------------------------------------------------------------------

(defmacro declaim-fiber-kinds (&body forms)
  "Bulk wrapper around DECLAIM-FIBER-KIND. Each form is
(SYMBOL KIND &key ALTERNATIVE REASON):

  (declaim-fiber-kinds
    (sb-ext:with-timeout :blocks-thread
      :reason \"SIGALRM cannot preempt foreign calls\")
    (epsilon.sys.semaphore:wait-on-semaphore :blocks-thread
      :reason \"futex wait\"))"
  `(progn
     ,@(loop for form in forms
             collect `(declaim-fiber-kind ,@form))))
