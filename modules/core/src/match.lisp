;;;; match.lisp - Pattern matching for Lisp
;;;;
;;;; Provides declarative pattern matching with destructuring,
;;;; guards, and integration with Option/Result types.

(defpackage :epsilon.match
  (:use :cl)
  (:local-nicknames
   (:map :epsilon.map)
   (:opt :epsilon.option)
   (:res :epsilon.result))
  (:export
   #:match
   #:match-1
   #:if-match
   #:when-match
   #:match-let
   #:match-lambda
   #:ematch
   #:match-pattern
   #:*match-fail*
   #:match-fail-p))

(in-package :epsilon.match)

;;; Pattern language:
;;;
;;; _              - Wildcard, matches anything
;;; symbol         - Binds value to symbol
;;; 'literal       - Matches literal value (quoted)
;;; (? pred)       - Predicate pattern
;;; (? pred var)   - Predicate with binding
;;; (list p1 p2)   - Matches list with exact length
;;; (list* p1 rest)- Matches list with head and rest
;;; (cons h t)     - Matches cons cell
;;; (vector p...)  - Matches vector elements
;;; (map :k1 p1)   - Matches map with key patterns
;;; (or p1 p2)     - Alternation
;;; (and p1 p2)    - Conjunction
;;; (guard p expr) - Pattern with guard clause
;;; (some p)       - Matches Option Some
;;; none           - Matches Option None
;;; (ok p)         - Matches Result Ok
;;; (err p)        - Matches Result Err

(defvar *match-fail* (gensym "MATCH-FAIL")
  "Sentinel value for pattern match failure")

(defun match-fail-p (result)
  "Check if result is a match failure"
  (eq result *match-fail*))

(defun pattern-keyword-p (sym name)
  "Check if SYM is a pattern keyword with the given NAME (case-insensitive)"
  (and (symbolp sym)
       (string-equal (symbol-name sym) name)))

(defun match-pattern (pattern value bindings)
  "Try to match PATTERN against VALUE, extending BINDINGS.
   Returns updated bindings on success, *match-fail* on failure."
  (cond
    ;; Wildcard
    ((and (symbolp pattern) (pattern-keyword-p pattern "_"))
     bindings)

    ;; Binding variable
    ((and (symbolp pattern)
          (not (keywordp pattern))
          (not (pattern-keyword-p pattern "T"))
          (not (pattern-keyword-p pattern "NIL"))
          (not (pattern-keyword-p pattern "NONE"))
          (not (pattern-keyword-p pattern "_")))
     (acons pattern value bindings))

    ;; Quoted literal
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "QUOTE"))
     (if (equal value (second pattern))
         bindings
         *match-fail*))

    ;; Predicate pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "?"))
     (let ((pred (second pattern))
           (var (third pattern)))
       (if (funcall pred value)
           (if var
               (acons var value bindings)
               bindings)
           *match-fail*)))

    ;; List pattern with exact length
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "LIST"))
     (let ((subpatterns (rest pattern)))
       (if (and (listp value)
                (= (length value) (length subpatterns)))
           (loop with result = bindings
                 for p in subpatterns
                 for v in value
                 do (setf result (match-pattern p v result))
                 when (match-fail-p result) return *match-fail*
                 finally (return result))
           *match-fail*)))

    ;; List* pattern (head + rest)
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "LIST*"))
     (let* ((subpatterns (rest pattern))
            (n (1- (length subpatterns))))
       (if (and (listp value) (>= (length value) n))
           (let ((result bindings))
             ;; Match head patterns
             (loop for i from 0 below n
                   for p in subpatterns
                   for v in value
                   do (setf result (match-pattern p v result))
                   when (match-fail-p result) return *match-fail*)
             ;; Match rest pattern
             (setf result (match-pattern
                           (nth n subpatterns)
                           (nthcdr n value)
                           result))
             result)
           *match-fail*)))

    ;; Cons pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "CONS"))
     (if (consp value)
         (let ((result (match-pattern (second pattern) (car value) bindings)))
           (if (match-fail-p result)
               *match-fail*
               (match-pattern (third pattern) (cdr value) result)))
         *match-fail*))

    ;; Vector pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "VECTOR"))
     (let ((subpatterns (rest pattern)))
       (if (and (vectorp value)
                (= (length value) (length subpatterns)))
           (loop with result = bindings
                 for p in subpatterns
                 for i from 0
                 do (setf result (match-pattern p (aref value i) result))
                 when (match-fail-p result) return *match-fail*
                 finally (return result))
           *match-fail*)))

    ;; Map pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "MAP"))
     (if (map:map-p value)
         (loop with result = bindings
               for (key pat) on (rest pattern) by #'cddr
               for val = (map:get value key *match-fail*)
               when (match-fail-p val) return *match-fail*
               do (setf result (match-pattern pat val result))
               when (match-fail-p result) return *match-fail*
               finally (return result))
         *match-fail*))

    ;; Or pattern (alternation)
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "OR"))
     (loop for alt in (rest pattern)
           for result = (match-pattern alt value bindings)
           unless (match-fail-p result) return result
           finally (return *match-fail*)))

    ;; And pattern (conjunction)
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "AND"))
     (loop with result = bindings
           for pat in (rest pattern)
           do (setf result (match-pattern pat value result))
           when (match-fail-p result) return *match-fail*
           finally (return result)))

    ;; Guard pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "GUARD"))
     (let* ((inner-pattern (second pattern))
            (guard-expr (third pattern))
            (result (match-pattern inner-pattern value bindings)))
       (if (match-fail-p result)
           *match-fail*
           ;; Evaluate guard with bindings
           (if (eval-with-bindings guard-expr result)
               result
               *match-fail*))))

    ;; Option Some pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "SOME"))
     (if (opt:some-p value)
         (match-pattern (second pattern) (opt:unwrap value) bindings)
         *match-fail*))

    ;; Option None pattern
    ((and (symbolp pattern) (pattern-keyword-p pattern "NONE"))
     (if (opt:none-p value)
         bindings
         *match-fail*))

    ;; Result Ok pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "OK"))
     (if (res:ok-p value)
         (match-pattern (second pattern) (res:unwrap value) bindings)
         *match-fail*))

    ;; Result Err pattern
    ((and (listp pattern)
          (pattern-keyword-p (first pattern) "ERR"))
     (if (res:err-p value)
         (match-pattern (second pattern) (res:unwrap-err value) bindings)
         *match-fail*))

    ;; ADT pattern - check if this looks like a constructor pattern
    ;; A pattern (constructor-name p1 p2 ...) matches if:
    ;; 1. There exists a predicate constructor-name-p that returns true
    ;; 2. The value's fields match the sub-patterns
    ((and (listp pattern)
          (symbolp (first pattern))
          (not (null (first pattern)))  ; Don't treat nil as ADT
          (not (eq (first pattern) t))  ; Don't treat t as ADT
          (not (keywordp (first pattern)))  ; Don't treat keywords as ADT
          (not (pattern-keyword-p (first pattern) "?"))
          (not (pattern-keyword-p (first pattern) "LIST"))
          (not (pattern-keyword-p (first pattern) "LIST*"))
          (not (pattern-keyword-p (first pattern) "CONS"))
          (not (pattern-keyword-p (first pattern) "VECTOR"))
          (not (pattern-keyword-p (first pattern) "MAP"))
          (not (pattern-keyword-p (first pattern) "OR"))
          (not (pattern-keyword-p (first pattern) "AND"))
          (not (pattern-keyword-p (first pattern) "GUARD"))
          (not (pattern-keyword-p (first pattern) "SOME"))
          (not (pattern-keyword-p (first pattern) "OK"))
          (not (pattern-keyword-p (first pattern) "ERR"))
          (not (pattern-keyword-p (first pattern) "QUOTE")))
     (let* ((constructor (first pattern))
            (pred-name (intern (format nil "~A-P" constructor)
                               (symbol-package constructor)))
            (subpatterns (rest pattern)))
       ;; Check if predicate exists and matches
       (if (and (fboundp pred-name)
                (funcall (symbol-function pred-name) value))
           ;; Get values from the ADT instance
           (let ((values (if (and (find-package :epsilon.data)
                                  (fboundp (find-symbol "ADT-INSTANCE-VALUES" :epsilon.data)))
                             (funcall (symbol-function
                                       (find-symbol "ADT-INSTANCE-VALUES" :epsilon.data))
                                      value)
                             nil)))
             (if (and values (= (length values) (length subpatterns)))
                 (loop with result = bindings
                       for p in subpatterns
                       for v in values
                       do (setf result (match-pattern p v result))
                       when (match-fail-p result) return *match-fail*
                       finally (return result))
                 (if (and (null values) (null subpatterns))
                     bindings  ; Nullary constructor matches
                     *match-fail*)))
           *match-fail*)))

    ;; Literal match
    ((or (numberp pattern)
         (stringp pattern)
         (keywordp pattern)
         (and (symbolp pattern) (pattern-keyword-p pattern "T"))
         (and (symbolp pattern) (pattern-keyword-p pattern "NIL")))
     (if (equal pattern value)
         bindings
         *match-fail*))

    ;; Unknown pattern
    (t
     (error "Unknown pattern: ~S" pattern))))

(defun eval-with-bindings (expr bindings)
  "Evaluate EXPR with BINDINGS in scope"
  (let ((vars (mapcar #'car bindings))
        (vals (mapcar #'cdr bindings)))
    (eval `(let ,(mapcar #'list vars (mapcar (lambda (v) `',v) vals))
             ,expr))))

(defun compile-clause (value-var pattern body bindings-var)
  "Compile a single match clause"
  (let ((binding-forms (extract-binding-forms pattern)))
    `(let ((,bindings-var (match-pattern ',pattern ,value-var '())))
       (unless (match-fail-p ,bindings-var)
         (let ,(mapcar (lambda (v)
                         `(,v (cdr (assoc ',v ,bindings-var))))
                       binding-forms)
           (declare (ignorable ,@binding-forms))
           (return (progn ,@body)))))))

(defun extract-binding-forms (pattern)
  "Extract all variable bindings from a pattern"
  (cond
    ((and (symbolp pattern) (pattern-keyword-p pattern "_")) nil)
    ((and (symbolp pattern) (pattern-keyword-p pattern "NONE")) nil)
    ((and (symbolp pattern)
          (not (keywordp pattern))
          (not (pattern-keyword-p pattern "T"))
          (not (pattern-keyword-p pattern "NIL"))
          (not (pattern-keyword-p pattern "_"))
          (not (pattern-keyword-p pattern "NONE")))
     (list pattern))
    ((and (listp pattern) (pattern-keyword-p (first pattern) "QUOTE"))
     nil)
    ((and (listp pattern) (pattern-keyword-p (first pattern) "?"))
     (when (third pattern) (list (third pattern))))
    ;; Guard pattern - only extract bindings from the pattern, not the guard expression
    ((and (listp pattern)
          (symbolp (first pattern))
          (pattern-keyword-p (first pattern) "GUARD"))
     (extract-binding-forms (second pattern)))
    ((and (listp pattern)
          (symbolp (first pattern))
          (not (pattern-keyword-p (first pattern) "?"))
          (not (pattern-keyword-p (first pattern) "LIST"))
          (not (pattern-keyword-p (first pattern) "LIST*"))
          (not (pattern-keyword-p (first pattern) "CONS"))
          (not (pattern-keyword-p (first pattern) "VECTOR"))
          (not (pattern-keyword-p (first pattern) "MAP"))
          (not (pattern-keyword-p (first pattern) "OR"))
          (not (pattern-keyword-p (first pattern) "AND"))
          (not (pattern-keyword-p (first pattern) "GUARD"))
          (not (pattern-keyword-p (first pattern) "SOME"))
          (not (pattern-keyword-p (first pattern) "OK"))
          (not (pattern-keyword-p (first pattern) "ERR"))
          (not (pattern-keyword-p (first pattern) "QUOTE")))
     ;; ADT pattern - extract bindings from subpatterns
     (mapcan #'extract-binding-forms (rest pattern)))
    ((listp pattern)
     (mapcan #'extract-binding-forms (rest pattern)))
    (t nil)))

(defmacro match (value &body clauses)
  "Pattern match VALUE against CLAUSES.
   Each clause is (pattern body...).
   Returns the result of the first matching clause's body.
   Signals error if no clause matches.

   Patterns:
   - _           : wildcard, matches anything
   - symbol      : binds value to symbol
   - 'literal    : matches quoted literal
   - (? pred)    : predicate pattern
   - (? pred var): predicate with binding
   - (list a b)  : matches list of exact length
   - (list* a rest): matches list with head and rest
   - (cons h t)  : matches cons cell
   - (vector a b): matches vector elements
   - (map :k v)  : matches map keys
   - (or p1 p2)  : alternation
   - (and p1 p2) : conjunction
   - (guard p e) : pattern with guard expression
   - (some x)    : matches Option Some
   - none        : matches Option None
   - (ok x)      : matches Result Ok
   - (err e)     : matches Result Err

   Example:
   (match x
     ((list a b) (+ a b))
     ((? numberp n) (* n 2))
     (_ 0))"
  (let ((value-var (gensym "VALUE"))
        (bindings-var (gensym "BINDINGS")))
    `(let ((,value-var ,value))
       (block nil
         ,@(loop for (pattern . body) in clauses
                 collect (compile-clause value-var pattern body bindings-var))
         (error "No pattern matched value: ~S" ,value-var)))))

(defmacro ematch (value &body clauses)
  "Exhaustive match - same as MATCH but intended for exhaustive patterns.
   Runtime behavior is identical to MATCH."
  `(match ,value ,@clauses))

(defmacro match-1 (value pattern then else)
  "Match a single pattern. If successful, evaluate THEN with bindings,
   otherwise evaluate ELSE."
  (let ((value-var (gensym "VALUE"))
        (bindings-var (gensym "BINDINGS"))
        (binding-forms (extract-binding-forms pattern)))
    `(let* ((,value-var ,value)
            (,bindings-var (match-pattern ',pattern ,value-var '())))
       (if (match-fail-p ,bindings-var)
           ,else
           (let ,(mapcar (lambda (v)
                           `(,v (cdr (assoc ',v ,bindings-var))))
                         binding-forms)
             (declare (ignorable ,@binding-forms))
             ,then)))))

(defmacro if-match (pattern value then &optional else)
  "If PATTERN matches VALUE, evaluate THEN with bindings, else evaluate ELSE"
  `(match-1 ,value ,pattern ,then ,else))

(defmacro when-match (pattern value &body body)
  "When PATTERN matches VALUE, evaluate BODY with bindings"
  `(if-match ,pattern ,value (progn ,@body)))

(defmacro match-let (bindings &body body)
  "Destructuring let using pattern matching.
   Each binding is (pattern value).

   Example:
   (match-let (((list x y) (get-point))
               ((map :name n) (get-user)))
     (format t \"~a at (~a,~a)\" n x y))"
  (if (null bindings)
      `(progn ,@body)
      (let* ((binding (first bindings))
             (pattern (first binding))
             (value (second binding)))
        `(if-match ,pattern ,value
                   (match-let ,(rest bindings) ,@body)
                   (error "Pattern match failed in match-let: ~S vs ~S"
                          ',pattern ,value)))))

(defmacro match-lambda (&body clauses)
  "Create a function that pattern matches its argument.

   Example:
   (match-lambda
     ((list a b) (+ a b))
     (_ 0))"
  (let ((arg (gensym "ARG")))
    `(lambda (,arg)
       (match ,arg ,@clauses))))
