(defpackage #:epsilon.effects.tests
  (:use
   #:cl
   #:epsilon.test)
  (:local-nicknames
   (#:fx #:epsilon.effects)))

(in-package #:epsilon.effects.tests)

;;; Effect definition tests

(deftest test-standard-effects-exist
  (is (eq :io fx:+io+))
  (is (eq :state fx:+state+))
  (is (eq :database fx:+database+))
  (is (eq :network fx:+network+))
  (is (eq :filesystem fx:+filesystem+))
  (is (eq :random fx:+random+))
  (is (eq :time fx:+time+)))

(deftest test-defeffect
  (fx:defeffect :test-effect
    :documentation "A test effect"
    :parent :io)
  (let ((info (fx:effect-info :test-effect)))
    (is (not (null info)))
    (is (eq :test-effect (fx::effect-info-name info)))
    (is (equal "A test effect" (fx::effect-info-documentation info)))
    (is (eq :io (fx::effect-info-parent info)))))

(deftest test-list-effects
  (let ((effects (fx:list-effects)))
    (is (member :io effects))
    (is (member :database effects))
    (is (member :network effects))))

;;; Effect declaration tests

(deftest test-declare-effects
  (fx:declare-effects test-fn-1 (:io :database))
  (let ((effects (fx:effects-of 'test-fn-1)))
    (is (equal '(:io :database) effects))))

(deftest test-effects-of-undeclared
  (is (null (fx:effects-of 'nonexistent-function-xyz))))

(deftest test-pure-p-declared-pure
  (fx:declare-effects pure-fn-test ())
  (is (fx:pure-p 'pure-fn-test)))

(deftest test-pure-p-undeclared
  ;; Undeclared functions are considered pure (no known effects)
  (is (fx:pure-p 'another-nonexistent-fn)))

(deftest test-pure-p-effectful
  (fx:declare-effects effectful-fn-test (:io))
  (is (not (fx:pure-p 'effectful-fn-test))))

(deftest test-requires-effect-p
  (fx:declare-effects multi-effect-fn (:io :database :network))
  (is (fx:requires-effect-p 'multi-effect-fn :io))
  (is (fx:requires-effect-p 'multi-effect-fn :database))
  (is (fx:requires-effect-p 'multi-effect-fn :network))
  (is (not (fx:requires-effect-p 'multi-effect-fn :random))))

;;; defun-effects tests

(deftest test-defun-effects
  (fx:defun-effects test-effectful-add (a b) (:io)
    (+ a b))
  (is (= 5 (test-effectful-add 2 3)))
  (is (equal '(:io) (fx:effects-of 'test-effectful-add))))

;;; defpure tests

(deftest test-defpure
  (fx:defpure test-pure-multiply (a b)
    (* a b))
  (is (= 12 (test-pure-multiply 3 4)))
  (is (fx:pure-p 'test-pure-multiply))
  (is (null (fx:effects-of 'test-pure-multiply))))

;;; Effect context tests

(deftest test-with-effects-allowed
  ;; Should not error when all effects are allowed
  (fx:with-allowed-effects (:io :database)
    (fx:with-effects (:io)
      :ok))
  (is t))

(deftest test-with-effects-disallowed
  ;; Should error when effect is not allowed
  (is-thrown (error)
    (fx:with-allowed-effects (:database)
      (fx:with-effects (:network)
        :should-not-reach))))

(deftest test-with-pure-context-no-effects
  ;; Pure context with no effectful code should work
  (is (= 42 (fx:with-pure-context
              (+ 40 2)))))

(deftest test-with-pure-context-rejects-effects
  ;; Pure context should reject any effects
  (is-thrown (error)
    (fx:with-pure-context
      (fx:with-effects (:io)
        :should-not-reach))))

(deftest test-effect-subsumption
  ;; :database is a child of :io
  (fx:with-allowed-effects (:io)
    (fx:with-effects (:database)
      :ok))
  (is t))

;;; Effect handler tests

(deftest test-with-effect-handler-basic
  (let ((log-messages '()))
    (fx:with-effect-handler
        ((:log (msg) (push msg log-messages)))
      (fx:perform :log "Hello")
      (fx:perform :log "World"))
    (is (equal '("World" "Hello") log-messages))))

(deftest test-with-effect-handler-multiple
  (let ((logs '())
        (metrics '()))
    (fx:with-effect-handler
        ((:log (msg) (push msg logs))
         (:metric (k v) (push (cons k v) metrics)))
      (fx:perform :log "Starting")
      (fx:perform :metric :count 42)
      (fx:perform :log "Done"))
    (is (equal '("Done" "Starting") logs))
    (is (equal '((:count . 42)) metrics))))

(deftest test-with-effect-handler-return-value
  (let ((result
          (fx:with-effect-handler
              ((:get-value () 100))
            (* 2 (fx:perform :get-value)))))
    (is (= 200 result))))

(deftest test-perform-unhandled-effect
  (is-thrown (error)
    (fx:perform :unhandled-effect-xyz)))

(deftest test-nested-effect-handlers
  ;; Inner handler should shadow outer
  (let ((results '()))
    (fx:with-effect-handler
        ((:action () (push :outer results)))
      (fx:perform :action)
      (fx:with-effect-handler
          ((:action () (push :inner results)))
        (fx:perform :action))
      (fx:perform :action))
    (is (equal '(:outer :inner :outer) results))))

;;; list-effectful-functions tests

(deftest test-list-effectful-functions
  ;; Clear any previous declarations from other tests
  (fx:declare-effects listed-fn-1 (:io))
  (fx:declare-effects listed-fn-2 (:database :network))
  (let ((fns (fx:list-effectful-functions)))
    ;; Should contain our declared functions
    (is (assoc 'listed-fn-1 fns))
    (is (assoc 'listed-fn-2 fns))
    ;; Check effects are correct
    (is (equal '(:io) (cdr (assoc 'listed-fn-1 fns))))
    (is (equal '(:database :network) (cdr (assoc 'listed-fn-2 fns))))))
