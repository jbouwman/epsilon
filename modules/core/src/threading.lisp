;;;; threading.lisp - Data flow pipelines
;;;;
;;;; Threading macros transform deeply nested expressions into readable,
;;;; linear pipelines. They are the most impactful ergonomic improvement
;;;; for functional code.

(defpackage :epsilon.threading
  (:use :cl)
  (:export
   #:->
   #:->>
   #:as->
   #:some->
   #:some->>
   #:cond->
   #:cond->>))

(in-package :epsilon.threading)

(defmacro -> (x &rest forms)
  "Thread X through FORMS as first argument.
   Each form can be a symbol (called as function) or a list (inserted as first arg).

   Examples:
     (-> x f)           => (f x)
     (-> x (f a b))     => (f x a b)
     (-> x f (g a) h)   => (h (g (f x) a))

   Transforms nested calls into readable pipelines:
     (-> config
         (assoc :host \"localhost\")
         (assoc :port 8080)
         validate)"
  (if (null forms)
      x
      (let ((form (first forms)))
        (if (listp form)
            `(-> (,(first form) ,x ,@(rest form)) ,@(rest forms))
            `(-> (,form ,x) ,@(rest forms))))))

(defmacro ->> (x &rest forms)
  "Thread X through FORMS as last argument.
   Each form can be a symbol (called as function) or a list (inserted as last arg).

   Examples:
     (->> x f)          => (f x)
     (->> x (f a b))    => (f a b x)
     (->> x f (g a) h)  => (h (g a (f x)))

   Useful for sequence operations:
     (->> items
          (filter even?)
          (map square)
          (reduce +))"
  (if (null forms)
      x
      (let ((form (first forms)))
        (if (listp form)
            `(->> (,@form ,x) ,@(rest forms))
            `(->> (,form ,x) ,@(rest forms))))))

(defmacro as-> (x name &rest forms)
  "Thread X through FORMS, binding to NAME at each step.
   Allows arbitrary placement of the threaded value.

   Examples:
     (as-> x $ (f $ a) (g b $))
     => (let (($  x))
          (let (($ (f $ a)))
            (g b $)))

   Useful when value position varies:
     (as-> data $
           (parse $)
           (transform a $ b)
           (format nil \"Result: ~a\" $))"
  (if (null forms)
      x
      `(let ((,name ,x))
         (as-> ,(first forms) ,name ,@(rest forms)))))

(defmacro some-> (x &rest forms)
  "Thread X through FORMS as first argument, short-circuiting on nil.
   Returns nil if X or any intermediate result is nil.

   Examples:
     (some-> nil f)           => nil
     (some-> x f)             => (f x) if x is non-nil
     (some-> x (f a) (g b))   => (g (f x a) b) if all non-nil

   Safe navigation pattern:
     (some-> request
             (get-header \"Authorization\")
             parse-token
             validate-token)"
  (if (null forms)
      x
      (let ((result (gensym "RESULT")))
        `(let ((,result ,x))
           (when ,result
             (some-> (-> ,result ,(first forms)) ,@(rest forms)))))))

(defmacro some->> (x &rest forms)
  "Thread X through FORMS as last argument, short-circuiting on nil.
   Returns nil if X or any intermediate result is nil.

   Examples:
     (some->> nil f)          => nil
     (some->> x (f a b))      => (f a b x) if x is non-nil

   Safe sequence operations:
     (some->> user-ids
              (filter valid-id-p)
              (map fetch-user)
              (remove nil))"
  (if (null forms)
      x
      (let ((result (gensym "RESULT")))
        `(let ((,result ,x))
           (when ,result
             (some->> (->> ,result ,(first forms)) ,@(rest forms)))))))

(defmacro cond-> (x &rest clauses)
  "Thread X through CLAUSES conditionally as first argument.
   Each clause is (test form) - form is applied only if test is true.
   The value threads through regardless of whether forms are applied.

   Examples:
     (cond-> x
       (test1 (f a))    ; Apply (f x a) if test1 is true
       (test2 (g b)))   ; Apply (g result b) if test2 is true

   Conditional transformations:
     (cond-> order
       ((> total 100) (apply-discount 0.1))
       (member-p (add-loyalty-bonus))
       (express-p (mark-priority)))"
  (if (null clauses)
      x
      (destructuring-bind ((test form) &rest rest) clauses
        (let ((result (gensym "RESULT")))
          `(let ((,result ,x))
             (cond-> (if ,test (-> ,result ,form) ,result)
               ,@rest))))))

(defmacro cond->> (x &rest clauses)
  "Thread X through CLAUSES conditionally as last argument.
   Each clause is (test form) - form is applied only if test is true.
   The value threads through regardless of whether forms are applied.

   Examples:
     (cond->> items
       (filter-p (filter predicate))
       (sort-p (sort-by key-fn)))

   Conditional sequence operations:
     (cond->> users
       (active-only (filter active-p))
       (with-limit (take limit))
       (sorted (sort-by :name)))"
  (if (null clauses)
      x
      (destructuring-bind ((test form) &rest rest) clauses
        (let ((result (gensym "RESULT")))
          `(let ((,result ,x))
             (cond->> (if ,test (->> ,result ,form) ,result)
               ,@rest))))))
