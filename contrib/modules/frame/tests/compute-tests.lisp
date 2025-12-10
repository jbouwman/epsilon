(defpackage #:epsilon.frame.compute-tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column)
   (#:ops #:epsilon.frame.ops)
   (#:compute #:epsilon.frame.compute)
   (#:frame #:epsilon.frame)))

(in-package #:epsilon.frame.compute-tests)

;;; Expression tests

(deftest test-expression-creation ()
  "Test creating expressions"
  (let ((expr1 (ops:col-ref :a))
        (expr2 (ops:lit 10))
        (expr3 (ops:+expr (ops:col-ref :a) (ops:lit 5))))
    (is-eq (ops:expression-op expr1) :col-ref)
    (is-equal (ops:expression-args expr1) '(:a))
    (is-eq (ops:expression-op expr2) :lit)
    (is-equal (ops:expression-args expr2) '(10))
    (is-eq (ops:expression-op expr3) :+)))

(deftest test-arithmetic-expressions ()
  "Test arithmetic expression builders"
  (let ((add (ops:+expr (ops:col-ref :a) (ops:col-ref :b)))
        (sub (ops:-expr (ops:col-ref :a) (ops:lit 10)))
        (mul (ops:*expr (ops:lit 2) (ops:col-ref :x)))
        (div (ops:/expr (ops:col-ref :total) (ops:col-ref :count))))
    (is-eq (ops:expression-op add) :+)
    (is-eq (ops:expression-op sub) :-)
    (is-eq (ops:expression-op mul) :*)
    (is-eq (ops:expression-op div) :/)))

(deftest test-comparison-expressions ()
  "Test comparison expression builders"
  (let ((gt (ops:>expr (ops:col-ref :age) (ops:lit 18)))
        (lt (ops:<expr (ops:col-ref :price) (ops:lit 100)))
        (eq (ops:=expr (ops:col-ref :status) (ops:lit "active"))))
    (is-eq (ops:expression-op gt) :>)
    (is-eq (ops:expression-op lt) :<)
    (is-eq (ops:expression-op eq) :=)))

(deftest test-logical-expressions ()
  "Test logical expression builders"
  (let ((and-expr (ops:and-expr 
                   (ops:>expr (ops:col-ref :age) (ops:lit 18))
                   (ops:<expr (ops:col-ref :age) (ops:lit 65))))
        (or-expr (ops:or-expr
                  (ops:=expr (ops:col-ref :status) (ops:lit "active"))
                  (ops:=expr (ops:col-ref :status) (ops:lit "pending"))))
        (not-expr (ops:not-expr (ops:=expr (ops:col-ref :deleted) (ops:lit t)))))
    (is-eq (ops:expression-op and-expr) :and)
    (is-eq (ops:expression-op or-expr) :or)
    (is-eq (ops:expression-op not-expr) :not)))

(deftest test-aggregation-expressions ()
  "Test aggregation expression builders"
  (let ((sum (ops:sum-expr (ops:col-ref :amount)))
        (mean (ops:mean-expr (ops:col-ref :score)))
        (min (ops:min-expr (ops:col-ref :price)))
        (max (ops:max-expr (ops:col-ref :price)))
        (count (ops:count-expr)))
    (is-eq (ops:expression-op sum) :sum)
    (is-eq (ops:expression-op mean) :mean)
    (is-eq (ops:expression-op min) :min)
    (is-eq (ops:expression-op max) :max)
    (is-eq (ops:expression-op count) :count)))

;;; Evaluation tests

(deftest test-expression-evaluation ()
  "Test evaluating expressions with a context"
  (let* ((context (make-hash-table :test 'equal)))
    ;; Set up context with columns
    (setf (gethash "a" context) (col:column :int32 1 2 3 4 5))
    (setf (gethash "b" context) (col:column :int32 10 20 30 40 50))
    
    ;; Test column reference
    (let ((result (ops:evaluate-expression (ops:col-ref :a) context)))
      (is-equal (col:column-to-list result) '(1 2 3 4 5)))
    
    ;; Test literal
    (let ((result (ops:evaluate-expression (ops:lit 100) context)))
      (is-= (col:column-length result) 5)
      (is-= (col:column-get result 0) 100))
    
    ;; Test arithmetic
    (let ((result (ops:evaluate-expression 
                   (ops:+expr (ops:col-ref :a) (ops:col-ref :b)) 
                   context)))
      (is-equal (col:column-to-list result) '(11 22 33 44 55)))
    
    ;; Test comparison
    (let ((result (ops:evaluate-expression
                   (ops:>expr (ops:col-ref :a) (ops:lit 3))
                   context)))
      (is-equal (col:column-to-list result) '(nil nil nil t t)))))

(deftest test-aggregation-evaluation ()
  "Test evaluating aggregation expressions"
  (let* ((context (make-hash-table :test 'equal)))
    (setf (gethash "values" context) (col:column :int32 1 2 3 4 5))
    
    (is-= (ops:evaluate-expression (ops:sum-expr (ops:col-ref :values)) context) 15)
    (is-= (ops:evaluate-expression (ops:mean-expr (ops:col-ref :values)) context) 3)
    (is-= (ops:evaluate-expression (ops:min-expr (ops:col-ref :values)) context) 1)
    (is-= (ops:evaluate-expression (ops:max-expr (ops:col-ref :values)) context) 5)
    (is-= (ops:evaluate-expression (ops:count-expr) context) 5)))

;;; Compute integration tests

(deftest test-compute-column ()
  "Test computing a new column"
  (let* ((frame (frame:frame
                 :a '(1 2 3 4 5)
                 :b '(10 20 30 40 50)))
         (result (compute:compute frame :sum
                                  (ops:+expr (ops:col-ref :a) 
                                             (ops:col-ref :b)))))
    (is-= (frame:ncols result) 3)
    (is-equal (frame:column-names result) '("a" "b" "sum"))
    (is-equal (col:column-to-list (frame:get-column result :sum))
                  '(11 22 33 44 55))))

(deftest test-compute-with-literals ()
  "Test computing with literal values"
  (let* ((frame (frame:frame :x '(1 2 3 4 5)))
         (result (compute:compute frame :doubled
                                  (ops:*expr (ops:col-ref :x) 
                                             (ops:lit 2)))))
    (is-equal (col:column-to-list (frame:get-column result :doubled))
                  '(2 4 6 8 10))))

(deftest test-compute-multiple ()
  "Test computing multiple columns at once"
  (let* ((frame (frame:frame 
                 :price '(100 200 150)
                 :quantity '(2 1 3)))
         (result (compute:compute* frame
                                   :total (ops:*expr (ops:col-ref :price)
                                                     (ops:col-ref :quantity))
                                   :tax (ops:*expr (ops:col-ref :price)
                                                   (ops:lit 0.1d0)))))
    (is-= (frame:ncols result) 4)
    (is-equal (col:column-to-list (frame:get-column result :total))
                  '(200 200 450))
    (is-equal (col:column-to-list (frame:get-column result :tax))
                  '(10.0d0 20.0d0 15.0d0))))

(deftest test-aggregation ()
  "Test frame aggregation"
  (let* ((frame (frame:frame
                 :values '(1 2 3 4 5)
                 :weights '(0.1 0.2 0.3 0.2 0.2)))
         (result (compute:agg frame
                              :sum (ops:sum-expr (ops:col-ref :values))
                              :mean (ops:mean-expr (ops:col-ref :values))
                              :min (ops:min-expr (ops:col-ref :values))
                              :max (ops:max-expr (ops:col-ref :values))
                              :count (ops:count-expr))))
    (is-= (frame:nrows result) 1)
    (is-= (frame:ncols result) 5)
    (let ((row (frame:get-row result 0)))
      (is-= (getf row :sum) 15)
      (is-= (getf row :mean) 3)
      (is-= (getf row :min) 1)
      (is-= (getf row :max) 5)
      (is-= (getf row :count) 5))))

(deftest test-filter-with-expression ()
  "Test filtering with computed expressions"
  (let* ((frame (frame:frame
                 :name '("Alice" "Bob" "Charlie" "David")
                 :age '(25 30 35 28)
                 :score '(92.5 87.3 95.0 88.5)))
         ;; Compute a boolean column for filtering
         (with-filter (compute:compute frame :adult
                                       (ops:>=expr (ops:col-ref :age)
                                                   (ops:lit 30))))
         ;; Filter using the computed column
         (filtered (frame:where with-filter
                                (lambda (row) (getf row :adult)))))
    (is-= (frame:nrows filtered) 2)
    (is-equal (col:column-to-list (frame:get-column filtered :name))
                  '("Bob" "Charlie"))))

(deftest test-chained-computations ()
  "Test chaining multiple compute operations"
  (let* ((frame (frame:frame
                 :base '(100 200 300)))
         (result (frame:-> frame
                           (compute:compute :doubled 
                                            (ops:*expr (ops:col-ref :base) 
                                                       (ops:lit 2)))
                           (compute:compute :tripled
                                            (ops:*expr (ops:col-ref :base)
                                                       (ops:lit 3)))
                           (compute:compute :sum
                                            (ops:+expr (ops:col-ref :doubled)
                                                       (ops:col-ref :tripled))))))
    (is-= (frame:ncols result) 4)
    (is-equal (col:column-to-list (frame:get-column result :sum))
                  '(500 1000 1500))))

(deftest test-standard-deviation ()
  "Test standard deviation calculation"
  (let* ((frame (frame:frame :values '(2 4 6 8 10)))
         (result (compute:agg frame
                              :std (ops:std-expr (ops:col-ref :values))
                              :var (ops:var-expr (ops:col-ref :values)))))
    (let ((row (frame:get-row result 0)))
      ;; Variance should be 8 (population variance)
      (is-= (getf row :var) 8.0d0)
      ;; Std dev should be sqrt(8) ≈ 2.828
      (is-true (< (abs (- (getf row :std) (sqrt 8.0d0))) 0.001)))))

(deftest test-complex-expression ()
  "Test complex nested expressions"
  (let* ((frame (frame:frame
                 :a '(1 2 3 4 5)
                 :b '(5 4 3 2 1)))
         (result (compute:compute frame :complex
                                  (ops:/expr 
                                   (ops:+expr (ops:*expr (ops:col-ref :a) 
                                                         (ops:lit 2))
                                              (ops:col-ref :b))
                                   (ops:lit 2)))))
    ;; Result should be ((a * 2) + b) / 2
    (is-equal (col:column-to-list (frame:get-column result :complex))
                  '(3.5d0 4.0d0 4.5d0 5.0d0 5.5d0))))
