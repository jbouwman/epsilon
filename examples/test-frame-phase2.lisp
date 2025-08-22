;; Test Phase 2 features of epsilon.frame module
(format t "~%=== Testing epsilon.frame Phase 2: Computation Engine ===~%~%")

;; Load the module
(epsilon.loader:load-module (epsilon.loader:environment) "epsilon.frame")

;; Test 1: Basic expressions and compute
(format t "1. Expression-based computation:~%")
(defparameter *sales*
  (epsilon.frame:frame
   :product '("Widget" "Gadget" "Gizmo" "Widget" "Gadget")
   :quantity '(10 5 8 15 7)
   :price '(19.99 29.99 39.99 19.99 29.99)))

;; Compute total sales
(defparameter *with-total*
  (epsilon.frame:compute *sales* :total
                        (epsilon.frame:*expr 
                         (epsilon.frame:col-ref :quantity)
                         (epsilon.frame:col-ref :price))))

(format t "   Added total column: ~a~%" 
        (epsilon.frame:column-names *with-total*))
(format t "   First row: ~a~%"
        (epsilon.frame:get-row *with-total* 0))

;; Test 2: Multiple computations
(format t "~%2. Multiple computations:~%")
(defparameter *with-calculations*
  (epsilon.frame:compute* *sales*
                          :total (epsilon.frame:*expr 
                                 (epsilon.frame:col-ref :quantity)
                                 (epsilon.frame:col-ref :price))
                          :tax (epsilon.frame:*expr
                               (epsilon.frame:*expr 
                                (epsilon.frame:col-ref :quantity)
                                (epsilon.frame:col-ref :price))
                               (epsilon.frame:lit 0.08))
                          :final (epsilon.frame:+expr
                                 (epsilon.frame:*expr 
                                  (epsilon.frame:col-ref :quantity)
                                  (epsilon.frame:col-ref :price))
                                 (epsilon.frame:*expr
                                  (epsilon.frame:*expr 
                                   (epsilon.frame:col-ref :quantity)
                                   (epsilon.frame:col-ref :price))
                                  (epsilon.frame:lit 0.08)))))

(format t "   Columns: ~a~%" (epsilon.frame:column-names *with-calculations*))
(format t "   Sample row: ~a~%"
        (epsilon.frame:get-row *with-calculations* 0))

;; Test 3: Aggregations
(format t "~%3. Aggregations:~%")
(defparameter *summary*
  (epsilon.frame:agg *with-total*
                    :total-quantity (epsilon.frame:sum-expr 
                                    (epsilon.frame:col-ref :quantity))
                    :avg-price (epsilon.frame:mean-expr 
                               (epsilon.frame:col-ref :price))
                    :total-revenue (epsilon.frame:sum-expr 
                                   (epsilon.frame:col-ref :total))
                    :product-count (epsilon.frame:count-expr)))

(format t "   Summary shape: ~a~%" (epsilon.frame:shape *summary*))
(format t "   Summary data: ~a~%"
        (epsilon.frame:get-row *summary* 0))

;; Test 4: Filtering with expressions
(format t "~%4. Expression-based filtering:~%")
(defparameter *high-value*
  (epsilon.frame:-> *with-total*
                    (epsilon.frame:compute :high-value
                                          (epsilon.frame:>expr 
                                           (epsilon.frame:col-ref :total)
                                           (epsilon.frame:lit 200)))
                    (epsilon.frame:where (lambda (row) (getf row :high-value)))))

(format t "   High-value sales (total > 200):~%")
(loop for i from 0 below (epsilon.frame:nrows *high-value*)
      do (format t "     ~a~%" (epsilon.frame:get-row *high-value* i)))

;; Test 5: Threading macro
(format t "~%5. Threading macro for pipeline:~%")
(defparameter *pipeline-result*
  (epsilon.frame:-> *sales*
                    ;; Add total column
                    (epsilon.frame:compute :total
                                          (epsilon.frame:*expr 
                                           (epsilon.frame:col-ref :quantity)
                                           (epsilon.frame:col-ref :price)))
                    ;; Filter high-value items
                    (epsilon.frame:where 
                     (lambda (row) (> (getf row :total) 150)))
                    ;; Select relevant columns
                    (epsilon.frame:select :product :quantity :total)
                    ;; Take top 3
                    (epsilon.frame:head 3)))

(format t "   Pipeline result shape: ~a~%" (epsilon.frame:shape *pipeline-result*))
(format t "   Pipeline result:~%")
(loop for i from 0 below (epsilon.frame:nrows *pipeline-result*)
      do (format t "     ~a~%" (epsilon.frame:get-row *pipeline-result* i)))

;; Test 6: Views (zero-copy)
(format t "~%6. Zero-copy views:~%")
(defparameter *large-frame*
  (epsilon.frame:frame
   :id (loop for i from 1 to 100 collect i)
   :value (loop for i from 1 to 100 collect (* i 10))))

;; Create a view of first 10 rows
(defparameter *view*
  (epsilon.frame:frame-view *large-frame* 
                           :rows (loop for i from 0 below 10 collect i)))

(format t "   Original frame shape: ~a~%" (epsilon.frame:shape *large-frame*))
(format t "   View rows: ~a~%" (epsilon.frame:nrows *view*))

;; Materialize the view
(defparameter *materialized*
  (epsilon.frame:materialize-view *view*))

(format t "   Materialized shape: ~a~%" (epsilon.frame:shape *materialized*))
(format t "   First row of materialized: ~a~%"
        (epsilon.frame:get-row *materialized* 0))

;; Test 7: Complex expressions
(format t "~%7. Complex nested expressions:~%")
(defparameter *complex*
  (epsilon.frame:frame
   :a '(1 2 3 4 5)
   :b '(5 4 3 2 1)
   :c '(2 2 2 2 2)))

(defparameter *complex-result*
  (epsilon.frame:compute *complex* :result
                        ;; ((a + b) * c) / 2
                        (epsilon.frame:/expr
                         (epsilon.frame:*expr
                          (epsilon.frame:+expr 
                           (epsilon.frame:col-ref :a)
                           (epsilon.frame:col-ref :b))
                          (epsilon.frame:col-ref :c))
                         (epsilon.frame:lit 2))))

(format t "   Complex expression ((a + b) * c) / 2:~%")
(format t "   Result column: ~a~%"
        (epsilon.frame.column:column-to-list 
         (epsilon.frame:get-column *complex-result* :result)))

;; Test 8: Statistical aggregations
(format t "~%8. Statistical aggregations:~%")
(defparameter *stats-frame*
  (epsilon.frame:frame
   :values '(10 20 30 40 50)))

(defparameter *stats*
  (epsilon.frame:agg *stats-frame*
                    :mean (epsilon.frame:mean-expr (epsilon.frame:col-ref :values))
                    :std (epsilon.frame:std-expr (epsilon.frame:col-ref :values))
                    :var (epsilon.frame:var-expr (epsilon.frame:col-ref :values))
                    :min (epsilon.frame:min-expr (epsilon.frame:col-ref :values))
                    :max (epsilon.frame:max-expr (epsilon.frame:col-ref :values))))

(format t "   Statistics: ~a~%"
        (epsilon.frame:get-row *stats* 0))

(format t "~%=== Phase 2 Tests Complete ===~%")