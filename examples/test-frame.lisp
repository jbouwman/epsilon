;; Test the epsilon.frame module
(format t "~%=== Testing epsilon.frame Module ===~%~%")

;; Load the module
(epsilon.loader:load-module (epsilon.loader:environment) "epsilon.frame")

;; Basic column operations
(format t "1. Column Operations:~%")
(let ((col (epsilon.frame.column:column :int32 1 2 3 4 5)))
  (format t "   Created column: ~a~%" col)
  (format t "   Length: ~a~%" (epsilon.frame.column:column-length col))
  (format t "   First element: ~a~%" (epsilon.frame.column:column-get col 0))
  (format t "   Slice [1:3]: ~a~%"
          (epsilon.frame.column:column-to-list 
           (epsilon.frame.column:column-slice col 1 3))))

;; Frame creation and access
(format t "~%2. Frame Creation:~%")
(defparameter *df* 
  (epsilon.frame:frame
   :name '("Alice" "Bob" "Charlie")
   :age '(25 30 35)
   :score '(92.5 87.3 95.0)))

(format t "   Shape: ~a~%" (epsilon.frame:shape *df*))
(format t "   Columns: ~a~%" (epsilon.frame:column-names *df*))
(format t "   First row: ~a~%" (epsilon.frame:get-row *df* 0))

;; Selection
(format t "~%3. Column Selection:~%")
(let ((selected (epsilon.frame:select *df* :name :score)))
  (format t "   Selected columns: ~a~%" (epsilon.frame:column-names selected))
  (format t "   Shape: ~a~%" (epsilon.frame:shape selected)))

;; Filtering
(format t "~%4. Row Filtering:~%")
(let ((filtered (epsilon.frame:where *df* 
                                    (lambda (row) 
                                      (> (getf row :age) 28)))))
  (format t "   Filtered rows (age > 28):~%")
  (loop for i from 0 below (epsilon.frame:nrows filtered)
        do (format t "     ~a~%" (epsilon.frame:get-row filtered i))))

;; Slicing
(format t "~%5. Slicing:~%")
(let ((head (epsilon.frame:head *df* 2)))
  (format t "   First 2 rows:~%")
  (loop for i from 0 below (epsilon.frame:nrows head)
        do (format t "     ~a~%" (epsilon.frame:get-row head i))))

;; Adding columns
(format t "~%6. Adding Columns:~%")
(let* ((bonus '(100 50 75))
       (with-bonus (epsilon.frame:add-column *df* :bonus bonus)))
  (format t "   Columns after adding bonus: ~a~%" 
          (epsilon.frame:column-names with-bonus))
  (format t "   First row with bonus: ~a~%" 
          (epsilon.frame:get-row with-bonus 0)))

;; Conversion
(format t "~%7. Conversions:~%")
(let ((as-lists (epsilon.frame:frame->lists *df*)))
  (format t "   As lists: ~a~%" as-lists))

(let ((as-plists (epsilon.frame:frame->plists *df*)))
  (format t "   First row as plist: ~a~%" (first as-plists)))

;; Create from plists
(format t "~%8. Create from plists:~%")
(let* ((data '((:product "Apple" :price 1.20 :quantity 100)
               (:product "Banana" :price 0.50 :quantity 200)
               (:product "Orange" :price 0.80 :quantity 150)))
       (products (epsilon.frame:plists->frame data)))
  (format t "   Products frame shape: ~a~%" (epsilon.frame:shape products))
  (format t "   Products columns: ~a~%" (epsilon.frame:column-names products)))

(format t "~%=== All Tests Passed ===~%")