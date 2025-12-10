(defpackage #:epsilon.frame.view-tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column)
   (#:view #:epsilon.frame.view)
   (#:frame #:epsilon.frame)))

(in-package #:epsilon.frame.view-tests)

(deftest test-frame-view-creation ()
  "Test creating frame views"
  (let* ((frame (frame:frame
                 :a '(1 2 3 4 5)
                 :b '(10 20 30 40 50)
                 :c '("x" "y" "z" "w" "v")))
         (view1 (view:frame-view frame :rows '(0 2 4)))
         (view2 (view:frame-view frame :columns '(:a :c)))
         (view3 (view:frame-view frame :rows '(1 3) :columns '(:b))))
    (is-not-null view1)
    (is-not-null view2)
    (is-not-null view3)
    (is-eq (view:view-source view1) frame)
    (is-equal (view:view-row-indices view1) '(0 2 4))
    (is-equal (view:view-column-names view2) '("a" "c"))))

(deftest test-column-view-creation ()
  "Test creating column views"
  (let* ((col (col:column :int32 10 20 30 40 50))
         (view1 (view:column-view col :indices '(0 2 4)))
         (view2 (view:column-view col)))
    (is-not-null view1)
    (is-not-null view2)
    (is-eq (view:column-view-source view1) col)
    (is-equal (view:column-view-indices view1) '(0 2 4))
    (is-not (view:column-view-indices view2))))

(deftest test-materialize-frame-view ()
  "Test materializing frame views into new frames"
  (let* ((frame (frame:frame
                 :a '(1 2 3 4 5)
                 :b '(10 20 30 40 50)
                 :c '("a" "b" "c" "d" "e")))
         ;; View with row selection
         (row-view (view:frame-view frame :rows '(0 2 4)))
         (mat1 (view:materialize-view row-view))
         ;; View with column selection
         (col-view (view:frame-view frame :columns '(:a :c)))
         (mat2 (view:materialize-view col-view))
         ;; View with both
         (both-view (view:frame-view frame :rows '(1 3) :columns '(:b :c)))
         (mat3 (view:materialize-view both-view)))
    
    ;; Test row-selected view
    (is-= (frame:nrows mat1) 3)
    (is-= (frame:ncols mat1) 3)
    (is-equal (col:column-to-list (frame:get-column mat1 :a)) '(1 3 5))
    
    ;; Test column-selected view
    (is-= (frame:nrows mat2) 5)
    (is-= (frame:ncols mat2) 2)
    (is-equal (frame:column-names mat2) '("a" "c"))
    
    ;; Test both row and column selected view
    (is-= (frame:nrows mat3) 2)
    (is-= (frame:ncols mat3) 2)
    (is-equal (col:column-to-list (frame:get-column mat3 :b)) '(20 40))))

(deftest test-materialize-column-view ()
  "Test materializing column views"
  (let* ((col (col:column :float64 1.0 2.0 3.0 4.0 5.0))
         (view (view:column-view col :indices '(1 3)))
         (materialized (view:materialize-column-view view)))
    (is-= (col:column-length materialized) 2)
    (is-equal (col:column-to-list materialized) '(2.0d0 4.0d0))))


(deftest test-view-zero-copy ()
  "Test that views don't copy data until materialized"
  (let* ((frame (frame:frame
                 :data (col:column :int32 1 2 3 4 5)))
         (view1 (view:frame-view frame :rows '(0 1 2)))
         (view2 (view:frame-view frame :rows '(2 3 4))))
    ;; Views should share the same source
    (is-eq (view:view-source view1) (view:view-source view2))
    ;; Modifying views should not affect each other after materialization
    (let ((mat1 (view:materialize-view view1))
          (mat2 (view:materialize-view view2)))
      (is-true (not (eq mat1 mat2)))
      (is-= (frame:nrows mat1) 3)
      (is-= (frame:nrows mat2) 3))))

(deftest test-view-chaining ()
  "Test creating views of views"
  (let* ((frame (frame:frame
                 :a '(1 2 3 4 5 6 7 8 9 10)
                 :b '(10 20 30 40 50 60 70 80 90 100)))
         ;; First view: rows 0-4
         (view1 (view:frame-view frame :rows '(0 1 2 3 4)))
         (mat1 (view:materialize-view view1))
         ;; Second view: even indices from first view
         (view2 (view:frame-view mat1 :rows '(0 2 4)))
         (mat2 (view:materialize-view view2)))
    (is-= (frame:nrows mat2) 3)
    (is-equal (col:column-to-list (frame:get-column mat2 :a))
                  '(1 3 5))))

(deftest test-empty-view ()
  "Test views with empty selections"
  (let* ((frame (frame:frame
                 :x '(1 2 3)
                 :y '(4 5 6)))
         (empty-view (view:frame-view frame :rows '()))
         (materialized (view:materialize-view empty-view)))
    (is-= (frame:nrows materialized) 0)
    (is-= (frame:ncols materialized) 2)))

(deftest test-full-view ()
  "Test view without any selection (full frame)"
  (let* ((frame (frame:frame
                 :a '(1 2 3)
                 :b '(4 5 6)))
         (full-view (view:frame-view frame))
         (materialized (view:materialize-view full-view)))
    (is-= (frame:nrows materialized) 3)
    (is-= (frame:ncols materialized) 2)
    (is-equal (frame:frame->lists materialized)
                  (frame:frame->lists frame))))