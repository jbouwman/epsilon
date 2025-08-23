(defpackage #:epsilon.frame-tests
  (:use #:cl #:epsilon.test)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column)))

(in-package #:epsilon.frame-tests)

;; Data type tests

(deftest test-dtype-registry ()
  "Test dtype registration and lookup"
  (is-not-null (dtype:find-dtype :int32))
  (is-not-null (dtype:find-dtype :float64))
  (is-not-null (dtype:find-dtype :string))
  (is-thrown (error) (dtype:find-dtype :invalid)))

(deftest test-dtype-inference ()
  "Test dtype inference from values"
  (is-eq (dtype:infer-dtype 42) :int32)
  (is-eq (dtype:infer-dtype 1000000000000) :int64)
  (is-eq (dtype:infer-dtype 3.14) :float32)
  (is-eq (dtype:infer-dtype 3.14d0) :float64)
  (is-eq (dtype:infer-dtype "hello") :string)
  (is-eq (dtype:infer-dtype t) :bool))

(deftest test-dtype-coercion ()
  "Test value coercion to dtypes"
  (is-= (dtype:coerce-to-dtype 3.14 :int32) 3)
  (is-= (dtype:coerce-to-dtype 42 :float64) 42.0d0)
  (is-equal (dtype:coerce-to-dtype 123 :string) "123")
  (is-eq (dtype:coerce-to-dtype 1 :bool) t)
  (is-eq (dtype:coerce-to-dtype nil :bool) nil))

;; Column tests

(deftest test-column-creation ()
  "Test column creation"
  (let ((col1 (col:column :int32 1 2 3 4 5))
        (col2 (col:make-column :float64 '(1.0 2.0 3.0)))
        (col3 (col:make-column :string #("a" "b" "c"))))
    (is-= (col:column-length col1) 5)
    (is-= (col:column-length col2) 3)
    (is-= (col:column-length col3) 3)
    (is-eq (col:column-dtype col1) :int32)
    (is-eq (col:column-dtype col2) :float64)
    (is-eq (col:column-dtype col3) :string)))

(deftest test-column-access ()
  "Test column element access"
  (let ((col (col:column :int32 10 20 30 40 50)))
    (is-= (col:column-get col 0) 10)
    (is-= (col:column-get col 2) 30)
    (is-= (col:column-get col 4) 50)
    (is-thrown (error) (col:column-get col -1))
    (is-thrown (error) (col:column-get col 5))))

(deftest test-column-slice ()
  "Test column slicing"
  (let* ((col (col:column :int32 1 2 3 4 5))
         (slice1 (col:column-slice col 1 4))
         (slice2 (col:column-slice col 0 2))
         (slice3 (col:column-slice col 3)))
    (is-= (col:column-length slice1) 3)
    (is-equal (col:column-to-list slice1) '(2 3 4))
    (is-= (col:column-length slice2) 2)
    (is-equal (col:column-to-list slice2) '(1 2))
    (is-= (col:column-length slice3) 2)
    (is-equal (col:column-to-list slice3) '(4 5))))

(deftest test-column-map ()
  "Test mapping function over column"
  (let* ((col (col:column :int32 1 2 3 4 5))
         (doubled (col:column-map (lambda (x) (* x 2)) col))
         (stringified (col:column-map #'princ-to-string col)))
    (is-equal (col:column-to-list doubled) '(2 4 6 8 10))
    (is-eq (col:column-dtype doubled) :int32)
    (is-equal (col:column-to-list stringified) '("1" "2" "3" "4" "5"))
    (is-eq (col:column-dtype stringified) :string)))

(deftest test-column-filter ()
  "Test filtering column by predicate"
  (let* ((col (col:column :int32 1 2 3 4 5 6))
         (evens (col:column-filter #'evenp col))
         (big (col:column-filter (lambda (x) (> x 3)) col)))
    (is-equal (col:column-to-list evens) '(2 4 6))
    (is-equal (col:column-to-list big) '(4 5 6))))

;; Frame tests

(deftest test-frame-creation ()
  "Test frame creation"
  (let ((frame1 (epsilon.frame:frame 
                 :a (col:column :int32 1 2 3)
                 :b (col:column :float64 1.0 2.0 3.0)
                 :c '("x" "y" "z")))
        (frame2 (epsilon.frame:make-frame
                 :x '(10 20 30)
                 :y #(100 200 300))))
    (is-= (epsilon.frame:nrows frame1) 3)
    (is-= (epsilon.frame:ncols frame1) 3)
    (is-= (epsilon.frame:nrows frame2) 3)
    (is-= (epsilon.frame:ncols frame2) 2)
    (is-equal (epsilon.frame:column-names frame1) '("a" "b" "c"))
    (is-equal (epsilon.frame:column-names frame2) '("x" "y"))))

(deftest test-frame-column-access ()
  "Test accessing columns from frame"
  (let ((frame (epsilon.frame:frame
                :a (col:column :int32 1 2 3)
                :b (col:column :string "x" "y" "z"))))
    (let ((col-a (epsilon.frame:get-column frame :a))
          (col-b (epsilon.frame:get-column frame "b")))
      (is-= (col:column-length col-a) 3)
      (is-= (col:column-length col-b) 3)
      (is-eq (col:column-dtype col-a) :int32)
      (is-eq (col:column-dtype col-b) :string))
    (is-thrown (error) (epsilon.frame:get-column frame :nonexistent))))

(deftest test-frame-row-access ()
  "Test accessing rows from frame"
  (let ((frame (epsilon.frame:frame
                :a (col:column :int32 1 2 3)
                :b (col:column :float64 10.0 20.0 30.0)
                :c (col:column :string "x" "y" "z"))))
    (is-equal (epsilon.frame:get-row frame 0) 
                  '(:a 1 :b 10.0d0 :c "x"))
    (is-equal (epsilon.frame:get-row frame 2)
                  '(:a 3 :b 30.0d0 :c "z"))
    (is-thrown (error) (epsilon.frame:get-row frame -1))
    (is-thrown (error) (epsilon.frame:get-row frame 3))))

(deftest test-frame-select ()
  "Test column selection"
  (let* ((frame (epsilon.frame:frame
                 :a '(1 2 3)
                 :b '(4 5 6)
                 :c '(7 8 9)
                 :d '(10 11 12)))
         (selected (epsilon.frame:select frame :a :c)))
    (is-= (epsilon.frame:ncols selected) 2)
    (is-= (epsilon.frame:nrows selected) 3)
    (is-equal (epsilon.frame:column-names selected) '("a" "c"))
    (is-thrown (error) (epsilon.frame:select frame :a :nonexistent))))

(deftest test-frame-slice ()
  "Test row slicing"
  (let* ((frame (epsilon.frame:frame
                 :a '(1 2 3 4 5)
                 :b '(10 20 30 40 50)))
         (slice1 (epsilon.frame:slice frame 1 3))
         (slice2 (epsilon.frame:head frame 2))
         (slice3 (epsilon.frame:tail frame 2)))
    (is-= (epsilon.frame:nrows slice1) 2)
    (is-equal (epsilon.frame:get-row slice1 0) '(:a 2 :b 20))
    (is-= (epsilon.frame:nrows slice2) 2)
    (is-equal (epsilon.frame:get-row slice2 0) '(:a 1 :b 10))
    (is-= (epsilon.frame:nrows slice3) 2)
    (is-equal (epsilon.frame:get-row slice3 0) '(:a 4 :b 40))))

(deftest test-frame-where ()
  "Test row filtering"
  (let* ((frame (epsilon.frame:frame
                 :name '("Alice" "Bob" "Charlie" "David")
                 :age '(25 30 35 28)
                 :score '(92.5 87.3 95.0 88.5)))
         (adults (epsilon.frame:where frame 
                                      (lambda (row) (>= (getf row :age) 30))))
         (high-score (epsilon.frame:where frame
                                          (lambda (row) (> (getf row :score) 90)))))
    (is-= (epsilon.frame:nrows adults) 2)
    (is-equal (col:column-to-list 
                   (epsilon.frame:get-column adults :name))
                  '("Bob" "Charlie"))
    (is-= (epsilon.frame:nrows high-score) 2)
    (is-equal (col:column-to-list
                   (epsilon.frame:get-column high-score :name))
                  '("Alice" "Charlie"))))

(deftest test-frame-add-column ()
  "Test adding columns"
  (let* ((frame (epsilon.frame:frame
                 :a '(1 2 3)
                 :b '(4 5 6)))
         (with-c (epsilon.frame:add-column frame :c '(7 8 9)))
         (computed (epsilon.frame:add-column frame :sum
                                             (col:column :int32 5 7 9))))
    (is-= (epsilon.frame:ncols with-c) 3)
    (is-equal (epsilon.frame:column-names with-c) '("a" "b" "c"))
    (is-= (epsilon.frame:ncols computed) 3)
    (is-equal (col:column-to-list 
                   (epsilon.frame:get-column computed :sum))
                  '(5 7 9))
    ;; Original frame unchanged
    (is-= (epsilon.frame:ncols frame) 2)))

(deftest test-frame-drop-column ()
  "Test dropping columns"
  (let* ((frame (epsilon.frame:frame
                 :a '(1 2 3)
                 :b '(4 5 6)
                 :c '(7 8 9)))
         (without-b (epsilon.frame:drop-column frame :b)))
    (is-= (epsilon.frame:ncols without-b) 2)
    (is-equal (epsilon.frame:column-names without-b) '("a" "c"))
    (is-thrown (error) (epsilon.frame:get-column without-b :b))
    ;; Original frame unchanged
    (is-= (epsilon.frame:ncols frame) 3)))

(deftest test-frame-rename-column ()
  "Test renaming columns"
  (let* ((frame (epsilon.frame:frame
                 :old-name '(1 2 3)
                 :other '(4 5 6)))
         (renamed (epsilon.frame:rename-column frame :old-name :new-name)))
    (is-equal (epsilon.frame:column-names renamed) '("new-name" "other"))
    (is-not-null (epsilon.frame:get-column renamed :new-name))
    (is-thrown (error) (epsilon.frame:get-column renamed :old-name))
    ;; Original frame unchanged
    (is-not-null (epsilon.frame:get-column frame :old-name))))

(deftest test-frame-conversions ()
  "Test frame conversions to/from lists"
  (let* ((data '((1 "a" 10.0)
                 (2 "b" 20.0)
                 (3 "c" 30.0)))
         (frame1 (epsilon.frame:lists->frame '(:id :name :value) data))
         (back-to-lists (epsilon.frame:frame->lists frame1))
         (plists '((:x 1 :y 10)
                   (:x 2 :y 20)
                   (:x 3 :y 30)))
         (frame2 (epsilon.frame:plists->frame plists))
         (back-to-plists (epsilon.frame:frame->plists frame2)))
    ;; Test lists->frame and frame->lists
    (is-= (epsilon.frame:nrows frame1) 3)
    (is-= (epsilon.frame:ncols frame1) 3)
    (is-equal back-to-lists data)
    ;; Test plists->frame and frame->plists
    (is-= (epsilon.frame:nrows frame2) 3)
    (is-= (epsilon.frame:ncols frame2) 2)
    (is-equal back-to-plists plists)))

(deftest test-empty-frame ()
  "Test empty frame operations"
  (let ((empty (epsilon.frame:frame)))
    (is-= (epsilon.frame:nrows empty) 0)
    (is-= (epsilon.frame:ncols empty) 0)
    (is-equal (epsilon.frame:column-names empty) nil)
    (is-equal (epsilon.frame:shape empty) '(0 0))))