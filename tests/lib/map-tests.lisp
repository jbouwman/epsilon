(defpackage #:epsilon.lib.map.tests
  (:use :cl
        :epsilon.tool.test)
  (:local-nicknames (#:map #:epsilon.lib.map)
                    (#:bind #:epsilon.lib.binding)))

(in-package #:epsilon.lib.map.tests)

(deftest empty-map-has-zero-count ()
  (let ((m map:+empty+))
    (is (= (map::hamt-count m) 0)
        "The empty map has count = 0")))

(deftest assoc-increases-count ()
  (let* ((m1 map:+empty+)
         (m2 (map:map-assoc m1 :a 1)))
    (is (= (map::hamt-count m2) 1))))

(deftest get-returns-default-for-missing-key ()
  (let ((m map:+empty+))
    (is (eql (map:map-get m :missing :default) :default))
    t))

(deftest get-returns-value-for-existing-key ()
  (let* ((m (map:map-assoc map:+empty+ :key "value")))
    (is (string= (map:map-get m :key) "value"))
    t))

(deftest assoc-overwrites-existing-value ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-assoc m1 :a 2)))
    (is (= (map:map-get m2 :a) 2))
    (is (= (map::hamt-count m2) 1))
    t))

(deftest dissoc-decreases-count ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-dissoc m1 :a)))
    (is (= (map::hamt-count m2) 0))
    t))

(deftest dissoc-preserves-other-values ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-assoc m1 :b 2))
         (m3 (map:map-dissoc m2 :a)))
    (is (= (map:map-get m3 :b) 2))
    (is (= (map::hamt-count m3) 1))
    t))

(deftest map-contains?-works-correctly ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1)))
    (is (map:map-contains? m1 :a))
    (is (not (map:map-contains? m1 :b)))
    t))

(deftest original-map-remains-unchanged ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-assoc m1 :b 2)))
    (is (= (map::hamt-count m1) 1))
    (is (= (map:map-get m1 :a) 1))
    (is (not (map:map-contains? m1 :b)))
    m2))

(deftest null-keys ()
  (let ((m (map:map-assoc map:+empty+ nil 1)))
    (is (= 1 (map:map-get m nil)))
    (is (map:map-contains? m nil))))

(deftest map-invert ()
  (let* ((m (bind:-> map:+empty+
                     (map:map-assoc :a 1)
                     (map:map-assoc :b 2)
                     (map:map-assoc :c 3)))
         (m2 (map::map-invert m)))
    (is (= 3 (map::hamt-count m2)))
    (is (eq :a (map:map-get m2 1)))
    (is (eq :b (map:map-get m2 2)))
    (is (eq :c (map:map-get m2 3)))))

(defun range (start end)
  (loop for i from start to end collect i))

(deftest large-maps ()
  (let* ((pairs (loop for i below 1000
                      collect (cons i i)))
         (m (map::map-from-pairs pairs)))
    (is (= 1000 (map::hamt-count m)))
    (is (every (lambda (i) (= i (map:map-get m i))) 
               (range 0 999)))))

(deftest map-seq ()
  (let* ((pairs '((:a . 1) (:b . 2) (:c . 3)))
         (m (map::map-from-pairs pairs)))
    (is (= (length pairs) (length (map::map-seq m))))
    (is (equal (sort (copy-list pairs) #'string< :key #'car)
               (sort (map::map-seq m) #'string< :key #'car)))))

(deftest single-assoc ()
  (let* ((m map:+empty+)
         (m2 (map:map-assoc m :a 1)))
    (is (= 1 (map::hamt-count m2)))
    (is (= 1 (map:map-get m2 :a)))
    (is (null (map:map-get m :a)))))

(deftest multiple-assocs ()
  (let ((m (bind:-> map:+empty+
                    (map:map-assoc :a 1)
                    (map:map-assoc :b 2)
                    (map:map-assoc :c 3))))
    (is (= 3 (map::hamt-count m)))
    (is (= 1 (map:map-get m :a)))
    (is (= 2 (map:map-get m :b)))
    (is (= 3 (map:map-get m :c)))))

(deftest update-existing ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-assoc m1 :a 2)))
    (is (= 2 (map:map-get m2 :a)))
    (is (= 1 (map:map-get m1 :a)))))

(deftest dissoc-existing ()
  (let* ((m1 (bind:-> map:+empty+
                      (map:map-assoc :a 1)
                      (map:map-assoc :b 2)))
         (m2 (map:map-dissoc m1 :a)))
    (is (= 1 (map::hamt-count m2)))
    (is (null (map:map-get m2 :a)))
    (is (= 2 (map:map-get m2 :b)))))

(deftest dissoc-missing ()
  (let* ((m1 (map:map-assoc map:+empty+ :a 1))
         (m2 (map:map-dissoc m1 :b)))
    (is (eq m1 m2))))

(deftest contains ()
  (let ((m (map:map-assoc map:+empty+ :a 1)))
    (is (map:map-contains? m :a))
    (is (not (map:map-contains? m :b)))))

(deftest map-merge ()
  (let* ((m1 (bind:-> map:+empty+
                      (map:map-assoc :a 1)
                      (map:map-assoc :b 2)))
         (m2 (bind:-> map:+empty+
                      (map:map-assoc :b 3)
                      (map:map-assoc :c 4)))
         (merged (map::map-merge m1 m2)))
    (is (= 3 (map::hamt-count merged)))
    (is (= 1 (map:map-get merged :a)))
    (is (= 3 (map:map-get merged :b)))
    (is (= 4 (map:map-get merged :c)))))

(deftest depth-handling ()
  (let* ((count 1000)
         (m (loop with map = map:+empty+
                  for i below count
                  do (setf map (map:map-assoc map i i))
                  finally (return map))))
    (is (= count (map::hamt-count m)))
    (is (every (lambda (i) (= i (map:map-get m i)))
               (range 0 (1- count))))))
