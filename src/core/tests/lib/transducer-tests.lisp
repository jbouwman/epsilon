(defpackage epsilon.lib.transducer.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (xf epsilon.transducer)
   (seq epsilon.sequence)
   (map epsilon.map)))

(in-package :epsilon.lib.transducer.tests)

;;;; Basic Transducer Tests

(deftest map-transducer
  "Test map transducer"
  (is-equal '(6 5 4 3 2)
            (xf:into '() (xf:map #'1+) '(1 2 3 4 5)))
  
  (is-equalp #(2 4 6 8 10)
             (xf:into #() (xf:map (lambda (x) (* x 2))) '(1 2 3 4 5))))

(deftest filter-transducer
  "Test filter transducer"
  (is-equal '(4 2)
            (xf:into '() (xf:filter #'evenp) '(1 2 3 4 5)))
  
  (is-equalp #(1 3 5)
             (xf:into #() (xf:filter #'oddp) '(1 2 3 4 5))))

(deftest remove-transducer
  "Test remove transducer"
  (is-equal '(5 3 1)
            (xf:into '() (xf:remove #'evenp) '(1 2 3 4 5)))
  
  (is-equalp #(2 4)
             (xf:into #() (xf:remove #'oddp) '(1 2 3 4 5))))

(deftest take-transducer
  "Test take transducer"
  (is-equal '(3 2 1)
            (xf:into '() (xf:take 3) '(1 2 3 4 5)))
  
  (is-equal '()
            (xf:into '() (xf:take 0) '(1 2 3 4 5)))
  
  (is-equal '(5 4 3 2 1)
            (xf:into '() (xf:take 10) '(1 2 3 4 5))))

(deftest take-while-transducer
  "Test take-while transducer"
  (is-equal '(3 2 1)
            (xf:into '() (xf:take-while (lambda (x) (< x 4))) '(1 2 3 4 5)))
  
  (is-equal '()
            (xf:into '() (xf:take-while (lambda (x) (< x 0))) '(1 2 3 4 5))))

(deftest drop-transducer
  "Test drop transducer"
  (is-equal '(5 4)
            (xf:into '() (xf:drop 3) '(1 2 3 4 5)))
  
  (is-equal '(5 4 3 2 1)
            (xf:into '() (xf:drop 0) '(1 2 3 4 5)))
  
  (is-equal '()
            (xf:into '() (xf:drop 10) '(1 2 3 4 5))))

(deftest drop-while-transducer
  "Test drop-while transducer"
  (is-equal '(5 4)
            (xf:into '() (xf:drop-while (lambda (x) (< x 4))) '(1 2 3 4 5)))
  
  (is-equal '(5 4 3 2 1)
            (xf:into '() (xf:drop-while (lambda (x) (< x 0))) '(1 2 3 4 5))))

(deftest take-nth-transducer
  "Test take-nth transducer"
  (is-equal '(10 8 6 4 2)
            (xf:into '() (xf:take-nth 2) '(1 2 3 4 5 6 7 8 9 10)))
  
  (is-equal '(9 6 3)
            (xf:into '() (xf:take-nth 3) '(1 2 3 4 5 6 7 8 9 10))))

(deftest dedupe-transducer
  "Test dedupe transducer"
  (is-equal '(1 2 3 2 1)
            (xf:into '() (xf:dedupe) '(1 1 2 3 3 3 2 1 1)))
  
  (is-equal '(1)
            (xf:into '() (xf:dedupe) '(1 1 1 1 1))))

(deftest partition-by-transducer
  "Test partition-by transducer"
  (is-equal '((1 1) (3) (2 2 2) (1 1))
            (xf:into '() (xf:partition-by #'identity) '(1 1 2 2 2 3 1 1)))
  
  (is-equal '((2 4 6) (1 3 5))
            (xf:into '() (xf:partition-by #'oddp) '(1 3 5 2 4 6))))

(deftest partition-all-transducer
  "Test partition-all transducer"
  (is-equal '((7 8) (4 5 6) (1 2 3))
            (xf:into '() (xf:partition-all 3) '(1 2 3 4 5 6 7 8)))
  
  (is-equal '((7 8) (5 6) (3 4) (1 2))
            (xf:into '() (xf:partition-all 2 3) '(1 2 3 4 5 6 7 8))))

(deftest keep-transducer
  "Test keep transducer"
  (is-equal '(3 1)
            (xf:into '() (xf:keep (lambda (x) (when (oddp x) x))) '(1 2 3 4)))
  
  (is-equal '(:c :a)
            (xf:into '() (xf:keep (lambda (x) (second x))) 
                     '((1) (2 :a) (3) (4 :c)))))

(deftest keep-indexed-transducer
  "Test keep-indexed transducer"
  (skip)
  (is-equal '((0 :a) (2 :c))
            (xf:into '() 
                     (xf:keep-indexed (lambda (i x) 
                                       (when (evenp i) (list i x))))
                     '(:a :b :c :d))))

(deftest replace-transducer
  "Test replace transducer"
  (let ((replacements (map:make-map 1 :one 2 :two 3 :three)))
    (is-equal '(5 4 :three :two :one)
              (xf:into '() (xf:replace replacements) '(1 2 3 4 5)))))

;;;; Transducer Composition Tests

(deftest comp-transducers
  "Test transducer composition"
  ;; Filter then map
  (skip)
  (is-equal '(4 8)
            (xf:into '() 
                     (xf:comp (xf:filter #'evenp)
                              (xf:map (lambda (x) (* x 2))))
                     '(1 2 3 4 5)))
  
  ;; Map then filter
  (is-equal '(4 6 8 10)
            (xf:into '() 
                     (xf:comp (xf:map (lambda (x) (* x 2)))
                              (xf:filter #'evenp))
                     '(1 2 3 4 5)))
  
  ;; Complex composition
  (is-equal '(6 12)
            (xf:into '()
                     (xf:comp (xf:filter #'evenp)
                              (xf:map (lambda (x) (* x 3)))
                              (xf:take 2))
                     '(1 2 3 4 5 6))))

(deftest mapcat-transducer
  "Test mapcat transducer"
  (is-equal '(3 3 3 2 2 1)
            (xf:into '() 
                     (xf:mapcat (lambda (x) (make-list x :initial-element x)))
                     '(1 2 3)))
  
  (is-equal '(3 :c 2 :b 1 :a)
            (xf:into '()
                     (xf:mapcat (lambda (x) x))
                     '((:a 1) (:b 2) (:c 3)))))

;;;; Early Termination Tests

(deftest reduced-early-termination
  "Test early termination with reduced"
  ;; Take uses reduced internally
  (is-equal '(3 2 1)
            (xf:into '() (xf:take 3) '(1 2 3 4 5 6 7 8 9 10)))
  
  ;; Custom early termination
  (let ((sum 0))
    (is-equal 3
              (xf:transduce (xf:map #'identity)
                           (lambda (acc x)
                             (let ((new-sum (+ acc x)))
                               (if (> new-sum 5)
                                   (xf:reduced acc)
                                   new-sum)))
                           0
                           '(1 2 3 4 5)))))

(deftest halt-when-transducer
  "Test halt-when transducer"
  (skip)
  (is-equal '(1 2 3)
            (xf:into '() 
                     (xf:halt-when (lambda (x) (= x 4)))
                     '(1 2 3 4 5 6)))
  
  ;; With return function
  (is-equal '(1 2 3 :halted)
            (xf:into '()
                     (xf:halt-when (lambda (x) (= x 4))
                                  (lambda (result input)
                                    (declare (ignore input))
                                    (append result '(:halted))))
                     '(1 2 3 4 5 6))))

;;;; Collection Support Tests

(deftest into-vector
  "Test transducing into vectors"
  (let ((v #(1 2 3)))
    (is-equalp #(1 2 3 4 8)
               (xf:into v 
                        (xf:comp (xf:filter #'evenp)
                                (xf:map (lambda (x) (* x 2))))
                        '(1 2 3 4 5)))))

(deftest into-map
  "Test transducing into maps"
  (let ((m (map:make-map :a 1)))
    (is-equal 3
              (map:size (xf:into m
                                (xf:map (lambda (x) (list (first x) (* 2 (second x)))))
                                '((:b 2) (:c 3)))))))

;;;; Stateful Transducer Tests

(deftest stateful-transducers
  "Test stateful transducers maintain independent state"
  ;; Each transducer instance has its own state
  (let ((xform (xf:comp (xf:drop 2) (xf:take 3))))
    (is-equal '(5 4 3)
              (xf:into '() xform '(1 2 3 4 5 6 7)))
    ;; Using same xform again works correctly
    (is-equal '(5 4 3)
              (xf:into '() xform '(1 2 3 4 5 6 7)))))

;;;; Performance Tests

(deftest transducer-performance
  "Test that transducers avoid intermediate collections"
  ;; This would create 2 intermediate collections with regular map/filter
  (let ((data (loop for i from 1 to 1000 collect i)))
    (is-equal 250500  ; sum of even numbers 2 to 1000
              (xf:transduce (xf:comp (xf:filter #'evenp)
                                    (xf:map #'identity))
                           #'+
                           0
                           data))))

;;;; Eduction Tests

(deftest eduction-lazy
  "Test eduction provides lazy evaluation"
  (let* ((calls 0)
         (ed (xf:eduction (xf:map (lambda (x) 
                                   (incf calls)
                                   (* x 2)))
                         '(1 2 3 4 5))))
    ;; No computation yet
    (is-equal 0 calls)
    
    ;; Force first 3 elements
    (is-equal '(2 4 6)
              (seq:take 3 (xf:eduction->seq ed)))
    
    ;; Should have processed at least 3 elements
    (is (>= calls 3))))

(deftest sequence-transducer
  "Test sequence with transducers"
  (let ((s (xf:sequence (xf:comp (xf:filter #'evenp)
                                (xf:map (lambda (x) (* x 2))))
                       '(1 2 3 4 5 6))))
    (is-equal '(4 8 12)
              (seq:realize s))))

;;;; Cat Transducer Tests

(deftest cat-transducer
  "Test cat transducer"
  (is-equal '(6 5 4 3 2 1)
            (xf:into '() (xf:cat) '((1 2) (3 4) (5 6))))
  
  ;; Cat with other transducers
  (is-equal '(6 4 2)
            (xf:into '()
                     (xf:comp (xf:cat)
                              (xf:filter #'evenp))
                     '((1 2) (3 4) (5 6)))))

;;;; Edge Cases

(deftest transducer-edge-cases
  "Test edge cases for transducers"
  ;; Empty collection
  (is-equal '()
            (xf:into '() (xf:map #'1+) '()))
  
  ;; Single element
  (is-equal '(2)
            (xf:into '() (xf:map #'1+) '(1)))
  
  ;; Take more than available
  (is-equal '(3 2 1)
            (xf:into '() (xf:take 10) '(1 2 3)))
  
  ;; Drop more than available
  (is-equal '()
            (xf:into '() (xf:drop 10) '(1 2 3))))
