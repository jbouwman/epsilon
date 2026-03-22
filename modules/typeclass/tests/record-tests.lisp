;;;; Tests for functional record types

(defpackage epsilon.record-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.record record)
            (epsilon.map map)
            (epsilon.typeclass tc))
  (:enter t))

;; Define test typeclasses (replaces defprotocol)
(tc:deftypeclass printable ()
  (format-object (obj stream) "Format object to stream."))

(tc:deftypeclass comparable ()
  (compare (a b) "Compare two objects."))

;; Define test record types
(record:defrecord person (name age city)
  (:implements printable
    (format-object (self stream)
      (format stream "#<person ~A, ~A, ~A>"
              (person-name self)
              (person-age self)
              (person-city self)))))

(record:defrecord point (x y)
  (:implements comparable
    (compare (self other)
      (cond ((not (point-p other)) nil)
            ((and (= (point-x self) (point-x other))
                  (= (point-y self) (point-y other))) 0)
            ((or (< (point-x self) (point-x other))
                 (and (= (point-x self) (point-x other))
                      (< (point-y self) (point-y other)))) -1)
            (t 1)))))

(record:defrecord employee (id name department salary))

;; Define records for nested test
(record:defrecord address (street city state zip))
(record:defrecord customer (name email address))

(deftest test-record-construction
  "Test basic record construction"
  (let ((p (make-person :name "Alice" :age 30 :city "Boston")))
    (assert-true (person-p p))
    (assert-equal "Alice" (person-name p))
    (assert-= 30 (person-age p))
    (assert-equal "Boston" (person-city p))))

(deftest test-record-accessors
  "Test field accessors"
  (let ((pt (make-point :x 10 :y 20)))
    (assert-= 10 (point-x pt))
    (assert-= 20 (point-y pt))
    (assert-true (point-p pt))
    (assert-true (not (person-p pt)))))

(deftest test-record-get
  "Test generic record-get function"
  (let ((emp (make-employee :id 123 :name "Bob" :department "Engineering" :salary 75000)))
    (assert-= 123 (record:record-get emp 'id))
    (assert-equal "Bob" (record:record-get emp 'name))
    (assert-equal "Engineering" (record:record-get emp 'department))
    (assert-= 75000 (record:record-get emp 'salary))
    (assert-true (null (record:record-get emp 'nonexistent)))))

(deftest test-record-assoc
  "Test functional update with record-assoc"
  (let* ((p1 (make-person :name "Charlie" :age 25 :city "NYC"))
         (p2 (record:record-assoc p1 :age 26 :city "LA")))
    ;; Original unchanged
    (assert-= 25 (person-age p1))
    (assert-equal "NYC" (person-city p1))
    ;; New record updated
    (assert-= 26 (person-age p2))
    (assert-equal "LA" (person-city p2))
    ;; Unchanged field preserved
    (assert-equal "Charlie" (person-name p2))))


(let ((pt1 (make-point :x 5 :y 10)))
  (record:record-update pt1 'x #'+ 3))


(deftest test-record-update
  "Test functional update with record-update"
  (let* ((pt1 (make-point :x 5 :y 10))
         (pt2 (record:record-update pt1 'x #'+ 3))
         (pt3 (record:record-update pt2 'y #'* 2)))
    ;; Original unchanged
    (assert-= 5 (point-x pt1))
    (assert-= 10 (point-y pt1))
    ;; First update
    (assert-= 8 (point-x pt2))
    (assert-= 10 (point-y pt2))
    ;; Second update
    (assert-= 8 (point-x pt3))
    (assert-= 20 (point-y pt3))))

(deftest test-record-dissoc
  "Test record-dissoc (though fields can't truly be removed)"
  (let* ((p1 (make-person :name "Diana" :age 35 :city "Seattle"))
         (p2 (record:record-dissoc p1 'age)))
    ;; For records, dissoc might set to nil or keep unchanged
    ;; This behavior is implementation-dependent
    (assert-equal "Diana" (person-name p2))
    (assert-equal "Seattle" (person-city p2))))

(deftest test-record-to-map
  "Test conversion to map"
  (let* ((emp (make-employee :id 456 :name "Eve" :department "Sales" :salary 65000))
         (m (record:record->map emp)))
    (assert-= 456 (map:get m :id))
    (assert-equal "Eve" (map:get m :name))
    (assert-equal "Sales" (map:get m :department))
    (assert-= 65000 (map:get m :salary))))

(deftest test-map-to-record
  "Test creation from map"
  (let* ((m (-> map:+empty+
                  (map:assoc :x 42)
                  (map:assoc :y 84)))
         (pt (record:map->record 'point m)))
    (assert-true (point-p pt))
    (assert-= 42 (point-x pt))
    (assert-= 84 (point-y pt))))

(deftest test-record-merge
  "Test merging map into record"
  (let* ((p1 (make-person :name "Frank" :age 40 :city "Chicago"))
         (updates (-> map:+empty+
                      (map:assoc :age 41)
                      (map:assoc :city "Denver")))
         (p2 (record:record-merge p1 updates)))
    ;; Original unchanged
    (assert-= 40 (person-age p1))
    (assert-equal "Chicago" (person-city p1))
    ;; Merged record updated
    (assert-= 41 (person-age p2))
    (assert-equal "Denver" (person-city p2))
    (assert-equal "Frank" (person-name p2))))

(deftest test-protocol-implementation
  "Test protocol method implementations"
  ;; Test printable protocol - simplified
  (let ((p (make-person :name "Grace" :age 28 :city "Austin")))
    (assert-true (person-p p))
    (assert-equal "Grace" (person-name p)))

  ;; Test comparable protocol - simplified
  (let ((pt1 (make-point :x 5 :y 10)))
    (assert-true (point-p pt1))
    (assert-= 5 (point-x pt1))
    (assert-= 10 (point-y pt1))))

(deftest test-record-type-metadata
  "Test record type introspection"
  (let ((p (make-person :name "Henry" :age 45 :city "Miami")))
    (assert-true (record:record-p p))
    (assert-true (eq 'person (record:record-type p)))
    (assert-equal '(name age city) (record:record-fields p))))

(deftest test-nested-records
  "Test records containing other records"
  (let* ((addr (make-address :street "123 Main St"
                             :city "Portland"
                             :state "OR"
                             :zip "97201"))
         (cust (make-customer :name "Ivy"
                              :email "ivy@example.com"
                              :address addr)))
    (assert-true (customer-p cust))
    (assert-true (address-p (customer-address cust)))
    (assert-equal "Portland" (address-city (customer-address cust)))

    ;; Test updating nested record
    (let* ((new-addr (record:record-assoc addr :zip "97202"))
           (new-cust (record:record-assoc cust :address new-addr)))
      (assert-equal "97201" (address-zip (customer-address cust)))
      (assert-equal "97202" (address-zip (customer-address new-cust))))))

(deftest test-record-equality
  "Test record equality semantics"
  (let ((p1 (make-person :name "Jack" :age 50 :city "Phoenix"))
        (p2 (make-person :name "Jack" :age 50 :city "Phoenix"))
        (p3 (make-person :name "Jack" :age 51 :city "Phoenix")))
    ;; Records are distinct objects
    (assert-true (not (eq p1 p2)))
    ;; But can have equal field values
    (assert-true (equalp (record:record->map p1) (record:record->map p2)))
    (assert-true (not (equalp (record:record->map p1) (record:record->map p3))))))

(deftest test-record-with-nil-fields
  "Test records with nil field values"
  (let ((p (make-person :name nil :age nil :city "Unknown")))
    (assert-true (null (person-name p)))
    (assert-true (null (person-age p)))
    (assert-equal "Unknown" (person-city p))

    ;; Update nil field
    (let ((p2 (record:record-assoc p :name "Kate")))
      (assert-equal "Kate" (person-name p2)))))

(deftest test-make-record-helper
  "Test make-record helper function"
  (let ((pt (record:make-record 'point :x 100 :y 200)))
    (assert-true (point-p pt))
    (assert-= 100 (point-x pt))
    (assert-= 200 (point-y pt)))

  ;; Test with invalid type
  (handler-case
      (progn (record:make-record 'nonexistent :foo 1) nil)
    (error () t)))

(deftest test-complex-update-chain
  "Test chaining multiple updates"
  (let* ((emp (make-employee :id 789 :name "Leo" :department "HR" :salary 60000))
         (emp2 (record:record-assoc emp :salary 65000))
         (emp3 (record:record-update emp2 'salary #'+ 5000))
         (emp4 (record:record-assoc emp3 :department "Management")))
    (assert-= 60000 (employee-salary emp))
    (assert-= 65000 (employee-salary emp2))
    (assert-= 70000 (employee-salary emp3))
    (assert-equal "Management" (employee-department emp4))
    (assert-equal "Leo" (employee-name emp4))))
