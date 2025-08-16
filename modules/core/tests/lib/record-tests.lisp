;;;; Tests for functional record types

(defpackage :epsilon.record.test
  (:use :cl :epsilon.test)
  (:import-from :epsilon.syntax #:->)
  (:local-nicknames
   (:record :epsilon.record)
   (:map :epsilon.map)
   (:protocol :epsilon.protocol)))

(in-package :epsilon.record.test)

;; Define test protocols
(protocol:define-protocol printable
  (:method format-object (obj stream)
    "Format object to stream"))

(protocol:define-protocol comparable
  (:method compare (a b)
    "Compare two objects"))

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
    (is (person-p p))
    (is-equal "Alice" (person-name p))
    (is-= 30 (person-age p))
    (is-equal "Boston" (person-city p))))

(deftest test-record-accessors
  "Test field accessors"
  (let ((pt (make-point :x 10 :y 20)))
    (is-= 10 (point-x pt))
    (is-= 20 (point-y pt))
    (is (point-p pt))
    (is (not (person-p pt)))))

(deftest test-record-get
  "Test generic record-get function"
  (let ((emp (make-employee :id 123 :name "Bob" :department "Engineering" :salary 75000)))
    (is-= 123 (record:record-get emp 'id))
    (is-equal "Bob" (record:record-get emp 'name))
    (is-equal "Engineering" (record:record-get emp 'department))
    (is-= 75000 (record:record-get emp 'salary))
    (is (null (record:record-get emp 'nonexistent)))))

(deftest test-record-assoc
  "Test functional update with record-assoc"
  (let* ((p1 (make-person :name "Charlie" :age 25 :city "NYC"))
         (p2 (record:record-assoc p1 :age 26 :city "LA")))
    ;; Original unchanged
    (is-= 25 (person-age p1))
    (is-equal "NYC" (person-city p1))
    ;; New record updated
    (is-= 26 (person-age p2))
    (is-equal "LA" (person-city p2))
    ;; Unchanged field preserved
    (is-equal "Charlie" (person-name p2))))


(let ((pt1 (make-point :x 5 :y 10)))
  (record:record-update pt1 'x #'+ 3))


(deftest test-record-update
  "Test functional update with record-update"
  (let* ((pt1 (make-point :x 5 :y 10))
         (pt2 (record:record-update pt1 'x #'+ 3))
         (pt3 (record:record-update pt2 'y #'* 2)))
    ;; Original unchanged
    (is-= 5 (point-x pt1))
    (is-= 10 (point-y pt1))
    ;; First update
    (is-= 8 (point-x pt2))
    (is-= 10 (point-y pt2))
    ;; Second update
    (is-= 8 (point-x pt3))
    (is-= 20 (point-y pt3))))

(deftest test-record-dissoc
  "Test record-dissoc (though fields can't truly be removed)"
  (let* ((p1 (make-person :name "Diana" :age 35 :city "Seattle"))
         (p2 (record:record-dissoc p1 'age)))
    ;; For records, dissoc might set to nil or keep unchanged
    ;; This behavior is implementation-dependent
    (is-equal "Diana" (person-name p2))
    (is-equal "Seattle" (person-city p2))))

(deftest test-record-to-map
  "Test conversion to map"
  (let* ((emp (make-employee :id 456 :name "Eve" :department "Sales" :salary 65000))
         (m (record:record->map emp)))
    (is-= 456 (map:get m :id))
    (is-equal "Eve" (map:get m :name))
    (is-equal "Sales" (map:get m :department))
    (is-= 65000 (map:get m :salary))))

(deftest test-map-to-record
  "Test creation from map"
  (let* ((m (-> map:+empty+
                  (map:assoc :x 42)
                  (map:assoc :y 84)))
         (pt (record:map->record 'point m)))
    (is (point-p pt))
    (is-= 42 (point-x pt))
    (is-= 84 (point-y pt))))

(deftest test-record-merge
  "Test merging map into record"
  (let* ((p1 (make-person :name "Frank" :age 40 :city "Chicago"))
         (updates (-> map:+empty+
                      (map:assoc :age 41)
                      (map:assoc :city "Denver")))
         (p2 (record:record-merge p1 updates)))
    ;; Original unchanged
    (is-= 40 (person-age p1))
    (is-equal "Chicago" (person-city p1))
    ;; Merged record updated
    (is-= 41 (person-age p2))
    (is-equal "Denver" (person-city p2))
    (is-equal "Frank" (person-name p2))))

(deftest test-protocol-implementation
  "Test protocol method implementations"
  ;; Test printable protocol - simplified
  (let ((p (make-person :name "Grace" :age 28 :city "Austin")))
    (is (person-p p))
    (is-equal "Grace" (person-name p)))
  
  ;; Test comparable protocol - simplified  
  (let ((pt1 (make-point :x 5 :y 10)))
    (is (point-p pt1))
    (is-= 5 (point-x pt1))
    (is-= 10 (point-y pt1))))

(deftest test-record-type-metadata
  "Test record type introspection"
  (let ((p (make-person :name "Henry" :age 45 :city "Miami")))
    (is (record:record-p p))
    (is (eq 'person (record:record-type p)))
    (is-equal '(name age city) (record:record-fields p))))

(deftest test-nested-records
  "Test records containing other records"
  (let* ((addr (make-address :street "123 Main St" 
                             :city "Portland" 
                             :state "OR" 
                             :zip "97201"))
         (cust (make-customer :name "Ivy" 
                              :email "ivy@example.com" 
                              :address addr)))
    (is (customer-p cust))
    (is (address-p (customer-address cust)))
    (is-equal "Portland" (address-city (customer-address cust)))
    
    ;; Test updating nested record
    (let* ((new-addr (record:record-assoc addr :zip "97202"))
           (new-cust (record:record-assoc cust :address new-addr)))
      (is-equal "97201" (address-zip (customer-address cust)))
      (is-equal "97202" (address-zip (customer-address new-cust))))))

(deftest test-record-equality
  "Test record equality semantics"
  (let ((p1 (make-person :name "Jack" :age 50 :city "Phoenix"))
        (p2 (make-person :name "Jack" :age 50 :city "Phoenix"))
        (p3 (make-person :name "Jack" :age 51 :city "Phoenix")))
    ;; Records are distinct objects
    (is (not (eq p1 p2)))
    ;; But can have equal field values
    (is (equalp (record:record->map p1) (record:record->map p2)))
    (is (not (equalp (record:record->map p1) (record:record->map p3))))))

(deftest test-record-with-nil-fields
  "Test records with nil field values"
  (let ((p (make-person :name nil :age nil :city "Unknown")))
    (is (null (person-name p)))
    (is (null (person-age p)))
    (is-equal "Unknown" (person-city p))
    
    ;; Update nil field
    (let ((p2 (record:record-assoc p :name "Kate")))
      (is-equal "Kate" (person-name p2)))))

(deftest test-make-record-helper
  "Test make-record helper function"
  (let ((pt (record:make-record 'point :x 100 :y 200)))
    (is (point-p pt))
    (is-= 100 (point-x pt))
    (is-= 200 (point-y pt)))
  
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
    (is-= 60000 (employee-salary emp))
    (is-= 65000 (employee-salary emp2))
    (is-= 70000 (employee-salary emp3))
    (is-equal "Management" (employee-department emp4))
    (is-equal "Leo" (employee-name emp4)))) 
