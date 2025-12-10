(defpackage :epsilon.web.validation-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:validation #:epsilon.web.validation)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)))

(in-package :epsilon.web.validation-tests)

(deftest test-type-validators ()
  "Test basic type validators"
  (is (validation:string-p "hello"))
  (is-not (validation:string-p 123))
  
  (is (validation:integer-p 42))
  (is (validation:integer-p "42"))
  (is-not (validation:integer-p "abc"))
  
  (is (validation:number-p 3.14))
  (is (validation:number-p "3.14"))
  (is (validation:number-p 42))
  
  (is (validation:boolean-p t))
  (is (validation:boolean-p nil))
  (is (validation:boolean-p "true"))
  (is (validation:boolean-p "false"))
  (is-not (validation:boolean-p "yes"))
  
  (is (validation:email-p "user@example.com"))
  (is (validation:email-p "test.user+tag@sub.example.co.uk"))
  (is-not (validation:email-p "invalid.email"))
  (is-not (validation:email-p "@example.com"))
  
  (is (validation:uuid-p "550e8400-e29b-41d4-a716-446655440000"))
  (is-not (validation:uuid-p "not-a-uuid")))

(deftest test-constraint-validators ()
  "Test constraint-based validators"
  (is (validation:min-length-p "hello" 3))
  (is-not (validation:min-length-p "hi" 3))
  
  (is (validation:max-length-p "hello" 10))
  (is-not (validation:max-length-p "hello world" 5))
  
  (is (validation:between-p 5 1 10))
  (is-not (validation:between-p 15 1 10))
  
  (is (validation:matches-p "hello" "^h.*o$"))
  (is-not (validation:matches-p "world" "^h.*o$"))
  
  (is (validation:one-of-p "red" '("red" "green" "blue")))
  (is-not (validation:one-of-p "yellow" '("red" "green" "blue"))))

(deftest test-validation-schema ()
  "Test validation schema definition and application"
  (let ((schema (validation:make-schema
                 :fields (list
                         (validation:field "name" 
                                         :type 'string
                                         :required t
                                         :min-length 2)
                         (validation:field "age"
                                         :type 'integer
                                         :required t
                                         :min 0
                                         :max 150)
                         (validation:field "email"
                                         :type 'email
                                         :required t)
                         (validation:field "role"
                                         :type 'string
                                         :one-of '("admin" "user" "guest"))))))
    
    ;; Valid data
    (let ((valid-data (map:make-map 
                      "name" "John Doe"
                      "age" 30
                      "email" "john@example.com"
                      "role" "user")))
      (multiple-value-bind (valid-p errors) 
          (validation:validate schema valid-data)
        (is valid-p)
        (is (null errors))))
    
    ;; Invalid data
    (let ((invalid-data (map:make-map
                        "name" "J"  ; too short
                        "age" 200   ; too old
                        "email" "not-an-email"
                        "role" "superuser"))) ; not allowed
      (multiple-value-bind (valid-p errors)
          (validation:validate schema invalid-data)
        (is-not valid-p)
        (is (> (length errors) 0))))))

(deftest test-nested-validation ()
  "Test nested object validation"
  (let ((schema (validation:make-schema
                 :fields (list
                         (validation:field "user"
                                         :type 'object
                                         :schema (validation:make-schema
                                                :fields (list
                                                        (validation:field "name" :type 'string :required t)
                                                        (validation:field "email" :type 'email :required t))))
                         (validation:field "settings"
                                         :type 'object
                                         :schema (validation:make-schema
                                                :fields (list
                                                        (validation:field "theme" :type 'string :one-of '("light" "dark"))
                                                        (validation:field "notifications" :type 'boolean))))))))
    
    (let ((data (map:make-map
                "user" (map:make-map 
                       "name" "Alice"
                       "email" "alice@example.com")
                "settings" (map:make-map
                          "theme" "dark"
                          "notifications" t))))
      (multiple-value-bind (valid-p errors)
          (validation:validate schema data)
        (is valid-p)))))

(deftest test-array-validation ()
  "Test array field validation"
  (let ((schema (validation:make-schema
                 :fields (list
                         (validation:field "tags"
                                         :type 'array
                                         :item-type 'string
                                         :min-items 1
                                         :max-items 5)
                         (validation:field "scores"
                                         :type 'array
                                         :item-type 'number
                                         :min-items 3)))))
    
    (let ((valid-data (map:make-map
                      "tags" '("lisp" "web" "framework")
                      "scores" '(85 90 78 92))))
      (multiple-value-bind (valid-p errors)
          (validation:validate schema valid-data)
        (is valid-p)))
    
    (let ((invalid-data (map:make-map
                        "tags" '("too" "many" "tags" "here" "now" "six")
                        "scores" '(85 90)))) ; too few
      (multiple-value-bind (valid-p errors)
          (validation:validate schema invalid-data)
        (is-not valid-p)))))

(deftest test-custom-validators ()
  "Test custom validation functions"
  (let ((schema (validation:make-schema
                 :fields (list
                         (validation:field "username"
                                         :type 'string
                                         :custom (lambda (value)
                                                 (and (> (length value) 3)
                                                      (not (find #\Space value))))
                                         :custom-message "Username must be >3 chars with no spaces")
                         (validation:field "password"
                                         :type 'string
                                         :custom (lambda (value)
                                                 (and (>= (length value) 8)
                                                      (some #'digit-char-p value)
                                                      (some #'upper-case-p value)))
                                         :custom-message "Password needs 8+ chars, digit, uppercase")))))
    
    (let ((valid-data (map:make-map
                      "username" "john_doe"
                      "password" "SecurePass123")))
      (multiple-value-bind (valid-p errors)
          (validation:validate schema valid-data)
        (is valid-p)))
    
    (let ((invalid-data (map:make-map
                        "username" "joe"
                        "password" "weak")))
      (multiple-value-bind (valid-p errors)
          (validation:validate schema invalid-data)
        (is-not valid-p)
        (is (>= (length errors) 2))))))

(deftest test-request-validation-middleware ()
  "Test request validation middleware"
  (let* ((schema (validation:make-schema
                  :fields (list
                          (validation:field "name" :type 'string :required t)
                          (validation:field "age" :type 'integer :min 0))))
         (middleware (web:validation-middleware schema))
         (handler (lambda (req) (web:respond "OK"))))
    
    ;; Valid request
    (let* ((valid-req (request:make-request "POST" "/api"
                                           :body "{\"name\": \"Alice\", \"age\": 25}"
                                           :headers (map:make-map "Content-Type" "application/json")))
           (wrapped (funcall middleware handler))
           (response (funcall wrapped valid-req)))
      (is-equal 200 (response:response-status response)))
    
    ;; Invalid request
    (let* ((invalid-req (request:make-request "POST" "/api"
                                             :body "{\"age\": -5}"
                                             :headers (map:make-map "Content-Type" "application/json")))
           (wrapped (funcall middleware handler))
           (response (funcall wrapped invalid-req)))
      (is-equal 400 (response:response-status response)))))