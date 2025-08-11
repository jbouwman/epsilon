(defpackage :epsilon.web.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package :epsilon.web.tests)

;; Test fixtures
(fixture test-server ()
  (:setup
   (let ((server (epsilon.http.server:start-server 
                  (lambda (req) (web:json (map:make-map "echo" "test")))
                  :port 0))) ; Use random port
     server))
  (:teardown
   (epsilon.http.server:stop-server server)))

(fixture sample-requests ()
  (:setup
   (list :get-req (request:make-request "GET" "/test")
         :post-req (request:make-request "POST" "/api/data"
                                        :headers (map:make-map "Content-Type" "application/json")
                                        :body "{\"name\": \"test\"}")
         :form-req (request:make-request "POST" "/form"
                                        :headers (map:make-map "Content-Type" "application/x-www-form-urlencoded")
                                        :body "name=test&value=123"))))

(deftest test-json-request-p ()
  "Test JSON request detection"
  (let ((json-req (request:make-request "POST" "/test"
                                       :headers (map:make-map "Content-Type" "application/json")))
        (html-req (request:make-request "POST" "/test"
                                       :headers (map:make-map "Content-Type" "text/html"))))
    (is (web:json-request-p json-req))
    (is-not (web:json-request-p html-req))))

(deftest test-parse-query-string ()
  "Test query string parsing"
  (let ((params1 (web:parse-query-string "foo=bar&baz=qux"))
        (params2 (web:parse-query-string "name=John%20Doe&age=30"))
        (params3 (web:parse-query-string ""))
        (params4 (web:parse-query-string nil)))
    (is-equal "bar" (map:get params1 "foo"))
    (is-equal "qux" (map:get params1 "baz"))
    (is-equal "John%20Doe" (map:get params2 "name"))
    (is-equal "30" (map:get params2 "age"))
    (is (= (map:count params3) 0))
    (is (= (map:count params4) 0))))

(deftest test-response-builders ()
  "Test response builder functions"
  ;; Test JSON response
  (let ((resp (web:json (map:make-map "status" "ok") :status 201)))
    (is-equal 201 (response:response-status resp))
    (is-equal "application/json" (map:get (response:response-headers resp) "Content-Type")))
  
  ;; Test HTML response
  (let ((resp (web:html "<h1>Hello</h1>")))
    (is-equal 200 (response:response-status resp))
    (is-equal "text/html; charset=utf-8" (map:get (response:response-headers resp) "Content-Type"))
    (is-equal "<h1>Hello</h1>" (response:response-body resp)))
  
  ;; Test redirect
  (let ((resp (web:redirect "/login")))
    (is-equal 302 (response:response-status resp))
    (is-equal "/login" (map:get (response:response-headers resp) "Location")))
  
  ;; Test not-found
  (let ((resp (web:not-found)))
    (is-equal 404 (response:response-status resp))
    (is (search "404" (response:response-body resp)))))

(deftest test-route-definition ()
  "Test route definition macros"
  ;; Define test routes
  (web:defroutes test-routes
    (:get "/" (lambda (req) (web:text "home")))
    (:post "/api" (lambda (req) (web:json (map:make-map "ok" t)))))
  
  (is-equal 2 (length test-routes))
  (is-equal "GET" (web:route-method (first test-routes)))
  (is-equal "/" (web:route-path (first test-routes)))
  (is-equal "POST" (web:route-method (second test-routes)))
  (is-equal "/api" (web:route-path (second test-routes))))

(deftest test-defhandler ()
  "Test defhandler macro"
  ;; Define a test handler
  (web:defhandler test-handler (req)
    (web:json (map:make-map "path" (request:request-path req))))
  
  ;; Test normal operation
  (let* ((req (request:make-request "GET" "/test"))
         (resp (test-handler req)))
    (is-equal 200 (response:response-status resp))
    (is (search "/test" (response:response-body resp)))))

(deftest test-request-param ()
  "Test parameter extraction"
  ;; Test query params
  (let ((req (request:make-request "GET" "/test"
                                  :params (map:make-map "name" "Alice"))))
    (is-equal "Alice" (web:request-param req "name"))
    (is-equal "default" (web:request-param req "missing" "default")))
  
  ;; Test JSON body params
  (let ((req (request:make-request "POST" "/test"
                                  :headers (map:make-map "Content-Type" "application/json")
                                  :body "{\"name\": \"Bob\", \"age\": 30}")))
    (is-equal "Bob" (web:request-param req "name"))
    ;; Note: This returns the JSON value, which is a number
    (is-equal 30 (web:request-param req "age"))))

(deftest test-with-json-body ()
  "Test JSON body parsing macro"
  ;; Valid JSON
  (let ((req (request:make-request "POST" "/test"
                                  :headers (map:make-map "Content-Type" "application/json")
                                  :body "{\"name\": \"Charlie\"}")))
    (let ((result (web:with-json-body (data req)
                    (web:json (map:make-map "greeting" 
                                          (format nil "Hello, ~A" (map:get data "name")))))))
      (is-equal 200 (response:response-status result))
      (is (search "Charlie" (response:response-body result)))))
  
  ;; Invalid JSON
  (let ((req (request:make-request "POST" "/test"
                                  :headers (map:make-map "Content-Type" "application/json")
                                  :body "invalid json")))
    (let ((result (web:with-json-body (data req)
                    (web:json data))))
      (is-equal 400 (response:response-status result))
      (is (search "Invalid JSON" (response:response-body result))))))

(deftest test-with-fixtures ()
  "Test using fixtures for setup/teardown"
  (with-fixture (requests sample-requests)
    ;; Test different request types
    (is (web:json-request-p (getf requests :post-req)))
    (is-not (web:json-request-p (getf requests :get-req)))
    (is-not (web:json-request-p (getf requests :form-req)))))

;; DISABLED: test-with-temp-file - requires with-temp-file macro
;;(deftest test-with-temp-file ()
;;  "Test using temporary file fixture"
;;  (with-temp-file (file :content "test data\nline 2" :suffix ".txt")
;;    (is (epsilon.sys.fs:exists-p file))
;;    (let ((content (with-open-file (s file) 
;;                     (read-line s))))
;;      (is-equal "test data" content))))

;; DISABLED: test-middleware-with-fixture - requires with-test-data macro and logging-middleware
;;(deftest test-middleware-with-fixture ()
;;  "Test middleware functionality with fixtures"
;;  (with-test-data (handler (lambda (req) 
;;                            (web:json (map:make-map "path" (request:request-path req)))))
;;    ;; Wrap with logging middleware
;;    (let ((wrapped (web:wrap-middleware handler web:logging-middleware)))
;;      ;; Test that middleware preserves functionality
;;      (let* ((req (request:make-request "GET" "/test"))
;;             (resp (funcall wrapped req)))
;;        (is-equal 200 (response:response-status resp))
;;        (is (search "/test" (response:response-body resp)))))))