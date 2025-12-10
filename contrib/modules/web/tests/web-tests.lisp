(defpackage :epsilon.web.tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string)))

(in-package :epsilon.web.tests)

;; Test request value extraction
(deftest test-request-value ()
  "Test unified request value extraction"
  (let ((req (request:make-request "GET" "/test"
                                  :params (map:make-map "name" "Alice")
                                  :headers (map:make-map "X-Token" "secret"))))
    (is-equal "Alice" (web:request-value req "name"))
    (is-equal "secret" (web:request-value req "X-Token" :from :header))
    (is-equal "default" (web:request-value req "missing" :default "default"))))

(deftest test-with-request-values ()
  "Test request value binding macro"
  (let ((req (request:make-request "POST" "/api"
                                  :params (map:make-map "id" "123" "name" "Bob"))))
    (web:with-request-values ((id "id")
                             (name "name")
                             (missing "missing" :default "none"))
        req
      (is-equal "123" id)
      (is-equal "Bob" name)
      (is-equal "none" missing))))

;; Test response builders
(deftest test-respond ()
  "Test unified respond function"
  (let ((resp1 (web:respond "Hello"))
        (resp2 (web:respond (map:make-map "status" "ok")))
        (resp3 (web:respond "Error" :status 500)))
    (is-equal 200 (response:response-status resp1))
    (is-equal 200 (response:response-status resp2))
    (is-equal 500 (response:response-status resp3))))

(deftest test-error-responses ()
  "Test error response helpers"
  (is-equal 400 (response:response-status (web:bad-request)))
  (is-equal 401 (response:response-status (web:unauthorized)))
  (is-equal 403 (response:response-status (web:forbidden)))
  (is-equal 404 (response:response-status (web:not-found)))
  (is-equal 500 (response:response-status (web:internal-error))))

;; Test content negotiation
(deftest test-accepts-p ()
  "Test Accept header checking"
  (let ((json-req (request:make-request "GET" "/"
                                       :headers (map:make-map "Accept" "application/json")))
        (html-req (request:make-request "GET" "/"
                                       :headers (map:make-map "Accept" "text/html")))
        (any-req (request:make-request "GET" "/"
                                      :headers (map:make-map "Accept" "*/*"))))
    (is (web:accepts-p json-req "application/json"))
    (is-not (web:accepts-p json-req "text/html"))
    (is (web:accepts-p html-req "text/html"))
    (is (web:accepts-p any-req "application/json"))
    (is (web:accepts-p any-req "text/html"))))

(deftest test-preferred-type ()
  "Test content type preference"
  (let ((req (request:make-request "GET" "/"
                                  :headers (map:make-map 
                                          "Accept" "text/html,application/json;q=0.9"))))
    (is-equal "text/html" 
              (web:preferred-type req "application/json" "text/html"))))

;; Test middleware chain
(deftest test-middleware-chain ()
  "Test middleware chain creation and execution"
  (let* ((counter 0)
         (mw1 (lambda (handler)
                (lambda (req)
                  (incf counter)
                  (funcall handler req))))
         (mw2 (lambda (handler)
                (lambda (req)
                  (incf counter 2)
                  (funcall handler req))))
         (chain (web:make-middleware-chain mw1 mw2))
         (handler (lambda (req) 
                   (web:respond "ok"))))
    
    (web:execute chain handler (request:make-request "GET" "/"))
    (is-equal 3 counter)))

;; Test static files middleware
(deftest test-mime-type ()
  "Test MIME type detection"
  (is-equal "text/html" (web::mime-type #p"test.html"))
  (is-equal "text/css" (web::mime-type #p"style.css"))
  (is-equal "application/javascript" (web::mime-type #p"app.js"))
  (is-equal "image/png" (web::mime-type #p"logo.png"))
  (is-equal "application/octet-stream" (web::mime-type #p"file.xyz")))