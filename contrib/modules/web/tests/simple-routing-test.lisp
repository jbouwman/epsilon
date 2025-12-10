(defpackage :epsilon.web.simple-routing-test
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:routing #:epsilon.web.routing)
   (#:map #:epsilon.map)))

(in-package :epsilon.web.simple-routing-test)

(deftest test-basic-route-creation ()
  "Test that we can create a basic route"
  (let ((route (routing:make-route :get "/test" 'test-handler)))
    (is route)
    (is-equal "GET" (routing:route-method route))
    (is-equal 'test-handler (routing:route-handler route))))

(deftest test-simple-pattern-compile ()
  "Test pattern compilation for simple path"
  (let ((pattern (routing:compile-route-pattern "/users")))
    (is pattern)
    (is (routing:route-matches-p pattern "/users"))
    (is-not (routing:route-matches-p pattern "/posts"))))

(deftest test-parameter-extraction ()
  "Test extracting path parameters"
  (let* ((pattern (routing:compile-route-pattern "/users/:id"))
         (matches (routing:route-matches-p pattern "/users/123")))
    (is matches)
    (when matches
      (let ((params (routing:extract-route-params pattern "/users/123")))
        (is-equal "123" (map:get params "id"))))))