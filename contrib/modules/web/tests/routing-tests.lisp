(defpackage :epsilon.web.routing-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:routing #:epsilon.web.routing)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)))

(in-package :epsilon.web.routing-tests)

(deftest test-path-pattern-matching ()
  "Test path pattern matching with parameters"
  (let ((pattern (routing:compile-route-pattern "/users/:id")))
    (is (routing:route-matches-p pattern "/users/123"))
    (is-not (routing:route-matches-p pattern "/users"))
    (is-not (routing:route-matches-p pattern "/users/123/edit"))
    (let ((params (routing:extract-route-params pattern "/users/123")))
      (is-equal "123" (map:get params "id"))))
  
  (let ((pattern (routing:compile-route-pattern "/api/:version/users/:id")))
    (is (routing:route-matches-p pattern "/api/v1/users/42"))
    (let ((params (routing:extract-route-params pattern "/api/v1/users/42")))
      (is-equal "v1" (map:get params "version"))
      (is-equal "42" (map:get params "id")))))

(deftest test-wildcard-routes ()
  "Test wildcard path matching"
  (let ((pattern (routing:compile-route-pattern "/static/*path")))
    (is (routing:route-matches-p pattern "/static/css/main.css"))
    (is (routing:route-matches-p pattern "/static/js/vendor/lib.js"))
    (is-not (routing:route-matches-p pattern "/api/static"))
    (let ((params (routing:extract-route-params pattern "/static/css/main.css")))
      (is-equal "css/main.css" (map:get params "path")))))

(deftest test-optional-segments ()
  "Test optional path segments"
  (let ((pattern (routing:compile-route-pattern "/posts/:id/:action?")))
    (is (routing:route-matches-p pattern "/posts/123"))
    (is (routing:route-matches-p pattern "/posts/123/edit"))
    (let ((params1 (routing:extract-route-params pattern "/posts/123")))
      (is-equal "123" (map:get params1 "id"))
      (is-equal nil (map:get params1 "action")))
    (let ((params2 (routing:extract-route-params pattern "/posts/123/edit")))
      (is-equal "123" (map:get params2 "id"))
      (is-equal "edit" (map:get params2 "action")))))

(deftest test-route-constraints ()
  "Test route parameter constraints"
  (let ((pattern (routing:compile-route-pattern "/users/:id"
                                           :constraints (map:make-map "id" "\\d+"))))
    (is (routing:route-matches-p pattern "/users/123"))
    (is-not (routing:route-matches-p pattern "/users/abc")))
  
  (let ((pattern (routing:compile-route-pattern "/posts/:year/:month/:day"
                                           :constraints (map:make-map 
                                                       "year" "\\d{4}"
                                                       "month" "\\d{2}"
                                                       "day" "\\d{2}"))))
    (is (routing:route-matches-p pattern "/posts/2024/01/15"))
    (is-not (routing:route-matches-p pattern "/posts/24/1/15"))))

(deftest test-route-priority ()
  "Test route matching priority"
  (let ((routes (list
                 (routing:make-route :get "/users/me" 'handle-current-user)
                 (routing:make-route :get "/users/:id" 'handle-user)
                 (routing:make-route :get "/users/*" 'handle-users-wildcard))))
    (is-equal 'handle-current-user
              (routing:find-handler routes "GET" "/users/me"))
    (is-equal 'handle-user
              (routing:find-handler routes "GET" "/users/123"))
    (is-equal 'handle-users-wildcard
              (routing:find-handler routes "GET" "/users/admin/settings"))))

(deftest test-route-compilation ()
  "Test route pattern compilation and caching"
  (let ((pattern1 (routing:compile-route-pattern "/users/:id"))
        (pattern2 (routing:compile-route-pattern "/users/:id")))
    (is (eq pattern1 pattern2)) ; Should be cached
    
    (let ((pattern3 (routing:compile-route-pattern "/posts/:id")))
      (is-not (eq pattern1 pattern3)))))

(deftest test-route-groups ()
  "Test grouped route definitions"
  (let ((api-routes 
         (list
          (routing:make-route :get "/api/v1/users" 'list-users)
          (routing:make-route :get "/api/v1/users/:id" 'get-user)
          (routing:make-route :post "/api/v1/users" 'create-user)
          (routing:make-route :get "/api/v1/admin/stats" 'admin-stats)
          (routing:make-route :post "/api/v1/admin/users/:id/ban" 'ban-user))))
    
    (is (routing:find-handler api-routes "GET" "/api/v1/users"))
    (is (routing:find-handler api-routes "GET" "/api/v1/users/123"))
    (is (routing:find-handler api-routes "GET" "/api/v1/admin/stats"))
    (is (routing:find-handler api-routes "POST" "/api/v1/admin/users/456/ban"))
    (is-not (routing:find-handler api-routes "GET" "/users"))))

(deftest test-method-routing ()
  "Test HTTP method-specific routing"
  (let ((rest-routes
         (list
          (routing:make-route :any "/health" 'health-check)
          (routing:make-route :get "/resource/:id" 'get-resource)
          (routing:make-route :post "/resource" 'create-resource)
          (routing:make-route :put "/resource/:id" 'update-resource)
          (routing:make-route :patch "/resource/:id" 'patch-resource)
          (routing:make-route :delete "/resource/:id" 'delete-resource))))
    
    (is (routing:find-handler rest-routes "GET" "/health"))
    (is (routing:find-handler rest-routes "POST" "/health"))
    (is (routing:find-handler rest-routes "GET" "/resource/123"))
    (is-not (routing:find-handler rest-routes "POST" "/resource/123"))))