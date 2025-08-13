(defpackage :epsilon.web.session-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:session #:epsilon.web.session)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)))

(in-package :epsilon.web.session-tests)

(deftest test-session-creation ()
  "Test session creation and ID generation"
  (let ((sess (session:create-session)))
    (is (stringp (session:session-id sess)))
    (is (= 36 (length (session:session-id sess)))) ; UUID length
    (is (hash-table-p (session:session-data sess)))
    (is (numberp (session:session-created sess)))
    (is (numberp (session:session-accessed sess)))))

(deftest test-session-store ()
  "Test in-memory session store"
  (let ((store (session:make-memory-store)))
    ;; Create and save session
    (let ((sess (session:create-session)))
      (session:set-value sess "user" "alice")
      (session:set-value sess "role" "admin")
      (session:save-session store sess)
      
      ;; Retrieve session
      (let ((loaded (session:load-session store (session:session-id sess))))
        (is loaded)
        (is-equal "alice" (session:get-value loaded "user"))
        (is-equal "admin" (session:get-value loaded "role")))
      
      ;; Delete session
      (session:delete-session store (session:session-id sess))
      (is-not (session:load-session store (session:session-id sess))))))

(deftest test-session-values ()
  "Test session value manipulation"
  (let ((sess (session:create-session)))
    ;; Set values
    (session:set-value sess "count" 0)
    (session:set-value sess "user" "bob")
    
    ;; Get values
    (is-equal 0 (session:get-value sess "count"))
    (is-equal "bob" (session:get-value sess "user"))
    (is-equal "default" (session:get-value sess "missing" "default"))
    
    ;; Update value
    (session:set-value sess "count" (1+ (session:get-value sess "count")))
    (is-equal 1 (session:get-value sess "count"))
    
    ;; Remove value
    (session:remove-value sess "user")
    (is-not (session:get-value sess "user"))
    
    ;; Clear all values
    (session:clear-session sess)
    (is-not (session:get-value sess "count"))))

(deftest test-session-expiry ()
  "Test session expiration"
  (let ((store (session:make-memory-store :ttl 1))) ; 1 second TTL
    (let ((sess (session:create-session)))
      (session:save-session store sess)
      
      ;; Session should exist immediately
      (is (session:load-session store (session:session-id sess)))
      
      ;; Wait for expiry
      (sleep 1.5)
      
      ;; Session should be expired
      (is-not (session:load-session store (session:session-id sess))))))

(deftest test-session-middleware ()
  "Test session middleware integration"
  (let* ((store (session:make-memory-store))
         (middleware (web:session-middleware store))
         (handler (lambda (req)
                   (let ((sess (request:request-session req)))
                     (if sess
                         (let ((count (or (session:get-value sess "count") 0)))
                           (session:set-value sess "count" (1+ count))
                           (web:respond (map:make-map "count" count)))
                         (web:respond "No session")))))
         (wrapped (funcall middleware handler)))
    
    ;; First request - creates new session
    (let* ((req1 (request:make-request "GET" "/"))
           (resp1 (funcall wrapped req1))
           (cookies (session:parse-set-cookie-header 
                    (map:get (response:response-headers resp1) "Set-Cookie"))))
      (is cookies)
      (is (gethash "session-id" cookies))
      
      ;; Second request - uses existing session
      (let* ((session-id (gethash "session-id" cookies))
             (req2 (request:make-request "GET" "/"
                                        :headers (map:make-map 
                                                "Cookie" 
                                                (format nil "session-id=~A" session-id))))
             (resp2 (funcall wrapped req2)))
        ;; Should see incremented count
        (is (search "\"count\":1" (response:response-body resp2)))))))

(deftest test-session-cookie-options ()
  "Test session cookie configuration"
  (let* ((store (session:make-memory-store))
         (middleware (web:session-middleware store
                                            :cookie-name "my-session"
                                            :http-only t
                                            :secure t
                                            :same-site "Strict"))
         (handler (lambda (req) (web:respond "OK")))
         (wrapped (funcall middleware handler))
         (req (request:make-request "GET" "/"))
         (resp (funcall wrapped req))
         (set-cookie (map:get (response:response-headers resp) "Set-Cookie")))
    
    (is (search "my-session=" set-cookie))
    (is (search "HttpOnly" set-cookie))
    (is (search "Secure" set-cookie))
    (is (search "SameSite=Strict" set-cookie))))

(deftest test-session-regeneration ()
  "Test session ID regeneration for security"
  (let* ((store (session:make-memory-store))
         (sess (session:create-session))
         (original-id (session:session-id sess)))
    
    (session:set-value sess "user" "alice")
    (session:save-session store sess)
    
    ;; Regenerate ID
    (let ((new-id (session:regenerate-id sess)))
      (is-not (string= original-id new-id))
      (is-equal "alice" (session:get-value sess "user"))
      
      ;; Save with new ID
      (session:delete-session store original-id)
      (session:save-session store sess)
      
      ;; Old ID should not work
      (is-not (session:load-session store original-id))
      
      ;; New ID should work
      (let ((loaded (session:load-session store new-id)))
        (is loaded)
        (is-equal "alice" (session:get-value loaded "user"))))))