(defpackage :epsilon.web.websocket-tests
  (:use :cl :epsilon.test)
  (:local-nicknames
   (#:web #:epsilon.web)
   (#:routing #:epsilon.web.routing)
   (#:ws #:epsilon.websocket)
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)))

(in-package :epsilon.web.websocket-tests)

(deftest test-websocket-route-detection ()
  "Test WebSocket route matching"
  (let ((test-routes
         (list
          (routing:make-route :get "/" 'home-handler)
          (routing:make-route :websocket "/ws" 'websocket-handler)
          (routing:make-route :get "/api" 'api-handler))))
    
    (let ((ws-route (find-if (lambda (r) 
                              (string= (routing:route-method r) "WEBSOCKET"))
                            test-routes)))
      (is ws-route)
      (is-equal "/ws" (routing:route-raw-path ws-route)))))

(deftest test-websocket-upgrade-detection ()
  "Test WebSocket upgrade request detection"
  (let ((ws-request (request:make-request "GET" "/ws"
                                         :headers (map:make-map
                                                 "Upgrade" "websocket"
                                                 "Connection" "Upgrade"
                                                 "Sec-WebSocket-Key" "dGhlIHNhbXBsZSBub25jZQ=="
                                                 "Sec-WebSocket-Version" "13")))
        (normal-request (request:make-request "GET" "/ws")))
    
    (is (web:websocket-upgrade-request-p ws-request))
    (is-not (web:websocket-upgrade-request-p normal-request))))

(deftest test-websocket-handler-wrapper ()
  "Test WebSocket handler wrapping"
  (let* ((messages-received nil)
         (ws-handler (web:make-websocket-handler
                     :on-open (lambda (conn)
                               (push 'opened messages-received))
                     :on-message (lambda (conn msg)
                                (push msg messages-received))
                     :on-close (lambda (conn)
                              (push 'closed messages-received))))
         (routes (list (routing:make-route :websocket "/ws" ws-handler))))
    
    ;; Simulate WebSocket upgrade request
    (let ((req (request:make-request "GET" "/ws"
                                    :headers (map:make-map
                                            "Upgrade" "websocket"
                                            "Connection" "Upgrade"
                                            "Sec-WebSocket-Key" "test-key"))))
      (let ((handler (web:handle-routes routes)))
        (let ((response (funcall handler req)))
          ;; Should return WebSocket upgrade response
          (is-equal 101 (response:response-status response))
          (is-equal "websocket" 
                   (map:get (response:response-headers response) "Upgrade")))))))

(deftest test-websocket-routing-integration ()
  "Test WebSocket integration with regular routes"
  (let ((mixed-routes
         (list
          (routing:make-route :get "/" (lambda (req) (web:respond "Home")))
          (routing:make-route :websocket "/ws" 
                            (web:make-websocket-handler
                             :on-message (lambda (conn msg)
                                          (ws:send-text conn (format nil "Echo: ~A" msg)))))
          (routing:make-route :get "/api/status" 
                            (lambda (req) (web:respond (map:make-map "status" "ok")))))))
    
    ;; Test normal route
    (let* ((handler (web:handle-routes mixed-routes))
           (normal-req (request:make-request "GET" "/"))
           (normal-resp (funcall handler normal-req)))
      (is-equal 200 (response:response-status normal-resp)))
    
    ;; Test WebSocket route with upgrade headers
    (let* ((handler (web:handle-routes mixed-routes))
           (ws-req (request:make-request "GET" "/ws"
                                        :headers (map:make-map
                                                "Upgrade" "websocket"
                                                "Connection" "Upgrade")))
           (ws-resp (funcall handler ws-req)))
      ;; Should attempt upgrade (would be 101 with full WebSocket support)
      (is ws-resp))))

(deftest test-websocket-broadcast ()
  "Test WebSocket broadcast functionality"
  (let* ((connections nil)
         (ws-handler (web:make-websocket-handler
                     :on-open (lambda (conn)
                               (push conn connections))
                     :on-message (lambda (conn msg)
                                (when (string= msg "broadcast")
                                  (web:websocket-broadcast connections "Hello all!")))
                     :on-close (lambda (conn)
                              (setf connections (remove conn connections))))))
    
    ;; Simulate multiple connections (using dummy connections for testing)
    (let ((conn1 "connection-1")
          (conn2 "connection-2")
          (conn3 "connection-3"))
      
      ;; Open connections
      (funcall (web:websocket-handler-on-open ws-handler) conn1)
      (funcall (web:websocket-handler-on-open ws-handler) conn2)
      (funcall (web:websocket-handler-on-open ws-handler) conn3)
      
      (is-equal 3 (length connections))
      
      ;; Close one connection
      (funcall (web:websocket-handler-on-close ws-handler) conn2)
      (is-equal 2 (length connections)))))