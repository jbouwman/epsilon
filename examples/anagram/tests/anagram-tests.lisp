(defpackage anagram.unit-tests
  (:use cl)
  (:local-nicknames
   (test epsilon.test)
   (json epsilon.json)
   (http epsilon.http.client)
   (map epsilon.map)
   (str epsilon.string)
   (web epsilon.web)
   (server epsilon.http.server))
  (:export run-tests))

(in-package anagram.unit-tests)

(defparameter *test-port* 8081)
(defparameter *base-url* (format nil "http://localhost:~A" *test-port*))

;; Test server fixture - returns (server . port) pair
(test:fixture anagram-server (&key port)
  (:setup
   (let* ((test-port (or port (+ 9000 (random 1000))))  ; Use random high port
          (app (web:wrap-middleware 
                (web:handle-routes anagram::*routes*)
                web:logging-middleware
                web:json-errors-middleware))
          (server (server:start-server app :port test-port :address "127.0.0.1")))
     ;; Give the server a moment to start up
     (sleep 0.1)
     (cons server test-port)))
  (:teardown
   (when anagram-server
     (ignore-errors (server:stop-server (car anagram-server))))))

(test:deftest test-shuffle-string ()
  "Test the shuffle-string function"
  (let ((original "hello")
        (shuffled (anagram:shuffle-string "hello")))
    ;; Should be same length
    (test:is (= (length original) (length shuffled)))
    ;; Should contain same characters (but allow same result occasionally)
    (test:is (every (lambda (char) (find char shuffled)) original))
    (test:is (every (lambda (char) (find char original)) shuffled))))

(test:deftest test-compute-anagram ()
  "Test the compute-anagram function"
  (let* ((original "hello world")
         (anagram (anagram:compute-anagram original)))
    ;; Should be same length  
    (test:is (= (length original) (length anagram)))
    ;; Should contain same characters
    (test:is-equal (sort (copy-seq original) #'char<)
                   (sort (copy-seq anagram) #'char<))))



(test:deftest test-anagram-generation ()
  "Test successful anagram generation"
  (test:with-fixture (server-info anagram-server)
    (let* ((port (cdr server-info))
           (base-url (format nil "http://localhost:~A" port))
           (request-body (with-output-to-string (s)
                           (json:encode (map:make-map "text" "hello world") s))))
      (multiple-value-bind (status-code headers body)
          (http:http-post (format nil "~A/api/anagram" base-url)
                     :body request-body
                     :headers (map:make-map "Content-Type" "application/json"))
        (declare (ignore headers))
        (test:is (= status-code 200))
        (let ((body-data (json:parse body)))
          (test:is-equal (map:get body-data "original") "hello world")
          (test:is (stringp (map:get body-data "anagram")))
          ;; Check anagram has same characters
          (test:is-equal (sort (copy-seq (map:get body-data "original")) #'char<)
                         (sort (copy-seq (map:get body-data "anagram")) #'char<)))))))

(test:deftest test-empty-text ()
  "Test anagram generation with empty text"
  (test:with-fixture (server-info anagram-server)
    (let* ((port (cdr server-info))
           (base-url (format nil "http://localhost:~A" port))
           (request-body (with-output-to-string (s)
                           (json:encode (map:make-map "text" "") s))))
      (multiple-value-bind (status-code headers body)
          (http:http-post (format nil "~A/api/anagram" base-url)
                     :body request-body
                     :headers (map:make-map "Content-Type" "application/json"))
        (declare (ignore headers))
        (test:is (= status-code 400))
        (let ((body-data (json:parse body)))
          (test:is (map:get body-data "error")))))))

(test:deftest test-missing-text-field ()
  "Test anagram generation without text field"
  (test:with-fixture (server-info anagram-server)
    (let* ((port (cdr server-info))
           (base-url (format nil "http://localhost:~A" port))
           (request-body (with-output-to-string (s)
                           (json:encode (map:make-map "wrong" "field") s))))
      (multiple-value-bind (status-code headers body)
          (http:http-post (format nil "~A/api/anagram" base-url)
                     :body request-body
                     :headers (map:make-map "Content-Type" "application/json"))
        (declare (ignore headers))
        (test:is (= status-code 400))
        (let ((body-data (json:parse body)))
          (test:is (map:get body-data "error")))))))

(test:deftest test-invalid-json ()
  "Test anagram generation with invalid JSON"
  (test:with-fixture (server-info anagram-server)
    (let* ((port (cdr server-info))
           (base-url (format nil "http://localhost:~A" port)))
      (multiple-value-bind (status-code headers body)
          (http:http-post (format nil "~A/api/anagram" base-url)
                     :body "not valid json"
                     :headers (map:make-map "Content-Type" "application/json"))
        (declare (ignore headers))
        (test:is (= status-code 400))
        (let ((body-data (json:parse body)))
          (test:is (map:get body-data "error")))))))

(test:deftest test-404-endpoint ()
  "Test that non-existent endpoints return 404"
  (test:with-fixture (server-info anagram-server)
    (let* ((port (cdr server-info))
           (base-url (format nil "http://localhost:~A" port)))
      (multiple-value-bind (status-code headers body)
          (http:http-get (format nil "~A/nonexistent" base-url))
        (declare (ignore headers body))
        (test:is (= status-code 404))))))

(defun run-tests ()
  "Run all anagram tests"
  ;; Tests are automatically discovered and run by epsilon test runner
  t)
