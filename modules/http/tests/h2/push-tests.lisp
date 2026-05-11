;;;; HTTP/2 Server Push Tests
;;;;
;;;; Unit tests for HTTP/2 server push promises, push cache,
;;;; and resource selection per RFC 7540 Section 8.2.

(defpackage :epsilon.http.h2.push-tests
  (:use :cl :epsilon.test)
  (:import
   (epsilon.http.h2.push push)
   (epsilon.http.h2.frames frames)))

;;;; Push Promise Structure Tests

(deftest test-push-promise-creation ()
  "Test push-promise struct creation and field access"
  (let ((promise (push:make-push-promise
                  :stream-id 1
                  :promised-stream-id 2
                  :headers '((":method" . "GET") (":path" . "/style.css")))))
    (assert-true (push:push-promise-p promise))
    (assert-true (= (push:push-promise-stream-id promise) 1))
    (assert-true (= (push:push-promise-promised-stream-id promise) 2))
    (assert-true (= (length (push:push-promise-headers promise)) 2))
    (assert-true (eq (push:push-promise-state promise) :promised))))

(deftest test-push-promise-defaults ()
  "Test push-promise default values"
  (let ((promise (push:make-push-promise)))
    (assert-true (= (push:push-promise-stream-id promise) 0))
    (assert-true (= (push:push-promise-promised-stream-id promise) 0))
    (assert-true (null (push:push-promise-headers promise)))
    (assert-true (eq (push:push-promise-state promise) :promised))))

(deftest test-push-promise-state-transitions ()
  "Test push promise state can be updated"
  (let ((promise (push:make-push-promise :state :promised)))
    (assert-true (eq (push:push-promise-state promise) :promised))
    (setf (push:push-promise-state promise) :sent)
    (assert-true (eq (push:push-promise-state promise) :sent))
    (setf (push:push-promise-state promise) :fulfilled)
    (assert-true (eq (push:push-promise-state promise) :fulfilled))))

(deftest test-push-promise-cancelled-state ()
  "Test push promise can transition to cancelled"
  (let ((promise (push:make-push-promise :state :promised)))
    (setf (push:push-promise-state promise) :cancelled)
    (assert-true (eq (push:push-promise-state promise) :cancelled))))

;;;; Push Cache Structure Tests

(deftest test-push-cache-creation ()
  "Test push-cache struct creation"
  (let ((cache (push:make-push-cache)))
    (assert-true (not (null cache)))
    (assert-true (hash-table-p (push::push-cache-resources cache)))
    (assert-true (hash-table-p (push::push-cache-push-history cache)))
    (assert-true (= (push::push-cache-max-age cache) 3600))))

(deftest test-push-cache-custom-max-age ()
  "Test push-cache with custom max-age"
  (let ((cache (push:make-push-cache :max-age 7200)))
    (assert-true (= (push::push-cache-max-age cache) 7200))))

;;;; cache-push-resource Tests

(deftest test-cache-push-resource ()
  "Test caching a pushable resource"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/style.css"
                              '(("content-type" . "text/css"))
                              "body { color: red; }")
    ;; Resource should be stored
    (let ((resource (gethash "/style.css" (push::push-cache-resources cache))))
      (assert-true (not (null resource)))
      (assert-true (equal (getf resource :data) "body { color: red; }")))))

(deftest test-cache-push-resource-with-dependencies ()
  "Test caching a resource with dependencies"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/app.js"
                              '(("content-type" . "application/javascript"))
                              "console.log('hi')"
                              :dependencies '("/lib.js"))
    (let ((resource (gethash "/app.js" (push::push-cache-resources cache))))
      (assert-true (not (null resource)))
      (assert-true (equal (getf resource :dependencies) '("/lib.js"))))))

;;;; should-push-resource-p Tests

(deftest test-should-push-not-recently-pushed ()
  "Test should-push returns t when not recently pushed"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/style.css" nil nil)
    (assert-true (push:should-push-resource-p cache "/style.css" nil (get-universal-time)))))

(deftest test-should-push-recently-pushed ()
  "Test should-push returns nil when recently pushed"
  (let ((cache (push:make-push-cache :max-age 3600))
        (now (get-universal-time)))
    ;; Mark as recently pushed
    (setf (gethash "/style.css" (push::push-cache-push-history cache)) now)
    (assert-true (not (push:should-push-resource-p cache "/style.css" nil now)))))

(deftest test-should-push-expired-history ()
  "Test should-push returns t when push history has expired"
  (let ((cache (push:make-push-cache :max-age 3600))
        (now (get-universal-time)))
    ;; Mark as pushed long ago (more than max-age ago)
    (setf (gethash "/style.css" (push::push-cache-push-history cache))
          (- now 7200))
    (assert-true (push:should-push-resource-p cache "/style.css" nil now))))

(deftest test-should-push-no-push-header ()
  "Test should-push returns nil when client sends no-push in Accept"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/style.css" nil nil)
    (let ((headers '(("accept" . "text/html; no-push"))))
      (assert-true (not (push:should-push-resource-p cache "/style.css"
                                            headers
                                            (get-universal-time)))))))

(deftest test-should-push-normal-accept-header ()
  "Test should-push returns t with normal Accept header"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/style.css" nil nil)
    (let ((headers '(("accept" . "text/html"))))
      (assert-true (push:should-push-resource-p cache "/style.css"
                                       headers
                                       (get-universal-time))))))

;;;; get-pushable-resources Tests

(deftest test-get-pushable-resources-empty ()
  "Test get-pushable-resources with empty cache"
  (let ((cache (push:make-push-cache)))
    (let ((resources (push:get-pushable-resources cache "/" nil)))
      (assert-true (null resources)))))

(deftest test-get-pushable-resources-sorted ()
  "Test get-pushable-resources returns resources sorted by path"
  (let ((cache (push:make-push-cache)))
    (push:cache-push-resource cache "/z.js" nil nil)
    (push:cache-push-resource cache "/a.css" nil nil)
    (push:cache-push-resource cache "/m.png" nil nil)
    (let ((resources (push:get-pushable-resources cache "/" nil)))
      (assert-true (= (length resources) 3))
      ;; Should be sorted alphabetically by path
      (assert-true (equal (car (first resources)) "/a.css"))
      (assert-true (equal (car (second resources)) "/m.png"))
      (assert-true (equal (car (third resources)) "/z.js")))))

(deftest test-get-pushable-resources-excludes-recently-pushed ()
  "Test get-pushable-resources excludes recently pushed items"
  (let ((cache (push:make-push-cache))
        (now (get-universal-time)))
    (push:cache-push-resource cache "/a.css" nil nil)
    (push:cache-push-resource cache "/b.js" nil nil)
    ;; Mark /a.css as recently pushed
    (setf (gethash "/a.css" (push::push-cache-push-history cache)) now)
    (let ((resources (push:get-pushable-resources cache "/" nil)))
      ;; Only /b.js should be returned
      (assert-true (= (length resources) 1))
      (assert-true (equal (car (first resources)) "/b.js")))))

;;;; make-push-promise-frame Tests

(deftest test-make-push-promise-frame-payload ()
  "Test PUSH_PROMISE frame payload format"
  (let* ((headers-block (make-array 10 :element-type '(unsigned-byte 8)
                                       :initial-element 42))
         (frame (push::make-push-promise-frame 1 2 headers-block)))
    (assert-true (= (frames:http2-frame-type frame) frames:+frame-push-promise+))
    (assert-true (= (frames:http2-frame-stream-id frame) 1))
    ;; END_HEADERS flag should be set
    (assert-true (logtest (frames:http2-frame-flags frame) frames:+flag-end-headers+))
    ;; Payload = 4 bytes promised-stream-id + 10 bytes header block = 14
    (assert-true (= (frames:http2-frame-length frame) 14))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; First 4 bytes: promised stream ID = 2
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) 0))
      (assert-true (= (aref payload 3) 2))
      ;; Header block starts at offset 4
      (assert-true (= (aref payload 4) 42)))))

(deftest test-make-push-promise-frame-large-stream-id ()
  "Test PUSH_PROMISE frame with large promised stream ID"
  (let* ((headers-block (make-array 1 :element-type '(unsigned-byte 8)
                                      :initial-element 0))
         (frame (push::make-push-promise-frame 1 256 headers-block)))
    (let ((payload (frames:http2-frame-payload frame)))
      ;; 256 = #x00000100
      (assert-true (= (aref payload 0) 0))
      (assert-true (= (aref payload 1) 0))
      (assert-true (= (aref payload 2) 1))
      (assert-true (= (aref payload 3) 0)))))
