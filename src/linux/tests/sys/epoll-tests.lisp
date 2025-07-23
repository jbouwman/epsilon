(defpackage :epsilon.sys.epoll.tests
  (:use :cl)
  (:local-nicknames
   (#:epoll #:epsilon.sys.epoll)
   (#:test #:epsilon.test)))

(in-package :epsilon.sys.epoll.tests)

(test:deftest epoll-creation ()
  "Test basic epoll creation and cleanup"
  (epoll:with-epoll (epfd)
    (test:is (integerp epfd))
    (test:is (> epfd 0))))

(test:deftest event-creation ()
  "Test epoll_event structure creation"
  (let ((event (epoll:make-epoll-event :events epoll:+epollin+ :data 42)))
    (test:is (= (epoll:epoll-event-events event) epoll:+epollin+))
    (test:is (= (epoll:epoll-event-data event) 42))))