(defpackage :epsilon.sys.kqueue.tests
  (:use :cl)
  (:local-nicknames
   (#:kqueue #:epsilon.sys.kqueue)
   (#:test #:epsilon.tool.test)))

(in-package :epsilon.sys.kqueue.tests)

(test:deftest kqueue-creation ()
  "Test basic kqueue creation and cleanup"
  (kqueue:with-kqueue (kq)
    (test:is (integerp kq))
    (test:is (> kq 0))))

(test:deftest event-creation ()
  "Test kevent structure creation"
  (let ((event (kqueue:make-kevent :ident 1 :filter kqueue:+evfilt-read+ :flags kqueue:+ev-add+)))
    (test:is (= (kqueue:kevent-ident event) 1))
    (test:is (= (kqueue:kevent-filter event) kqueue:+evfilt-read+))
    (test:is (= (kqueue:kevent-flags event) kqueue:+ev-add+))))