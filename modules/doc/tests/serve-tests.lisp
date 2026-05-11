;;;; Tests for epsilon.doc.serve -- routing helpers
;;;;
;;;; The HTTP handler itself reads the live loader and renders pages
;;;; on each request; an end-to-end test would need a real server
;;;; bound to a port and a client.  These unit tests cover the pure
;;;; route-parsing helpers, which is where most of the routing logic
;;;; lives.

(defpackage epsilon.doc.serve-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.serve serve)))

(in-package :epsilon.doc.serve-tests)

;;; --------------------------------------------------------------------------
;;; %strip-query
;;; --------------------------------------------------------------------------

(deftest test-strip-query-without-query
  "A path with no `?` is returned unchanged"
  (assert-equal "/" (serve::%strip-query "/"))
  (assert-equal "/index.html" (serve::%strip-query "/index.html"))
  (assert-equal "/modules/x.html"
                (serve::%strip-query "/modules/x.html")))

(deftest test-strip-query-removes-query-string
  "Everything from the first `?` onwards is dropped"
  (assert-equal "/" (serve::%strip-query "/?foo=bar"))
  (assert-equal "/modules/x.html"
                (serve::%strip-query "/modules/x.html?ref=index"))
  (assert-equal "" (serve::%strip-query "?leading-question")))

;;; --------------------------------------------------------------------------
;;; %module-name-from-path
;;; --------------------------------------------------------------------------

(deftest test-module-name-from-path-typical
  "A well-formed module path returns the module name"
  (assert-equal "epsilon.json"
                (serve::%module-name-from-path "/modules/epsilon.json.html"))
  (assert-equal "kreisler.score"
                (serve::%module-name-from-path "/modules/kreisler.score.html")))

(deftest test-module-name-from-path-rejects-non-module-paths
  "Paths that don't match /modules/<name>.html return NIL"
  (assert-true (null (serve::%module-name-from-path "/")))
  (assert-true (null (serve::%module-name-from-path "/index.html")))
  (assert-true (null (serve::%module-name-from-path "/style.css")))
  (assert-true (null (serve::%module-name-from-path "/modules/")))
  (assert-true (null (serve::%module-name-from-path "/modules/x")))
  ;; Wrong prefix
  (assert-true (null (serve::%module-name-from-path "/m/x.html")))
  ;; Wrong suffix
  (assert-true (null (serve::%module-name-from-path "/modules/x.htm"))))

(deftest test-module-name-from-path-handles-dots-in-name
  "Module names contain dots; the matcher splits on the path
   components and the .html suffix only, not on every dot"
  (assert-equal "epsilon.codestats.record"
                (serve::%module-name-from-path
                 "/modules/epsilon.codestats.record.html")))

;;; --------------------------------------------------------------------------
;;; make-handler shape
;;; --------------------------------------------------------------------------

(deftest test-make-handler-returns-callable
  "make-handler produces a function of one argument"
  (let ((h (serve:make-handler)))
    (assert-true (functionp h))))
