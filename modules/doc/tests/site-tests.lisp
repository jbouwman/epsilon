;;;; Tests for epsilon.doc.site -- static HTML API reference site

(defpackage epsilon.doc.site-tests
  (:use :cl :epsilon.test)
  (:import (epsilon.doc.site site)))

(in-package :epsilon.doc.site-tests)

;;; --------------------------------------------------------------------------
;;; HTML escaping
;;; --------------------------------------------------------------------------

(deftest test-escape-html-passes-plain-text
  "Plain ASCII text is unchanged"
  (assert-equal "hello world" (site::escape-html "hello world")))

(deftest test-escape-html-handles-all-entities
  "<, >, &, \", ' are all replaced by their named/numeric entities"
  (assert-equal "&lt;b&gt;x &amp; y&lt;/b&gt;"
                (site::escape-html "<b>x & y</b>"))
  (assert-equal "&quot;hi&quot;" (site::escape-html "\"hi\""))
  (assert-equal "it&#39;s"      (site::escape-html "it's")))

(deftest test-escape-html-tolerates-non-string-input
  "Numbers and nil coerce instead of signalling -- defensive against
   describe-symbol returning unexpected shapes"
  (assert-equal "42"  (site::escape-html 42))
  (assert-equal ""    (site::escape-html nil)))

;;; --------------------------------------------------------------------------
;;; Symbol anchors
;;; --------------------------------------------------------------------------

(deftest test-symbol-anchor-shape
  "Anchor is `package.name`, lowercased"
  (assert-equal "epsilon.json.parse"
                (site::symbol-anchor '(:package "EPSILON.JSON" :name "PARSE"))))

(deftest test-symbol-anchor-handles-missing-fields
  "Missing package or name is replaced by the empty string"
  (assert-equal ".foo"
                (site::symbol-anchor '(:name "FOO")))
  (assert-equal "epsilon.foo."
                (site::symbol-anchor '(:package "EPSILON.FOO"))))

;;; --------------------------------------------------------------------------
;;; Stylesheet
;;; --------------------------------------------------------------------------

(deftest test-default-stylesheet-is-non-empty
  "The embedded CSS is a non-empty string"
  (let ((css (site:default-stylesheet)))
    (assert-true (stringp css))
    (assert-true (> (length css) 100))
    (assert-true (search "body" css))
    (assert-true (search ".symbol" css))))

;;; --------------------------------------------------------------------------
;;; Per-symbol rendering
;;; --------------------------------------------------------------------------

(deftest test-render-symbol-includes-name-and-package
  "render-symbol writes a div with the symbol name, package, and kind"
  (let* ((sym '(:name "PARSE"
                :package "EPSILON.JSON"
                :type :function
                :lambda-list (input)
                :summary "Parse a JSON string."))
         (html (with-output-to-string (out) (site::render-symbol out sym))))
    (assert-true (search "PARSE" html))
    (assert-true (search "epsilon.json" html))
    (assert-true (search "function" html))
    (assert-true (search "Parse a JSON string." html))
    ;; The anchor should be present so the table-of-contents can link.
    (assert-true (search "id=\"epsilon.json.parse\"" html))))

(deftest test-render-symbol-emits-lambda-list
  "The lambda-list is rendered as part of the signature"
  (let* ((sym '(:name "MAKE-MAP"
                :package "EPSILON.MAP"
                :type :function
                :lambda-list (&rest pairs)))
         (html (with-output-to-string (out) (site::render-symbol out sym))))
    (assert-true (search "&amp;rest pairs" html))))

(deftest test-render-symbol-omits-empty-sections
  "No `Parameters` heading when :params is absent; same for :returns
   and :examples.  Empty sections add visual noise."
  (let* ((sym '(:name "X" :package "P" :type :function
                :summary "Just a thing."))
         (html (with-output-to-string (out) (site::render-symbol out sym))))
    (assert-false (search "Parameters" html))
    (assert-false (search "Returns" html))
    (assert-false (search "Example" html))))

(deftest test-render-symbol-renders-params-and-returns
  "When :params and :returns are present, both render as their own
   sections"
  (let* ((sym '(:name "ADD"
                :package "P"
                :type :function
                :lambda-list (a b)
                :summary "Add two numbers."
                :params ((:name "a" :description "Left addend")
                         (:name "b" :description "Right addend"))
                :returns "The sum, an integer."))
         (html (with-output-to-string (out) (site::render-symbol out sym))))
    (assert-true (search "<h4>Parameters</h4>" html))
    (assert-true (search "Left addend" html))
    (assert-true (search "Right addend" html))
    (assert-true (search "<h4>Returns</h4>" html))
    (assert-true (search "The sum, an integer." html))))

;;; --------------------------------------------------------------------------
;;; Page rendering
;;; --------------------------------------------------------------------------

(deftest test-render-module-page-shape
  "A module page has the HTML5 doctype, the module name in the
   header, and a contents block when symbols exist"
  (let* ((doc '(:name "epsilon.demo"
                :description "A demo module."
                :packages ("EPSILON.DEMO")
                :symbols ((:name "FOO" :package "EPSILON.DEMO" :type :function
                           :summary "First fn.")
                          (:name "BAR" :package "EPSILON.DEMO" :type :variable
                           :summary "A var."))))
         (html (with-output-to-string (out) (site:render-module-page out doc))))
    (assert-true (search "<!doctype html>" html))
    (assert-true (search "epsilon.demo" html))
    (assert-true (search "A demo module." html))
    (assert-true (search "Contents" html))
    (assert-true (search "FOO" html))
    (assert-true (search "BAR" html))
    (assert-true (search "../index.html" html))))

(deftest test-render-module-page-handles-empty-module
  "A module with no exported symbols still renders without error"
  (let* ((doc '(:name "epsilon.empty" :description ""))
         (html (with-output-to-string (out) (site:render-module-page out doc))))
    (assert-true (search "epsilon.empty" html))
    (assert-false (search "<h2>Package" html))))

;;; --------------------------------------------------------------------------
;;; End-to-end smoke test
;;; --------------------------------------------------------------------------

(defun %make-tmp-dir ()
  "Create a unique temporary directory under /tmp."
  (let ((p (format nil "/tmp/epsilon-doc-site-test-~D-~D/"
                   (get-universal-time)
                   (random 100000))))
    (ensure-directories-exist p)
    p))

(deftest test-generate-site-writes-expected-files
  "generate-site against an empty :only list still produces an index
   page and a stylesheet"
  (let ((dir (%make-tmp-dir)))
    (unwind-protect
         (let ((result (site:generate-site dir :only '("__definitely-not-a-real-module__"))))
           (assert-equal 0 (getf result :modules-written))
           (assert-true (probe-file (concatenate 'string dir "index.html")))
           (assert-true (probe-file (concatenate 'string dir "style.css")))
           ;; Stylesheet has the expected shape
           (with-open-file (s (concatenate 'string dir "style.css"))
             (let ((buf (make-string (file-length s))))
               (read-sequence buf s)
               (assert-true (search ".symbol" buf)))))
      ;; Best-effort cleanup
      (ignore-errors
        (dolist (f (directory (concatenate 'string dir "*")))
          (delete-file f))
        (dolist (f (directory (concatenate 'string dir "modules/*")))
          (delete-file f))
        (sb-posix:rmdir (concatenate 'string dir "modules"))
        (sb-posix:rmdir dir)))))
