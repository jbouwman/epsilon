;;;; epsilon.doc.serve -- live HTTP server for the API reference site
;;;;
;;;; Same renderers as epsilon.doc.site, but routes are answered from
;;;; the *live* image rather than from a pre-rendered tree on disk.
;;;; Modules loaded after the server starts show up on the very next
;;;; request -- no rebuild, no restart.  This is the natural read
;;;; surface during interactive development; the static generator is
;;;; for shipping a snapshot to GitHub Pages.
;;;;
;;;; Routes:
;;;;   GET /                    -> index.html
;;;;   GET /index.html          -> index.html
;;;;   GET /style.css           -> embedded stylesheet
;;;;   GET /modules/<name>.html -> per-module page
;;;;   anything else            -> 404 with a one-line body

(defpackage epsilon.doc.serve
  (:use :cl)
  (:import (epsilon.http http)
            (epsilon.http.request request)
            (epsilon.http.response response)
            (epsilon.loader loader)
            (epsilon.doc.site site)
            (epsilon.doc.extract extract)
            (epsilon.log log))
  (:export
   #:make-handler
   #:serve
   #:stop))

(in-package :epsilon.doc.serve)

;;; --------------------------------------------------------------------------
;;; Path helpers
;;; --------------------------------------------------------------------------

(defun %strip-query (path)
  "Drop everything from `?` onwards.  The router only inspects the
   path component."
  (let ((q (position #\? path)))
    (if q (subseq path 0 q) path)))

(defun %module-name-from-path (path)
  "If PATH matches /modules/<name>.html, return <name>; else NIL."
  (let ((prefix "/modules/")
        (suffix ".html"))
    (when (and (>= (length path) (+ (length prefix) (length suffix)))
               (string= path prefix :end1 (length prefix))
               (string= path suffix
                        :start1 (- (length path) (length suffix))))
      (subseq path (length prefix)
              (- (length path) (length suffix))))))

;;; --------------------------------------------------------------------------
;;; Page builders
;;; --------------------------------------------------------------------------

(defun %render-index-string ()
  "Render the index page from the *current* loader state.  Each
   request rebuilds the entries list so newly-loaded modules appear
   immediately."
  (let ((entries
          (loop for info in (loader:query-modules)
                for metadata = (loader:module-metadata info)
                collect (cons info (or (getf metadata :description) "")))))
    (with-output-to-string (out)
      (site:render-index out entries))))

(defun %render-module-string (name)
  "Render one module's page.  Returns NIL if the module isn't known
   to the loader (the route handler turns NIL into a 404)."
  (let ((info (loader:get-module name)))
    (when info
      (with-output-to-string (out)
        (site:render-module-page out (extract:describe-module name))))))

;;; --------------------------------------------------------------------------
;;; Response helpers
;;; --------------------------------------------------------------------------

(defun %html (body)
  (response:make-response
   :status 200
   :headers (epsilon.map:make-map "content-type" "text/html; charset=utf-8")
   :body body))

(defun %css (body)
  (response:make-response
   :status 200
   :headers (epsilon.map:make-map "content-type" "text/css; charset=utf-8")
   :body body))

(defun %not-found (path)
  (response:make-response
   :status 404
   :headers (epsilon.map:make-map "content-type" "text/plain; charset=utf-8")
   :body (format nil "Not found: ~A~%" path)))

(defun %method-not-allowed ()
  (response:make-response
   :status 405
   :headers (epsilon.map:make-map "content-type" "text/plain; charset=utf-8"
                                  "allow" "GET")
   :body "Method not allowed; only GET is supported.~%"))

;;; --------------------------------------------------------------------------
;;; Handler
;;; --------------------------------------------------------------------------

(defun make-handler ()
  "Return a request handler `(lambda (req) -> response)` suitable for
   passing to epsilon.http:start-server.  The closure captures
   nothing; everything is resolved against the live loader on each
   request."
  (lambda (req)
    (let* ((method (request:request-method req))
           (path (%strip-query (request:request-path req))))
      (handler-case
          (cond
            ((not (string= method "GET"))
             (%method-not-allowed))
            ((or (string= path "/") (string= path "/index.html"))
             (%html (%render-index-string)))
            ((string= path "/style.css")
             (%css (site:default-stylesheet)))
            (t
             (let ((module-name (%module-name-from-path path)))
               (cond
                 ((null module-name) (%not-found path))
                 (t
                  (let ((html (%render-module-string module-name)))
                    (if html (%html html) (%not-found path))))))))
        (error (e)
          (log:warn "epsilon.doc.serve: error handling ~A: ~A" path e)
          (response:make-response
           :status 500
           :headers (epsilon.map:make-map "content-type" "text/plain; charset=utf-8")
           :body (format nil "Internal error: ~A~%" e)))))))

;;; --------------------------------------------------------------------------
;;; Lifecycle
;;; --------------------------------------------------------------------------

(defun serve (&key (port 8089) (address "127.0.0.1"))
  "Start the live API reference server on PORT/ADDRESS and return its
   server handle.  Bind it for stop().  Default port 8089 is chosen
   to sit outside the application 3xxx range and the registry 8098
   slot.  ADDRESS defaults to localhost; pass \"0.0.0.0\" to expose
   on all interfaces."
  (log:info "epsilon.doc.serve: starting on http://~A:~D/" address port)
  (http:start-server (make-handler) :port port :address address))

(defun stop (server)
  "Stop a server returned by SERVE."
  (when server
    (http:stop-server server)
    (log:info "epsilon.doc.serve: stopped"))
  (values))
