;;;; epsilon.doc.site -- static HTML API reference site generator
;;;;
;;;; Walks the loader's module table, runs describe-module against each,
;;;; and emits a minimal static site (index + per-module pages + CSS)
;;;; suitable for GitHub Pages or any plain web server.  No JavaScript,
;;;; no external CDN; the entire site is self-contained and reads
;;;; cleanly in any text browser.
;;;;
;;;; Layout under OUTPUT-DIR:
;;;;   index.html               module list grouped by realm
;;;;   style.css                one stylesheet
;;;;   modules/<name>.html      per-module page
;;;;
;;;; The generator is read-only over the loaded image: it never writes
;;;; into source trees and never mutates the environment.  Only modules
;;;; whose code is actually loaded are documented (the extractor walks
;;;; live packages); a fresh image will produce a near-empty site.
;;;; Callers that want full coverage should pre-load with
;;;;   `epsilon doc site --load-all` (or run after `epsilon test --all`
;;;;   in the same session).

(defpackage epsilon.doc.site
  (:use :cl)
  (:import (epsilon.loader loader)
            (epsilon.doc.extract extract))
  (:export
   #:generate-site
   #:render-index
   #:render-module-page
   #:default-stylesheet))

(in-package :epsilon.doc.site)

;;; --------------------------------------------------------------------------
;;; Realm classification
;;; --------------------------------------------------------------------------
;;;
;;; Group modules by realm (epsilon, avalon, kreisler, kreisleriana,
;;; other) for the index page.  Avalon modules use the `epsilon.*`
;;; package prefix per the naming convention, so realm is derived from
;;; the module's location on disk, not its name.

(defun module-realm (info)
  "Return a keyword realm for INFO based on its on-disk location.
   :epsilon for `epsilon/modules/`, :avalon for `avalon/`,
   :kreisler for `kreisler/`, :kreisleriana for the public-facing
   tier, :other otherwise."
  (let* ((loc (loader:module-location info))
         (path (etypecase loc
                 (string loc)
                 (pathname (namestring loc)))))
    (cond
      ((search "/epsilon/modules/" path) :epsilon)
      ((search "/avalon/" path) :avalon)
      ((search "/kreisleriana/" path) :kreisleriana)
      ((search "/kreisler/" path) :kreisler)
      (t :other))))

(defparameter *realm-order*
  '((:epsilon      "Epsilon -- runtime and stdlib")
    (:avalon       "Avalon -- developer and operations tooling")
    (:kreisler     "Kreisler -- shared domain libraries")
    (:kreisleriana "Kreisleriana -- end-user applications")
    (:other        "Other"))
  "Ordered (KEY . LABEL) pairs for index page sections.")

;;; --------------------------------------------------------------------------
;;; HTML escaping
;;; --------------------------------------------------------------------------

(defun escape-html (s)
  "Replace HTML-special characters in S.  Tolerates non-string input by
   coercing to a string first."
  (let ((str (if (stringp s) s (princ-to-string (or s "")))))
    (with-output-to-string (out)
      (loop for c across str do
        (case c
          (#\& (write-string "&amp;" out))
          (#\< (write-string "&lt;" out))
          (#\> (write-string "&gt;" out))
          (#\" (write-string "&quot;" out))
          (#\' (write-string "&#39;" out))
          (otherwise (write-char c out)))))))

;;; --------------------------------------------------------------------------
;;; Stylesheet
;;; --------------------------------------------------------------------------

(defun default-stylesheet ()
  "Embedded CSS for the generated site.  Plain, no fonts beyond the
   system stack, prose-width measure, comfortable line-height."
  "
:root {
  --fg: #1a1a1a;
  --muted: #666;
  --bg: #fdfdfd;
  --border: #e0e0e0;
  --code-bg: #f4f4f4;
  --link: #2548a4;
  --link-hover: #103080;
}
* { box-sizing: border-box; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
  color: var(--fg);
  background: var(--bg);
  margin: 0;
  line-height: 1.55;
}
.wrap { max-width: 56rem; margin: 0 auto; padding: 2rem 1.25rem 4rem; }
header { border-bottom: 1px solid var(--border); margin-bottom: 1.5rem; padding-bottom: 0.75rem; }
header h1 { margin: 0 0 0.25rem; font-size: 1.6rem; }
header .crumbs { color: var(--muted); font-size: 0.95rem; }
header .crumbs a { color: var(--muted); }
h1, h2, h3, h4 { line-height: 1.25; }
h2 { margin-top: 2rem; padding-bottom: 0.25rem; border-bottom: 1px solid var(--border); }
h3 { margin-top: 1.5rem; }
a { color: var(--link); text-decoration: none; }
a:hover { color: var(--link-hover); text-decoration: underline; }
code, pre, .sig {
  font-family: 'SF Mono', Consolas, Menlo, monospace;
  font-size: 0.92em;
}
pre, .sig { background: var(--code-bg); padding: 0.6rem 0.8rem; border-radius: 4px; overflow-x: auto; }
.sig { display: block; margin: 0.25rem 0 0.6rem; }
.muted { color: var(--muted); font-size: 0.92rem; }
.module-list { list-style: none; padding: 0; }
.module-list li { margin: 0.35rem 0; }
.module-list .desc { color: var(--muted); }
.symbol { margin: 1.25rem 0; padding-left: 0.75rem; border-left: 3px solid var(--border); }
.symbol h3 { margin: 0 0 0.4rem; }
.symbol .meta { color: var(--muted); font-size: 0.85rem; margin-bottom: 0.4rem; }
.symbol .docstring { white-space: pre-wrap; }
.kind { display: inline-block; font-size: 0.75rem; padding: 0.05em 0.45em;
        background: var(--code-bg); color: var(--muted); border-radius: 3px;
        text-transform: uppercase; letter-spacing: 0.05em; vertical-align: middle; }
.toc { font-size: 0.92rem; columns: 2; column-gap: 1.25rem; }
.toc ul { list-style: none; padding: 0; margin: 0; }
.toc li { break-inside: avoid; margin: 0.1rem 0; }
footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid var(--border);
         color: var(--muted); font-size: 0.85rem; }
")

;;; --------------------------------------------------------------------------
;;; Page chrome
;;; --------------------------------------------------------------------------

(defun render-page-shell (out title body-thunk &key (relative-css "style.css"))
  "Emit the HTML5 boilerplate around BODY-THUNK's output."
  (format out "<!doctype html>~%<html lang=\"en\">~%<head>~%")
  (format out "  <meta charset=\"utf-8\">~%")
  (format out "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
  (format out "  <title>~A</title>~%" (escape-html title))
  (format out "  <link rel=\"stylesheet\" href=\"~A\">~%" relative-css)
  (format out "</head>~%<body>~%<div class=\"wrap\">~%")
  (funcall body-thunk)
  (format out "<footer>Generated by <code>epsilon doc site</code></footer>~%")
  (format out "</div>~%</body>~%</html>~%"))

;;; --------------------------------------------------------------------------
;;; Index page
;;; --------------------------------------------------------------------------

(defun render-index (out modules)
  "Emit the index page listing MODULES grouped by realm.
   Each MODULES entry is (info . description-string)."
  (render-page-shell
   out "Kreisler API reference"
   (lambda ()
     (format out "<header><h1>Kreisler API reference</h1>~%")
     (format out "<div class=\"crumbs\">~D module~:P documented.</div></header>~%"
             (length modules))
     (dolist (realm-spec *realm-order*)
       (let* ((key (first realm-spec))
              (label (second realm-spec))
              (in-realm (sort (remove-if-not
                               (lambda (entry)
                                 (eq (module-realm (car entry)) key))
                               modules)
                              #'string<
                              :key (lambda (e) (loader:module-name (car e))))))
         (when in-realm
           (format out "<h2>~A</h2>~%<ul class=\"module-list\">~%" (escape-html label))
           (dolist (entry in-realm)
             (let* ((info (car entry))
                    (desc (cdr entry))
                    (name (loader:module-name info)))
               (format out
                "  <li><a href=\"modules/~A.html\"><code>~A</code></a>~@[ <span class=\"desc\">-- ~A</span>~]</li>~%"
                       (escape-html name)
                       (escape-html name)
                       (and desc (plusp (length desc))
                            (escape-html desc)))))
           (format out "</ul>~%")))))))

;;; --------------------------------------------------------------------------
;;; Per-module page
;;; --------------------------------------------------------------------------

(defun symbol-anchor (sym-plist)
  "Stable anchor id for SYM-PLIST: lowercase package + name, dotted."
  (let ((pkg (or (getf sym-plist :package) ""))
        (name (or (getf sym-plist :name) "")))
    (string-downcase (concatenate 'string pkg "." name))))

(defun render-symbol (out sym-plist)
  "Emit one symbol entry inside a module page."
  (let* ((name (or (getf sym-plist :name) ""))
         (pkg (or (getf sym-plist :package) ""))
         (type (or (getf sym-plist :type) :unknown))
         (lambda-list (getf sym-plist :lambda-list))
         (summary (getf sym-plist :summary))
         (params (getf sym-plist :params))
         (returns (getf sym-plist :returns))
         (signals (getf sym-plist :signals))
         (examples (getf sym-plist :examples))
         (anchor (symbol-anchor sym-plist)))
    (format out "<div class=\"symbol\" id=\"~A\">~%" (escape-html anchor))
    (format out "  <h3><span class=\"kind\">~A</span> <code>~A:~A</code></h3>~%"
            (escape-html (string-downcase (string type)))
            (escape-html (string-downcase pkg))
            (escape-html name))
    (when lambda-list
      (format out "  <div class=\"sig\">(<code>~A</code> ~A)</div>~%"
              (escape-html (string-downcase name))
              (escape-html (format nil "~{~A~^ ~}"
                                   (mapcar (lambda (x) (string-downcase (princ-to-string x)))
                                           lambda-list)))))
    (when (and summary (plusp (length summary)))
      (format out "  <div class=\"docstring\">~A</div>~%" (escape-html summary)))
    (when params
      (format out "  <h4>Parameters</h4>~%<ul>~%")
      (dolist (p params)
        (format out "    <li><code>~A</code> -- ~A</li>~%"
                (escape-html (or (getf p :name) ""))
                (escape-html (or (getf p :description) ""))))
      (format out "  </ul>~%"))
    (when returns
      (format out "  <h4>Returns</h4>~%<p>~A</p>~%" (escape-html returns)))
    (when signals
      (format out "  <h4>Signals</h4>~%<ul>~%")
      (dolist (s signals)
        (format out "    <li><code>~A</code> -- ~A</li>~%"
                (escape-html (or (getf s :name) ""))
                (escape-html (or (getf s :description) ""))))
      (format out "  </ul>~%"))
    (when examples
      (format out "  <h4>Example~P</h4>~%" (length examples))
      (dolist (ex examples)
        (let ((code (or (getf ex :code) (and (stringp ex) ex))))
          (when code
            (format out "  <pre>~A</pre>~%" (escape-html code))))))
    (format out "</div>~%")))

(defun render-module-page (out doc)
  "Emit the per-module page for DOC, the plist returned by
   describe-module."
  (let* ((name (getf doc :name))
         (description (getf doc :description))
         (packages (getf doc :packages))
         (symbols (getf doc :symbols))
         ;; Group symbols by package for the table of contents and the
         ;; rendered body.  Sort within each package by name.
         (by-pkg (let ((table (make-hash-table :test 'equal)))
                   (dolist (s symbols)
                     (push s (gethash (or (getf s :package) "") table)))
                   table)))
    (render-page-shell
     out (format nil "~A -- Kreisler API" name)
     (lambda ()
       (format out "<header><div class=\"crumbs\"><a href=\"../index.html\">All modules</a></div>~%")
       (format out "<h1><code>~A</code></h1>~%" (escape-html name))
       (when (and description (plusp (length description)))
         (format out "<p>~A</p>~%" (escape-html description)))
       (format out "</header>~%")
       (when (or packages symbols)
         (format out "<h2>Contents</h2>~%<div class=\"toc\">~%")
         (dolist (pkg (or packages (list (or (and symbols (getf (first symbols) :package)) ""))))
           (let ((syms (sort (copy-list (gethash pkg by-pkg))
                             #'string< :key (lambda (s) (or (getf s :name) "")))))
             (when syms
               (format out "<h3><code>~A</code></h3>~%<ul>~%" (escape-html pkg))
               (dolist (s syms)
                 (format out "  <li><a href=\"#~A\"><code>~A</code></a></li>~%"
                         (escape-html (symbol-anchor s))
                         (escape-html (or (getf s :name) ""))))
               (format out "</ul>~%"))))
         (format out "</div>~%"))
       (dolist (pkg (or packages (list (or (and symbols (getf (first symbols) :package)) ""))))
         (let ((syms (sort (copy-list (gethash pkg by-pkg))
                           #'string< :key (lambda (s) (or (getf s :name) "")))))
           (when syms
             (format out "<h2>Package <code>~A</code></h2>~%" (escape-html pkg))
             (dolist (s syms) (render-symbol out s)))))))))

;;; --------------------------------------------------------------------------
;;; Top-level driver
;;; --------------------------------------------------------------------------

(defun %ensure-output-dir (output-dir)
  "Ensure OUTPUT-DIR (and modules/ subdir) exist; never mkdir if
   already present.  Returns the trailing-slash form."
  (let ((d (if (and (plusp (length output-dir))
                    (char= (char output-dir (1- (length output-dir))) #\/))
               output-dir
               (concatenate 'string output-dir "/"))))
    (ensure-directories-exist d)
    (ensure-directories-exist (concatenate 'string d "modules/"))
    d))

(defun %write-text (path content)
  "Atomic-ish write: open with :supersede, write, close."
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'character)
    (write-string content s))
  path)

(defun generate-site (output-dir &key only)
  "Generate a static API reference site under OUTPUT-DIR.

   ONLY, if non-nil, restricts to the named modules (a list of
   strings).  Otherwise every loaded module is documented.

   Returns a plist:
     :output-dir DIR
     :modules-written N
     :modules NAMES   (list of module names that produced a page)

   Side effects: writes index.html, style.css, and one
   modules/<name>.html per documented module.  Never mutates source
   trees or the running image."
  (let* ((dir (%ensure-output-dir output-dir))
         (all-mods (loader:query-modules))
         (selected (if only
                       (remove-if-not (lambda (m)
                                        (member (loader:module-name m)
                                                only :test #'string=))
                                      all-mods)
                       all-mods))
         (entries nil)
         (written nil))
    ;; Build (info . description) entries for the index, and emit one
    ;; module page per selected module.  Modules with zero exported
    ;; symbols still get a page so cross-links don't 404.
    (dolist (info selected)
      (let* ((name (loader:module-name info))
             (metadata (loader:module-metadata info))
             (description (or (getf metadata :description) ""))
             (doc (extract:describe-module name)))
        (push (cons info description) entries)
        (let ((page-path (format nil "~Amodules/~A.html" dir name)))
          (with-output-to-string (page-out)
            (render-module-page page-out doc)
            (%write-text page-path (get-output-stream-string page-out))))
        (push name written)))
    ;; Index + stylesheet last so a partial run still leaves a useful
    ;; tree.
    (with-output-to-string (idx)
      (render-index idx (nreverse entries))
      (%write-text (concatenate 'string dir "index.html")
                   (get-output-stream-string idx)))
    (%write-text (concatenate 'string dir "style.css")
                 (default-stylesheet))
    (list :output-dir dir
          :modules-written (length written)
          :modules (nreverse written))))
