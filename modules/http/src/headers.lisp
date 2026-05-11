;;;; epsilon.http.headers - Case-folding HTTP header map
;;;;
;;;; HTTP header field names are case-insensitive (RFC 7230 Section 3.2),
;;;; but most code in the wild stores them in a plain map keyed by string.
;;;; Two parts of the same codebase looking up the same header with
;;;; different casing ("Content-Type" vs "content-type") will see different
;;;; results — a defect this module exists to eliminate.
;;;;
;;;; A header-map is an immutable wrapper around `epsilon.map` that:
;;;;
;;;;   * folds string keys to a canonical lowercase form for lookup,
;;;;   * remembers the *original* casing each key was supplied with so
;;;;     iteration (and therefore wire-format output) presents headers in
;;;;     their conventional form,
;;;;   * tolerates non-string keys without coercing them.
;;;;
;;;; Internally we store entries keyed by lowercase string, with the value
;;;; being a (cons original-name actual-value) pair. Lookups always lowercase
;;;; the requested key. Iteration walks the underlying map and yields the
;;;; original-name side of each cons.

(defpackage :epsilon.http.headers
  (:use :cl)
  (:shadow #:get #:merge #:count)
  (:import (epsilon.map map)
           (epsilon.string str))
  (:export
   ;; Type
   #:header-map
   #:headers-p
   #:+empty+

   ;; Construction
   #:make-headers
   #:headers-from-map
   #:headers-from-pairs

   ;; Accessors
   #:headers-get
   #:headers-contains-p
   #:headers-count

   ;; Functional update
   #:headers-assoc
   #:headers-dissoc
   #:headers-merge

   ;; Iteration / inspection
   #:headers-each
   #:headers-keys
   #:headers-values
   #:headers-to-alist
   #:headers-to-map

   ;; Equality
   #:headers-equal

   ;; Wire canonicalization
   #:canonicalize-name))

(in-package :epsilon.http.headers)

;;; Internal canonicalization

(declaim (inline canonical-key))
(defun canonical-key (key)
  "Return the lookup form of KEY. Strings are folded to lowercase; non-string
   keys are returned unchanged so the map can also store keyword/symbol keys
   if a caller wants to."
  (if (stringp key)
      (string-downcase key)
      key))

;;; Type

(defstruct (header-map (:constructor %make-header-map (entries))
                       (:predicate headers-p)
                       (:copier nil))
  "Immutable case-folding HTTP header map.
   Entries are stored as canonical-key -> (original-name . value)."
  (entries map:+empty+ :read-only t))

(defparameter +empty+ (%make-header-map map:+empty+)
  "The empty header-map. Like map:+empty+, this value is shared.")

;;; Construction

(defun headers-from-map (raw-map)
  "Build a header-map from a plain epsilon.map. Each key is canonicalized
   for lookup; the original key (whatever the caller stored) becomes the
   display name."
  (let ((entries map:+empty+))
    (map:each
     (lambda (k v)
       (setf entries (map:assoc entries (canonical-key k) (cons k v))))
     raw-map)
    (%make-header-map entries)))

(defun headers-from-pairs (pairs)
  "Build a header-map from an alist of (name . value) pairs. Later pairs
   override earlier ones, case-insensitively."
  (let ((entries map:+empty+))
    (dolist (pair pairs)
      (let ((k (car pair)))
        (setf entries (map:assoc entries (canonical-key k) (cons k (cdr pair))))))
    (%make-header-map entries)))

(defun make-headers (&rest kvs)
  "Build a header-map from alternating key/value arguments.
   Example: (make-headers \"Content-Type\" \"text/plain\")"
  (when (oddp (length kvs))
    (error "make-headers requires an even number of arguments (key/value pairs)"))
  (let ((entries map:+empty+))
    (loop for (k v) on kvs by #'cddr
          do (setf entries (map:assoc entries (canonical-key k) (cons k v))))
    (%make-header-map entries)))

;;; Accessors

(defun headers-get (h key &optional default)
  "Look up KEY in the header-map H. Comparison is case-insensitive for
   string keys. Returns DEFAULT (nil) when the key is absent."
  (let ((entry (map:get (header-map-entries h) (canonical-key key))))
    (if entry
        (cdr entry)
        default)))

(defun headers-contains-p (h key)
  "T if KEY is present in H, ignoring case for string keys."
  (map:contains-p (header-map-entries h) (canonical-key key)))

(defun headers-count (h)
  "Return the number of distinct header entries in H."
  (map:count (header-map-entries h)))

;;; Functional update

(defun headers-assoc (h key value)
  "Return a new header-map like H but with KEY mapped to VALUE.
   The supplied KEY replaces whatever original casing was previously stored
   for the same canonical name."
  (%make-header-map
   (map:assoc (header-map-entries h)
              (canonical-key key)
              (cons key value))))

(defun headers-dissoc (h key)
  "Return a new header-map like H with KEY removed (case-insensitive)."
  (%make-header-map
   (map:dissoc (header-map-entries h) (canonical-key key))))

(defun headers-merge (h1 h2)
  "Return a header-map containing all entries from H1 and H2. When the same
   canonical name appears in both, the entry from H2 wins (and its original
   casing is what iteration will present)."
  (let ((entries (header-map-entries h1)))
    (map:each
     (lambda (canonical entry)
       (setf entries (map:assoc entries canonical entry)))
     (header-map-entries h2))
    (%make-header-map entries)))

;;; Iteration

(defun headers-each (fn h)
  "Call FN with (original-name value) for each entry in H. Returns nil."
  (map:each
   (lambda (canonical entry)
     (declare (ignore canonical))
     (funcall fn (car entry) (cdr entry)))
   (header-map-entries h)))

(defun headers-keys (h)
  "Return a list of the original-cased header names in H."
  (let ((acc nil))
    (headers-each (lambda (k v) (declare (ignore v)) (push k acc)) h)
    (nreverse acc)))

(defun headers-values (h)
  "Return a list of the header values in H."
  (let ((acc nil))
    (headers-each (lambda (k v) (declare (ignore k)) (push v acc)) h)
    (nreverse acc)))

(defun headers-to-alist (h)
  "Return ((original-name . value)...) pairs for H."
  (let ((acc nil))
    (headers-each (lambda (k v) (push (cons k v) acc)) h)
    (nreverse acc)))

(defun headers-to-map (h)
  "Project H to a plain epsilon.map keyed by the *original* (display) names.
   Useful for serialization paths that haven't been migrated yet."
  (let ((m map:+empty+))
    (headers-each
     (lambda (k v) (setf m (map:assoc m k v)))
     h)
    m))

;;; Wire canonicalization
;;;
;;; HTTP/1.x header names are case-insensitive, but the conventional
;;; presentation on the wire is Title-Case ("Content-Type", not
;;; "content-type"). Internal storage and proxy code in this codebase
;;; routinely uses lowercase, so we canonicalize at emission time rather
;;; than enforce a casing convention everywhere.
;;;
;;; The transform matches Go's textproto.CanonicalMIMEHeaderKey: split on
;;; '-', uppercase the first character of each segment, lowercase the rest.
;;; This is the most widely-recognized form on the wire and is what Go's
;;; net/http, Python's wsgiref, and similar emit.

(defun canonicalize-name (name)
  "Return the canonical Title-Case form of HTTP header NAME.
   For non-string inputs, returns NAME unchanged."
  (if (stringp name)
      (let* ((len (length name))
             (out (make-string len))
             (upper-next t))
        (dotimes (i len)
          (let ((ch (char name i)))
            (cond
              ((char= ch #\-)
               (setf (char out i) #\-)
               (setf upper-next t))
              (upper-next
               (setf (char out i) (char-upcase ch))
               (setf upper-next nil))
              (t
               (setf (char out i) (char-downcase ch))))))
        out)
      name))

;;; Equality

(defun headers-equal (h1 h2)
  "T iff H1 and H2 contain the same set of headers. Case-insensitive on
   names; values are compared with EQUAL."
  (and (= (headers-count h1) (headers-count h2))
       (let ((all-equal t))
         (block check
           (map:each
            (lambda (canonical entry)
              (let ((other (map:get (header-map-entries h2) canonical)))
                (unless (and other (equal (cdr entry) (cdr other)))
                  (setf all-equal nil)
                  (return-from check))))
            (header-map-entries h1)))
         all-equal)))
