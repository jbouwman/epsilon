(defpackage #:sys.gc
  (:use #:cl)
  (:shadow #:make-hash-table)
  (:export
   #:gc
   #:make-weak-pointer
   #:weak-pointer-value
   #:weak-pointer-p
   #:make-weak-hash-table
   #:hash-table-weakness
   #:finalize
   #:cancel-finalization))

(in-package #:sys.gc)

;;;; GC

(defun gc (&key full verbose)
  "Initiates a garbage collection. @code{full} forces the collection
   of all generations, when applicable. When @code{verbose} is
   @em{true}, diagnostic information about the collection is printed
   if possible."
  (sb-ext:gc :full full))

;;;; Weak Pointers

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to @code{object}. For
   portability reasons, @code{object} must not be @code{nil}."
  (assert (not (null object)))
  (sb-ext:make-weak-pointer object))

(defun weak-pointer-p (object)
  "Returns @em{true} if @code{object} is a weak pointer and @code{nil}
   otherwise."
 (sb-ext:weak-pointer-p object))

(defun weak-pointer-value (weak-pointer)
  "If @code{weak-pointer} is valid, returns its value. Otherwise,
   returns @code{nil}."
 (values (sb-ext:weak-pointer-value weak-pointer)))

;;;; Weak Hash-tables

;;; Allegro can apparently create weak hash-tables with both weak keys
;;; and weak values but it's not obvious whether it's an OR or an AND
;;; relation. TODO: figure that out.

(defun weakness-keyword-arg (weakness)
  :weakness)

(defun weakness-keyword-opt (weakness errorp)
  (declare (ignorable errorp))
  (ecase weakness
    (:key
     :key)
    (:value
     :value)
    (:key-or-value
     :key-or-value)
    (:key-and-value
     :key-and-value)))

(defun make-weak-hash-table (&rest args &key weakness (weakness-matters t)
                             &allow-other-keys)
  "Returns a new weak hash table. In addition to the standard
   arguments accepted by @code{cl:make-hash-table}, this function adds
   extra keywords: @code{:weakness} being the kind of weak table it
   should create, and @code{:weakness-matters} being whether an error
   should be signalled when that weakness isn't available (the default
   is to signal an error).  @code{weakness} can be one of @code{:key},
   @code{:value}, @code{:key-or-value}, @code{:key-and-value}.

   If @code{weakness} is @code{:key} or @code{:value}, an entry is
   kept as long as its key or value is reachable, respectively. If
   @code{weakness} is @code{:key-or-value} or @code{:key-and-value},
   an entry is kept if either or both of its key and value are
   reachable, respectively.

   @code{tg::make-hash-table} is available as an alias for this
   function should you wish to import it into your package and shadow
   @code{cl:make-hash-table}."
  (remf args :weakness)
  (remf args :weakness-matters)
  (if weakness
      (let ((arg (weakness-keyword-arg weakness))
            (opt (weakness-keyword-opt weakness weakness-matters)))
        (apply #'cl:make-hash-table
               (if arg
                   (list* arg opt args)
                   args)))
      (apply #'cl:make-hash-table args)))

(defun make-hash-table (&rest args)
  (apply #'make-weak-hash-table args))

(defun hash-table-weakness (ht)
  "Returns one of @code{nil}, @code{:key}, @code{:value},
   @code{:key-or-value} or @code{:key-and-value}."
  ;; keep this first if any of the other lisps bugously insert a NIL
  ;; for the returned (values) even when *read-suppress* is NIL (e.g. clisp)
  #.(if (find :sbcl *features*)
        (if (find-symbol "HASH-TABLE-WEAKNESS" "SB-EXT")
            (read-from-string "(sb-ext:hash-table-weakness ht)")
            nil)
        (values)))

;;;; Finalizers

(defun finalize (object function)
  "Pushes a new @code{function} to the @code{object}'s list of
   finalizers. @code{function} should take no arguments. Returns
   @code{object}.

   @b{Note:} @code{function} should not attempt to look at
   @code{object} by closing over it because that will prevent it from
   being garbage collected."
  (sb-ext:finalize object function))

(defun cancel-finalization (object)
  "Cancels all of @code{object}'s finalizers, if any."
  (sb-ext:cancel-finalization object))
