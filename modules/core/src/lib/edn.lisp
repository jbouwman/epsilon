(defpackage epsilon.edn
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence)
   (set epsilon.set))
  (:export
   ;; Reading
   read-edn
   read-edn-from-string
   ;; Writing
   write-edn
   write-edn-to-string
   ;; Syntax control
   enable-edn-syntax
   disable-edn-syntax
   *edn-readtable*
   ;; Tagged literals
   register-tag-reader
   register-tag-writer
   *edn-tag-readers*
   *edn-tag-writers*
   ;; Keywords and symbols
   edn-keyword
   edn-keyword-p
   make-edn-keyword
   edn-keyword-name
   edn-keyword-namespace
   edn-symbol
   edn-symbol-p
   make-edn-symbol
   edn-symbol-name
   edn-symbol-namespace))

(in-package epsilon.edn)

(defparameter *edn-readtable* nil)

(defun read-edn-map (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (unless (evenp (length forms))
      (error "Map literal must have an even number of forms"))
    (apply #'map:make-map forms)))

(defun read-edn-vector (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\] stream t)))
    (apply #'vector forms)))

(defun read-edn-set (stream char)
  (declare (ignore char))
  (let ((forms (read-delimited-list #\} stream t)))
    (apply #'set:make-set forms)))

(defun read-edn-set-dispatch (stream char n)
  (declare (ignore char n))
  (read-edn-set stream #\{))

(defun enable-edn-syntax ()
  (setf *readtable* *edn-readtable*)
  (set-macro-character #\{ #'read-edn-map nil *edn-readtable*)
  (set-macro-character #\} (get-macro-character #\) nil) nil *edn-readtable*)
  (set-macro-character #\[ #'read-edn-vector nil *edn-readtable*)
  (set-macro-character #\] (get-macro-character #\) nil) nil *edn-readtable*)
  (set-dispatch-macro-character #\# #\{ #'read-edn-set-dispatch *edn-readtable*)
  t)

(defun disable-edn-syntax ()
  (setf *readtable* (copy-readtable nil))
  t)

(defun read-edn-from-string (string)
  (let ((*readtable* *edn-readtable*))
    (read-from-string string)))

(defun read-edn (stream)
  (let ((*readtable* *edn-readtable*))
    (read stream)))

(defun initialize-edn-readtable ()
  (unless *edn-readtable*
    (setf *edn-readtable* (copy-readtable nil))
    (set-macro-character #\{ #'read-edn-map nil *edn-readtable*)
    (set-macro-character #\} (get-macro-character #\) nil) nil *edn-readtable*)
    (set-macro-character #\[ #'read-edn-vector nil *edn-readtable*)
    (set-macro-character #\] (get-macro-character #\) nil) nil *edn-readtable*)
    (set-dispatch-macro-character #\# #\{ #'read-edn-set-dispatch *edn-readtable*)))

(initialize-edn-readtable)

;;;; ==========================================================================
;;;; EDN Keywords and Symbols
;;;; ==========================================================================

(defstruct (edn-keyword (:constructor %make-edn-keyword))
  "EDN keyword type"
  namespace
  name)

(defstruct (edn-symbol (:constructor %make-edn-symbol))
  "EDN symbol type"
  namespace  
  name)

(defun make-edn-keyword (name &optional namespace)
  "Create an EDN keyword"
  (%make-edn-keyword :namespace namespace :name name))

(defun make-edn-symbol (name &optional namespace)
  "Create an EDN symbol"
  (%make-edn-symbol :namespace namespace :name name))

(defmethod print-object ((k edn-keyword) stream)
  (if (edn-keyword-namespace k)
      (format stream ":~A/~A" (edn-keyword-namespace k) (edn-keyword-name k))
      (format stream ":~A" (edn-keyword-name k))))

(defmethod print-object ((s edn-symbol) stream)
  (if (edn-symbol-namespace s)
      (format stream "~A/~A" (edn-symbol-namespace s) (edn-symbol-name s))
      (format stream "~A" (edn-symbol-name s))))

;;;; ==========================================================================
;;;; Tagged Literals
;;;; ==========================================================================

(defparameter *edn-tag-readers* (make-hash-table :test 'equal)
  "Registry of EDN tag readers")

(defparameter *edn-tag-writers* (make-hash-table :test 'equal)
  "Registry of EDN tag writers")

(defun register-tag-reader (tag reader-fn)
  "Register a reader function for a tagged literal"
  (setf (gethash (string tag) *edn-tag-readers*) reader-fn))

(defun register-tag-writer (tag writer-fn)
  "Register a writer function for a tagged literal"
  (setf (gethash (string tag) *edn-tag-writers*) writer-fn))

(defun read-edn-tagged (stream char n)
  "Read a tagged literal"
  (declare (ignore char n))
  (let* ((tag (read stream t nil t))
         (value (read stream t nil t))
         (reader (gethash (string tag) *edn-tag-readers*)))
    (if reader
        (funcall reader value)
        (error "Unknown EDN tag: ~A" tag))))

;;;; ==========================================================================
;;;; Enhanced Reader Functions
;;;; ==========================================================================

(defun read-edn-keyword (stream char)
  "Read an EDN keyword"
  (declare (ignore char))
  (let ((name (read stream t nil t)))
    (if (symbolp name)
        (let ((name-str (symbol-name name)))
          (if (find #\/ name-str)
              (let ((slash-pos (position #\/ name-str)))
                (make-edn-keyword (subseq name-str (1+ slash-pos))
                                  (subseq name-str 0 slash-pos)))
              (make-edn-keyword name-str)))
        (error "Invalid keyword syntax"))))

;;;; ==========================================================================
;;;; Writing Functions
;;;; ==========================================================================

(defgeneric write-edn-value (value stream)
  (:documentation "Write a value in EDN format"))

;; Nil, booleans, and numbers
(defmethod write-edn-value ((value null) stream)
  (write-string "nil" stream))

(defmethod write-edn-value ((value (eql t)) stream)
  (write-string "true" stream))

(defmethod write-edn-value ((value number) stream)
  (prin1 value stream))

;; Strings
(defmethod write-edn-value ((value string) stream)
  (write-char #\" stream)
  (loop for char across value do
    (case char
      (#\Newline (write-string "\\n" stream))
      (#\Tab (write-string "\\t" stream))
      (#\Return (write-string "\\r" stream))
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (otherwise (write-char char stream))))
  (write-char #\" stream))

;; Characters
(defmethod write-edn-value ((value character) stream)
  (write-char #\\ stream)
  (case value
    (#\Newline (write-string "newline" stream))
    (#\Tab (write-string "tab" stream))
    (#\Return (write-string "return" stream))
    (#\Space (write-string "space" stream))
    (otherwise (write-char value stream))))

;; Keywords
(defmethod write-edn-value ((value edn-keyword) stream)
  (write-char #\: stream)
  (when (edn-keyword-namespace value)
    (write-string (edn-keyword-namespace value) stream)
    (write-char #\/ stream))
  (write-string (edn-keyword-name value) stream))

;; Symbols
(defmethod write-edn-value ((value edn-symbol) stream)
  (when (edn-symbol-namespace value)
    (write-string (edn-symbol-namespace value) stream)
    (write-char #\/ stream))
  (write-string (edn-symbol-name value) stream))

(defmethod write-edn-value ((value symbol) stream)
  (let ((name (symbol-name value)))
    (cond
      ((string= name "NIL") (write-string "nil" stream))
      ((string= name "T") (write-string "true" stream))
      (t (write-string (string-downcase name) stream)))))

;; Lists and vectors
(defmethod write-edn-value ((value list) stream)
  (write-char #\( stream)
  (loop for (item . rest) on value do
    (write-edn-value item stream)
    (when rest (write-char #\Space stream)))
  (write-char #\) stream))

(defmethod write-edn-value ((value vector) stream)
  (write-char #\[ stream)
  (loop for i from 0 below (length value) do
    (when (> i 0) (write-char #\Space stream))
    (write-edn-value (aref value i) stream))
  (write-char #\] stream))

;; Maps (using epsilon.map)
(defmethod write-edn-value ((value map:hamt) stream)
  (write-char #\{ stream)
  (let ((first t))
    (map:each (lambda (k v)
                (unless first (write-char #\Space stream))
                (setf first nil)
                (write-edn-value k stream)
                (write-char #\Space stream)
                (write-edn-value v stream))
              value))
  (write-char #\} stream))

;; Hash tables (convert to EDN map notation)
(defmethod write-edn-value ((value hash-table) stream)
  (write-char #\{ stream)
  (let ((first t))
    (maphash (lambda (k v)
               (unless first (write-char #\Space stream))
               (setf first nil)
               (write-edn-value k stream)
               (write-char #\Space stream)
               (write-edn-value v stream))
             value))
  (write-char #\} stream))

;; Sets (using epsilon.set)
(defmethod write-edn-value ((value set:hamt-set) stream)
  (write-string "#{" stream)
  (let ((first t))
    (dolist (item (set:seq value))
      (unless first (write-char #\Space stream))
      (setf first nil)
      (write-edn-value item stream)))
  (write-char #\} stream))

;; Tagged literals
(defun write-edn-tagged (tag value stream)
  "Write a tagged literal"
  (write-char #\# stream)
  (write-string (string tag) stream)
  (write-char #\Space stream)
  (write-edn-value value stream))

;;;; ==========================================================================
;;;; Main Writing Functions
;;;; ==========================================================================

(defun write-edn (value &optional (stream *standard-output*))
  "Write a value in EDN format to a stream"
  (write-edn-value value stream)
  value)

(defun write-edn-to-string (value)
  "Write a value in EDN format to a string"
  (with-output-to-string (stream)
    (write-edn value stream)))

;;;; ==========================================================================
;;;; Enhanced Reading Functions
;;;; ==========================================================================

(defun read-edn-nil-or-boolean (stream char)
  "Read nil, true, or false"
  (declare (ignore char))
  (let ((symbol (read stream t nil t)))
    (cond
      ((eq symbol '|nil|) nil)
      ((eq symbol '|true|) t)
      ((eq symbol '|false|) nil)
      (t symbol))))

;;;; ==========================================================================
;;;; Initialize Enhanced Reader
;;;; ==========================================================================

(defun initialize-enhanced-edn-readtable ()
  "Initialize the EDN readtable with all features"
  (unless *edn-readtable*
    (setf *edn-readtable* (copy-readtable nil)))
  
  ;; Basic EDN structures
  (set-macro-character #\{ #'read-edn-map nil *edn-readtable*)
  (set-macro-character #\} (get-macro-character #\) nil) nil *edn-readtable*)
  (set-macro-character #\[ #'read-edn-vector nil *edn-readtable*)
  (set-macro-character #\] (get-macro-character #\) nil) nil *edn-readtable*)
  
  ;; Sets
  (set-dispatch-macro-character #\# #\{ #'read-edn-set-dispatch *edn-readtable*)
  
  ;; Keywords
  (set-macro-character #\: #'read-edn-keyword nil *edn-readtable*)
  
  ;; Tagged literals
  (set-dispatch-macro-character #\# #\_ #'read-edn-tagged *edn-readtable*)
  
  *edn-readtable*)

;; Initialize the enhanced readtable
(initialize-enhanced-edn-readtable)

;;;; ==========================================================================
;;;; Default Tagged Literal Handlers
;;;; ==========================================================================

;; Register default inst (instant/timestamp) handler
(register-tag-reader "inst"
  (lambda (value)
    ;; For now, just return the string representation
    ;; Could integrate with a date/time library later
    (list 'inst value)))

(register-tag-writer "inst"
  (lambda (value stream)
    (write-edn-tagged "inst" value stream)))

;; Register default uuid handler
(register-tag-reader "uuid"
  (lambda (value)
    ;; Could integrate with epsilon.uuid later
    (list 'uuid value)))

(register-tag-writer "uuid"
  (lambda (value stream)
    (write-edn-tagged "uuid" value stream)))
