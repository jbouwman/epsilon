(defpackage :epsilon.tool.format
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string))
  (:export
   ;; generics
   #:to-ir
   #:emit-tokens
   #:collect-token
   
   ;; types
   #:ir-pair
   
   ;; functions
   #:format-object
   #:key
   #:make-text
   
   ;; IR helper functions
   #:make-ir-primitive
   #:make-ir-container
   #:make-ir-pair
   #:make-ir-named-object
   #:make-ir-array
   #:make-ir-object

   ;; tokenization
   :tokenize
   
   ;; emitters
   #:compact-json-emitter
   #:pretty-json-emitter
   #:make-compact-json-emitter
   #:make-pretty-json-emitter))

(in-package :epsilon.tool.format)

(defclass ir-node () ())

(defclass ir-primitive (ir-node)
  ((value :initarg :value :accessor value)))

(defclass ir-container (ir-node)
  ((elements :initarg :elements :accessor elements)))

(defclass ir-pair (ir-node)
  ((key :initarg :key :accessor key)
   (value :initarg :value :accessor value)))

(defclass ir-named-object (ir-node)
  ((name :initarg :name :accessor name)
   (attributes :initarg :attributes :accessor attributes)))

(defgeneric to-ir (object)
  (:documentation "Convert a Lisp object to an intermediate representation that supports formatted pretining."))

;; Helper functions for creating IR instances

(defun make-ir-primitive (value)
  "Create an IR primitive node with the given value"
  (make-instance 'ir-primitive :value value))

(defun make-ir-container (elements)
  "Create an IR container node with elements"
  (make-instance 'ir-container :elements elements))

(defun make-ir-pair (key value)
  "Create an IR pair node with key and value"
  (make-instance 'ir-pair :key key :value value))

(defun make-ir-named-object (name attributes)
  "Create an IR named object node with name and attribute alist"
  (make-instance 'ir-named-object :name name :attributes attributes))

;; Define token types

(defclass token ()
  ((width :initarg :width :accessor token-width :initform 0)
   (context-id :initform nil :accessor token-context-id)))

(defclass open-token (token)
  ((delimiter :initarg :delimiter :accessor token-delimiter)
   (content-type :initarg :content-type :accessor token-content-type)
   (complexity :initform 0 :accessor token-complexity)))

;; Add token specific metadata
(defclass newline-token (token)
  ((structural :initarg :structural :accessor token-structural :initform t)))

(defclass indent-token (token)
  ((level :initarg :level :accessor token-level)
   (structural :initarg :structural :accessor token-structural :initform t)))

(defclass dedent-token (token)
  ((level :initarg :level :accessor token-level)
   (structural :initarg :structural :accessor token-structural :initform t)))

(defclass text-token (token)
  ((text :initarg :text :accessor token-text)))

(defclass close-token (token)
  ((delimiter :initarg :delimiter :accessor token-delimiter)))

(defclass separator-token (token)
  ((text :initarg :text :accessor token-text)))

;; Constructors for convenience
(defun make-text (text)
  (make-instance 'text-token :text text :width (length text)))

(defun make-open (delimiter content-type)
  (make-instance 'open-token :delimiter delimiter :content-type content-type 
                             :width (length delimiter)))

(defun make-close (delimiter)
  (make-instance 'close-token :delimiter delimiter :width (length delimiter)))

(defun make-separator (text)
  (make-instance 'separator-token :text text :width (length text)))

(defun make-newline ()
  (make-instance 'newline-token))

(defun make-indent (level)
  (make-instance 'indent-token :level level))

(defun make-dedent (level)
  (make-instance 'dedent-token :level level))

(defclass token-collector ()
  ((tokens :initform nil :accessor tokens)
   (context-stack :initform nil :accessor context-stack)))

(defgeneric collect-token (collector token)
  (:documentation "Add a token to the collector"))

(defmethod collect-token ((collector token-collector) token)
  (push token (tokens collector))
  token)

(defmethod finalize-tokens ((collector token-collector))
  "Return the collected tokens in the correct order"
  (nreverse (tokens collector)))

(defclass object-formatter ()
  ((indent-width :initarg :indent-width :accessor indent-width
                 :initform 2)
   (line-limit :initarg :line-limit :accessor line-limit :initform 80)
   (current-indent :initform 0 :accessor current-indent)
   (current-column :initform 0 :accessor current-column)
   (current-line :initform 1 :accessor current-line)
   (result-stream :initarg :stream :accessor result-stream)))

(defgeneric format-token (object-formatter token)
  (:documentation "Format a token to the output"))

(defmethod format-token ((fmt object-formatter) (token text-token))
  (format (result-stream fmt) "~a" (token-text token))
  (incf (current-column fmt) (token-width token)))

(defmethod format-token ((fmt object-formatter) (token open-token))
  (format (result-stream fmt) "~a" (token-delimiter token))
  (incf (current-column fmt) (token-width token)))

(defmethod format-token ((fmt object-formatter) (token close-token))
  (format (result-stream fmt) "~a" (token-delimiter token))
  (incf (current-column fmt) (token-width token)))

(defmethod format-token ((fmt object-formatter) (token separator-token))
  (format (result-stream fmt) "~a" (token-text token))
  (incf (current-column fmt) (token-width token)))

(defmethod format-token ((fmt object-formatter) (token newline-token))
  ;; Render as actual newlines with proper indentation
  (format (result-stream fmt) "~%")
  (setf (current-column fmt) 0)
  (incf (current-line fmt))
  (format (result-stream fmt) "~v@{~A~:*~}" 
          (current-indent fmt) " ")
  (incf (current-column fmt) (current-indent fmt)))

(defmethod format-token ((fmt object-formatter) (token indent-token))
  ;; Update indent level
  (incf (current-indent fmt) (* (token-level token) (indent-width fmt)))
  ;; If we're at the beginning of a line, emit the indentation
  (when (zerop (current-column fmt))
    (let ((indent-spaces (current-indent fmt)))
      (format (result-stream fmt) "~v@{~A~:*~}" indent-spaces " ")
      (incf (current-column fmt) indent-spaces))))

(defmethod format-token ((fmt object-formatter) (token dedent-token))
  ;; Update indent level  
  (decf (current-indent fmt) (* (token-level token) (indent-width fmt))))

(defun format-tokens (tokens stream &key (indent-width 2) (line-limit 80))
  "Format a sequence of tokens to the output stream"
  (let ((formatter (make-instance 'object-formatter 
                                  :stream stream
                                  :indent-width indent-width
                                  :line-limit line-limit)))
    (loop for token in tokens
          do (format-token formatter token))))

;; Format-specific emitter classes

(defclass emitter ()
  ()
  (:documentation "Base class for format-specific emitters"))

(defgeneric emit-tokens (emitter ir collector)
  (:documentation "Emit tokens for an IR node to the collector using format-specific emitter"))

(defclass json-emitter (emitter)
  ()
  (:documentation "Base class for format-specific emitters"))

(defun format-json-primitive (value)
  "Format a primitive value as exact JSON text"
  (cond
    ((stringp value) (format nil "~S" value))  ; Properly quote and escape strings
    ((eq value t) "true")
    ((eq value nil) "false")
    ((null value) "null")
    (t (format nil "~a" value))))

(defmethod emit-tokens ((emitter json-emitter) (ir ir-primitive) collector)
  (collect-token collector 
                 (make-text (format-json-primitive (value ir)))))

(defclass compact-json-emitter (json-emitter)
  ()
  (:documentation "Compact JSON format emitter - produces minimal, single-line JSON"))

(defclass pretty-json-emitter (json-emitter)
  ((indent-width :initarg :indent-width :accessor indent-width :initform 2))
  (:documentation "Pretty JSON format emitter - produces formatted, indented JSON"))

(defun make-compact-json-emitter ()
  "Create a new compact JSON emitter"
  (make-instance 'compact-json-emitter))

(defun make-pretty-json-emitter (&key (indent-width 2))
  "Create a new pretty JSON emitter with formatting options"
  (make-instance 'pretty-json-emitter :indent-width indent-width))

;; Add container type metadata to distinguish arrays from objects

(defclass ir-array (ir-container) ())

(defclass ir-object (ir-container) ())

(defun make-ir-array (elements)
  "Create an IR array container"
  (make-instance 'ir-array :elements elements))

(defun make-ir-object (elements)
  "Create an IR object container"
  (make-instance 'ir-object :elements elements))

(defmethod emit-tokens ((emitter compact-json-emitter) (ir ir-array) collector)
  (let ((contents (elements ir)))
    ;; Array uses brackets
    (collect-token collector (make-open "[" :container))
    
    ;; Contents with minimal spacing
    (loop for item in contents
          for i from 1
          do (emit-tokens emitter item collector)
             (when (< i (length contents))
               (collect-token collector (make-separator ","))))
    
    (collect-token collector (make-close "]"))))

(defmethod emit-tokens ((emitter pretty-json-emitter) (ir ir-array) collector)
  (let ((contents (elements ir)))
    ;; Array uses brackets
    (collect-token collector (make-open "[" :container))
    
    ;; Emit structural tokens for pretty formatting
    (when contents
      (collect-token collector (make-newline))
      (collect-token collector (make-indent 1)))
    
    ;; Contents with newlines
    (loop for item in contents
          for i from 1
          do (emit-tokens emitter item collector)
             (when (< i (length contents))
               (collect-token collector (make-separator ","))
               (collect-token collector (make-newline))))
    
    ;; Close with proper formatting
    (when contents
      (collect-token collector (make-dedent 1))
      (collect-token collector (make-newline)))
    
    (collect-token collector (make-close "]"))))

(defmethod emit-tokens ((emitter compact-json-emitter) (ir ir-object) collector)
  (let ((contents (elements ir)))
    ;; Object uses braces
    (collect-token collector (make-open "{" :container))
    
    ;; Contents with minimal spacing
    (loop for item in contents
          for i from 1
          do (emit-tokens emitter item collector)
             (when (< i (length contents))
               (collect-token collector (make-separator ","))))
    
    (collect-token collector (make-close "}"))))

(defmethod emit-tokens ((emitter pretty-json-emitter) (ir ir-object) collector)
  (let ((contents (elements ir)))
    ;; Object uses braces
    (collect-token collector (make-open "{" :container))
    
    ;; Emit structural tokens for pretty formatting
    (when contents
      (collect-token collector (make-newline))
      (collect-token collector (make-indent 1)))
    
    ;; Contents with newlines
    (loop for item in contents
          for i from 1
          do (emit-tokens emitter item collector)
             (when (< i (length contents))
               (collect-token collector (make-separator ","))
               (collect-token collector (make-newline))))
    
    ;; Close with proper formatting
    (when contents
      (collect-token collector (make-newline))
      (collect-token collector (make-dedent 1)))
    
    (collect-token collector (make-close "}"))))

(defmethod emit-tokens ((emitter compact-json-emitter) (ir ir-pair) collector)
  (emit-tokens emitter (key ir) collector)
  (collect-token collector (make-text ":"))
  (emit-tokens emitter (value ir) collector))

(defmethod emit-tokens ((emitter pretty-json-emitter) (ir ir-pair) collector)
  (emit-tokens emitter (key ir) collector)
  (collect-token collector (make-text ": "))
  (emit-tokens emitter (value ir) collector))

(defmethod emit-tokens ((emitter compact-json-emitter) (ir ir-named-object) collector)
  (collect-token collector (make-text (name ir)))
  (collect-token collector (make-open "(" :object))
  
  (let ((attrs (attributes ir)))
    (loop for (attr-name . attr-value) in attrs
          for i from 1
          do (collect-token collector (make-text (format nil "~a=" attr-name)))
             (emit-tokens emitter attr-value collector)
             (when (< i (length attrs))
               (collect-token collector (make-separator ",")))))
  
  (collect-token collector (make-close ")")))

(defmethod emit-tokens ((emitter pretty-json-emitter) (ir ir-named-object) collector)
  (collect-token collector (make-text (name ir)))
  (collect-token collector (make-open "(" :object))
  
  (let ((attrs (attributes ir)))
    (loop for (attr-name . attr-value) in attrs
          for i from 1
          do (collect-token collector (make-text (format nil "~a=" attr-name)))
             (emit-tokens emitter attr-value collector)
             (when (< i (length attrs))
               (collect-token collector (make-separator ", ")))))
  
  (collect-token collector (make-close ")")))


(defun generate-tokens-from-ir (ir &optional (emitter (make-pretty-json-emitter)))
  "Convert IR to a sequence of tokens using the specified emitter"
  (let ((collector (make-instance 'token-collector)))
    (emit-tokens emitter ir collector)
    (finalize-tokens collector)))

(defun format-object (stream object
                      &key (indent-width 2) (line-limit 80) (format-style :expanded))
  "Convert a Common Lisp object to IR, generate tokens, and format
   FORMAT-STYLE can be :expanded (uses newlines and indentation) or :compact (single line)"
  (let* ((emitter (case format-style
                    (:compact (make-compact-json-emitter))
                    (:expanded (make-pretty-json-emitter :indent-width indent-width))
                    (otherwise (make-pretty-json-emitter :indent-width indent-width))))
         (ir (to-ir object))
         (tokens (generate-tokens-from-ir ir emitter)))
    (format-tokens tokens stream 
                   :indent-width indent-width 
                   :line-limit line-limit)
    object))

(defun tokenize (object
                 &key (indent-width 2) (format-style :expanded))
  "Convert a Common Lisp object to IR, generate tokens, and format
   FORMAT-STYLE can be :expanded (uses newlines and indentation) or :compact (single line)"
  (let* ((emitter (case format-style
                    (:compact (make-compact-json-emitter))
                    (:expanded (make-pretty-json-emitter :indent-width indent-width))
                    (otherwise (make-pretty-json-emitter :indent-width indent-width))))
         (ir (to-ir object)))
    (generate-tokens-from-ir ir emitter)))
