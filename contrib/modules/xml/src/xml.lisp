(defpackage :epsilon.xml
  (:use
   :cl
   :epsilon.syntax)
  (:local-nicknames
   (:re :epsilon.regex)
   (:str :epsilon.string))
  (:export
   :element
   :attribute
   :text
   :emit
   :xml-element
   :xml-text
   :xml-node
   :tag
   :attributes
   :children
   :content))

(in-package #:epsilon.xml)

(defun escape-xml (string)
  "Escape XML special characters in STRING"
  (->> string
       (re:sub "&" "&amp;")
       (re:sub "<" "&lt;")
       (re:sub ">" "&gt;")
       (re:sub "\"" "&quot;")
       (re:sub "'" "&apos;")))

(defclass xml-node ()
  ())

(defclass xml-element (xml-node)
  ((tag :initarg :tag :reader tag)
   (attributes :initarg :attributes :reader attributes :initform nil)
   (children :initarg :children)))

(defmethod children ((element xml-element))
  "Safe children accessor that returns nil for unbound slot"
  (if (slot-boundp element 'children)
      (slot-value element 'children)
      nil))

(defclass xml-text (xml-node)
  ((content :initarg :content :reader content)))

(defun element (tag &key attributes (children nil children-provided-p))
  "Create an XML element with TAG, optional ATTRIBUTES plist, and CHILDREN list"
  (let ((instance (make-instance 'xml-element 
                                 :tag tag 
                                 :attributes attributes)))
    ;; Set children slot if provided, otherwise leave it unbound for self-closing logic
    (when children-provided-p
      (setf (slot-value instance 'children) children))
    instance))

(defun attribute (name value)
  "Create an attribute pair for use in element attributes"
  (list name value))

(defun text (content)
  "Create XML text content"
  (make-instance 'xml-text :content content))

(defgeneric emit (node stream)
  (:documentation "Emit XML node to stream"))

(defparameter *self-closing-tags* 
  '(:area :base :br :col :embed :hr :img :input :link :meta :param :source :track :wbr)
  "HTML tags that should be self-closing")

(defmethod emit ((node xml-element) stream)
  (with-slots (tag attributes) node
    (let ((children-provided-p (slot-boundp node 'children))
          (children-value (when (slot-boundp node 'children) 
                           (slot-value node 'children))))
      (format stream "<~(~A~)" tag)  ; Use ~( ~) for lowercase
      (loop for (name value) on attributes by #'cddr
            when value
            do (format stream " ~(~A~)=\"~A\"" name (escape-xml (princ-to-string value))))
      (cond
        ;; Known HTML self-closing tags should always self-close
        ((member tag *self-closing-tags*)
         (format stream "/>"))
        ;; Tags with actual children content
        (children-value
         (format stream ">")
         (dolist (child children-value)
           (emit child stream))
         (format stream "</~(~A~)>" tag))
        ;; Tags with explicitly provided empty children - use full tags  
        (children-provided-p
         (format stream "></~(~A~)>" tag))
        ;; Tags with no children parameter - self-close
        (t
         (format stream "/>"))))))

(defmethod emit ((node xml-text) stream)
  (format stream "~A" (escape-xml (content node))))

(defmethod emit ((node string) stream)
  (format stream "~A" (escape-xml node)))

(defmethod emit ((node null) stream)
  (declare (ignore node stream)))
