(defpackage :epsilon.lib.xml
  (:use
   :cl
   :epsilon.lib.syntax)
  (:local-nicknames
   (:re :epsilon.lib.regex)
   (:str :epsilon.lib.string))
  (:export
   :element
   :attribute
   :text
   :emit))

(in-package #:epsilon.lib.xml)

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
   (children :initarg :children :reader children :initform nil)))

(defclass xml-text (xml-node)
  ((content :initarg :content :reader content)))

(defun element (tag &key attributes children)
  "Create an XML element with TAG, optional ATTRIBUTES plist, and CHILDREN list"
  (make-instance 'xml-element 
                 :tag tag 
                 :attributes attributes 
                 :children children))

(defun attribute (name value)
  "Create an attribute pair for use in element attributes"
  (list name value))

(defun text (content)
  "Create XML text content"
  (make-instance 'xml-text :content content))

(defgeneric emit (node stream)
  (:documentation "Emit XML node to stream"))

(defmethod emit ((node xml-element) stream)
  (with-slots (tag attributes children) node
    (format stream "<~A" tag)
    (loop for (name value) on attributes by #'cddr
          when value
          do (format stream " ~A=\"~A\"" name (escape-xml (princ-to-string value))))
    (if children
        (progn
          (format stream ">")
          (dolist (child children)
            (emit child stream))
          (format stream "</~A>~%" tag))
        (format stream "/>~%"))))

(defmethod emit ((node xml-text) stream)
  (format stream "~A" (escape-xml (content node))))

(defmethod emit ((node string) stream)
  (format stream "~A" (escape-xml node)))

(defmethod emit ((node null) stream)
  (declare (ignore node stream)))
