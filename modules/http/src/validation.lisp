;;;; HTTP Input Validation
;;;;
;;;; Request validation and sanitization utilities

(defpackage :epsilon.http.validation
  (:use :cl)
  (:local-nicknames
   (#:request #:epsilon.http.request)
   (#:response #:epsilon.http.response)
   (#:map #:epsilon.map)
   (#:str #:epsilon.string))
  (:export
   ;; Validators
   #:make-validator
   #:validate
   #:validation-error
   #:validation-errors
   
   ;; Common validators
   #:required
   #:string-type
   #:integer-type
   #:float-type
   #:boolean-type
   #:email
   #:url
   #:uuid
   #:length-between
   #:min-length
   #:max-length
   #:in-range
   #:one-of
   #:matches
   #:custom
   
   ;; Request validation
   #:validate-request-params
   #:validate-request-body
   #:validate-json-body
   #:validation-middleware
   
   ;; Sanitizers
   #:trim
   #:lowercase
   #:uppercase
   #:remove-html
   #:escape-html
   #:normalize-email))

(in-package :epsilon.http.validation)

;;;; Validation Errors

(define-condition validation-error (error)
  ((field :initarg :field :reader validation-error-field)
   (message :initarg :message :reader validation-error-message)
   (value :initarg :value :reader validation-error-value))
  (:report (lambda (condition stream)
             (format stream "Validation error for ~A: ~A"
                     (validation-error-field condition)
                     (validation-error-message condition)))))

(defstruct validation-result
  "Result of validation"
  (valid-p t :type boolean)
  (errors '() :type list)
  (values (map:make-map)))

;;;; Validator Functions

(defun required (field value)
  "Validate that field is present and not empty"
  (cond
    ((null value) 
     (error 'validation-error :field field :message "is required" :value value))
    ((and (stringp value) (string= value ""))
     (error 'validation-error :field field :message "cannot be empty" :value value))
    (t value)))

(defun string-type (field value)
  "Validate that value is a string"
  (if (stringp value)
      value
      (error 'validation-error :field field :message "must be a string" :value value)))

(defun integer-type (field value)
  "Validate and convert to integer"
  (cond
    ((integerp value) value)
    ((stringp value)
     (handler-case
         (parse-integer value)
       (error ()
         (error 'validation-error :field field :message "must be an integer" :value value))))
    (t (error 'validation-error :field field :message "must be an integer" :value value))))

(defun float-type (field value)
  "Validate and convert to float"
  (cond
    ((floatp value) value)
    ((integerp value) (float value))
    ((stringp value)
     (handler-case
         (read-from-string value)
       (error ()
         (error 'validation-error :field field :message "must be a number" :value value))))
    (t (error 'validation-error :field field :message "must be a number" :value value))))

(defun boolean-type (field value)
  "Validate and convert to boolean"
  (cond
    ((eq value t) t)
    ((eq value nil) nil)
    ((stringp value)
     (cond
       ((member value '("true" "1" "yes" "on") :test #'string-equal) t)
       ((member value '("false" "0" "no" "off") :test #'string-equal) nil)
       (t (error 'validation-error :field field :message "must be a boolean" :value value))))
    (t (error 'validation-error :field field :message "must be a boolean" :value value))))

(defun email (field value)
  "Validate email format"
  (unless (and (stringp value)
               ;; Simple email validation - check for @ and .
               (find #\@ value)
               (find #\. value)
               (> (length value) 5))
    (error 'validation-error :field field :message "must be a valid email" :value value))
  value)

(defun url (field value)
  "Validate URL format"
  (unless (and (stringp value)
               (or (str:starts-with-p value "http://")
                   (str:starts-with-p value "https://"))
               (> (length value) 7))
    (error 'validation-error :field field :message "must be a valid URL" :value value))
  value)

(defun uuid (field value)
  "Validate UUID format"
  (unless (and (stringp value)
               (= (length value) 36)
               (char= (char value 8) #\-)
               (char= (char value 13) #\-)
               (char= (char value 18) #\-)
               (char= (char value 23) #\-))
    (error 'validation-error :field field :message "must be a valid UUID" :value value))
  value)

(defun length-between (min max)
  "Create validator for string length"
  (lambda (field value)
    (let ((len (length value)))
      (unless (<= min len max)
        (error 'validation-error 
               :field field 
               :message (format nil "must be between ~D and ~D characters" min max)
               :value value)))
    value))

(defun min-length (min)
  "Create validator for minimum length"
  (lambda (field value)
    (unless (>= (length value) min)
      (error 'validation-error 
             :field field 
             :message (format nil "must be at least ~D characters" min)
             :value value))
    value))

(defun max-length (max)
  "Create validator for maximum length"
  (lambda (field value)
    (unless (<= (length value) max)
      (error 'validation-error 
             :field field 
             :message (format nil "must be at most ~D characters" max)
             :value value))
    value))

(defun in-range (min max)
  "Create validator for numeric range"
  (lambda (field value)
    (let ((num (if (numberp value) value (float-type field value))))
      (unless (<= min num max)
        (error 'validation-error 
               :field field 
               :message (format nil "must be between ~A and ~A" min max)
               :value value)))
    value))

(defun one-of (options)
  "Create validator for allowed values"
  (lambda (field value)
    (unless (member value options :test #'equal)
      (error 'validation-error 
             :field field 
             :message (format nil "must be one of: ~{~A~^, ~}" options)
             :value value))
    value))

(defun matches (pattern)
  "Create validator for simple pattern matching"
  (lambda (field value)
    (unless (and (stringp value)
                 (search pattern value))
      (error 'validation-error 
             :field field 
             :message "has invalid format"
             :value value))
    value))

(defun custom (predicate message)
  "Create custom validator"
  (lambda (field value)
    (unless (funcall predicate value)
      (error 'validation-error :field field :message message :value value))
    value))

;;;; Validator Composition

(defun make-validator (rules)
  "Create a validator from a set of rules"
  (lambda (data)
    (let ((result (make-validation-result)))
      (map:each (lambda (field validators)
                  (let ((final-value (map:get data field)))
                    (dolist (validator (if (listp validators) validators (list validators)))
                      (handler-case
                          (setf final-value (funcall validator field final-value))
                        (validation-error (e)
                          (setf (validation-result-valid-p result) nil)
                          (push e (validation-result-errors result)))))
                    (when (validation-result-valid-p result)
                      (setf (validation-result-values result)
                            (map:assoc (validation-result-values result) field final-value)))))
                rules)
      result)))

(defun validate (validator data)
  "Validate data with validator"
  (funcall validator data))

;;;; Request Validation

(defun validate-request-params (rules)
  "Middleware to validate request parameters"
  (let ((validator (make-validator rules)))
    (lambda (handler)
      (lambda (request)
        (let ((result (validate validator (request:request-params request))))
          (if (validation-result-valid-p result)
              ;; Update request with validated values
              (let ((new-request (copy-structure request)))
                (setf (request:request-params new-request)
                      (validation-result-values result))
                (funcall handler new-request))
              ;; Return validation errors
              (response:json-response
               (map:make-map
                "errors" (mapcar (lambda (e)
                                   (map:make-map
                                    "field" (validation-error-field e)
                                    "message" (validation-error-message e)))
                                 (validation-result-errors result)))
               :status 400)))))))

(defun validate-request-body (rules content-type)
  "Middleware to validate request body"
  (lambda (handler)
    (lambda (request)
      (let* ((body (request:request-body request))
             (body-data
              (cond
                ((string= content-type "application/json")
                 ;; Parse JSON body
                 (handler-case
                     (if (and body (not (str:empty-p body)))
                         (epsilon.json:parse body)
                         (map:make-map))
                   (error ()
                     (return-from validate-request-body
                       (response:text-response "Invalid JSON" :status 400)))))
                ((string= content-type "application/x-www-form-urlencoded")
                 ;; Parse form data
                 (request:parse-form-data body))
                (t (map:make-map)))))
        
        (let* ((validator (make-validator rules))
               (result (validate validator body-data)))
          (if (validation-result-valid-p result)
              ;; Update request with validated body
              (let ((new-request (copy-structure request)))
                (setf (slot-value new-request 'validated-body)
                      (validation-result-values result))
                (funcall handler new-request))
              ;; Return validation errors
              (response:json-response
               (map:make-map
                "errors" (mapcar (lambda (e)
                                   (map:make-map
                                    "field" (validation-error-field e)
                                    "message" (validation-error-message e)))
                                 (validation-result-errors result)))
               :status 400)))))))

;;;; Sanitizers

(defun trim (value)
  "Trim whitespace from string"
  (if (stringp value)
      (str:trim value)
      value))

(defun lowercase (value)
  "Convert string to lowercase"
  (if (stringp value)
      (string-downcase value)
      value))

(defun uppercase (value)
  "Convert string to uppercase"  
  (if (stringp value)
      (string-upcase value)
      value))

(defun remove-html (value)
  "Remove HTML tags from string"
  (if (stringp value)
      ;; Simple HTML tag removal
      (let ((result "")
            (in-tag nil))
        (dotimes (i (length value))
          (let ((char (char value i)))
            (cond
              ((char= char #\<) (setf in-tag t))
              ((char= char #\>) (setf in-tag nil))
              ((not in-tag) (setf result (concatenate 'string result (string char)))))))
        result)
      value))

(defun escape-html (value)
  "Escape HTML special characters"
  (if (stringp value)
      (let ((result value))
        (setf result (str:replace-all result "&" "&amp;"))
        (setf result (str:replace-all result "<" "&lt;"))
        (setf result (str:replace-all result ">" "&gt;"))
        (setf result (str:replace-all result "\"" "&quot;"))
        (setf result (str:replace-all result "'" "&#39;"))
        result)
      value))

(defun normalize-email (value)
  "Normalize email address"
  (when (stringp value)
    (string-downcase (trim value))))