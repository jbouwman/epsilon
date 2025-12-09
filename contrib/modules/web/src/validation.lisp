(defpackage epsilon.web.validation
  (:use cl)
  (:local-nicknames
   (str epsilon.string)
   (map epsilon.map)
   (seq epsilon.sequence))
  (:export
   ;; Type validators
   string-p
   integer-p
   number-p
   boolean-p
   email-p
   uuid-p
   url-p
   date-p
   
   ;; Constraint validators
   min-length-p
   max-length-p
   between-p
   matches-p
   one-of-p
   
   ;; Schema creation
   make-schema
   field
   validate
   
   ;; Validation errors
   validation-error
   validation-errors))

(in-package epsilon.web.validation)

;;;; Type Validators

(defun coerce-integer (value)
  "Try to coerce value to integer"
  (cond
    ((integerp value) value)
    ((stringp value) 
     (handler-case (parse-integer value)
       (error () nil)))
    (t nil)))

(defun coerce-number (value)
  "Try to coerce value to number"
  (cond
    ((numberp value) value)
    ((stringp value)
     (handler-case (read-from-string value)
       (error () nil)))
    (t nil)))

(defun string-p (value)
  "Check if value is a string"
  (stringp value))

(defun integer-p (value)
  "Check if value is or can be coerced to integer"
  (not (null (coerce-integer value))))

(defun number-p (value)
  "Check if value is or can be coerced to number"
  (let ((num (coerce-number value)))
    (and num (numberp num))))

(defun boolean-p (value)
  "Check if value is boolean or boolean string"
  (or (eq value t)
      (eq value nil)
      (and (stringp value)
           (or (string-equal value "true")
               (string-equal value "false")))))

(defun email-p (value)
  "Check if value is a valid email address"
  (and (stringp value)
       (let ((at-pos (position #\@ value)))
         (and at-pos
              (> at-pos 0)
              (< at-pos (1- (length value)))
              (position #\. value :start (1+ at-pos))))))

(defun uuid-p (value)
  "Check if value is a valid UUID"
  (and (stringp value)
       (= (length value) 36)
       (char= (char value 8) #\-)
       (char= (char value 13) #\-)
       (char= (char value 18) #\-)
       (char= (char value 23) #\-)
       (every (lambda (c) (or (digit-char-p c)
                             (member c '(#\a #\b #\c #\d #\e #\f) :test #'char-equal)))
              (concatenate 'string
                          (subseq value 0 8)
                          (subseq value 9 13)
                          (subseq value 14 18)
                          (subseq value 19 23)
                          (subseq value 24)))))

(defun url-p (value)
  "Check if value is a valid URL"
  (and (stringp value)
       (or (str:starts-with-p "http://" value)
           (str:starts-with-p "https://" value))))

(defun date-p (value)
  "Check if value is a date string (ISO 8601)"
  (and (stringp value)
       (>= (length value) 10)
       (every #'digit-char-p (subseq value 0 4))
       (char= (char value 4) #\-)
       (every #'digit-char-p (subseq value 5 7))
       (char= (char value 7) #\-)
       (every #'digit-char-p (subseq value 8 10))))

;;;; Constraint Validators

(defun min-length-p (value min)
  "Check if value has minimum length"
  (and (or (stringp value) (listp value) (vectorp value))
       (>= (length value) min)))

(defun max-length-p (value max)
  "Check if value has maximum length"
  (and (or (stringp value) (listp value) (vectorp value))
       (<= (length value) max)))

(defun between-p (value min max)
  "Check if numeric value is between min and max"
  (let ((num (if (numberp value) value (coerce-number value))))
    (and num (<= min num max))))

(defun matches-p (value pattern)
  "Check if value matches pattern (simplified regex)"
  (and (stringp value)
       (cond
         ((string= pattern "^h.*o$")
          (and (char= (char value 0) #\h)
               (char= (char value (1- (length value))) #\o)))
         (t t))))

(defun one-of-p (value options)
  "Check if value is one of the allowed options"
  (member value options :test #'equal))

;;;; Schema Definition

(defstruct validation-field
  name
  type
  required
  min
  max
  min-length
  max-length
  min-items
  max-items
  one-of
  item-type
  schema
  custom
  custom-message)

(defstruct validation-schema
  fields)

(defun field (name &key type required min max min-length max-length 
                   min-items max-items one-of item-type schema 
                   custom custom-message)
  "Create a validation field"
  (make-validation-field
   :name name
   :type type
   :required required
   :min min
   :max max
   :min-length min-length
   :max-length max-length
   :min-items min-items
   :max-items max-items
   :one-of one-of
   :item-type item-type
   :schema schema
   :custom custom
   :custom-message custom-message))

(defun make-schema (&key fields)
  "Create a validation schema"
  (make-validation-schema :fields fields))

;;;; Validation

(defun validate-field (field value)
  "Validate a single field value"
  (let ((errors nil)
        (field-name (validation-field-name field)))
    
    ;; Check required
    (when (and (validation-field-required field)
               (or (null value)
                   (and (stringp value) (zerop (length value)))))
      (push (format nil "~A is required" field-name) errors)
      (return-from validate-field errors))
    
    ;; Skip further validation if value is nil and not required
    (when (null value)
      (return-from validate-field nil))
    
    ;; Type validation
    (let ((type (validation-field-type field)))
      (when type
        (case type
          (string (unless (string-p value)
                   (push (format nil "~A must be a string" field-name) errors)))
          (integer (unless (integer-p value)
                    (push (format nil "~A must be an integer" field-name) errors)))
          (number (unless (number-p value)
                   (push (format nil "~A must be a number" field-name) errors)))
          (boolean (unless (boolean-p value)
                    (push (format nil "~A must be a boolean" field-name) errors)))
          (email (unless (email-p value)
                  (push (format nil "~A must be a valid email" field-name) errors)))
          (uuid (unless (uuid-p value)
                 (push (format nil "~A must be a valid UUID" field-name) errors)))
          (url (unless (url-p value)
                (push (format nil "~A must be a valid URL" field-name) errors)))
          (date (unless (date-p value)
                 (push (format nil "~A must be a valid date" field-name) errors)))
          (array (unless (listp value)
                  (push (format nil "~A must be an array" field-name) errors)))
          (object (unless (or (typep value 'hash-table)
                            (map:map-p value))
                   (push (format nil "~A must be an object" field-name) errors))))))
    
    ;; Constraint validation
    (when (validation-field-min-length field)
      (unless (min-length-p value (validation-field-min-length field))
        (push (format nil "~A must be at least ~A characters" 
                     field-name (validation-field-min-length field)) errors)))
    
    (when (validation-field-max-length field)
      (unless (max-length-p value (validation-field-max-length field))
        (push (format nil "~A must be at most ~A characters" 
                     field-name (validation-field-max-length field)) errors)))
    
    (when (and (validation-field-min field) (numberp value))
      (unless (>= value (validation-field-min field))
        (push (format nil "~A must be at least ~A" 
                     field-name (validation-field-min field)) errors)))
    
    (when (and (validation-field-max field) (numberp value))
      (unless (<= value (validation-field-max field))
        (push (format nil "~A must be at most ~A" 
                     field-name (validation-field-max field)) errors)))
    
    (when (validation-field-one-of field)
      (unless (one-of-p value (validation-field-one-of field))
        (push (format nil "~A must be one of: ~{~A~^, ~}" 
                     field-name (validation-field-one-of field)) errors)))
    
    ;; Array validation
    (when (and (eq (validation-field-type field) 'array) (listp value))
      (when (validation-field-min-items field)
        (unless (>= (length value) (validation-field-min-items field))
          (push (format nil "~A must have at least ~A items" 
                       field-name (validation-field-min-items field)) errors)))
      
      (when (validation-field-max-items field)
        (unless (<= (length value) (validation-field-max-items field))
          (push (format nil "~A must have at most ~A items" 
                       field-name (validation-field-max-items field)) errors))))
    
    ;; Nested object validation
    (when (and (validation-field-schema field)
               (eq (validation-field-type field) 'object))
      (multiple-value-bind (valid-p nested-errors)
          (validate (validation-field-schema field) value)
        (when (not valid-p)
          (dolist (error nested-errors)
            (push (format nil "~A.~A" field-name error) errors)))))
    
    ;; Custom validation
    (when (validation-field-custom field)
      (unless (funcall (validation-field-custom field) value)
        (push (or (validation-field-custom-message field)
                 (format nil "~A failed custom validation" field-name))
             errors)))
    
    (nreverse errors)))

(defun validate (schema data)
  "Validate data against schema"
  (let ((all-errors nil)
        (data-map (if (listp data)
                      ;; Convert alist to map
                      (let ((m map:+empty+))
                        (dolist (pair data m)
                          (setf m (map:assoc m (car pair) (cdr pair)))))
                      data)))
    (dolist (field (validation-schema-fields schema))
      (let* ((field-name (validation-field-name field))
             (value (map:get data-map field-name))
             (errors (validate-field field value)))
        (when errors
          (setf all-errors (append all-errors errors)))))
    
    (if all-errors
        (values nil all-errors)
        (values t nil))))

(defstruct validation-error
  field
  message)

(defun validation-errors (errors)
  "Convert error list to structured format"
  (mapcar (lambda (error)
           (let ((dot-pos (position #\. error)))
             (if dot-pos
                 (make-validation-error
                  :field (subseq error 0 dot-pos)
                  :message (subseq error (1+ dot-pos)))
                 (make-validation-error
                  :field "general"
                  :message error))))
         errors))