;;;; Keyring Management
;;;;
;;;; This module provides keyring functionality for managing trusted
;;;; and revoked keys.

(defpackage :epsilon.crypto.keyring
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:str #:epsilon.string)
   (#:keys #:epsilon.crypto.keys))
  (:export
   ;; Keyring structure
   #:keyring
   #:make-keyring
   #:keyring-trusted-keys
   #:keyring-revoked-keys
   #:keyring-default-key
   
   ;; Keyring operations
   #:create-keyring
   #:add-trusted-key
   #:remove-trusted-key
   #:find-public-key
   #:list-trusted-keys
   
   ;; Key revocation
   #:revoke-key
   #:key-revoked-p
   #:list-revoked-keys
   
   ;; Keyring persistence
   #:save-keyring
   #:load-keyring
   #:merge-keyrings
   
   ;; Global keyring
   #:*default-keyring*
   #:initialize-default-keyring))

(in-package :epsilon.crypto.keyring)

;;;; Utility Macros

(defmacro when-let (bindings &body body)
  "Bind variables in BINDINGS and execute BODY if all values are non-nil"
  (let ((var (caar bindings))
        (value (cadar bindings)))
    `(let ((,var ,value))
       (when ,var ,@body))))

;;;; Keyring Structure

(defstruct keyring
  "Container for managing cryptographic keys"
  (trusted-keys (map:make-map) :type map:map)
  (revoked-keys (map:make-map) :type map:map)
  default-key)

;;;; Global Keyring

(defparameter *default-keyring* nil
  "Default keyring for package verification")

(defun initialize-default-keyring ()
  "Initialize the default keyring"
  (setf *default-keyring* (create-keyring)))

;;;; Keyring Operations

(defun create-keyring ()
  "Create a new empty keyring"
  (make-keyring))

(defun add-trusted-key (keyring public-key key-id &key (trust-level :full) metadata)
  "Add a trusted public key to keyring"
  (when (key-revoked-p keyring key-id)
    (error "Cannot add revoked key ~A to trusted keys" key-id))
  
  (let ((key-info (map:make-map
                   :public-key public-key
                   :key-id key-id
                   :trust-level trust-level
                   :added (get-universal-time)
                   :metadata (or metadata (map:make-map)))))
    (setf (keyring-trusted-keys keyring)
          (map:assoc (keyring-trusted-keys keyring) key-id key-info))
    key-info))

(defun remove-trusted-key (keyring key-id)
  "Remove a key from the trusted keys"
  (setf (keyring-trusted-keys keyring)
        (map:dissoc (keyring-trusted-keys keyring) key-id)))

(defun find-public-key (key-id &optional (keyring *default-keyring*))
  "Find public key in keyring by key ID"
  (when keyring
    (when-let ((key-info (map:get (keyring-trusted-keys keyring) key-id)))
      (when (not (key-revoked-p keyring key-id))
        (map:get key-info :public-key)))))

(defun list-trusted-keys (keyring)
  "List all trusted key IDs"
  (let ((key-ids '()))
    (map:each (lambda (key-id key-info)
                (unless (key-revoked-p keyring key-id)
                  (push key-id key-ids)))
              (keyring-trusted-keys keyring))
    (nreverse key-ids)))

;;;; Key Revocation

(defun revoke-key (keyring key-id &key (reason "No reason provided") revoked-by)
  "Revoke a key in the keyring"
  (let ((revocation-info (map:make-map
                          :key-id key-id
                          :revoked (get-universal-time)
                          :reason reason
                          :revoked-by (or revoked-by "Unknown"))))
    (setf (keyring-revoked-keys keyring)
          (map:assoc (keyring-revoked-keys keyring) key-id revocation-info))
    revocation-info))

(defun key-revoked-p (keyring key-id)
  "Check if a key is revoked"
  (map:contains-p (keyring-revoked-keys keyring) key-id))

(defun list-revoked-keys (keyring)
  "List all revoked key IDs"
  (map:keys (keyring-revoked-keys keyring)))

;;;; Keyring Persistence

(defun save-keyring (keyring filepath)
  "Save keyring to file"
  (with-open-file (out filepath :direction :output :if-exists :supersede)
    (format out "# Epsilon Keyring~%")
    (format out "# Saved: ~A~%~%" (format-timestamp (get-universal-time)))
    
    ;; Save trusted keys
    (format out "## Trusted Keys~%")
    (map:each (lambda (key-id key-info)
                (format out "### Key: ~A~%" key-id)
                (format out "Trust-Level: ~A~%" (map:get key-info :trust-level))
                (format out "Added: ~A~%" (format-timestamp (map:get key-info :added)))
                (format out "Public-Key:~%~A~%~%" (map:get key-info :public-key)))
              (keyring-trusted-keys keyring))
    
    ;; Save revoked keys
    (format out "## Revoked Keys~%")
    (map:each (lambda (key-id revocation-info)
                (format out "### Key: ~A~%" key-id)
                (format out "Revoked: ~A~%" (format-timestamp (map:get revocation-info :revoked)))
                (format out "Reason: ~A~%" (map:get revocation-info :reason))
                (format out "Revoked-By: ~A~%~%" (map:get revocation-info :revoked-by)))
              (keyring-revoked-keys keyring))
    
    ;; Save default key
    (when (keyring-default-key keyring)
      (format out "## Default Key~%")
      (format out "~A~%" (keyring-default-key keyring)))))

(defun load-keyring (filepath)
  "Load keyring from file"
  (unless (fs:exists-p filepath)
    (error "Keyring file not found: ~A" filepath))
  
  (let ((keyring (create-keyring))
        (content (fs:read-file filepath)))
    ;; Simple parser - in production would use more robust format
    (parse-keyring-content keyring content)
    keyring))

(defun parse-keyring-content (keyring content)
  "Parse keyring file content"
  ;; Simplified implementation - would need proper parser
  (let ((lines (str:split #\Newline content))
        (current-section nil)
        (current-key-id nil)
        (current-data (map:make-map)))
    
    (dolist (line lines)
      (cond
        ((str:starts-with-p line "## Trusted Keys")
         (setf current-section :trusted))
        ((str:starts-with-p line "## Revoked Keys")
         (setf current-section :revoked))
        ((str:starts-with-p line "## Default Key")
         (setf current-section :default))
        ((str:starts-with-p line "### Key:")
         (when (and current-key-id current-data)
           (process-parsed-key keyring current-section current-key-id current-data))
         (setf current-key-id (str:strip (subseq line 8) #\Space))
         (setf current-data (map:make-map)))
        ((and current-section (str:contains-p line ":"))
         (let ((colon-pos (position #\: line)))
           (when colon-pos
             (let ((key (str:strip (subseq line 0 colon-pos) #\Space))
                   (value (str:strip (subseq line (1+ colon-pos)) #\Space)))
               (setf current-data (map:assoc current-data key value))))))
        ((eq current-section :default)
         (setf (keyring-default-key keyring) (str:strip line #\Space)))))
    
    ;; Process last key if any
    (when (and current-key-id current-data)
      (process-parsed-key keyring current-section current-key-id current-data))))

(defun process-parsed-key (keyring section key-id data)
  "Process a parsed key entry"
  (case section
    (:trusted
     (when-let ((public-key (map:get data "Public-Key")))
       (add-trusted-key keyring public-key key-id
                       :trust-level (or (map:get data "Trust-Level") :full))))
    (:revoked
     (revoke-key keyring key-id
                :reason (or (map:get data "Reason") "Unknown")
                :revoked-by (or (map:get data "Revoked-By") "Unknown")))))

(defun merge-keyrings (keyring1 keyring2)
  "Merge two keyrings, with keyring2 taking precedence"
  (let ((merged (create-keyring)))
    ;; Copy trusted keys from keyring1
    (map:each (lambda (key-id key-info)
                (unless (key-revoked-p keyring2 key-id)
                  (setf (keyring-trusted-keys merged)
                        (map:assoc (keyring-trusted-keys merged) key-id key-info))))
              (keyring-trusted-keys keyring1))
    
    ;; Add/override with trusted keys from keyring2
    (map:each (lambda (key-id key-info)
                (setf (keyring-trusted-keys merged)
                      (map:assoc (keyring-trusted-keys merged) key-id key-info)))
              (keyring-trusted-keys keyring2))
    
    ;; Merge revoked keys
    (setf (keyring-revoked-keys merged)
          (map:merge (keyring-revoked-keys keyring1)
                     (keyring-revoked-keys keyring2)))
    
    ;; Use keyring2's default key if set
    (setf (keyring-default-key merged)
          (or (keyring-default-key keyring2)
              (keyring-default-key keyring1)))
    
    merged))

;;;; Utility Functions

(defun format-timestamp (universal-time)
  "Format timestamp for display"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

;;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (unless *default-keyring*
    (initialize-default-keyring)))