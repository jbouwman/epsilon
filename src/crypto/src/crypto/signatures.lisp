;;;; Package Signature Generation and Verification
;;;;
;;;; This module provides functionality for signing and verifying
;;;; Epsilon packages using Ed25519 signatures.

(defpackage :epsilon.crypto.signatures
  (:use :cl)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:str #:epsilon.string)
   (#:hex #:epsilon.hex)
   (#:digest #:epsilon.digest)
   (#:keys #:epsilon.crypto.keys)
   (#:keyring #:epsilon.crypto.keyring))
  (:export
   ;; Signing
   #:sign-package
   #:sign-file
   #:sign-data
   
   ;; Verification
   #:verify-package-signature
   #:verify-file-signature
   #:verify-data-signature
   
   ;; Signature format
   #:signature-info
   #:make-signature-info
   #:signature-info-algorithm
   #:signature-info-key-id
   #:signature-info-signature
   #:signature-info-hash
   #:signature-info-timestamp
   #:signature-info-version
   
   ;; Signature files
   #:write-signature-file
   #:read-signature-file))

(in-package :epsilon.crypto.signatures)

;;;; Signature Information Structure

(defstruct signature-info
  "Container for signature metadata"
  algorithm
  key-id
  signature
  hash
  timestamp
  version
  metadata)

;;;; Package Signing

(defun sign-package (package-path keypair &key (output-path nil) metadata)
  "Sign an Epsilon package file with Ed25519 signature"
  (let* ((package-hash (calculate-file-hash package-path))
         (signature (sign-data package-hash keypair))
         (sig-info (make-signature-info
                    :algorithm (keys:keypair-algorithm keypair)
                    :key-id (keys:keypair-key-id keypair)
                    :signature signature
                    :hash package-hash
                    :timestamp (get-universal-time)
                    :version "1.0"
                    :metadata (or metadata (map:make-map)))))
    
    ;; Write signature to file
    (let ((sig-path (or output-path 
                       (make-signature-path package-path))))
      (write-signature-file sig-path sig-info)
      (values sig-path sig-info))))

(defun sign-file (file-path keypair)
  "Sign any file with Ed25519 signature"
  (sign-package file-path keypair))

(defun sign-data (data keypair)
  "Sign raw data with Ed25519 signature"
  (let ((private-key (keys:keypair-private-key keypair)))
    (cond
      ((openssl-available-p)
       (sign-with-openssl data private-key))
      ((epsilon-crypto-available-p)
       (sign-with-epsilon-crypto data private-key))
      (t
       (sign-with-placeholder data private-key)))))

;;;; Package Verification

(defun verify-package-signature (package-path signature-path &key (keyring keyring:*default-keyring*))
  "Verify package signature"
  (let* ((sig-info (read-signature-file signature-path))
         (key-id (signature-info-key-id sig-info))
         (public-key (keyring:find-public-key key-id keyring)))
    
    (unless public-key
      (error "Public key not found for key ID: ~A" key-id))
    
    ;; Calculate current package hash
    (let ((current-hash (calculate-file-hash package-path))
          (stored-hash (signature-info-hash sig-info)))
      
      ;; Verify hash matches
      (unless (string= current-hash stored-hash)
        (error "Package hash mismatch: expected ~A, got ~A" 
               stored-hash current-hash))
      
      ;; Verify signature
      (verify-data-signature (signature-info-signature sig-info)
                            current-hash
                            public-key))))

(defun verify-file-signature (file-path signature-path &key (keyring keyring:*default-keyring*))
  "Verify any file signature"
  (verify-package-signature file-path signature-path :keyring keyring))

(defun verify-data-signature (signature data public-key)
  "Verify raw data signature"
  (let ((valid-p (cond
                   ((openssl-available-p)
                    (verify-with-openssl signature data public-key))
                   ((epsilon-crypto-available-p)
                    (verify-with-epsilon-crypto signature data public-key))
                   (t
                    (verify-with-placeholder signature data public-key)))))
    (unless valid-p
      (error "Signature verification failed"))
    valid-p))

;;;; Hash Calculation

(defun calculate-file-hash (file-path)
  "Calculate SHA-256 hash of file"
  (let ((path (if (pathnamep file-path)
                 file-path
                 (path:make-path file-path))))
    (digest:sha256 (fs:read-file-bytes path))))

;;;; OpenSSL Integration

(defun openssl-available-p ()
  "Check if OpenSSL is available"
  (handler-case
      (zerop (sb-ext:process-exit-code
              (sb-ext:run-program "openssl" '("version")
                                 :search t
                                 :output nil
                                 :error nil)))
    (error () nil)))

(defun sign-with-openssl (data private-key)
  "Sign data using OpenSSL"
  (with-temp-files ((data-file data)
                    (key-file private-key))
    (let ((sig-file (path:make-temp-path :prefix "signature")))
      (unwind-protect
           (progn
             (run-openssl-command
              (format nil "pkeyutl -sign -inkey ~A -in ~A -out ~A"
                      (path:path-string key-file)
                      (path:path-string data-file)
                      (path:path-string sig-file)))
             (hex:encode (fs:read-file-bytes sig-file)))
        (ignore-errors (fs:delete-file sig-file))))))

(defun verify-with-openssl (signature data public-key)
  "Verify signature using OpenSSL"
  (with-temp-files ((sig-file (hex:decode signature))
                    (data-file data)
                    (key-file public-key))
    (zerop (run-openssl-command-status
            (format nil "pkeyutl -verify -pubin -inkey ~A -in ~A -sigfile ~A"
                    (path:path-string key-file)
                    (path:path-string data-file)
                    (path:path-string sig-file))))))

;;;; Epsilon Crypto Integration

(defun epsilon-crypto-available-p ()
  "Check if Epsilon crypto module is available"
  ;; Placeholder for future epsilon crypto implementation
  nil)

(defun sign-with-epsilon-crypto (data private-key)
  "Sign using Epsilon crypto module"
  (declare (ignore data private-key))
  (error "Epsilon crypto not yet implemented"))

(defun verify-with-epsilon-crypto (signature data public-key)
  "Verify using Epsilon crypto module"
  (declare (ignore signature data public-key))
  (error "Epsilon crypto not yet implemented"))

;;;; Placeholder Implementation

(defun sign-with-placeholder (data private-key)
  "Placeholder signing for testing"
  (let* ((data-string (if (stringp data) data (hex:encode data)))
         (combined (str:concat data-string private-key))
         (hash (digest:sha256 (map 'vector #'char-code combined))))
    (hex:encode hash)))

(defun verify-with-placeholder (signature data public-key)
  "Placeholder verification for testing"
  ;; In placeholder mode, we re-sign with the public key and compare
  (let ((expected (sign-with-placeholder data public-key)))
    (string= signature expected)))

;;;; Signature File I/O

(defun write-signature-file (filepath sig-info)
  "Write signature to file"
  (with-open-file (out filepath :direction :output :if-exists :supersede)
    (format out "{~%")
    (format out "  \"algorithm\": ~S,~%" 
            (string-downcase (symbol-name (signature-info-algorithm sig-info))))
    (format out "  \"keyId\": ~S,~%" (signature-info-key-id sig-info))
    (format out "  \"signature\": ~S,~%" (signature-info-signature sig-info))
    (format out "  \"hash\": ~S,~%" (signature-info-hash sig-info))
    (format out "  \"timestamp\": ~D,~%" (signature-info-timestamp sig-info))
    (format out "  \"version\": ~S" (signature-info-version sig-info))
    
    ;; Add metadata if present
    (when (and (signature-info-metadata sig-info)
               (> (map:count (signature-info-metadata sig-info)) 0))
      (format out ",~%  \"metadata\": {")
      (let ((first t))
        (map:each (lambda (k v)
                    (unless first (format out ","))
                    (format out "~%    ~S: ~S" (string k) v)
                    (setf first nil))
                  (signature-info-metadata sig-info)))
      (format out "~%  }"))
    
    (format out "~%}~%")))

(defun read-signature-file (filepath)
  "Read signature from file"
  (let ((content (fs:read-file-string filepath)))
    (parse-signature-json content)))

(defun parse-signature-json (content)
  "Parse signature JSON content"
  ;; Simple JSON parsing - in production would use epsilon.lib.json
  (make-signature-info
   :algorithm (intern (string-upcase (extract-json-string content "algorithm")) :keyword)
   :key-id (extract-json-string content "keyId")
   :signature (extract-json-string content "signature")
   :hash (extract-json-string content "hash")
   :timestamp (parse-integer (extract-json-string content "timestamp"))
   :version (extract-json-string content "version")
   :metadata (parse-json-metadata content)))

(defun extract-json-string (content field-name)
  "Extract string field from JSON content"
  (let* ((pattern (format nil "\"~A\"\\s*:\\s*\"([^\"]+)\"" field-name))
         (start (search (format nil "\"~A\"" field-name) content)))
    (when start
      (let* ((colon-pos (position #\: content :start start))
             (quote-start (when colon-pos 
                           (position #\" content :start (1+ colon-pos))))
             (quote-end (when quote-start 
                         (position #\" content :start (1+ quote-start)))))
        (when (and quote-start quote-end)
          (subseq content (1+ quote-start) quote-end))))))

(defun parse-json-metadata (content)
  "Parse metadata object from JSON"
  ;; Simplified - returns empty map for now
  (map:make-map))

;;;; Utility Functions

(defun make-signature-path (package-path)
  "Generate signature file path from package path"
  (path:make-path 
   (str:concat (path:path-string package-path) ".sig")))

(defmacro with-temp-files (bindings &body body)
  "Create temporary files with content, ensuring cleanup"
  (let ((temp-vars (mapcar (lambda (b) (gensym "TEMP")) bindings)))
    `(let ,(mapcar (lambda (var binding)
                    `(,var (write-temp-file ,(first binding) ,(second binding))))
                  temp-vars bindings)
       (unwind-protect
            (progn ,@body)
         ,@(mapcar (lambda (var)
                    `(ignore-errors (fs:delete-file ,var)))
                  temp-vars)))))

(defun write-temp-file (name content)
  "Write content to temporary file"
  (let ((path (path:make-temp-path :prefix (format nil "epsilon_~A" name))))
    (if (stringp content)
        (fs:write-file-string path content)
        (fs:write-file-bytes path content))
    path))

(defun run-openssl-command (args)
  "Run OpenSSL command and check for errors"
  (let ((process (sb-ext:run-program "openssl" 
                                    (str:split #\Space args)
                                    :output nil
                                    :error nil
                                    :search t)))
    (let ((exit-code (sb-ext:process-exit-code process)))
      (unless (zerop exit-code)
        (error "OpenSSL command failed with exit code ~D: openssl ~A" 
               exit-code args))
      exit-code)))

(defun run-openssl-command-status (args)
  "Run OpenSSL command and return exit status"
  (handler-case
      (run-openssl-command args)
    (error () 1)))
