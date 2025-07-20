#!/usr/bin/env epsilon
;;;; Epsilon Package Signing Key Management Tool
;;;; Command-line interface for managing cryptographic keys

(defpackage :epsilon.keytool
  (:use :cl)
  (:export :main))

(in-package :epsilon.keytool)

;;;; ==========================================================================
;;;; Command Line Interface
;;;; ==========================================================================

(defun main (&optional args)
  "Main entry point for keytool"
  (handler-case
      (let ((command (first args))
            (remaining-args (rest args)))
        (cond
          ((null command)
           (show-help))
          ((string= command "generate")
           (handle-generate remaining-args))
          ((string= command "list")
           (handle-list remaining-args))
          ((string= command "export")
           (handle-export remaining-args))
          ((string= command "import")
           (handle-import remaining-args))
          ((string= command "revoke")
           (handle-revoke remaining-args))
          ((string= command "verify")
           (handle-verify remaining-args))
          ((member command '("help" "-h" "--help"))
           (show-help))
          (t
           (format t "Unknown command: ~A~%" command)
           (show-help)
           (sb-ext:exit :code 1))))
    (error (e)
      (format t "Error: ~A~%" e)
      (sb-ext:exit :code 1))))

(defun show-help ()
  "Show help information"
  (format t "Epsilon Package Signing Key Management Tool~%")
  (format t "==========================================~%~%")
  (format t "Usage: epsilon keytool <command> [options]~%~%")
  (format t "Commands:~%")
  (format t "  generate [--id KEY-ID] [--output FILE]    Generate new signing keypair~%")
  (format t "  list [--keyring FILE]                     List keys in keyring~%")
  (format t "  export --key KEY-ID [--output FILE]       Export public key~%")
  (format t "  import --file FILE [--trust LEVEL]        Import public key~%")
  (format t "  revoke --key KEY-ID [--reason TEXT]       Revoke a key~%")
  (format t "  verify --package FILE --signature FILE    Verify package signature~%")
  (format t "  help                                      Show this help~%~%")
  (format t "Examples:~%")
  (format t "  epsilon keytool generate --id epsilon-release-2024~%")
  (format t "  epsilon keytool export --key epsilon-release-2024 --output pubkey.pem~%")
  (format t "  epsilon keytool verify --package app.epk --signature app.epk.sig~%~%"))

;;;; ==========================================================================
;;;; Command Handlers
;;;; ==========================================================================

(defun handle-generate (args)
  "Handle key generation command"
  (let ((key-id nil)
        (output-file nil)
        (algorithm :ed25519))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--id")
                  (setf key-id (pop args)))
                 ((string= arg "--output")
                  (setf output-file (pop args)))
                 ((string= arg "--algorithm")
                  (setf algorithm (intern (string-upcase (pop args)) :keyword)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    ;; Generate keypair
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((generate-fn (find-symbol "GENERATE-KEYPAIR" crypto-pkg))
             (save-fn (find-symbol "SAVE-KEYPAIR" crypto-pkg))
             (keypair (funcall generate-fn :algorithm algorithm :key-id key-id))
             (actual-key-id (getf keypair :key-id))
             (output-path (or output-file 
                             (format nil "~A.key" actual-key-id))))
        
        (funcall save-fn keypair output-path)
        
        (format t "Generated ~A keypair:~%" algorithm)
        (format t "  Key ID: ~A~%" actual-key-id)
        (format t "  Saved to: ~A~%" output-path)
        (format t "~%IMPORTANT: Keep the private key secure and backed up!~%")
        (format t "           Share only the public key for verification.~%"))))

(defun handle-list (args)
  "Handle key listing command"
  (let ((keyring-file nil))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--keyring")
                  (setf keyring-file (pop args)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    ;; List keys
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((keyring-path (or keyring-file 
                              (merge-pathnames ".epsilon/keyring.lisp" 
                                             (user-homedir-pathname))))
             (keyring (when (probe-file keyring-path)
                       (load-keyring keyring-path crypto-pkg))))
        
        (if keyring
            (list-keyring-keys keyring)
            (format t "No keyring found at: ~A~%" keyring-path))))))

(defun handle-export (args)
  "Handle public key export command"
  (let ((key-id nil)
        (output-file nil)
        (keyring-file nil))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--key")
                  (setf key-id (pop args)))
                 ((string= arg "--output")
                  (setf output-file (pop args)))
                 ((string= arg "--keyring")
                  (setf keyring-file (pop args)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    (unless key-id
      (error "Key ID required (--key KEY-ID)"))
    
    ;; Export key
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((keyring-path (or keyring-file 
                              (merge-pathnames ".epsilon/keyring.lisp" 
                                             (user-homedir-pathname))))
             (keyring (when (probe-file keyring-path)
                       (load-keyring keyring-path crypto-pkg)))
             (public-key (when keyring
                          (funcall (find-symbol "FIND-PUBLIC-KEY" crypto-pkg) 
                                  key-id keyring))))
        
        (unless public-key
          (error "Key not found: ~A" key-id))
        
        (let ((output-path (or output-file 
                              (format nil "~A.pub" key-id))))
          (with-open-file (out output-path :direction :output :if-exists :supersede)
            (format out "# Epsilon Public Key~%")
            (format out "# Key ID: ~A~%" key-id)
            (format out "# Exported: ~A~%~%" (format-timestamp (get-universal-time)))
            (format out "-----BEGIN EPSILON PUBLIC KEY-----~%")
            (format out "~A~%" public-key)
            (format out "-----END EPSILON PUBLIC KEY-----~%"))
          
          (format t "Exported public key ~A to: ~A~%" key-id output-path))))))

(defun handle-import (args)
  "Handle public key import command"
  (let ((import-file nil)
        (trust-level :partial)
        (keyring-file nil))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--file")
                  (setf import-file (pop args)))
                 ((string= arg "--trust")
                  (setf trust-level (intern (string-upcase (pop args)) :keyword)))
                 ((string= arg "--keyring")
                  (setf keyring-file (pop args)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    (unless import-file
      (error "Import file required (--file FILE)"))
    
    ;; Import key
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((keyring-path (or keyring-file 
                              (merge-pathnames ".epsilon/keyring.lisp" 
                                             (user-homedir-pathname))))
             (keyring (or (when (probe-file keyring-path)
                           (load-keyring keyring-path crypto-pkg))
                         (funcall (find-symbol "CREATE-KEYRING" crypto-pkg))))
             (key-data (import-public-key import-file)))
        
        (funcall (find-symbol "ADD-TRUSTED-KEY" crypto-pkg)
                keyring 
                (getf key-data :public-key)
                (getf key-data :key-id)
                :trust-level trust-level)
        
        (save-keyring keyring keyring-path)
        
        (format t "Imported public key:~%")
        (format t "  Key ID: ~A~%" (getf key-data :key-id))
        (format t "  Trust level: ~A~%" trust-level)
        (format t "  Saved to keyring: ~A~%" keyring-path))))

(defun handle-revoke (args)
  "Handle key revocation command"
  (let ((key-id nil)
        (reason nil)
        (keyring-file nil))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--key")
                  (setf key-id (pop args)))
                 ((string= arg "--reason")
                  (setf reason (pop args)))
                 ((string= arg "--keyring")
                  (setf keyring-file (pop args)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    (unless key-id
      (error "Key ID required (--key KEY-ID)"))
    
    ;; Revoke key
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((keyring-path (or keyring-file 
                              (merge-pathnames ".epsilon/keyring.lisp" 
                                             (user-homedir-pathname))))
             (keyring (when (probe-file keyring-path)
                       (load-keyring keyring-path crypto-pkg))))
        
        (unless keyring
          (error "No keyring found"))
        
        (funcall (find-symbol "REVOKE-KEY" crypto-pkg)
                keyring key-id :reason reason)
        
        (save-keyring keyring keyring-path)
        
        (format t "Revoked key: ~A~%" key-id)
        (when reason
          (format t "Reason: ~A~%" reason)))))

(defun handle-verify (args)
  "Handle signature verification command"
  (let ((package-file nil)
        (signature-file nil)
        (keyring-file nil))
    
    ;; Parse arguments
    (loop while args
          do (let ((arg (pop args)))
               (cond
                 ((string= arg "--package")
                  (setf package-file (pop args)))
                 ((string= arg "--signature")
                  (setf signature-file (pop args)))
                 ((string= arg "--keyring")
                  (setf keyring-file (pop args)))
                 (t
                  (format t "Unknown option: ~A~%" arg)
                  (sb-ext:exit :code 1)))))
    
    (unless package-file
      (error "Package file required (--package FILE)"))
    
    (unless signature-file
      (error "Signature file required (--signature FILE)"))
    
    ;; Verify signature
    (load-crypto-system)
    (let ((crypto-pkg (find-package "EPSILON.CRYPTO-SIGNATURES")))
      (unless crypto-pkg
        (error "Crypto system not available"))
      
      (let* ((keyring-path (or keyring-file 
                              (merge-pathnames ".epsilon/keyring.lisp" 
                                             (user-homedir-pathname))))
             (keyring (when (probe-file keyring-path)
                       (load-keyring keyring-path crypto-pkg))))
        
        (unless keyring
          (error "No keyring found for verification"))
        
        (if (funcall (find-symbol "VERIFY-PACKAGE-SIGNATURE" crypto-pkg)
                    package-file signature-file :keyring keyring)
            (progn
              (format t "✓ Signature verification: PASSED~%")
              (format t "  Package: ~A~%" package-file)
              (format t "  Signature: ~A~%" signature-file))
            (progn
              (format t "✗ Signature verification: FAILED~%")
              (sb-ext:exit :code 1))))))

;;;; ==========================================================================
;;;; Utility Functions
;;;; ==========================================================================

(defun load-crypto-system ()
  "Load the crypto signatures system"
  (unless (find-package "EPSILON.CRYPTO-SIGNATURES")
    (load "scripts/crypto-signatures.lisp")))

(defun load-keyring (keyring-path crypto-pkg)
  "Load keyring from file"
  (with-open-file (in keyring-path)
    (read in)))

(defun save-keyring (keyring keyring-path)
  "Save keyring to file"
  (ensure-directories-exist keyring-path)
  (with-open-file (out keyring-path :direction :output :if-exists :supersede)
    (format out ";; Epsilon Keyring~%")
    (format out ";; Generated: ~A~%~%" (format-timestamp (get-universal-time)))
    (write keyring :stream out :pretty t)))

(defun import-public-key (filepath)
  "Import public key from file"
  (with-open-file (in filepath)
    (let ((content (make-string (file-length in))))
      (read-sequence content in)
      
      ;; Extract key ID and public key data
      (let ((key-id (extract-key-field content "Key ID"))
            (public-key (extract-public-key-data content)))
        (list :key-id key-id :public-key public-key)))))

(defun extract-key-field (content field-name)
  "Extract field from key file"
  (let ((pattern (format nil "# ~A: " field-name)))
    (let ((start (search pattern content)))
      (when start
        (let* ((line-start (+ start (length pattern)))
               (line-end (position #\Newline content :start line-start)))
          (when line-end
            (string-trim " " (subseq content line-start line-end))))))))

(defun extract-public-key-data (content)
  "Extract public key data from PEM format"
  (let ((begin-marker "-----BEGIN EPSILON PUBLIC KEY-----")
        (end-marker "-----END EPSILON PUBLIC KEY-----"))
    (let ((start (search begin-marker content)))
      (when start
        (let ((data-start (+ start (length begin-marker)))
              (end (search end-marker content :start2 start)))
          (when end
            (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (subseq content data-start end))))))))

(defun list-keyring-keys (keyring)
  "List all keys in keyring"
  (format t "Keyring Contents:~%")
  (format t "================~%~%")
  
  (let ((trusted-keys (getf keyring :trusted-keys))
        (revoked-keys (getf keyring :revoked-keys)))
    
    (format t "Trusted Keys:~%")
    (if (hash-table-p trusted-keys)
        (maphash (lambda (key-id key-data)
                   (format t "  ~A (~A)~%" 
                           key-id 
                           (getf key-data :trust-level)))
                 trusted-keys)
        (format t "  No trusted keys~%"))
    
    (format t "~%Revoked Keys:~%")
    (if (hash-table-p revoked-keys)
        (maphash (lambda (key-id revocation-data)
                   (format t "  ~A - ~A~%" 
                           key-id 
                           (getf revocation-data :reason)))
                 revoked-keys)
        (format t "  No revoked keys~%"))))

(defun ensure-directories-exist (filepath)
  "Ensure directory exists for filepath"
  (let ((dir (directory-namestring filepath)))
    (sb-ext:run-program "/bin/mkdir" (list "-p" dir))))

(defun format-timestamp (universal-time)
  "Format timestamp"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))

;; Entry point for command line usage
(when (and (boundp 'sb-ext:*posix-argv*) 
           (member "keytool" sb-ext:*posix-argv* :test #'search))
  (main (rest (member "keytool" sb-ext:*posix-argv* :test #'search))))
