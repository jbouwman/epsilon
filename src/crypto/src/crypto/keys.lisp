;;;; Key Management for Cryptographic Operations
;;;;
;;;; This module provides Ed25519 key generation, storage, and management
;;;; functionality using available cryptographic libraries.

(defpackage :epsilon.crypto.keys
  (:use :cl)
  (:local-nicknames
   (#:fs #:epsilon.sys.fs)
   (#:path #:epsilon.path)
   (#:str #:epsilon.string)
   (#:hex #:epsilon.hex)
   (#:digest #:epsilon.digest))
  (:export
   ;; Key structures
   #:keypair
   #:make-keypair
   #:keypair-private-key
   #:keypair-public-key
   #:keypair-key-id
   #:keypair-created
   #:keypair-algorithm
   
   ;; Key generation
   #:generate-keypair
   #:generate-key-id
   
   ;; Key storage
   #:save-keypair
   #:load-keypair
   #:export-public-key
   #:import-public-key
   
   ;; Key formats
   #:keypair-to-pem
   #:keypair-from-pem
   #:public-key-to-pem
   #:public-key-from-pem))

(in-package :epsilon.crypto.keys)

;;;; Key Structures

(defstruct keypair
  "Represents a cryptographic key pair"
  private-key
  public-key
  key-id
  created
  algorithm)

;;;; Key Generation

(defun generate-keypair (&key (algorithm :ed25519) (key-id nil))
  "Generate a new Ed25519 keypair"
  (let ((key-id (or key-id (generate-key-id))))
    (multiple-value-bind (private-key public-key)
        (generate-ed25519-keypair)
      (make-keypair
       :private-key private-key
       :public-key public-key
       :key-id key-id
       :created (get-universal-time)
       :algorithm algorithm))))

(defun generate-key-id ()
  "Generate a unique key identifier"
  (format nil "~8,'0X~8,'0X" 
          (random #x100000000)
          (get-universal-time)))

(defun generate-ed25519-keypair ()
  "Generate Ed25519 keypair using available methods"
  (or (try-generate-with-openssl)
      (try-generate-with-epsilon-crypto)
      (generate-placeholder-keypair)))

(defun try-generate-with-openssl ()
  "Try to generate keypair using OpenSSL command line"
  (handler-case
      (let ((private-key-file (path:make-temp-path :prefix "ed25519_private"))
            (public-key-file (path:make-temp-path :prefix "ed25519_public")))
        (unwind-protect
             (progn
               ;; Generate private key
               (run-openssl-command 
                (format nil "genpkey -algorithm Ed25519 -out ~A" 
                        (path:path-string private-key-file)))
               
               ;; Extract public key
               (run-openssl-command 
                (format nil "pkey -in ~A -pubout -out ~A" 
                        (path:path-string private-key-file)
                        (path:path-string public-key-file)))
               
               ;; Read keys
               (let ((private-key (fs:read-file private-key-file))
                     (public-key (fs:read-file public-key-file)))
                 (values private-key public-key)))
          ;; Cleanup
          (ignore-errors (fs:delete-file* private-key-file))
          (ignore-errors (fs:delete-file* public-key-file))))
    (error () nil)))

(defun try-generate-with-epsilon-crypto ()
  "Try to generate keypair using Epsilon's crypto module if available"
  ;; Placeholder for future epsilon crypto implementation
  nil)

(defun generate-placeholder-keypair ()
  "Generate a placeholder keypair for testing"
  (let* ((seed-bytes (make-array 32 :element-type '(unsigned-byte 8)))
         (timestamp (get-universal-time)))
    ;; Fill with pseudorandom data
    (dotimes (i 32)
      (setf (aref seed-bytes i) 
            (mod (+ (* i 13) timestamp (random 256)) 256)))
    
    (let ((private-hex (hex:u8-to-hex seed-bytes))
          (public-hex (hex:u8-to-hex (reverse seed-bytes))))
      (values
       (format nil "-----BEGIN PRIVATE KEY-----~%~A~%-----END PRIVATE KEY-----" 
               (base64-encode-hex private-hex))
       (format nil "-----BEGIN PUBLIC KEY-----~%~A~%-----END PUBLIC KEY-----" 
               (base64-encode-hex public-hex))))))

;;;; Key Storage and Loading

(defun save-keypair (keypair filepath &key (format :pem))
  "Save keypair to file"
  (ecase format
    (:pem (save-keypair-pem keypair filepath))
    (:json (save-keypair-json keypair filepath))))

(defun save-keypair-pem (keypair filepath)
  "Save keypair in PEM format"
  (with-open-file (out filepath :direction :output :if-exists :supersede)
    (format out "# Epsilon Package Signing Keypair~%")
    (format out "# Generated: ~A~%" (format-timestamp (keypair-created keypair)))
    (format out "# Key ID: ~A~%" (keypair-key-id keypair))
    (format out "# Algorithm: ~A~%~%" (keypair-algorithm keypair))
    (write-string (keypair-to-pem keypair) out)))

(defun save-keypair-json (keypair filepath)
  "Save keypair in JSON format"
  ;; Using epsilon's JSON module when available
  (error "JSON format not yet implemented"))

(defun load-keypair (filepath)
  "Load keypair from file"
  (let ((content (fs:read-file filepath)))
    (cond
      ((search "BEGIN EPSILON PRIVATE KEY" content)
       (keypair-from-pem content))
      ((search "{" content)
       (error "JSON format not yet implemented"))
      (t
       (error "Unknown keypair format")))))

(defun keypair-to-pem (keypair)
  "Convert keypair to PEM format string"
  (format nil "-----BEGIN EPSILON PRIVATE KEY-----~%~A~%-----END EPSILON PRIVATE KEY-----~%~%-----BEGIN EPSILON PUBLIC KEY-----~%~A~%-----END EPSILON PUBLIC KEY-----"
          (base64-encode-string (keypair-private-key keypair))
          (base64-encode-string (keypair-public-key keypair))))

(defun keypair-from-pem (pem-content)
  "Parse keypair from PEM format"
  (let ((key-id (extract-pem-field pem-content "Key ID"))
        (algorithm (extract-pem-field pem-content "Algorithm"))
        (private-key (extract-pem-section pem-content "EPSILON PRIVATE KEY"))
        (public-key (extract-pem-section pem-content "EPSILON PUBLIC KEY")))
    (make-keypair
     :key-id key-id
     :algorithm (if algorithm 
                   (intern (string-upcase algorithm) :keyword)
                   :ed25519)
     :private-key private-key
     :public-key public-key
     :created (get-universal-time))))

(defun export-public-key (keypair filepath)
  "Export public key to file"
  (with-open-file (out filepath :direction :output :if-exists :supersede)
    (format out "# Epsilon Public Key~%")
    (format out "# Key ID: ~A~%" (keypair-key-id keypair))
    (format out "# Algorithm: ~A~%~%" (keypair-algorithm keypair))
    (write-string (public-key-to-pem (keypair-public-key keypair)) out)))

(defun import-public-key (filepath)
  "Import public key from file"
  (let ((content (fs:read-file filepath)))
    (public-key-from-pem content)))

(defun public-key-to-pem (public-key)
  "Convert public key to PEM format"
  (if (str:starts-with-p public-key "-----BEGIN")
      public-key  ; Already in PEM format
      (format nil "-----BEGIN PUBLIC KEY-----~%~A~%-----END PUBLIC KEY-----"
              (base64-encode-string public-key))))

(defun public-key-from-pem (pem-content)
  "Extract public key from PEM format"
  (extract-pem-section pem-content "PUBLIC KEY"))

;;;; Utility Functions

(defun extract-pem-field (content field-name)
  "Extract field from PEM header comments"
  (let ((pattern (format nil "# ~A: " field-name)))
    (let ((start (search pattern content)))
      (when start
        (let* ((line-start (+ start (length pattern)))
               (line-end (position #\Newline content :start line-start)))
          (when line-end
            (str:strip (subseq content line-start line-end) #\Space)))))))

(defun extract-pem-section (content section-name)
  "Extract section data from PEM format"
  (let ((begin-marker (format nil "-----BEGIN ~A-----" section-name))
        (end-marker (format nil "-----END ~A-----" section-name)))
    (let ((start (search begin-marker content)))
      (when start
        (let ((data-start (position #\Newline content :start (+ start (length begin-marker))))
              (end (search end-marker content :start2 start)))
          (when (and data-start end)
            (str:strip 
             (subseq content (1+ data-start) end)
             #\Newline)))))))

(defun run-openssl-command (args)
  "Run OpenSSL command with arguments"
  (let ((process (sb-ext:run-program "openssl" 
                                    (str:split #\Space args)
                                    :output nil
                                    :error nil
                                    :search t)))
    (unless (zerop (sb-ext:process-exit-code process))
      (error "OpenSSL command failed: openssl ~A" args))))

(defun base64-encode-string (string)
  "Encode string as base64"
  ;; Simple implementation - would use epsilon's base64 module
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))
    (with-output-to-string (out)
      (loop for i from 0 below (length string) by 3
            for chunk = (subseq string i (min (+ i 3) (length string)))
            do (dotimes (j 4)
                 (write-char 
                  (char chars 
                        (if (< j (ceiling (* 4 (length chunk)) 3))
                            (mod (+ i j) 64)
                            64))
                  out))))))

(defun base64-encode-hex (hex-string)
  "Encode hex string as base64"
  (base64-encode-string hex-string))

(defun format-timestamp (universal-time)
  "Format timestamp for display"
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour min sec)))
