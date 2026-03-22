;;;; epsilon.digest.ssl-hashers - Hasher protocol bridge for epsilon.ssl states
;;;;
;;;; Wraps epsilon.ssl hash states (md5-state, sha256-state, etc.) in a
;;;; uniform struct that implements the epsilon.digest.protocol hasher typeclass.

(defpackage epsilon.digest.ssl-hashers
  (:use :cl)
  (:require (epsilon.typeclass tc)
            (epsilon.digest.protocol proto)
            (epsilon.ssl ssl))
  (:export ssl-hasher
           make-ssl-hasher)
  (:enter t))

;;; ============================================================================
;;; SSL Hasher Wrapper
;;; ============================================================================

(defstruct (ssl-hasher (:constructor %make-ssl-hasher))
  "Wrapper around epsilon.ssl hash states that implements the hasher protocol."
  (algorithm :md5 :type keyword)
  (state nil)
  (update-fn nil :type function)
  (finalize-fn nil :type function)
  (copy-fn nil :type function)
  (make-fn nil :type function)
  (output-length 16 :type fixnum)
  (block-length 64 :type fixnum)
  (finalized-p nil :type boolean))

(defun make-ssl-hasher (algorithm)
  "Create an ssl-hasher for ALGORITHM keyword."
  (ecase algorithm
    (:md5 (%make-ssl-hasher
            :algorithm :md5
            :state (ssl:make-md5-state)
            :update-fn #'ssl:md5-update
            :finalize-fn #'ssl:md5-finalize
            :copy-fn #'ssl:md5-copy
            :make-fn #'ssl:make-md5-state
            :output-length 16
            :block-length 64))
    (:sha1 (%make-ssl-hasher
             :algorithm :sha1
             :state (ssl:make-sha1-state)
             :update-fn #'ssl:sha1-update
             :finalize-fn #'ssl:sha1-finalize
             :copy-fn #'ssl:sha1-copy
             :make-fn #'ssl:make-sha1-state
             :output-length 20
             :block-length 64))
    (:sha256 (%make-ssl-hasher
               :algorithm :sha256
               :state (ssl:make-sha256-state)
               :update-fn #'ssl:sha256-update
               :finalize-fn #'ssl:sha256-finalize
               :copy-fn #'ssl:sha256-copy
               :make-fn #'ssl:make-sha256-state
               :output-length 32
               :block-length 64))
    (:sha384 (%make-ssl-hasher
               :algorithm :sha384
               :state (ssl:make-sha384-state)
               :update-fn #'ssl:sha384-update
               :finalize-fn #'ssl:sha384-finalize
               :copy-fn #'ssl:sha384-copy
               :make-fn #'ssl:make-sha384-state
               :output-length 48
               :block-length 128))
    (:sha512 (%make-ssl-hasher
               :algorithm :sha512
               :state (ssl:make-sha512-state)
               :update-fn #'ssl:sha512-update
               :finalize-fn #'ssl:sha512-finalize
               :copy-fn #'ssl:sha512-copy
               :make-fn #'ssl:make-sha512-state
               :output-length 64
               :block-length 128))
    (:sha3-256 (%make-ssl-hasher
                 :algorithm :sha3-256
                 :state (ssl:make-sha3-256-state)
                 :update-fn #'ssl:sha3-256-update
                 :finalize-fn #'ssl:sha3-256-finalize
                 :copy-fn #'ssl:sha3-256-copy
                 :make-fn #'ssl:make-sha3-256-state
                 :output-length 32
                 :block-length 136))
    (:sha3-384 (%make-ssl-hasher
                 :algorithm :sha3-384
                 :state (ssl:make-sha3-384-state)
                 :update-fn #'ssl:sha3-384-update
                 :finalize-fn #'ssl:sha3-384-finalize
                 :copy-fn #'ssl:sha3-384-copy
                 :make-fn #'ssl:make-sha3-384-state
                 :output-length 48
                 :block-length 104))
    (:sha3-512 (%make-ssl-hasher
                 :algorithm :sha3-512
                 :state (ssl:make-sha3-512-state)
                 :update-fn #'ssl:sha3-512-update
                 :finalize-fn #'ssl:sha3-512-finalize
                 :copy-fn #'ssl:sha3-512-copy
                 :make-fn #'ssl:make-sha3-512-state
                 :output-length 64
                 :block-length 72))))

;;; ============================================================================
;;; Hasher Protocol Implementation
;;; ============================================================================

(tc:definstance proto:hasher ssl-hasher
  (proto:hasher-update (hasher data &key (start 0) (end (length data)))
    (when (ssl-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (funcall (ssl-hasher-update-fn hasher) (ssl-hasher-state hasher) data
             :start start :end end)
    hasher)

  (proto:hasher-finalize (hasher &key output output-length)
    (declare (ignore output output-length))
    (when (ssl-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (setf (ssl-hasher-finalized-p hasher) t)
    (funcall (ssl-hasher-finalize-fn hasher) (ssl-hasher-state hasher)))

  (proto:hasher-reset (hasher)
    (setf (ssl-hasher-state hasher) (funcall (ssl-hasher-make-fn hasher)))
    (setf (ssl-hasher-finalized-p hasher) nil)
    hasher)

  (proto:hasher-copy (hasher)
    (when (ssl-hasher-finalized-p hasher)
      (error 'proto:hasher-finalized-error))
    (%make-ssl-hasher
     :algorithm (ssl-hasher-algorithm hasher)
     :state (funcall (ssl-hasher-copy-fn hasher) (ssl-hasher-state hasher))
     :update-fn (ssl-hasher-update-fn hasher)
     :finalize-fn (ssl-hasher-finalize-fn hasher)
     :copy-fn (ssl-hasher-copy-fn hasher)
     :make-fn (ssl-hasher-make-fn hasher)
     :output-length (ssl-hasher-output-length hasher)
     :block-length (ssl-hasher-block-length hasher)))

  (proto:hasher-algorithm (hasher)
    (ssl-hasher-algorithm hasher))

  (proto:hasher-output-length (hasher)
    (ssl-hasher-output-length hasher))

  (proto:hasher-block-length (hasher)
    (ssl-hasher-block-length hasher)))
