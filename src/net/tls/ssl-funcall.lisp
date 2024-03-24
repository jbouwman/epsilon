(in-package :net.tls)

;;; Waiting for output to be possible

(defun seconds-until-deadline (deadline)
  (/ (- deadline (get-internal-real-time))
     internal-time-units-per-second))

(defun output-wait (stream fd deadline)
  (declare (ignore stream))
  (let ((timeout
         ;; *deadline* is handled by wait-until-fd-usable automatically,
         ;; but we need to turn a user-specified deadline into a timeout
         (when deadline
           (seconds-until-deadline deadline))))
    (sb-sys:wait-until-fd-usable fd :output timeout)))

;;; Waiting for input to be possible

(defun input-wait (stream fd deadline)
  (declare (ignore stream))
  (let ((timeout
         ;; *deadline* is handled by wait-until-fd-usable automatically,
         ;; but we need to turn a user-specified deadline into a timeout
         (when deadline
           (seconds-until-deadline deadline))))
    (sb-sys:wait-until-fd-usable fd :input timeout)))

;;; Funcall wrapper

(declaim (inline ensure-ssl-funcall))
(defun ensure-ssl-funcall (stream success-test func handle &rest other-args)
  (loop
     (let ((ret
            (let ((*bio-socket* (ssl-stream-socket stream))) ;for Lisp-BIO callbacks
              (apply func handle other-args))))
       (when (funcall success-test ret)
         (return ret))
       (let ((error (ssl-get-error handle ret)))
         (case error
           (#.+ssl-error-want-read+
            (input-wait stream
                        (ssl-get-fd handle)
                        (ssl-stream-deadline stream)))
           (#.+ssl-error-want-write+
            (output-wait stream
                         (ssl-get-fd handle)
                         (ssl-stream-deadline stream)))
           (t
            (ssl-signal-error handle func error ret)))))))

(declaim (inline nonblocking-ssl-funcall))
(defun nonblocking-ssl-funcall (stream success-test func handle &rest other-args)
  (loop
     (let ((ret
            (let ((*bio-socket* (ssl-stream-socket stream))) ;for Lisp-BIO callbacks
              (apply func handle other-args))))
       (when (funcall success-test ret)
         (return ret))
       (let ((error (ssl-get-error handle ret)))
         (case error
           ((#.+ssl-error-want-read+ #.+ssl-error-want-write+)
            (return ret))
           (t
            (ssl-signal-error handle func error ret)))))))

