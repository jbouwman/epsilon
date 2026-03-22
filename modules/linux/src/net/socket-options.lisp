;;;; Socket option management
;;;;
;;;; High-level keyword-based socket option API matching the Darwin interface.
;;;; Wraps the raw set-socket-option/get-socket-option from epsilon.net.core
;;;; with keyword dispatch for option names.

(defpackage epsilon.net.socket-options
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign)
   (const epsilon.net.constants)
   (core epsilon.net.core)
   (types epsilon.net.types)
   (errors epsilon.net.errors))
  (:export
   set-socket-option
   get-socket-option)
  (:enter t))

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(defun option-to-constants (option)
  "Convert keyword option to level and optname constants"
  (case option
    (:reuse-address (values const:+sol-socket+ const:+so-reuseaddr+))
    (:keep-alive    (values const:+sol-socket+ const:+so-keepalive+))
    (:broadcast     (values const:+sol-socket+ const:+so-broadcast+))
    (:linger        (values const:+sol-socket+ const:+so-linger+))
    (:recv-buffer   (values const:+sol-socket+ const:+so-rcvbuf+))
    (:send-buffer   (values const:+sol-socket+ const:+so-sndbuf+))
    (:recv-timeout  (values const:+sol-socket+ const:+so-rcvtimeo+))
    (:send-timeout  (values const:+sol-socket+ const:+so-sndtimeo+))
    (:no-delay      (values const:+ipproto-tcp-level+ const:+tcp-nodelay+))
    (:nodelay       (values const:+ipproto-tcp-level+ const:+tcp-nodelay+))
    (otherwise (error 'errors:network-error
                      :message (format nil "Unknown socket option: ~A" option)))))

(defun get-socket-handle (socket)
  "Get the file descriptor from any socket type"
  (etypecase socket
    (types:tcp-listener (types:tcp-listener-handle socket))
    (types:tcp-stream (types:tcp-stream-handle socket))
    (types:udp-socket (types:udp-socket-handle socket))))

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun set-socket-option (socket option value)
  "Set a socket option using keyword names.
   SOCKET is a tcp-listener, tcp-stream, or udp-socket.
   OPTION is a keyword: :keep-alive, :no-delay, :recv-timeout, etc.
   VALUE depends on the option type."
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (handler-case
          (case option
            ((:reuse-address :keep-alive :broadcast :no-delay :nodelay)
             ;; Boolean options
             (lib:with-foreign-memory ((optval :int :count 1))
               (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
               (const:%setsockopt fd level optname optval 4)))
            ((:recv-buffer :send-buffer)
             ;; Integer size options
             (lib:with-foreign-memory ((optval :int :count 1))
               (setf (sb-sys:sap-ref-32 optval 0) value)
               (const:%setsockopt fd level optname optval 4)))
            ((:recv-timeout :send-timeout)
             ;; Timeout options (milliseconds to timeval)
             (lib:with-foreign-memory ((optval :char :count 16))
               ;; struct timeval { long tv_sec; long tv_usec; }
               (let ((seconds (floor value 1000))
                     (microseconds (* (mod value 1000) 1000)))
                 (setf (sb-sys:sap-ref-64 optval 0) seconds)
                 (setf (sb-sys:sap-ref-64 optval 8) microseconds))
               (const:%setsockopt fd level optname optval 16)))
            (:linger
             ;; Linger option
             (lib:with-foreign-memory ((optval :char :count 8))
               ;; struct linger { int l_onoff; int l_linger; }
               (if value
                   (progn
                     (setf (sb-sys:sap-ref-32 optval 0) 1)
                     (setf (sb-sys:sap-ref-32 optval 4) value))
                   (setf (sb-sys:sap-ref-32 optval 0) 0))
               (const:%setsockopt fd level optname optval 8))))
        (error (e)
          (error 'errors:network-error
                 :message (format nil "Failed to set socket option: ~A" e)))))))

(defun get-socket-option (socket option)
  "Get a socket option value using keyword names.
   SOCKET is a tcp-listener, tcp-stream, or udp-socket.
   OPTION is a keyword: :keep-alive, :no-delay, :recv-timeout, etc."
  (multiple-value-bind (level optname) (option-to-constants option)
    (let ((fd (get-socket-handle socket)))
      (handler-case
          (case option
            ((:reuse-address :keep-alive :broadcast :no-delay :nodelay)
             ;; Boolean options
             (lib:with-foreign-memory ((optval :int :count 1)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 4)
               (const:%getsockopt fd level optname optval optlen)
               (not (zerop (sb-sys:sap-ref-32 optval 0)))))
            ((:recv-buffer :send-buffer)
             ;; Integer size options
             (lib:with-foreign-memory ((optval :int :count 1)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 4)
               (const:%getsockopt fd level optname optval optlen)
               (sb-sys:sap-ref-32 optval 0)))
            ((:recv-timeout :send-timeout)
             ;; Timeout options (timeval to milliseconds)
             (lib:with-foreign-memory ((optval :char :count 16)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 16)
               (const:%getsockopt fd level optname optval optlen)
               (let ((seconds (sb-sys:sap-ref-64 optval 0))
                     (microseconds (sb-sys:sap-ref-64 optval 8)))
                 (+ (* seconds 1000) (floor microseconds 1000)))))
            (:linger
             ;; Linger option
             (lib:with-foreign-memory ((optval :char :count 8)
                                       (optlen :int :count 1))
               (setf (sb-sys:sap-ref-32 optlen 0) 8)
               (const:%getsockopt fd level optname optval optlen)
               (let ((onoff (sb-sys:sap-ref-32 optval 0)))
                 (if (zerop onoff)
                     nil
                     (sb-sys:sap-ref-32 optval 4))))))
        (error (e)
          (error 'errors:network-error
                 :message (format nil "Failed to get socket option: ~A" e)))))))
