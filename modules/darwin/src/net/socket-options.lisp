;;;; Socket option management

(defpackage epsilon.net.socket-options
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign))
  (:import-from epsilon.net.constants
   +sol-socket+ +so-reuseaddr+ +so-keepalive+ +so-broadcast+ +so-linger+
   +so-rcvbuf+ +so-sndbuf+ +so-rcvtimeo+ +so-sndtimeo+
   +ipproto-tcp-level+ +tcp-nodelay+
   %setsockopt %getsockopt)
  (:import-from epsilon.net.core
   tcp-listener tcp-listener-handle
   tcp-stream tcp-stream-handle
   udp-socket udp-socket-handle
   network-error)
  (:export
   set-socket-option
   get-socket-option))

(in-package epsilon.net.socket-options)

;;; ============================================================================
;;; Socket Options
;;; ============================================================================

(defun option-to-constants (option)
  "Convert keyword option to level and optname constants"
    (case option
      (:reuse-address (values +sol-socket+ +so-reuseaddr+))
      (:keep-alive (values +sol-socket+ +so-keepalive+))
      (:broadcast (values +sol-socket+ +so-broadcast+))
      (:linger (values +sol-socket+ +so-linger+))
      (:recv-buffer (values +sol-socket+ +so-rcvbuf+))
      (:send-buffer (values +sol-socket+ +so-sndbuf+))
      (:recv-timeout (values +sol-socket+ +so-rcvtimeo+))
      (:send-timeout (values +sol-socket+ +so-sndtimeo+))
      (:nodelay (values +ipproto-tcp-level+ +tcp-nodelay+))
      (otherwise (error "Unknown socket option: ~A" option))))

(defun get-socket-handle (socket)
  "Get the file descriptor from any socket type"
    (etypecase socket
      (tcp-listener (tcp-listener-handle socket))
      (tcp-stream (tcp-stream-handle socket))
      (udp-socket (udp-socket-handle socket))))

(defun set-socket-option (socket option value)
  "Set a socket option"
    (multiple-value-bind (level optname) (option-to-constants option)
      (let ((fd (get-socket-handle socket)))
        (handler-case
            (case option
              ((:reuse-address :keep-alive :broadcast :nodelay)
               ;; Boolean options
               (lib:with-foreign-memory ((optval :int :count 1))
                 (setf (sb-sys:sap-ref-32 optval 0) (if value 1 0))
                 (%setsockopt fd level optname optval 4)))
              ((:recv-buffer :send-buffer)
               ;; Integer size options
               (lib:with-foreign-memory ((optval :int :count 1))
                 (setf (sb-sys:sap-ref-32 optval 0) value)
                 (%setsockopt fd level optname optval 4)))
              ((:recv-timeout :send-timeout)
               ;; Timeout options (milliseconds to timeval)
               (lib:with-foreign-memory ((optval :char :count 16))
                 ;; struct timeval { long tv_sec; long tv_usec; }
                 (let ((seconds (floor value 1000))
                       (microseconds (* (mod value 1000) 1000)))
                   (setf (sb-sys:sap-ref-64 optval 0) seconds)
                   (setf (sb-sys:sap-ref-64 optval 8) microseconds))
                 (%setsockopt fd level optname optval 16)))
              (:linger
               ;; Linger option
               (lib:with-foreign-memory ((optval :char :count 8))
                 ;; struct linger { int l_onoff; int l_linger; }
                 (if value
                     (progn
                       (setf (sb-sys:sap-ref-32 optval 0) 1)
                       (setf (sb-sys:sap-ref-32 optval 4) value))
                     (setf (sb-sys:sap-ref-32 optval 0) 0))
                 (%setsockopt fd level optname optval 8))))
          (error (e)
            (error 'network-error :message (format nil "Failed to set socket option: ~A" e)))))))

(defun get-socket-option (socket option)
  "Get a socket option value"
    (multiple-value-bind (level optname) (option-to-constants option)
      (let ((fd (get-socket-handle socket)))
        (handler-case
            (case option
              ((:reuse-address :keep-alive :broadcast :nodelay)
               ;; Boolean options
               (lib:with-foreign-memory ((optval :int :count 1)
                                         (optlen :int :count 1))
                 (setf (sb-sys:sap-ref-32 optlen 0) 4)
                 (%getsockopt fd level optname optval optlen)
                 (not (zerop (sb-sys:sap-ref-32 optval 0)))))
              ((:recv-buffer :send-buffer)
               ;; Integer size options
               (lib:with-foreign-memory ((optval :int :count 1)
                                         (optlen :int :count 1))
                 (setf (sb-sys:sap-ref-32 optlen 0) 4)
                 (%getsockopt fd level optname optval optlen)
                 (sb-sys:sap-ref-32 optval 0)))
              ((:recv-timeout :send-timeout)
               ;; Timeout options (timeval to milliseconds)
               (lib:with-foreign-memory ((optval :char :count 16)
                                         (optlen :int :count 1))
                 (setf (sb-sys:sap-ref-32 optlen 0) 16)
                 (%getsockopt fd level optname optval optlen)
                 (let ((seconds (sb-sys:sap-ref-64 optval 0))
                       (microseconds (sb-sys:sap-ref-64 optval 8)))
                   (+ (* seconds 1000) (floor microseconds 1000)))))
              (:linger
               ;; Linger option
               (lib:with-foreign-memory ((optval :char :count 8)
                                         (optlen :int :count 1))
                 (setf (sb-sys:sap-ref-32 optlen 0) 8)
                 (%getsockopt fd level optname optval optlen)
                 (let ((onoff (sb-sys:sap-ref-32 optval 0)))
                   (if (zerop onoff)
                       nil
                       (sb-sys:sap-ref-32 optval 4))))))
          (error (e)
            (error 'network-error :message (format nil "Failed to get socket option: ~A" e)))))))