(defpackage epsilon.lib.net
  (:use cl)
  (:export
   ;; Core networking
   make-tcp-socket
   make-udp-socket
   make-listener
   connect-tcp
   accept-connection
   close-socket
   
   ;; Stream operations
   socket-stream
   
   ;; Address utilities
   resolve-hostname
   parse-address
   
   ;; High-level utilities
   with-tcp-connection
   with-tcp-listener
   
   ;; Socket options
   set-socket-option
   get-socket-option
   
   ;; Conditions
   network-error
   connection-error
   timeout-error))

(in-package :epsilon.lib.net)

;;; Conditions

(define-condition network-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c s) (format s "Network error: ~A" (error-message c)))))

(define-condition connection-error (network-error) ())

(define-condition timeout-error (network-error) ())

;;; Core Socket Creation

(defun make-tcp-socket ()
  "Create a TCP socket using sb-bsd-sockets"
  (handler-case
      (make-instance 'sb-bsd-sockets:inet-socket
                     :type :stream
                     :protocol :tcp)
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to create TCP socket: ~A" e)))))

(defun make-udp-socket ()
  "Create a UDP socket using sb-bsd-sockets"
  (handler-case
      (make-instance 'sb-bsd-sockets:inet-socket
                     :type :datagram
                     :protocol :udp)
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to create UDP socket: ~A" e)))))

;;; TCP Server Operations

(defun make-listener (port &key (address "0.0.0.0") (reuse-address t) (backlog 5))
  "Create a TCP listener socket bound to address:port"
  (handler-case
      (let ((socket (make-tcp-socket)))
        ;; Set socket options
        (when reuse-address
          (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
        
        ;; Bind to address
        (sb-bsd-sockets:socket-bind socket 
                                   (if (string= address "*") #(0 0 0 0)
                                       (resolve-hostname address))
                                   port)
        
        ;; Start listening
        (sb-bsd-sockets:socket-listen socket backlog)
        
        socket)
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to create listener on ~A:~A: ~A" 
                              address port e)))))

(defun accept-connection (listener &key (timeout nil))
  "Accept a connection from a listener socket"
  (declare (ignore timeout)) ; TODO: implement timeout
  (handler-case
      (sb-bsd-sockets:socket-accept listener)
    (error (e)
      (error 'connection-error 
             :message (format nil "Failed to accept connection: ~A" e)))))

;;; TCP Client Operations

(defun connect-tcp (host port &key (timeout 30))
  "Connect to a TCP server at host:port"
  (declare (ignore timeout)) ; TODO: implement timeout
  (handler-case
      (let ((socket (make-tcp-socket)))
        (sb-bsd-sockets:socket-connect socket 
                                      (resolve-hostname host) 
                                      port)
        socket)
    (error (e)
      (error 'connection-error 
             :message (format nil "Failed to connect to ~A:~A: ~A" 
                              host port e)))))

;;; Socket Operations

(defun close-socket (socket)
  "Close a socket"
  (handler-case
      (sb-bsd-sockets:socket-close socket)
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to close socket: ~A" e)))))

(defun socket-stream (socket &key (input t) (output t) (buffering :full))
  "Create a stream from a socket"
  (handler-case
      (sb-bsd-sockets:socket-make-stream socket 
                                        :input input 
                                        :output output
                                        :buffering buffering)
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to create socket stream: ~A" e)))))

;;; Utility Functions

(defun split-string (string delimiter)
  "Split string by delimiter character"
  (let ((parts '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) parts)
             (setf start (1+ pos))
          finally (push (subseq string start) parts))
    (nreverse parts)))

;;; Address Resolution

(defun resolve-hostname (hostname)
  "Resolve hostname to IP address"
  (cond
    ;; Handle special cases
    ((or (string= hostname "localhost") (string= hostname "127.0.0.1"))
     #(127 0 0 1))
    ((or (string= hostname "0.0.0.0") (string= hostname "*"))
     #(0 0 0 0))
    
    ;; Check if it's already an IP address
    ((every (lambda (char) (or (digit-char-p char) (char= char #\.))) hostname)
     (let ((parts (split-string hostname #\.)))
       (when (= (length parts) 4)
         (handler-case
             (map 'vector (lambda (part) (parse-integer part)) parts)
           (error ()
             ;; Fall back to DNS resolution
             (resolve-hostname-dns hostname))))))
    
    ;; DNS resolution
    (t (resolve-hostname-dns hostname))))

(defun resolve-hostname-dns (hostname)
  "Resolve hostname using DNS"
  (handler-case
      (sb-bsd-sockets:host-ent-address 
       (sb-bsd-sockets:get-host-by-name hostname))
    (error (e)
      (error 'network-error 
             :message (format nil "Failed to resolve hostname ~A: ~A" hostname e)))))

(defun parse-address (address-string)
  "Parse address string in format 'host:port' or just 'host'"
  (let ((colon-pos (position #\: address-string)))
    (if colon-pos
        (values (subseq address-string 0 colon-pos)
                (parse-integer (subseq address-string (1+ colon-pos))))
        (values address-string 80)))) ; Default to port 80

;;; Socket Options

(defun set-socket-option (socket option value)
  "Set a socket option"
  (case option
    (:reuse-address 
     (setf (sb-bsd-sockets:sockopt-reuse-address socket) value))
    (:keep-alive
     (setf (sb-bsd-sockets:sockopt-keep-alive socket) value))
    (:tcp-no-delay
     (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) value))
    (otherwise
     (error 'network-error 
            :message (format nil "Unknown socket option: ~A" option)))))

(defun get-socket-option (socket option)
  "Get a socket option"
  (case option
    (:reuse-address 
     (sb-bsd-sockets:sockopt-reuse-address socket))
    (:keep-alive
     (sb-bsd-sockets:sockopt-keep-alive socket))
    (:tcp-no-delay
     (sb-bsd-sockets:sockopt-tcp-nodelay socket))
    (otherwise
     (error 'network-error 
            :message (format nil "Unknown socket option: ~A" option)))))

;;; High-level Utilities

(defmacro with-tcp-connection ((var host port &rest options) &body body)
  "Establish TCP connection with automatic cleanup"
  `(let ((,var (connect-tcp ,host ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (close-socket ,var)))))

(defmacro with-tcp-listener ((var port &rest options) &body body)
  "Create TCP listener with automatic cleanup"
  `(let ((,var (make-listener ,port ,@options)))
     (unwind-protect
          (progn ,@body)
       (when ,var
         (close-socket ,var)))))

;;; Example Usage Functions

(defun simple-http-get (host port path)
  "Simple HTTP GET request example"
  (with-tcp-connection (socket host port)
    (let ((stream (socket-stream socket)))
      ;; Send HTTP request
      (format stream "GET ~A HTTP/1.1~C~CHost: ~A~C~CConnection: close~C~C~C~C"
              path #\Return #\Linefeed host #\Return #\Linefeed 
              #\Return #\Linefeed #\Return #\Linefeed)
      (force-output stream)
      
      ;; Read response
      (let ((response (make-string-output-stream)))
        (loop for line = (read-line stream nil nil)
              while line
              do (write-line line response))
        (get-output-stream-string response)))))

(defun simple-echo-server (port &key (host "127.0.0.1"))
  "Simple echo server example"
  (format t "Starting echo server on ~A:~A~%" host port)
  (with-tcp-listener (listener port :address host)
    (loop
      (let ((client (accept-connection listener)))
        (when client
          (handler-case
              (let ((stream (socket-stream client)))
                (format t "Client connected~%")
                (loop for line = (read-line stream nil nil)
                      while line
                      do (format stream "Echo: ~A~%" line)
                         (force-output stream)
                         (format t "Echoed: ~A~%" line)))
            (error (e)
              (format t "Client error: ~A~%" e)))
          (when client
            (close-socket client)
            (format t "Client disconnected~%")))))))
