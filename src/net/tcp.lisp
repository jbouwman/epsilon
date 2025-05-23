(defpackage #:epsilon.net.tcp
  (:use #:cl)
  (:export
   ;; Address structure
   #:tcp-address #:make-tcp-address
   #:address-host #:address-port
   
   ;; Address utilities
   #:parse-tcp-address #:format-tcp-address
   #:localhost-address #:any-address
   
   ;; Port utilities
   #:find-open-port #:port-open-p #:probe-port
   #:random-port #:ephemeral-port-range))

(in-package :epsilon.net.tcp)

;;; TCP Address Structure

(defstruct tcp-address
  (host "localhost" :type string)
  (port 80 :type (unsigned-byte 16)))

;;; Address Constructors

(defun localhost-address (port)
  "Create a TCP address for localhost with the given port."
  (make-tcp-address :host "localhost" :port port))

(defun any-address (port)
  "Create a TCP address that binds to any interface (0.0.0.0)."
  (make-tcp-address :host "0.0.0.0" :port port))

;;; Address Parsing and Formatting

(defun parse-tcp-address (address-string)
  "Parse a TCP address from string format (host:port or just port)."
  (let ((colon-pos (position #\: address-string :from-end t)))
    (if colon-pos
        (let ((host (subseq address-string 0 colon-pos))
              (port-str (subseq address-string (1+ colon-pos))))
          (make-tcp-address 
           :host (if (string= host "") "localhost" host)
           :port (parse-integer port-str)))
        (make-tcp-address 
         :host "localhost" 
         :port (parse-integer address-string)))))

(defun format-tcp-address (address)
  "Format a TCP address as a string (host:port)."
  (format nil "~A:~D" 
          (tcp-address-host address) 
          (tcp-address-port address)))

;;; Port Range Constants

(defconstant +ephemeral-port-start+ 49152)
(defconstant +ephemeral-port-end+ 65535)

(defun ephemeral-port-range ()
  "Return the ephemeral port range as (start . end)."
  (cons +ephemeral-port-start+ +ephemeral-port-end+))

;;; Port Probing

(defun port-open-p (host port &key (timeout 1))
  "Check if a port is open on the given host."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
                                   :type :stream 
                                   :protocol :tcp)))
        (unwind-protect
             (progn
               (setf (sb-bsd-sockets:non-blocking-mode socket) t)
               (sb-bsd-sockets:socket-connect socket host port)
               t)
          (sb-bsd-sockets:socket-close socket)))
    (error () nil)))

(defun probe-port (host port)
  "Probe a specific port on a host. Returns T if connection succeeds."
  (port-open-p host port :timeout 0.5))

;;; Port Discovery

(defun random-port (&optional (start +ephemeral-port-start+) (end +ephemeral-port-end+))
  "Generate a random port number in the given range."
  (+ start (random (- end start))))

(defun find-open-port (&key (host "localhost") 
                           (start +ephemeral-port-start+) 
                           (end +ephemeral-port-end+)
                           (max-attempts 100))
  "Find an open port on localhost by probing a range randomly.
   Returns the port number if found, or NIL if no open port found after max-attempts."
  (let ((attempted-ports (make-hash-table)))
    (loop repeat max-attempts
          for port = (random-port start end)
          unless (gethash port attempted-ports)
          do (progn
               (setf (gethash port attempted-ports) t)
               (unless (port-open-p host port)
                 (return port)))
          finally (return nil))))

;;; Port Range Scanning

(defun scan-port-range (host start-port end-port &key (timeout 1))
  "Scan a range of ports and return a list of open ports."
  (loop for port from start-port to end-port
        when (port-open-p host port :timeout timeout)
        collect port))

(defun find-n-open-ports (n &key (host "localhost")
                              (start +ephemeral-port-start+)
                              (end +ephemeral-port-end+)
                              (max-attempts (* n 10)))
  "Find N open ports on the given host."
  (let ((ports '())
        (attempted-ports (make-hash-table)))
    (loop repeat max-attempts
          while (< (length ports) n)
          for port = (random-port start end)
          unless (gethash port attempted-ports)
          do (progn
               (setf (gethash port attempted-ports) t)
               (unless (port-open-p host port)
                 (push port ports)))
          finally (return (nreverse ports)))))

;;; Address Validation

(defun valid-port-p (port)
  "Check if port number is valid (1-65535)."
  (and (integerp port) (<= 1 port 65535)))

(defun valid-host-p (host)
  "Basic validation of host string."
  (and (stringp host) 
       (> (length host) 0)
       (not (find #\space host))))

(defun valid-tcp-address-p (address)
  "Check if TCP address is valid."
  (and (tcp-address-p address)
       (valid-host-p (tcp-address-host address))
       (valid-port-p (tcp-address-port address))))

;;; Address Comparison

(defun tcp-address-equal (addr1 addr2)
  "Compare two TCP addresses for equality."
  (and (string= (tcp-address-host addr1) (tcp-address-host addr2))
       (= (tcp-address-port addr1) (tcp-address-port addr2))))

(defun tcp-address-< (addr1 addr2)
  "Compare TCP addresses for ordering (by host then port)."
  (let ((host-cmp (string< (tcp-address-host addr1) (tcp-address-host addr2))))
    (if (string= (tcp-address-host addr1) (tcp-address-host addr2))
        (< (tcp-address-port addr1) (tcp-address-port addr2))
        host-cmp)))
