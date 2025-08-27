;;;; Address Utilities for Linux Networking
;;;;
;;;; Address parsing, creation, and DNS resolution

(defpackage epsilon.net.address
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (types epsilon.net.types)
   (errors epsilon.net.errors)
   (lib epsilon.foreign))
  (:export
   ;; Address creation
   #:make-socket-address
   #:parse-address
   #:normalize-address
   
   ;; DNS resolution
   #:resolve-address
   #:dns-resolve-hostname
   
   ;; sockaddr manipulation
   #:make-sockaddr-in-into
   #:parse-sockaddr-in
   #:sockaddr-size
   
   ;; Utilities
   #:split-string
   #:is-ip-address-p
   #:is-ipv4-address-p
   #:is-ipv6-address-p))

(in-package epsilon.net.address)

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun split-string (string delimiter)
  "Split string by delimiter"
  (let ((parts '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) parts)
             (setf start (1+ pos))
          finally (push (subseq string start) parts))
    (nreverse parts)))

(defun is-ipv4-address-p (string)
  "Check if string looks like an IPv4 address"
  (and (stringp string)
       (let ((parts (split-string string #\.)))
         (and (= 4 (length parts))
              (every (lambda (part)
                       (and (> (length part) 0)
                            (<= (length part) 3)
                            (every #'digit-char-p part)
                            (let ((num (parse-integer part :junk-allowed t)))
                              (and num (<= 0 num 255)))))
                     parts)))))

(defun is-ipv6-address-p (string)
  "Check if string looks like an IPv6 address"
  (and (stringp string)
       (find #\: string)))

(defun is-ip-address-p (string)
  "Check if string looks like an IP address (v4 or v6)"
  (or (is-ipv4-address-p string)
      (is-ipv6-address-p string)))

;;; ============================================================================
;;; Address Creation and Parsing
;;; ============================================================================

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (types:make-socket-address ip port))

(defun parse-address (address-string)
  "Parse an address string like '127.0.0.1:8080' into socket-address"
  (let ((colon-pos (position #\: address-string :from-end t)))
    (if colon-pos
        (let ((ip (subseq address-string 0 colon-pos))
              (port-str (subseq address-string (1+ colon-pos))))
          (handler-case
              (let ((port (parse-integer port-str)))
                (unless (<= 0 port 65535)
                  (error "Port out of range"))
                (make-socket-address ip port))
            (error ()
              (error 'errors:network-error 
                     :message (format nil "Invalid address format: ~A" address-string)))))
        (error 'errors:network-error 
               :message (format nil "Invalid address format (missing port): ~A" address-string)))))

(defun normalize-address (address)
  "Convert various address representations to socket-address"
  (etypecase address
    (types:socket-address address)
    (string (parse-address address))
    (cons (make-socket-address (first address) (second address)))))

;;; ============================================================================
;;; sockaddr_in Structure Manipulation
;;; ============================================================================

(defun sockaddr-size ()
  "Return size of sockaddr_in structure"
  16) ; sizeof(struct sockaddr_in) on Linux

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Linux version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
                 (sb-alien:alien-sap addr))))
    ;; sin_family (2 bytes)
    (setf (sb-sys:sap-ref-16 sap 0) const:+af-inet+)
    ;; sin_port (network byte order) 
    (setf (sb-sys:sap-ref-16 sap 2)
          (logior (ash (logand port #xff) 8)
                  (ash (logand port #xff00) -8)))
    ;; sin_addr (convert IP string to network byte order)
    (cond
      ((string= ip-address "0.0.0.0")
       ;; INADDR_ANY
       (setf (sb-sys:sap-ref-32 sap 4) 0))
      ((is-ipv4-address-p ip-address)
       (let ((ip-parts (mapcar #'parse-integer 
                               (split-string ip-address #\.))))
         (setf (sb-sys:sap-ref-32 sap 4)
               (logior (ash (first ip-parts) 0)
                       (ash (second ip-parts) 8)
                       (ash (third ip-parts) 16)
                       (ash (fourth ip-parts) 24)))))
      (t
       (error 'errors:network-error 
              :message (format nil "Invalid IPv4 address: ~A" ip-address))))
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-sys:sap-ref-8 sap i) 0))))

(defun parse-sockaddr-in (addr)
  "Parse sockaddr_in structure to extract IP and port"
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         ;; Extract port (network byte order)
         (port-bytes (sb-sys:sap-ref-16 sap 2))
         (port (logior (ash (logand port-bytes #xff) 8)
                       (ash (logand port-bytes #xff00) -8)))
         ;; Extract IP address (host byte order on little-endian)
         (ip-bytes (sb-sys:sap-ref-32 sap 4))
         (ip (format nil "~D.~D.~D.~D"
                     (ldb (byte 8 0) ip-bytes)
                     (ldb (byte 8 8) ip-bytes)
                     (ldb (byte 8 16) ip-bytes)
                     (ldb (byte 8 24) ip-bytes))))
    (make-socket-address ip port)))

;;; ============================================================================
;;; DNS Resolution
;;; ============================================================================

(defun resolve-address (hostname-or-address &optional (port 0))
  "Resolve hostname to socket addresses using getaddrinfo"
  (cond
    ;; If it's already an IP address, use it directly
    ((is-ip-address-p hostname-or-address)
     (list (make-socket-address hostname-or-address port)))
    
    ;; Special case for localhost
    ((or (string= hostname-or-address "localhost")
         (string= hostname-or-address "localhost.localdomain"))
     (list (make-socket-address "127.0.0.1" port)))
    
    ;; Otherwise use DNS resolution
    (t
     (handler-case
         (dns-resolve-hostname hostname-or-address port)
       (error (e)
         (error 'errors:network-error 
                :message (format nil "Failed to resolve ~A: ~A" 
                                 hostname-or-address e)))))))

(defun dns-resolve-hostname (hostname port)
  "Resolve hostname to IP addresses using getaddrinfo"
  (lib:with-foreign-memory ((hints :char :count 48)  ; sizeof(struct addrinfo)
                            (result-ptr :pointer :count 1))
    ;; Initialize hints structure to zero
    (loop for i from 0 below 48 do (setf (sb-sys:sap-ref-8 hints i) 0))
    
    ;; Set hints: ai_family = AF_INET, ai_socktype = SOCK_STREAM
    (setf (sb-sys:sap-ref-32 hints 0) 0)    ; ai_flags = 0
    (setf (sb-sys:sap-ref-32 hints 4) const:+af-inet+)    ; ai_family = AF_INET
    (setf (sb-sys:sap-ref-32 hints 8) const:+sock-stream+)  ; ai_socktype = SOCK_STREAM
    (setf (sb-sys:sap-ref-32 hints 12) 0)   ; ai_protocol = 0
    
    ;; Call getaddrinfo
    (let* ((port-str (if port (format nil "~D" port) "0"))
           (result (lib:shared-call '("getaddrinfo" "libc")
                                    :int 
                                    '(:c-string :c-string :pointer :pointer)
                                    hostname port-str hints result-ptr)))
      (if (= result 0)
          (let* ((addrinfo-ptr (sb-sys:sap-ref-sap result-ptr 0))
                 (addrinfo-list (parse-addrinfo-results addrinfo-ptr)))
            ;; Free the result after processing
            (lib:shared-call '("freeaddrinfo" "libc")
                             :void '(:pointer)
                             addrinfo-ptr)
            (or addrinfo-list
                (error 'errors:network-error
                       :message (format nil "No addresses found for ~A" hostname))))
          ;; getaddrinfo failed
          (error 'errors:network-error
                 :message (format nil "getaddrinfo failed with code ~D for hostname ~A" 
                                  result hostname))))))

(defun parse-addrinfo-results (addrinfo-ptr)
  "Parse linked list of addrinfo structures"
  (let ((results '())
        (current addrinfo-ptr))
    (loop while (and current (not (sb-sys:sap= current (sb-sys:int-sap 0))))
          do (let* ((ai-family (sb-sys:sap-ref-32 current 4))     ; ai_family at offset 4
                    (ai-addr-ptr (sb-sys:sap-ref-sap current 24))) ; ai_addr at offset 24 on Linux
               ;; Check if ai_addr is valid and it's IPv4
               (when (and ai-addr-ptr 
                          (not (sb-sys:sap= ai-addr-ptr (sb-sys:int-sap 0)))
                          (= ai-family const:+af-inet+))
                 (let ((socket-addr (parse-sockaddr-in ai-addr-ptr)))
                   (push socket-addr results)))
               ;; Move to next addrinfo in linked list
               (setf current (sb-sys:sap-ref-sap current 40))) ; ai_next at offset 40
    (nreverse results)))