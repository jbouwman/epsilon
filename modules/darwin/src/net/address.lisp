;;;; Address utilities and resolution

(defpackage epsilon.net.address
  (:use cl)
  (:local-nicknames
   (lib epsilon.foreign)
   (log epsilon.log)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:import-from epsilon.net.constants
   +af-inet+ +af-inet6+ +sock-stream+ +sockaddr-in6-size+ +ipv6-addr-size+)
  (:import-from epsilon.net.core
   socket-address socket-address-ip socket-address-port socket-address-family
   ipv4-address ipv6-address ipv6-address-words)
  (:export
   ;; Address creation and parsing
   make-sockaddr-in-into
   make-sockaddr-in
   parse-sockaddr-in
   make-sockaddr-in6-into
   parse-sockaddr-in6
   
   ;; IP address parsing
   parse-ipv4-address
   parse-ipv6-address
   expand-ipv6-address
   
   ;; Address resolution
   make-socket-address
   resolve-address
   parse-address
   normalize-address
   
   ;; Utilities
   detect-address-family
   detect-ip-address
   dns-resolve-hostname))

(in-package epsilon.net.address)

;;; ============================================================================
;;; Address Utilities
;;; ============================================================================

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Darwin version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign, use sb-sys:sap-ref-* functions
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
                 (sb-alien:alien-sap addr))))
    (log:debug "make-sockaddr-in-into: Setting up address ~A:~D~%" ip-address port)
    (finish-output)
    
    ;; sin_len
    (setf (sb-sys:sap-ref-8 sap 0) 16)
    (log:debug "make-sockaddr-in-into: Set sin_len=16~%")
    
    ;; sin_family 
    (setf (sb-sys:sap-ref-8 sap 1) +af-inet+)
    (log:debug "make-sockaddr-in-into: Set sin_family=~D~%" +af-inet+)
    
    ;; sin_port (network byte order - set bytes directly)
    ;; For port 6379 (0x18EB): we need bytes 0x18 0xEB in memory
    (setf (sb-sys:sap-ref-8 sap 2) (ash (logand port #xFF00) -8))  ; high byte
    (setf (sb-sys:sap-ref-8 sap 3) (logand port #xFF))             ; low byte
    (log:debug "make-sockaddr-in-into: Set sin_port=~D (bytes: ~2,'0X ~2,'0X)~%" 
               port (sb-sys:sap-ref-8 sap 2) (sb-sys:sap-ref-8 sap 3))
    
    ;; sin_addr (convert IP string to network byte order)
    (let* ((ip-parts (mapcar #'parse-integer 
                             (seq:realize (str:split #\. ip-address))))
           (ip-addr-be (logior (ash (first ip-parts) 24)
                               (ash (second ip-parts) 16)
                               (ash (third ip-parts) 8)
                               (fourth ip-parts))))
      (setf (sb-sys:sap-ref-32 sap 4) ip-addr-be)
      (log:debug "make-sockaddr-in-into: Set sin_addr=~D.~D.~D.~D (BE: ~X)~%" 
                 (first ip-parts) (second ip-parts) (third ip-parts) (fourth ip-parts) ip-addr-be))
    
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-sys:sap-ref-8 sap i) 0))
    (log:debug "make-sockaddr-in-into: Zeroed sin_zero fields~%")
    (finish-output)))

;; make-sockaddr-in removed - use make-sockaddr-in-into instead

(defun parse-sockaddr-in (addr)
  "Parse sockaddr_in structure to extract IP and port"
  ;; addr could be a SAP or alien value, normalize to SAP
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         (port-bytes (sb-sys:sap-ref-16 sap 2))
         (port (logior (ash (logand port-bytes #xff) 8)
                       (ash (logand port-bytes #xff00) -8)))
         ;; Read IP address bytes individually (they're in network byte order)
         (ip (format nil "~D.~D.~D.~D"
                     (sb-sys:sap-ref-8 sap 4)
                     (sb-sys:sap-ref-8 sap 5)
                     (sb-sys:sap-ref-8 sap 6)
                     (sb-sys:sap-ref-8 sap 7))))
    (make-instance 'socket-address :ip ip :port port :family :ipv4)))

(defun make-sockaddr-in6-into (addr ip-address port)
  "Fill sockaddr_in6 structure for IPv6 into provided buffer"
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
                 (sb-alien:alien-sap addr))))
    ;; sin6_len
    (setf (sb-sys:sap-ref-8 sap 0) +sockaddr-in6-size+)
    ;; sin6_family
    (setf (sb-sys:sap-ref-8 sap 1) +af-inet6+)
    ;; sin6_port (network byte order)
    (setf (sb-sys:sap-ref-8 sap 2) (ash (logand port #xFF00) -8))
    (setf (sb-sys:sap-ref-8 sap 3) (logand port #xFF))
    ;; sin6_flowinfo (4 bytes - set to 0)
    (setf (sb-sys:sap-ref-32 sap 4) 0)
    ;; sin6_addr (16 bytes for IPv6 address)
    (let ((ipv6-obj (parse-ipv6-address ip-address)))
      (if ipv6-obj
          (loop for i from 0 below 8
                for offset from 8 by 2
                do (setf (sb-sys:sap-ref-16 sap offset)
                         (aref (ipv6-address-words ipv6-obj) i)))
          ;; Fallback - zero address
          (loop for i from 8 to 23
                do (setf (sb-sys:sap-ref-8 sap i) 0))))
    ;; sin6_scope_id (4 bytes - set to 0)
    (setf (sb-sys:sap-ref-32 sap 24) 0)))

(defun parse-sockaddr-in6 (addr)
  "Parse sockaddr_in6 structure to extract IPv6 address and port"
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         (port-bytes (sb-sys:sap-ref-16 sap 2))
         (port (logior (ash (logand port-bytes #xff) 8)
                       (ash (logand port-bytes #xff00) -8)))
         (words (make-array 8 :element-type '(unsigned-byte 16))))
    ;; Extract IPv6 address words (network byte order - need to swap bytes)
    (loop for i from 0 below 8
          for offset from 8 by 2
          do (let ((word (sb-sys:sap-ref-16 sap offset)))
               ;; Swap bytes from little-endian to big-endian
               (setf (aref words i) (logior (ash (logand word #xff) 8)
                                            (ash (logand word #xff00) -8)))))
    ;; Convert to string representation
    (let ((ip (format nil "~{~4,'0X~^:~}" (coerce words 'list))))
      (make-instance 'socket-address :ip ip :port port :family :ipv6))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun normalize-address (address)
  "Convert an address specification to a socket-address object"
  (etypecase address
    (socket-address address)
    (string 
     (multiple-value-bind (host port)
         (parse-address address)
       (make-socket-address host port)))))

;;; ============================================================================
;;; Address Resolution
;;; ============================================================================

(defun parse-ipv4-address (ip-string)
  "Parse IPv4 address string into octets"
  (let ((parts (mapcar #'parse-integer 
                       (seq:realize (str:split "." ip-string)))))
    (when (and (= (length parts) 4)
               (every (lambda (x) (and (>= x 0) (<= x 255))) parts))
      (make-instance 'ipv4-address 
                     :octets (coerce parts '(vector (unsigned-byte 8) 4))))))

(defun parse-ipv6-address (ip-string)
  "Parse IPv6 address string into words"
  (let* ((expanded (expand-ipv6-address ip-string))
         (parts (seq:realize (str:split ":" expanded))))
    (when (= (length parts) 8)
      (let ((words (mapcar (lambda (x) (parse-integer x :radix 16)) parts)))
        (when (every (lambda (x) (and (>= x 0) (<= x 65535))) words)
          (make-instance 'ipv6-address
                         :words (coerce words '(vector (unsigned-byte 16) 8))))))))

(defun expand-ipv6-address (ip-string)
  "Expand compressed IPv6 address notation"
  ;; Simple implementation - handle :: expansion
  (if (search "::" ip-string)
      (let* ((parts (seq:realize (str:split "::" ip-string)))
             (left (if (first parts) (seq:realize (str:split ":" (first parts))) '()))
             (right (if (second parts) (seq:realize (str:split ":" (second parts))) '()))
             (missing (- 8 (length left) (length right))))
        (format nil "~{~A~^:~}" 
                (append left 
                        (make-list missing :initial-element "0000")
                        right)))
      ip-string))

(defun detect-address-family (host)
  "Detect if host is IPv4, IPv6, or hostname"
  (cond
    ((position #\. host) :ipv4)    ; Contains dots - likely IPv4
    ((position #\: host) :ipv6)    ; Contains colons - likely IPv6
    (t :hostname)))                ; Otherwise hostname

(defun make-socket-address (host port)
  "Create a socket address from host and port"
  (let ((family (detect-address-family host)))
    (make-instance 'socket-address 
                   :ip host 
                   :port port
                   :family (if (eq family :hostname) :ipv4 family))))

(defun resolve-address (address-spec)
  "Resolve an address specification to socket addresses"
  (etypecase address-spec
    (socket-address (list address-spec))
    (string
     (let* ((addr (parse-address address-spec))
            (host (socket-address-ip addr))
            (port (socket-address-port addr)))
       (if (or (detect-ip-address host) (null host))
           ;; It's already an IP address or localhost
           (list addr)
           ;; Need DNS resolution
           (dns-resolve-hostname host port))))
    (list
       (list (make-socket-address (first address-spec) (second address-spec))))))

(defun detect-ip-address (host)
  "Check if host string is already an IP address"
  (or (and (position #\. host)
           (ignore-errors (parse-ipv4-address host)))
      (and (position #\: host)
           (ignore-errors (parse-ipv6-address host)))))

(defun dns-resolve-hostname (hostname port)
  "Resolve hostname to IP addresses using getaddrinfo"
    (handler-case
        (lib:with-foreign-memory ((hints :char :count 48)  ; sizeof(struct addrinfo) = 48 on Darwin
                                  (result-ptr :pointer :count 1))
          ;; Initialize hints structure
          (loop for i from 0 below 48 do (setf (sb-sys:sap-ref-8 hints i) 0))
          
          ;; Set hints: ai_family = AF_UNSPEC (0), ai_socktype = SOCK_STREAM (1)
          (setf (sb-sys:sap-ref-32 hints 4) 0)    ; ai_family = AF_UNSPEC
          (setf (sb-sys:sap-ref-32 hints 8) +sock-stream+)  ; ai_socktype
          
          ;; Call getaddrinfo
          (let* ((port-str (if port (format nil "~D" port) "0"))
                 (result (lib:shared-call '("getaddrinfo" "/usr/lib/libSystem.B.dylib")
                                          :int 
                                          '(:c-string :c-string :pointer :pointer)
                                          hostname port-str hints result-ptr)))
            (if (= result 0)
                (let ((addrinfo-list (parse-addrinfo-results (sb-sys:sap-ref-sap result-ptr 0))))
                  ;; Free the result
                  (lib:shared-call '("freeaddrinfo" "/usr/lib/libSystem.B.dylib")
                                   :void '(:pointer)
                                   (sb-sys:sap-ref-sap result-ptr 0))
                  addrinfo-list)
                (progn
                  (log:warn "DNS resolution failed for ~A: error ~D" hostname result)
                  ;; Fallback to treating as literal
                  (list (make-socket-address hostname (or port 80)))))))
      (error (e)
        (log:warn "DNS resolution error for ~A: ~A" hostname e)
        ;; Fallback
        (list (make-socket-address hostname (or port 80))))))

(defun parse-addrinfo-results (addrinfo-ptr)
  "Parse linked list of addrinfo structures"
    (let ((results '())
          (current addrinfo-ptr))
      (loop while (and current (not (sb-sys:sap= current (sb-sys:int-sap 0))))
            do (let* ((ai-addr-ptr (sb-sys:sap-ref-sap current 32)))  ; ai_addr is at offset 32
                 ;; Check if ai_addr is null before dereferencing
                 (when (and ai-addr-ptr (not (sb-sys:sap= ai-addr-ptr (sb-sys:int-sap 0))))
                   (let* ((ai-family (sb-sys:sap-ref-8 ai-addr-ptr 1))  ; sa_family is a single byte at offset 1
                          (socket-addr (cond 
                                         ((= ai-family +af-inet+) 
                                          (parse-sockaddr-in ai-addr-ptr))
                                         ((= ai-family +af-inet6+) 
                                          (parse-sockaddr-in6 ai-addr-ptr))
                                         (t nil))))
                     (when socket-addr
                       (push socket-addr results))))
                 ;; Move to next in linked list
                 (setf current (sb-sys:sap-ref-sap current 40))))  ; ai_next is at offset 40
      (nreverse results)))

(defun parse-address (string)
  "Parse an address string into a socket-address object"
  (let* ((colon-pos (position #\: string :from-end t))
         (host (if colon-pos
                   (subseq string 0 colon-pos)
                   string))
         (port (if colon-pos
                   (parse-integer (subseq string (1+ colon-pos)))
                   80)))
    (make-socket-address host port)))