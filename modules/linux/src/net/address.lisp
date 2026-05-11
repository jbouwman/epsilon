;;;; Address Handling for Linux Networking
;;;;
;;;; This module handles socket address creation, parsing, and resolution.
;;;; Supports both IPv4 and IPv6 address families.

(defpackage epsilon.net.address
  (:use cl)
  (:import
   (epsilon.net.constants const)
   (epsilon.net.errors errors)
   (epsilon.net.types types)
   (epsilon.string str)
   (epsilon.sequence seq)
   (epsilon.foreign lib))
  (:export
   ;; Address creation
   #:make-socket-address
   #:parse-address
   #:resolve-address
   #:ip-literal-p

   ;; Sockaddr utilities - IPv4
   #:make-sockaddr-in-into
   #:parse-sockaddr-in

   ;; Sockaddr utilities - IPv6
   #:make-sockaddr-in6-into
   #:parse-sockaddr-in6
   #:parse-ipv6-address
   #:expand-ipv6-address

   ;; Family-aware dispatch
   #:fill-sockaddr-for-address
   #:sockaddr-size-for-family
   #:detect-address-family
   #:parse-sockaddr-by-family
   #:socket-family-constant

   ;; Unix socket address
   #:make-sockaddr-un-into))

;;; ============================================================================
;;; IPv4 Sockaddr Utilities
;;; ============================================================================

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Linux version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign, use sb-sys:sap-ref-* functions
  (unless (and (stringp ip-address)
               (plusp (length ip-address))
               (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                      ip-address))
    (error 'errors:network-error
           :message (format nil "make-sockaddr-in-into: expected an IPv4 literal (digits and dots), got ~S. Hostnames must be resolved via address:resolve-address before filling sockaddr_in."
                            ip-address)))
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
    (let ((ip-parts (mapcar #'parse-integer
                            (seq:realize (str:split #\. ip-address)))))
      ;; Store in network byte order (big-endian)
      (setf (sb-sys:sap-ref-8 sap 4) (first ip-parts))
      (setf (sb-sys:sap-ref-8 sap 5) (second ip-parts))
      (setf (sb-sys:sap-ref-8 sap 6) (third ip-parts))
      (setf (sb-sys:sap-ref-8 sap 7) (fourth ip-parts)))
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-sys:sap-ref-8 sap i) 0))))

(defun parse-sockaddr-in (addr)
  "Parse sockaddr_in structure to extract IP and port"
  ;; addr could be a SAP or alien value, normalize to SAP
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         (port-bytes (sb-sys:sap-ref-16 sap 2))
         (port (logior (ash (logand port-bytes #xff) 8)
                       (ash (logand port-bytes #xff00) -8)))
         ;; Read IP address bytes directly
         (ip (format nil "~D.~D.~D.~D"
                     (sb-sys:sap-ref-8 sap 4)
                     (sb-sys:sap-ref-8 sap 5)
                     (sb-sys:sap-ref-8 sap 6)
                     (sb-sys:sap-ref-8 sap 7))))
    (types:make-socket-address ip port :family :ipv4)))

;;; ============================================================================
;;; IPv6 Sockaddr Utilities
;;; ============================================================================

(defun parse-ipv6-address (ip-string)
  "Parse IPv6 address string into a vector of 8 16-bit words.
   Returns the word vector, or NIL if parsing fails."
  (let* ((expanded (expand-ipv6-address ip-string))
         (parts (seq:realize (str:split #\: expanded))))
    (when (= (length parts) 8)
      (let ((words (mapcar (lambda (x) (parse-integer x :radix 16 :junk-allowed t)) parts)))
        (when (every (lambda (x) (and x (>= x 0) (<= x 65535))) words)
          (coerce words '(vector (unsigned-byte 16) 8)))))))

(defun expand-ipv6-address (ip-string)
  "Expand compressed IPv6 address notation (:: expansion)."
  (if (search "::" ip-string)
      (let* ((dc-pos (search "::" ip-string))
             (left-str (subseq ip-string 0 dc-pos))
             (right-str (subseq ip-string (+ dc-pos 2)))
             (left (if (plusp (length left-str))
                       (seq:realize (str:split #\: left-str)) '()))
             (right (if (plusp (length right-str))
                        (seq:realize (str:split #\: right-str)) '()))
             (missing (- 8 (length left) (length right))))
        (format nil "~{~A~^:~}"
                (append left
                        (make-list missing :initial-element "0")
                        right)))
      ip-string))

(defun make-sockaddr-in6-into (addr ip-address port)
  "Fill sockaddr_in6 structure for IPv6 (Linux version) into provided buffer.
   Linux sockaddr_in6 layout (28 bytes):
     Offset 0-1:   sin6_family (AF_INET6 = 10, uint16)
     Offset 2-3:   sin6_port (network byte order)
     Offset 4-7:   sin6_flowinfo (uint32, set to 0)
     Offset 8-23:  sin6_addr (16 bytes, 8 x uint16 in network byte order)
     Offset 24-27: sin6_scope_id (uint32, set to 0)"
  (let ((sap (if (typep addr 'sb-sys:system-area-pointer)
                 addr
                 (sb-alien:alien-sap addr))))
    ;; sin6_family (2 bytes)
    (setf (sb-sys:sap-ref-16 sap 0) const:+af-inet6+)
    ;; sin6_port (network byte order)
    (setf (sb-sys:sap-ref-8 sap 2) (ash (logand port #xff00) -8))
    (setf (sb-sys:sap-ref-8 sap 3) (logand port #xff))
    ;; sin6_flowinfo (4 bytes - set to 0)
    (setf (sb-sys:sap-ref-32 sap 4) 0)
    ;; sin6_addr (16 bytes)
    (let ((words (parse-ipv6-address ip-address)))
      (if words
          ;; Write each 16-bit word in network byte order (big-endian)
          (loop for i from 0 below 8
                for offset from 8 by 2
                do (let ((word (aref words i)))
                     (setf (sb-sys:sap-ref-8 sap offset) (ash (logand word #xff00) -8))
                     (setf (sb-sys:sap-ref-8 sap (1+ offset)) (logand word #xff))))
          ;; Fallback: zero address
          (loop for i from 8 to 23
                do (setf (sb-sys:sap-ref-8 sap i) 0))))
    ;; sin6_scope_id (4 bytes - set to 0)
    (setf (sb-sys:sap-ref-32 sap 24) 0)))

(defun parse-sockaddr-in6 (addr)
  "Parse sockaddr_in6 structure to extract IPv6 address and port"
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         ;; sin6_port at offset 2-3 (network byte order)
         (port (logior (ash (sb-sys:sap-ref-8 sap 2) 8)
                       (sb-sys:sap-ref-8 sap 3)))
         ;; sin6_addr at offset 8-23 (8 words in network byte order)
         (words (loop for offset from 8 by 2 repeat 8
                      collect (logior (ash (sb-sys:sap-ref-8 sap offset) 8)
                                      (sb-sys:sap-ref-8 sap (1+ offset))))))
    (let ((ip (format-ipv6-words words)))
      (types:make-socket-address ip port :family :ipv6))))

(defun format-ipv6-words (words)
  "Format a list of 8 16-bit words as a canonical IPv6 string.
   Uses :: compression for the longest run of zero words."
  (let ((best-start -1)
        (best-len 0)
        (cur-start -1)
        (cur-len 0))
    ;; Find longest run of zeros
    (loop for i from 0 below 8
          for w in words
          do (if (zerop w)
                 (progn
                   (when (= cur-start -1) (setf cur-start i))
                   (incf cur-len)
                   (when (> cur-len best-len)
                     (setf best-start cur-start)
                     (setf best-len cur-len)))
                 (progn
                   (setf cur-start -1)
                   (setf cur-len 0))))
    (if (>= best-len 2)
        ;; Use :: compression
        (let ((left (subseq words 0 best-start))
              (right (subseq words (+ best-start best-len))))
          (format nil "~{~(~X~)~^:~}::~{~(~X~)~^:~}" left right))
        ;; No compression
        (format nil "~{~(~X~)~^:~}" words))))

;;; ============================================================================
;;; Family-Aware Dispatch
;;; ============================================================================

(defun detect-address-family (host)
  "Detect address family from host string.
   Returns :ipv6 for any string containing a colon, :ipv4 only for
   dotted-quad literals (digits and dots only), and :hostname
   otherwise.  Strings like 'foo.example.com' are :hostname so the
   caller resolves them via DNS instead of treating them as bogus
   IPv4 literals."
  (cond
    ((null host) :ipv4)
    ((position #\: host) :ipv6)
    ((and (position #\. host)
          (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                 host))
     :ipv4)
    (t :hostname)))

(defun sockaddr-size-for-family (family)
  "Return the sockaddr structure size for the given address family."
  (ecase family
    (:ipv4 16)
    (:ipv6 const:+sockaddr-in6-size+)))

(defun fill-sockaddr-for-address (addr socket-addr)
  "Fill sockaddr buffer ADDR for SOCKET-ADDR, using appropriate IPv4/IPv6 format.
   Returns the size of the sockaddr structure."
  (let ((family (types:socket-address-family socket-addr))
        (ip (types:socket-address-ip socket-addr))
        (port (types:socket-address-port socket-addr)))
    (ecase family
      (:ipv4
       (make-sockaddr-in-into addr ip port)
       16)
      (:ipv6
       (make-sockaddr-in6-into addr ip port)
       const:+sockaddr-in6-size+))))

(defun parse-sockaddr-by-family (addr)
  "Parse a sockaddr buffer by inspecting the sa_family field at offset 0.
   On Linux, sa_family is a 16-bit value at offset 0."
  (let* ((sap (if (typep addr 'sb-sys:system-area-pointer)
                  addr
                  (sb-alien:alien-sap addr)))
         (family (sb-sys:sap-ref-16 sap 0)))
    (cond
      ((= family const:+af-inet+) (parse-sockaddr-in sap))
      ((= family const:+af-inet6+) (parse-sockaddr-in6 sap))
      (t (error "Unknown address family: ~D" family)))))

(defun socket-family-constant (family)
  "Convert socket-address family keyword to AF_* constant."
  (ecase family
    (:ipv4 const:+af-inet+)
    (:ipv6 const:+af-inet6+)))

;;; ============================================================================
;;; Address Creation and Resolution
;;; ============================================================================

(defun ip-literal-p (string)
  "Return T if STRING is an IP address literal (IPv4 or IPv6), not a hostname."
  (or ;; IPv4: digits and dots only
      (every (lambda (c) (or (digit-char-p c) (char= c #\.))) string)
      ;; IPv6: contains colons
      (position #\: string)))

(defun make-socket-address (host-or-ip port &key family)
  "Create a socket address from an IP string or hostname and port number.
   If HOST-OR-IP is a hostname (not an IP literal), resolves it via DNS
   and returns a socket-address for the first result.
   FAMILY is auto-detected from IP if not specified."
  (if (ip-literal-p host-or-ip)
      (types:make-socket-address host-or-ip port :family family)
      ;; Hostname: resolve via DNS, return the first address
      (let ((addrs (resolve-address host-or-ip port)))
        (if addrs
            (first addrs)
            (error "Failed to resolve hostname: ~A" host-or-ip)))))

(defun parse-address (address-string)
  "Parse an address string into socket-address.
   Supports IPv4 ('127.0.0.1:8080'), IPv6 bracket notation ('[::1]:8080'),
   and bare IPv6 with port ('::', '::1')."
  (cond
    ;; IPv6 bracket notation: [::1]:8080
    ((and (plusp (length address-string))
          (char= (char address-string 0) #\[))
     (let ((bracket-end (position #\] address-string)))
       (unless bracket-end
         (error "Invalid IPv6 bracket notation: ~A" address-string))
       (let ((ip (subseq address-string 1 bracket-end))
             (rest (subseq address-string (1+ bracket-end))))
         (if (and (plusp (length rest)) (char= (char rest 0) #\:))
             (make-socket-address ip (parse-integer (subseq rest 1)) :family :ipv6)
             (make-socket-address ip 0 :family :ipv6)))))
    ;; IPv4: last colon separates host:port and host has dots
    (t
     (let ((colon-pos (position #\: address-string :from-end t)))
       (if colon-pos
           (let ((host (subseq address-string 0 colon-pos)))
             (if (position #\: host)
                 ;; Multiple colons = bare IPv6 without port
                 (make-socket-address address-string 0 :family :ipv6)
                 ;; Single colon = host:port
                 (make-socket-address host
                                      (parse-integer (subseq address-string (1+ colon-pos))))))
           (error "Invalid address format: ~A" address-string))))))

(defun resolve-address (hostname-or-address port)
  "Resolve hostname to socket addresses using getaddrinfo.
   Returns a list of socket-address objects.
   Signals network-error with detailed message on failure."
  (cond
    ;; IPv4 literal (digits and dots only)
    ((and (stringp hostname-or-address)
          (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                 hostname-or-address))
     (list (make-socket-address hostname-or-address port :family :ipv4)))

    ;; IPv6 literal (contains colons)
    ((and (stringp hostname-or-address)
          (position #\: hostname-or-address))
     (list (make-socket-address hostname-or-address port :family :ipv6)))

    ;; "localhost" resolves to both IPv4 and IPv6 loopback
    ((string= hostname-or-address "localhost")
     (list (make-socket-address "127.0.0.1" port :family :ipv4)
           (make-socket-address "::1" port :family :ipv6)))

    ;; Otherwise use getaddrinfo for DNS resolution
    (t
     (dns-resolve-hostname hostname-or-address port))))

(defun gai-error-string (code)
  "Convert getaddrinfo error code to human-readable string.
   See man getaddrinfo for error codes."
  (case code
    (-2 "EAI_NONAME - Name or service not known")
    (-3 "EAI_AGAIN - Temporary failure in name resolution")
    (-4 "EAI_FAIL - Non-recoverable failure in name resolution")
    (-5 "EAI_FAMILY - Address family not supported")
    (-6 "EAI_SOCKTYPE - Socket type not supported")
    (-7 "EAI_SERVICE - Service not supported for socket type")
    (-10 "EAI_MEMORY - Memory allocation failure")
    (-11 "EAI_SYSTEM - System error (check errno)")
    (-12 "EAI_OVERFLOW - Argument buffer overflow")
    ;; Positive codes on some systems
    (1 "EAI_BADFLAGS - Invalid flags")
    (2 "EAI_NONAME - Name or service not known")
    (3 "EAI_AGAIN - Temporary failure in name resolution")
    (4 "EAI_FAIL - Non-recoverable failure in name resolution")
    (5 "EAI_FAMILY - Address family not supported")
    (6 "EAI_SOCKTYPE - Socket type not supported")
    (7 "EAI_SERVICE - Service not supported for socket type")
    (8 "EAI_NONAME - Name or service not known")
    (10 "EAI_MEMORY - Memory allocation failure")
    (11 "EAI_SYSTEM - System error (check errno)")
    (t (format nil "Unknown getaddrinfo error (code ~D)" code))))

(defun dns-resolve-hostname (hostname port)
  "Resolve hostname to IP addresses using getaddrinfo"
  (handler-case
      (lib:with-foreign-memory ((hints :char :count 48)  ; sizeof(struct addrinfo)
                                (result-ptr :pointer :count 1))
        ;; Initialize hints structure to zero
        (loop for i from 0 below 48 do (setf (sb-sys:sap-ref-8 hints i) 0))

        ;; Set hints: ai_family = AF_UNSPEC (0), ai_socktype = SOCK_STREAM (1)
        (setf (sb-sys:sap-ref-32 hints 0) 0)    ; ai_flags = 0
        (setf (sb-sys:sap-ref-32 hints 4) 0)    ; ai_family = AF_UNSPEC
        (setf (sb-sys:sap-ref-32 hints 8) const:+sock-stream+)  ; ai_socktype = SOCK_STREAM
        (setf (sb-sys:sap-ref-32 hints 12) 0)   ; ai_protocol = 0

        ;; Call getaddrinfo
        (let* ((port-str (if port (format nil "~D" port) "0"))
               (result (lib:shared-call '("getaddrinfo" "libc")
                                        :int
                                        '(:string :string :pointer :pointer)
                                        hostname port-str hints result-ptr)))
          (if (= result 0)
              (let* ((addrinfo-ptr (sb-sys:sap-ref-sap result-ptr 0))
                     (addrinfo-list (parse-addrinfo-results addrinfo-ptr)))
                ;; Free the result after processing
                (lib:shared-call '("freeaddrinfo" "libc")
                                 :void '(:pointer)
                                 addrinfo-ptr)
                (if addrinfo-list
                    addrinfo-list
                    ;; getaddrinfo succeeded but returned no results
                    (error 'errors:network-error
                           :message (format nil "DNS lookup for ~A:~A returned no addresses"
                                            hostname port))))
              ;; getaddrinfo failed - provide detailed error message
              (error 'errors:network-error
                     :message (format nil "DNS lookup failed for ~A:~A - ~A"
                                      hostname port (gai-error-string result))))))
    (errors:network-error (e)
      ;; Re-raise network errors as-is
      (error e))
    (error (e)
      ;; Wrap other errors with context
      (error 'errors:network-error
             :message (format nil "DNS resolution error for ~A:~A - ~A"
                              hostname port e)))))

(defun parse-addrinfo-results (addrinfo-ptr)
  "Parse linked list of addrinfo structures.
   Handles both AF_INET and AF_INET6 entries."
  (let ((results '())
        (current addrinfo-ptr))
    (loop while (and current (not (sb-sys:sap= current (sb-sys:int-sap 0))))
          do (let* ((ai-family (sb-sys:sap-ref-32 current 4))     ; ai_family at offset 4
                    (ai-addr-ptr (sb-sys:sap-ref-sap current 24))) ; ai_addr at offset 24 on Linux
               ;; Check if ai_addr is valid
               (when (and ai-addr-ptr (not (sb-sys:sap= ai-addr-ptr (sb-sys:int-sap 0))))
                 (let ((socket-addr
                        (cond
                          ((= ai-family const:+af-inet+)
                           (parse-sockaddr-in ai-addr-ptr))
                          ((= ai-family const:+af-inet6+)
                           (parse-sockaddr-in6 ai-addr-ptr))
                          (t nil))))
                   (when socket-addr
                     (push socket-addr results))))
               ;; Move to next addrinfo in linked list
               (setf current (sb-sys:sap-ref-sap current 40)))) ; ai_next at offset 40
    (nreverse results)))

;;; ============================================================================
;;; Unix Domain Socket Address
;;; ============================================================================

(defun make-sockaddr-un-into (addr path)
  "Fill sockaddr_un structure (Linux layout) into provided buffer.
   Linux: sun_family is a 2-byte uint16 at offset 0, sun_path at offset 2.
   Returns the actual address length for bind/connect."
  (let* ((path-bytes (sb-ext:string-to-octets path :external-format :utf-8))
         (path-len (length path-bytes)))
    (when (>= path-len const:+sockaddr-un-path-max+)
      (error "Unix socket path too long (~D bytes, max ~D)"
             path-len (1- const:+sockaddr-un-path-max+)))
    ;; Zero the entire buffer
    (dotimes (i const:+sockaddr-un-size+)
      (setf (sb-sys:sap-ref-8 addr i) 0))
    ;; sun_family (2 bytes LE) = AF_UNIX
    (setf (sb-sys:sap-ref-8 addr 0) const:+af-unix+)
    (setf (sb-sys:sap-ref-8 addr 1) 0)
    ;; sun_path at offset 2
    (dotimes (i path-len)
      (setf (sb-sys:sap-ref-8 addr (+ 2 i)) (aref path-bytes i)))
    ;; Return address length
    (+ 2 path-len)))
