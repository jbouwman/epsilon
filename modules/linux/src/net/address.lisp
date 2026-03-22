;;;; Address Handling for Linux Networking
;;;;
;;;; This module handles socket address creation, parsing, and resolution.

(defpackage epsilon.net.address
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (str epsilon.string)
   (seq epsilon.sequence)
   (lib epsilon.foreign))
  (:export
   ;; Address creation
   #:make-socket-address
   #:parse-address
   #:resolve-address

   ;; Sockaddr utilities
   #:make-sockaddr-in-into
   #:parse-sockaddr-in)
  (:enter t))

;;; ============================================================================
;;; Sockaddr Utilities
;;; ============================================================================

(defun make-sockaddr-in-into (addr ip-address port)
  "Fill sockaddr_in structure for IPv4 (Linux version) into provided buffer"
  ;; addr is a SAP from epsilon.foreign, use sb-sys:sap-ref-* functions
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
    (types:make-socket-address ip port)))

;;; ============================================================================
;;; Address Creation and Resolution
;;; ============================================================================

(defun make-socket-address (ip port)
  "Create a socket address from IP string and port number"
  (types:make-socket-address ip port))

(defun parse-address (address-string)
  "Parse an address string like '127.0.0.1:8080' into socket-address"
  (let ((colon-pos (position #\: address-string :from-end t)))
    (if colon-pos
        (make-socket-address
         (subseq address-string 0 colon-pos)
         (parse-integer (subseq address-string (1+ colon-pos))))
        (error "Invalid address format: ~A" address-string))))

(defun resolve-address (hostname-or-address port)
  "Resolve hostname to socket addresses using getaddrinfo.
   Returns a list of socket-address objects.
   Signals network-error with detailed message on failure."
  (cond
    ;; If it looks like an IP address, use it directly
    ((and (stringp hostname-or-address)
          (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                 hostname-or-address))
     (list (make-socket-address hostname-or-address port)))

    ;; If it's "localhost", resolve to 127.0.0.1
    ((string= hostname-or-address "localhost")
     (list (make-socket-address "127.0.0.1" port)))

    ;; Otherwise use getaddrinfo for DNS resolution
    (t
     ;; dns-resolve-hostname already signals network-error with details
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
  "Parse linked list of addrinfo structures"
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
                           ;; Parse IPv4 address
                           (let* ((port-bytes (sb-sys:sap-ref-16 ai-addr-ptr 2))
                                  (port (logior (ash (logand port-bytes #xff) 8)
                                                (ash (logand port-bytes #xff00) -8)))
                                  (ip (format nil "~D.~D.~D.~D"
                                              (sb-sys:sap-ref-8 ai-addr-ptr 4)
                                              (sb-sys:sap-ref-8 ai-addr-ptr 5)
                                              (sb-sys:sap-ref-8 ai-addr-ptr 6)
                                              (sb-sys:sap-ref-8 ai-addr-ptr 7))))
                             (make-socket-address ip port)))
                          (t nil))))
                   (when socket-addr
                     (push socket-addr results))))
               ;; Move to next addrinfo in linked list
               (setf current (sb-sys:sap-ref-sap current 40)))) ; ai_next at offset 40
    (nreverse results)))
