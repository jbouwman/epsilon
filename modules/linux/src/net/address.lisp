;;;; Address Handling for Linux Networking
;;;;
;;;; This module handles socket address creation, parsing, and resolution.

(defpackage epsilon.net.address
  (:use cl)
  (:local-nicknames
   (const epsilon.net.constants)
   (errors epsilon.net.errors)
   (types epsilon.net.types)
   (lib epsilon.foreign))
  (:export
   ;; Address creation
   #:make-socket-address
   #:parse-address
   #:resolve-address
   
   ;; Sockaddr utilities
   #:make-sockaddr-in-into
   #:parse-sockaddr-in
   
   ;; Utilities
   #:split-string))

(in-package epsilon.net.address)

;;; ============================================================================
;;; Utilities
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
                            (split-string ip-address #\.))))
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
  "Resolve hostname to socket addresses using getaddrinfo"
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
     (handler-case
         (dns-resolve-hostname hostname-or-address port)
       (error (e)
         (error 'errors:network-error 
                :message (format nil "Failed to resolve ~A: ~A" 
                                 hostname-or-address e)))))))

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
                                        '(:c-string :c-string :pointer :pointer)
                                        hostname port-str hints result-ptr)))
          (if (= result 0)
              (let* ((addrinfo-ptr (sb-sys:sap-ref-sap result-ptr 0))
                     (addrinfo-list (parse-addrinfo-results addrinfo-ptr)))
                ;; Free the result after processing
                (lib:shared-call '("freeaddrinfo" "libc")
                                 :void '(:pointer)
                                 addrinfo-ptr)
                addrinfo-list)
              ;; getaddrinfo failed
              (error "getaddrinfo failed with code ~D for hostname ~A" result hostname))))
    (error (e)
      ;; DNS resolution error
      (error "DNS resolution failed for ~A: ~A" hostname e))))

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
               (setf current (sb-sys:sap-ref-sap current 40))) ; ai_next at offset 40
    (nreverse results))))
