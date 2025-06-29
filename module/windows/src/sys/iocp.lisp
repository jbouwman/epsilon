(defpackage :epsilon.sys.iocp
  (:use :cl)
  (:local-nicknames
   (#:lib #:epsilon.sys.lib))
  (:export
   ;; Core IOCP operations
   #:create-io-completion-port
   #:get-queued-completion-status
   #:post-queued-completion-status
   #:close-handle
   
   ;; Overlapped I/O structures
   #:make-overlapped
   #:overlapped-internal
   #:overlapped-internal-high
   #:overlapped-offset
   #:overlapped-offset-high
   #:overlapped-hevent
   
   ;; IOCP constants
   #:+invalid-handle-value+
   #:+infinite+
   #:+wait-object-0+
   #:+wait-timeout+
   #:+wait-failed+
   
   ;; Socket integration
   #:associate-socket
   #:async-read-socket
   #:async-write-socket
   #:async-accept-socket
   #:async-connect-socket
   
   ;; High-level interface
   #:with-iocp
   #:wait-for-completion
   #:process-completions
   
   ;; Completion key handling
   #:make-completion-key
   #:completion-key-socket
   #:completion-key-operation
   #:completion-key-data
   
   ;; Event checking predicates
   #:completion-success-p
   #:completion-error-p
   #:completion-timeout-p))

(in-package :epsilon.sys.iocp)

;;;; Windows IOCP Constants

;; Handle values
(defconstant +invalid-handle-value+ -1)

;; Wait constants
(defconstant +infinite+ #xFFFFFFFF)
(defconstant +wait-object-0+ 0)
(defconstant +wait-timeout+ 258)
(defconstant +wait-failed+ #xFFFFFFFF)

;; Error codes
(defconstant +error-success+ 0)
(defconstant +error-io-pending+ 997)
(defconstant +error-operation-aborted+ 995)

;; Socket constants
(defconstant +af-inet+ 2)
(defconstant +af-inet6+ 23)
(defconstant +sock-stream+ 1)
(defconstant +sock-dgram+ 2)
(defconstant +ipproto-tcp+ 6)
(defconstant +ipproto-udp+ 17)

;;;; FFI Bindings

;; Core IOCP functions
(lib:defshared %create-io-completion-port "CreateIoCompletionPort" "kernel32" :pointer
  (file-handle :pointer)
  (existing-completion-port :pointer)
  (completion-key :pointer)
  (number-of-concurrent-threads :unsigned-long)
  :documentation "Create or associate with I/O completion port")

(lib:defshared %get-queued-completion-status "GetQueuedCompletionStatus" "kernel32" :int
  (completion-port :pointer)
  (number-of-bytes-transferred :pointer)
  (completion-key :pointer)
  (overlapped :pointer)
  (milliseconds :unsigned-long)
  :documentation "Wait for I/O completion on completion port")

(lib:defshared %post-queued-completion-status "PostQueuedCompletionStatus" "kernel32" :int
  (completion-port :pointer)
  (number-of-bytes-transferred :unsigned-long)
  (completion-key :pointer)
  (overlapped :pointer)
  :documentation "Post completion packet to completion port")

(lib:defshared %close-handle "CloseHandle" "kernel32" :int
  (handle :pointer)
  :documentation "Close handle")

;; Winsock functions
(lib:defshared %wsa-socket "WSASocketW" "ws2_32" :pointer
  (af :int) (type :int) (protocol :int) (protocol-info :pointer) (group :unsigned-long) (flags :unsigned-long)
  :documentation "Create socket with extended attributes")

(lib:defshared %bind "bind" "ws2_32" :int
  (socket :pointer) (addr :pointer) (namelen :int)
  :documentation "Bind socket to address")

(lib:defshared %listen "listen" "ws2_32" :int
  (socket :pointer) (backlog :int)
  :documentation "Listen for connections")

(lib:defshared %wsa-recv "WSARecv" "ws2_32" :int
  (socket :pointer) (buffers :pointer) (buffer-count :unsigned-long) 
  (bytes-received :pointer) (flags :pointer) (overlapped :pointer) (completion-routine :pointer)
  :documentation "Asynchronous receive")

(lib:defshared %wsa-send "WSASend" "ws2_32" :int
  (socket :pointer) (buffers :pointer) (buffer-count :unsigned-long)
  (bytes-sent :pointer) (flags :unsigned-long) (overlapped :pointer) (completion-routine :pointer)
  :documentation "Asynchronous send")

(lib:defshared %wsa-accept "WSAAccept" "ws2_32" :pointer
  (listen-socket :pointer) (addr :pointer) (addrlen :pointer) 
  (condition-func :pointer) (callback-data :pointer)
  :documentation "Accept connection with conditions")

(lib:defshared %wsa-connect "WSAConnect" "ws2_32" :int
  (socket :pointer) (name :pointer) (namelen :int) (caller-data :pointer)
  (callee-data :pointer) (sqos :pointer) (gqos :pointer)
  :documentation "Connect with quality of service")

(lib:defshared %closesocket "closesocket" "ws2_32" :int
  (socket :pointer)
  :documentation "Close socket")

;;;; OVERLAPPED Structure

;; typedef struct _OVERLAPPED {
;;   ULONG_PTR Internal;
;;   ULONG_PTR InternalHigh;
;;   union {
;;     struct {
;;       DWORD Offset;
;;       DWORD OffsetHigh;
;;     };
;;     PVOID Pointer;
;;   };
;;   HANDLE hEvent;
;; } OVERLAPPED, *LPOVERLAPPED;

(defstruct overlapped
  "Represents a Windows OVERLAPPED structure"
  (internal 0 :type (unsigned-byte 64))        ; ULONG_PTR Internal
  (internal-high 0 :type (unsigned-byte 64))   ; ULONG_PTR InternalHigh  
  (offset 0 :type (unsigned-byte 32))          ; DWORD Offset
  (offset-high 0 :type (unsigned-byte 32))     ; DWORD OffsetHigh
  (hevent 0 :type (unsigned-byte 64)))         ; HANDLE hEvent (as integer)

(defun overlapped-size ()
  "Size of OVERLAPPED structure in bytes"
  ;; Internal(8) + InternalHigh(8) + Offset(4) + OffsetHigh(4) + hEvent(8) = 32 bytes on x64
  32)

(defun pack-overlapped (overlapped buffer offset)
  "Pack OVERLAPPED structure into foreign buffer"
  (let ((ptr (sb-alien:sap+ (sb-alien:alien-sap buffer) offset)))
    ;; Internal (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 0) (overlapped-internal overlapped))
    ;; InternalHigh (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 8) (overlapped-internal-high overlapped))
    ;; Offset (4 bytes)
    (setf (sb-alien:sap-ref-32 ptr 16) (overlapped-offset overlapped))
    ;; OffsetHigh (4 bytes)
    (setf (sb-alien:sap-ref-32 ptr 20) (overlapped-offset-high overlapped))
    ;; hEvent (8 bytes)
    (setf (sb-alien:sap-ref-64 ptr 24) (overlapped-hevent overlapped))))

(defun unpack-overlapped (buffer offset)
  "Unpack OVERLAPPED structure from foreign buffer"
  (let ((ptr (sb-alien:sap+ (sb-alien:alien-sap buffer) offset)))
    (make-overlapped
     :internal (sb-alien:sap-ref-64 ptr 0)
     :internal-high (sb-alien:sap-ref-64 ptr 8)
     :offset (sb-alien:sap-ref-32 ptr 16)
     :offset-high (sb-alien:sap-ref-32 ptr 20)
     :hevent (sb-alien:sap-ref-64 ptr 24))))

;;;; Completion Key Structure

(defstruct completion-key
  "User-defined completion key for IOCP operations"
  (socket 0 :type (unsigned-byte 64))          ; Socket handle
  (operation :unknown :type keyword)           ; :read, :write, :accept, :connect
  (data nil :type t))                          ; User data

(defun completion-key-to-pointer (key)
  "Convert completion key to pointer value"
  ;; In a real implementation, we'd allocate and manage these
  ;; For now, just use the socket handle as the key
  (completion-key-socket key))

;;;; High-Level IOCP Interface

(defun create-io-completion-port (&optional file-handle existing-port completion-key (threads 0))
  "Create I/O completion port or associate file handle with existing port"
  (let ((port (%create-io-completion-port 
               (or file-handle (sb-alien:null-alien))
               (or existing-port (sb-alien:null-alien))
               (or completion-key (sb-alien:null-alien))
               threads)))
    (when (= (sb-alien:alien-sap port) +invalid-handle-value+)
      (error "Failed to create I/O completion port"))
    port))

(defun get-queued-completion-status (completion-port timeout)
  "Wait for completion status on IOCP"
  (lib:with-foreign-memory ((bytes-ptr :unsigned-long :count 1)
                            (key-ptr :pointer :count 1)
                            (overlapped-ptr :pointer :count 1))
    (let ((result (%get-queued-completion-status
                   completion-port
                   bytes-ptr
                   key-ptr
                   overlapped-ptr
                   (or timeout +infinite+))))
      (values result
              (sb-alien:deref bytes-ptr 0)
              (sb-alien:deref key-ptr 0)
              (sb-alien:deref overlapped-ptr 0)))))

(defun post-queued-completion-status (completion-port bytes-transferred completion-key overlapped)
  "Post completion status to IOCP"
  (let ((result (%post-queued-completion-status
                 completion-port
                 bytes-transferred
                 completion-key
                 overlapped)))
    (when (zerop result)
      (error "Failed to post completion status"))
    result))

(defun close-handle (handle)
  "Close Windows handle"
  (%close-handle handle))

(defmacro with-iocp ((iocp-var &optional (threads 0)) &body body)
  "Execute body with IOCP, automatically closing on exit"
  `(let ((,iocp-var (create-io-completion-port nil nil nil ,threads)))
     (unwind-protect
          (progn ,@body)
       (close-handle ,iocp-var))))

;;;; Socket Operations

(defun create-tcp-socket ()
  "Create TCP socket for Windows"
  (let ((sock (%wsa-socket +af-inet+ +sock-stream+ +ipproto-tcp+ 
                           (sb-alien:null-alien) 0 0)))
    (when (= (sb-alien:alien-sap sock) +invalid-handle-value+)
      (error "Failed to create TCP socket"))
    sock))

(defun create-udp-socket ()
  "Create UDP socket for Windows"
  (let ((sock (%wsa-socket +af-inet+ +sock-dgram+ +ipproto-udp+
                           (sb-alien:null-alien) 0 0)))
    (when (= (sb-alien:alien-sap sock) +invalid-handle-value+)
      (error "Failed to create UDP socket"))
    sock))

(defun close-socket (socket)
  "Close socket"
  (%closesocket socket))

(defun associate-socket (iocp socket completion-key)
  "Associate socket with I/O completion port"
  (let ((result (create-io-completion-port socket iocp completion-key 0)))
    (when (= (sb-alien:alien-sap result) +invalid-handle-value+)
      (error "Failed to associate socket with IOCP"))
    result))

;;;; Async Socket Operations

(defun async-read-socket (socket buffer overlapped)
  "Initiate asynchronous read on socket"
  (lib:with-foreign-memory ((wsabuf :char :count 16)  ; WSABUF structure
                            (bytes-received :unsigned-long :count 1)
                            (flags :unsigned-long :count 1))
    ;; Setup WSABUF
    (setf (sb-alien:sap-ref-32 (sb-alien:alien-sap wsabuf) 0) (length buffer))  ; len
    (setf (sb-alien:sap-ref-64 (sb-alien:alien-sap wsabuf) 8) 
          (sb-alien:alien-sap buffer))  ; buf
    
    (setf (sb-alien:deref flags 0) 0)
    
    (let ((result (%wsa-recv socket wsabuf 1 bytes-received flags overlapped 
                             (sb-alien:null-alien))))
      (values result (sb-alien:deref bytes-received 0)))))

(defun async-write-socket (socket buffer overlapped)
  "Initiate asynchronous write on socket"
  (lib:with-foreign-memory ((wsabuf :char :count 16)  ; WSABUF structure
                            (bytes-sent :unsigned-long :count 1))
    ;; Setup WSABUF
    (setf (sb-alien:sap-ref-32 (sb-alien:alien-sap wsabuf) 0) (length buffer))  ; len
    (setf (sb-alien:sap-ref-64 (sb-alien:alien-sap wsabuf) 8) 
          (sb-alien:alien-sap buffer))  ; buf
    
    (let ((result (%wsa-send socket wsabuf 1 bytes-sent 0 overlapped
                             (sb-alien:null-alien))))
      (values result (sb-alien:deref bytes-sent 0)))))

(defun async-accept-socket (listen-socket overlapped)
  "Initiate asynchronous accept on listening socket"
  (lib:with-foreign-memory ((addr :char :count 64)  ; sockaddr storage
                            (addrlen :int :count 1))
    (setf (sb-alien:deref addrlen 0) 64)
    
    (let ((accept-socket (%wsa-accept listen-socket addr addrlen 
                                      (sb-alien:null-alien) 0)))
      (if (= (sb-alien:alien-sap accept-socket) +invalid-handle-value+)
          (values nil addr)
          (values accept-socket addr)))))

(defun async-connect-socket (socket addr overlapped)
  "Initiate asynchronous connect on socket"
  (let ((result (%wsa-connect socket addr (length addr) 
                              (sb-alien:null-alien) (sb-alien:null-alien)
                              (sb-alien:null-alien) (sb-alien:null-alien))))
    result))

;;;; Address Handling (Windows sockaddr_in)

;; struct sockaddr_in {
;;   short   sin_family;
;;   u_short sin_port;
;;   struct  in_addr sin_addr;
;;   char    sin_zero[8];
;; };

(defun make-sockaddr-in (ip-address port)
  "Create sockaddr_in structure for IPv4 (Windows version)"
  (lib:with-foreign-memory ((addr :char :count 16))
    ;; sin_family (2 bytes)
    (setf (sb-alien:sap-ref-16 (sb-alien:alien-sap addr) 0) +af-inet+)
    ;; sin_port (2 bytes, network byte order)
    (setf (sb-alien:sap-ref-16 (sb-alien:alien-sap addr) 2)
          (logior (ash (logand port #xff) 8)
                  (ash (logand port #xff00) -8)))
    ;; sin_addr (4 bytes, network byte order)
    (let ((ip-parts (mapcar #'parse-integer 
                            (split-string ip-address #\.))))
      (setf (sb-alien:sap-ref-32 (sb-alien:alien-sap addr) 4)
            (logior (ash (first ip-parts) 24)
                    (ash (second ip-parts) 16)
                    (ash (third ip-parts) 8)
                    (fourth ip-parts))))
    ;; sin_zero (8 bytes of zeros)
    (loop for i from 8 to 15
          do (setf (sb-alien:deref addr i) 0))
    addr))

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

;;;; Convenience Functions

(defun wait-for-completion (iocp &optional (timeout +infinite+))
  "Wait for single completion on IOCP"
  (multiple-value-bind (result bytes-transferred completion-key overlapped)
      (get-queued-completion-status iocp timeout)
    (values result bytes-transferred completion-key overlapped)))

(defun process-completions (iocp handler-fn &optional (timeout +infinite+))
  "Process completions on IOCP with handler function"
  (multiple-value-bind (result bytes-transferred completion-key overlapped)
      (get-queued-completion-status iocp timeout)
    (when result
      (funcall handler-fn bytes-transferred completion-key overlapped))
    result))

;;;; Event Checking Predicates

(defun completion-success-p (result)
  "Check if completion was successful"
  (not (zerop result)))

(defun completion-error-p (result)
  "Check if completion had an error"
  (zerop result))

(defun completion-timeout-p (result)
  "Check if completion timed out"
  (and (zerop result) 
       ;; Would need to check GetLastError() for WAIT_TIMEOUT
       nil))