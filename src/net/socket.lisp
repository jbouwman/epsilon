(defpackage #:epsilon.net.socket
  (:use
   #:cl
   #:epsilon.lib.type)
  (:local-nicknames
   (#:string #:epsilon.lib.string))
  (:export
   #:host-to-hostname
   #:socket-connect
   #:socket-option
   #:socket-stream
   #:with-connected-socket
   #:with-server-socket
   #:with-client-socket))

(in-package #:epsilon.net.socket)

(defvar *ipv6-only-p* nil
  "When enabled, all SOCKET functions assume IPv6 addresses only.")

(defparameter *wildcard-host* #(0 0 0 0)
  "Hostname to pass when all interfaces in the current system are to
  be bound.  If this variable is passed to socket-listen, IPv6 capable
  systems will also listen for IPv6 connections.")

(defparameter *auto-port* 0
  "Port number to pass when an auto-assigned port number is wanted.")

(defparameter *version* "0.8.2"
  "socket version string")

(defconstant +max-datagram-packet-size+ 65507
  "The theoretical maximum amount of data in a UDP datagram.

The IPv4 UDP packets have a 16-bit length constraint, and IP+UDP header has 28-byte.

IP_MAXPACKET = 65535,       /* netinet/ip.h */
sizeof(struct ip) = 20,     /* netinet/ip.h */
sizeof(struct udphdr) = 8,  /* netinet/udp.h */

65535 - 20 - 8 = 65507

(But for UDP broadcast, the maximum message size is limited by the MTU size of the underlying link)")

(defclass usocket ()
  ((socket
    :initarg :socket
    :accessor socket
    :documentation "Implementation specific socket object instance.'")
   (wait-list
    :initform nil
    :accessor wait-list
    :documentation "WAIT-LIST the object is associated with.")
   (state
    :initform nil
    :accessor state
    :documentation "Per-socket return value for the `wait-for-input' function.

The value stored in this slot can be any of
 NIL          - not ready
 :READ        - ready to read
 :READ-WRITE  - ready to read and write
 :WRITE       - ready to write

The last two remain unused in the current version.
")
   #+(and win32 (or sbcl ecl lispworks))
   (%ready-p
    :initform nil
    :accessor %ready-p
    :documentation "Indicates whether the socket has been signalled
as ready for reading a new connection.

The value will be set to T by `wait-for-input-internal' (given the
right conditions) and reset to NIL by `socket-accept'.

Don't modify this slot or depend on it as it is really intended
to be internal only.

Note: Accessed, but not used for 'stream-socket'.
"
   ))
  (:documentation
"The main socket class.

Sockets should be closed using the `socket-close' method."))

(defgeneric socket-state (socket)
  (:documentation "NIL          - not ready
:READ        - ready to read
:READ-WRITE  - ready to read and write
:WRITE       - ready to write"))

(defmethod socket-state ((socket usocket))
  (state socket))

(defclass stream-usocket (usocket)
   ((stream
     :initarg :stream
     :accessor socket-stream
     :documentation "Stream instance associated with the socket."
;;
;;Iff an external-format was passed to `socket-connect' or `socket-listen'
;;the stream is a flexi-stream. Otherwise the stream is implementation
;;specific."
))
   (:documentation
"Stream socket class.
'
Contrary to other sockets, these sockets may be closed either
with the `socket-close' method or by closing the associated stream
(which can be retrieved with the `socket-stream' accessor)."))

(defclass stream-server-usocket (usocket)
  ((element-type
    :initarg :element-type
    :initform #-lispworks 'character
              #+lispworks 'base-char
    :reader element-type
    :documentation "Default element type for streams created by
`socket-accept'."))
  (:documentation "Socket which listens for stream connections to
be initiated from remote sockets."))

(defclass datagram-usocket (usocket)
  ((connected-p :type boolean
                :accessor connected-p
                :initarg :connected-p)
   #+(or cmu scl lispworks mcl
         (and clisp ffi (not rawsock)))
   (%open-p     :type boolean
                :accessor %open-p
                :initform t
                :documentation "Flag to indicate if socket is open,
for GC on implementions operate on raw socket fd.")
   #+(or lispworks mcl
         (and clisp ffi (not rawsock)))
   (recv-buffer :documentation "Private RECV buffer.")
   #+(or lispworks mcl)
   (send-buffer :documentation "Private SEND buffer."))
  (:documentation "UDP (inet-datagram) socket"))

(defun socket-p (socket)
  (typep socket 'usocket))

(defun stream-socket-p (socket)
  (typep socket 'stream-usocket))

(defun stream-server-socket-p (socket)
  (typep socket 'stream-server-usocket))

(defun datagram-socket-p (socket)
  (typep socket 'datagram-usocket))

(defun make-socket (&key socket)
  "Create a usocket socket type from implementation specific socket."
  (unless socket
    (error 'invalid-socket-error))
  (make-stream-socket :socket socket))

(defun make-stream-socket (&key socket stream)
  "Create a usocket socket type from implementation specific socket
and stream objects.

Sockets returned should be closed using the `socket-close' method or
by closing the stream associated with the socket.
"
  (unless socket
    (error 'invalid-socket-error))
  (unless stream
    (error 'invalid-socket-stream-error))
  (make-instance 'stream-usocket
                 :socket socket
                 :stream stream))

(defun make-stream-server-socket (socket &key (element-type
                                               #-lispworks 'character
                                               #+lispworks 'base-char))
  "Create a usocket-server socket type from an
implementation-specific socket object.

The returned value is a subtype of `stream-server-usocket'.
"
  (unless socket
    (error 'invalid-socket-error))
  (make-instance 'stream-server-usocket
                 :socket socket
                 :element-type element-type))

(defun make-datagram-socket (socket &key connected-p)
  (unless socket
    (error 'invalid-socket-error))
  (make-instance 'datagram-usocket
                 :socket socket
                 :connected-p connected-p))

(defgeneric socket-accept (socket &key element-type)
  (:documentation
      "Accepts a connection from `socket', returning a `stream-socket'.

The stream associated with the socket returned has `element-type' when
explicitly specified, or the element-type passed to `socket-listen' otherwise."))

(defgeneric socket-close (usocket)
  (:documentation "Close a previously opened `usocket'."))

(defmethod socket-close :before ((usocket usocket))
  (when (wait-list usocket)
    (remove-waiter (wait-list usocket) usocket)))

;; also see http://stackoverflow.com/questions/4160347/close-vs-shutdown-socket
(defgeneric socket-shutdown (usocket direction)
  (:documentation "Shutdown communication on the socket in DIRECTION.

After a shutdown no input and/or output of the indicated DIRECTION
can be performed on the `usocket'.

DIRECTION should be either :INPUT or :OUTPUT or :IO"))

(defgeneric socket-send (usocket buffer length &key host port)
  (:documentation "Send packets through a previously opend `usocket'."))

(defgeneric socket-receive (usocket buffer length &key)
  (:documentation "Receive packets from a previously opend `usocket'.

Returns 4 values: (values buffer size host port)"))

(defgeneric get-local-address (socket)
  (:documentation "Returns the IP address of the socket."))

(defgeneric get-peer-address (socket)
  (:documentation
   "Returns the IP address of the peer the socket is connected to."))

(defgeneric get-local-port (socket)
  (:documentation "Returns the IP port of the socket.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-port (socket)
  (:documentation "Returns the IP port of the peer the socket to."))

(defgeneric get-local-name (socket)
  (:documentation "Returns the IP address and port of the socket as values.

This function applies to both `stream-usocket' and `server-stream-usocket'
type objects."))

(defgeneric get-peer-name (socket)
  (:documentation
   "Returns the IP address and port of the peer
the socket is connected to as values."))

(defmacro with-connected-socket ((var socket) &body body)
  "Bind `socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(let ((,var ,socket))
     (unwind-protect
         (when ,var
           (with-mapped-conditions (,var)
             ,@body))
       (when ,var
         (socket-close ,var)))))

(defmacro with-client-socket ((socket-var stream-var &rest socket-connect-args)
                              &body body)
  "Bind the socket resulting from a call to `socket-connect' with
the arguments `socket-connect-args' to `socket-var' and if `stream-var' is
non-nil, bind the associated socket stream to it."
  `(with-connected-socket (,socket-var (socket-connect ,@socket-connect-args))
     ,(if (null stream-var)
          `(progn ,@body)
           `(let ((,stream-var (socket-stream ,socket-var)))
              ,@body))))

(defmacro with-server-socket ((var server-socket) &body body)
  "Bind `server-socket' to `var', ensuring socket destruction on exit.

`body' is only evaluated when `var' is bound to a non-null value.

The `body' is an implied progn form."
  `(with-connected-socket (,var ,server-socket)
     ,@body))

(defmacro with-socket-listener ((socket-var &rest socket-listen-args)
                                &body body)
  "Bind the socket resulting from a call to `socket-listen' with arguments
`socket-listen-args' to `socket-var'."
  `(with-server-socket (,socket-var (socket-listen ,@socket-listen-args))
     ,@body))

(defstruct (wait-list (:constructor %make-wait-list))
  %wait     ;; implementation specific
  waiters ;; the list of all usockets
  map)  ;; maps implementation sockets to usockets

;; Implementation specific:
;;
;;  %setup-wait-list
;;  %add-waiter
;;  %remove-waiter

(defun make-wait-list (waiters)
  (let ((wl (%make-wait-list)))
    (setf (wait-list-map wl) (make-hash-table))
    (%setup-wait-list wl)
    (dolist (x waiters wl) ; wl is returned
      (add-waiter wl x))))

(defun add-waiter (wait-list input)
  (setf (gethash (socket input) (wait-list-map wait-list)) input
        (wait-list input) wait-list)
  (pushnew input (wait-list-waiters wait-list))
  (%add-waiter wait-list input))

(defun remove-waiter (wait-list input)
  (%remove-waiter wait-list input)
  (setf (wait-list-waiters wait-list)
        (remove input (wait-list-waiters wait-list))
        (wait-list input) nil)
  (remhash (socket input) (wait-list-map wait-list)))

(defun remove-all-waiters (wait-list)
  (dolist (waiter (wait-list-waiters wait-list))
    (%remove-waiter wait-list waiter))
  (setf (wait-list-waiters wait-list) nil)
  (clrhash (wait-list-map wait-list)))

(defun wait-for-input (socket-or-sockets &key timeout ready-only
                                         &aux (single-socket-p
                                               (socket-p socket-or-sockets)))
  "Waits for one or more streams to become ready for reading from
the socket.  When `timeout' (a non-negative real number) is
specified, wait `timeout' seconds, or wait indefinitely when
it isn't specified.  A `timeout' value of 0 (zero) means polling.

Returns two values: the first value is the list of streams which
are readable (or in case of server streams acceptable).  NIL may
be returned for this value either when waiting timed out or when
it was interrupted (EINTR).  The second value is a real number
indicating the time remaining within the timeout period or NIL if
none.

Without the READY-ONLY arg, WAIT-FOR-INPUT will return all sockets in
the original list you passed it. This prevents a new list from being
consed up. Some users of USOCKET were reluctant to use it if it
wouldn't behave that way, expecting it to cost significant performance
to do the associated garbage collection.

Without the READY-ONLY arg, you need to check the socket STATE slot for
the values documented in usocket.lisp in the usocket class."

  ;; for NULL sockets, return NIL with respect of TIMEOUT.
  (when (null socket-or-sockets)
    (when timeout
      (sleep timeout))
    (return-from wait-for-input nil))

  ;; create a new wait-list if it's not created by the caller.
  (unless (wait-list-p socket-or-sockets)
    ;; OPTIMIZATION: in case socket-or-sockets is an atom, create the wait-list
    ;; only once and store it into the usocket itself.
    (let ((wl (if (and single-socket-p
                       (wait-list socket-or-sockets))
                  (wait-list socket-or-sockets) ; reuse the per-usocket wait-list
                (make-wait-list (if (listp socket-or-sockets)
                                    socket-or-sockets (list socket-or-sockets))))))
      (multiple-value-bind (sockets to-result)
          (wait-for-input wl :timeout timeout :ready-only ready-only)
        ;; in case of single socket, keep the wait-list
        (unless single-socket-p
          (remove-all-waiters wl))
        (return-from wait-for-input
          (values (if ready-only sockets socket-or-sockets) to-result)))))

  (let* ((start (get-internal-real-time))
         (sockets-ready 0))
    (dolist (x (wait-list-waiters socket-or-sockets))
      (when (setf (state x)
                  #+(and win32 (or sbcl ecl)) nil ; they cannot rely on LISTEN
                  #-(and win32 (or sbcl ecl))
                  (if (and (stream-socket-p x)
                           (listen (socket-stream x)))
                      :read
                      nil))
        (incf sockets-ready)))
    ;; the internal routine is responsibe for
    ;; making sure the wait doesn't block on socket-streams of
    ;; which theready- socket isn't ready, but there's space left in the
    ;; buffer.  socket-or-sockets is not destructed.
    (wait-for-input-internal socket-or-sockets
                             :timeout (if (zerop sockets-ready) timeout 0))
    (let ((to-result (when timeout
                       (let ((elapsed (/ (- (get-internal-real-time) start)
                                         internal-time-units-per-second)))
                         (when (< elapsed timeout)
                           (- timeout elapsed))))))
      ;; two return values:
      ;; 1) the original wait-list, or available sockets (ready-only)
      ;; 2) remaining timeout
      (values (cond (ready-only
                     (cond (single-socket-p
                            (if (null (state (car (wait-list-waiters socket-or-sockets))))
                                nil ; nothing left if the only socket is not waiting
                              (wait-list-waiters socket-or-sockets)))
                           (t (remove-if #'null (wait-list-waiters socket-or-sockets) :key #'state))))
                    (t socket-or-sockets))
              to-result))))

;;
;; Data utility functions
;;

(defun integer-to-octet-buffer (integer buffer octets &key (start 0))
  (do ((b start (1+ b))
       (i (ash (1- octets) 3) ;; * 8
          (- i 8)))
      ((> 0 i) buffer)
    (setf (aref buffer b)
          (ldb (byte 8 i) integer))))

(defun octet-buffer-to-integer (buffer octets &key (start 0))
  (let ((integer 0))
    (do ((b start (1+ b))
         (i (ash (1- octets) 3) ;; * 8
            (- i 8)))
        ((> 0 i)
         integer)
      (setf (ldb (byte 8 i) integer)
            (aref buffer b)))))

(defmacro port-to-octet-buffer (port buffer &key (start 0))
  `(integer-to-octet-buffer ,port ,buffer 2 :start ,start))

(defmacro ip-to-octet-buffer (ip buffer &key (start 0))
  `(integer-to-octet-buffer (host-byte-order ,ip) ,buffer 4 :start ,start))

(defmacro port-from-octet-buffer (buffer &key (start 0))
  `(octet-buffer-to-integer ,buffer 2 :start ,start))

(defmacro ip-from-octet-buffer (buffer &key (start 0))
  `(octet-buffer-to-integer ,buffer 4 :start ,start))

;;
;; IPv4 utility functions
;;

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun ip-address-string-p (string)
  "Return a true value if the given string could be an IP address."
  (every (lambda (char)
           (or (digit-char-p char)
               (eql char #\.)))
         string))

(defun hbo-to-dotted-quad (integer) ; exported
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~D.~D.~D.~D" first second third fourth)))

(defun hbo-to-vector-quad (integer) ; exported
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun vector-quad-to-dotted-quad (vector) ; exported
  (format nil "~D.~D.~D.~D"
          (aref vector 0)
          (aref vector 1)
          (aref vector 2)
          (aref vector 3)))

(defun dotted-quad-to-vector-quad (string) ; exported
  (let ((list (list-of-strings-to-integers (string:split #\. string))))
    (vector (first list) (second list) (third list) (fourth list))))

(defgeneric host-byte-order (address)) ; exported

(defmethod host-byte-order ((string string))
  "Convert a string, such as 192.168.1.1, to host-byte-order,
such as 3232235777."
  (let ((list (list-of-strings-to-integers (string:split #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defmethod host-byte-order ((vector vector)) ; IPv4 only
  "Convert a vector, such as #(192 168 1 1), to host-byte-order, such as
3232235777."
  (+ (* (aref vector 0) 256 256 256) (* (aref vector 1) 256 256)
     (* (aref vector 2) 256) (aref vector 3)))

(defmethod host-byte-order ((int integer))
  int) ; this assume input integer is already host-byte-order

;;
;; IPv6 utility functions
;;

(defun vector-to-ipv6-host (vector) ; exported
  (with-output-to-string (*standard-output*)
    (loop with zeros-collapsed-p
          with collapsing-zeros-p
          for i below 16 by 2
          for word = (+ (ash (aref vector i) 8)
                        (aref vector (1+ i)))
          do (cond
               ((and (zerop word)
                     (not collapsing-zeros-p)
                     (not zeros-collapsed-p))
                (setf collapsing-zeros-p t))
               ((or (not (zerop word))
                    zeros-collapsed-p)
                (when collapsing-zeros-p
                  (write-string ":")
                  (setf collapsing-zeros-p nil
                        zeros-collapsed-p t))
                (format t "~:[~;:~]~X" (plusp i) word)))
          finally (when collapsing-zeros-p
                    (write-string "::")))))

(defun split-ipv6-address (string)
  (let ((pos 0)
        (word nil)
        (double-colon-seen-p nil)
        (words-before-double-colon nil)
        (words-after-double-colon nil))
    (loop
      (multiple-value-setq (word pos) (parse-integer string :radix 16 :junk-allowed t :start pos))
      (labels ((at-end-p ()
                 (= pos (length string)))
               (looking-at-colon-p ()
                 (char= (char string pos) #\:))
               (ensure-colon ()
                 (unless (looking-at-colon-p)
                   (error "unsyntactic IPv6 address string ~S, expected a colon at position ~D"
                          string pos))
                 (incf pos)))
        (cond
          ((null word)
           (when double-colon-seen-p
             (error "unsyntactic IPv6 address string ~S, can only have one double-colon filler mark"
                    string))
           (setf double-colon-seen-p t))
          (double-colon-seen-p
           (push word words-after-double-colon))
          (t
           (push word words-before-double-colon)))
        (if (at-end-p)
            (return (list (nreverse words-before-double-colon) (nreverse words-after-double-colon)))
            (ensure-colon))))))

(defun ipv6-host-to-vector (string) ; exported
  (assert (> (length string) 2) ()
          "Unsyntactic IPv6 address literal ~S, expected at least three characters" string)
  (destructuring-bind (words-before-double-colon words-after-double-colon)
      (split-ipv6-address (concatenate 'string
                                       (when (eql (char string 0) #\:)
                                         "0")
                                       string
                                       (when (eql (char string (1- (length string))) #\:)
                                         "0")))
    (let ((number-of-words-specified (+ (length words-before-double-colon) (length words-after-double-colon))))
      (assert (<= number-of-words-specified 8) ()
              "Unsyntactic IPv6 address literal ~S, too many colon separated address components" string)
      (assert (or (= number-of-words-specified 8) words-after-double-colon) ()
              "Unsyntactic IPv6 address literal ~S, too few address components and no double-colon filler found" string)
      (loop with vector = (make-array 16 :element-type 'u8)
            for i below 16 by 2
            for word in (append words-before-double-colon
                                (make-list (- 8 number-of-words-specified) :initial-element 0)
                                words-after-double-colon)
            do (setf (aref vector i) (ldb (byte 8 8) word)
                     (aref vector (1+ i)) (ldb (byte 8 0) word))
            finally (return vector)))))

;; exported since 0.8.0
(defun host-to-hostname (host) ; host -> string
  "Translate a string, vector quad or 16 byte IPv6 address to a
stringified hostname."
  (etypecase host
    (string host)      ; IPv4 or IPv6
    ((or (vector t 4)  ; IPv4
         (array u8 (4)))
     (vector-quad-to-dotted-quad host))
    ((or (vector t 16) ; IPv6
         (array u8 (16)))
     (vector-to-ipv6-host host))
    (integer (hbo-to-dotted-quad host)) ; integer input is IPv4 only
    (null
     (if *ipv6-only-p* "::" "0.0.0.0")))) ;; "::" is the IPv6 wildcard address

(defun ip= (ip1 ip2) ; exported
  (etypecase ip1
    (string (string= ip1                  ; IPv4 or IPv6
                     (host-to-hostname ip2)))
    ((or (vector t 4)                     ; IPv4
         (array u8 (4))    ; IPv4
         (vector t 16)                    ; IPv6
         (array u8 (16)))  ; IPv6
     (equalp ip1 ip2))
    (integer (= ip1                       ; IPv4 only
                (host-byte-order ip2))))) ; convert ip2 to integer (hbo)

(defun ip/= (ip1 ip2) ; exported
  (not (ip= ip1 ip2)))

;;
;; DNS helper functions
;;

(defun get-host-by-name (name)
  "0.7.1+: if there're IPv4 addresses, return the first IPv4 address.
(unless in IPv6-only mode)"
  (let* ((hosts (get-hosts-by-name name))
         (ipv4-hosts (remove-if-not #'(lambda (ip) (= 4 (length ip))) hosts))
	 (ipv6-hosts (remove-if #'(lambda (ip) (= 4 (length ip))) hosts)))
    (cond (*ipv6-only-p* (car ipv6-hosts))
	  (ipv4-hosts    (car ipv4-hosts))
	  (t             (car hosts)))))

(defun get-random-host-by-name (name)
  "0.7.1+: if there're IPv4 addresses, only return a random IPv4 address.
(unless in IPv6-only mode)"
  (let* ((hosts (get-hosts-by-name name))
         (ipv4-hosts (remove-if-not #'(lambda (ip) (= 4 (length ip))) hosts))
	 (ipv6-hosts (remove-if #'(lambda (ip) (= 4 (length ip))) hosts)))
    (cond (*ipv6-only-p* (elt ipv6-hosts (random (length ipv6-hosts))))
	  (ipv4-hosts    (elt ipv4-hosts (random (length ipv4-hosts))))
          (hosts         (elt hosts      (random (length hosts)))))))

(defun host-to-vector-quad (host) ; internal
  "Translate a host specification (vector quad, dotted quad or domain name)
to a vector quad."
  (etypecase host
    (string (let ((ip (when (ip-address-string-p host)
                         (dotted-quad-to-vector-quad host))))
              (if (and ip (= 4 (length ip)))
                  ;; valid IP dotted quad? not sure
                  ip
                (get-random-host-by-name host))))
    ((or (vector t 4)
         (array u8 (4)))
     host)
    (integer (hbo-to-vector-quad host))))

(defun host-to-hbo (host) ; internal
  (etypecase host
    (string (let ((ip (when (ip-address-string-p host)
                        (dotted-quad-to-vector-quad host))))
              (if (and ip (= 4 (length ip)))
                  (host-byte-order ip)
                  (host-to-hbo (get-host-by-name host)))))
    ((or (vector t 4)
         (array u8 (4)))
     (host-byte-order host))
    (integer host)))

;;
;; Other utility functions
;;

(defun split-timeout (timeout &optional (fractional 1000000))
  "Split real value timeout into seconds and microseconds.
Optionally, a different fractional part can be specified."
  (multiple-value-bind
      (secs sec-frac)
      (truncate timeout 1)
    (values secs
            (truncate (* fractional sec-frac) 1))))

;;
;; Setting of documentation for backend defined functions
;;

;; Documentation for the function
;;
;; (defun SOCKET-CONNECT (host port &key element-type nodelay some-other-keys...) ..)
;;
(setf (documentation 'socket-connect 'function)
      "Connect to `host' on `port'.  `host' is assumed to be a string or
an IP address represented in vector notation, such as #(192 168 1 1).
`port' is assumed to be an integer.

`element-type' specifies the element type to use when constructing the
stream associated with the socket.  The default is 'character.

`nodelay' Allows to disable/enable Nagle's algorithm (http://en.wikipedia.org/wiki/Nagle%27s_algorithm).
If this parameter is omitted, the behaviour is inherited from the
CL implementation (in most cases, Nagle's algorithm is
enabled by default, but for example in ACL it is disabled).
If the parameter is specified, one of these three values is possible:
  T - Disable Nagle's algorithm; signals an UNSUPPORTED
      condition if the implementation does not support explicit
      manipulation with that option.
  NIL - Leave Nagle's algorithm enabled on the socket;
      signals an UNSUPPORTED condition if the implementation does
      not support explicit manipulation with that option.
  :IF-SUPPORTED - Disables Nagle's algorithm if the implementation
      allows this, otherwises just ignore this option.

Returns a usocket object.")

;; Documentation for the function
;;
;; (defun SOCKET-LISTEN (host port &key reuseaddress backlog element-type) ..)
;;###FIXME: extend with default-element-type
(setf (documentation 'socket-listen 'function)
      "Bind to interface `host' on `port'. `host' should be the
representation of an ready-interface address.  The implementation is
not required to do an address lookup, making no guarantees that
hostnames will be correctly resolved.  If `*wildcard-host*' or NIL is
passed for `host', the socket will be bound to all available
interfaces for the system.  `port' can be selected by the IP stack by
passing `*auto-port*'.

Returns an object of type `stream-server-usocket'.

`reuse-address' and `backlog' are advisory parameters for setting socket
options at creation time. `element-type' is the element type of the
streams to be created by `socket-accept'.  `reuseaddress' is supported for
backward compatibility (but deprecated); when both `reuseaddress' and
`reuse-address' have been specified, the latter takes precedence.
")

;;; Small utility functions mapping true/false to 1/0, moved here from option.lisp

(proclaim '(inline bool->int int->bool))

(defun bool->int (bool) (if bool 1 0))
(defun int->bool (int) (= 1 int))


;; Condition signalled by operations with unsupported arguments
;; For trivial-sockets compatibility.

(define-condition insufficient-implementation (error)
  ((feature :initarg :feature :reader feature)
   (context :initarg :context :reader context
    :documentation "String designator of the public API function which
the feature belongs to."))
  (:documentation "The ancestor of all errors usocket may generate
because of insufficient support from the underlying implementation
with respect to the arguments given to `function'.

One call may signal several errors, if the caller allows processing
to continue.
"))

(define-condition unsupported (insufficient-implementation)
  ((minimum :initarg :minimum :reader minimum
            :documentation "Indicates the minimal version of the
implementation required to support the requested feature."))
  (:report (lambda (c stream)
             (format stream "~A in ~A is unsupported."
                     (feature c) (context c))
             (when (minimum c)
               (format stream " Minimum version (~A) is required."
                       (minimum c)))))
  (:documentation "Signalled when the underlying implementation
doesn't allow supporting the requested feature.

When you see this error, go bug your vendor/implementation developer!"))

(define-condition unimplemented (insufficient-implementation)
  ()
  (:report (lambda (c stream)
             (format stream "~A in ~A is unimplemented."
                     (feature c) (context c))))
  (:documentation "Signalled if a certain feature might be implemented,
based on the features of the underlying implementation, but hasn't
been implemented yet."))

;; Conditions raised by sockets operations

(define-condition socket-condition (condition)
  ((socket :initarg :socket
           :accessor usocket-socket))
  ;;###FIXME: no slots (yet); should at least be the affected usocket...
  (:documentation "Parent condition for all socket related conditions."))

(define-condition socket-error (socket-condition error)
  () ;; no slots (yet)
  (:documentation "Parent error for all socket related errors"))

(define-condition ns-condition (condition)
  ((host-or-ip :initarg :host-or-ip
               :accessor host-or-ip))
  (:documentation "Parent condition for all name resolution conditions."))

(define-condition ns-error (ns-condition error)
  (#+(or clasp ecl)
   (socket :initarg :socket
           :accessor usocket-socket))
  (:documentation "Parent error for all name resolution errors."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun define-usocket-condition-class (class &rest parents)
    `(progn
       (define-condition ,class ,parents ())
       (eval-when (:load-toplevel :execute)
         (export ',class)))))

(defmacro define-usocket-condition-classes (class-list parents)
  `(progn ,@(mapcar #'(lambda (x)
                        (apply #'define-usocket-condition-class
                               x parents))
                    class-list)))

;; Mass define and export our conditions
(define-usocket-condition-classes
  (interrupted-condition)
  (socket-condition))

(define-condition unknown-condition (socket-condition)
  ((real-condition :initarg :real-condition
                   :accessor usocket-real-condition))
  (:documentation "Condition raised when there's no other - more applicable -
condition available."))

;; Mass define and export our errors
(define-usocket-condition-classes
  (address-in-use-error
   address-not-available-error
   already-shutdown-error
   bad-file-descriptor-error
   connection-refused-error
   connection-aborted-error
   connection-reset-error
   invalid-argument-error
   no-buffers-error
   operation-not-supported-error
   operation-not-permitted-error
   protocol-not-supported-error
   socket-type-not-supported-error
   network-unreachable-error
   network-down-error
   network-reset-error
   host-down-error
   host-unreachable-error
   out-of-memory-error
   shutdown-error
   timeout-error
   deadline-timeout-error
   invalid-socket-error
   invalid-socket-stream-error)
  (socket-error))

(define-condition unknown-error (socket-error)
  ((real-error :initarg :real-error
               :accessor usocket-real-error
               :initform nil)
   (errno      :initarg :errno
               :reader usocket-errno
               :initform 0))
  (:report (lambda (c stream)
             (typecase c
               (simple-condition
                (format stream
                        (simple-condition-format-control (usocket-real-error c))
                        (simple-condition-format-arguments (usocket-real-error c))))
               (otherwise
                (format stream "The condition ~A occurred with errno: ~D."
                        (usocket-real-error c)
                        (usocket-errno c))))))
  (:documentation "Error raised when there's no other - more applicable -
error available."))

(define-usocket-condition-classes
  (ns-try-again-condition) ; obsoleted
  (socket-condition))

(define-condition ns-unknown-condition (ns-condition)
  ((real-condition :initarg :real-condition
                   :accessor ns-real-condition
                   :initform nil))
  (:documentation "Condition raised when there's no other - more applicable -
condition available."))

(define-usocket-condition-classes
  ;; the no-data error code in the Unix 98 api
  ;; isn't really an error: there's just no data to return.
  ;; with lisp, we just return NIL (indicating no data) instead of
  ;; raising an exception...
  (ns-host-not-found-error
   ns-no-recovery-error
   ns-try-again-error)
  (ns-error))

(define-condition ns-unknown-error (ns-error)
  ((real-error :initarg :real-error
               :accessor ns-real-error
               :initform nil))
  (:report (lambda (c stream)
             (typecase c
               (simple-condition
                (format stream
                        (simple-condition-format-control (usocket-real-error c))
                        (simple-condition-format-arguments (usocket-real-error c))))
               (otherwise
                (format stream "The condition ~A occurred." (usocket-real-error c))))))
  (:documentation "Error raised when there's no other - more applicable -
error available."))

(defmacro with-mapped-conditions ((&optional socket host-or-ip) &body body)
  `(handler-bind ((condition
                   #'(lambda (c) (handle-condition c ,socket ,host-or-ip))))
     ,@body))

(defparameter +unix-errno-condition-map+
  `(((11) . ns-try-again-error)     ;; EAGAIN
    ((35) . ns-try-again-error)     ;; EDEADLCK
    ((4) . interrupted-condition))) ;; EINTR

(defparameter +unix-errno-error-map+
  ;;### the first column is for non-(linux or srv4) systems
  ;; the second for linux
  ;; the third for srv4
  ;;###FIXME: How do I determine on which Unix we're running
  ;;          (at least in clisp and sbcl; I know about cmucl...)
  ;; The table below works under the assumption we'll *only* see
  ;; socket associated errors...
  `(((48 98) . address-in-use-error)
    ((49 99) . address-not-available-error)
    ((9) . bad-file-descriptor-error)
    ((61 111) . connection-refused-error)
    ((54 104) . connection-reset-error)
    ((53 103) . connection-aborted-error)
    ((22) . invalid-argument-error)
    ((55 105) . no-buffers-error)
    ((12) . out-of-memory-error)
    ((45 95) . operation-not-supported-error)
    ((1 13) . operation-not-permitted-error)
    ((43 92) . protocol-not-supported-error)
    ((44 93) . socket-type-not-supported-error)
    ((51 101) . network-unreachable-error)
    ((50 100) . network-down-error)
    ((52 102) . network-reset-error)
    ((58 108) . already-shutdown-error)
    ((60 110) . timeout-error)
    ((64 112) . host-down-error)
    ((65 113) . host-unreachable-error)))

(defun map-errno-condition (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))

(defun map-errno-error (errno)
  (cdr (assoc errno +unix-errno-error-map+ :test #'member)))

(defparameter +unix-ns-error-map+
  `((1 . ns-host-not-found-error)
    (2 . ns-try-again-error)
    (3 . ns-no-recovery-error)))

(defmacro unsupported (feature context &key minimum)
  `(cerror "Ignore it and continue" 'unsupported
           :feature ,feature
           :context ,context
           :minimum ,minimum))

(defmacro unimplemented (feature context)
  `(signal 'unimplemented :feature ,feature :context ,context))

;;; People may want to ignore all unsupported warnings, here it is.
(defmacro ignore-unsupported-warnings (&body body)
  `(handler-bind ((unsupported
                   #'(lambda (c)
                       (declare (ignore c)) (continue))))
     (progn ,@body)))


#+sbcl
(progn
  #-win32
  (defun get-host-name ()
    (sb-unix:unix-gethostname))

  ;; we assume winsock has already been loaded, after all,
  ;; we already loaded sb-bsd-sockets and sb-alien
  #+win32
  (defun get-host-name ()
    (sb-alien:with-alien ((buf (sb-alien:array sb-alien:char 256)))
       (let ((result (sb-alien:alien-funcall
                      (sb-alien:extern-alien "gethostname"
                                             (sb-alien:function sb-alien:int
                                                                (* sb-alien:char)
                                                                sb-alien:int))
                      (sb-alien:cast buf (* sb-alien:char))
                      256)))
         (when (= result 0)
           (sb-alien:cast buf sb-alien:c-string))))))

#+(or mkcl (and ecl (not ecl-bytecmp)))
(progn
  #-:wsock
  (ffi:clines
   "#include <errno.h>"
   "#include <sys/socket.h>"
   "#include <unistd.h>")
  #+:wsock
  (ffi:clines
   "#ifndef FD_SETSIZE"
   "#define FD_SETSIZE 1024"
   "#endif"
   "#include <winsock2.h>")

  (ffi:clines
   #+:msvc "#include <time.h>"
   #-:msvc "#include <sys/time.h>"
   "#include <ecl/ecl-inl.h>")
#|
  #+:prefixed-api
  (ffi:clines
   "#define CONS(x, y) ecl_cons((x), (y))"
   "#define MAKE_INTEGER(x) ecl_make_integer((x))")
  #-:prefixed-api
  (ffi:clines
   "#define CONS(x, y) make_cons((x), (y))"
   "#define MAKE_INTEGER(x) make_integer((x))")
|#

  (defun cerrno ()
    (ffi:c-inline () () :int
     "errno" :one-liner t))

  (defun fd-setsize ()
    (ffi:c-inline () () :fixnum
     "FD_SETSIZE" :one-liner t))

  #+ecl
  (defun fdset-alloc ()
    (ffi:c-inline () () :pointer-void
     "ecl_alloc_atomic(sizeof(fd_set))" :one-liner t))
  #+mkcl
  (defun fdset-alloc ()
    (ffi:c-inline () () :pointer-void
     "mkcl_alloc_atomic(MKCL_ENV(), sizeof(fd_set))" :one-liner t))

  (defun fdset-zero (fdset)
    (ffi:c-inline (fdset) (:pointer-void) :void
     "FD_ZERO((fd_set*)#0)" :one-liner t))

  (defun fdset-set (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :void
     "FD_SET(#1,(fd_set*)#0)" :one-liner t))

  (defun fdset-clr (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :void
     "FD_CLR(#1,(fd_set*)#0)" :one-liner t))

  (defun fdset-fd-isset (fdset fd)
    (ffi:c-inline (fdset fd) (:pointer-void :fixnum) :bool
     "FD_ISSET(#1,(fd_set*)#0)" :one-liner t))

  (declaim (inline cerrno
                   fd-setsize
                   fdset-alloc
                   fdset-zero
                   fdset-set
                   fdset-clr
                   fdset-fd-isset))

  #+ecl
  (defun get-host-name ()
    (ffi:c-inline
     () () :object
     "{ char *buf = (char *) ecl_alloc_atomic(257);

        if (gethostname(buf,256) == 0)
          @(return) = make_simple_base_string(buf);
        else
          @(return) = Cnil;
      }" :one-liner nil :side-effects nil))

  #+mkcl
  (defun get-host-name ()
    (ffi:c-inline
     () () :object
     "{ char *buf = (char *) mkcl_alloc_atomic(MKCL_ENV(),257);

        if (gethostname(buf,256) == 0)
          @(return) = mkcl_cstring_to_base_string(MKCL_ENV(), (buf));
        else
          @(return) = mk_cl_Cnil;
      }" :one-liner nil :side-effects nil))

  (defun read-select (wl to-secs &optional (to-musecs 0))
    (let* ((sockets (wait-list-waiters wl))
           (rfds (wait-list-%wait wl))
           (max-fd (reduce #'(lambda (x y)
                               (let ((sy (sb-bsd-sockets:socket-file-descriptor
                                          (socket y))))
                                 (if (< x sy) sy x)))
                           (cdr sockets)
                           :initial-value (sb-bsd-sockets:socket-file-descriptor
                                           (socket (car sockets))))))
      (fdset-zero rfds)
      (dolist (sock sockets)
        (fdset-set rfds (sb-bsd-sockets:socket-file-descriptor
                         (socket sock))))
      (let ((count
             (ffi:c-inline (to-secs to-musecs rfds max-fd)
                           (t :unsigned-int :pointer-void :int)
                           :int
#+ecl
      "
          int count;
          struct timeval tv;
          struct timeval tvs;
          struct timeval tve;
          unsigned long elapsed;
          unsigned long remaining;
          int retval = -1;

          if (#0 != Cnil) {
            tv.tv_sec = fixnnint(#0);
            tv.tv_usec = #1;
          }
          remaining = ((tv.tv_sec*1000000) + tv.tv_usec);

          do {
              (void)gettimeofday(&tvs, NULL);   // start time

              retval = select(#3 + 1, (fd_set*)#2, NULL, NULL,
                           (#0 != Cnil) ? &tv : NULL);

              if ( (retval < 0) && (errno == EINTR) && (#0 != Cnil) ) {
                  (void)gettimeofday(&tve, NULL);            // end time
                  elapsed = (tve.tv_sec - tvs.tv_sec)*1000000 + (tve.tv_usec - tvs.tv_usec);
                  remaining = remaining - elapsed;
                  if ( remaining < 0 ) {                     // already past timeout, just exit
                      retval = 0;
                      break;
                  }

                  tv.tv_sec = remaining / 1000000;
                  tv.tv_usec = remaining - (tv.tv_sec * 1000000);
              }

          } while ((retval < 0) && (errno == EINTR));

          @(return) = retval;
"
#+mkcl
      "
          int count;
          struct timeval tv;
          struct timeval tvs;
          struct timeval tve;
          unsigned long elapsed;
          unsigned long remaining;
          int retval = -1;

          if (#0 != mk_cl_Cnil) {
            tv.tv_sec = mkcl_integer_to_word(MKCL_ENV(), #0);
            tv.tv_usec = #1;
          }
          remaining = ((tv.tv_sec*1000000) + tv.tv_usec);

          do {
              (void)gettimeofday(&tvs, NULL);   // start time

              retval = select(#3 + 1, (fd_set*)#2, NULL, NULL,
                           (#0 != mk_cl_Cnil) ? &tv : NULL);

              if ( (retval < 0) && (errno == EINTR) && (#0 != mk_cl_Cnil) ) {
                  (void)gettimeofday(&tve, NULL);            // end time
                  elapsed = (tve.tv_sec - tvs.tv_sec)*1000000 + (tve.tv_usec - tvs.tv_usec);
                  remaining = remaining - elapsed;
                  if ( remaining < 0 ) {                     // already past timeout, just exit
                      retval = 0;
                      break;
                  }

                  tv.tv_sec = remaining / 1000000;
                  tv.tv_usec = remaining - (tv.tv_sec * 1000000);
              }

          } while ((retval < 0) && (errno == EINTR));

          @(return) = retval;
"



 :one-liner nil)))
        (cond
          ((= 0 count)
           (values nil nil))
          ((< count 0)
           ;; check for EAGAIN; these should not err
           (values nil (cerrno)))
          (t
           (dolist (sock sockets)
             (when (fdset-fd-isset rfds (sb-bsd-sockets:socket-file-descriptor
                                         (socket sock)))
               (setf (state sock) :READ))))))))
) ; progn

(defun map-socket-error (sock-err)
  (map-errno-error (sb-bsd-sockets::socket-error-errno sock-err)))

(defparameter +sbcl-condition-map+
  '((interrupted-error . interrupted-condition)
    #+(or ecl mkcl clasp)
    (sb-bsd-sockets::host-not-found-error . ns-host-not-found-error)))

(defparameter +sbcl-error-map+
  `((sb-bsd-sockets:address-in-use-error . address-in-use-error)
    (sb-bsd-sockets::no-address-error . address-not-available-error)
    (sb-bsd-sockets:bad-file-descriptor-error . bad-file-descriptor-error)
    (sb-bsd-sockets:connection-refused-error . connection-refused-error)
    (sb-bsd-sockets:invalid-argument-error . invalid-argument-error)
    (sb-bsd-sockets:no-buffers-error . no-buffers-error)
    (sb-bsd-sockets:operation-not-supported-error
     . operation-not-supported-error)
    (sb-bsd-sockets:operation-not-permitted-error
     . operation-not-permitted-error)
    (sb-bsd-sockets:protocol-not-supported-error
     . protocol-not-supported-error)
    #-(or ecl mkcl clasp)
    (sb-bsd-sockets:unknown-protocol
     . protocol-not-supported-error)
    (sb-bsd-sockets:socket-type-not-supported-error
     . socket-type-not-supported-error)
    (sb-bsd-sockets:network-unreachable-error . network-unreachable-error)
    (sb-bsd-sockets:operation-timeout-error . timeout-error)
    #-(or ecl mkcl clasp)
    (sb-sys:io-timeout . timeout-error)
    #+sbcl (sb-ext:timeout . timeout-error)

    ;; learnt from fiveam (suggested by Anton Vodonosov) for early SBCL versions
    #+#.(cl:if (cl:ignore-errors (cl:find-symbol "BROKEN-PIPE" "SB-INT"))
               '(and) '(or))
    (sb-int:broken-pipe . connection-aborted-error)

    (sb-bsd-sockets:socket-error . ,#'map-socket-error)

    ;; Nameservice errors: mapped to unknown-error
    #-(or ecl mkcl clasp)
    (sb-bsd-sockets:no-recovery-error . ns-no-recovery-error)
    #-(or ecl mkcl clasp)
    (sb-bsd-sockets:try-again-error . ns-try-again-error)
    #-(or ecl mkcl clasp)
    (sb-bsd-sockets:host-not-found-error . ns-host-not-found-error)))

;; this function servers as a general template for other backends
(defun handle-condition (condition &optional (socket nil) (host-or-ip nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (serious-condition
     (let* ((usock-error (cdr (assoc (type-of condition) +sbcl-error-map+)))
            (usock-error (if (functionp usock-error)
                             (funcall usock-error condition)
                             usock-error)))
       (declare (type symbol usock-error))
       (when usock-error
         (cond ((subtypep usock-error 'ns-error)
                (error usock-error :socket socket :host-or-ip host-or-ip))
               (t
                (error usock-error :socket socket))))))
    (condition
     (let* ((usock-cond (cdr (assoc (type-of condition) +sbcl-condition-map+)))
            (usock-cond (if (functionp usock-cond)
                            (funcall usock-cond condition)
                            usock-cond)))
       (when usock-cond
         (cond ((subtypep usock-cond 'ns-condition)
                (signal usock-cond :socket socket :host-or-ip host-or-ip))
               (t
                (signal usock-cond :socket socket))))))))

;;; "The socket stream ends up with a bogus name as it is created before
;;; the socket is connected, making things harder to debug than they need
;;; to be." -- Nikodemus Siivola <nikodemus@random-state.net>

(defvar *dummy-stream*
  (let ((stream (make-broadcast-stream)))
    (close stream)
    stream))

;;; Amusingly, neither SBCL's own, nor GBBopen's WITH-TIMEOUT is asynch
;;; unwind safe. The one I posted is -- that's what the WITHOUT-INTERRUPTS
;;; and WITH-LOCAL-INTERRUPTS were for. :) But yeah, it's miles saner than
;;; the SB-EXT:WITH-TIMEOUT. -- Nikodemus Siivola <nikodemus@random-state.net>

#+(and sbcl (not win32))
(defmacro %with-timeout ((seconds timeout-form) &body body)
  "Runs BODY as an implicit PROGN with timeout of SECONDS. If
timeout occurs before BODY has finished, BODY is unwound and
TIMEOUT-FORM is executed with its values returned instead.

Note that BODY is unwound asynchronously when a timeout occurs,
so unless all code executed during it -- including anything
down the call chain -- is asynch unwind safe, bad things will
happen. Use with care."
  (let ((exec (gensym)) (unwind (gensym)) (timer (gensym))
        (timeout (gensym)) (block (gensym)))
    `(block ,block
       (tagbody
          (flet ((,unwind ()
                   (go ,timeout))
                 (,exec ()
                   ,@body))
            (declare (dynamic-extent #',exec #',unwind))
            (let ((,timer (sb-ext:make-timer #',unwind)))
              (sb-sys:without-interrupts
                  (unwind-protect
                       (progn
                         (sb-ext:schedule-timer ,timer ,seconds)
                         (return-from ,block
                           (sb-sys:with-local-interrupts
                               (,exec))))
                    (sb-ext:unschedule-timer ,timer)))))
          ,timeout
          (return-from ,block ,timeout-form)))))

(defun get-hosts-by-name (name)
  (with-mapped-conditions (nil name)
    (multiple-value-bind (host4 host6)
        (sb-bsd-sockets:get-host-by-name name)
      (let ((addr4 (when host4
                     (sb-bsd-sockets::host-ent-addresses host4)))
            (addr6 (when host6
                     (sb-bsd-sockets::host-ent-addresses host6))))
        (append addr4 addr6)))))


;; determine if a socket condition indicates operation is in progress
(defun %socket-operation-condition-in-progress-p (condition)
  (typep condition 'sb-bsd-sockets:operation-in-progress))

;; determine if a socket condition indicates not-connected (yet) status
(defun %socket-operation-condition-not-connected-p (condition)
  (typep condition 'sb-bsd-sockets:not-connected-error))

;; enable the new non-blocking socket method
(defparameter *socket-connect-nonblock-wait*
  ;; trust SBCL errno to be handled correctly in SB-BSD-SOCKETS on all platforms
  #+sbcl
  t
  ;; trust that errno is done correctly above - how are BSD flavors marked in *features*?
  #+(and (or ecl mkcl) (or darwin linux openbsd freebsd netbsd bsd))
  t
  ;; for all other cases, we have no reason to think we handle errno correctly in
  ;; %socket-operation-condition-*
  #+(or clasp
        (and (or ecl mkcl) (not (or darwin linux openbsd freebsd netbsd bsd))))
  nil)

(defun socket-connect (host port &key (protocol :stream) (element-type 'character)
                       timeout deadline (nodelay t nodelay-specified)
                       local-host local-port
                       &aux
                       (sockopt-tcp-nodelay-p
                        (fboundp 'sb-bsd-sockets::sockopt-tcp-nodelay)))
  (when deadline (unsupported 'deadline 'socket-connect))
  #+(or ecl mkcl clasp)
  (when (and timeout #-clasp (not *socket-connect-nonblock-wait*))
    (unsupported 'timeout 'socket-connect))
  (when (and nodelay-specified
             ;; 20080802: ECL added this function to its sockets
             ;; package today. There's no guarantee the functions
             ;; we need are available, but we can make sure not to
             ;; call them if they aren't
             (not (eq nodelay :if-supported))
             (not sockopt-tcp-nodelay-p))
    (unsupported 'nodelay 'socket-connect))
  (when (eq nodelay :if-supported)
    (setf nodelay t))

  (let* ((remote (when host
                   (car (get-hosts-by-name (host-to-hostname host)))))
         (local (when local-host
                  (car (get-hosts-by-name (host-to-hostname local-host)))))
         (ipv6 (or (and remote (= 16 (length remote)))
                   (and local (= 16 (length local)))))
         (socket (make-instance #+sbcl (if ipv6
                                           'sb-bsd-sockets::inet6-socket
                                           'sb-bsd-sockets:inet-socket)
                                #+(or ecl mkcl clasp) 'sb-bsd-sockets:inet-socket
                                :type protocol
                                :protocol (case protocol
                                            (:stream :tcp)
                                            (:datagram :udp))))
         usocket
         ok)

    (unwind-protect
         (progn
           (ecase protocol
             (:stream
              ;; If make a real socket stream before the socket is
              ;; connected, it gets a misleading name so supply a
              ;; dummy value to start with.
              (setf usocket (make-stream-socket :socket socket :stream *dummy-stream*))
              ;; binghe: use SOCKOPT-TCP-NODELAY as internal symbol
              ;;         to pass compilation on ECL without it.
              (when (and nodelay-specified sockopt-tcp-nodelay-p)
                (setf (sb-bsd-sockets::sockopt-tcp-nodelay socket) nodelay))
              (when (or local-host local-port)
                (sb-bsd-sockets:socket-bind socket
                                            (if ipv6
                                                (or local (ipv6-host-to-vector "::0"))
                                                (or local (host-to-vector-quad *wildcard-host*)))
                                            (or local-port *auto-port*)))

              (with-mapped-conditions (usocket host)
                (if *socket-connect-nonblock-wait* ;; global var to disable new connect timeout
                    ;; case of new timeout code
                    (if timeout 
                        (let ((initial-blocking-mode  (sb-bsd-sockets:non-blocking-mode socket)))
                          ;; first connect
                          (progn
                            (setf (sb-bsd-sockets:non-blocking-mode socket) t) ;; non-blocking mode
                            (multiple-value-bind (retval err)
                                (ignore-errors (sb-bsd-sockets:socket-connect socket remote port))
                              ;; if the error generated is not EINPROGRESS then throw error
                              (when (and (not retval) err)
                                (when (not (%socket-operation-condition-in-progress-p err))
                                  (error err)))))
                          ;; then loop/sleep until ready
                          (loop
                              ;; start with very short wait time
                              with dt-sleep of-type double-float = 10d-6
                              with start-time of-type double-float
                                = (/ (* 1d0 (get-internal-real-time)) internal-time-units-per-second)
                              with end-time  of-type double-float 
                                = (+ start-time (float timeout 1d0))
                              with current-time of-type double-float = start-time
                              do
                                 ;;(format t "TIME: ~A DT: ~,7F~%" current-time dt-sleep)
                                 ;; check if there is a peer on other end
                                 (multiple-value-bind (peer err)
                                     (ignore-errors (sb-bsd-sockets:socket-peername socket))
                                   (cond (peer (return)) ;; socket has peer, so is connected
                                         ((and err ;; not 'not-connected' error, so it failed
                                               (not (%socket-operation-condition-not-connected-p err)))
                                          (error err))))
                                 (sleep dt-sleep)
                                 (setf current-time (/ (* 1d0 (get-internal-real-time))
                                                       internal-time-units-per-second))
                                 
                                 (when (>= current-time end-time)
                                   (error 'timeout-error))
                                 
                                 ;; Keep increasing sleep time in
                                 ;; 4 steps per decade but don't exceed the
                                 ;; end-time.  Max is 0.1 sec.
                                 (setf dt-sleep (min (* dt-sleep #.(sqrt (sqrt 10d0)))
                                                     0.1d0  ;; but not more than 0.1 sec
                                                     (max (- end-time current-time) ;; but don't oversleep
                                                          10d-6))))
                          ;; restore blocking mode
                          (setf (sb-bsd-sockets:non-blocking-mode socket) initial-blocking-mode))
                        ;; no timeout case
                        (sb-bsd-sockets:socket-connect socket remote port))
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; if not *socket-connect-nonblock-wait*, then use old timeout code
                    ;; which uses interrupts on SBCL, and doesn't work on ECL
                    #+(and sbcl (not win32))
                    (labels ((connect ()
                               (sb-bsd-sockets:socket-connect socket remote port)))
                      (if timeout
                          (%with-timeout (timeout (error 'sb-ext:timeout)) (connect))
                          (connect)))
                    #+(or ecl mkcl clasp (and sbcl win32))
                    (sb-bsd-sockets:socket-connect socket remote port))
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                ;; Now that we're connected make the stream.
                (setf (socket-stream usocket)
                      (sb-bsd-sockets:socket-make-stream socket 
                        :input t :output t :buffering :full
                        :element-type element-type
                        ;; Robert Brown <robert.brown@gmail.com> said on Aug 4, 2011:
                        ;; ... This means that SBCL streams created by usocket have a true
                        ;; serve-events property.  When writing large amounts of data to several
                        ;; streams, the kernel will eventually stop accepting data from SBCL.
                        ;; When this happens, SBCL either waits for I/O to be possible on
                        ;; the file descriptor it's writing to or queues the data to be flushed later.
                        ;; Because usocket streams specify serve-events as true, SBCL
                        ;; always queues.  Instead, it should wait for I/O to be available and
                        ;; write the remaining data to the socket.  That's what serve-events
                        ;; equal to NIL gets you.
                        ;;
                        ;; Nikodemus Siivola <nikodemus@random-state.net> said on Aug 8, 2011:
                        ;; It's set to T for purely historical reasons, and will soon change to
                        ;; NIL in SBCL. (The docstring has warned of T being a temporary default
                        ;; for as long as the :SERVE-EVENTS keyword argument has existed.)
                        :serve-events nil))))
             (:datagram
              (when (or local-host local-port)
                (sb-bsd-sockets:socket-bind socket
                                            (if ipv6
                                                (or local (ipv6-host-to-vector "::0"))
                                                (or local (host-to-vector-quad *wildcard-host*)))
                                            (or local-port *auto-port*)))
              (setf usocket (make-datagram-socket socket))
              (when (and host port)
                (with-mapped-conditions (usocket)
                  (sb-bsd-sockets:socket-connect socket remote port)
                  (setf (connected-p usocket) t)))))
           (setf ok t))
      ;; Clean up in case of an error.
      (unless ok
        (sb-bsd-sockets:socket-close socket :abort t)))
    usocket))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'character))
  (let* (#+sbcl
         (local (when host
                  (car (get-hosts-by-name (host-to-hostname host)))))
         #+sbcl
         (ipv6 (and local (= 16 (length local))))
         (reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (ip #+sbcl (if (and local (not (eq host *wildcard-host*)))
                        local
                        (hbo-to-vector-quad sb-bsd-sockets-internal::inaddr-any))
             #+(or ecl mkcl clasp) (host-to-vector-quad host))
         (sock (make-instance #+sbcl (if ipv6
                                         'sb-bsd-sockets::inet6-socket
                                         'sb-bsd-sockets:inet-socket)
                              #+(or ecl mkcl clasp) 'sb-bsd-sockets:inet-socket
                              :type :stream
                              :protocol :tcp)))
    (handler-case
        (with-mapped-conditions (nil host)
          (setf (sb-bsd-sockets:sockopt-reuse-address sock) reuseaddress)
          (sb-bsd-sockets:socket-bind sock ip port)
          (sb-bsd-sockets:socket-listen sock backlog)
          (make-stream-server-socket sock :element-type element-type))
      (t (c)
        ;; Make sure we don't leak filedescriptors
        (sb-bsd-sockets:socket-close sock)
        (error c)))))

;;; "2. SB-BSD-SOCKETS:SOCKET-ACCEPT method returns NIL for EAGAIN/EINTR,
;;; instead of raising a condition. It's always possible for
;;; SOCKET-ACCEPT on non-blocking socket to fail, even after the socket
;;; was detected to be ready: connection might be reset, for example.
;;;
;;; "I had to redefine SOCKET-ACCEPT method of STREAM-SERVER-USOCKET to
;;; handle this situation. Here is the redefinition:" -- Anton Kovalenko <anton@sw4me.com>

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (with-mapped-conditions (usocket)
    (let ((socket (sb-bsd-sockets:socket-accept (socket usocket))))
      (when socket
        (prog1
          (make-stream-socket
           :socket socket
           :stream (sb-bsd-sockets:socket-make-stream
                    socket
                    :input t :output t :buffering :full
                    :element-type (or element-type
                                      (element-type usocket))))

          ;; next time wait for event again if we had EAGAIN/EINTR
          ;; or else we'd enter a tight loop of failed accepts
          #+win32
          (setf (%ready-p usocket) nil))))))

;; Sockets and their associated streams are modelled as
;; different objects. Be sure to close the stream (which
;; closes the socket too) when closing a stream-socket.
(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
    (sb-bsd-sockets:socket-close (socket usocket))))

;; usocket leaks file descriptors on sb-int:broken-pipe conditions (#64)
;;
;; "If abort is true, an attempt is made to clean up any side effects of having
;;  created stream. If stream performs output to a file that was created when
;;  the stream was created, the file is deleted and any previously existing file
;;  is not superseded. ... If abort is true and the stream is an output file stream,
;;  its associated file might be deleted." (ANSI)
;;
;; adding (:abort t) fixes the potential leaks of socket fds.
(defmethod socket-close ((usocket stream-usocket))
  (with-mapped-conditions (usocket)
    (close (socket-stream usocket) :abort t)))

(defmethod socket-shutdown ((usocket stream-usocket) direction)
  (with-mapped-conditions (usocket)
    (sb-bsd-sockets::socket-shutdown (socket usocket) :direction direction)))

(defmethod socket-send ((usocket datagram-usocket) buffer size &key host port (offset 0))
  (let ((remote (when host
                  (car (get-hosts-by-name (host-to-hostname host))))))
    (with-mapped-conditions (usocket host)
      (let* ((s (socket usocket))
             (dest (if (and host port) (list remote port) nil))
             (real-buffer (if (zerop offset)
                              buffer
                              (subseq buffer offset (+ offset size)))))
        (sb-bsd-sockets:socket-send s real-buffer size :address dest)))))

(defmethod socket-receive ((usocket datagram-usocket) buffer length
                           &key (element-type 'u8))
  #+sbcl
  (declare (values (simple-array u8 (*)) ; buffer
                   (integer 0)                          ; size
                   (simple-array u8 (*)) ; host
                   (unsigned-byte 16)                   ; port
                   &optional))
  (with-mapped-conditions (usocket)
    (let ((s (socket usocket)))
      (sb-bsd-sockets:socket-receive s buffer length :element-type element-type))))

(defmethod get-local-name ((usocket usocket))
  (sb-bsd-sockets:socket-name (socket usocket)))

(defmethod get-peer-name ((usocket stream-usocket))
  (sb-bsd-sockets:socket-peername (socket usocket)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun get-host-by-address (address)
  (with-mapped-conditions (nil address)
    (sb-bsd-sockets::host-ent-name
        (sb-bsd-sockets:get-host-by-address address))))

#+(and sbcl (not win32))
(progn
  (defun %setup-wait-list (wait-list)
    (declare (ignore wait-list)))

  (defun %add-waiter (wait-list waiter)
    (push (socket waiter) (wait-list-%wait wait-list)))

  (defun %remove-waiter (wait-list waiter)
    (setf (wait-list-%wait wait-list)
          (remove (socket waiter) (wait-list-%wait wait-list))))

  (defun wait-for-input-internal (sockets &key timeout)
    (with-mapped-conditions ()
      (sb-alien:with-alien ((rfds (sb-alien:struct sb-unix:fd-set)))
         (sb-unix:fd-zero rfds)
         (dolist (socket (wait-list-%wait sockets))
           (sb-unix:fd-set
            (sb-bsd-sockets:socket-file-descriptor socket)
            rfds))
         (multiple-value-bind
             (secs musecs)
             (split-timeout (or timeout 1))
           (let* ((wait-list (wait-list-%wait sockets))
                  count err)
             (if (null wait-list)
                 (setq count 0) ;; no need to call
               (multiple-value-setq (count err)
                 (sb-unix:unix-fast-select
                  ;; "invalid number of arguments: 0" if wait-list is null.
                  (1+ (reduce #'max wait-list
                              :key #'sb-bsd-sockets:socket-file-descriptor))
                  (sb-alien:addr rfds) nil nil
                  (when timeout secs) (when timeout musecs))))
             (if (null count) ; something wrong in #'sb-unix:unix-fast-select
                 (unless (= err sb-unix:eintr)
                   (error (map-errno-error err)))
                 (when (< 0 count) ; do nothing if count = 0
                   ;; process the result...
                   (dolist (x (wait-list-waiters sockets))
                     (when (sb-unix:fd-isset
                            (sb-bsd-sockets:socket-file-descriptor
                             (socket x))
                            rfds)
                       (setf (state x) :READ))))))))))
) ; progn

;;; WAIT-FOR-INPUT support for SBCL on Windows platform (Chun Tian (binghe))
;;; Based on LispWorks version written by Erik Huelsmann.

#+win32 ; shared by ECL and SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +wsa-wait-failed+ #xffffffff)
  (defconstant +wsa-infinite+ #xffffffff)
  (defconstant +wsa-wait-event-0+ 0)
  (defconstant +wsa-wait-timeout+ 258))

#+win32 ; shared by ECL and SBCL
(progn
  (defconstant fd-read 1)
  (defconstant fd-read-bit 0)
  (defconstant fd-write 2)
  (defconstant fd-write-bit 1)
  (defconstant fd-oob 4)
  (defconstant fd-oob-bit 2)
  (defconstant fd-accept 8)
  (defconstant fd-accept-bit 3)
  (defconstant fd-connect 16)
  (defconstant fd-connect-bit 4)
  (defconstant fd-close 32)
  (defconstant fd-close-bit 5)
  (defconstant fd-qos 64)
  (defconstant fd-qos-bit 6)
  (defconstant fd-group-qos 128)
  (defconstant fd-group-qos-bit 7)
  (defconstant fd-routing-interface 256)
  (defconstant fd-routing-interface-bit 8)
  (defconstant fd-address-list-change 512)
  (defconstant fd-address-list-change-bit 9)
  (defconstant fd-max-events 10)
  (defconstant fionread 1074030207)

  ;; Note: for ECL, socket-handle will return raw Windows Handle,
  ;;       while SBCL returns OSF Handle instead.
  (defun socket-handle (usocket)
    (sb-bsd-sockets:socket-file-descriptor (socket usocket)))

  (defun socket-ready-p (socket)
    (if (typep socket 'stream-usocket)
        (plusp (bytes-available-for-read socket))
      (%ready-p socket)))

  (defun waiting-required (sockets)
    (notany #'socket-ready-p sockets))

  (defun raise-usock-err (errno &optional socket)
    (error 'unknown-error
           :socket socket
           :real-error errno))

  (defun wait-for-input-internal (wait-list &key timeout)
    (when (waiting-required (wait-list-waiters wait-list))
      (let ((rv (wsa-wait-for-multiple-events 1 (wait-list-%wait wait-list)
                                              nil
                                              (if timeout
                                                  (truncate (* 1000 timeout))
                                                  +wsa-infinite+)
                                              nil)))
        (ecase rv
          ((#.+wsa-wait-event-0+)
           (update-ready-and-state-slots wait-list))
          ((#.+wsa-wait-timeout+)) ; do nothing here
          ((#.+wsa-wait-failed+)
           (maybe-wsa-error rv))))))

  (defun %add-waiter (wait-list waiter)
    (let ((events (etypecase waiter
                    (stream-server-usocket (logior fd-connect fd-accept fd-close))
                    (stream-usocket (logior fd-read))
                    (datagram-usocket (logior fd-read)))))
      (maybe-wsa-error
       (wsa-event-select (os-socket-handle waiter) (os-wait-list-%wait wait-list) events)
       waiter)))

  (defun %remove-waiter (wait-list waiter)
    (maybe-wsa-error
     (wsa-event-select (os-socket-handle waiter) (os-wait-list-%wait wait-list) 0)
     waiter))
) ; progn

#+(and sbcl win32)
(progn
  ;; "SOCKET is defined as intptr_t in Windows headers; however, WS-SOCKET
  ;; is defined as unsigned-int, i.e. 32-bit even on 64-bit platform.  It
  ;; seems to be a good thing to redefine WS-SOCKET as SB-ALIEN:SIGNED,
  ;; which is always machine word-sized (exactly as intptr_t;
  ;; N.B. as of Windows/x64, long and signed-long are 32-bit, and thus not
  ;; enough -- potentially)."
  ;; -- Anton Kovalenko <anton@sw4me.com>, Mar 22, 2011
  (sb-alien:define-alien-type ws-socket sb-alien:signed)

  (sb-alien:define-alien-type ws-dword sb-alien:unsigned-long)
  
  ;; WS-EVENT was formerly defined as [internal, now removed] SB-ALIEN::HINSTANCE (synonym for SIGNED)
  (sb-alien:define-alien-type ws-event sb-alien:signed) 

  (sb-alien:define-alien-type nil
    (sb-alien:struct wsa-network-events
      (network-events sb-alien:long)
      (error-code (array sb-alien:int 10)))) ; 10 = fd-max-events

  (sb-alien:define-alien-routine ("WSACreateEvent" wsa-event-create)
      ws-event) ; return type only

  (sb-alien:define-alien-routine ("WSACloseEvent" wsa-event-close)
      (boolean #.sb-vm::n-machine-word-bits)
    (event-object ws-event))

  ;; not used
  (sb-alien:define-alien-routine ("WSAResetEvent" wsa-reset-event)
      (boolean #.sb-vm::n-machine-word-bits)
    (event-object ws-event))

  (sb-alien:define-alien-routine ("WSAEnumNetworkEvents" wsa-enum-network-events)
      sb-alien:int
    (socket ws-socket)
    (event-object ws-event)
    (network-events (* (sb-alien:struct wsa-network-events))))

  (sb-alien:define-alien-routine ("WSAEventSelect" wsa-event-select)
      sb-alien:int
    (socket ws-socket)
    (event-object ws-event)
    (network-events sb-alien:long))

  (sb-alien:define-alien-routine ("WSAWaitForMultipleEvents" wsa-wait-for-multiple-events)
      ws-dword
    (number-of-events ws-dword)
    (events (* ws-event))
    (wait-all-p (boolean #.sb-vm::n-machine-word-bits))
    (timeout ws-dword)
    (alertable-p (boolean #.sb-vm::n-machine-word-bits)))

  (sb-alien:define-alien-routine ("ioctlsocket" wsa-ioctlsocket)
      sb-alien:int
    (socket ws-socket)
    (cmd sb-alien:long)
    (argp (* sb-alien:unsigned-long)))

  (defun maybe-wsa-error (rv &optional socket)
    (unless (zerop rv)
      (raise-usock-err (sockint::wsa-get-last-error) socket)))

  (defun os-socket-handle (usocket)
    (sb-bsd-sockets:socket-file-descriptor (socket usocket)))

  (defun bytes-available-for-read (socket)
    (sb-alien:with-alien ((int-ptr sb-alien:unsigned-long))
      (maybe-wsa-error (wsa-ioctlsocket (os-socket-handle socket) fionread (sb-alien:addr int-ptr))
                       socket)
      (prog1 int-ptr
        (when (plusp int-ptr)
          (setf (state socket) :read)))))

  (defun map-network-events (func network-events)
    (let ((event-map (sb-alien:slot network-events 'network-events))
          (error-array (sb-alien:slot network-events 'error-code)))
      (unless (zerop event-map)
        (dotimes (i fd-max-events)
          (unless (zerop (ldb (byte 1 i) event-map)) ;;### could be faster with ash and logand?
            (funcall func (sb-alien:deref error-array i)))))))

  (defun update-ready-and-state-slots (wait-list)
    (loop with sockets = (wait-list-waiters wait-list)
          for socket in sockets do
      (if (%ready-p socket)
          (progn
            (setf (state socket) :READ))
        (sb-alien:with-alien ((network-events (sb-alien:struct wsa-network-events)))
          (let ((rv (wsa-enum-network-events (os-socket-handle socket)
                                             (os-wait-list-%wait wait-list)
                                             (sb-alien:addr network-events))))
            (if (zerop rv)
                (map-network-events
                 #'(lambda (err-code)
                     (if (zerop err-code)
                         (progn
                           (setf (state socket) :READ)
                           (when (stream-server-socket-p socket)
                             (setf (%ready-p socket) t)))
                       (raise-usock-err err-code socket)))
                 network-events)
              (maybe-wsa-error rv socket)))))))

  (defun os-wait-list-%wait (wait-list)
    (sb-alien:deref (wait-list-%wait wait-list)))

  (defun (setf os-wait-list-%wait) (value wait-list)
    (setf (sb-alien:deref (wait-list-%wait wait-list)) value))

  ;; "Event handles are leaking in current SBCL backend implementation,
  ;; because of SBCL-unfriendly usage of finalizers.
  ;;
  ;; "SBCL never calls a finalizer that closes over a finalized object: a
  ;; reference from that closure prevents its collection forever. That's
  ;; the case with USOCKET in %SETUP-WAIT-LIST.
  ;;
  ;; "I use the following redefinition of %SETUP-WAIT-LIST: 
  ;;
  ;; "Of course it may be rewritten with more clarity, but you can see the
  ;; core idea: I'm closing over those components of WAIT-LIST that I need
  ;; for finalization, not the wait-list itself. With the original
  ;; %SETUP-WAIT-LIST, hunchentoot stops working after ~100k accepted
  ;; connections; it doesn't happen with redefined %SETUP-WAIT-LIST."
  ;;
  ;; -- Anton Kovalenko <anton@sw4me.com>, Mar 22, 2011

  (defun %setup-wait-list (wait-list)
    (setf (wait-list-%wait wait-list) (sb-alien:make-alien ws-event))
    (setf (os-wait-list-%wait wait-list) (wsa-event-create))
    (sb-ext:finalize wait-list
                     (let ((event-handle (os-wait-list-%wait wait-list))
                           (alien (wait-list-%wait wait-list)))
                       #'(lambda ()
                           (wsa-event-close event-handle)
                           (unless (null alien)
                             (sb-alien:free-alien alien))))))

) ; progn

#+(and (or ecl mkcl clasp) (not win32))
(progn
  (defun wait-for-input-internal (wl &key timeout)
    (with-mapped-conditions ()
      (multiple-value-bind (secs usecs)
          (split-timeout (or timeout 1))
        (multiple-value-bind (result-fds err)
            (read-select wl (when timeout secs) usecs)
          (declare (ignore result-fds))
          (unless (null err)
            (error (map-errno-error err)))))))

  (defun %setup-wait-list (wl)
    (setf (wait-list-%wait wl)
          (fdset-alloc)))

  (defun %add-waiter (wl w)
    (declare (ignore wl w)))

  (defun %remove-waiter (wl w)
    (declare (ignore wl w)))
) ; progn

#+(and (or ecl mkcl clasp) win32 (not ecl-bytecmp))
(progn
  (defun maybe-wsa-error (rv &optional syscall)
    (unless (zerop rv)
      (sb-bsd-sockets::socket-error syscall)))

  (defun %setup-wait-list (wl)
    (setf (wait-list-%wait wl)
          (ffi:c-inline () () :int
           "WSAEVENT event;
            event = WSACreateEvent();
            @(return) = event;")))

  (defun %add-waiter (wait-list waiter)
    (let ((events (etypecase waiter
                    (stream-server-usocket (logior fd-connect fd-accept fd-close))
                    (stream-usocket (logior fd-read))
                    (datagram-usocket (logior fd-read)))))
      (maybe-wsa-error
       (ffi:c-inline ((socket-handle waiter) (wait-list-%wait wait-list) events)
                     (:fixnum :fixnum :fixnum) :fixnum
        "int result;
         result = WSAEventSelect((SOCKET)#0, (WSAEVENT)#1, (long)#2);
         @(return) = result;")
       '%add-waiter)))

  (defun %remove-waiter (wait-list waiter)
    (maybe-wsa-error
     (ffi:c-inline ((socket-handle waiter) (wait-list-%wait wait-list))
                   (:fixnum :fixnum) :fixnum
      "int result;
       result = WSAEventSelect((SOCKET)#0, (WSAEVENT)#1, 0L);
       @(return) = result;")
     '%remove-waiter))

  ;; TODO: how to handle error (result) in this call?
  (declaim (inline %bytes-available-for-read))
  (defun %bytes-available-for-read (socket)
    (ffi:c-inline ((socket-handle socket)) (:fixnum) :fixnum
     "u_long nbytes;
      int result;
      nbytes = 0L;
      result = ioctlsocket((SOCKET)#0, FIONREAD, &nbytes);
      @(return) = nbytes;"))

  (defun bytes-available-for-read (socket)
    (let ((nbytes (%bytes-available-for-read socket)))
      (when (plusp nbytes)
        (setf (state socket) :read))
      nbytes))

  (defun update-ready-and-state-slots (wait-list)
    (loop with sockets = (wait-list-waiters wait-list)
          for socket in sockets do
      (if (%ready-p socket)
          (setf (state socket) :READ)
        (let ((events (etypecase socket
                        (stream-server-usocket (logior fd-connect fd-accept fd-close))
                        (stream-usocket (logior fd-read))
                        (datagram-usocket (logior fd-read)))))
          ;; TODO: check the iErrorCode array
          (multiple-value-bind (valid-p ready-p)
              (ffi:c-inline ((socket-handle socket) events) (:fixnum :fixnum)
                                                            (values :bool :bool)
                ;; TODO: replace 0 (2nd arg) with (wait-list-%wait wait-list)
                "WSANETWORKEVENTS network_events;
                 int i, result;
                 result = WSAEnumNetworkEvents((SOCKET)#0, 0, &network_events);
                 if (!result) {
                   @(return 0) = Ct;
                   @(return 1) = (#1 & network_events.lNetworkEvents)? Ct : Cnil;
                 } else {
                   @(return 0) = Cnil;
                   @(return 1) = Cnil;
                 }")
            (if valid-p
                (when ready-p
                  (setf (state socket) :READ)
                  (when (stream-server-socket-p socket)
                    (setf (%ready-p socket) t)))
              (sb-bsd-sockets::socket-error 'update-ready-and-state-slots)))))))

  (defun wait-for-input-internal (wait-list &key timeout)
    (when (waiting-required (wait-list-waiters wait-list))
      (let ((rv (ffi:c-inline ((wait-list-%wait wait-list)
                               (if timeout
                                   (truncate (* 1000 timeout))
                                   +wsa-infinite+))
                              (:fixnum :fixnum) :fixnum
                 "DWORD result;
                  WSAEVENT events[1];
                  events[0] = (WSAEVENT)#0;
                  result = WSAWaitForMultipleEvents(1, events, NULL, #1, NULL);
                  @(return) = result;")))
        (ecase rv
          ((#.+wsa-wait-event-0+)
           (update-ready-and-state-slots (wait-list-waiters wait-list)))
          ((#.+wsa-wait-timeout+)) ; do nothing here
          ((#.+wsa-wait-failed+)
           (sb-bsd-sockets::socket-error 'wait-for-input-internal))))))

) ; progn


;; put here because option.lisp is for native backend only
(defparameter *backend* :native)

;;; Interface definition

(defgeneric socket-option (socket option &key)
  (:documentation
   "Get a socket's internal options"))

(defgeneric (setf socket-option) (new-value socket option &key)
  (:documentation
   "Set a socket's internal options"))

;;; Handling of wrong type of arguments

(defmethod socket-option ((socket usocket) (option t) &key)
  (declare (ignore socket))
  (error 'type-error :datum option :expected-type 'keyword))

(defmethod (setf socket-option) (new-value (socket usocket) (option t) &key)
  (declare (ignore new-value))
  (socket-option socket option))

(defmethod socket-option ((socket usocket) (option symbol) &key)
  (declare (ignore socket))
  (if (keywordp option)
    (error 'unimplemented :feature option :context 'socket-option)
    (error 'type-error :datum option :expected-type 'keyword)))

(defmethod (setf socket-option) (new-value (socket usocket) (option symbol) &key)
  (declare (ignore new-value))
  (socket-option socket option))

;;; Socket option: RECEIVE-TIMEOUT (SO_RCVTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :receive-timeout)) &key)
  (declare (ignore option))
  (sb-impl::fd-stream-timeout (socket-stream usocket)))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :receive-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    (declare (ignorable socket timeout))
    (setf (sb-impl::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'single-float))
    new-value))

;;; Socket option: SEND-TIMEOUT (SO_SNDTIMEO)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :send-timeout)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (sb-impl::fd-stream-timeout (socket-stream usocket))))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :send-timeout)) &key)
  (declare (type number new-value) (ignorable new-value option))
  (let ((socket (socket usocket))
        (timeout new-value))
    (setf (sb-impl::fd-stream-timeout (socket-stream usocket))
          (coerce timeout 'single-float))
    new-value))

;;; Socket option: REUSE-ADDRESS (SO_REUSEADDR), for TCP server

(defmethod socket-option ((usocket stream-server-usocket)
                          (option (eql :reuse-address)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (sb-bsd-sockets:sockopt-reuse-address socket)))

(defmethod (setf socket-option) (new-value (usocket stream-server-usocket)
                                           (option (eql :reuse-address)) &key)
  (declare (type boolean new-value) (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) new-value)
    new-value))

;;; Socket option: BROADCAST (SO_BROADCAST), for UDP client

(defmethod socket-option ((usocket datagram-usocket)
                          (option (eql :broadcast)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (sb-bsd-sockets:sockopt-broadcast socket)))

(defmethod (setf socket-option) (new-value (usocket datagram-usocket)
                                           (option (eql :broadcast)) &key)
  (declare (type boolean new-value)
           (ignorable new-value option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    (setf (sb-bsd-sockets:sockopt-broadcast socket) new-value)
    new-value))

;;; Socket option: TCP-NODELAY (TCP_NODELAY), for TCP client

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-no-delay)) &key)
  (declare (ignorable option))
  (socket-option usocket :tcp-nodelay))

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-nodelay)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (declare (ignorable socket))
    (sb-bsd-sockets::sockopt-tcp-nodelay socket)))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-no-delay)) &key)
  (declare (ignorable option))
  (setf (socket-option usocket :tcp-nodelay) new-value))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-nodelay)) &key)
  (declare (type boolean new-value)
           (ignorable new-value option))
  (let ((socket (socket usocket)))
    (setf (sb-bsd-sockets::sockopt-tcp-nodelay socket) new-value)
    new-value))

;;; Socket option: TCP-KEEPALIVE (SO_KEEPALIVE)

(defmethod socket-option ((usocket stream-usocket)
                          (option (eql :tcp-keepalive)) &key)
  (declare (ignorable option))
  (let ((socket (socket usocket)))
    (sb-bsd-sockets:sockopt-keep-alive socket)))

(defmethod (setf socket-option) (new-value (usocket stream-usocket)
                                           (option (eql :tcp-keepalive)) &key)
  (declare (type boolean new-value)
           (ignorable new-value option))
  (let ((socket (socket usocket)))
    (setf (sb-bsd-sockets:sockopt-keep-alive socket) new-value)
    new-value))

