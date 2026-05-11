;;;; epsilon.http.protocol.negotiator - HTTP Protocol Negotiation
;;;;
;;;; Handles protocol negotiation between HTTP/1.1, HTTP/2, and HTTP/3.
;;;; Supports ALPN (Application-Layer Protocol Negotiation) for TLS connections,
;;;; h2c (HTTP/2 cleartext) upgrade for non-TLS connections, and HTTP/3 over QUIC.
;;;;
;;;; HTTP/3 Discovery:
;;;;   - Alt-Svc header: Servers advertise HTTP/3 support in HTTP/1.1 or HTTP/2 responses
;;;;   - DNS HTTPS records: Can indicate HTTP/3 support
;;;;   - Prior knowledge: Client knows server supports HTTP/3

(defpackage :epsilon.http.protocol.negotiator
  (:use :cl)
  (:import
   (epsilon.crypto crypto)
   (epsilon.net net)
   (epsilon.http.h2 h2))
  (:export
   ;; Protocol identifiers
   +protocol-h3+
   +protocol-h2+
   +protocol-h1+
   +protocol-h2c+

   ;; Protocol detection
   protocol-version
   protocol-version-p
   http3-p
   http2-p
   http1-p

   ;; ALPN negotiation
   alpn-protocols
   negotiate-protocol
   negotiated-protocol

   ;; Protocol selection
   select-protocol
   preferred-protocol
   select-best-protocol

   ;; HTTP/3 availability
   h3-available-p
   h3-supported-p

   ;; Alt-Svc support
   parse-alt-svc
   process-alt-svc-header
   *alt-svc-cache*
   clear-alt-svc-cache

   ;; Connection wrapper
   make-protocol-connection
   make-protocol-connection-for-tls
   make-protocol-connection-for-cleartext
   make-protocol-connection-for-quic
   protocol-connection
   protocol-connection-p
   protocol-connection-socket
   protocol-connection-tls
   protocol-connection-protocol
   protocol-connection-host
   protocol-connection-port

   ;; Upgrade support
   can-upgrade-p
   initiate-h2c-upgrade

   ;; Protocol fallback
   *protocol-fallback-enabled*
   *h3-connect-timeout*
   protocol-fallback-order
   select-protocol-with-fallback
   make-request-with-fallback
   parse-url-for-protocol))

;;;; Protocol Constants
;;;; Use EVAL-WHEN to ensure constants are defined at compile time properly

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+protocol-h3+)
    (defconstant +protocol-h3+ "h3"
      "HTTP/3 over QUIC (ALPN identifier)"))

  (unless (boundp '+protocol-h2+)
    (defconstant +protocol-h2+ "h2"
      "HTTP/2 over TLS (ALPN identifier)"))

  (unless (boundp '+protocol-h1+)
    (defconstant +protocol-h1+ "http/1.1"
      "HTTP/1.1 (ALPN identifier)"))

  (unless (boundp '+protocol-h2c+)
    (defconstant +protocol-h2c+ "h2c"
      "HTTP/2 cleartext (upgrade token)")))

;;;; Default ALPN Protocol List

(defparameter *default-alpn-protocols* (list +protocol-h2+ +protocol-h1+)
  "Default ALPN protocol preference order for TLS connections.
   HTTP/2 is preferred when available.
   Note: HTTP/3 uses QUIC, not TLS directly, so is not in this list.")

(defparameter *prefer-http2* t
  "When true, prefer HTTP/2 over HTTP/1.1 when both are available.")

(defparameter *prefer-http3* nil
  "When true, prefer HTTP/3 over HTTP/2 when available.
   Requires Alt-Svc discovery or prior knowledge of HTTP/3 support.")

;;;; Alt-Svc Cache for HTTP/3 Discovery

(defvar *alt-svc-cache* (make-hash-table :test 'equal)
  "Cache of Alt-Svc entries: (host . port) -> (list entry ...)")

(defvar *alt-svc-cache-mutex* nil
  "Mutex for thread-safe Alt-Svc cache access")

(defstruct alt-svc-entry
  "Alt-Svc entry for HTTP/3 discovery"
  (protocol "h3" :type string)
  (host nil :type (or null string))
  (port 443 :type integer)
  (max-age 86400 :type integer)
  (persist nil :type boolean)
  (expires-at 0 :type integer))

;;;; Protocol Version Detection

(defun protocol-version (identifier)
  "Convert protocol identifier to version keyword.
   Returns :h3, :h2, :h1, or nil for unknown."
  (cond
    ((string= identifier +protocol-h3+) :h3)
    ((string= identifier +protocol-h2+) :h2)
    ((string= identifier +protocol-h2c+) :h2)
    ((string= identifier +protocol-h1+) :h1)
    ((string= identifier "HTTP/1.1") :h1)
    ((string= identifier "HTTP/1.0") :h1)
    ((string= identifier "HTTP/2") :h2)
    ((string= identifier "HTTP/2.0") :h2)
    ((string= identifier "HTTP/3") :h3)
    ((string= identifier "HTTP/3.0") :h3)
    (t nil)))

(defun protocol-version-p (identifier version)
  "Check if identifier matches the given version keyword."
  (eq (protocol-version identifier) version))

(defun http3-p (identifier)
  "Check if identifier indicates HTTP/3."
  (protocol-version-p identifier :h3))

(defun http2-p (identifier)
  "Check if identifier indicates HTTP/2."
  (protocol-version-p identifier :h2))

(defun http1-p (identifier)
  "Check if identifier indicates HTTP/1.1."
  (protocol-version-p identifier :h1))

;;;; ALPN Protocol List

(defun alpn-protocols (&key prefer-h2)
  "Return ALPN protocol list in preference order.
   If PREFER-H2 is true (default based on *prefer-http2*),
   HTTP/2 is listed first."
  (let ((prefer (if (null prefer-h2) *prefer-http2* prefer-h2)))
    (if prefer
        (list +protocol-h2+ +protocol-h1+)
        (list +protocol-h1+ +protocol-h2+))))

;;;; Protocol Connection Wrapper

(defstruct protocol-connection
  "Wrapper for a connection with known protocol.
   Allows unified handling of HTTP/1.1 and HTTP/2 connections."
  (socket nil :type t)
  (tls nil :type t)
  (protocol :h1 :type keyword)
  (host "" :type string)
  (port 80 :type integer)
  (alpn-negotiated nil :type boolean)
  (created-time 0 :type integer)
  (stream-count 0 :type integer)      ; For HTTP/2: active streams
  (max-streams 100 :type integer)     ; For HTTP/2: max concurrent streams
  (settings nil :type list))          ; For HTTP/2: connection settings

;;;; Protocol Negotiation

(defun negotiate-protocol (tls-connection)
  "Get the negotiated ALPN protocol from a TLS connection.
   Returns protocol keyword (:h2 or :h1) or nil if not negotiated."
  (when tls-connection
    (let ((alpn (ignore-errors
                  (crypto:connection-alpn-protocol tls-connection))))
      (when alpn
        (protocol-version alpn)))))

(defun negotiated-protocol (connection)
  "Get the negotiated protocol for a protocol-connection."
  (if (protocol-connection-p connection)
      (protocol-connection-protocol connection)
      :h1))  ; Default to HTTP/1.1 for raw connections

;;;; Protocol Selection

(defun select-protocol (host port &key
                                    ssl-p
                                    (prefer-h2 *prefer-http2*)
                                    force-protocol)
  "Determine which protocol to use for a connection.

   Arguments:
     HOST - Target hostname
     PORT - Target port
     SSL-P - Whether to use TLS
     PREFER-H2 - Prefer HTTP/2 when available
     FORCE-PROTOCOL - Force specific protocol (:h1 or :h2)

   Returns: (values protocol alpn-list)
     PROTOCOL - :h1 or :h2
     ALPN-LIST - ALPN protocols to advertise (nil for cleartext)"
  (declare (ignore host port))  ; Reserved for future host-specific protocol selection
  (cond
    ;; Forced protocol selection
    (force-protocol
     (values force-protocol
             (when ssl-p
               (list (if (eq force-protocol :h2) +protocol-h2+ +protocol-h1+)))))

    ;; TLS: Use ALPN negotiation
    (ssl-p
     (values (if prefer-h2 :h2 :h1)
             (alpn-protocols :prefer-h2 prefer-h2)))

    ;; Cleartext: HTTP/1.1 only (h2c upgrade is separate path)
    (t
     (values :h1 nil))))

(defun preferred-protocol (&key ssl-p h2-available)
  "Return the preferred protocol given capabilities.

   Arguments:
     SSL-P - Using TLS
     H2-AVAILABLE - HTTP/2 is available/loaded

   Returns: :h1 or :h2"
  (if (and *prefer-http2* h2-available)
      (if ssl-p :h2 :h1)  ; h2c not preferred by default
      :h1))

;;;; Connection Creation

(defun make-protocol-connection-for-tls (socket tls-conn host port)
  "Create a protocol-connection from a TLS connection.
   Detects negotiated protocol from ALPN."
  (let ((protocol (or (negotiate-protocol tls-conn) :h1)))
    (make-protocol-connection
     :socket socket
     :tls tls-conn
     :protocol protocol
     :host host
     :port port
     :alpn-negotiated (not (null (negotiate-protocol tls-conn)))
     :created-time (get-universal-time))))

(defun make-protocol-connection-for-cleartext (socket host port &key (protocol :h1))
  "Create a protocol-connection for cleartext HTTP.
   Default is HTTP/1.1; HTTP/2 requires explicit h2c upgrade."
  (make-protocol-connection
   :socket socket
   :tls nil
   :protocol protocol
   :host host
   :port port
   :alpn-negotiated nil
   :created-time (get-universal-time)))

;;;; HTTP/2 Cleartext (h2c) Upgrade Support

(defparameter *h2c-upgrade-enabled* nil
  "When true, attempt h2c upgrade for cleartext HTTP/2.
   Disabled by default for security (prefer TLS).")

(defun can-upgrade-p (connection)
  "Check if connection can be upgraded to HTTP/2.
   Only cleartext HTTP/1.1 connections can upgrade via h2c."
  (and *h2c-upgrade-enabled*
       (protocol-connection-p connection)
       (eq (protocol-connection-protocol connection) :h1)
       (null (protocol-connection-tls connection))))

(defun h2c-upgrade-request-headers ()
  "Generate headers for h2c upgrade request.
   Returns alist of headers to include in upgrade request."
  (list
   (cons "Connection" "Upgrade, HTTP2-Settings")
   (cons "Upgrade" "h2c")
   ;; HTTP2-Settings is base64url-encoded SETTINGS frame payload
   ;; Empty settings uses defaults
   (cons "HTTP2-Settings" "")))

(defun string-to-bytes (string)
  "Convert an ASCII string to a byte vector."
  (map 'vector #'char-code string))

(defun read-response-line (socket)
  "Read a single CRLF-terminated line from SOCKET as a string.
   Reads byte-by-byte until CR LF is found. Returns the line
   without the trailing CRLF, or nil on read failure."
  (let ((buf (make-array 1 :element-type '(unsigned-byte 8)))
        (chars '()))
    (handler-case
        (loop
          (let ((n (net:tcp-read socket buf :start 0 :end 1)))
            (when (or (null n) (zerop n))
              (return nil))
            (let ((byte (aref buf 0)))
              (cond
                ;; CR - check for following LF
                ((= byte 13)
                 (let ((n2 (net:tcp-read socket buf :start 0 :end 1)))
                   (when (and n2 (> n2 0) (= (aref buf 0) 10))
                     ;; Complete CRLF found - return the line
                     (return (coerce (nreverse chars) 'string)))))
                (t
                 (push (code-char byte) chars))))))
      (error () nil))))

(defun parse-status-code (status-line)
  "Extract the numeric status code from an HTTP/1.1 status line.
   STATUS-LINE is expected to be in the form 'HTTP/1.1 NNN reason'.
   Returns the status code as an integer, or nil if parsing fails."
  (when (and status-line (>= (length status-line) 12))
    (let ((space-pos (position #\Space status-line)))
      (when space-pos
        (let ((code-str (subseq status-line (1+ space-pos)
                                (min (+ space-pos 4) (length status-line)))))
          (parse-integer code-str :junk-allowed t))))))

(defun consume-upgrade-response-headers (socket)
  "Read and discard all remaining HTTP/1.1 response headers from SOCKET.
   Reads lines until an empty line (the blank line after headers) is found."
  (loop
    (let ((line (read-response-line socket)))
      (when (or (null line) (string= line ""))
        (return)))))

(defun initiate-h2c-upgrade (connection)
  "Initiate HTTP/2 cleartext (h2c) upgrade on a connection.
   Sends an HTTP/1.1 request with Upgrade: h2c headers. If the server
   responds with 101 Switching Protocols, creates an HTTP/2 connection
   on the existing socket using epsilon.http.h2:make-http2-connection.

   Returns the updated protocol-connection with :h2 protocol on success,
   or nil if the server does not support h2c or an error occurs."
  (handler-case
      (let* ((socket (protocol-connection-socket connection))
             (host (protocol-connection-host connection))
             (port (protocol-connection-port connection))
             ;; Build the HTTP/1.1 upgrade request.
             ;; Each header line ends with CRLF; a final blank CRLF terminates headers.
             (crlf (format nil "~C~C" #\Return #\Newline))
             (host-header (if (= port 80)
                              host
                              (format nil "~A:~D" host port)))
             (request (concatenate 'string
                        "GET / HTTP/1.1" crlf
                        "Host: " host-header crlf
                        "Connection: Upgrade, HTTP2-Settings" crlf
                        "Upgrade: h2c" crlf
                        "HTTP2-Settings: " crlf
                        crlf)))
        ;; Send the upgrade request
        (net:tcp-write socket (string-to-bytes request))

        ;; Read the response status line
        (let* ((status-line (read-response-line socket))
               (status-code (parse-status-code status-line)))
          (cond
            ;; 101 Switching Protocols - server accepted the upgrade
            ((eql status-code 101)
             ;; Consume remaining response headers before switching protocols
             (consume-upgrade-response-headers socket)
             ;; Create HTTP/2 connection on the existing socket.
             ;; make-http2-connection with :client-p t sends the connection
             ;; preface and initial SETTINGS frame.
             (let ((h2-conn (h2:make-http2-connection socket :client-p t)))
               (declare (ignore h2-conn))
               ;; Update the protocol-connection to reflect HTTP/2
               (setf (protocol-connection-protocol connection) :h2)
               connection))

            ;; Any other status - server does not support h2c
            (t nil))))
    ;; On any error, return nil gracefully
    (error () nil)))

;;;; Protocol Capability Detection

(defun connection-supports-multiplexing-p (connection)
  "Check if connection supports request multiplexing.
   Only HTTP/2 connections support multiplexing."
  (and (protocol-connection-p connection)
       (eq (protocol-connection-protocol connection) :h2)))

(defun connection-available-streams (connection)
  "Return number of available streams on HTTP/2 connection.
   Returns nil for HTTP/1.1 connections."
  (when (connection-supports-multiplexing-p connection)
    (- (protocol-connection-max-streams connection)
       (protocol-connection-stream-count connection))))

(defun connection-can-send-request-p (connection)
  "Check if connection can accept a new request.
   HTTP/1.1: Always true if connection is alive
   HTTP/2: True if under max streams limit"
  (if (connection-supports-multiplexing-p connection)
      (> (connection-available-streams connection) 0)
      t))

;;;; HTTP/3 Support

(defun h3-available-p ()
  "Check if HTTP/3 support is available (libraries loaded).
   Returns T if ngtcp2 and nghttp3 libraries are available."
  (handler-case
      (progn
        ;; Try to load the HTTP/3 FFI module and check availability
        (let ((h3-ffi (find-package :epsilon.http3.ffi)))
          (when h3-ffi
            (let ((available-fn (find-symbol "HTTP3-AVAILABLE-P" h3-ffi)))
              (when available-fn
                (funcall available-fn))))))
    (error () nil)))

(defun h3-supported-p (host port)
  "Check if host:port supports HTTP/3 based on cached Alt-Svc.
   Returns the Alt-Svc entry if HTTP/3 is supported, NIL otherwise."
  (let ((key (cons host port)))
    (let ((entries (gethash key *alt-svc-cache*)))
      (find-if (lambda (entry)
                 (and (string= (alt-svc-entry-protocol entry) "h3")
                      (> (alt-svc-entry-expires-at entry) (get-universal-time))))
               entries))))

(defun parse-alt-svc (header-value)
  "Parse Alt-Svc header value (RFC 7838).
   Returns list of alt-svc-entry structures.

   Example header: h3=\":443\"; ma=86400, h2=\":443\"; ma=3600"
  (let ((entries '()))
    (dolist (part (split-string header-value #\,))
      (let ((trimmed (string-trim '(#\Space #\Tab) part)))
        (when (> (length trimmed) 0)
          (let ((entry (parse-single-alt-svc trimmed)))
            (when entry
              (push entry entries))))))
    (nreverse entries)))

(defun parse-single-alt-svc (value)
  "Parse a single Alt-Svc entry like 'h3=\":443\"; ma=86400'"
  (let ((eq-pos (position #\= value)))
    (when eq-pos
      (let ((protocol (subseq value 0 eq-pos))
            (rest (subseq value (1+ eq-pos))))
        ;; Parse authority (host:port in quotes)
        (let* ((quote-start (position #\" rest))
               (quote-end (when quote-start
                            (position #\" rest :start (1+ quote-start))))
               (authority (when (and quote-start quote-end)
                            (subseq rest (1+ quote-start) quote-end))))
          (when authority
            (multiple-value-bind (host port)
                (parse-authority authority)
              (let ((max-age 86400)
                    (persist nil))
                ;; Parse parameters
                (let ((params-start (position #\; rest :start (or quote-end 0))))
                  (when params-start
                    (dolist (param (split-string (subseq rest params-start) #\;))
                      (let* ((trimmed (string-trim '(#\Space #\Tab) param))
                             (param-eq (position #\= trimmed)))
                        (when param-eq
                          (let ((param-name (string-downcase (subseq trimmed 0 param-eq)))
                                (param-value (subseq trimmed (1+ param-eq))))
                            (cond
                              ((string= param-name "ma")
                               (setf max-age (parse-integer param-value :junk-allowed t)))
                              ((string= param-name "persist")
                               (setf persist (string= param-value "1"))))))))))
                (make-alt-svc-entry
                 :protocol protocol
                 :host host
                 :port (or port 443)
                 :max-age (or max-age 86400)
                 :persist persist
                 :expires-at (+ (get-universal-time) (or max-age 86400)))))))))))

(defun parse-authority (authority)
  "Parse authority string like ':443' or 'example.com:443'.
   Returns (values host port)."
  (let ((colon-pos (position #\: authority :from-end t)))
    (if colon-pos
        (let ((host (when (> colon-pos 0)
                      (subseq authority 0 colon-pos)))
              (port-str (subseq authority (1+ colon-pos))))
          (values host (parse-integer port-str :junk-allowed t)))
        (values authority nil))))

(defun split-string (string char)
  "Split string by character. Returns list of strings."
  (loop with start = 0
        for end = (position char string :start start)
        collect (subseq string start (or end (length string)))
        while end
        do (setf start (1+ end))))

(defun process-alt-svc-header (response origin-host origin-port)
  "Process Alt-Svc header from response and update cache.
   ORIGIN-HOST and ORIGIN-PORT identify the origin server."
  (let ((alt-svc-value (cond
                         ((listp response)
                          (cdr (assoc "alt-svc" response :test #'string-equal)))
                         ((hash-table-p response)
                          (gethash "alt-svc" response))
                         (t nil))))
    (when alt-svc-value
      (let ((entries (parse-alt-svc alt-svc-value)))
        (when entries
          (let ((key (cons origin-host origin-port)))
            ;; Update entries, inheriting origin host if not specified
            (dolist (entry entries)
              (unless (alt-svc-entry-host entry)
                (setf (alt-svc-entry-host entry) origin-host)))
            (setf (gethash key *alt-svc-cache*) entries)))))))

(defun clear-alt-svc-cache ()
  "Clear the Alt-Svc cache"
  (clrhash *alt-svc-cache*))

(defun select-best-protocol (host port &key preferred (h3-available (h3-available-p)))
  "Select the best available protocol for host:port.
   Considers HTTP/3 availability and Alt-Svc cache.

   Arguments:
     HOST - Target hostname
     PORT - Target port
     PREFERRED - Preferred protocol (:h3, :h2, or :h1)
     H3-AVAILABLE - Whether HTTP/3 libraries are available

   Returns: protocol keyword (:h3, :h2, or :h1)"
  (cond
    ;; Explicit preference
    ((and (eq preferred :h3) h3-available (h3-supported-p host port))
     :h3)
    ((eq preferred :h2)
     :h2)
    ((eq preferred :h1)
     :h1)
    ;; Auto-select based on availability
    ((and *prefer-http3* h3-available (h3-supported-p host port))
     :h3)
    (*prefer-http2*
     :h2)
    (t :h1)))

;;;; HTTP/3 Connection Support

(defun make-protocol-connection-for-quic (socket quic-conn host port)
  "Create a protocol-connection for HTTP/3 over QUIC.
   QUIC-CONN is the ngtcp2 connection handle."
  (make-protocol-connection
   :socket socket
   :tls nil  ; QUIC has built-in encryption, no separate TLS
   :protocol :h3
   :host host
   :port port
   :alpn-negotiated t
   :created-time (get-universal-time)
   :settings (list (cons :quic-connection quic-conn))))

;;;; Automatic Protocol Selection with Fallback

(defparameter *protocol-fallback-enabled* t
  "When true, automatically fall back to lower protocols on failure.")

(defparameter *h3-connect-timeout* 5
  "Timeout in seconds for HTTP/3 connection attempts before fallback.")

(defun try-h3-connection (host port &key timeout)
  "Try to establish an HTTP/3 connection.
   Returns the H3 connection object or NIL on failure.

   This is a quick probe - actual requests should use h3:http3-request."
  (when (and (h3-available-p) (h3-supported-p host port))
    (handler-case
        (let ((h3-pkg (find-package :epsilon.http3)))
          (when h3-pkg
            (let ((connect-fn (find-symbol "CONNECT" h3-pkg)))
              (when connect-fn
                ;; Try to connect with timeout
                (funcall connect-fn host port
                         :timeout (or timeout *h3-connect-timeout*))))))
      (error (e)
        (declare (ignore e))
        nil))))

(defun protocol-fallback-order (&key (prefer-h3 *prefer-http3*) (prefer-h2 *prefer-http2*))
  "Return list of protocols to try in order.

   Default order depends on preferences:
   - If prefer-h3: (:h3 :h2 :h1)
   - If prefer-h2: (:h2 :h1)
   - Otherwise: (:h1)"
  (cond
    (prefer-h3 '(:h3 :h2 :h1))
    (prefer-h2 '(:h2 :h1))
    (t '(:h1))))

(defun select-protocol-with-fallback (host port &key
                                                  ssl-p
                                                  preferred
                                                  (fallback *protocol-fallback-enabled*)
                                                  (h3-available (h3-available-p)))
  "Select protocol with automatic fallback on connection failure.

   Arguments:
     HOST - Target hostname
     PORT - Target port
     SSL-P - Whether to use TLS (for H1/H2)
     PREFERRED - Explicitly preferred protocol
     FALLBACK - Enable fallback to other protocols
     H3-AVAILABLE - Whether HTTP/3 libraries are loaded

   Returns: (values protocol fallback-used)
     PROTOCOL - Selected protocol keyword
     FALLBACK-USED - T if fell back from preferred protocol

   Protocol selection logic:
   1. If PREFERRED is specified and available, use it
   2. If H3 is preferred and supported for host:port, try H3
   3. Fall back through protocol list on failure"
  (declare (ignore ssl-p))  ; SSL-P reserved for future H2/H1 selection
  (let* ((h3-supported (and h3-available (h3-supported-p host port)))
         (initial-protocol (cond
                             (preferred preferred)
                             ((and *prefer-http3* h3-supported) :h3)
                             (*prefer-http2* :h2)
                             (t :h1))))

    (if (not fallback)
        ;; No fallback - use selected protocol
        (values initial-protocol nil)

        ;; With fallback - return initial choice
        ;; Actual fallback happens at request time
        (values initial-protocol nil))))

(defun make-request-with-fallback (url &key method headers body timeout protocol)
  "Make HTTP request with automatic protocol fallback.

   Tries the preferred protocol first, falling back on failure.

   Arguments:
     URL - Request URL
     METHOD - HTTP method (default: GET)
     HEADERS - Request headers
     BODY - Request body
     TIMEOUT - Request timeout
     PROTOCOL - Force specific protocol (nil for auto)

   Returns: (values response used-protocol)"
  (let* ((parsed (parse-url-for-protocol url))
         (protocols (if protocol
                        (list protocol)
                        (protocol-fallback-order)))
         (last-error nil))
    (declare (ignore parsed))  ; URL parsing for future host-specific fallback

    (dolist (proto protocols)
      (handler-case
          (let ((response (case proto
                            (:h3
                             (when (h3-available-p)
                               (let ((h3-pkg (find-package :epsilon.http3)))
                                 (when h3-pkg
                                   (let ((req-fn (find-symbol "HTTP3-REQUEST" h3-pkg)))
                                     (when req-fn
                                       (funcall req-fn url
                                                :method (or method "GET")
                                                :headers headers
                                                :body body
                                                :timeout timeout)))))))
                            ((:h2 :h1)
                             ;; Use standard client for H1/H2
                             (let ((client-pkg (find-package :epsilon.http.client)))
                               (when client-pkg
                                 (let ((req-fn (find-symbol "REQUEST" client-pkg)))
                                   (when req-fn
                                     (funcall req-fn url
                                              :method (string (or method "GET"))
                                              :headers headers
                                              :body body)))))))))
            (when response
              (return-from make-request-with-fallback
                (values response proto))))
        (error (e)
          (setf last-error e)
          ;; Continue to next protocol
          nil)))

    ;; All protocols failed
    (error "All protocols failed for ~A. Last error: ~A" url last-error)))

(defun parse-url-for-protocol (url)
  "Parse URL and extract components for protocol selection.
   Returns plist with :host, :port, :path, :ssl-p"
  (let* ((https-p (and (>= (length url) 8)
                       (string-equal "https://" (subseq url 0 8))))
         (http-p (and (not https-p)
                      (>= (length url) 7)
                      (string-equal "http://" (subseq url 0 7))))
         (scheme-len (cond (https-p 8) (http-p 7) (t 0)))
         (authority-start scheme-len)
         (path-start (or (position #\/ url :start authority-start) (length url)))
         (authority (subseq url authority-start path-start))
         (colon-pos (position #\: authority :from-end t))
         (host (if colon-pos
                   (subseq authority 0 colon-pos)
                   authority))
         (port (if colon-pos
                   (parse-integer (subseq authority (1+ colon-pos)) :junk-allowed t)
                   (if https-p 443 80)))
         (path (if (< path-start (length url))
                   (subseq url path-start)
                   "/")))
    (list :host host
          :port port
          :path path
          :ssl-p https-p)))
