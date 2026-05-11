;;;; epsilon.http.pool.unified - Unified HTTP Connection Pool
;;;;
;;;; Multi-protocol connection pool supporting both HTTP/1.1 and HTTP/2.
;;;; HTTP/1.1: Multiple connections per host (configurable limit)
;;;; HTTP/2: Single multiplexed connection per host with stream tracking

(defpackage :epsilon.http.pool.unified
  (:use :cl)
  (:import
   (epsilon.http.protocol.negotiator negotiator)
   (epsilon.crypto crypto)
   (epsilon.net net)
   (epsilon.map map)
   (epsilon.sys.lock lock))
  (:export
   ;; Pool creation
   make-unified-pool
   unified-pool
   unified-pool-p

   ;; Pool configuration
   *default-h1-pool-size*
   *default-h2-max-streams*
   *default-idle-timeout*
   *default-connect-timeout*

   ;; Connection management
   acquire-connection
   release-connection
   close-connection

   ;; Pool operations
   pool-stats
   pool-cleanup
   pool-clear
   pool-shutdown

   ;; Stats structure
   unified-pool-stats
   unified-pool-stats-h1-connections
   unified-pool-stats-h2-connections
   unified-pool-stats-h1-active
   unified-pool-stats-h2-active
   unified-pool-stats-h2-streams
   unified-pool-stats-pool-hits
   unified-pool-stats-pool-misses))

;;;; Configuration

(defparameter *default-h1-pool-size* 6
  "Default max HTTP/1.1 connections per host (matches browser default)")

(defparameter *default-h2-max-streams* 100
  "Default max concurrent HTTP/2 streams per connection")

(defparameter *default-idle-timeout* 300
  "Default idle timeout in seconds before closing connection")

(defparameter *default-connect-timeout* 30
  "Default connection establishment timeout in seconds")

(defparameter *cleanup-interval* 60
  "Interval between pool cleanup sweeps in seconds")

;;;; Pool Statistics

(defstruct unified-pool-stats
  "Statistics for unified connection pool"
  (h1-connections 0 :type integer)    ; Total H1 connections in pool
  (h2-connections 0 :type integer)    ; Total H2 connections in pool
  (h1-active 0 :type integer)         ; Active H1 connections
  (h2-active 0 :type integer)         ; Active H2 connections
  (h2-streams 0 :type integer)        ; Active H2 streams across all connections
  (connections-created 0 :type integer)
  (connections-reused 0 :type integer)
  (connections-closed 0 :type integer)
  (pool-hits 0 :type integer)
  (pool-misses 0 :type integer)
  (h2-upgrades 0 :type integer))

;;;; Pooled Connection Entry

(defstruct pool-entry
  "Entry in the connection pool"
  (connection nil :type t)            ; protocol-connection
  (in-use nil :type boolean)          ; Whether currently in use
  (last-used 0 :type integer)         ; Universal time of last use
  (request-count 0 :type integer)     ; Number of requests on this connection
  (h2-streams nil :type list))        ; For H2: list of active stream IDs

;;;; Unified Pool Structure

(defstruct unified-pool
  "Multi-protocol HTTP connection pool"
  ;; Pool storage: host:port -> pool-entry list
  (h1-pools (make-hash-table :test 'equal) :type hash-table)
  (h2-connections (make-hash-table :test 'equal) :type hash-table)

  ;; Configuration
  (h1-max-per-host *default-h1-pool-size* :type integer)
  (h2-max-streams *default-h2-max-streams* :type integer)
  (idle-timeout *default-idle-timeout* :type integer)
  (connect-timeout *default-connect-timeout* :type integer)

  ;; Protocol preference
  (prefer-h2 t :type boolean)
  (h2-enabled t :type boolean)        ; Whether to use HTTP/2 at all

  ;; Synchronization
  (lock (lock:make-lock "unified-pool") :type lock:lock)

  ;; Statistics
  (stats (make-unified-pool-stats) :type unified-pool-stats)

  ;; Cleanup
  (cleanup-thread nil :type t)
  (shutdown-p nil :type boolean))

;;;; Pool Key Generation

(defun pool-key (host port &optional ssl-p)
  "Generate pool key for host:port combination"
  (format nil "~A~A:~D" (if ssl-p "https://" "http://") host port))

;;;; HTTP/1.1 Pool Operations

(defun get-h1-connection (pool host port ssl-p)
  "Get an HTTP/1.1 connection from the pool or create new.
   Returns pool-entry or nil if pool is exhausted and can't create."
  (let* ((key (pool-key host port ssl-p))
         (entries (gethash key (unified-pool-h1-pools pool))))

    ;; Try to find an idle connection
    (dolist (entry entries)
      (when (and (not (pool-entry-in-use entry))
                 (connection-alive-p (pool-entry-connection entry)))
        (setf (pool-entry-in-use entry) t
              (pool-entry-last-used entry) (get-universal-time))
        (incf (unified-pool-stats-pool-hits (unified-pool-stats pool)))
        (incf (unified-pool-stats-connections-reused (unified-pool-stats pool)))
        (incf (unified-pool-stats-h1-active (unified-pool-stats pool)))
        (return-from get-h1-connection entry)))

    ;; No idle connection - check if we can create new
    (let ((active-count (count-if #'pool-entry-in-use entries)))
      (when (< active-count (unified-pool-h1-max-per-host pool))
        ;; Create new connection
        (incf (unified-pool-stats-pool-misses (unified-pool-stats pool)))
        (let ((conn (create-h1-connection host port ssl-p
                                          (unified-pool-connect-timeout pool))))
          (when conn
            (let ((entry (make-pool-entry
                          :connection conn
                          :in-use t
                          :last-used (get-universal-time))))
              (setf (gethash key (unified-pool-h1-pools pool))
                    (cons entry entries))
              (incf (unified-pool-stats-h1-connections (unified-pool-stats pool)))
              (incf (unified-pool-stats-h1-active (unified-pool-stats pool)))
              (incf (unified-pool-stats-connections-created (unified-pool-stats pool)))
              entry)))))))

(defun release-h1-connection (pool entry)
  "Release an HTTP/1.1 connection back to the pool"
  (setf (pool-entry-in-use entry) nil
        (pool-entry-last-used entry) (get-universal-time))
  (incf (pool-entry-request-count entry))
  (decf (unified-pool-stats-h1-active (unified-pool-stats pool))))

;;;; HTTP/2 Pool Operations

(defun get-h2-connection (pool host port ssl-p)
  "Get an HTTP/2 connection from the pool or create new.
   HTTP/2 uses a single multiplexed connection per host."
  (let* ((key (pool-key host port ssl-p))
         (entry (gethash key (unified-pool-h2-connections pool))))

    (cond
      ;; Existing connection with available streams
      ((and entry
            (connection-alive-p (pool-entry-connection entry))
            (< (length (pool-entry-h2-streams entry))
               (unified-pool-h2-max-streams pool)))
       (incf (unified-pool-stats-pool-hits (unified-pool-stats pool)))
       (incf (unified-pool-stats-connections-reused (unified-pool-stats pool)))
       entry)

      ;; Need new connection
      (t
       (incf (unified-pool-stats-pool-misses (unified-pool-stats pool)))
       ;; Close old connection if exists
       (when entry
         (close-pool-entry entry)
         (incf (unified-pool-stats-connections-closed (unified-pool-stats pool))))

       ;; Create new HTTP/2 connection
       (let ((conn (create-h2-connection host port ssl-p
                                         (unified-pool-connect-timeout pool))))
         (when conn
           (let ((new-entry (make-pool-entry
                             :connection conn
                             :in-use nil  ; H2 tracks streams, not whole connection
                             :last-used (get-universal-time)
                             :h2-streams nil)))
             (setf (gethash key (unified-pool-h2-connections pool)) new-entry)
             (incf (unified-pool-stats-h2-connections (unified-pool-stats pool)))
             (incf (unified-pool-stats-connections-created (unified-pool-stats pool)))
             new-entry)))))))

(defun acquire-h2-stream (pool entry)
  "Acquire a stream on an HTTP/2 connection.
   Returns stream-id or nil if max streams reached."
  (when (< (length (pool-entry-h2-streams entry))
           (unified-pool-h2-max-streams pool))
    (let ((stream-id (next-stream-id entry)))
      (push stream-id (pool-entry-h2-streams entry))
      (setf (pool-entry-last-used entry) (get-universal-time))
      (incf (unified-pool-stats-h2-streams (unified-pool-stats pool)))
      (incf (unified-pool-stats-h2-active (unified-pool-stats pool)))
      stream-id)))

(defun release-h2-stream (pool entry stream-id)
  "Release a stream on an HTTP/2 connection"
  (setf (pool-entry-h2-streams entry)
        (remove stream-id (pool-entry-h2-streams entry)))
  (setf (pool-entry-last-used entry) (get-universal-time))
  (incf (pool-entry-request-count entry))
  (decf (unified-pool-stats-h2-streams (unified-pool-stats pool)))
  (decf (unified-pool-stats-h2-active (unified-pool-stats pool))))

(defun next-stream-id (entry)
  "Generate next client stream ID for HTTP/2.
   Client-initiated streams use odd IDs starting from 1."
  (let ((existing (pool-entry-h2-streams entry)))
    (if existing
        (+ 2 (apply #'max existing))  ; Next odd number
        1)))                          ; First stream

;;;; Unified Connection Interface

(defun acquire-connection (pool host port &key ssl-p prefer-h2 force-protocol)
  "Acquire a connection from the pool.

   Returns: (values connection stream-id-or-nil)
     CONNECTION - The connection to use
     STREAM-ID - For HTTP/2, the stream ID to use (nil for HTTP/1.1)

   Protocol selection:
     1. FORCE-PROTOCOL overrides all
     2. PREFER-H2 (default from pool) chooses HTTP/2 if available
     3. SSL connections use ALPN to negotiate
     4. Non-SSL connections use HTTP/1.1 (h2c upgrade disabled by default)"
  (lock:with-lock ((unified-pool-lock pool))
    (let* ((use-h2 (and (unified-pool-h2-enabled pool)
                        ssl-p  ; Only use H2 with TLS by default
                        (or (eq force-protocol :h2)
                            (and (null force-protocol)
                                 (or prefer-h2
                                     (unified-pool-prefer-h2 pool))))))
           (protocol (cond
                       (force-protocol force-protocol)
                       (use-h2 :h2)
                       (t :h1))))

      (case protocol
        (:h2
         (let ((entry (get-h2-connection pool host port ssl-p)))
           (when entry
             (let ((stream-id (acquire-h2-stream pool entry)))
               (when stream-id
                 (values (pool-entry-connection entry) stream-id))))))

        (:h1
         (let ((entry (get-h1-connection pool host port ssl-p)))
           (when entry
             (values (pool-entry-connection entry) nil))))))))

(defun release-connection (pool connection &key stream-id error-p)
  "Release a connection back to the pool.

   Arguments:
     POOL - The unified pool
     CONNECTION - The connection to release
     STREAM-ID - For HTTP/2, the stream ID being released
     ERROR-P - If true, connection had an error and should be closed"
  (lock:with-lock ((unified-pool-lock pool))
    (let ((protocol (negotiator:negotiated-protocol connection)))
      (case protocol
        (:h2
         (let ((entry (find-h2-entry pool connection)))
           (when entry
             (if error-p
                 (progn
                   (close-pool-entry entry)
                   (remove-h2-entry pool connection))
                 (when stream-id
                   (release-h2-stream pool entry stream-id))))))

        (:h1
         (let ((entry (find-h1-entry pool connection)))
           (when entry
             (if error-p
                 (progn
                   (close-pool-entry entry)
                   (remove-h1-entry pool entry))
                 (release-h1-connection pool entry)))))))))

;;;; Helper Functions

(defun find-h1-entry (pool connection)
  "Find pool entry for an HTTP/1.1 connection"
  (maphash (lambda (key entries)
             (declare (ignore key))
             (dolist (entry entries)
               (when (eq (pool-entry-connection entry) connection)
                 (return-from find-h1-entry entry))))
           (unified-pool-h1-pools pool))
  nil)

(defun find-h2-entry (pool connection)
  "Find pool entry for an HTTP/2 connection"
  (maphash (lambda (key entry)
             (declare (ignore key))
             (when (eq (pool-entry-connection entry) connection)
               (return-from find-h2-entry entry)))
           (unified-pool-h2-connections pool))
  nil)

(defun remove-h1-entry (pool entry)
  "Remove an HTTP/1.1 entry from the pool"
  (maphash (lambda (key entries)
             (let ((remaining (remove entry entries)))
               (if remaining
                   (setf (gethash key (unified-pool-h1-pools pool)) remaining)
                   (remhash key (unified-pool-h1-pools pool)))))
           (unified-pool-h1-pools pool))
  (decf (unified-pool-stats-h1-connections (unified-pool-stats pool))))

(defun remove-h2-entry (pool connection)
  "Remove an HTTP/2 entry from the pool"
  (block remove-entry
    (maphash (lambda (key entry)
               (when (eq (pool-entry-connection entry) connection)
                 (remhash key (unified-pool-h2-connections pool))
                 (return-from remove-entry)))
             (unified-pool-h2-connections pool)))
  (decf (unified-pool-stats-h2-connections (unified-pool-stats pool))))

(defun close-pool-entry (entry)
  "Close the connection in a pool entry"
  (ignore-errors
   (let ((conn (pool-entry-connection entry)))
     (when (negotiator:protocol-connection-p conn)
       (when (negotiator:protocol-connection-tls conn)
         (crypto:tls-close (negotiator:protocol-connection-tls conn)))
       (when (negotiator:protocol-connection-socket conn)
         (net:tcp-shutdown (negotiator:protocol-connection-socket conn)))))))

(defun connection-alive-p (connection)
  "Check if a connection is still alive"
  (and connection
       (negotiator:protocol-connection-p connection)
       (let ((socket (negotiator:protocol-connection-socket connection)))
         (and socket
              ;; Simple alive check - more sophisticated probing could be added
              t))))

;;;; Connection Creation (Stubs - actual implementation uses crypto/net modules)

(defun create-h1-connection (host port ssl-p timeout)
  "Create a new HTTP/1.1 connection"
  (declare (ignore timeout))
  (handler-case
      (let ((socket (net:tcp-connect (format nil "~A:~D" host port))))
        (if ssl-p
            (let* ((ctx (crypto:make-client-context))
                   (tls (crypto:tls-connect socket ctx
                                            :hostname host
                                            :alpn-protocols (list negotiator:+protocol-h1+))))
              (negotiator:make-protocol-connection-for-tls socket tls host port))
            (negotiator:make-protocol-connection-for-cleartext socket host port)))
    (error (e)
      (declare (ignore e))
      nil)))

(defun create-h2-connection (host port ssl-p timeout)
  "Create a new HTTP/2 connection"
  (declare (ignore timeout))
  (handler-case
      (let ((socket (net:tcp-connect (format nil "~A:~D" host port))))
        (if ssl-p
            (let* ((ctx (crypto:make-client-context))
                   (tls (crypto:tls-connect socket ctx
                                            :hostname host
                                            :alpn-protocols (list negotiator:+protocol-h2+
                                                                  negotiator:+protocol-h1+))))
              (let ((conn (negotiator:make-protocol-connection-for-tls socket tls host port)))
                ;; Verify HTTP/2 was negotiated
                (if (eq (negotiator:protocol-connection-protocol conn) :h2)
                    conn
                    (progn
                      ;; Fallback: return H1 connection
                      conn))))
            ;; Non-TLS HTTP/2 (h2c) not supported in this path
            nil))
    (error (e)
      (declare (ignore e))
      nil)))

;;;; Pool Cleanup

(defun pool-cleanup (pool)
  "Remove expired idle connections from the pool"
  (lock:with-lock ((unified-pool-lock pool))
    (let ((now (get-universal-time))
          (timeout (unified-pool-idle-timeout pool))
          (closed 0))

      ;; Cleanup HTTP/1.1 pools
      (maphash (lambda (key entries)
                 (let ((remaining
                         (remove-if
                          (lambda (entry)
                            (let ((idle-time (- now (pool-entry-last-used entry))))
                              (when (and (not (pool-entry-in-use entry))
                                         (> idle-time timeout))
                                (close-pool-entry entry)
                                (incf closed)
                                t)))
                          entries)))
                   (if remaining
                       (setf (gethash key (unified-pool-h1-pools pool)) remaining)
                       (remhash key (unified-pool-h1-pools pool)))))
               (unified-pool-h1-pools pool))

      ;; Cleanup HTTP/2 connections (only if no active streams)
      (maphash (lambda (key entry)
                 (let ((idle-time (- now (pool-entry-last-used entry))))
                   (when (and (null (pool-entry-h2-streams entry))
                              (> idle-time timeout))
                     (close-pool-entry entry)
                     (remhash key (unified-pool-h2-connections pool))
                     (incf closed))))
               (unified-pool-h2-connections pool))

      (incf (unified-pool-stats-connections-closed (unified-pool-stats pool)) closed)
      closed)))

(defun pool-clear (pool)
  "Clear all connections from the pool"
  (lock:with-lock ((unified-pool-lock pool))
    ;; Close all HTTP/1.1 connections
    (maphash (lambda (key entries)
               (declare (ignore key))
               (dolist (entry entries)
                 (close-pool-entry entry)))
             (unified-pool-h1-pools pool))
    (clrhash (unified-pool-h1-pools pool))

    ;; Close all HTTP/2 connections
    (maphash (lambda (key entry)
               (declare (ignore key))
               (close-pool-entry entry))
             (unified-pool-h2-connections pool))
    (clrhash (unified-pool-h2-connections pool))

    ;; Reset stats
    (setf (unified-pool-stats pool) (make-unified-pool-stats))))

(defun pool-shutdown (pool)
  "Shutdown the pool and cleanup thread"
  (setf (unified-pool-shutdown-p pool) t)
  (pool-clear pool))

;;;; Pool Statistics

(defun pool-stats (pool)
  "Return current pool statistics"
  (lock:with-lock ((unified-pool-lock pool))
    (copy-structure (unified-pool-stats pool))))
