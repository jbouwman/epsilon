;;;; epsilon.http.cache - HTTP Response Caching
;;;;
;;;; In-memory LRU HTTP cache with Cache-Control, ETag, and Last-Modified
;;;; support. Provides automatic cacheability checks and conditional request
;;;; header generation for cache revalidation.

(defpackage :epsilon.http.cache
  (:use :cl)
  (:import
   (epsilon.string str)
   (epsilon.sys.lock lock))
  (:export
   ;; Cache creation and management
   #:make-http-cache
   #:http-cache-p
   #:cache-get
   #:cache-put
   #:cache-invalidate
   #:cache-clear
   #:cache-stats
   #:cache-size
   ;; Cache entry accessors
   #:cache-entry-key
   #:cache-entry-response-body
   #:cache-entry-response-headers
   #:cache-entry-status-code
   #:cache-entry-etag
   #:cache-entry-last-modified
   #:cache-entry-expires-at
   #:cache-entry-created-at
   #:cache-entry-last-access-time
   #:cache-entry-access-count
   ;; Cache-Control parsing
   #:parse-cache-control
   #:cacheable-response-p
   ;; Conditional request helpers
   #:conditional-headers-for
   #:handle-304-response
   ;; Cache key helpers
   #:make-cache-key))

;;;; Cache Entry

(defstruct cache-entry
  "A single cached HTTP response."
  (key "" :type string)
  (response-body nil)
  (response-headers nil)
  (status-code 200 :type integer)
  (etag nil :type (or null string))
  (last-modified nil :type (or null string))
  (expires-at 0 :type integer)
  (created-at 0 :type integer)
  (last-access-time 0 :type integer)
  (access-count 0 :type integer)
  (byte-size 0 :type integer))

;;;; HTTP Cache

(defstruct (http-cache (:constructor %make-http-cache))
  "In-memory LRU HTTP cache.

   Entries are stored in a hash table keyed by cache key (typically
   METHOD:URL). Eviction uses access timestamps -- when the cache is
   full the least-recently-used entry is removed."
  (entries (make-hash-table :test 'equal) :type hash-table)
  (max-entries 1000 :type integer)
  (max-bytes (* 50 1024 1024) :type integer)  ; 50 MB
  (current-bytes 0 :type integer)
  (hits 0 :type integer)
  (misses 0 :type integer)
  (lock (lock:make-lock "http-cache") :type lock:lock))

(defun make-http-cache (&key (max-entries 1000) (max-bytes (* 50 1024 1024)))
  "Create a new HTTP cache.

   Arguments:
     MAX-ENTRIES - Maximum number of cached responses (default 1000)
     MAX-BYTES   - Maximum total size in bytes (default 50 MB)

   Returns: http-cache instance"
  (%make-http-cache :max-entries max-entries
                    :max-bytes max-bytes))

;;;; Internal Helpers

(defun estimate-entry-bytes (body headers)
  "Estimate the memory footprint of a cache entry in bytes."
  (let ((body-size (etypecase body
                     (null 0)
                     (string (length body))
                     ((vector (unsigned-byte 8)) (length body))))
        ;; Rough estimate for headers overhead
        (header-size (if headers 512 0)))
    (+ body-size header-size)))

(defun entry-fresh-p (entry)
  "Return T if the cache entry has not expired."
  (let ((expires (cache-entry-expires-at entry)))
    (or (zerop expires)
        (> expires (get-universal-time)))))

(defun find-lru-key (entries)
  "Find the key of the least-recently-used entry in the hash table."
  (let ((oldest-key nil)
        (oldest-time most-positive-fixnum))
    (maphash (lambda (key entry)
               (when (< (cache-entry-last-access-time entry) oldest-time)
                 (setf oldest-key key
                       oldest-time (cache-entry-last-access-time entry))))
             entries)
    oldest-key))

(defun evict-one (cache)
  "Evict the least-recently-used entry from the cache.
   Must be called while holding the cache lock."
  (let* ((entries (http-cache-entries cache))
         (lru-key (find-lru-key entries)))
    (when lru-key
      (let ((entry (gethash lru-key entries)))
        (when entry
          (decf (http-cache-current-bytes cache)
                (cache-entry-byte-size entry))
          (remhash lru-key entries))))))

(defun ensure-capacity (cache needed-bytes)
  "Evict LRU entries until there is room for NEEDED-BYTES.
   Must be called while holding the cache lock."
  ;; Evict until we are under entry count limit (leaving room for 1)
  (loop while (>= (hash-table-count (http-cache-entries cache))
                  (http-cache-max-entries cache))
        do (evict-one cache))
  ;; Evict until we are under byte limit
  (loop while (and (> (+ (http-cache-current-bytes cache) needed-bytes)
                      (http-cache-max-bytes cache))
                   (> (hash-table-count (http-cache-entries cache)) 0))
        do (evict-one cache)))

;;;; Public API -- Cache Operations

(defun cache-get (cache key)
  "Look up KEY in CACHE.

   Returns two values:
     1. The cache-entry (or NIL if not found / expired)
     2. T if there was a cache hit, NIL otherwise

   A hit is counted when a fresh entry is found. Stale entries are
   removed and counted as misses."
  (lock:with-lock ((http-cache-lock cache))
    (let ((entry (gethash key (http-cache-entries cache))))
      (cond
        ;; No entry at all
        ((null entry)
         (incf (http-cache-misses cache))
         (values nil nil))
        ;; Entry exists but is expired -- remove it
        ((not (entry-fresh-p entry))
         ;; Keep the entry around for conditional revalidation but
         ;; report a miss so the caller knows to revalidate.
         (incf (http-cache-misses cache))
         (values entry nil))
        ;; Fresh hit
        (t
         (incf (http-cache-hits cache))
         (incf (cache-entry-access-count entry))
         (setf (cache-entry-last-access-time entry) (get-universal-time))
         (values entry t))))))

(defun cache-put (cache key response-body response-headers status-code
                  &key etag last-modified max-age)
  "Store a response in the cache under KEY.

   Arguments:
     CACHE            - The http-cache instance
     KEY              - Cache key string (e.g. \"GET:https://example.com/api\")
     RESPONSE-BODY    - The response body (string or byte vector)
     RESPONSE-HEADERS - The response headers
     STATUS-CODE      - The HTTP status code
     ETAG             - ETag header value (optional)
     LAST-MODIFIED    - Last-Modified header value (optional)
     MAX-AGE          - Time-to-live in seconds (optional, derived from
                        Cache-Control if not provided)

   Returns: The newly created cache-entry"
  (let* ((now (get-universal-time))
         (byte-size (estimate-entry-bytes response-body response-headers))
         (expires-at (if max-age (+ now max-age) 0))
         (entry (make-cache-entry
                 :key key
                 :response-body response-body
                 :response-headers response-headers
                 :status-code status-code
                 :etag etag
                 :last-modified last-modified
                 :expires-at expires-at
                 :created-at now
                 :last-access-time now
                 :access-count 1
                 :byte-size byte-size)))
    (lock:with-lock ((http-cache-lock cache))
      ;; Remove existing entry for this key if present
      (let ((old (gethash key (http-cache-entries cache))))
        (when old
          (decf (http-cache-current-bytes cache)
                (cache-entry-byte-size old))))
      ;; Make room
      (ensure-capacity cache byte-size)
      ;; Insert
      (setf (gethash key (http-cache-entries cache)) entry)
      (incf (http-cache-current-bytes cache) byte-size))
    entry))

(defun cache-invalidate (cache key)
  "Remove the entry for KEY from CACHE.
   Returns T if an entry was removed, NIL otherwise."
  (lock:with-lock ((http-cache-lock cache))
    (let ((entry (gethash key (http-cache-entries cache))))
      (when entry
        (decf (http-cache-current-bytes cache)
              (cache-entry-byte-size entry))
        (remhash key (http-cache-entries cache))
        t))))

(defun cache-clear (cache)
  "Remove all entries from the cache."
  (lock:with-lock ((http-cache-lock cache))
    (clrhash (http-cache-entries cache))
    (setf (http-cache-current-bytes cache) 0
          (http-cache-hits cache) 0
          (http-cache-misses cache) 0))
  (values))

(defun cache-stats (cache)
  "Return cache statistics as a plist.

   Keys: :hits, :misses, :size (bytes), :entries (count)"
  (lock:with-lock ((http-cache-lock cache))
    (list :hits (http-cache-hits cache)
          :misses (http-cache-misses cache)
          :size (http-cache-current-bytes cache)
          :entries (hash-table-count (http-cache-entries cache)))))

(defun cache-size (cache)
  "Return the number of entries currently in the cache."
  (lock:with-lock ((http-cache-lock cache))
    (hash-table-count (http-cache-entries cache))))

;;;; Cache-Control Header Parsing

(defun trim-whitespace (s)
  "Remove leading and trailing whitespace from string S."
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun parse-cache-control (header-value)
  "Parse a Cache-Control header string into an alist.

   Each directive becomes a cons cell:
     - Directives with values: (keyword . value), where value is an
       integer for max-age/s-maxage or a string otherwise
     - Boolean directives: (keyword . T)

   Example:
     (parse-cache-control \"max-age=3600, no-cache\")
     => ((:MAX-AGE . 3600) (:NO-CACHE . T))

   Returns NIL for a NIL or empty input."
  (when (and header-value (not (string= header-value "")))
    (let ((directives nil))
      (dolist (part (split-on-comma header-value))
        (let ((trimmed (trim-whitespace part)))
          (unless (string= trimmed "")
            (let ((eq-pos (position #\= trimmed)))
              (if eq-pos
                  (let* ((name (trim-whitespace (subseq trimmed 0 eq-pos)))
                         (raw-value (trim-whitespace (subseq trimmed (1+ eq-pos))))
                         ;; Strip surrounding quotes
                         (value (if (and (>= (length raw-value) 2)
                                         (char= (char raw-value 0) #\")
                                         (char= (char raw-value (1- (length raw-value))) #\"))
                                    (subseq raw-value 1 (1- (length raw-value)))
                                    raw-value))
                         (keyword (directive-keyword name)))
                    (push (cons keyword
                                (if (member keyword '(:max-age :s-maxage))
                                    (parse-integer value :junk-allowed t)
                                    value))
                          directives))
                  (push (cons (directive-keyword trimmed) t)
                        directives))))))
      (nreverse directives))))

(defun split-on-comma (s)
  "Split string S on commas, returning a list of substrings."
  (let ((parts nil)
        (start 0)
        (len (length s)))
    (loop for i from 0 below len
          when (char= (char s i) #\,)
            do (push (subseq s start i) parts)
               (setf start (1+ i)))
    (push (subseq s start) parts)
    (nreverse parts)))

(defun directive-keyword (name)
  "Convert a Cache-Control directive name to a keyword symbol."
  (intern (string-upcase (trim-whitespace name)) :keyword))

;;;; Cacheability Check

(defparameter *cacheable-statuses* '(200 301 304 404 410)
  "HTTP status codes whose responses are cacheable by default.")

(defun cacheable-response-p (response-headers status-code)
  "Return T if the response with RESPONSE-HEADERS and STATUS-CODE
   may be stored in a private cache.

   A response is cacheable when:
     - The status code is in *cacheable-statuses*
     - Cache-Control does not contain no-store

   This implementation treats the cache as private, so the `private`
   directive does NOT prevent caching."
  (and (member status-code *cacheable-statuses*)
       (let* ((cc-header (find-header response-headers "cache-control"))
              (directives (when cc-header (parse-cache-control cc-header))))
         (not (assoc :no-store directives)))))

(defun find-header (headers name)
  "Look up a header value by NAME in HEADERS.
   HEADERS may be an alist, a hash-table, or an epsilon.map.
   Returns NIL if not found."
  (typecase headers
    ;; epsilon.map -- try calling map:get via funcall to avoid hard dep
    (t (handler-case
           (funcall (intern "GET" :epsilon.map) headers name)
         (error ()
           ;; Fall back to alist / hash-table handling
           (cond
             ((hash-table-p headers)
              (or (gethash name headers)
                  (gethash (string-downcase name) headers)))
             ((listp headers)
              (cdr (or (assoc name headers :test #'string-equal)
                       (assoc (string-downcase name) headers :test #'string-equal))))
             (t nil)))))))

;;;; Conditional Request Helpers

(defun conditional-headers-for (cache key)
  "Build conditional request headers from a cached entry.

   If the cache contains an entry for KEY (even if stale), return an
   alist of headers suitable for a conditional request:
     - (\"If-None-Match\" . etag)       when the entry has an ETag
     - (\"If-Modified-Since\" . date)   when the entry has Last-Modified

   Returns NIL if there is no entry to revalidate against."
  (lock:with-lock ((http-cache-lock cache))
    (let ((entry (gethash key (http-cache-entries cache))))
      (when entry
        (let ((headers nil))
          (when (cache-entry-etag entry)
            (push (cons "If-None-Match" (cache-entry-etag entry)) headers))
          (when (cache-entry-last-modified entry)
            (push (cons "If-Modified-Since" (cache-entry-last-modified entry)) headers))
          headers)))))

(defun handle-304-response (cache key new-response-headers)
  "Handle a 304 Not Modified response by refreshing the cached entry.

   Updates the stored headers and resets the expiry timer based on
   the new Cache-Control header (if present).

   Returns the cached response body, or NIL if no entry exists."
  (lock:with-lock ((http-cache-lock cache))
    (let ((entry (gethash key (http-cache-entries cache))))
      (when entry
        (let ((now (get-universal-time)))
          ;; Update headers if new ones were provided
          (when new-response-headers
            (setf (cache-entry-response-headers entry) new-response-headers))
          ;; Refresh expiry from new Cache-Control
          (let* ((cc-header (find-header new-response-headers "cache-control"))
                 (directives (when cc-header (parse-cache-control cc-header)))
                 (max-age (cdr (assoc :max-age directives))))
            (when max-age
              (setf (cache-entry-expires-at entry) (+ now max-age))))
          ;; Update access metadata
          (setf (cache-entry-last-access-time entry) now)
          (incf (cache-entry-access-count entry))
          ;; Update ETag / Last-Modified if the 304 carries new values
          (let ((new-etag (find-header new-response-headers "etag")))
            (when new-etag
              (setf (cache-entry-etag entry) new-etag)))
          (let ((new-lm (find-header new-response-headers "last-modified")))
            (when new-lm
              (setf (cache-entry-last-modified entry) new-lm))))
        (cache-entry-response-body entry)))))

;;;; Cache Key Helpers

(defun make-cache-key (method url)
  "Build a default cache key from METHOD and URL.

   Format: \"METHOD:URL\"
   Example: (make-cache-key \"GET\" \"https://example.com/api\")
            => \"GET:https://example.com/api\""
  (concatenate 'string (string-upcase (string method)) ":" url))
