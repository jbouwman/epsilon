(defpackage epsilon.web.session
  (:use cl)
  (:local-nicknames
   (str epsilon.string)
   (seq epsilon.sequence)
   (map epsilon.map)
   (uuid epsilon.uuid))
  (:export
   ;; Session object
   create-session
   session-id
   session-data
   session-created
   session-accessed
   
   ;; Session operations
   get-value
   set-value
   remove-value
   clear-session
   regenerate-id
   
   ;; Session store
   make-memory-store
   save-session
   load-session
   delete-session
   
   ;; Cookie handling
   parse-cookie-header
   parse-set-cookie-header
   make-cookie-string))

(in-package epsilon.web.session)

;;;; Session Object

(defstruct session
  id
  data
  created
  accessed)

(defun create-session ()
  "Create a new session with unique ID"
  (make-session
   :id (uuid:to-string (uuid:make-v4))
   :data (make-hash-table :test 'equal)
   :created (get-universal-time)
   :accessed (get-universal-time)))

(defun get-value (session key &optional default)
  "Get value from session"
  (gethash key (session-data session) default))

(defun set-value (session key value)
  "Set value in session"
  (setf (gethash key (session-data session)) value)
  (setf (session-accessed session) (get-universal-time))
  value)

(defun remove-value (session key)
  "Remove value from session"
  (remhash key (session-data session)))

(defun clear-session (session)
  "Clear all session data"
  (clrhash (session-data session))
  (setf (session-accessed session) (get-universal-time)))

(defun regenerate-id (session)
  "Generate new session ID (for security)"
  (setf (session-id session) (uuid:to-string (uuid:make-v4))))

;;;; Session Store Interface

(defclass session-store () ())

(defgeneric save-session (store session)
  (:documentation "Save session to store"))

(defgeneric load-session (store session-id)
  (:documentation "Load session from store"))

(defgeneric delete-session (store session-id)
  (:documentation "Delete session from store"))

;;;; Memory Store Implementation

(defclass memory-store (session-store)
  ((sessions :initform (make-hash-table :test 'equal) :accessor store-sessions)
   (ttl :initarg :ttl :initform (* 60 60 24) :accessor store-ttl) ; 24 hours default
   (lock :initform (epsilon.sys.lock:make-lock "session-store") :accessor store-lock)))

(defun make-memory-store (&key (ttl (* 60 60 24)))
  "Create in-memory session store"
  (make-instance 'memory-store :ttl ttl))

(defmethod save-session ((store memory-store) session)
  "Save session to memory store"
  (epsilon.sys.lock:with-lock ((store-lock store))
    (setf (gethash (session-id session) (store-sessions store))
          session)))

(defmethod load-session ((store memory-store) session-id)
  "Load session from memory store"
  (epsilon.sys.lock:with-lock ((store-lock store))
    (let ((session (gethash session-id (store-sessions store))))
      (when session
        ;; Check if expired
        (if (> (- (get-universal-time) (session-accessed session))
               (store-ttl store))
            (progn
              (remhash session-id (store-sessions store))
              nil)
            (progn
              (setf (session-accessed session) (get-universal-time))
              session))))))

(defmethod delete-session ((store memory-store) session-id)
  "Delete session from memory store"
  (epsilon.sys.lock:with-lock ((store-lock store))
    (remhash session-id (store-sessions store))))

;;;; Cookie Handling

(defun parse-cookie-header (cookie-string)
  "Parse Cookie header into hash table"
  (let ((cookies (make-hash-table :test 'equal)))
    (when cookie-string
      (let ((pairs-seq (str:split #\; cookie-string)))
        (dolist (cookie-pair (seq:realize pairs-seq))
          (let* ((trimmed (str:trim cookie-pair))
                 (eq-pos (position #\= trimmed)))
            (when eq-pos
              (let ((name (subseq trimmed 0 eq-pos))
                    (value (subseq trimmed (1+ eq-pos))))
                (setf (gethash name cookies) value)))))))
    cookies))

(defun parse-set-cookie-header (set-cookie-string)
  "Parse Set-Cookie header"
  (parse-cookie-header set-cookie-string))

(defun make-cookie-string (name value &key max-age path domain secure http-only same-site)
  "Create Set-Cookie header value"
  (with-output-to-string (out)
    (format out "~A=~A" name value)
    (when max-age
      (format out "; Max-Age=~A" max-age))
    (when path
      (format out "; Path=~A" path))
    (when domain
      (format out "; Domain=~A" domain))
    (when secure
      (format out "; Secure"))
    (when http-only
      (format out "; HttpOnly"))
    (when same-site
      (format out "; SameSite=~A" same-site))))