(defpackage #:epsilon.net.http.error
  (:use
   #:cl)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri))
  (:export
   #:http-request-failed
   
   ;; 4xx
   #:bad-request
   #:unauthorized
   #:payment-required
   #:forbidden
   #:not-found
   #:method-not-allowed
   #:not-acceptable
   #:proxy-authentication-required
   #:request-timeout
   #:conflict
   #:gone
   #:length-required
   #:precondition-failed
   #:payload-too-large
   #:uri-too-long
   #:unsupported-media-type
   #:range-not-satisfiable
   #:expectation-failed
   #:misdirected-request
   #:upgrade-required
   #:too-many-requests
   
   ;; 5xx
   #:internal-server-error
   #:not-implemented
   #:bad-gateway
   #:service-unavailable
   #:gateway-timeout
   #:http-version-not-supported
   
   ;; accessors
   :response-body
   :response-status
   :response-headers
   :request-uri
   :request-method

   ;; Proxy errors
   :socks5-proxy-request-failed))

(in-package :epsilon.net.http.error)

(define-condition http-request-failed (error)
  ((body :initarg :body
         :reader response-body)
   (status :initarg :status
           :reader response-status)
   (headers :initarg :headers
            :reader response-headers)
   (uri :initarg :uri
        :reader request-uri)
   (method :initarg :method
           :reader request-method))
  (:report (lambda (condition stream)
             (with-slots (uri status) condition
               (format stream "An HTTP request to ~S has failed (status=~D)."
                       (uri:render-uri uri)
                       status)))))

(defmacro define-request-failed-condition (name code)
  `(define-condition ,name (http-request-failed)
     ()
     (:report (lambda (condition stream)
                (with-slots (body uri) condition
                  (format stream ,(format nil "An HTTP request to ~~S returned ~D ~A.~~2%~~A"
                                          code
                                          (substitute #\Space #\- (string-downcase name)))
                          (uri:render-uri uri)
                          body))))))


(defvar *request-failed-error* (make-hash-table :test 'eql))

#.`(progn
     ,@(loop for (name . code) in '(;; 4xx (Client Errors)
                                    (bad-request                   . 400)
                                    (unauthorized                  . 401)
                                    (payment-required              . 402)
                                    (forbidden                     . 403)
                                    (not-found                     . 404)
                                    (method-not-allowed            . 405)
                                    (not-acceptable                . 406)
                                    (proxy-authentication-required . 407)
                                    (request-timeout               . 408)
                                    (conflict                      . 409)
                                    (gone                          . 410)
                                    (length-required               . 411)
                                    (precondition-failed           . 412)
                                    (payload-too-large             . 413)
                                    (uri-too-long                  . 414)
                                    (unsupported-media-type        . 415)
                                    (range-not-satisfiable         . 416)
                                    (expectation-failed            . 417)
                                    (misdirected-request           . 421)
                                    (upgrade-required              . 426)
                                    (too-many-requests             . 429)

                                    ;; 5xx (Server Errors)
                                    (internal-server-error      . 500)
                                    (not-implemented            . 501)
                                    (bad-gateway                . 502)
                                    (service-unavailable        . 503)
                                    (gateway-timeout            . 504)
                                    (http-version-not-supported . 505))
             collect `(define-request-failed-condition ,name ,code)
             collect `(setf (gethash ,code *request-failed-error*)
                            ',(intern (format nil "~A-~A" :http-request name)))))

(defun http-request-failed (status &key body headers uri method)
  (cerror
   "Ignore and continue"
   (gethash status *request-failed-error* 'http-request-failed)
   :body body
   :status status
   :headers headers
   :uri uri
   :method method))

(define-condition socks5-proxy-request-failed (http-request-failed)
  ((reason :initarg :reason))
  (:report (lambda (condition stream)
             (with-slots (uri reason) condition
               (format stream "An HTTP request to ~S via SOCKS5 has failed (reason=~S)."
                       (uri:render-uri uri)
                       reason)))))
