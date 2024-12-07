(defpackage #:epsilon.lib.uri
  (:use
   #:cl
   #:sb-cltl2
   #:epsilon.lib.binding
   #:epsilon.lib.char
   #:epsilon.lib.collect
   #:epsilon.lib.control
   #:epsilon.lib.hash
   #:epsilon.lib.list
   #:epsilon.lib.sequence
   #:epsilon.lib.symbol
   #:epsilon.lib.type)
  (:shadow
   #:merge)
  (:export
   #:uri
   #:input-stream
   #:make-uri
   #:make-basic-uri
   #:uri-p
   #:scheme
   #:userinfo
   #:host
   #:port
   #:path
   #:query
   #:fragment
   #:authority
   #:url-encode
   #:url-encode-params
   #:urn
   #:make-urn
   #:render-uri
   #:merge
   #:urn-p
   #:urn-nid
   #:urn-nss))

(in-package #:epsilon.lib.uri)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defvar +default-ports+
    (plist-hash-table
     '("ssh" 22
       "http" 80
       "https" 443
       "ws" 80
       "wss" 443)
     :test 'equal))

  (defun scheme-default-port (scheme)
    (gethash scheme +default-ports+))

  )

(define-condition parsing-end-unexpectedly (simple-error)
  ((state :initarg :state
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parsing ended unexpectedly~:[~;~:* at ~A~]"
                     (slot-value condition 'state)))))

(define-condition no-next-state (simple-error) ())

(defmacro with-string-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let ((,elem #\Nul))
     (declare (type character ,elem))
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro with-byte-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let ((,elem 0))
     (declare (type u8 ,elem))
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let (,elem)
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro %with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  (with-gensyms (g-end no-next-state last key-fn)
    (let ((eof-exists nil))
      `(let (,@(and key `((,key-fn ,key)))
             (,p ,start)
             (,g-end (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                       (or ,end (length ,seq)))))
         (declare (ignorable ,p ,g-end))
         ,@(loop for (exp . rest) on body
                 while (and (listp exp) (eq (car exp) 'declare))
                 collect exp
                 do (setq body rest))
         (macrolet ((goto (tag &optional (amount 1))
                      `(progn
                         (incf ,',p ,amount)
                         ,@(if (eql amount 0)
                               ()
                               `((when (= ,',p ,',g-end)
                                   (go :eof))
                                 (setq ,',elem
                                       ,',(if key
                                              `(if ,key-fn
                                                   (funcall ,key-fn (aref ,seq ,p))
                                                   (aref ,seq ,p))
                                              `(aref ,seq ,p)))))
                         (go ,tag))))
           (tagbody
              (when (= ,p ,g-end)
                (go :eof))
              (setq ,elem ,@(if key
                                `((if ,key-fn
                                      (funcall ,key-fn (aref ,seq ,p))
                                      (aref ,seq ,p)))
                                `((aref ,seq ,p))))
              ,@(loop for (tagpart . rest) on body
                      for (tag . part) = tagpart
                      if (eq tag :eof)
                        append (progn
                                 (setf eof-exists t)
                                 `(,@tagpart
                                   (go ,last)))
                      else
                        append
                        (list tag
                              `(macrolet ((redo (&optional (amount 1))
                                            `(goto ,',tag ,amount))
                                          (gonext (&optional (amount 1))
                                            `(goto ,',(or (caar rest) no-next-state)
                                                   ,amount)))
                                 ,@part
                                 (error 'parsing-end-unexpectedly :state ',tag))))

              ,no-next-state
              (error 'no-next-state)

              ,@(if eof-exists
                    ()
                    '(:eof))

              ,last))))))

(define-condition uri-error (error) ())

(define-condition uri-malformed-string (uri-error)
  ((data :initarg :data)
   (position :initarg :position))
  (:report (lambda (condition stream)
             (with-slots (data position) condition
               (format stream "URI ~S contains an illegal character ~S at position ~S."
                       data (aref data position) position)))))

(define-condition uri-invalid-port (uri-malformed-string)
  ()
  (:report (lambda (condition stream)
             (with-slots (data position) condition
               (format stream "URI ~S contains an illegal character ~S at position ~S."
                       data (aref data position) position)))))

(define-condition url-decoding-error (uri-error) ())

(define-condition uri-malformed-urlencoded-string (uri-error) ())

(declaim (type (simple-array fixnum (128)) +uri-char+))
(define-constant +uri-char+
    (let ((uri-char (make-array 128 :element-type 'fixnum :initial-element 0)))
      (dotimes (i 128 uri-char)
        (let ((char (code-char i)))
          (when (or (alphanumericp char)
                    (char= char #\%)
                    (char= char #\:)
                    (char= char #\@)
                    (char= char #\-)
                    (char= char #\.)
                    (char= char #\_)
                    (char= char #\~)
                    (char= char #\!)
                    (char= char #\$)
                    (char= char #\&)
                    (char= char #\')
                    (char= char #\()
                    (char= char #\))
                    (char= char #\*)
                    (char= char #\+)
                    (char= char #\,)
                    (char= char #\;)
                    (char= char #\=))
            (setf (aref uri-char i) 1))))))

(define-compiler-macro parse-uri (&whole form &environment env data &key start end)
  (declare (ignore start end))
  (let ((type (cond
                ((constantp data) (type-of data))
                ((symbolp data) (cdr (assoc 'type (nth-value 2 (variable-information data env))))))))
    (cond
      ((null type) form)
      ((subtypep type 'simple-string) `(parse-uri-string ,@(cdr form)))
      ((subtypep type '->u8) `(parse-uri-byte-vector ,@(cdr form)))
      (t form))))

(defun parse-uri-string (data &key (start 0) end)
  (declare (type simple-string data))
  (let (scheme userinfo host port path query fragment
               (parse-start start)
               (parse-end (or end (length data))))
    (declare (type fixnum parse-start parse-end))
    (block nil
      (flet ((parse-from-path (data start)
               (declare (type simple-string data)
                        (type fixnum start))
               (multiple-value-bind (data start end)
                   (parse-path-string data :start start :end parse-end)
                 (declare (type simple-string data)
                          (type fixnum start end))
                 (unless (= start end)
                   (setq path (subseq data start end)))
                 ;; Pitfall: There may be no query but a fragment that has a '?', e.g.
                 ;; https://example.org/#/?b
                 (let ((maybe-query-start (or (nth-value 1 (parse-query-string data :start end :end parse-end))
                                              (1+ parse-end)))
                       (maybe-fragment-start (or (nth-value 1 (parse-fragment-string data :start end :end parse-end))
                                                 (1+ parse-end))))
                   (flet ((parse-fragment (path-end)
                            (multiple-value-bind (data start end)
                                (parse-fragment-string data :start (or path-end end) :end parse-end)
                              (when data
                                (setq fragment (subseq (the string data) (the fixnum start) (the fixnum end)))))))
                     (if (< (the fixnum maybe-query-start) (the fixnum maybe-fragment-start))
                         (multiple-value-bind (parsed-data path-start path-end)
                             (parse-query-string data :start end :end parse-end)
                           (when parsed-data
                             (setq query (subseq (the string parsed-data) (the fixnum path-start) (the fixnum path-end))))
                           (parse-fragment path-end))
                         (parse-fragment end)))))))
        (multiple-value-bind (parsed-data start end got-scheme)
            (parse-scheme-string data :start parse-start :end parse-end)
          (if parsed-data
              (locally (declare (type fixnum start end))
                (setq scheme
                      (or got-scheme
                          (string-downcase (subseq data start end))))
                (incf end))             ;eat the trailing #\:
              (setq scheme nil
                    end parse-start))
          (locally (declare (type fixnum end))
            (unless (= end parse-end)
              (multiple-value-bind (parsed-data userinfo-start userinfo-end
                                    host-start host-end port-start port-end)
                  (parse-authority-string data :start end :end parse-end)
                (when parsed-data
                  (locally (declare (type fixnum host-start host-end))
                    (when userinfo-start
                      (setq userinfo (subseq (the string data) (the fixnum userinfo-start) (the fixnum userinfo-end))))
                    (unless (= host-start host-end)
                      (setq host (subseq data host-start host-end)))
                    (cond
                      (port-start
                       (locally (declare (type fixnum port-start port-end))
                         (unless (= port-start port-end)
                           (handler-case
                               (setq port
                                     (parse-integer data :start (the fixnum port-start) :end (the fixnum port-end)))
                             (error ()
                               (error 'uri-invalid-port
                                      :data data :position port-start))))))
                      (scheme
                       (setq port (scheme-default-port scheme))))))
                (locally (declare (optimize (safety 0)))
                  (parse-from-path data (or port-end host-end end)))))))))
    (values scheme userinfo host port path query fragment)))

(defun parse-uri-byte-vector (data &key (start 0) end)
  (declare (type ->u8 data))
  (let (scheme userinfo host port path query fragment
               (parse-start start)
               (parse-end (or end (length data))))
    (declare (type fixnum parse-start parse-end))
    (flet ((subseq* (data &optional (start 0) end)
             (declare (type ->u8 data))
             (values (u8-to-string data :start start :end end)))
           (parse-integer-from-bv (data &key (start 0) end)
             (declare (type fixnum start end))
             (when (= start end)
               (return-from parse-integer-from-bv nil))
             (do ((i start (1+ i))
                  (res 0))
                 ((= i end) res)
               (declare (type fixnum i res))
               (let ((code (aref data i)))
                 (declare (type fixnum code)
                          #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                 (unless (<= #.(char-code #\0) code #.(char-code #\9))
                   (error 'uri-invalid-port
                          :data data :position i))

                 (setq res (+ (* res 10)
                              (- code #.(char-code #\0))))))))
      (block nil
        (flet ((parse-from-path (data start)
                 (declare (type ->u8 data)
                          (type fixnum start))
                 (multiple-value-bind (data start end)
                     (parse-path-byte-vector data :start start :end parse-end)
                   (declare (type fixnum start end))
                   (unless (= start end)
                     (setq path (subseq* data start end)))
                   (multiple-value-bind (parsed-data path-start path-end)
                       (parse-query-byte-vector data :start end :end parse-end)
                     (when parsed-data
                       (setq query (subseq* parsed-data (the fixnum path-start) (the fixnum path-end))))
                     (multiple-value-bind (data start end)
                         (parse-fragment-byte-vector data :start (or path-end end) :end parse-end)
                       (when data
                         (setq fragment (subseq* data (the fixnum start) (the fixnum end)))))))))
          (multiple-value-bind (parsed-data start end got-scheme)
              (parse-scheme-byte-vector data :start parse-start :end parse-end)
            (if parsed-data
                (locally (declare (type fixnum start end))
                  (setq scheme
                        (or got-scheme
                            (let ((data-str (make-string (- end start))))
                              (do ((i start (1+ i))
                                   (j 0 (1+ j)))
                                  ((= i end) data-str)
                                (let ((code (aref data i)))
                                  (setf (aref data-str j)
                                        (code-char
                                         (if (<= #.(char-code #\A) code #.(char-code #\Z))
                                             (+ code 32)
                                             code))))))))
                  (incf end))           ;eat the trailing #\:
                (setq scheme nil
                      end parse-start))
            (locally (declare (type fixnum end))
              (unless (= end parse-end)
                (multiple-value-bind (parsed-data userinfo-start userinfo-end
                                      host-start host-end port-start port-end)
                    (parse-authority-byte-vector data :start end :end parse-end)
                  (when parsed-data
                    (locally (declare (type ->u8 data)
                                      (type fixnum host-start host-end))
                      (when userinfo-start
                        (setq userinfo (subseq* data (the fixnum userinfo-start) (the fixnum userinfo-end))))
                      (unless (= host-start host-end)
                        (setq host (subseq* data host-start host-end)))
                      (cond
                        (port-start
                         (setq port
                               (parse-integer-from-bv data :start port-start :end port-end)))
                        (scheme
                         (setq port (scheme-default-port scheme))))))
                  (locally (declare (optimize (safety 0)))
                    (parse-from-path data (or port-end host-end (1+ end)))))))))))
    (values scheme userinfo host port path query fragment)))

(defun parse-uri (data &key (start 0) end)
  "Parse a URI string or a URI byte vector and return 7 URI components:
- scheme,
- userinfo,
- host name,
- port,
- path,
- query,
- fragment."
  (etypecase data
    (simple-string (parse-uri-string data :start start :end end))
    (->u8 (parse-uri-byte-vector data :start start :end end))
    (string (parse-uri (coerce data 'simple-string) :start start :end end))))

(defmacro defun-with-array-parsing (name (char p data start end &rest other-args) &body body)
  (with-gensyms (args type form env)
    (flet ((intern-proper-case (a b)
             (intern (format nil "~:@(~a-~a~)" a b))))
      (let ((fn-for-string (intern-proper-case name :string))
            (fn-for-byte-vector (intern-proper-case name :byte-vector)))
        `(progn
           (defun ,name (,data &rest ,args &key ,start ,end)
             (declare (ignore ,start ,end))
             (etypecase ,data
               (simple-string (apply ',(intern-proper-case name :string) data ,args))
               (->u8 (apply ',(intern-proper-case name :byte-vector) data ,args))))

           (define-compiler-macro ,name (&whole ,form &environment ,env ,data &rest ,args)
             (declare (ignore ,args))
             (let ((,type (cond
                            ((constantp ,data) (type-of ,data))
                            ((symbolp ,data) (cdr (assoc 'type (nth-value 2 (variable-information ,data ,env))))))))
               (cond
                 ((null ,type) ,form)
                 ((subtypep ,type 'simple-string) `(,',fn-for-string ,@(cdr ,form)))
                 ((subtypep ,type '->u8) `(,',fn-for-byte-vector ,@(cdr ,form)))
                 (t ,form))))

           (defun ,fn-for-string (,data &key (,start 0) (,end (length ,data)) ,@other-args)
             (declare (type simple-string ,data)
                      (type fixnum ,start ,end))
             (macrolet ((char=* (char1 char2)
                          `(char= ,char1 ,char2))
                        (char-code* (char)
                          `(char-code ,char))
                        (scheme-char-p* (char)
                          `(scheme-char-p ,char))
                        (standard-alpha-char-p* (char)
                          `(standard-alpha-char-p ,char)))
               (block ,name
                 (with-string-parsing (,char ,p ,data ,start ,end)
                   (declare (type fixnum ,p))
                   ,@body))))

           (defun ,fn-for-byte-vector (,data &key (,start 0) (,end (length ,data)) ,@other-args)
             (declare (type ->u8 ,data)
                      (type fixnum ,start ,end))
             (macrolet ((char=* (byte char)
                          `(= ,byte ,(char-code char)))
                        (char-code* (byte)
                          byte)
                        (scheme-char-p* (byte)
                          `(scheme-byte-p ,byte))
                        (standard-alpha-char-p* (byte)
                          `(standard-alpha-byte-p ,byte)))
               (block ,name
                 (with-byte-array-parsing (,char ,p ,data ,start ,end)
                   (declare (type fixnum ,p))
                   ,@body)))))))))

(defun scheme-char-p (char)
  (declare (type character char))
  (or (standard-alphanumeric-p char)
      (char= char #\+)
      (char= char #\-)
      (char= char #\.)))

(defun scheme-byte-p (byte)
  (declare (type u8 byte))
  (or (standard-alphanumeric-byte-p byte)
      (= byte (char-code #\+))
      (= byte (char-code #\-))
      (= byte (char-code #\.))))

(defun-with-array-parsing parse-scheme (char p data start end)
  (parsing-scheme-start
   (when (or (char=* char #\h)
             (char=* char #\H))
     (goto parsing-H))
   (unless (standard-alpha-char-p* char)
     (return-from parse-scheme nil))
   (gonext))

  (parsing-scheme
   (cond
     ((char=* char #\:)
      (return-from parse-scheme
        (values data start p)))
     ((scheme-char-p* char)
      (redo))
     (t
      (return-from parse-scheme nil))))

  (parsing-H
   (if (or (char=* char #\t)
           (char=* char #\T))
       (goto parsing-HT)
       (goto parsing-scheme 0)))

  (parsing-HT
   (if (or (char=* char #\t)
           (char=* char #\T))
       (goto parsing-HTT)
       (goto parsing-scheme 0)))

  (parsing-HTT
   (if (or (char=* char #\p)
           (char=* char #\P))
       (goto parsing-HTTP)
       (goto parsing-scheme 0)))

  (parsing-HTTP
   (cond
     ((char=* char #\:)
      (return-from parse-scheme
        (values data start p "http")))
     ((or (char=* char #\s)
          (char=* char #\S))
      (goto parsing-HTTPS))
     (t (goto parsing-scheme 0))))

  (parsing-HTTPS
   (if (char=* char #\:)
       (return-from parse-scheme
         (values data start p "https"))
       (goto parsing-scheme 0)))

  (:eof (return-from parse-scheme nil)))

(defun-with-array-parsing parse-authority (char p data start end
                                                &aux
                                                (authority-mark nil)
                                                (colon-mark nil)
                                                userinfo-start
                                                userinfo-end
                                                host-start
                                                host-end
                                                port-start
                                                port-end)
  (parsing-first
   (cond
     ((char=* char #\/)
      (gonext))
     (t
      (return-from parse-authority
        (values data nil nil start start nil nil)))))

  (parsing-authority-starting
   (unless (char=* char #\/)
     (return-from parse-authority
       (values data nil nil start start nil nil)))
   (setq authority-mark (1+ p))
   (gonext))

  (parsing-authority-start
   (if (char=* char #\[)
       (goto parsing-ipliteral)
       (gonext 0)))

  ;; parsing host or userinfo
  (parsing-authority
   (cond
     ((char=* char #\:)
      (setq colon-mark p)
      (redo))
     ((char=* char #\@)
      (when userinfo-start
        (error 'uri-malformed-string :data data :position p))
      (setq userinfo-start authority-mark
            userinfo-end p)
      (setq authority-mark (1+ p)
            colon-mark nil)
      (redo))
     ((or (char=* char #\/)
          (char=* char #\?)
          (char=* char #\#))
      (go :eof))
     ((let ((code (char-code* char)))
        (or (<= 128 code)
            (= (aref +uri-char+ code) 1)))
      (redo))
     (t (error 'uri-malformed-string
               :data data :position p))))

  (parsing-ipliteral
   (if (char=* char #\])
       (goto parsing-authority)
       (redo)))

  (:eof
   (unless authority-mark
     (return-from parse-authority
       (values data
               nil nil
               start start
               nil nil)))
   (if colon-mark
       (setq host-start authority-mark
             host-end colon-mark
             port-start (1+ colon-mark)
             port-end p)
       (setq host-start authority-mark
             host-end p))
   (return-from parse-authority
     (values data
             userinfo-start userinfo-end
             host-start host-end
             port-start port-end))))

(defun path-char-p (char)
  (declare (type character char))
  (let ((byte (char-code char)))
    (and (< byte 128)
         (or (= (aref +uri-char+ byte) 1)
             (= byte #.(char-code #\/))))))

(defun path-byte-p (byte)
  (declare (type u8 byte))
  (or (= (aref +uri-char+ byte) 1)
      (= byte (char-code #\/))))

(defun query-char-p (char)
  (declare (type character char))
  (or (path-char-p char)
      (char= char #\?)))

(defun query-byte-p (byte)
  (declare (type u8 byte))
  (or (path-byte-p byte)
      (= byte (char-code #\?))))

(defmacro parse-until-string (delimiters data &key start end test)
  (with-gensyms (p char)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type fixnum ,p))
           (let ((,char (aref ,data ,p)))
             (declare (type character ,char))
             (when (or ,@(loop for delim in delimiters
                               collect `(char= ,delim ,char)))
               (return (values ,data ,start ,p)))
             ,@(when test
                 `((unless (funcall ,test ,char)
                     (error 'uri-malformed-string
                            :data ,data :position ,p))))))))))

(defmacro parse-until-byte-vector (delimiters data &key start end test)
  (with-gensyms (p byte)
    `(block nil
       (progn
         (do ((,p ,start (1+ ,p)))
             ((= ,p ,end)
              (values ,data ,start ,end))
           (declare (type fixnum ,p))
           (let ((,byte (aref ,data ,p)))
             (declare (type u8 ,byte))
             (when (or ,@(loop for delim in delimiters
                               collect `(= ,(char-code delim) ,byte)))
               (return (values ,data ,start ,p)))
             ,@(when test
                 `((unless (funcall ,test ,byte)
                     (error 'uri-malformed-string
                            :data ,data :position ,p))))))))))

(defun parse-path (data &key (start 0) (end (length data)))
  (etypecase data
    (simple-string
     (parse-path-string data :start start :end end))
    (->u8
     (parse-path-byte-vector data :start start :end end))))

(defun parse-path-string (data &key (start 0) (end (length data)))
  (declare (type simple-string data)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until-string (#\? #\#) data :start start :end end))

(defun parse-path-byte-vector (data &key (start 0) (end (length data)))
  (declare (type ->u8 data)
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (parse-until-byte-vector (#\? #\#) data :start start :end end))

(defun parse-query (data &key (start 0) (end (length data)))
  (etypecase data
    (string
     (parse-query-string data :start start :end end))
    (->u8
     (parse-query-byte-vector data :start start :end end))))

(define-compiler-macro parse-query (&whole form &environment env data &key start end)
  (declare (ignore start end))
  (let ((type (cond
                ((constantp data) (type-of data))
                ((symbolp data) (cdr (assoc 'type (nth-value 2 (variable-information data env))))))))
    (cond
      ((null type) form)
      ((subtypep type 'simple-string) `(parse-query-string ,@(cdr form)))
      ((subtypep type '->u8) `(parse-query-byte-vector ,@(cdr form)))
      (t form))))

(defun parse-query-string (data &key (start 0) (end (length data)))
  (declare (type simple-string data)
           (type fixnum start end))
  (let ((?-pos (position #\? data :start start :end end)))
    (when ?-pos
      (parse-until-string (#\#) data :start (1+ (the fixnum ?-pos)) :end end))))

(defun parse-query-byte-vector (data &key (start 0) (end (length data)))
  (declare (type ->u8 data)
           (type fixnum start end))
  (let ((?-pos (position #.(char-code #\?) data :start start :end end)))
    (when ?-pos
      (parse-until-byte-vector (#\#) data :start (1+ (the fixnum ?-pos)) :end end))))

(defun parse-fragment (data &key (start 0) (end (length data)))
  (etypecase data
    (string (parse-fragment-string data :start start :end end))
    (->u8 (parse-fragment-byte-vector data :start start :end end))))

(define-compiler-macro parse-fragment (&whole form &environment env data &key start end)
  (declare (ignore start end))
  (let ((type (cond
                ((constantp data) (type-of data))
                ((symbolp data) (cdr (assoc 'type (nth-value 2 (variable-information data env))))))))
    (cond
      ((null type) form)
      ((subtypep type 'simple-string) `(parse-fragment-string ,@(cdr form)))
      ((subtypep type '->u8) `(parse-fragment-byte-vector ,@(cdr form)))
      (t form))))

(defun parse-fragment-string (data &key (start 0) (end (length data)))
  (declare (type simple-string data)
           (type fixnum start end))
  (let ((|#-pos| (position #\# data
                           :start start
                           :end end)))
    (when |#-pos|
      (values data (1+ (the fixnum |#-pos|)) end))))

(defun parse-fragment-byte-vector (data &key (start 0) (end (length data)))
  (declare (type ->u8 data)
           (type fixnum start end))
  (let ((|#-pos| (position #\# data
                           :start start
                           :end end
                           :key #'code-char)))
    (when |#-pos|
      (values data (1+ (the fixnum |#-pos|)) end))))

(declaim (ftype (function (character) (unsigned-byte 4)) hexdigit-to-integer))
(defun hexdigit-to-integer (char)
  (declare (type character char))
  (let ((code (char-code char)))
    (declare (type fixnum code))
    (cond
      ((<= #.(char-code #\0) code #.(char-code #\9))
       (- code #.(char-code #\0)))
      ((<= #.(char-code #\A) code #.(char-code #\F))
       (- code #.(- (char-code #\A) 10)))
      ((<= #.(char-code #\a) code #.(char-code #\f))
       (- code #.(- (char-code #\a) 10)))
      (t (error 'url-decoding-error)))))

(defun url-decode (data &key
                          (encoding epsilon.lib.char:*default-character-encoding*)
                          (start 0)
                          end
                          (lenient nil))
  (declare (type (or string ->u8) data)
           (type integer start))
  (let* ((end (or end (length data)))
         (buffer (make-array (- end start)
                             :element-type 'u8))
         (i 0)
         parsing-encoded-part)
    (declare (type integer end i)
             (type ->u8 buffer))
    (flet ((write-to-buffer (byte)
             (setf (aref buffer i) byte)
             (incf i)))
      (with-array-parsing (char p data start end (and (not (stringp data))
                                                      #'code-char))
        (parsing
         (cond
           ((char= char #\%)
            (gonext))
           ((char= char #\+)
            (write-to-buffer #.(char-code #\Space))
            (redo))
           (t
            (write-to-buffer (char-code char))
            (redo))))

        (parsing-encoded-part
         (setq parsing-encoded-part char)
         (gonext))

        (parsing-encoded-part-second
         (handler-bind ((url-decoding-error
                          (lambda (error)
                            (declare (ignore error))
                            (when lenient
                              (write-to-buffer #.(char-code #\%))
                              (write-to-buffer (char-code parsing-encoded-part))
                              (write-to-buffer (char-code char))
                              (setq parsing-encoded-part nil)
                              (goto parsing)))))
           (write-to-buffer
            (+ (* 16 (hexdigit-to-integer parsing-encoded-part))
               (hexdigit-to-integer char))))
         (setq parsing-encoded-part nil)
         (goto parsing))

        (:eof
         (when parsing-encoded-part
           (error 'url-decoding-error)))))
    (u8-to-string buffer :end i :encoding encoding :errorp (not lenient))))

(defun url-decode-params (data &key
                                 (delimiter #\&)
                                 (encoding epsilon.lib.char:*default-character-encoding*)
                                 (start 0)
                                 end
                                 (lenient nil))
  (declare (type (or string ->u8) data)
           (type integer start)
           (type character delimiter))
  (let ((end (or end (length data)))
        (start-mark nil)
        (=-mark nil))
    (declare (type integer end))
    (collecting
      (flet ((collect-pair (p)
               (tagbody
                  (handler-bind ((url-decoding-error
                                   (lambda (error)
                                     (declare (ignore error))
                                     (when lenient
                                       (go continue)))))
                    (collect
                        (cons (url-decode data :encoding encoding
                                               :start start-mark :end =-mark
                                               :lenient lenient)
                              (url-decode data :encoding encoding
                                               :start (1+ =-mark) :end p
                                               :lenient lenient))))
                continue)
               (setq start-mark nil
                     =-mark nil))
             (collect-field (p)
               (tagbody
                  (handler-bind ((url-decoding-error
                                   (lambda (error)
                                     (declare (ignore error))
                                     (when lenient
                                       (go continue)))))
                    (collect
                        (cons (url-decode data :encoding encoding
                                               :start start-mark :end p
                                               :lenient lenient)
                              nil)))
                continue)
               (setq start-mark nil)))
        (with-array-parsing (char p data start end (and (not (stringp data))
                                                        #'code-char))
          (start
           (setq start-mark p)
           (if lenient
               (cond
                 ((char= char #\=)
                  (setq =-mark p)
                  (goto parsing-value))
                 ((char= char delimiter)
                  (redo)))
               (when (or (char= char #\=)
                         (char= char delimiter))
                 (error 'uri-malformed-urlencoded-string)))
           (gonext))

          (parsing-field
           (cond
             ((char= char #\=)
              (setq =-mark p)
              (gonext))
             ((char= char delimiter)
              ;; field only
              (collect-field p)
              (goto start)))
           (redo))

          (parsing-value
           (cond
             ((char= char #\=)
              (unless lenient
                (error 'uri-malformed-urlencoded-string)))
             ((char= char delimiter)
              (collect-pair p)
              (goto start)))
           (redo))

          (:eof
           (cond
             (=-mark (collect-pair p))
             (start-mark (collect-field p)))))))))

(declaim (type (simple-array character (16)) +hexdigit-char+))
(defvar +hexdigit-char+
  (let ((ary (make-array 16 :element-type 'character)))
    (loop for char across "0123456789ABCDEF"
          for i from 0
          do (setf (aref ary i) char))
    ary))

(defun integer-to-hexdigit (byte)
  (declare (type u8 byte))
  (let ((res (make-string 2)))
    (multiple-value-bind (quotient remainder)
        (floor byte 16)
      (setf (aref res 0) (aref +hexdigit-char+ quotient)
            (aref res 1) (aref +hexdigit-char+ remainder)))
    res))

(defun unreservedp (byte)
  (declare (type u8 byte))
  (or (<= (char-code #\A) byte (char-code #\Z))
      (<= (char-code #\a) byte (char-code #\z))
      (<= (char-code #\0) byte (char-code #\9))
      #.`(or ,@(loop for char across "-._~"
                     collect `(= byte ,(char-code char))))))

(declaim (type (simple-array string (97)) +byte-to-string+))
(defvar +byte-to-string+
  (let ((ary (make-array 97 :element-type 'string :initial-element "")))
    (loop for i from 0 to 96
          unless (unreservedp i)
            do (setf (aref ary i) (integer-to-hexdigit i)))
    ary))

(defun url-encode (data &key
                          (encoding epsilon.lib.char:*default-character-encoding*)
                          (start 0)
                          end
                          space-to-plus)
  (declare (type (or string ->u8) data)
           (type integer start))
  (let* ((octets (if (stringp data)
                     (epsilon.lib.char:string-to-u8 data :encoding encoding :start start :end end)
                     data))
         (res (make-array (* (length octets) 3) :element-type 'character :fill-pointer t))
         (i 0))
    (declare (type ->u8 octets)
             (type string res)
             (type integer i))
    (loop for byte of-type u8 across octets do
      (cond
        ((and space-to-plus
              (= byte #.(char-code #\Space)))
         (setf (aref res i) #\+)
         (incf i))
        ((< byte #.(char-code #\a))
         (let ((converted (aref +byte-to-string+ byte)))
           (if (zerop (length converted))
               (progn
                 (setf (aref res i) (code-char byte))
                 (incf i))
               (progn
                 (setf (aref res i) #\%)
                 (incf i)
                 (replace res converted :start1 i)
                 (incf i 2)))))
        ((unreservedp byte)
         (setf (aref res i) (code-char byte))
         (incf i))
        (t
         (setf (aref res i) #\%)
         (incf i)
         (replace res (integer-to-hexdigit byte) :start1 i)
         (incf i 2))))
    (setf (fill-pointer res) i)
    res))

(defun url-encode-params (params-alist &key (encoding epsilon.lib.char:*default-character-encoding*)
                                         space-to-plus)
  (check-type params-alist list)
  (with-output-to-string (s)
    (loop for ((field . value) . rest) on params-alist do
      (write-string (url-encode field :encoding encoding :space-to-plus space-to-plus) s)
      (when value
        (write-char #\= s)
        (check-type value (or string number ->u8))
        (write-string (url-encode (if (numberp value)
                                      (with-standard-io-syntax
                                        (write-to-string value))
                                      value)
                                  :encoding encoding
                                  :space-to-plus space-to-plus)
                      s))
      (when rest
        (write-char #\& s)))))

(defstruct (uri (:constructor %make-uri)
                (:conc-name nil)
                (:copier %copy-uri))
  (scheme nil :read-only t)
  userinfo
  host
  port
  path
  query
  fragment)

(defun make-basic-uri (&rest args &key scheme userinfo host port path query fragment)
  (declare (ignore scheme userinfo host port path query fragment))
  (let ((uri (apply #'%make-uri args)))
    (unless (port uri)
      (setf (port uri) (scheme-default-port (scheme uri))))
    uri))

(defun authority (uri)
  (when (host uri)
    (let ((default-port (scheme-default-port (scheme uri))))
      (with-standard-io-syntax
        (format nil "~:[~;~:*~A@~]~A~:[:~A~;~*~]"
                (userinfo uri)
                (host uri)
                (eql (port uri) default-port)
                (port uri))))))

(defstruct (urn (:include uri (scheme :urn))
                (:constructor %make-urn))
  nid
  nss)

(defun make-urn (&rest initargs)
  (let ((urn (apply #'%make-urn initargs)))
    (when (path urn)
      (let ((colon-pos (position #\: (path urn))))
        (if colon-pos
            (setf (urn-nid urn) (subseq (path urn) 0 colon-pos)
                  (urn-nss urn) (subseq (path urn) (1+ colon-pos)))
            (setf (urn-nid urn) (path urn)))))
    urn))

(defun uri-tld (uri)
  (let ((host (host uri)))
    (when (and host
               (not (ip-addr-p host)))
      (let ((pos (position #\. host :from-end t)))
        (if pos
            (subseq host (1+ pos))
            host)))))

(defun uri-domain (uri)
  (host uri))

(defun ipv4-addr-p (host)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-type host string)
  (flet ((read-byte-string (string start)
           (declare (type fixnum start))
           (when (<= (length string) start)
             (return-from read-byte-string nil))
           (let* ((end (+ start 2))
                  (endp (<= (1- (length string)) end))
                  (end (if endp
                           (1- (length string))
                           end))
                  (res 0))
             (declare (type fixnum end res))
             (do ((i start (1+ i)))
                 ((< end i))
               (declare (type fixnum i))
               (unless (char<= #\0 (aref string i) #\9)
                 (return-from read-byte-string
                   (if (= i start)
                       nil
                       (values res i nil))))
               (setq res
                     (+ (* res 10)
                        (- (char-code (aref string i)) 48))))
             (cond
               (endp
                (values res end t))
               ((char= (aref string (1+ end)) #\.)
                (values res (1+ end) nil))))))
    (let ((start 0))
      (dotimes (i 4 t)
        (multiple-value-bind (byte pos endp)
            (read-byte-string host start)
          (unless (typep byte 'u8)
            (return nil))
          (unless (xor endp (not (= i 3)))
            (return nil))
          (setq start (1+ pos)))))))

(defun trim-brackets (host)
  (if (char= (aref host 0) #\[)
      (if (char= (aref host (1- (length host))) #\])
          (subseq host 1 (1- (length host)))
          nil)
      host))

(defun ipv6-addr-p (host)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-type host string)
  (when (= (length host) 0)
    (return-from ipv6-addr-p nil))

  (labels ((read-section (string start &optional read-colons)
             (declare (type string string)
                      (type fixnum start))
             (when (<= (length string) start)
               (return-from read-section
                 (values start read-colons t)))
             (when (char= (aref string start) #\:)
               (cond
                 ((<= (length string) (1+ start))
                  (return-from read-section nil))
                 ((char= (aref string (1+ start)) #\:)
                  (if read-colons
                      (return-from read-section nil)
                      (return-from read-section (read-section string (+ 2 start) t))))
                 (t (incf start))))
             (let* ((end (+ start 4))
                    (endp (<= (length string) end))
                    (end (if endp
                             (length string)
                             end)))
               (declare (type fixnum end))

               (do ((i start (1+ i)))
                   ((= end i))
                 (let ((ch (aref string i)))
                   (cond
                     ((char= ch #\:)
                      (return-from read-section
                        (values i read-colons nil)))
                     ((or (char<= #\0 ch #\9)
                          (char<= #\a ch #\f)
                          (char<= #\A ch #\F)))
                     (t (return-from read-section nil)))))

               (if endp
                   (values end read-colons endp)
                   (if (char= (aref string end) #\:)
                       (values end read-colons endp)
                       nil)))))

    (setq host (trim-brackets host))
    (unless host
      (return-from ipv6-addr-p nil))

    (let ((start 0)
          (read-colons-p nil))
      (dotimes (i 8 t)
        (multiple-value-bind (e read-colons endp)
            (read-section host start read-colons-p)
          (unless e
            (return-from ipv6-addr-p nil))
          (when endp
            (when (and (not (= i 7))
                       (not read-colons))
              (return-from ipv6-addr-p nil))
            (return-from ipv6-addr-p t))
          (when (and (= i 7) (not endp))
            (return-from ipv6-addr-p nil))
          (setq start e
                read-colons-p read-colons))))))

(defun ip-addr-p (host)
  (or (ipv4-addr-p host)
      (ipv6-addr-p host)))

(defun ip-addr= (ip1 ip2)
  (flet ((parse-ipv6 (ip)
           (setq ip (trim-brackets ip))
           (cond
             ((char= (aref ip 0) #\:)
              (setq ip (concatenate 'string "0" ip)))
             ((char= (aref ip (1- (length ip))) #\:)
              (setq ip (concatenate 'string ip "0"))))
           (let* ((ip-parsed (epsilon.lib.seq:split-sequence #\: ip))
                  (len (length ip-parsed)))
             (loop for section in ip-parsed
                   if (string= section "")
                     append (make-list (- 9 len) :initial-element 0)
                   else
                     collect (parse-integer section :radix 16)))))
    (cond
      ((ipv4-addr-p ip1)
       (string= ip1 ip2))
      ((ipv6-addr-p ip1)
       (and (ipv6-addr-p ip2)
            (equal (parse-ipv6 ip1)
                   (parse-ipv6 ip2)))))))

(defstruct (uri-http (:include uri (scheme "http") (port #.(scheme-default-port "http")))))

(defstruct (uri-https (:include uri-http (scheme "https") (port #.(scheme-default-port "https")))))

(defun uri-query-params (http &key (lenient t))
  (when-let (query (query http))
    (url-decode-params query :lenient lenient)))

(defun (setf uri-query-params) (new http &key lenient)
  (declare (ignore lenient))
  (setf (query http) (if new
                         (url-encode-params new)
                         nil)))



(defstruct (uri-file (:include uri (scheme "file") (port nil))))

(declaim (ftype (function (uri-file) pathname) uri-file-pathname))
(defun uri-file-pathname (file)
  "Get a lisp pathname object from a file URI.
Assumes that the path of the file URI is correct path syntax for the environment."
  (parse-namestring (path file)))

(defun scheme-constructor (scheme)
  "Get a constructor function appropriate for the scheme."
  (cond
    ((string= scheme "http")  #'make-uri-http)
    ((string= scheme "https") #'make-uri-https)
    ((string= scheme "file")  #'make-uri-file)
    ((string= scheme "urn")   #'make-urn)
    (t                        #'make-basic-uri)))

(defun uri (data &key (start 0) end)
  (if (uri-p data)
      data
      (multiple-value-bind (scheme userinfo host port path query fragment)
          (parse-uri data :start start :end end)
        (apply (scheme-constructor scheme)
               :scheme scheme
               :userinfo userinfo
               :host host
               :path path
               :query query
               :fragment fragment
               (and port
                    `(:port ,port))))))

(defun copy-uri (uri &key (scheme (scheme uri))
                       (userinfo (userinfo uri))
                       (host (host uri))
                       (port (port uri))
                       (path (path uri))
                       (query (query uri))
                       (fragment (fragment uri)))
  (make-uri :scheme scheme
            :userinfo userinfo
            :host host
            :port port
            :path path
            :query query
            :fragment fragment))

(defun make-uri (&rest initargs &key scheme userinfo host port path query fragment defaults)
  (declare (ignore userinfo host port path fragment))
  (setf initargs (delete-from-plist initargs :defaults))
  (if defaults
      (apply #'copy-uri (uri defaults) initargs)
      (progn
        (when (consp query)
          (setf (getf initargs :query) (url-encode-params query)))
        (apply (scheme-constructor scheme) initargs))))

(defun render-uri (uri &optional stream)
  (flet ((maybe-slash (authority path)
           (if (and (not (emptyp authority)) (not (emptyp path))
                    (char/= (epsilon.lib.string:last-char authority) #\/)
                    (char/= (epsilon.lib.string:first-char path) #\/))
               "/"
               "")))
    (cond
      ((uri-file-p uri)
       (format stream
               "~@[~(~A~)://~]~@[~A~]~@[?~A~]~@[#~A~]"
               (scheme uri)
               (path uri)
               (query uri)
               (fragment uri)))
      (t
       (format stream
               "~@[~(~A~):~]~@[//~A~]~a~@[~A~]~@[?~A~]~@[#~A~]"
               (scheme uri)
               (authority uri)
               (maybe-slash (authority uri) (path uri))
               (path uri)
               (query uri)
               (fragment uri))))))

(defun %uri= (uri1 uri2 &key normalize-path-p)
  (check-type uri1 uri)
  (check-type uri2 uri)
  (flet ((%path (path)
           "Define path equivalence relations."
           (cond (normalize-path-p
                  (if (or (null path) (equal path ""))
                      "/"
                      path))
                 (t
                  (or path "")))))
    (and (eq (type-of uri1) (type-of uri2))
         (equal (%path (path uri1)) (%path (path uri2)))
         (equal (query uri1) (query uri2))
         (equal (fragment uri1) (fragment uri2))
         (equalp (authority uri1) (authority uri2)))))

(defun uri= (uri1 uri2)
  "Whether URI1 refers to the same URI as URI2.
Paths are not normalized. See `uri-equal'."
  (%uri= uri1 uri2))

(defun uri-equal (uri1 uri2)
  "Whether URI1 refers to the same URI as URI2.
Empty paths are normalized to '/' as per RFC 3986
(https://tools.ietf.org/html/rfc3986#section-6.2.3).
See `uri='."
  (%uri= uri1 uri2  :normalize-path-p t))

(defmethod print-object ((uri uri) stream)
  (if (and (null *print-readably*) (null *print-escape*))
      (render-uri uri stream)
      (format stream "#<~S ~A>"
              (type-of uri)
              (render-uri uri))))

(defun merge-paths (ref-path base-path)
  (declare (type (or string null) ref-path base-path))
  (let* ((path-list (and base-path (nreverse (epsilon.lib.seq:split-sequence #\/ base-path))))
         (ref-components (and ref-path (epsilon.lib.seq:split-sequence #\/ ref-path)))
         ending-slash-p)
    ;; remove last component of base
    (pop path-list)
    (dolist (component ref-components)
      (cond ((string= ".." component)
             (pop path-list)
             (setf ending-slash-p t))
            ((string= "." component)
             (setf ending-slash-p t))
            (t
             (push component path-list)
             (setf ending-slash-p nil))))
    (setf path-list (nreverse path-list))
    (with-output-to-string (s)
      (loop for (component . more) on path-list
            do (progn
                 (write-string component s)
                 (when (or more ending-slash-p)
                   (write-char #\/ s)))))))

(defun merge (base reference)
  "Merge a reference URI into the base URI as described in RFC 2396 Section 5.a2.
The returned URI is always a new instance. Neither REFERENCE nor BASE
is mutated."
  (let* ((reference (uri reference))
         (base (uri base))
         (merged-uri (copy-uri reference)))
    (declare (uri reference base))
    ;; Steps described at
    ;; https://datatracker.ietf.org/doc/html/rfc2396#section-5.2
    ;; Step 1 is absent since it's implicit
    (flet ((return-merged-uri () (return-from merge (uri merged-uri)))
           (%merge-paths () (setf (path merged-uri)
                                  (merge-paths (path merged-uri) nil))))
      ;; Step 2
      (when (uri-equal reference base)
        (return-merged-uri))
      ;; Step 3
      (when (scheme merged-uri)
        (%merge-paths)
        (return-merged-uri))
      (setf merged-uri (copy-uri merged-uri :scheme (scheme base)))
      ;; Step 4
      (when (null (port merged-uri))
        (setf (port merged-uri) (scheme-default-port (scheme merged-uri))))
      (when (host merged-uri)
        (%merge-paths)
        (return-merged-uri))
      (setf (userinfo merged-uri) (userinfo base))
      (setf (host merged-uri) (host base))
      (setf (port merged-uri) (port base))
      ;; Step 5
      (when (null (path merged-uri))
        (setf (path merged-uri) (path base))
        (return-merged-uri))
      ;; Step 6
      (when-let* ((p (path merged-uri))
                  (first-char (and (> (length p) 0) (char p 0)))
                  (_ (char= #\/ first-char)))
        (%merge-paths)
        (return-merged-uri))
      ;; Step 7
      (setf (path merged-uri)
            (merge-paths (path merged-uri) (path base)))
      (return-merged-uri))))
