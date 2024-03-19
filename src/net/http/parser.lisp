(defpackage #:net.http.parser
  (:use
   #:cl
   #:sb-cltl2
   #:lib.binding
   #:lib.buffer
   #:lib.char
   #:lib.collect
   #:lib.list
   #:lib.symbol
   #:lib.type
   #:lib.xsubseq)
  (:export

   :make-parser
   :http-request
   :http-response
   :make-http-request
   :make-http-response
   :http-request-p
   :http-response-p
   :make-callbacks
   :http-version
   :http-major-version
   :http-minor-version
   :http-method
   :http-resource
   :http-status
   :http-status-text
   :http-content-length
   :http-chunked-p
   :http-upgrade-p
   :http-headers

   ;; multipart parser
   :make-multipart-parser

   ;; Low-level parser API
   :http
   :http-p
   :make-http
   :parse-request
   :parse-response

   :http-multipart-parse
   :ll-multipart-parser
   :make-ll-multipart-parser

   ;; Error
   :fast-http-error

   :callback-error
   :cb-message-begin
   :cb-url
   :cb-first-line
   :cb-header-field
   :cb-header-value
   :cb-headers-complete
   :cb-body
   :cb-message-complete
   :cb-status

   :parsing-error
   :invalid-eof-state
   :header-overflow
   :closed-connection
   :invalid-version
   :invalid-status
   :invalid-method
   :invalid-url
   :invalid-host
   :invalid-port
   :invalid-path
   :invalid-query-string
   :invalid-fragment
   :lf-expected
   :invalid-header-token
   :invalid-content-length
   :invalid-chunk-size
   :invalid-constant
   :invalid-internal-state
   :strict-error
   :paused-error
   :unknown-error

   :multipart-parsing-error
   :invalid-multipart-body
   :invalid-boundary

   :header-value-parsing-error
   :invalid-header-value
   :invalid-parameter-key
   :invalid-parameter-value))
   

(in-package #:net.http.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)

(define-condition match-failed (error)
  ((elem :initarg :elem
         :initform nil)
   (expected :initarg :expected
             :initform nil))
  (:report (lambda (condition stream)
             (with-slots (elem expected) condition
               (format stream
                       "Match failed~:[~;~:*: ~S~]~:[~;~:* (expected: ~{~S~^, ~})~]"
                       (ensure-char-elem elem) expected)))))

(defun convert-case-conditions (var chars)
  (cond
    ((consp chars)
     `(or ,@(loop for ch in chars
                  if (characterp ch)
                    collect `(char= ,var ,ch)
                  else
                    collect `(= ,var ,ch))))
    ((eq chars 'otherwise)
     t)
    (t (if (characterp chars)
           `(char= ,var ,chars)
           `(= ,var ,chars)))))

(defun typed-case-tagbodies (var &rest cases)
  (cond
    ((null cases) nil)
    ((= 1 (length cases))
     `((when ,(convert-case-conditions var (car (first cases)))
         ,@(cdr (first cases)))))
    ((and (= 2 (length cases))
          (eq (car (second cases)) 'otherwise))
     `((unless ,(convert-case-conditions var (car (first cases)))
         ,@(cdr (second cases)))
       ,@(cdr (first cases))))
    (t
     (let ((tags (make-array (length cases) :initial-contents (loop repeat (length cases)
                                                                    collect (gensym))))
           (end (gensym "END")))
       `(,@(loop for (chars . body) in cases
                 for i from 0
                 collect `(when ,(convert-case-conditions var chars)
                            (go ,(aref tags i))))
         ,@(loop for case in cases
                 for i from 0
                 append `(,(aref tags i)
                          ,@(cdr case)
                          (go ,end)))
         ,end)))))

(defmacro vector-case (elem-var vec-and-options &body cases)
  (destructuring-bind (vec &key case-insensitive)
      (ensure-cons vec-and-options)
    (with-gensyms (otherwise end-tag vector-case-block)
      (labels ((case-candidates (el)
                 (cond
                   ((not case-insensitive) el)
                   ((characterp el)
                    (cond
                      ((char<= #\a el #\z)
                       `(,el
                         ,(code-char
                           (- (char-code el)
                              #.(- (char-code #\a) (char-code #\A))))))
                      ((char<= #\A el #\Z)
                       `(,el
                         ,(code-char
                           (+ (char-code el)
                              #.(- (char-code #\a) (char-code #\A))))))
                      (t el)))
                   ((typep el 'u8)
                    (cond
                      ((<= #.(char-code #\a) el #.(char-code #\z))
                       `(,el
                         ,(- el #.(- (char-code #\a) (char-code #\A)))))
                      ((<= #.(char-code #\A) el #.(char-code #\Z))
                       `(,el
                         ,(+ el #.(- (char-code #\a) (char-code #\A)))))
                      (t el)))
                   (t el)))
               (build-case (i cases vec)
                 (when cases
                   (let ((map (make-hash-table)))
                     (map nil
                          (lambda (case)
                            (unless (vectorp (car case))
                              (error "The first element of cases must be a constant vector"))
                            (unless (<= (length (car case)) i)
                              (push case (gethash (aref (car case) i) map))))
                          cases)
                     (let (res-cases)
                       (maphash (lambda (el cases)
                                  (let ((next-case (build-case (1+ i) cases vec)))
                                    (cond
                                      (next-case
                                       (push
                                        `(,(case-candidates el)
                                          (unless (advance*)
                                            ,(if (= (length (caar cases)) (1+ i))
                                                 `(progn ,@(cdr (car cases))
                                                         (go ,end-tag))
                                                 `(go :eof)))
                                          ,@(apply #'typed-case-tagbodies elem-var
                                                   (append
                                                    next-case
                                                    `((otherwise (go ,otherwise))))))
                                        res-cases))
                                      (t
                                       (push `(,(case-candidates el)
                                               (advance*)
                                               (return-from ,vector-case-block
                                                 (progn ,@(cdr (car cases)))))
                                             res-cases)))))
                                map)
                       res-cases)))))
        (let ((otherwise-case nil))
          (when (eq (caar (last cases)) 'otherwise)
            (setq otherwise-case (car (last cases))
                  cases (butlast cases)))
          `(block ,vector-case-block
             (tagbody
                ,@(apply #'typed-case-tagbodies elem-var
                         (append
                          (build-case 0 cases vec)
                          `((otherwise (go ,otherwise)))))
                (go ,end-tag)
                ,otherwise
                ,@(when otherwise-case
                    `(unless (eofp)
                       (return-from ,vector-case-block
                         (progn ,@(cdr otherwise-case)))))
                ,end-tag)))))))

(defun variable-type (var &optional env)
  (declare (ignorable env))
  (cond
    ((constantp var) (type-of var))
    ((and (symbolp var)
          (cdr (assoc 'type (nth-value 2 (variable-information var env))))))
    ((and (listp var)
          (eq (car var) 'the)
          (cadr var)))))

(defun variable-type* (var &optional env)
  (let ((type (variable-type var env)))
    (cond
      ((null type) nil)
      ((subtypep type 'string) 'string)
      ((subtypep type '->u8) '->u8))))

(defun check-skip-elems (elems)
  (or (every (lambda (elem)
               (or (characterp elem)
                   (and (consp elem)
                        (null (cddr elem))
                        (eq (first elem) 'not)
                        (characterp (second elem)))))
             elems)
      (error "'skip' takes only constant characters, or a cons starts with 'not'.")))

(defun check-match-cases (cases)
  (or (every (lambda (case)
               (and (consp case)
                    (or (eq (car case) 'otherwise)
                        (stringp (car case)))))
             cases)
      (error "'match-case' takes only constant strings at the car position.~%  ~S" cases)))

)

(defmacro bind ((symb &body bind-forms) &body body)
  (declare (ignore symb bind-forms body)))

(defmacro subseq* (data start &optional end)
  `(subseq ,data ,start ,end))
(defmacro get-elem (form) form)
(defun ensure-char-elem (elem)
  (if (characterp elem)
      elem
      (code-char elem)))

(defmacro tagbody-with-match-failed (elem &body body)
  (with-gensyms (block)
    `(block ,block
       (tagbody
          (return-from ,block ,@body)
        :match-failed
          (error 'match-failed :elem ,elem)))))

(defmacro parsing-macrolet ((elem data p end)
                            (&rest macros) &body body)
  `(macrolet ((advance (&optional (step 1))
                `(or (advance* ,step)
                     (go :eof)))
              (advance* (&optional (step 1))
                `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                   (incf ,',p ,step)
                   ,@(if (eql step 0)
                         ()
                         `((if (<= ,',end ,',p)
                               nil
                               (progn
                                 (setq ,',elem
                                       (aref ,',data ,',p))
                                 t))))))
              (advance-to (to)
                `(or (advance-to* ,to)
                     (go :eof)))
              (advance-to* (to)
                (once-only (to)
                  `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                     (check-type ,to fixnum)
                     (setq ,',p ,to)
                     (if (<= ,',end ,',p)
                         nil
                         (progn
                           (setq ,',elem
                                 (aref ,',data ,',p))
                           t)))))
              (skip (&rest elems)
                (check-skip-elems elems)
                `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                   (if (skip-conditions ,',elem ,elems)
                       (advance)
                       (error 'match-failed
                              :elem ,',elem
                              :expected ',elems))))
              (skip* (&rest elems)
                (check-skip-elems elems)
                `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                   (unless (eofp)
                     (loop
                       (unless (skip-conditions ,',elem ,elems)
                         (return))
                       (or (advance*) (go :eof))))))
              (skip+ (&rest elems)
                `(progn
                   (skip ,@elems)
                   (skip* ,@elems)))
              (skip? (&rest elems)
                (check-skip-elems elems)
                `(locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                   (when (skip-conditions ,',elem ,elems)
                     (or (advance*) (go :eof)))))
              (skip-until (fn)
                `(loop until ,(if (symbolp fn)
                                  `(,fn (get-elem ,',elem))
                                  `(funcall ,fn (get-elem ,',elem)))
                       do (or (advance*) (go :eof))))
              (skip-while (fn)
                `(loop while ,(if (symbolp fn)
                                  `(,fn (get-elem ,',elem))
                                  `(funcall ,fn (get-elem ,',elem)))
                       do (or (advance*) (go :eof))))
              (bind ((symb &body bind-forms) &body body)
                (with-gensyms (start)
                  `(let ((,start ,',p))
                     (tagbody
                        ,@bind-forms
                      :eof)
                     (prog1
                         (let ((,symb (subseq* ,',data ,start ,',p)))
                           ,@body)
                       (when (eofp)
                         (go :eof))))))
              (%match (&rest vectors)
                `(%match-case
                  ,@(loop for vec in vectors
                          collect `(,vec))))
              (match (&rest vectors)
                `(block match-block
                   (tagbody
                      (return-from match-block (%match ,@vectors))
                    :match-failed
                      (error 'match-failed :elem ,',elem))))
              (match? (&rest vectors)
                (with-gensyms (start start-elem)
                  `(let ((,start ,',p)
                         (,start-elem ,',elem))
                     (block match?-block
                       (tagbody
                          (%match ,@vectors)
                          (return-from match?-block t)
                        :match-failed
                          (setq ,',p ,start
                                ,',elem ,start-elem))))))
              (match-i (&rest vectors)
                `(match-i-case
                  ,@(loop for vec in vectors
                          collect `(,vec))))
              ,@macros)
     #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
     (labels ((eofp ()
                (declare (optimize (speed 3) (safety 0) (debug 0)))
                (<= ,end ,p))
              (current () (get-elem ,elem))
              (peek (&key eof-value)
                (declare (optimize (speed 3) (safety 0) (debug 0)))
                (let ((len (length ,data)))
                  (declare (type fixnum len))
                  (if (or (eofp) (>= ,p (- ,end 1)) (= ,p (- len 1)))
                      eof-value
                      (aref ,data (+ 1 ,p)))))
              (pos () (the fixnum ,p)))
       (declare (inline eofp current pos))
       ,@body)))

(defmacro with-string-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p body-block)
    (once-only (data)
      `(let ((,elem #\Nul)
             (,p ,(if start
                      `(or ,start 0)
                      0))
             (,g-end ,(if end
                          `(or ,end (length ,data))
                          `(length ,data))))
         (declare (type simple-string ,data)
                  (type fixnum ,p ,g-end)
                  (type character ,elem))
         (parsing-macrolet (,elem ,data ,p ,g-end)
             ((skip-conditions (elem-var elems)
                               `(or ,@(loop for el in elems
                                            if (and (consp el)
                                                    (eq (car el) 'not))
                                              collect `(not (char= ,(cadr el) ,elem-var))
                                            else
                                              collect `(char= ,el ,elem-var))))
              (%match-case (&rest cases)
                           (check-match-cases cases)
                           `(prog1
                                (vector-case ,',elem (,',data)
                                  ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                        cases
                                        (append cases
                                                '((otherwise (go :match-failed))))))
                              (when (eofp) (go :eof))))
              (%match-i-case (&rest cases)
                             (check-match-cases cases)
                             `(prog1
                                  (vector-case ,',elem (,',data :case-insensitive t)
                                    ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                          cases
                                          (append cases
                                                  '((otherwise (go :match-failed))))))
                                (when (eofp) (go :eof))))
              (match-case
               (&rest cases)
               `(tagbody-with-match-failed ,',elem (%match-case ,@cases)))
              (match-i-case
               (&rest cases)
               `(tagbody-with-match-failed ,',elem (%match-i-case ,@cases))))
           (block ,body-block
             (tagbody
                (when (eofp)
                  (go :eof))
                (setq ,elem (aref ,data ,p))
                (return-from ,body-block (progn ,@body))
              :eof)))))))

(defmacro with-octets-parsing ((data &key start end) &body body)
  (with-gensyms (g-end elem p body-block)
    (once-only (data)
      `(let ((,elem 0)
             (,p ,(if start
                      `(or ,start 0)
                      0))
             (,g-end ,(if end
                          `(or ,end (length ,data))
                          `(length ,data))))
         (declare (type ->u8 ,data)
                  (type fixnum ,p ,g-end)
                  (type u8 ,elem))
         (parsing-macrolet (,elem ,data ,p ,g-end)
             ((skip-conditions (elem-var elems)
                               `(or ,@(loop for el in elems
                                            if (and (consp el)
                                                    (eq (car el) 'not))
                                              collect `(not (= ,(char-code (cadr el)) ,elem-var))
                                            else
                                              collect `(= ,(char-code el) ,elem-var))))
              (%match-case (&rest cases)
                           (check-match-cases cases)
                           (setf cases
                                 (loop for case in cases
                                       if (stringp (car case))
                                         collect (cons (string-to-u8 (car case))
                                                       (cdr case))
                                       else
                                         collect case))
                           `(prog1
                                (vector-case ,',elem (,',data)
                                  ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                        cases
                                        (append cases
                                                '((otherwise (go :match-failed))))))
                              (when (eofp) (go :eof))))
              (%match-i-case (&rest cases)
                             (check-match-cases cases)
                             (setf cases
                                   (loop for case in cases
                                         if (stringp (car case))
                                           collect (cons (string-to-u8 (car case))
                                                         (cdr case))
                                         else
                                           collect case))
                             `(prog1
                                  (vector-case ,',elem (,',data :case-insensitive t)
                                    ,@(if (find 'otherwise cases :key #'car :test #'eq)
                                          cases
                                          (append cases
                                                  '((otherwise (go :match-failed))))))
                                (when (eofp) (go :eof))))
              (match-case
               (&rest cases)
               `(tagbody-with-match-failed ,',elem (%match-case ,@cases)))
              (match-i-case
               (&rest cases)
               `(tagbody-with-match-failed ,',elem (%match-i-case ,@cases))))
           (block ,body-block
             (tagbody
                (when (eofp)
                  (go :eof))
                (setq ,elem (aref ,data ,p))
                (return-from ,body-block (progn ,@body))
              :match-failed
                (error 'match-failed :elem ,elem)
              :eof)))))))

(defmacro with-vector-parsing ((data &key (start 0) end) &body body &environment env)
  (let ((data-type (variable-type* data env)))
    (case data-type
      (string `(with-string-parsing (,data :start ,start :end ,end) ,@body))
      (->u8 `(macrolet ((get-elem (form) `(code-char ,form))
                          (subseq* (data start &optional end)
                            `(u8-to-string ,data :start ,start :end ,end)))
                 (with-octets-parsing (,data :start ,start :end ,end) ,@body)))
      (otherwise (once-only (data)
                   `(etypecase ,data
                      (string (with-string-parsing (,data :start ,start :end ,end) ,@body))
                      (->u8 (macrolet ((get-elem (form) `(code-char ,form))
                                         (subseq* (data start &optional end)
                                           `(u8-to-string ,data :start ,start :end ,end)))
                                (with-octets-parsing (,data :start ,start :end ,end) ,@body)))))))))



;; Types

(deftype status-code () '(integer 0 10000))

;;
;; States

(defconstant +state-first-line+ 0)
(defconstant +state-headers+ 1)
(defconstant +state-chunk-size+ 2)
(defconstant +state-body+ 3)
(defconstant +state-chunk-body-end-crlf+ 4)
(defconstant +state-trailing-headers+ 5)

(defstruct (http (:conc-name :http-))
  (method nil :type symbol)
  (major-version 0 :type fixnum)
  (minor-version 9 :type fixnum)
  (status 0 :type status-code)
  (content-length nil :type (or null integer))
  (chunked-p nil :type boolean)
  (upgrade-p nil :type boolean)

  headers

  ;; private
  (header-read 0 :type fixnum)
  (mark -1 :type fixnum)
  (state +state-first-line+ :type fixnum))

(defun http-version (http)
  (float
   (+ (http-major-version http)
      (/ (http-minor-version http) 10))))

(defstruct (http-request (:include http)
                         (:conc-name :http-))
  resource)

(defstruct (http-response (:include http)
                          (:conc-name :http-))
  status-text)


(defconstant +cr+ (char-code #\Return))
(defconstant +lf+ (char-code #\Newline))
(defconstant +space+ (char-code #\Space))
(defconstant +tab+ (char-code #\Tab))
(defconstant +page+ (char-code #\Page))
(defconstant +dash+ #.(char-code #\-))

(define-constant +crlf+
  (make-array 2 :element-type 'u8
                :initial-contents (list +cr+ +lf+)))

(deftype simple-byte-vector (&optional (len '*))
  `(simple-array u8 (,len)))

(declaim (inline digit-byte-char-p
                 digit-byte-char-to-integer
                 alpha-byte-char-p
                 alpha-byte-char-to-lower-char
                 alphanumeric-byte-char-p
                 mark-byte-char-p))

;; FIXME repetitious

(defun digit-byte-char-p (byte)
  (declare (type u8 byte)
           (optimize (speed 3) (safety 0)))
  (<= #.(char-code #\0) byte #.(char-code #\9)))

(declaim (ftype (function (u8) fixnum) digit-byte-char-to-integer))
(defun digit-byte-char-to-integer (byte)
  (declare (type u8 byte)
           (optimize (speed 3) (safety 0)))
  (the fixnum (- byte #.(char-code #\0))))

(defun alpha-byte-char-p (byte)
  (declare (type u8 byte)
           (optimize (speed 3) (safety 0)))
  (or (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\a) byte #.(char-code #\z))))

(defun alpha-byte-char-to-lower-char (byte)
  (declare (type u8 byte)
           (optimize (speed 3) (safety 0)))
  (the character
       (cond
         ((<= #.(char-code #\A) byte #.(char-code #\Z))
          (code-char (+ byte #x20)))
         (T #+nil(<= #.(char-code #\a) byte #.(char-code #\z))
            (code-char byte)))))

(defun alphanumeric-byte-char-p (byte)
  (declare (type u8 byte))
  (or (alpha-byte-char-p byte)
      (digit-byte-char-p byte)))

(defun mark-byte-char-p (byte)
  (declare (type u8 byte)
           (optimize (speed 3) (safety 0)))
  (or (= byte #.(char-code #\-))
      (= byte #.(char-code #\_))
      (= byte #.(char-code #\.))
      (= byte #.(char-code #\!))
      (= byte #.(char-code #\~))
      (= byte #.(char-code #\*))
      (= byte #.(char-code #\'))
      (= byte #.(char-code #\())
      (= byte #.(char-code #\)))))

(declaim (ftype (function (u8) u8) byte-to-ascii-lower)
         (inline byte-to-ascii-lower))
(defun byte-to-ascii-lower (x)
  (declare (type u8 x)
           (optimize (speed 3) (safety 0)))
  (if (<= #.(char-code #\A) x #.(char-code #\Z))
      (- x #.(- (char-code #\A) (char-code #\a)))
      x))

(declaim (inline ascii-octets-to-string))
(defun ascii-octets-to-string (octets &key (start 0) (end (length octets)))
  (declare (type simple-byte-vector octets)
           (type (unsigned-byte 64) start end)
           (optimize (speed 3) (safety 0)))
  (let* ((len (the (unsigned-byte 64) (- end start)))
         (string (make-string len :element-type 'character)))
    (declare (type (unsigned-byte 64) len)
             (type simple-string string))
    (do ((i 0 (1+ i))
         (j start (1+ j)))
        ((= j end) string)
      (setf (aref string i)
            (code-char (aref octets j))))))

(declaim (inline ascii-octets-to-lower-string))
(defun ascii-octets-to-lower-string (octets &key (start 0) (end (length octets)))
  (declare (type simple-byte-vector octets)
           (type (unsigned-byte 64) start end)
           (optimize (speed 3) (safety 0)))
  (let* ((len (the (unsigned-byte 64) (- end start)))
         (string (make-string len :element-type 'character)))
    (declare (type (unsigned-byte 64) len)
             (type simple-string string))
    (do ((i 0 (1+ i))
         (j start (1+ j)))
        ((= j end) string)
      (setf (aref string i)
            (code-char (byte-to-ascii-lower (aref octets j)))))))

(defun append-byte-vectors (vec1 vec2)
  (declare (type simple-byte-vector vec1 vec2)
           (optimize (speed 3) (safety 0)))
  (let* ((vec1-len (length vec1))
         (vec2-len (length vec2))
         (result (make-array (+ vec1-len vec2-len)
                             :element-type 'u8)))
    (declare (type simple-byte-vector result))
    (replace result vec1 :start1 0)
    (replace result vec2 :start1 vec1-len)
    result))



(define-condition fast-http-error (simple-error)
  (description)
  (:report
   (lambda (condition stream)
     (format stream "~A: ~A" (type-of condition) (slot-value condition 'description)))))


;;
;; Callback-related errors

(define-condition callback-error (fast-http-error)
  ((error :initarg :error
          :initform nil))
  (:report (lambda (condition stream)
             (with-slots (description error) condition
               (format stream "Callback Error: ~A~:[~;~:*~%  ~A~]"
                       description
                       error)))))

(define-condition cb-message-begin (callback-error)
  ((description :initform "the message-begin callback failed")))
(define-condition cb-url (callback-error)
  ((description :initform "the url callback failed")))
(define-condition cb-first-line (callback-error)
  ((description :initform "the first line callback failed")))
(define-condition cb-header-field (callback-error)
  ((description :initform "the header-field callback failed")))
(define-condition cb-header-value (callback-error)
  ((description :initform "the header-value callback failed")))
(define-condition cb-headers-complete (callback-error)
  ((description :initform "the headers-complete callback failed")))
(define-condition cb-body (callback-error)
  ((description :initform "the body callback failed")))
(define-condition cb-message-complete (callback-error)
  ((description :initform "the message-complete callback failed")))
(define-condition cb-status (callback-error)
  ((description :initform "the status callback failed")))


;;
;; Parsing-related errors

(define-condition parsing-error (fast-http-error) ())

(define-condition invalid-eof-state (parsing-error)
  ((description :initform "stream ended at an unexpected time")))
(define-condition header-overflow (parsing-error)
  ((description :initform "too many header bytes seen; overflow detected")))
(define-condition closed-connection (parsing-error)
  ((description :initform "data received after completed connection: close message")))
(define-condition invalid-version (parsing-error)
  ((description :initform "invalid HTTP version")))
(define-condition invalid-status (parsing-error)
  ((description :initform "invalid HTTP status code")
   (status-code :initarg :status-code
                :initform nil))
  (:report (lambda (condition stream)
             (with-slots (description status-code) condition
               (format stream "~A: ~A~:[~;~:* (Code=~A)~]"
                       (type-of condition)
                       description
                       status-code)))))
(define-condition invalid-method (parsing-error)
  ((description :initform "invalid HTTP method")))
(define-condition invalid-url (parsing-error)
  ((description :initform "invalid URL")))
(define-condition invalid-host (parsing-error)
  ((description :initform "invalid host")))
(define-condition invalid-port (parsing-error)
  ((description :initform "invalid port")))
(define-condition invalid-path (parsing-error)
  ((description :initform "invalid path")))
(define-condition invalid-query-string (parsing-error)
  ((description :initform "invalid query string")))
(define-condition invalid-fragment (parsing-error)
  ((description :initform "invalid fragment")))
(define-condition lf-expected (parsing-error)
  ((description :initform "LF character expected")))
(define-condition invalid-header-token (parsing-error)
  ((description :initform "invalid character in header")))
(define-condition invalid-content-length (parsing-error)
  ((description :initform "invalid character in content-length header")))
(define-condition invalid-chunk-size (parsing-error)
  ((description :initform "invalid character in chunk size header")))
(define-condition invalid-constant (parsing-error)
  ((description :initform "invalid constant string")))

(define-condition invalid-internal-state (parsing-error)
  ((description :initform "encountered unexpected internal state")
   (code :initarg :code))
  (:report
   (lambda (condition stream)
     (format stream "~A: ~A (Code=~A)"
             (type-of condition)
             (slot-value condition 'description)
             (slot-value condition 'code)))))
(define-condition strict-error (parsing-error)
  ((description :initform "strict mode assertion failed")
   (form :initarg :form))
  (:report
   (lambda (condition stream)
     (format stream "~A: ~A~%  ~A"
             (type-of condition)
             (slot-value condition 'description)
             (slot-value condition 'form)))))
(define-condition paused-error (parsing-error)
  ((description :initform "parser is paused")))
(define-condition unknown-error (parsing-error)
  ((description :initform "an unknown error occured")))


;;
;; Multipart parsing

(define-condition multipart-parsing-error (fast-http-error) ())

(define-condition invalid-multipart-body (multipart-parsing-error)
  ((description :initform "invalid multipart body")))
(define-condition invalid-boundary (multipart-parsing-error)
  ((description :initform "invalid boundary")))


;;
;; Header value parsing

(define-condition header-value-parsing-error (multipart-parsing-error) ())

(define-condition invalid-header-value (header-value-parsing-error)
  ((description :initform "invalid header value")))
(define-condition invalid-parameter-key (header-value-parsing-error)
  ((description :initform "invalid parameter key")))
(define-condition invalid-parameter-value (header-value-parsing-error)
  ((description :initform "invalid parameter value")))


(defmacro casev (keyform &body clauses)
  (once-only (keyform)
    (flet ((get-val (val)
             (cond
               ((eq val 'otherwise) val)
               ((symbolp val) (symbol-value val))
               ((constantp val) val)
               (T (error "CASEV can be used only with variables or constants")))))
      `(case ,keyform
         ,@(loop for (val . clause) in clauses
                 if (eq val 'otherwise)
                   collect `(otherwise ,@clause)
                 else if (listp val)
                   collect `((,@(mapcar #'get-val val)) ,@clause)
                 else
                   collect `(,(get-val val) ,@clause))))))

(defmacro casev= (keyform &body clauses)
  (once-only (keyform)
    (flet ((get-val (val)
             (cond
               ((eq val 'otherwise) val)
               ((symbolp val) (symbol-value val))
               ((constantp val) val)
               (T (error "CASEV can be used only with variables or constants")))))
      `(cond
         ,@(loop for (val . clause) in clauses
                 if (eq val 'otherwise)
                   collect `(T ,@clause)
                 else if (listp val)
                        collect `((or ,@(mapcar (lambda (val)
                                                  `(= ,keyform ,(get-val val)))
                                                val))
                                  ,@clause)
                 else
                   collect `((= ,keyform ,(get-val val)) ,@clause))))))

(defmacro case-byte (byte &body cases)
  `(casev= ,byte
     ,@(loop for (val . form) in cases
             if (eq val 'otherwise)
               collect `(,val ,@form)
             else if (listp val)
               collect `(,(mapcar #'char-code val) ,@form)
             else
               collect `(,(char-code val) ,@form))))

(defmacro tagcase (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (case ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                    collect `(otherwise ,@body (go ,end))
                  else
                    collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                  append tag
                else
                  collect tag
                collect `(progn ,@body
                                (go ,end)))
      ,end)))

(defmacro tagcasev (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (casev ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                    collect `(otherwise ,@body (go ,end))
                  else
                    collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                  append tag
                else if (not (eq tag 'otherwise))
                       collect tag
                collect `(progn ,@body
                                (go ,end)))
      ,end)))

(defmacro tagcasev= (keyform &body blocks)
  (let ((end (gensym "END")))
    `(tagbody
        (casev= ,keyform
          ,@(loop for (tag . body) in blocks
                  if (eq tag 'otherwise)
                    collect `(otherwise ,@body (go ,end))
                  else
                    collect `(,tag (go ,(if (listp tag) (car tag) tag)))))
        (go ,end)
        ,@(loop for (tag . body) in blocks
                if (listp tag)
                  append tag
                else if (not (eq tag 'otherwise))
                       collect tag
                collect `(progn ,@body
                                (go ,end)))
      ,end)))

(defun make-collector ()
  (let ((none '#:none))
    (declare (dynamic-extent none))
    (with-collectors (buffer)
      (return-from make-collector
        (lambda (&optional (data none))
          (unless (eq data none)
            (buffer data))
          buffer)))))

(declaim (inline %whitespacep))
(defun %whitespacep (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0)))
  (or (char= char #\Space)
      (char= char #\Tab)))

(declaim (inline position-not-whitespace))
(defun position-not-whitespace (string &key from-end)
  (declare (type #+ecl string #-ecl simple-string string)
           (optimize (speed 3) (safety 0)))
  (let* ((len (length string))
         (start (if from-end (1- len) 0))
         (end (if from-end 0 (1- len)))
         (step-fn (if from-end #'1- #'1+)))
    (declare (type integer len start end))
    (do ((i start (funcall step-fn i)))
        ((= i end) i)
      (declare (type integer i))
      (unless (%whitespacep (aref string i))
        (return-from position-not-whitespace i)))))

(declaim (inline number-string-p))
(defun number-string-p (string)
  (declare (type #+ecl string #-ecl simple-string string)
           (optimize (speed 3) (safety 2)))
  ;; empty string
  (when (zerop (length string))
    (return-from number-string-p nil))
  (let ((end (position-not-whitespace string :from-end t))
        (dot-read-p nil))
    ;; spaces string
    (when (null end)
      (return-from number-string-p nil))
    (locally (declare (type integer end)
                      (optimize (safety 0)))
      (incf end)
      (do ((i (the integer (or (position-not-whitespace string) 0)) (1+ i)))
          ((= i end) T)
        (declare (type integer i))
        (let ((char (aref string i)))
          (declare (type character char))
          (cond
            ((alpha-char-p char)
             (return-from number-string-p nil))
            ((digit-char-p char))
            ((char= char #\.)
             (when dot-read-p
               (return-from number-string-p nil))
             (setq dot-read-p t))
            (T (return-from number-string-p nil))))))))

;;
;; Variables

(declaim (type fixnum +max-header-line+))
(defconstant +max-header-line+ 1024
  "Maximum number of header lines allowed.

This restriction is for protecting users' application
against denial-of-service attacks where the attacker feeds
us a never-ending header that the application keeps buffering.")


;;
;; Types

(deftype pointer () 'integer)


;;
;; Callbacks

(defstruct callbacks
  (message-begin nil :type (or null function))     ;; 1 arg
  (url nil :type (or null function))
  (first-line nil :type (or null function))
  (status nil :type (or null function))
  (header-field nil :type (or null function))
  (header-value nil :type (or null function))
  (headers-complete nil :type (or null function))  ;; 1 arg
  (body nil :type (or null function))
  (message-complete nil :type (or null function)))

(defmacro callback-data (name http callbacks data start end)
  (with-gensyms (callback e)
    `(when-let (,callback (,(format-symbol t "~A-~A" :callbacks name) ,callbacks))
       (handler-bind ((error
                        (lambda (,e)
                          (unless (typep ,e 'fast-http-error)
                            (error ',(format-symbol t "~A-~A" :cb name)
                                   :error ,e)
                            (abort ,e)))))
         (funcall ,callback ,http ,data ,start ,end)))))

(defmacro callback-notify (name http callbacks)
  (with-gensyms (callback e)
    `(when-let (,callback (,(format-symbol t "~A-~A" :callbacks name) ,callbacks))
       (handler-bind ((error
                        (lambda (,e)
                          (unless (typep ,e 'fast-http-error)
                            (error ',(format-symbol t "~A-~A" :cb name)
                                   :error ,e)
                            (abort ,e)))))
         (funcall ,callback ,http)))))


;;
;; Parser utilities

(define-condition eof () ())

(define-condition expect-failed (parsing-error)
  ((description :initform "expect failed")))


;;
;; Tokens

(declaim (type (simple-array character (128)) +tokens+))
(define-constant +tokens+
    (make-array 128
                :element-type 'character
                :initial-contents
                '( #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
                   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
                   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
                   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
                   #\Nul   #\!   #\Nul   #\#    #\$    #\%    #\&    #\'
                   #\Nul  #\Nul   #\*    #\+   #\Nul    #\-   #\.   #\Nul
                   #\0    #\1    #\2    #\3    #\4    #\5    #\6    #\7
                   #\8    #\9   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
                   #\Nul   #\a    #\b    #\c    #\d    #\e    #\f    #\g
                   #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
                   #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
                   #\x    #\y    #\z   #\Nul  #\Nul  #\Nul   #\^    #\_
                   #\`    #\a    #\b    #\c    #\d    #\e    #\f    #\g
                   #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
                   #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
                   #\x    #\y    #\z   #\Nul   #\|   #\Nul   #\~   #\Nul ))
)

(declaim (type (simple-array fixnum (128)) +unhex+))
(define-constant +unhex+
    (make-array 128 :element-type 'fixnum :initial-contents
                '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  0  1  2  3  4  5  6  7  8  9 -1 -1 -1 -1 -1 -1
                  -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
                  -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))
)

(defun unhex-byte (byte)
  (aref +unhex+ byte))

;;
;; Main

(defun parse-method (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (with-octets-parsing (data :start start :end end)
    (return-from parse-method
      (values
       (prog1
           (match-case
            ("CONNECT"     :CONNECT)
            ("COPY"        :COPY)
            ("CHECKOUT"    :CHECKOUT)
            ("DELETE"      :DELETE)
            ("GET"         :GET)
            ("HEAD"        :HEAD)
            ("LOCK"        :LOCK)
            ("MKCOL"       :MKCOL)
            ("MKCALENDAR"  :MKCALENDAR)
            ("MKACTIVITY"  :MKACTIVITY)
            ("MOVE"        :MOVE)
            ("MERGE"       :MERGE)
            ("M-SEARCH"    :M-SEARCH)
            ("NOTIFY"      :NOTIFY)
            ("OPTIONS"     :OPTIONS)
            ("POST"        :POST)
            ("PROPFIND"    :PROPFIND)
            ("PROPPATCH"   :PROPPATCH)
            ("PUT"         :PUT)
            ("PURGE"       :PURGE)
            ("PATCH"       :PATCH)
            ("REPORT"      :REPORT)
            ("SEARCH"      :SEARCH)
            ("SUBSCRIBE"   :SUBSCRIBE)
            ("TRACE"       :TRACE)
            ("UNLOCK"      :UNLOCK)
            ("UNSUBSCRIBE" :UNSUBSCRIBE)
            (otherwise (error 'invalid-method)))
         (unless (= (current) +space+)
           (error 'invalid-method)))
       (pos))))
  (error 'eof))

(defun parse-url (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (flet ((url-char-byte-p (byte)
           (or (<= (char-code #\!) byte (char-code #\~))
               (<= 128 byte))))
    (with-octets-parsing (data :start start :end end)
      (skip-while url-char-byte-p)
      (return-from parse-url (pos)))
    (error 'eof)))

(defun parse-http-version (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let (major minor)
    (with-octets-parsing (data :start start :end end)
      (or (match? "HTTP/")
          (return-from parse-http-version (values nil nil (pos))))
      (if (digit-byte-char-p (current))
          (setq major (digit-byte-char-to-integer (current)))
          (return-from parse-http-version (values nil nil (pos))))
      (advance)
      (or (skip? #\.) (return-from parse-http-version (values nil nil (pos))))
      (if (digit-byte-char-p (current))
          (setq minor (digit-byte-char-to-integer (current)))
          (return-from parse-http-version (values nil nil (pos))))
      (advance)
      (return-from parse-http-version
        (values major minor (pos))))
    (error 'eof)))

(defun parse-status-code (http callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (or (with-octets-parsing (data :start start :end end)
        (if (digit-byte-char-p (current))
            (setf (http-status http) (digit-byte-char-to-integer (current)))
            (error 'invalid-status))
        (loop
          (advance)
          (cond
            ((digit-byte-char-p (current))
             (setf (http-status http)
                   (+ (the fixnum (* 10 (http-status http)))
                      (digit-byte-char-to-integer (current))))
             (when (< 999 (http-status http))
               (error 'invalid-status :status-code (http-status http))))
            ((= (current) +space+)
             ;; Reading the status text
             (advance)
             (let ((status-text-start (pos)))
               (skip* (not #\Return))
               (advance)
               (skip #\Newline)
               (callback-data :status http callbacks data status-text-start (- (pos) 1)))
             (return))
            ((= (current) +cr+)
             ;; No status text
             (advance)
             (skip #\Newline)
             (return))
            (T (error 'invalid-status))))
        (pos))
      (error 'eof)))

(defun parse-header-field-and-value (http callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (or
   (with-octets-parsing (data :start start :end end)
     (let ((field-start (pos))
           field-end)
       (macrolet ((skip-until-value-start-and (&body body)
                    `(progn
                       ;; skip #\: and leading spaces
                       (skip #\:)
                       (skip* #\Space #\Tab)
                       (cond
                         ((= (current) +cr+)
                          ;; continue to the next line
                          (advance)
                          (skip #\Newline)
                          (cond
                            ((or (= (current) +space+)
                                 (= (current) +tab+))
                             (skip* #\Space #\Tab)
                             (if (= (current) +cr+)
                                 ;; empty body
                                 (progn
                                   (advance)
                                   (skip #\Newline)
                                   (callback-data :header-field http callbacks data field-start field-end)
                                   (callback-data :header-value http callbacks data (pos) (pos)))
                                 (progn ,@body)))
                            ;; empty body
                            (t
                             (callback-data :header-field http callbacks data field-start field-end)
                             (callback-data :header-value http callbacks data (pos) (pos)))))
                         (t ,@body))))
                  (handle-otherwise ()
                    `(progn
                       ;; skip until field end
                       (do ((char (aref +tokens+ (current))
                                  (aref +tokens+ (current))))
                           ((= (current) (char-code #\:)))
                         (declare (type character char))
                         (when (char= char #\Nul)
                           (error 'invalid-header-token))
                         (advance))

                       (setq field-end (pos))
                       (skip-until-value-start-and
                        (advance-to*
                         (parse-header-value http callbacks data (pos) end field-start field-end)))))
                  (expect-field-end (&body body)
                    `(if (= (current) #.(char-code #\:))
                         (progn
                           (setq field-end (pos))
                           ,@body)
                         (handle-otherwise))))
         (match-i-case
          ("content-length"
           (expect-field-end
            (skip-until-value-start-and
             (multiple-value-bind (value-start value-end next content-length)
                 (parse-header-value-content-length data (pos) end)
               (declare (type pointer next))
               (setf (http-content-length http) content-length)
               (advance-to* next)
               (callback-data :header-field http callbacks data field-start field-end)
               (callback-data :header-value http callbacks data value-start value-end)))))
          ("transfer-encoding"
           (expect-field-end
            (skip-until-value-start-and
             (multiple-value-bind (value-start value-end next chunkedp)
                 (parse-header-value-transfer-encoding data (pos) end)
               (declare (type pointer next))
               (setf (http-chunked-p http) chunkedp)
               (advance-to* next)
               (callback-data :header-field http callbacks data field-start field-end)
               (callback-data :header-value http callbacks data value-start value-end)))))
          ("upgrade"
           (expect-field-end
            (skip-until-value-start-and
             (setf (http-upgrade-p http) T)
             (let ((value-start (pos)))
               (skip* (not #\Return))
               (advance)
               (skip #\Newline)
               (callback-data :header-field http callbacks data field-start field-end)
               (callback-data :header-value http callbacks data value-start (- (pos) 2))))))
          (otherwise (handle-otherwise)))))
     (pos))
   (error 'eof)))

(defun parse-header-value (http callbacks data start end &optional field-start field-end)
  (or (with-octets-parsing (data :start start :end end)
        (skip* (not #\Return))
        (advance)
        (skip #\Newline)
        (when field-start
          (callback-data :header-field http callbacks data field-start field-end))
        (callback-data :header-value http callbacks data start (- (pos) 2))
        (pos))
      (error 'eof)))

(defun parse-header-value-transfer-encoding (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (with-octets-parsing (data :start start :end end)
    (match-i-case
     ("chunked"
      (if (= (current) +cr+)
          (progn
            (advance)
            (skip #\Newline)
            (return-from parse-header-value-transfer-encoding
              (values start (- (pos) 2) (pos) t)))
          (progn
            (skip+ (not #\Return))
            (advance)
            (skip #\Newline)
            (return-from parse-header-value-transfer-encoding
              (values start (- (pos) 2) (pos) nil)))))
     (otherwise
      (skip* (not #\Return))
      (advance)
      (skip #\Newline)
      (return-from parse-header-value-transfer-encoding
        (values start (- (pos) 2) (pos) nil)))))
  (error 'eof))

(defun parse-header-value-content-length (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let ((content-length 0))
    (declare (type integer content-length))
    (with-octets-parsing (data :start start :end end)
      (if (digit-byte-char-p (current))
          (setq content-length (digit-byte-char-to-integer (current)))
          (error 'invalid-content-length))
      (loop
        (advance)
        (cond
          ((digit-byte-char-p (current))
           (setq content-length
                 (+ (* 10 content-length)
                    (digit-byte-char-to-integer (current)))))
          ((= (current) +cr+)
           (advance)
           (skip #\Newline)
           (return-from parse-header-value-content-length
             (values start (- (pos) 2) (pos) content-length)))
          ((= (current) +space+)
           ;; Discard spaces
           )
          (t (error 'invalid-content-length)))))
    (error 'eof)))

(defun parse-header-line (http callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (when (<= end start)
    (error 'eof))
  (let ((current (aref data start)))
    (declare (type u8 current))
    (cond
      ((or (= current +tab+)
           (= current +space+))
       (parse-header-value http callbacks data start end))
      ((/= current +cr+)
       (parse-header-field-and-value http callbacks data start end))
      (t
       (incf start)
       (when (= start end)
         (error 'eof))
       (setq current (aref data start))
       (unless (= current +lf+)
         (error 'expect-failed))
       (values (1+ start) t)))))

(defun parse-headers (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (or (with-octets-parsing (data :start start :end end)
        ;; empty headers
        (when (= (current) +cr+)
          (advance)
          (if (= (current) +lf+)
              (return-from parse-headers (1+ (pos)))
              (error 'expect-failed)))

        (advance-to* (parse-header-field-and-value http callbacks data start end))

        (setf (http-mark http) (pos))
        (loop
          (when (= +max-header-line+ (the fixnum (incf (http-header-read http))))
            (error 'header-overflow))
          (multiple-value-bind (next endp)
              (parse-header-line http callbacks data (pos) end)
            (advance-to* next)
            (when endp
              (return)))
          (setf (http-mark http) (pos)))
        (setf (http-mark http) (pos))
        (setf (http-state http) +state-body+)

        (pos))
      (error 'eof)))

(defun read-body-data (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (let ((readable-count (the pointer (- end start))))
    (declare (dynamic-extent readable-count)
             (type pointer readable-count))
    (if (<= (http-content-length http) readable-count)
        (let ((body-end (+ start (http-content-length http))))
          (declare (dynamic-extent body-end))
          (setf (http-content-length http) 0)
          (callback-data :body http callbacks data start body-end)
          (setf (http-mark http) body-end)
          (values body-end t))
        ;; still needs to read
        (progn
          (decf (http-content-length http) readable-count)
          (callback-data :body http callbacks data start end)
          (setf (http-mark http) end)
          (values end nil)))))

(defun http-message-needs-eof-p (http)
  (let ((status-code (http-status http)))
    (declare (type status-code status-code))
    (when (= status-code 0) ;; probably request
      (return-from http-message-needs-eof-p nil))

    (when (or (< 99 status-code 200) ;; 1xx e.g. Continue
              (= status-code 204)    ;; No Content
              (= status-code 304))   ;; Not Modified
      (return-from http-message-needs-eof-p nil))

    (when (or (http-chunked-p http)
              (http-content-length http))
      (return-from http-message-needs-eof-p nil))
    T))

(defun parse-http-body (http callbacks data start end requestp)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (macrolet ((message-complete ()
               `(progn
                  (callback-notify :message-complete http callbacks)
                  (setf (http-state http) +state-first-line+))))
    (case (http-content-length http)
      (0
       ;; Content-Length header given but zero: Content-Length: 0\r\n
       (message-complete)
       start)
      ('nil
       (if (or requestp
               (not (http-message-needs-eof-p http)))
           ;; Assume content-length 0 - read the next
           (progn
             (message-complete)
             ;; By returning "start", we'll continue
             ;; to parse the next request in case if
             ;; HTTP pipelining is used. Probably
             ;; we need some way to enable (or disable)
             ;; HTTP pipelining support.
             start)
           ;; read until EOF
           (progn
             (callback-data :body http callbacks data start end)
             (setf (http-mark http) end)
             end)))
      (otherwise
       ;; Content-Length header given and non-zero
       (multiple-value-bind (next completedp)
           (read-body-data http callbacks data start end)
         (when completedp
           (message-complete))
         next)))))

(defun parse-chunked-body (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))

  (when (= start end)
    (return-from parse-chunked-body start))

  (or (with-octets-parsing (data :start start :end end)
        (tagbody
           (cond
             ((= (http-state http) +state-chunk-size+)
              (go chunk-size))
             ((= (http-state http) +state-body+)
              (go body))
             ((= (http-state http) +state-chunk-body-end-crlf+)
              (go body-end-crlf))
             ((= (http-state http) +state-trailing-headers+)
              (go trailing-headers))
             (T (error 'invalid-internal-state :code (http-state http))))

         chunk-size
           (let ((unhex-val (unhex-byte (current))))
             (declare (type fixnum unhex-val)
                      (dynamic-extent unhex-val))
             (when (= unhex-val -1)
               (error 'invalid-chunk-size))
             (setf (http-content-length http) unhex-val)

             (loop
               (advance)
               (if (= (current) +cr+)
                   (progn
                     (advance)
                     (tagbody
                        (skip #\Newline)
                      :eof
                        (return)))
                   (progn
                     (setq unhex-val (unhex-byte (current)))
                     (cond
                       ((= unhex-val -1)
                        (cond
                          ((or (= (current) (char-code #\;))
                               (= (current) (char-code #\Space)))
                           (skip* (not #\Return))
                           (advance)
                           (tagbody
                              (skip #\Newline)
                            :eof
                              (return)))
                          (t (error 'invalid-chunk-size))))
                       (t (setf (http-content-length http)
                                (+ (* 16 (http-content-length http)) unhex-val)))))))
             (setf (http-state http) +state-body+) 
             (if (eofp)
                 (return-from parse-chunked-body (pos))
                 (setf (http-mark http) (pos))))

         body
           (cond
             ((zerop (http-content-length http))
              ;; trailing headers
              (setf (http-state http) +state-trailing-headers+)
              (go trailing-headers))
             (T
              (multiple-value-bind (next completedp)
                  (read-body-data http callbacks data (pos) end)
                (declare (type pointer next))
                (unless completedp
                  (return-from parse-chunked-body (pos)))
                (setf (http-state http) +state-chunk-body-end-crlf+)
                (advance-to next))))

         body-end-crlf
           (skip #\Return)
           (tagbody
              (skip #\Newline)
            :eof
              (setf (http-state http) +state-chunk-size+)
              (when (eofp)
                (return-from parse-chunked-body (pos))))
           (setf (http-mark http) (pos))
           (go chunk-size)

         trailing-headers
           (return-from parse-chunked-body
             (prog1 (parse-headers http callbacks data (pos) end)
               (callback-notify :message-complete http callbacks)))))
      (error 'eof)))

(defun parse-request (http callbacks data &key (start 0) end)
  (declare (type http http)
           (type simple-byte-vector data))
  (let ((end (or end (length data))))
    (declare (type pointer start end))
    (handler-bind ((match-failed
                     (lambda (c)
                       (declare (ignore c))
                       (error 'expect-failed))))
      (with-octets-parsing (data :start start :end end)
        (setf (http-mark http) start)

        (tagbody
           (let ((state (http-state http)))
             (declare (type fixnum state))
             (cond
               ((= +state-first-line+ state)
                (go first-line))
               ((= +state-headers+ state)
                (go headers))
               ((<= +state-chunk-size+ state +state-trailing-headers+)
                (go body))
               (T (error 'invalid-internal-state :code state))))

         first-line
           ;; skip first empty line (some clients add CRLF after POST content)
           (when (= (current) +cr+)
             (advance)
             (tagbody
                (skip #\Newline)
              :eof
                (when (eofp)
                  (return-from parse-request (pos)))))

           (setf (http-mark http) (pos))
           (callback-notify :message-begin http callbacks)

           (multiple-value-bind (method next)
               (parse-method data (pos) end)
             (declare (type pointer next))
             (setf (http-method http) method)
             (advance-to* next))
           (skip* #\Space)
           (let ((url-start-mark (pos))
                 (url-end-mark (parse-url data (pos) end)))
             (declare (type pointer url-start-mark url-end-mark))
             (tagbody retry-url-parse
                (advance-to* url-end-mark)

                (skip* #\Space)

                (cond
                  ;; No HTTP version
                  ((= (current) +cr+)
                   (callback-data :url http callbacks data url-start-mark url-end-mark)
                   (advance)
                   (skip #\Newline))
                  (T (multiple-value-bind (major minor next)
                         (parse-http-version data (pos) end)
                       (declare (type pointer next))
                       (unless major
                         ;; Invalid HTTP version.
                         ;; Assuming it's also a part of URI.
                         (setq url-end-mark (parse-url data next end))
                         (go retry-url-parse))
                       (callback-data :url http callbacks data url-start-mark url-end-mark)
                       (setf (http-major-version http) major
                             (http-minor-version http) minor)
                       (advance-to* next))
                     (skip #\Return)
                     (skip #\Newline)))))

           (setf (http-mark http) (pos))
           (setf (http-state http) +state-headers+)
           (callback-notify :first-line http callbacks)

         headers
           (advance-to* (parse-headers http callbacks data (pos) end))

           (callback-notify :headers-complete http callbacks)
           (setf (http-header-read http) 0)

           ;; Exit, the rest of the connect is in a different protocol.
           (when (http-upgrade-p http)
             (setf (http-state http) +state-first-line+)
             (callback-notify :message-complete http callbacks)
             (return-from parse-request (pos)))

           (setf (http-state http)
                 (if (http-chunked-p http)
                     +state-chunk-size+
                     +state-body+))

         body
           (if (http-chunked-p http)
               (advance-to* (parse-chunked-body http callbacks data (pos) end))
               (progn
                 (and (advance-to* (parse-http-body http callbacks data (pos) end t))
                      (go first-line))))
           (return-from parse-request (pos)))))
    (error 'eof)))

(defun parse-response (http callbacks data &key (start 0) end)
  (declare (type http http)
           (type simple-byte-vector data))
  (let ((end (or end
                 (length data))))
    (declare (type pointer start end))
    (handler-bind ((match-failed
                     (lambda (c)
                       (declare (ignore c))
                       (error 'expect-failed))))
      (with-octets-parsing (data :start start :end end)
        (setf (http-mark http) start)

        (tagbody
           (let ((state (http-state http)))
             (declare (type fixnum state))
             (cond
               ((= +state-first-line+ state)
                (go first-line))
               ((= +state-headers+ state)
                (go headers))
               ((<= +state-chunk-size+ state +state-trailing-headers+)
                (go body))
               (T (error 'invalid-internal-state :code state))))

         first-line
           (setf (http-mark http) (pos))
           (callback-notify :message-begin http callbacks)

           (multiple-value-bind (major minor next)
               (parse-http-version data (pos) end)
             (declare (type pointer next))
             (setf (http-major-version http) major
                   (http-minor-version http) minor)
             (advance-to* next))

           (cond
             ((= (current) +space+)
              (advance)
              (advance-to (parse-status-code http callbacks data (pos) end)))
             ((= (current) +cr+)
              (skip #\Newline))
             (T (error 'invalid-version)))

           (setf (http-mark http) (pos))
           (setf (http-state http) +state-headers+)
           (callback-notify :first-line http callbacks)

         headers
           (advance-to* (parse-headers http callbacks data (pos) end))

           (callback-notify :headers-complete http callbacks)
           (setf (http-header-read http) 0)
           (setf (http-state http)
                 (if (http-chunked-p http)
                     +state-chunk-size+
                     +state-body+))

         body
           (if (http-chunked-p http)
               (advance-to* (parse-chunked-body http callbacks data (pos) end))
               (progn
                 (advance-to* (parse-http-body http callbacks data (pos) end nil))
                 (unless (eofp)
                   (go first-line))))
           (return-from parse-response (pos)))))
    (error 'eof)))

(defun parse-header-value-parameters (data &key
                                             header-value-callback
                                             header-parameter-key-callback
                                             header-parameter-value-callback)
  (declare (type simple-string data)
           (optimize (speed 3) (safety 2)))

  (let* ((header-name-mark 0)
         parameter-key-mark
         parameter-value-mark
         parsing-quoted-string-p
         (p 0)
         (end (length data))
         (char (aref data p)))
    (declare (type character char))

    (when (= end 0)
      (return-from parse-header-value-parameters 0))

    (macrolet ((go-state (state &optional (advance 1))
                   `(locally (declare (optimize (speed 3) (safety 0)))
                      (incf p ,advance)
                      (when (= p end)
                        (go eof))
                      (setq char (aref data p))
                      (go ,state))))
      (flet ((tokenp (char)
               (declare (optimize (speed 3) (safety 0)))
               (let ((byte (char-code char)))
                 (and (< byte 128)
                      (not (char= (the character (aref +tokens+ byte)) #\Nul))))))
        (tagbody
         parsing-header-value-start
           (case char
             ((#\Space #\Tab)
              (go-state parsing-header-value))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-header-value))
              (setq header-name-mark p)
              (go-state parsing-header-value 0)))

         parsing-header-value
           (case char
             (#\;
              (when header-value-callback
                (funcall (the function header-value-callback)
                         data header-name-mark p))
              (setq header-name-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise (go-state parsing-header-value)))

         looking-for-parameter-key
           (case char
             ((#\Space #\Tab #\; #\Newline #\Return)
              (go-state looking-for-parameter-key))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (setq parameter-key-mark p)
              (go-state parsing-parameter-key)))

         parsing-parameter-key
           (case char
             (#\=
              (assert parameter-key-mark)
              (when header-parameter-key-callback
                (funcall (the function header-parameter-key-callback)
                         data parameter-key-mark p))
              (setq parameter-key-mark nil)
              (go-state parsing-parameter-value-start))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (go-state parsing-parameter-key)))

         parsing-parameter-value-start
           (case char
             (#\"
              ;; quoted-string
              (setq parameter-value-mark (1+ p))
              (setq parsing-quoted-string-p t)
              (go-state parsing-parameter-quoted-value))
             ((#.+space+ #.+tab+)
              (go-state parsing-parameter-value-start))
             (otherwise
              (setq parameter-value-mark p)
              (go-state parsing-parameter-value 0)))

         parsing-parameter-quoted-value
           (if (char= char #\")
               (progn
                 (assert parameter-value-mark)
                 (setq parsing-quoted-string-p nil)
                 (when header-parameter-value-callback
                   (funcall (the function header-parameter-value-callback)
                            data parameter-value-mark p))
                 (setq parameter-value-mark nil)
                 (go-state looking-for-parameter-key))
               (go-state parsing-parameter-quoted-value))

         parsing-parameter-value
           (case char
             (#\;
              (assert parameter-value-mark)
              (when header-parameter-value-callback
                (funcall (the function header-parameter-value-callback)
                         data parameter-value-mark p))
              (setq parameter-value-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise
              (go-state parsing-parameter-value)))

         eof
           (when header-name-mark
             (when header-value-callback
               (funcall (the function header-value-callback)
                        data header-name-mark p)))
           (when parameter-key-mark
             (error 'invalid-eof-state))
           (when parameter-value-mark
             (when parsing-quoted-string-p
               (error 'invalid-eof-state))
             (when header-parameter-value-callback
               (funcall (the function header-parameter-value-callback)
                        data parameter-value-mark p))))))
    p))


(defstruct (ll-multipart-parser (:constructor make-ll-multipart-parser
                                  (&key boundary
                                   &aux (header-parser
                                         (let ((parser (make-http)))
                                           (setf (http-state parser) +state-headers+)
                                           parser)))))
  (state 0 :type fixnum)
  (header-parser)
  boundary
  body-mark
  body-buffer
  boundary-mark
  boundary-buffer)

#.`(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for i from 0
             for state in '(parsing-delimiter-dash-start
                            parsing-delimiter-dash
                            parsing-delimiter
                            parsing-delimiter-end
                            parsing-delimiter-almost-done
                            parsing-delimiter-done
                            header-field-start
                            body-start
                            looking-for-delimiter
                            maybe-delimiter-start
                            maybe-delimiter-first-dash
                            maybe-delimiter-second-dash
                            body-almost-done
                            body-done)
             collect `(defconstant ,(format-symbol t "+~A+" state) ,i)))

(defun http-multipart-parse (parser callbacks data &key (start 0) end)
  (declare (type simple-byte-vector data))
  (let* ((end (or end (length data)))
         (boundary (map '(simple-array u8 (*)) #'char-code (ll-multipart-parser-boundary parser)))
         (boundary-length (length boundary))
         (header-parser (ll-multipart-parser-header-parser parser)))
    (declare (type simple-byte-vector boundary))
    (when (= start end)
      (return-from http-multipart-parse start))

    (macrolet ((with-body-cb (callback &body body)
                 `(handler-case (when-let (,callback (callbacks-body callbacks))
                                  ,@body)
                    (error (e)
                      (error 'cb-body :error e))))
               (call-body-cb (&optional (end '(ll-multipart-parser-boundary-mark parser)))
                 (let ((g-end (gensym "END")))
                   `(with-body-cb callback
                      (when (ll-multipart-parser-body-buffer parser)
                        (funcall callback parser
                                 (ll-multipart-parser-body-buffer parser)
                                 0 (length (ll-multipart-parser-body-buffer parser)))
                        (setf (ll-multipart-parser-body-buffer parser) nil))
                      (when-let (,g-end ,end)
                        (funcall callback parser data
                                 (ll-multipart-parser-body-mark parser)
                                 ,g-end)))))
               (flush-boundary-buffer ()
                 `(with-body-cb callback
                    (when (ll-multipart-parser-boundary-buffer parser)
                      (funcall callback parser
                               (ll-multipart-parser-boundary-buffer parser)
                               0 (length (ll-multipart-parser-boundary-buffer parser)))
                      (setf (ll-multipart-parser-boundary-buffer parser) nil)))))
      (let* ((p start)
             (byte (aref data p)))
        #+fast-http-debug
        (log:debug (code-char byte))
        (tagbody
           (macrolet ((go-state (tag &optional (advance 1))
                          `(progn
                             ,(case advance
                                (0 ())
                                (1 '(incf p))
                                (otherwise `(incf p ,advance)))
                             (setf (ll-multipart-parser-state parser) ,tag)
                             #+fast-http-debug
                             (log:debug ,(princ-to-string tag))
                             ,@(and (not (eql advance 0))
                                    `((when (= p end)
                                        (go exit-loop))
                                      (setq byte (aref data p))
                                      #+fast-http-debug
                                      (log:debug (code-char byte))))
                             (go ,tag))))
             (tagcasev (ll-multipart-parser-state parser)
               (+parsing-delimiter-dash-start+
                (unless (= byte +dash+)
                  (go-state +header-field-start+ 0))
                (go-state +parsing-delimiter-dash+))

               (+parsing-delimiter-dash+
                (unless (= byte +dash+)
                  (error 'invalid-multipart-body))
                (go-state +parsing-delimiter+))

               (+parsing-delimiter+
                (let ((end2 (+ p boundary-length)))
                  (cond
                    ((ll-multipart-parser-boundary-buffer parser)
                     (when (< (+ end (length (ll-multipart-parser-boundary-buffer parser)) -3) end2)
                       (setf (ll-multipart-parser-boundary-buffer parser)
                             (concatenate 'simple-byte-vector
                                          (ll-multipart-parser-boundary-buffer parser)
                                          data))
                       (go exit-loop))
                     (let ((data2 (make-array boundary-length :element-type 'u8))
                           (boundary-buffer-length (length (ll-multipart-parser-boundary-buffer parser))))
                       (replace data2 (ll-multipart-parser-boundary-buffer parser)
                                :start2 2)
                       (replace data2 data
                                :start1 (- boundary-buffer-length 2))
                       (unless (search boundary data2)
                         ;; Still in the body
                         (when (ll-multipart-parser-body-mark parser)
                           (call-body-cb nil)
                           (flush-boundary-buffer)
                           (go-state +looking-for-delimiter+))
                         (error 'invalid-boundary))
                       (go-state +parsing-delimiter-end+ (- boundary-length boundary-buffer-length -2))))
                    ((< (1- end) end2)
                     ;; EOF
                     (setf (ll-multipart-parser-boundary-buffer parser)
                           (if (ll-multipart-parser-boundary-buffer parser)
                               (concatenate 'simple-byte-vector
                                            (ll-multipart-parser-boundary-buffer parser)
                                            (subseq data (max 0 (- p 2))))
                               (subseq data (max 0 (- p 2)))))
                     (go exit-loop))
                    (T
                     (unless (search boundary data :start2 p :end2 end2)
                       ;; Still in the body
                       (when (ll-multipart-parser-body-mark parser)
                         (go-state +looking-for-delimiter+))
                       (error 'invalid-boundary))
                     (go-state +parsing-delimiter-end+ boundary-length)))))

               (+parsing-delimiter-end+
                (casev byte
                  (+cr+ (go-state +parsing-delimiter-almost-done+))
                  (+lf+ (go-state +parsing-delimiter-almost-done+ 0))
                  (+dash+ (go-state +body-almost-done+))
                  (otherwise
                   ;; Still in the body
                   (when (ll-multipart-parser-body-mark parser)
                     (call-body-cb nil)
                     (flush-boundary-buffer)
                     (go-state +looking-for-delimiter+))
                   (error 'invalid-boundary))))

               (+parsing-delimiter-almost-done+
                (unless (= byte +lf+)
                  (error 'invalid-boundary))
                (when (ll-multipart-parser-body-mark parser)
                  ;; got a part
                  (when (ll-multipart-parser-boundary-mark parser)
                    (call-body-cb))
                  (when-let (callback (callbacks-message-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-message-complete :error e)))))
                (go-state +parsing-delimiter-done+))

               (+parsing-delimiter-done+
                (when-let (callback (callbacks-message-begin callbacks))
                  (handler-case (funcall callback parser)
                    (error (e)
                      (error 'cb-message-begin :error e))))
                (setf (ll-multipart-parser-body-mark parser) p)
                (go-state +header-field-start+ 0))

               (+header-field-start+
                (let ((next (parse-headers header-parser callbacks data p end)))
                  (setq p (1- next)) ;; XXX
                  ;; parsing headers done
                  (when (= (http-state header-parser) +state-body+)
                    (when-let (callback (callbacks-headers-complete callbacks))
                      (handler-case (funcall callback parser)
                        (error (e)
                          (error 'cb-headers-complete :error e))))
                    (setf (http-state header-parser) +state-headers+))
                  (go-state +body-start+ 0)))

               (+body-start+
                (setf (ll-multipart-parser-body-mark parser) (1+ p))
                (go-state +looking-for-delimiter+))

               (+looking-for-delimiter+
                (setf (ll-multipart-parser-boundary-mark parser) nil)
                (casev byte
                  (+cr+ (setf (ll-multipart-parser-boundary-mark parser) p)
                        (go-state +maybe-delimiter-start+))
                  (otherwise (go-state +looking-for-delimiter+))))

               (+maybe-delimiter-start+
                (unless (= byte +lf+)
                  (go-state +looking-for-delimiter+ 0))
                (go-state +maybe-delimiter-first-dash+))

	       (+maybe-delimiter-first-dash+
                (if (= byte +dash+)
                    (go-state +maybe-delimiter-second-dash+)
		    (if (= byte +cr+)
			(progn
			  (setf (ll-multipart-parser-boundary-mark parser) p)
			  (go-state +maybe-delimiter-start+))
			(go-state +looking-for-delimiter+))))

               (+maybe-delimiter-second-dash+
                (if (= byte +dash+)
                    (go-state +parsing-delimiter+)
                    (go-state +looking-for-delimiter+)))

               (+body-almost-done+
                (casev byte
                  (+dash+ (go-state +body-done+ 0))
                  (otherwise (error 'invalid-multipart-body))))

               (+body-done+
                (when (ll-multipart-parser-body-mark parser)
                  ;; got a part
                  (setf (ll-multipart-parser-body-buffer parser) nil)
                  (call-body-cb)
                  (when-let (callback (callbacks-message-complete callbacks))
                    (handler-case (funcall callback parser)
                      (error (e)
                        (error 'cb-message-complete :error e))))
                  (setf (ll-multipart-parser-body-mark parser) nil))
                (go exit-loop))))
         exit-loop)
        (when (ll-multipart-parser-body-mark parser)
          (when (<= +looking-for-delimiter+
                    (ll-multipart-parser-state parser)
                    +maybe-delimiter-second-dash+)
            (call-body-cb (or (ll-multipart-parser-boundary-mark parser) p)))
          ;; buffer the last part
          (when (ll-multipart-parser-boundary-mark parser)
            (setf (ll-multipart-parser-body-buffer parser)
                  (if (ll-multipart-parser-body-buffer parser)
                      (concatenate 'simple-byte-vector
                                   (ll-multipart-parser-body-buffer parser)
                                   (subseq data (ll-multipart-parser-boundary-mark parser)))
                      (subseq data (ll-multipart-parser-boundary-mark parser)))))

          (setf (ll-multipart-parser-body-mark parser) 0
                (ll-multipart-parser-boundary-mark parser) nil))
        p))))

(defun make-parser (http &key first-line-callback header-callback body-callback finish-callback)
  (declare (type http http))
  (let (callbacks

        (parse-fn (etypecase http
                    (http-request #'parse-request)
                    (http-response #'parse-response)))

        (headers nil)

        (header-value-buffer nil)
        parsing-header-field
        data-buffer

        header-complete-p
        completedp)
    (flet ((collect-prev-header-value ()
             (when header-value-buffer
               (let ((header-value
                       (locally (declare (optimize (speed 3) (safety 0)))
                         (coerce-to-string
                          (the (or octets-concatenated-xsubseqs
                                   octets-xsubseq)
                               header-value-buffer)))))
                 (if (string= parsing-header-field "set-cookie")
                     (push header-value (gethash "set-cookie" headers))
                     (multiple-value-bind (previous-value existp)
                         (gethash (the simple-string parsing-header-field) headers)
                       (setf (gethash (the simple-string parsing-header-field) headers)
                             (if existp
                                 (if (simple-string-p previous-value)
                                     (concatenate 'string (the simple-string previous-value) ", " header-value)
                                     (format nil "~A, ~A" previous-value header-value))
                                 header-value))))))))
      (setq callbacks
            (make-callbacks
             :message-begin (lambda (http)
                              (declare (ignore http))
                              (setq headers (make-hash-table :test 'equal)
                                    header-complete-p nil
                                    completedp nil))
             :url (lambda (http data start end)
                    (declare (type simple-byte-vector data)
                             (type pointer start end))
                    (setf (http-resource http)
                          (ascii-octets-to-string data :start start :end end)))
             :status (lambda (http data start end)
                       (declare (type simple-byte-vector data)
                                (type pointer start end))
                       (setf (http-status-text http)
                             (ascii-octets-to-string data :start start :end end)))
             :first-line (and first-line-callback
                              (lambda (http)
                                (declare (ignore http))
                                (funcall (the function first-line-callback))))
             :header-field (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (collect-prev-header-value)
                             (setq header-value-buffer (make-concatenated-xsubseqs))
                             (setq parsing-header-field
                                   (ascii-octets-to-lower-string data :start start :end end)))
             :header-value (lambda (http data start end)
                             (declare (ignore http)
                                      (type simple-byte-vector data)
                                      (type pointer start end))
                             (xnconcf header-value-buffer
                                      (xsubseq (subseq (the simple-byte-vector data) start end) 0)))
             :headers-complete (lambda (http)
                                 (collect-prev-header-value)
                                 (setq header-value-buffer nil)
                                 (when (gethash "set-cookie" headers)
                                   (setf (gethash "set-cookie" headers)
                                         (nreverse (gethash "set-cookie" headers))))
                                 (setf (http-headers http) headers)
                                 (when header-callback
                                   (funcall (the function header-callback) headers))
                                 (when (and (not (http-chunked-p http))
                                            (not (numberp (http-content-length http))))
                                   (setq completedp t))
                                 (setq header-complete-p t))
             :body (and body-callback
                        (lambda (http data start end)
                          (declare (ignore http)
                                   (type simple-byte-vector data)
                                   (type pointer start end))
                          (funcall (the function body-callback)
                                   data start end)))
             :message-complete (lambda (http)
                                 (declare (ignore http))
                                 (collect-prev-header-value)
                                 (when finish-callback
                                   (funcall (the function finish-callback)))
                                 (setq completedp t)))))

    (lambda (data &key (start 0) end)
      (declare (optimize (speed 3) (safety 2)))
      (cond
        ((eql data :eof)
         (setq completedp t)
         (when finish-callback
           (funcall (the function finish-callback))))
        (T
         (locally (declare (type simple-byte-vector data)
                           (type pointer start))
           (check-type end (or null pointer))
           (when data-buffer
             (setq data
                   (coerce-to-sequence
                    (xnconc (xsubseq data-buffer 0)
                            (xsubseq (the simple-byte-vector data) start (or end (length data))))))
             (setq data-buffer nil
                   start 0
                   end nil))
           (setf (http-mark http) start)
           (handler-case
               (funcall parse-fn http callbacks (the simple-byte-vector data) :start start :end end)
             (eof ()
               (setq data-buffer
                     (subseq data (http-mark http) (or end (length data)))))))))
      (values http header-complete-p completedp))))

(defun find-boundary (content-type)
  (declare (type string content-type))
  (let ((parsing-boundary nil))
    (parse-header-value-parameters content-type
                                   :header-value-callback
                                   (lambda (data start end)
                                     (unless (string= data "multipart/form-data"
                                                      :start1 start :end1 end)
                                       (return-from find-boundary nil)))
                                   :header-parameter-key-callback
                                   (lambda (data start end)
                                     (when (string= data "boundary"
                                                    :start1 start :end1 end)
                                       (setq parsing-boundary t)))
                                   :header-parameter-value-callback
                                   (lambda (data start end)
                                     (when parsing-boundary
                                       (return-from find-boundary (subseq data start end)))))))

(defun make-multipart-parser (content-type callback)
  (check-type content-type string)
  (let ((boundary (find-boundary content-type)))
    (unless boundary
      (return-from make-multipart-parser nil))

    (let ((parser (make-ll-multipart-parser :boundary boundary))
          (headers (make-hash-table :test 'equal))
          parsing-content-disposition
          parsing-header-field
          field-meta
          header-value-buffer
          (body-buffer (make-smart-buffer))
          callbacks)
      (flet ((collect-prev-header-value ()
               (when header-value-buffer
                 (let ((header-value
                         (u8-to-string
                          (coerce-to-sequence header-value-buffer))))
                   (when parsing-content-disposition
                     (setq field-meta
                           (let (parsing-key
                                 (field-meta (make-hash-table :test 'equal)))
                             (parse-header-value-parameters header-value
                                                            :header-parameter-key-callback
                                                            (lambda (data start end)
                                                              (setq parsing-key
                                                                    (string-downcase (subseq data start end))))
                                                            :header-parameter-value-callback
                                                            (lambda (data start end)
                                                              (setf (gethash parsing-key field-meta)
                                                                    (subseq data start end))))
                             field-meta)))
                   (setf (gethash parsing-header-field headers)
                         header-value)))))
        (setq callbacks
              (make-callbacks
               :header-field (lambda (parser data start end)
                               (declare (ignore parser))
                               (collect-prev-header-value)
                               (setq header-value-buffer (make-concatenated-xsubseqs))

                               (let ((header-name
                                       (ascii-octets-to-lower-string data :start start :end end)))
                                 (setq parsing-content-disposition
                                       (string= header-name "content-disposition"))
                                 (setq parsing-header-field header-name)))
               :header-value (lambda (parser data start end)
                               (declare (ignore parser))
                               (xnconcf header-value-buffer
                                        (xsubseq (subseq data start end) 0)))
               :headers-complete (lambda (parser)
                                   (declare (ignore parser))
                                   (collect-prev-header-value))
               :message-complete (lambda (parser)
                                   (declare (ignore parser))
                                   (funcall callback
                                            (gethash "name" field-meta)
                                            headers
                                            field-meta
                                            (finalize-buffer body-buffer))
                                   (setq headers (make-hash-table :test 'equal)
                                         body-buffer (make-smart-buffer)
                                         header-value-buffer nil))
               :body (lambda (parser data start end)
                       (declare (ignore parser))
                       (write-to-buffer body-buffer data start end)))))
      (lambda (data)
        (http-multipart-parse parser callbacks data)
        (= (ll-multipart-parser-state parser) +body-done+)))))
