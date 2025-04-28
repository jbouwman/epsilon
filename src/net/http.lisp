(defpackage #:epsilon.net.http
  (:use
   #:cl
   #:sb-gray
   #:sb-cltl2
   #:epsilon.lib.control
   #:epsilon.lib.binding
   #:epsilon.lib.buffer
   #:epsilon.lib.char
   #:epsilon.lib.collect
   #:epsilon.lib.list
   #:epsilon.lib.stream
   #:epsilon.lib.string
   #:epsilon.lib.symbol
   #:epsilon.lib.type
   #:epsilon.lib.xsubseq
   #:epsilon.sys.sync.thread
   #:epsilon.lib.codec.base64
   #:epsilon.lib.sequence
   #:epsilon.net.tls
   #:epsilon.sys.fs)
  (:local-nicknames
   (#:uri #:epsilon.lib.uri))
  (:shadow :get
   :delete)
  (:export
   :request
   :get
   :post
   :head
   :put
   :patch
   :delete
   :fetch
   :*default-connect-timeout*
   :*default-read-timeout*
   :*default-proxy*
   :*verbose*
   :*not-verify-ssl*
   :*connection-pool*
   :*use-connection-pool*
   :make-connection-pool
   :clear-connection-pool

   ;; Restarts
   :retry-request
   :ignore-and-continue))

(in-package #:epsilon.net.http)

(defconstant +output-buffer-size+ 8192)

(define-constant +crlf+
  (make-array 2 :element-type 'u8
                :initial-contents (mapcar 'char-code '(#\Return #\Linefeed)))
  "A 2-element array consisting of the character codes for a CRLF
sequence.")

(define-constant +hex-digits+ '#.(coerce "0123456789ABCDEF" 'list)
  "The hexadecimal digits.")

(defvar *current-error-message* nil
  "Used by the parsing functions in `read.lisp' as an
introduction to a standardized error message about unexpected
characters unless it is NIL.")

(defvar *current-error-function* nil
  "Used by the functions in `read.lisp' as a function to signal
errors about unexpected characters when *CURRENT-ERROR-MESSAGE*
is NIL.")

(defvar *accept-bogus-eols* nil
  "Some web servers do not respond with a correct CRLF line ending for
HTTP headers but with a lone linefeed or carriage return instead.  If
this variable is bound to a true value, READ-LINE* will treat a lone
LF or CR character as an acceptable end of line.  The initial value is
NIL.")

(defvar *treat-semicolon-as-continuation* nil
  "According to John Foderaro, Netscape v3 web servers bogusly split
Set-Cookie headers over multiple lines which means that we'd have to
treat Set-Cookie headers ending with a semicolon as incomplete and
combine them with the next header.  This will only be done if this
variable has a true value, though.")

(defvar *char-buffer* nil
  "A `buffer' for one character.  Used by PEEK-CHAR* and
UNREAD-CHAR*.")

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun read-char* (stream &optional (eof-error-p t) eof-value)
  "The streams we're dealing with are all binary with element type
\(UNSIGNED-BYTE 8) and we're only interested in ISO-8859-1, so we use
this to `simulate' READ-CHAR."
  (cond (*char-buffer*
         (prog1 *char-buffer*
           (setq *char-buffer* nil)))
        (t
         ;; this assumes that character codes are identical to Unicode code
         ;; points, at least for Latin1
         (let ((char-code (read-byte stream eof-error-p eof-value)))
           (and char-code
                (code-char char-code))))))

(defun unread-char* (char)
  "Were simulating UNREAD-CHAR by putting the character into
*CHAR-BUFFER*."
  ;; no error checking, only used internally
  (setq *char-buffer* char)
  nil)
  
(defun peek-char* (stream &optional eof-error-p eof-value)
  "We're simulating PEEK-CHAR by reading a character and putting it
into *CHAR-BUFFER*."
  ;; no error checking, only used internally  
  (setq *char-buffer* (read-char* stream eof-error-p eof-value)))

(defmacro with-character-stream-semantics (&body body)
  "Binds *CHAR-BUFFER* around BODY so that within BODY we can use
READ-CHAR* and friends \(see above) to simulate a character stream
although we're reading from a binary stream."
  `(let ((*char-buffer* nil))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +known-words+
    '(;; headers including WebDAV and some de facto standard headers
      "Accept"
      "Accept-Charset"
      "Accept-Encoding"
      "Accept-Language"
      "Accept-Ranges"
      "Age"
      "Allow"
      "Authorization"
      "Cache-Control"
      "Connection"
      "Content-Encoding"
      "Content-Language"
      "Content-Length"
      "Content-Location"
      "Content-MD5"
      "Content-Range"
      "Content-Type"
      "DAV"
      "Date"
      "Depth"
      "Destination"
      "ETag"
      "Expect"
      "Expires"
      "From"
      "Host"
      "If"
      "If-Match"
      "If-Modified-Since"
      "If-None-Match"
      "If-Range"
      "If-Unmodified-Since"
      "Last-Modified"
      "Location"
      "Lock-Token"
      "Max-Forwards"
      "Overwrite"
      "Pragma"
      "Proxy-Authenticate"
      "Proxy-Authorization"
      "Range"
      "Referer"
      "Retry-After"
      "Server"
      "TE"
      "TimeOut"
      "Trailer"
      "Transfer-Encoding"
      "Upgrade"
      "User-Agent"
      "Vary"
      "Via"
      "WWW-Authenticate"
      "Warning"
      ;; methods including WebDAV
      "CONNECT"
      "COPY"
      "DELETE"
      "GET"
      "HEAD"
      "LOCK"
      "MKCOL"
      "MOVE"
      "OPTIONS"
      "POST"
      "PROPFIND"
      "PROPPATCH"
      "PUT"
      "TRACE"
      "UNLOCK"
      ;; protocols
      "HTTP/1.1"
      "HTTP/1.0"
      ;; only a few and only the "preferred MIME names" - see
      ;; <http://www.iana.org/assignments/character-sets> for a
      ;; complete list
      "US-ASCII"
      "ISO-8859-1"
      "UTF-8"
      "UTF-16"
      "UTF-32BE"
      "UTF-32LE")
    "A list of words \(headers, methods, protocols, character sets)
that are typically seen in HTTP communication.  Mostly from RFC 2616,
but includes WebDAV stuff and other things as well."))

(define-constant +string-to-keyword-hash+
  (let ((hash (make-hash-table :test 'equal :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash word hash) (make-keyword word)))
    hash)
  "A hash table which case-insensitively maps the strings from
+KNOWN-WORDS+ to keywords.")

(define-constant +keyword-to-string-hash+
  (let ((hash (make-hash-table :test 'eq :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash (make-keyword word) hash)
                   (string-capitalize word)))
    hash)
  "A hash table which maps keywords derived from +KNOWN-WORDS+ to
capitalized strings.")

(defun as-keyword (string)
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (or (gethash string +string-to-keyword-hash+)
      (make-keyword string)))

(defun as-capitalized-string (keyword)
  "Kind of the inverse of AS-KEYWORD.  Has essentially the same effect
as STRING-CAPITALIZE but is optimized for \"known\" keywords like
:CONTENT-LENGTH or :GET."
  (or (gethash keyword +keyword-to-string-hash+)
      (string-capitalize keyword)))

(defun charp (char)
  "Returns true if the Lisp character CHAR is a CHAR according to RFC 2616."
  (<= 0 (char-code char) 127))

(defun controlp (char)
  "Returns true if the Lisp character CHAR is a CTL according to RFC 2616."
  (or (<= 0 (char-code char) 31)
      (= (char-code char) 127)))

(defun separatorp (char)
  "Returns true if the Lisp character CHAR is a separator
according to RFC 2616."
  (find char #.(format nil " ()<>@,;:\\\"/[]?={}~C" #\Tab)
        :test #'char=))

(defun whitespacep (char)
  "Returns true if the Lisp character CHAR is whitespace
according to RFC 2616."
  (member char '(#\Space #\Tab) :test #'char=))

(defun token-char-p (char)
  "Returns true if the Lisp character CHAR is a token constituent
according to RFC 2616."
  (and (charp char)
       (not (or (controlp char)
                (separatorp char)))))

(defun assert-char (stream expected-char)
  "Reads the next character from STREAM and checks if it is the
character EXPECTED-CHAR.  Signals an error otherwise."
  (let ((char (read-char* stream)))
    (unless (char= char expected-char)
      (error 'stream-error :stream stream
                           :format-control "Unexpected character, saw ~S, expected ~S."
                           :format-arguments (list char expected-char)))
    char))

(defun assert-crlf (stream)
  "Reads the next two characters from STREAM and checks if these
are a carriage return and a linefeed.  Signals an error
otherwise."
  (assert-char stream #\Return)
  (assert-char stream #\Linefeed))

(defun read-line* (stream &optional log-stream)
  "Reads and assembles characters from the binary stream STREAM until
a carriage return is read.  Makes sure that the following character is
a linefeed.  If *ACCEPT-BOGUS-EOLS* is not NIL, then the function will
also accept a lone carriage return or linefeed as an acceptable line
break.  Returns the string of characters read excluding the line
break.  Returns NIL if input ends before one character was read.
Additionally logs this string to LOG-STREAM if it is not NIL."
  (let ((result
         (with-output-to-string (line)
           (loop for char-seen-p = nil then t
                 for char = (read-char* stream nil)
                 for is-cr-p = (and char (char= char #\Return))
                 until (or (null char)
                           is-cr-p
                           (and *accept-bogus-eols*
                                (char= char #\Linefeed)))
                 do (write-char char line)
                 finally (cond ((and (not char-seen-p)
                                     (null char))
                                (return-from read-line* nil))
                               ((not *accept-bogus-eols*)
                                (assert-char stream #\Linefeed))
                               (is-cr-p
                                (when (eql (peek-char* stream) #\Linefeed)
                                  (read-char* stream))))))))
    (when log-stream
      (write-line result log-stream)
      (finish-output log-stream))
    result))

(defun trim-whitespace (string &key (start 0) (end (length string)))
  "Returns a version of the string STRING \(between START and END)
where spaces and tab characters are trimmed from the start and the
end.  Might return STRING."
  ;; optimized version to replace STRING-TRIM, suggested by Jason Kantz
  (declare (string string))
  (let* ((start% (loop for i of-type fixnum from start below end
                       while (or (char= #\space (char string i))
                                 (char= #\tab (char string i)))
                       finally (return i)))
         (end% (loop for i of-type fixnum downfrom (1- end) to start
                     while (or (char= #\space (char string i))
                               (char= #\tab (char string i)))
                     finally (return (1+ i)))))
    (declare (fixnum start% end%))
    (cond ((and (zerop start%) (= end% (length string))) string)
          ((> start% end%) "")
          (t (subseq string start% end%)))))

(defun read-http-headers (stream &optional log-stream)
  "Reads HTTP header lines from STREAM \(except for the initial
status line which is supposed to be read already) and returns a
corresponding alist of names and values where the names are
keywords and the values are strings.  Multiple lines with the
same name are combined into one value, the individual values
separated by commas.  Header lines which are spread across
multiple lines are recognized and treated correctly.  Additonally
logs the header lines to LOG-STREAM if it is not NIL."
  (let (headers
        (*current-error-message* "While reading HTTP headers:"))
    (labels ((read-header-line ()
               "Reads one header line, considering continuations."
               (with-output-to-string (header-line)
                 (loop
                  (let ((line (trim-whitespace (read-line* stream log-stream))))
                    (when (zerop (length line))
                      (return))
                    (write-sequence line header-line)
                    (let ((next (peek-char* stream)))
                      (unless (whitespacep next)
                        (return)))
                    ;; we've seen whitespace starting a continutation,
                    ;; so we loop
                    (write-char #\Space header-line)))))
             (split-header (line)
               "Splits line at colon and converts it into a cons.
Returns NIL if LINE consists solely of whitespace."
               (unless (zerop (length (trim-whitespace line)))
                 (let ((colon-pos (or (position #\: line :test #'char=)
                                      (error 'stream-error
                                             :stream stream
                                             :format-control "Couldn't find colon in header line ~S."
                                             :format-arguments (list line)))))
                   (cons (as-keyword (subseq line 0 colon-pos))
                         (trim-whitespace (subseq line (1+ colon-pos)))))))
             (add-header (pair)
               "Adds the name/value cons PAIR to HEADERS.  Takes
care of multiple headers with the same name."
               (let* ((name (car pair))
                      (existing-header (assoc name headers :test #'eq))
                      (existing-value (cdr existing-header)))
                 (cond (existing-header
                        (setf (cdr existing-header)
                              (format nil "~A~:[,~;~]~A"
                                      existing-value
                                      (and *treat-semicolon-as-continuation*
                                           (eq name :set-cookie)
                                           (ends-with-p (trim-whitespace existing-value) ";"))
                                      (cdr pair))))
                       (t (push pair headers))))))
      (loop for header-pair = (split-header (read-header-line))
            while header-pair
            do (add-header header-pair)))
    (nreverse headers)))

(defun skip-whitespace (stream)
  "Consume characters from STREAM until an END-OF-FILE is
encountered or a non-whitespace \(according to RFC 2616)
characters is seen.  This character is returned \(or NIL in case
of END-OF-FILE)."
  (loop for char = (peek-char* stream nil)
        while (and char (whitespacep char))
        do (read-char* stream)
        finally (return char)))

(defun read-token (stream)
  "Read characters from STREAM while they are token constituents
\(according to RFC 2616).  It is assumed that there's a token
character at the current position.  The token read is returned as
a string.  Doesn't signal an error \(but simply stops reading) if
END-OF-FILE is encountered after the first character."
  (with-output-to-string (out)
    (loop for first = t then nil
          for char = (if first
                       (peek-char* stream)
                       (or (peek-char* stream nil) (return)))
          while (token-char-p char)
          do (write-char (read-char* stream) out))))

(defun read-quoted-string (stream)
  "Reads a quoted string \(according to RFC 2616).  It is assumed
that the character at the current position is the opening quote
character.  Returns the string read without quotes and escape
characters."
  (read-char* stream)
  (with-output-to-string (out)
    (loop for char = (read-char* stream)
          until (char= char #\")
          do (case char
               (#\\ (write-char (read-char* stream) out))
               (#\Return (assert-char stream #\Linefeed)
                         (let ((char (read-char* stream)))
                           (unless (whitespacep char)
                             (error 'stream-error
                                    :format-control "Unexpected char, saw ~S, expected ~S."
                                    :format-arguments (list char '(#\Space #\Tab))))))
               (otherwise (write-char char out))))))

(defun read-cookie-value (stream &key (separators ";"))
  "Reads a cookie parameter value from STREAM which is returned as a
string.  Simply reads until a semicolon is seen \(or an element of
SEPARATORS).  Also reads quoted strings if the first non-whitespace
character is a quotation mark \(as in RFC 2109)."
  (if (char= #\" (peek-char* stream))
      (read-quoted-string stream)
      (trim-whitespace
       (with-output-to-string (out)
         (loop for char = (peek-char* stream nil)
               until (or (null char) (find char separators :test #'char=))
               do (write-char (read-char* stream) out))))))

(defun read-name-value-pair (stream &key (value-required-p t) cookie-syntax)
  "Reads a typical \(in RFC 2616) name/value or attribute/value
combination from STREAM - a token followed by a #\\= character and
another token or a quoted string.  Returns a cons of name and value,
both as strings.  If VALUE-REQUIRED-P is NIL, the #\\= sign and the
value are optional.  If COOKIE-SYNTAX is true, uses READ-COOKIE-VALUE
internally."
  (skip-whitespace stream)
  (let ((name (if cookie-syntax
                (read-cookie-value stream :separators "=;")
                (read-token stream))))
    (skip-whitespace stream)
    (cons name
          (when (or value-required-p
                    (eql (peek-char* stream nil) #\=))
            (assert-char stream #\=)
            (skip-whitespace stream)
            (cond (cookie-syntax (read-cookie-value stream))
                  ((char= (peek-char* stream) #\") (read-quoted-string stream))
                  (t (read-token stream)))))))

(defun read-name-value-pairs (stream &key (value-required-p t) cookie-syntax)
  "Uses READ-NAME-VALUE-PAIR to read and return an alist of
name/value pairs from STREAM.  It is assumed that the pairs are
separated by semicolons and that the first char read \(except for
whitespace) will be a semicolon.  The parameters are used as in
READ-NAME-VALUE-PAIR.  Stops reading in case of END-OF-FILE
\(instead of signaling an error)."
  (loop for char = (skip-whitespace stream)
        while (and char (char= char #\;))
        do (read-char* stream)
        ;; guard against a stray semicolon at the end
        when (skip-whitespace stream)
        collect (read-name-value-pair stream
                                      :value-required-p value-required-p
                                      :cookie-syntax cookie-syntax)))

(defclass chunked-stream ()
  ((real-stream :initarg :real-stream
                :reader chunked-stream-stream
                :documentation "The actual stream that's used for
input and/or output."))
  (:documentation "Every chunked stream returned by
MAKE-CHUNKED-STREAM is of this type which is a subtype of
STREAM."))

(defclass chunked-input-stream (chunked-stream fundamental-binary-input-stream)
  ((input-chunking-p :initform nil
                     :reader chunked-stream-input-chunking-p
                     :documentation "Whether input chunking is currently enabled.")
   (input-buffer :initform nil
                 :documentation "A vector containing the binary
data from the most recent chunk that was read.")
   (input-index :initform 0
                :accessor chunked-stream-input-index
                :documentation "The current position within INPUT-BUFFER.")
   (input-limit :initform 0
                :accessor chunked-stream-input-limit
                :documentation "Only the content in INPUT-BUFFER
up to INPUT-LIMIT belongs to the current chunk.")
   (chunk-extensions :initform nil
                     :reader chunked-input-stream-extensions
                     :documentation "An alist of attribute/value
pairs corresponding to the optional `chunk extensions' which
might be encountered when reading from a chunked stream.")
   (chunk-trailers :initform nil
                   :reader chunked-input-stream-trailers
                   :documentation "An alist of attribute/value
pairs corresponding to the optional `trailer' HTTP headers which
might be encountered at the end of a chunked stream.")
   (expecting-crlf-p :initform nil
                     :accessor expecting-crlf-p
                     :documentation "Whether we expect to see
CRLF before we can read the next chunk-size header part from the
stream.  \(This will actually be the CRLF from the end of the
last chunk-data part.)")
   (signal-eof :initform nil
               :accessor chunked-input-stream-eof-after-last-chunk
               :documentation "Return EOF after the last chunk instead
of simply switching chunking off."))
  (:documentation "A chunked stream is of this type if its
underlying stream is an input stream. This is a subtype of
CHUNKED-STREAM."))

(defclass chunked-output-stream (chunked-stream fundamental-binary-output-stream)
  ((output-chunking-p :initform nil
                      :reader chunked-stream-output-chunking-p
                      :documentation "Whether output chunking is
currently enabled.")
   (output-buffer :initform (make-array +output-buffer-size+ :element-type 'u8)
                  :accessor output-buffer
                  :documentation "A vector used to temporarily
store data which will output in one chunk.")
   (output-index :initform 0
                 :accessor output-index
                 :documentation "The current end of OUTPUT-BUFFER."))
  (:documentation "A chunked stream is of this type if its
underlying stream is an output stream. This is a subtype of
CHUNKED-STREAM."))

(defclass chunked-io-stream (chunked-input-stream chunked-output-stream)
  ()
  (:documentation "A chunked stream is of this type if it is both
a CHUNKED-INPUT-STREAM as well as a CHUNKED-OUTPUT-STREAM."))

(defmethod stream-element-type ((stream chunked-stream))
  "Chunked streams are always binary streams."
  'u8)                   ; TODO u8

(defmethod open-stream-p ((stream chunked-stream))
  "A chunked stream is open if its underlying stream is open."
  (open-stream-p (chunked-stream-stream stream)))

(defmethod close ((stream chunked-stream) &key abort)
  "If a chunked stream is closed, we close the underlying stream as well."
  (with-slots (real-stream)
      stream
    (cond ((open-stream-p real-stream)
           (close real-stream :abort abort))
          (t nil))))

(defun make-chunked-stream (stream)
  "Creates and returns a chunked stream \(a stream of type
CHUNKED-STREAM) which wraps STREAM.  STREAM must be an open
binary stream."
  (unless (and (streamp stream)
               (open-stream-p stream))
    (error 'stream-error
           :stream stream
           :format-control "~S should have been an open stream."
           :format-arguments (list stream)))
  (make-instance ;; actual type depends on STREAM
                 (cond ((and (input-stream-p stream)
                             (output-stream-p stream))
                        'chunked-io-stream)
                       ((input-stream-p stream)
                        'chunked-input-stream)
                       ((output-stream-p stream)
                        'chunked-output-stream))
                 :real-stream stream))

(defmethod chunked-input-stream-extensions ((object t))
  "The default method which always returns the empty list."
  nil)

(defmethod chunked-input-stream-trailers ((object t))
  "The default method which always returns the empty list."
  nil)

(defmethod chunked-stream-input-chunking-p ((object t))
  "The default method for all objects which are not of type
CHUNKED-INPUT-STREAM."
  nil)

(defmethod (setf chunked-stream-input-chunking-p) (new-value (stream chunked-input-stream))
  "Switches input chunking for STREAM on or off."
  (unless (eq (not new-value) (not (chunked-stream-input-chunking-p stream)))
    (with-slots (input-limit input-index expecting-crlf-p chunk-extensions chunk-trailers)
        stream
      (cond (new-value
             (setq expecting-crlf-p nil
                   input-limit 0
                   input-index 0
                   chunk-extensions nil
                   chunk-trailers nil))
            (t (when (< input-index input-limit)
                 (error 'stream-error
                        :stream stream
                        :format-control "Not all chunks from ~S have been read completely."
                        :format-arguments (list stream)))))))
  (setf (slot-value stream 'input-chunking-p) new-value))

(defmethod stream-clear-input ((stream chunked-input-stream))
  "Implements CLEAR-INPUT by resetting the internal chunk buffer."
  (when (chunked-stream-input-chunking-p stream)
    (setf (chunked-stream-input-index stream) 0
          (chunked-stream-input-limit stream) 0))
  ;; clear input on inner stream
  (clear-input (chunked-stream-stream stream))
  nil)

(defmethod chunked-input-available-p ((stream chunked-input-stream))
  "Whether there's unread input waiting in the chunk buffer."
  (< (chunked-stream-input-index stream)
     (chunked-stream-input-limit stream)))

(defmethod stream-listen ((stream chunked-input-stream))
  "We first check if input chunking is enabled and if there's
something in the buffer.  Otherwise we poll the underlying stream."
  (cond ((chunked-stream-input-chunking-p stream)
         (or (chunked-input-available-p stream)
             (fill-buffer stream)))
        ((eq (chunked-input-stream-eof-after-last-chunk stream) :eof)
         nil)
        (t (listen (chunked-stream-stream stream)))))

(defmethod fill-buffer ((stream chunked-input-stream))
  "Re-fills the chunk buffer.  Returns NIL if chunking has ended."
  (let ((inner-stream (chunked-stream-stream stream))
        ;; set up error function for the functions in `read.lisp'
        (*current-error-function*
         (lambda (last-char expected-chars)
             "The function which is called when an unexpected
character is seen."
             (error 'stream-error
                    :stream stream
                    :format-control "Unexpected char ~A, expected one of ~A."
                    :format-arguments (list last-char expected-chars)))))
    (labels ((add-extensions ()
               "Reads chunk extensions \(if there are any) and stores
them into the corresponding slot of the stream."
               (when-let (extensions (read-name-value-pairs inner-stream))
                 (warn 'stream-warning
                       :stream stream
                       :format-control "Adding uninterpreted extensions to stream ~S."
                       :format-arguments (list stream))
                 (setf (slot-value stream 'chunk-extensions)
                       (append (chunked-input-stream-extensions stream) extensions)))
               (assert-crlf inner-stream))
             (get-chunk-size ()
               "Reads chunk size header \(including optional
extensions) and returns the size."
               (with-character-stream-semantics
                 (when (expecting-crlf-p stream)
                   (assert-crlf inner-stream))
                 (setf (expecting-crlf-p stream) t)
                 ;; read hexadecimal number
                 (let (last-char)
                   (prog1 (loop for weight = (digit-char-p (setq last-char (read-char* inner-stream))
                                                           16)
                                for result = (if weight
                                               (+ weight (* 16 (or result 0)))
                                               (return (or result
                                                           (error 'input-chunking-body-corrupted
                                                                  :stream stream
                                                                  :last-char last-char
                                                                  :expected-chars +hex-digits+)))))
                     ;; unread first octet which wasn't a digit
                     (unread-char* last-char)
                     (add-extensions))))))
      (let ((chunk-size (get-chunk-size)))
        (with-slots (input-buffer input-limit input-index)
            stream
          (setq input-index 0
                input-limit chunk-size)
          (cond ((zerop chunk-size)
                 ;; turn chunking off
                 (setf (chunked-stream-input-chunking-p stream) nil
                       (slot-value stream 'chunk-trailers) (with-character-stream-semantics
                                                             (read-http-headers inner-stream))
                       input-limit 0)
                 (when (chunked-input-stream-eof-after-last-chunk stream)
                   (setf (chunked-input-stream-eof-after-last-chunk stream) :eof))
                 ;; return NIL
                 (return-from fill-buffer))
                ((> chunk-size (length input-buffer))
                 ;; replace buffer if it isn't big enough for the next chunk
                 (setq input-buffer (make-array chunk-size :element-type 'u8))))
          (unless (= (read-sequence input-buffer inner-stream :start 0 :end chunk-size)
                     chunk-size)
            (error 'input-chunking-unexpected-end-of-file
                   :stream stream))
          chunk-size)))))

(defmethod stream-read-byte ((stream chunked-input-stream))
  "Reads one byte from STREAM.  Checks the chunk buffer first, if
input chunking is enabled.  Re-fills buffer is necessary."
  (unless (chunked-stream-input-chunking-p stream)
    (return-from stream-read-byte
      (if (eq (chunked-input-stream-eof-after-last-chunk stream) :eof)
          :eof
          (read-byte (chunked-stream-stream stream) nil :eof))))
  (unless (chunked-input-available-p stream)
    (unless (fill-buffer stream)
      (return-from stream-read-byte :eof)))
  (with-slots (input-buffer input-index)
      stream
    (prog1 (aref input-buffer input-index)
      (incf input-index))))

(defmethod stream-read-sequence ((stream chunked-input-stream) sequence &optional start end)
  "Fills SEQUENCE by adding data from the chunk buffer and re-filling
it until enough data was read.  Works directly on the underlying
stream if input chunking is off."
  (unless (chunked-stream-input-chunking-p stream)
    (return-from stream-read-sequence
      (if (eq (chunked-input-stream-eof-after-last-chunk stream) :eof)
          0
          (read-sequence sequence (chunked-stream-stream stream) :start start :end end))))
  (loop
   (when (>= start end)
     (return-from stream-read-sequence start))   
   (unless (chunked-input-available-p stream)
     (unless (fill-buffer stream)
       (return-from stream-read-sequence start)))
   (with-slots (input-buffer input-limit input-index)
       stream
     (replace sequence input-buffer
              :start1 start :end1 end
              :start2 input-index :end2 input-limit)
     (let ((length (min (- input-limit input-index)
                        (- end start))))
       (incf start length)
       (incf input-index length)))))

(defmethod chunked-stream-output-chunking-p ((object t))
  "The default method for all objects which are not of type
CHUNKED-OUTPUT-STREAM."
  nil)

(defmethod write-chunk ((stream chunked-output-stream) sequence
                        &key (start 0)
                             (end (length sequence)))
  "Writes the contents of SEQUENCE from START to END to the
underlying stream of STREAM as one chunk."
  (let ((output-stream (chunked-stream-stream stream)))
    ;; chunk size
    (loop for char across (format nil "~X" (- end start))
          do (write-byte (char-code char) output-stream))
    (write-sequence +crlf+ output-stream)
    ;; data
    (write-sequence sequence output-stream :start start :end end)
    (write-sequence +crlf+ output-stream)))

(defmethod flush-buffer ((stream chunked-output-stream))
  "Uses WRITE-CHUNK to empty the output buffer unless it is
already empty."
  (with-slots (output-buffer output-index)
      stream
    (when (plusp output-index)
      (write-chunk stream output-buffer :end output-index)
      (setq output-index 0))))

(defmethod (setf chunked-stream-output-chunking-p) (new-value (stream chunked-output-stream))
  "Switches output chunking for STREAM on or off."
  (unless (eq (not new-value) (not (chunked-stream-output-chunking-p stream)))
    (with-slots (real-stream output-index)
        stream
      (cond (new-value
             ;; get rid of "old" data
             (force-output real-stream)
             ;; initialize output buffer as being empty
             (setq output-index 0))
            (t (flush-buffer stream)
               ;; last chunk to signal end of chunking
               (write-byte #.(char-code #\0) real-stream)
               (write-sequence +crlf+ real-stream)
               (write-sequence +crlf+ real-stream)
               (force-output real-stream)))))
  (setf (slot-value stream 'output-chunking-p) new-value))

(defmethod stream-clear-output ((stream chunked-output-stream))
  "We clear output by resetting the output buffer and clearing
the underlying stream."
  (when (chunked-stream-output-chunking-p stream)
    (setf (slot-value stream 'output-index) 0))
  (clear-output (chunked-stream-stream stream)))

(defmethod stream-finish-output ((stream chunked-output-stream))
  "Flush the output buffer if output chunking is on, then operate
on the underlying stream."
  (when (chunked-stream-output-chunking-p stream)
    (flush-buffer stream))
  (finish-output (chunked-stream-stream stream)))

(defmethod stream-force-output ((stream chunked-output-stream))
  "Flush the output buffer if output chunking is on, then operate
on the underlying stream."
  (when (chunked-stream-output-chunking-p stream)
    (flush-buffer stream))
  (force-output (chunked-stream-stream stream)))

(defmethod stream-write-byte ((stream chunked-output-stream) byte)
  "Writes one byte by simply adding it to the end of the output
buffer \(if output chunking is enabled).  The buffer is flushed
if necessary."
  (unless (chunked-stream-output-chunking-p stream)
    (return-from stream-write-byte
      (write-byte byte (chunked-stream-stream stream))))
  (with-slots (output-index output-buffer)
      stream
    (when (>= output-index +output-buffer-size+)
      (flush-buffer stream))
    (setf (aref output-buffer output-index) byte)
    (incf output-index)
    byte))

(defmethod stream-write-sequence ((stream chunked-output-stream) sequence &optional start end)
  "Outputs SEQUENCE by appending it to the output buffer if it's
small enough.  Large sequences are written directly using
WRITE-CHUNK."
  (unless (chunked-stream-output-chunking-p stream)
    (return-from stream-write-sequence
      (write-sequence sequence (chunked-stream-stream stream) :start start :end end)))
  (with-slots (output-buffer output-index)
      stream
    (let ((length (- end start)))
      (cond ((<= length (- +output-buffer-size+ output-index))
             (replace output-buffer sequence :start1 output-index
                      :start2 start :end2 end)
             (incf output-index length))
            (t (flush-buffer stream)
               (write-chunk stream sequence :start start :end end)))))
  sequence)

(defmethod close ((stream chunked-output-stream) &key abort)
  "When a stream is closed and ABORT isn't true we have to make
sure to send the last chunk."
  (unless abort
    (setf (chunked-stream-output-chunking-p stream) nil))
  (call-next-method))

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
                `(progn
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
                  `(progn
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
                `(progn
                   (if (skip-conditions ,',elem ,elems)
                       (advance)
                       (error 'match-failed
                              :elem ,',elem
                              :expected ',elems))))
              (skip* (&rest elems)
                (check-skip-elems elems)
                `(progn
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
                `(progn
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
                (<= ,end ,p))
              (current () (get-elem ,elem))
              (peek (&key eof-value)
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
  (declare (type u8 byte))
  (<= #.(char-code #\0) byte #.(char-code #\9)))

(declaim (ftype (function (u8) fixnum) digit-byte-char-to-integer))
(defun digit-byte-char-to-integer (byte)
  (declare (type u8 byte))
  (the fixnum (- byte #.(char-code #\0))))

(defun alpha-byte-char-p (byte)
  (declare (type u8 byte))
  (or (<= #.(char-code #\A) byte #.(char-code #\Z))
      (<= #.(char-code #\a) byte #.(char-code #\z))))

(defun alpha-byte-char-to-lower-char (byte)
  (declare (type u8 byte))
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
  (declare (type u8 byte))
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
  (declare (type u8 x))
  (if (<= #.(char-code #\A) x #.(char-code #\Z))
      (- x #.(- (char-code #\A) (char-code #\a)))
      x))

(declaim (inline ascii-octets-to-string))
(defun ascii-octets-to-string (octets &key (start 0) (end (length octets)))
  (declare (type simple-byte-vector octets)
           (type (unsigned-byte 64) start end))
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
           (type (unsigned-byte 64) start end))
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
  (declare (type simple-byte-vector vec1 vec2))
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
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)))

(declaim (inline position-not-whitespace))
(defun position-not-whitespace (string &key from-end)
  (declare (type #+ecl string #-ecl simple-string string))
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
  (declare (type simple-string string))
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
  (declare (type simple-string data))

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
                   `(progn
                      (incf p ,advance)
                      (when (= p end)
                        (go eof))
                      (setq char (aref data p))
                      (go ,state))))
      (flet ((tokenp (char)
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
                       (coerce-to-string
                        (the (or octets-concatenated-xsubseqs
                                 octets-xsubseq)
                             header-value-buffer))))
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

(defun parse-content-type (content-type)
  (let ((types
          (nth-value 1
                     (epsilon.lib.regex:scan-to-strings "^\\s*?(\\w+)/([^;\\s]+)(?:\\s*;\\s*charset=([A-Za-z0-9_-]+))?"
                                            content-type))))
    (when types
      (values (aref types 0)
              (aref types 1)
              (aref types 2)))))

(defun charset-to-encoding (charset &optional
                                      (default epsilon.lib.char:*default-character-encoding*))
  (cond
    ((null charset)
     default)
    ((string-equal charset "utf-8")
     :utf-8)
    ((string-equal charset "euc-jp")
     :eucjp)
    ((or (string-equal charset "shift_jis")
         (string-equal charset "shift-jis"))
     :cp932)
    ((string-equal charset "windows-31j")
     :cp932)
    (t (or (find charset (epsilon.lib.char:list-character-encodings)
                 :test #'string-equal)
           default))))

(defun detect-charset (content-type body)
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (cond
      ((charset-to-encoding charset nil))
      ((string-equal type "text")
       (or (charset-to-encoding charset nil)
           (if (and (string-equal subtype "html")
                    (typep body '->u8))
               (charset-to-encoding (detect-charset-from-html body) nil)
               nil)
           :utf-8))
      ((and (string-equal type "application")
            (or (string-equal subtype "json")
                (string-equal subtype "javascript")))
       ;; According to RFC4627 (http://www.ietf.org/rfc/rfc4627.txt),
       ;; JSON text SHALL be encoded in Unicode. The default encoding is UTF-8.
       ;; It's possible to determine if the encoding is UTF-16 or UTF-36
       ;; by looking at the first four octets, however, I leave it to the future.
       ;;
       ;; According to RFC4329 (https://datatracker.ietf.org/doc/html/rfc4329),
       ;; javascript also is specified by charset, or defaults to UTF-8
       ;; It's also possible to specify in the first four octets, but
       ;; like application/json I leave it to the future.
       (charset-to-encoding charset :utf-8))
      ((and (string-equal type "application")
            (epsilon.lib.regex:scan "(?:[^+]+\\+)?xml" subtype))
       (charset-to-encoding charset)))))

(defun detect-charset-from-html (body)
  "Detect the body's charset by (roughly) searching meta tags which has \"charset\" attribute."
  (labels ((find-meta (start)
             (search #.(epsilon.lib.char:string-to-u8 "<meta ") body :start2 start))
           (main (start)
             (let ((start (find-meta start)))
               (unless start
                 (return-from main nil))
               (let ((end (position (char-code #\>) body :start start :test #'=)))
                 (unless end
                   (return-from main nil))
                 (incf end)
                 (let ((match (nth-value 1 (epsilon.lib.regex:scan-to-strings
                                            "charset=[\"']?([^\\s\"'>]+)[\"']?"
                                            (epsilon.lib.char:u8-to-string body :start start :end end :errorp nil)))))
                   (if match
                       (aref match 0)
                       (main end)))))))
    (main 0)))

(defvar *use-connection-pool* t)
(defvar *max-active-connections* 8
  "Allowed number of active connections to all hosts.  If you change this,
  then call (make-new-connection-pool).")

(defstruct lru-pool-elt
  (prev nil :type (or null lru-pool-elt))
  (next nil :type (or null lru-pool-elt))
  (elt nil :type t)
  (key nil :type t)
  (eviction-callback nil :type (or null function)))

;; An LRU-POOL can have multiple entries for the same key
(defstruct lru-pool
  (lock #+thread-support (bt2:make-lock :name "connection pool lock")
        #-thread-support nil)
  (hash-table nil :type (or null hash-table)) ;; hash table entries are lists of elements
  (head nil :type (or null lru-pool-elt)) ;; most recently used is here and it's a doubly-linked-list
  (tail nil :type (or null lru-pool-elt)) ;; least recently used is here
  (num-elts 0 :type fixnum)
  (max-elts 8 :type fixnum))

(defun make-connection-pool (&optional (max-active-connections *max-active-connections*))
  (make-lru-pool :hash-table (make-hash-table :test 'equal) :max-elts max-active-connections))

(defvar *connection-pool* nil)

(defun make-new-connection-pool (&optional (max-active-connections *max-active-connections*))
  (clear-connection-pool)
  (setf *connection-pool* (make-connection-pool max-active-connections)))

(defun get-from-lru-pool (lru-pool key)
  "Takes an element from the LRU-POOL matching KEY.  Must be called with LRU-POOL-LOCK held.
   The element is removed from the pool."
  (let* ((hash-table (lru-pool-hash-table lru-pool))
         (possible-elts (gethash key (lru-pool-hash-table lru-pool))))
    (when possible-elts
      (let ((remaining-elts (cdr possible-elts)))
        (if remaining-elts
            (setf (gethash key hash-table) remaining-elts)
            (remhash key hash-table)))
      (let ((elt (car possible-elts)))
        (let ((prev (lru-pool-elt-prev elt))
              (next (lru-pool-elt-next elt)))
          (if prev
              (setf (lru-pool-elt-next prev) next)
              (setf (lru-pool-head lru-pool) next))
          (if next
              (setf (lru-pool-elt-prev next) prev)
              (setf (lru-pool-tail lru-pool) prev)))
        (decf (lru-pool-num-elts lru-pool))
        (lru-pool-elt-elt elt)))))

(defun evict-tail (lru-pool)
  "Removes the least recently used element of the LRU-POOL and returns
    (values evicted-element eviction-callback t) if there was
   an element to remove, otherwise nil.  Must be called with LRU-POOL-LOCK held.

   Outside the LRU-POOL-LOCK you must call the returned EVICTION-CALLBACK with the EVICTED-ELEMENT."
  ;; slightly different from get-from-lru-pool because we want to get rid of the
  ;; actual oldest element (one could in principle call get-from-lru-pool on
  ;; (lru-pool-elt-key (lru-pool-tail lru-pool)) if you didn't care
  (let* ((tail (lru-pool-tail lru-pool)))
    (when tail
      (let ((prev (lru-pool-elt-prev tail)))
        (if prev
            (setf (lru-pool-elt-next prev) nil)
            (setf (lru-pool-head lru-pool) nil))
        (setf (lru-pool-tail lru-pool) prev)
        (let* ((hash-table (lru-pool-hash-table lru-pool))
               (key (lru-pool-elt-key tail))
               (remaining (delete tail (gethash key hash-table))))
          (if remaining
              (setf (gethash key hash-table) remaining)
              (remhash key hash-table))))
      (decf (lru-pool-num-elts lru-pool))
      (values (lru-pool-elt-elt tail) (lru-pool-elt-eviction-callback tail) t))))

(defun add-to-lru-pool (lru-pool key elt eviction-callback)
  "Adds ELT to an LRU-POOL with potentially non-unique KEY, potentially evicting another element to
   make room.  EVICTION-CALLBACK will be called with one parameter ELT, when ELT is evicted from the
   LRU-POOL.  ADD-TO-LRU-POOL must be called with LRU-POOL-LOCK held.

   If an element was evicted to make space, returns (values evicted-elt eviction-callback t)
   otherwise nil.  The EVICTION-CALLBACK should take one parameter, the evicted element."
  (declare (type lru-pool lru-pool))
  (let* ((old-head (lru-pool-head lru-pool))
         (lru-pool-elt (make-lru-pool-elt :prev nil :next old-head :elt elt :key key :eviction-callback eviction-callback))
         (hash-table (lru-pool-hash-table lru-pool)))
    (setf (lru-pool-head lru-pool) lru-pool-elt)
    (push lru-pool-elt (gethash key hash-table))
    (when old-head
      (setf (lru-pool-elt-prev old-head) lru-pool-elt))
    (unless (lru-pool-tail lru-pool)
      (setf (lru-pool-tail lru-pool) lru-pool-elt))
    (when (> (incf (lru-pool-num-elts lru-pool)) (lru-pool-max-elts lru-pool))
      (evict-tail lru-pool))))

(defmethod print-object ((obj lru-pool-elt) str) ;; avoid printing loops
  (print-unreadable-object (obj str :type "LRU-POOL-ELT")
    (format str "~A NEXT ~A" (lru-pool-elt-key obj) (lru-pool-elt-next obj))))

(defmethod print-object ((obj lru-pool) str) ;; avoid printing loops
  (print-unreadable-object (obj str :type "LRU-POOL")
    (let (objs)
      (loop with lru-pool-elt = (lru-pool-head obj)
            while lru-pool-elt
            do (push (list (lru-pool-elt-key lru-pool-elt) (lru-pool-elt-elt lru-pool-elt)) objs)
            do (setf lru-pool-elt (lru-pool-elt-next lru-pool-elt)))
      (if objs
          (format str "~A/~A elts~%~{ ~{~A~^: ~}~^~%~}" (lru-pool-num-elts obj) (lru-pool-max-elts obj) objs)
          (format str "empty")))))

(defmacro with-lock (lock &body body)
  (declare (ignorable lock))
  #+thread-support `(bt2:with-lock-held (,lock)
                      ,@body)
  #-thread-support `(progn ,@body))

(defun push-connection (host-port stream &optional eviction-callback)
  "Add STREAM back to connection pool with key HOST-PORT.  EVICTION-CALLBACK
   must be a function of a single parameter, and will be called with STREAM
   if the HOST-PORT/SOCKET pair is evicted from the connection pool."
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (multiple-value-bind (evicted-elt eviction-callback)
          (with-lock (lru-pool-lock pool)
            (add-to-lru-pool pool host-port stream eviction-callback))
        (and eviction-callback (funcall eviction-callback evicted-elt))
        (values)))))

(defun steal-connection (host-port)
  "Return the STREAM associated with key HOST-PORT"
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (with-lock (lru-pool-lock pool)
        (get-from-lru-pool pool host-port)))))

(defun clear-connection-pool ()
  "Remove all elements from the connection pool, calling their eviction-callbacks."
  (when *use-connection-pool*
    (let ((pool *connection-pool*)
          evicted-element eviction-callback element-was-evicted)
      (when pool
        (loop for count from 0
              do (setf (values evicted-element eviction-callback element-was-evicted)
                       (with-lock (lru-pool-lock pool)
                         (evict-tail pool)))
              do (when eviction-callback (funcall eviction-callback evicted-element))
              while element-was-evicted)))))

(make-new-connection-pool)

(defclass keep-alive-stream (fundamental-input-stream)
  ((stream :type (or null stream)
           :initarg :stream
           :initform (error ":stream is required")
           :accessor keep-alive-stream-stream
           :documentation "A stream; when we read END elements from it, we call CLOSE-ACTION on it and
   set this slot to nil.")
   (end :initarg :end
        :initform nil
        :accessor keep-alive-stream-end)
   (close-action :initarg :on-close-or-eof :reader close-action
                 :documentation "A (lambda (stream abort)) which will be called with keep-alive-stream-stream
   when the stream is either closed or we hit end of file or we hit end")))

(defun keep-alive-stream-close-underlying-stream (underlying-stream abort)
  (when (and underlying-stream (open-stream-p underlying-stream))
    (close underlying-stream :abort abort)))

(defclass keep-alive-chunked-stream (keep-alive-stream)
  ((chunked-stream :initarg :chunked-stream :accessor chunked-stream)))

(defun make-keep-alive-stream (stream &key end chunked-stream (on-close-or-eof #'keep-alive-stream-close-underlying-stream))
  "ON-CLOSE-OR-EOF takes a single parameter, STREAM (the stream passed in here, not the
keep-alive-stream), and should handle clean-up of it"
  (assert (xor end chunked-stream))
  (if chunked-stream
      (make-instance 'keep-alive-chunked-stream :stream stream :chunked-stream chunked-stream :on-close-or-eof on-close-or-eof)
      (make-instance 'keep-alive-stream :stream stream :end end :on-close-or-eof on-close-or-eof)))

(defun maybe-close (stream &optional (close-if nil))
  "Will close the underlying stream if close-if is T (unless it is already closed).
   If the stream is already closed or we closed it returns :EOF otherwise NIL."
  (let ((underlying-stream (keep-alive-stream-stream stream)))
    (cond
      ((not underlying-stream)
       :eof)
      (close-if
       (funcall (close-action stream) underlying-stream nil)
       (setf (keep-alive-stream-stream stream) nil)
       :eof)
      (t nil))))

(defmethod stream-read-byte ((stream keep-alive-stream))
  "Return :EOF or byte read.  When we hit EOF or finish reading our allowed content,
   call the close-action on our underlying-stream and return EOF."
  (let ((byte :eof)
        (underlying-stream (keep-alive-stream-stream stream)))
    (or (maybe-close stream (<= (keep-alive-stream-end stream) 0))
        (progn
          (setf byte (read-byte underlying-stream nil :eof))
          (decf (keep-alive-stream-end stream) 1)
          (maybe-close stream (or (<= (keep-alive-stream-end stream) 0) (eql byte :eof)))
          byte))))

(defmethod stream-read-byte ((stream keep-alive-chunked-stream))
  "Return :EOF or byte read.  When we hit :EOF or finish reading our chunk,
   call the close-action on our underlying-stream and return :EOF"
  (or (maybe-close stream)
      (if (chunked-stream-input-chunking-p (chunked-stream stream))
          (let ((byte (read-byte (chunked-stream stream) nil :eof)))
            (if (eql byte :eof)
                (prog1
                    byte
                  (maybe-close stream t))
                byte))
          (or (maybe-close stream t) :eof))))

(defmethod stream-read-sequence ((stream keep-alive-stream) sequence &optional start end)
  (if (null (keep-alive-stream-stream stream)) ;; we already closed it
      start
      (let* ((to-read (min (- end start) (keep-alive-stream-end stream)))
             (n (read-sequence sequence (keep-alive-stream-stream stream)
                               :start start
                               :end (+ start to-read))))
        (decf (keep-alive-stream-end stream) (- n start))
        (maybe-close stream (<= (keep-alive-stream-end stream) 0))
        n)))

(defmethod stream-read-sequence ((stream keep-alive-chunked-stream) sequence &optional start end)
  (if (null (keep-alive-stream-stream stream)) ;; we already closed it
      start
      (if (chunked-stream-input-chunking-p (chunked-stream stream))
          (prog1
              (let ((num-read (read-sequence sequence (chunked-stream stream) :start start :end end)))
                num-read)
            (maybe-close stream (not (chunked-stream-input-chunking-p (chunked-stream stream)))))
          start)))

(defmethod stream-element-type ((stream keep-alive-chunked-stream))
  (stream-element-type (chunked-stream stream)))

(defmethod stream-element-type ((stream keep-alive-stream))
  'u8)

(defmethod open-stream-p ((stream keep-alive-stream))
  (let ((underlying-stream (keep-alive-stream-stream stream)))
    (and underlying-stream (open-stream-p underlying-stream))))

(defmethod close ((stream keep-alive-stream) &key abort)
  (funcall (close-action stream) (keep-alive-stream-stream stream) abort)
  (setf (keep-alive-stream-stream stream) nil))

(defvar *default-connect-timeout* 10)
(defvar *default-read-timeout* 10)
(defvar *verbose* nil)
(defvar *not-verify-ssl* nil)
(defvar *default-proxy* (or #-windows (epsilon.sys.env:getenv "HTTPS_PROXY")
                            #-windows (epsilon.sys.env:getenv "HTTP_PROXY"))
  "If specified will be used as the default value of PROXY in calls to epsilon.net.http.  Defaults to
 the value of the environment variable HTTPS_PROXY or HTTP_PROXY if not on Windows.")

(declaim (ftype (function (simple-string) ->u8) string->u8))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %string->u8 (string)
    (let ((result (make-array (length string) :element-type 'u8)))
      (declare (type ->u8 result))
      (dotimes (i (length string) result)
        (declare (type fixnum i))
        (setf (aref result i)
              (char-code (aref string i))))))

  (defun string->u8 (string)
    (%string->u8 string))

  ;;FIXME merge
  (define-compiler-macro string->u8 (&whole form string)
    (if (constantp string)
        (%string->u8 string)
        form))

  (declaim (type ->u8 +crlf+)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; The user agent reported by an Applie iPhone 6
  
  (defparameter *default-user-agent*
    "Mozilla/5.0 (Apple-iPhone7C2/1202.466; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1A543 Safari/419.3"))

(defparameter *header-buffer* nil)

(defun write-first-line (method uri version &optional (buffer *header-buffer*))
  (fast-write-sequence (string->u8 (string method)) buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8
                         (format nil "~A~:[~;~:*?~A~]"
                                 (or (uri:path uri) "/")
                                 (uri:query uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string->u8 "HTTP/1.1"))
                         (1.0 (string->u8 "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer))

(defun write-header-field (name buffer)
  (fast-write-sequence (if (typep name '->u8)
                           name
                           (string->u8 (string-capitalize name)))
                       buffer))

(defun write-header-value (value buffer)
  (fast-write-sequence (if (typep value '->u8)
                           value
                           (string->u8 (princ-to-string value)))
                       buffer))

(defun write-header (name value &optional (buffer *header-buffer*))
  (write-header-field name buffer)
  (fast-write-sequence (string->u8 ": ") buffer)
  (write-header-value value buffer)
  (fast-write-sequence +crlf+ buffer))

(define-compiler-macro write-header (name value &optional (buffer '*header-buffer*))
  `(progn
     ,(if (and (constantp name)
               (typep name '(or keyword string)))
          `(fast-write-sequence (string->u8 ,(string-capitalize name)) ,buffer)
          `(write-header-field ,name ,buffer))
     (fast-write-sequence (string->u8 ": ") ,buffer)
     ,(if (constantp value)
          `(fast-write-sequence (string->u8 ,(string value)) ,buffer)
          `(write-header-value ,value ,buffer))
     (fast-write-sequence +crlf+ ,buffer)))

(defmacro with-header-output ((buffer &optional output) &body body)
  `(with-fast-output (,buffer ,output)
     (declare (ignorable ,buffer))
     (let ((*header-buffer* ,buffer))
       ,@body)))

(defun write-connect-header (uri version buffer &optional proxy-auth)
  (fast-write-sequence (string->u8 "CONNECT") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8 (format nil "~A:~A"
                                           (uri:host uri)
                                           (uri:port uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (string->u8 "HTTP/1.1"))
                         (1.0 (string->u8 "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence (string->u8 "Host:") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (string->u8 (format nil "~A:~A"
                                           (uri:host uri)
                                           (uri:port uri)))
                       buffer)
  (when proxy-auth
    (fast-write-sequence +crlf+ buffer)
    (fast-write-sequence (string->u8 "Proxy-Authorization:") buffer)
    (fast-write-byte #.(char-code #\Space) buffer)
    (fast-write-sequence (string->u8 proxy-auth) buffer))
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence +crlf+ buffer))

(defun decode-body (content-type body &key default-charset on-close)
  (let ((charset (or (and content-type
                          (detect-charset content-type body))
                     default-charset))
        (epsilon.lib.char:*suppress-character-coding-errors* t))
    (if charset
        (handler-case
            (if (streamp body)
                (make-decoding-stream body :encoding charset :on-close on-close)
                (epsilon.lib.char:u8-to-string body :encoding charset))
          (epsilon.lib.char:character-decoding-error (e)
            (warn (format nil "Failed to decode the body to ~S due to the following error (falling back to binary):~%  ~A"
                          charset
                          e))
            (return-from decode-body body)))
        body)))

(defun content-disposition (key val)
  (typecase val
    (cons (content-disposition key (first val)))
    (pathname
     (let* ((filename (file-namestring val))
            (utf8-filename-p (find-if (lambda (char)
                                        (< 127 (char-code char)))
                                      filename)))
       (format nil "Content-Disposition: form-data; name=\"~A\"; ~:[filename=\"~A\"~;filename*=UTF-8''~A~]~C~C"
               key
               utf8-filename-p
               (if utf8-filename-p
                   (uri:url-encode filename :encoding :utf-8)
                   filename)
               #\Return #\Newline)))
    (otherwise
      (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
              key
              #\Return #\Newline))))


(defmacro define-alist-cache (cache-name)
  (let ((var (intern (format nil "*~A*" cache-name))))
  `(progn
     (defvar ,var)
     (defun ,(intern (format nil "LOOKUP-IN-~A" cache-name)) (elt)
       (when (boundp ',var)
         (assoc-value ,var elt)))
     (defun (setf ,(intern (format nil "LOOKUP-IN-~A" cache-name))) (val elt)
       (when (boundp ',var)
         (setf (assoc-value ,var elt) val))
       val))))

;; If bound, an alist mapping content to content-type,
;; used to avoid determining content type multiple times
(define-alist-cache content-type-cache)
;; If bound, an alist mapping content to encoded content, to avoid
;; double converting content when we must calculate its length first
(define-alist-cache content-encoding-cache)

;; TODO use e.lib.map

(defmacro with-content-caches (&body body)
  `(let ((*content-type-cache* nil)
         (*content-encoding-cache* nil))
     ,@body))

(defun content-type (value)
  (typecase value
    (pathname (or (lookup-in-content-type-cache value)
                  (setf (lookup-in-content-type-cache value) "application/octet-stream")))
    (otherwise nil)))

(defun multipart-value-content-type (value)
  (typecase value
    (cons
     (destructuring-bind (val &key content-type)
         value
       (or content-type (content-type val))))
    (otherwise (content-type value))))

(defun convert-to-octets (val)
  (or (lookup-in-content-encoding-cache val)
      (setf (lookup-in-content-encoding-cache val)
            (typecase val
              (string (epsilon.lib.char:string-to-u8 val))
              (->u8 val)
              (symbol (epsilon.lib.char:string-to-u8 (princ-to-string val)))
              (cons (convert-to-octets (first val)))
              (otherwise (epsilon.lib.char:string-to-u8 (princ-to-string val)))))))

(defun write-as-octets (stream val)
  (typecase val
    (->u8 (write-sequence val stream))
    (pathname
     (with-open-file (in val :element-type 'u8)
       (copy-stream in stream)))
    (string
     (write-sequence (convert-to-octets val) stream))
    (cons (write-as-octets stream (first val)))
    (otherwise (write-sequence (convert-to-octets val) stream))))

(defun content-length (val)
  (typecase val
    (pathname (with-open-file (in val)
                (file-length in)))
    (cons (content-length (first val)))
    (otherwise (length (convert-to-octets val)))))

(defun multipart-content-length (content boundary)
  (declare (type simple-string boundary))
  (let ((boundary-length (length boundary)))
    (+ (loop for (key . val) in content
             sum (+ 2 ;; --
                    boundary-length
                    2 ;; CR LF
                    (length (the simple-string (content-disposition key val)))
                    (let ((content-type (multipart-value-content-type val)))
                      (if content-type
                          (+ #.(length "Content-Type: ") (length content-type) 2)
                          0))
                    2
                    (content-length val)
                    2)
               into total-length
             finally (return total-length))
       2 boundary-length 2 2)))

(defun write-multipart-content (content boundary stream)
  (let ((boundary (string->u8 boundary)))
    (labels ((boundary-line (&optional endp)
               (write-sequence (string->u8 "--") stream)
               (write-sequence boundary stream)
               (when endp
                 (write-sequence (string->u8 "--") stream))
               (crlf))
             (crlf () (write-sequence +crlf+ stream)))
      (loop for (key . val) in content
            do (boundary-line)
               (write-sequence (string->u8 (content-disposition key val)) stream)
               (let ((content-type (multipart-value-content-type val)))
                 (when content-type
                   (write-sequence
                     (string->u8
                       (format nil "Content-Type: ~A~C~C" content-type #\Return #\Newline))
                     stream)))
               (crlf)
               (write-as-octets stream val)
               (crlf)
            finally
               (boundary-line t)))))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))

  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (epsilon.lib.codec:make-decompressing-stream :gzip body)
         (epsilon.lib.codec::decompress nil (epsilon.lib.codec::make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (epsilon.lib.codec:make-decompressing-stream :zlib body)
         (epsilon.lib.codec::decompress nil (epsilon.lib.codec::make-dstate :zlib) body)))
    (t body)))

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

(defun ca-bundle ()
  (uri:merge (current-dir) "certs/cacert.pem"))

(defun read-until-crlf*2 (stream)
  (with-fast-output (buf)
    (tagbody
     read-cr
       (loop for byte of-type (or u8 null) = (read-byte stream nil nil)
             if byte
               do (fast-write-byte byte buf)
             else
               do (go eof)
             until (= byte (char-code #\Return)))

     read-lf
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type u8 next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf)
              (go read-cr2))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-cr2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type u8 next-byte))
           (cond
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf2))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-lf2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type u8 next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     eof)))

(defvar +empty-body+
  (make-array 0 :element-type 'u8))

(defun read-response (stream has-body collect-headers read-body)
  (let* ((http (make-http-response))
         body
         body-data
         (headers-data (and collect-headers
                            (make-output-buffer)))
         (header-finished-p nil)
         (finishedp nil)
         (content-length nil)
         (transfer-encoding-p)
         (parser (make-parser http
                              :header-callback
                              (lambda (headers)
                                (setq header-finished-p t
                                      content-length (gethash "content-length" headers)
                                      transfer-encoding-p (gethash "transfer-encoding" headers))
                                (unless (and has-body
                                             (or content-length
                                                 transfer-encoding-p))
                                  (setq finishedp t)))
                              :body-callback
                              (lambda (data start end)
                                (when body-data
                                  (fast-write-sequence data body-data start end)))
                              :finish-callback
                              (lambda ()
                                (setq finishedp t)))))
    (let ((buf (read-until-crlf*2 stream)))
      (declare (type ->u8 buf))
      (when collect-headers
        (fast-write-sequence buf headers-data))
      (funcall parser buf))
    (unless header-finished-p
      (error "maybe invalid header"))
    (cond
      ((not read-body)
       (setq body stream))
      ((not has-body)
       (setq body +empty-body+))
      ((and content-length (not transfer-encoding-p))
       (let ((buf (make-array (etypecase content-length
                                (integer content-length)
                                (string (parse-integer content-length)))
                              :element-type 'u8)))
         (read-sequence buf stream)
         (setq body buf)))
      ((let ((status (http-status http)))
         (or (= status 100)    ;; Continue
             (= status 101)    ;; Switching Protocols
             (= status 204)    ;; No Content
             (= status 304))) ;; Not Modified
       (setq body +empty-body+))
      (T
       (setq body-data (make-output-buffer))
       (loop for buf of-type ->u8 = (read-until-crlf*2 stream)
             do (funcall parser buf)
             until (or finishedp
                       (zerop (length buf)))
             finally
                (setq body (finish-output-buffer body-data)))))
    (values http
            body
            (and collect-headers
                 (finish-output-buffer headers-data))
            transfer-encoding-p)))

(defun print-verbose-data (direction &rest data)
  (flet ((boundary-line ()
           (let ((char (ecase direction
                         (:incoming #\<)
                         (:outgoing #\>))))
             (fresh-line)
             (dotimes (i 50)
               (write-char char))
             (fresh-line))))
    (boundary-line)
    (dolist (d data)
      (map nil (lambda (byte)
                 (princ (code-char byte)))
           d))
    (boundary-line)))

(defun convert-body (body content-encoding content-type content-length chunkedp force-binary force-string keep-alive-p on-close)
  (when (streamp body)
    (cond
      ((and keep-alive-p chunkedp)
       (setf body (make-keep-alive-stream body :chunked-stream
                                          (let ((chunked-stream (make-chunked-stream body)))
                                            (setf (chunked-stream-input-chunking-p chunked-stream) t)
                                            chunked-stream) :on-close-or-eof on-close)))
      ((and keep-alive-p content-length)
       (setf body (make-keep-alive-stream body :end content-length :on-close-or-eof on-close)))
      (chunkedp
       (let ((chunked-stream (make-chunked-stream body)))
         (setf (chunked-stream-input-chunking-p chunked-stream) t)
         (setf body chunked-stream)))))
  (let ((body (decompress-body content-encoding body)))
    (if force-binary
        body
        (decode-body content-type body
                     :default-charset (if force-string
                                          epsilon.lib.char:*default-character-encoding*
                                          nil)))))

(defun make-connect-stream (uri version stream &optional proxy-auth)
  (let ((header (with-fast-output (buffer)
                  (write-connect-header uri version buffer proxy-auth))))
    (write-sequence header stream)
    (force-output stream)
    (read-until-crlf*2 stream)
    stream))

(defun make-proxy-authorization (uri)
  (let ((proxy-auth (uri:userinfo uri)))
    (when proxy-auth
      (format nil "Basic ~A"
              (string-to-base64-string proxy-auth)))))

(defconstant +socks5-version+ 5)
(defconstant +socks5-reserved+ 0)
(defconstant +socks5-no-auth+ 0)
(defconstant +socks5-connect+ 1)
(defconstant +socks5-domainname+ 3)
(defconstant +socks5-succeeded+ 0)
(defconstant +socks5-ipv4+ 1)
(defconstant +socks5-ipv6+ 4)

(defun ensure-socks5-connected (input output uri http-method)
  (labels ((fail (condition &key reason)
             (error (make-condition condition
                                    :body nil :status nil :headers nil
                                    :uri uri
                                    :method http-method
                                    :reason reason)))
           (exact (n reason)
             (unless (eql n (read-byte input nil 'eof))
               (fail 'socks5-proxy-request-failed :reason reason)))
           (drop (n reason)
             (dotimes (i n)
               (when (eq (read-byte input nil 'eof) 'eof)
                 (fail 'socks5-proxy-request-failed :reason reason)))))
    ;; Send Version + Auth Method
    ;; Currently, only supports no-auth method.
    (write-byte +socks5-version+ output)
    (write-byte 1 output)
    (write-byte +socks5-no-auth+ output)
    (finish-output output)

    ;; Receive Auth Method
    (exact +socks5-version+ "Unexpected version")
    (exact +socks5-no-auth+ "Unsupported auth method")

    ;; Send domainname Request
    (let* ((host (epsilon.lib.char:string-to-u8 (uri:host uri)))
           (hostlen (length host))
           (port (uri:port uri)))
      (unless (<= 1 hostlen 255)
        (fail 'socks5-proxy-request-failed :reason "domainname too long"))
      (unless (<= 1 port 65535)
        (fail 'socks5-proxy-request-failed :reason "Invalid port"))
      (write-byte +socks5-version+ output)
      (write-byte +socks5-connect+ output)
      (write-byte +socks5-reserved+ output)
      (write-byte +socks5-domainname+ output)
      (write-byte hostlen output)
      (write-sequence host output)
      (write-byte (ldb (byte 8 8) port) output)
      (write-byte (ldb (byte 8 0) port) output)
      (finish-output output)

      ;; Receive reply
      (exact +socks5-version+ "Unexpected version")
      (exact +socks5-succeeded+ "Unexpected result code")
      (drop 1 "Should be reserved byte")
      (let ((atyp (read-byte input nil 'eof)))
        (cond
          ((eql atyp +socks5-ipv4+)
           (drop 6 "Should be IPv4 address and port"))
          ((eql atyp +socks5-ipv6+)
           (drop 18 "Should be IPv6 address and port"))
          ((eql atyp +socks5-domainname+)
           (let ((n (read-byte input nil 'eof)))
             (when (eq n 'eof)
               (fail 'socks5-proxy-request-failed :reason "Invalid domainname length"))
             (drop n "Should be domainname and port")))
          (t
           (fail 'socks5-proxy-request-failed :reason "Unknown address")))))))

(defun make-ssl-stream (stream ca-path ssl-key-file ssl-cert-file ssl-key-password hostname insecure)
  (epsilon.net.tls:ensure-initialized)
  (let ((ctx (epsilon.net.tls:make-context :verify-mode
                                           (if insecure
                                               epsilon.net.tls:+ssl-verify-none+
                                               epsilon.net.tls:+ssl-verify-peer+)
                                           :verify-location
                                           (cond
                                             (ca-path ca-path)
                                             ((epsilon.sys.fs:file-p (uri:path (ca-bundle))) (uri:path (ca-bundle)))
                                             ;; In executable environment, perhaps *ca-bundle* doesn't exist.
                                             (t :default))))
        (ssl-cert-pem-p (and ssl-cert-file
                             (ends-with-subseq ".pem" ssl-cert-file))))
    (epsilon.net.tls:with-global-context (ctx :auto-free-p t)
      (when ssl-cert-pem-p
        (epsilon.net.tls:use-certificate-chain-file ssl-cert-file))
      (epsilon.net.tls:make-ssl-client-stream stream
                                              :hostname hostname
                                              :verify (not insecure)
                                              :key ssl-key-file
                                              :certificate (and (not ssl-cert-pem-p)
                                                                ssl-cert-file)
                                              :password ssl-key-password))))

(defstruct socket-wrapped-stream
  stream)

;; Forward methods the user might want to use on this.
;; User is not meant to interact with this object except
;; potentially to close it when they decide they don't
;; need the :keep-alive connection anymore.
(defmethod close ((u socket-wrapped-stream) &key abort)
  (close (socket-wrapped-stream-stream u) :abort abort))

(defmethod open-stream-p ((u socket-wrapped-stream))
  (open-stream-p (socket-wrapped-stream-stream u)))

(defun request (uri &rest args
                &key (method :get) (version 1.1)
                  content headers
                  basic-auth
                  (connect-timeout *default-connect-timeout*) (read-timeout *default-read-timeout*)
                  (keep-alive t) (use-connection-pool t)
                  (max-redirects 5)
                  ssl-key-file ssl-cert-file ssl-key-password
                  stream (verbose *verbose*)
                  force-binary
                  force-string
                  want-stream
                  (proxy *default-proxy*)
                  (insecure *not-verify-ssl*)
                  ca-path
                &aux
                  (proxy-uri (and proxy (uri:uri proxy)))
                  (original-user-supplied-stream stream)
                  (user-supplied-stream (if (socket-wrapped-stream-p stream) (socket-wrapped-stream-stream stream) stream)))
  (declare (ignorable ssl-key-file ssl-cert-file ssl-key-password
                      connect-timeout ca-path)
           (type real version)
           (type fixnum max-redirects))
  (with-content-caches
    (labels ((make-new-connection (uri)
               (restart-case
                   (let* ((con-uri (uri:uri (or proxy uri)))
                          (connection (epsilon.net.socket:socket-connect (uri:host con-uri)
                                                                         (uri:port con-uri)
                                                                         :timeout connect-timeout
                                                                         :element-type 'u8))
                          (stream
                            (epsilon.net.socket:socket-stream connection))
                          (scheme (uri:scheme uri)))
                     (declare (type string scheme))
                     (when read-timeout
                       (setf (epsilon.net.socket:socket-option connection :receive-timeout) read-timeout))
                     (when (socks5-proxy-p proxy-uri)
                       (ensure-socks5-connected stream stream uri method))
                     (if (string= scheme "https")
                         (make-ssl-stream (if (http-proxy-p proxy-uri)
                                              (make-connect-stream uri version stream (make-proxy-authorization con-uri))
                                              stream) ca-path ssl-key-file ssl-cert-file ssl-key-password (uri:host uri) insecure)
                         stream))
                 (retry-request ()
                   :report "Retry the same request."
                   (return-from request
                     (apply #'request uri :use-connection-pool nil args)))
                 (retry-insecure ()
                   :report "Retry the same request without checking for SSL certificate validity."
                   (return-from request
                     (apply #'request uri :use-connection-pool nil :insecure t args)))))
             (http-proxy-p (uri)
               (and uri
                    (let ((scheme (uri:scheme uri)))
                      (and (stringp scheme)
                           (or (string= scheme "http")
                               (string= scheme "https"))))))
             (socks5-proxy-p (uri)
               (and uri
                    (let ((scheme (uri:scheme uri)))
                      (and (stringp scheme)
                           (string= scheme "socks5")))))
             (connection-keep-alive-p (connection-header)
               (and keep-alive
                    (or (and (= (the real version) 1.0)
                             (equalp connection-header "keep-alive"))
                        (not (equalp connection-header "close")))))
             (return-stream-to-pool (stream uri)
               (push-connection (format nil "~A://~A"
                                        (uri:scheme uri)
                                        (uri:authority uri)) stream #'close))
             (return-stream-to-pool-or-close (stream connection-header uri)
               (if (and (not user-supplied-stream) use-connection-pool (connection-keep-alive-p connection-header))
                   (return-stream-to-pool stream uri)
                   (when (open-stream-p stream)
                     (close stream))))
             (finalize-connection (stream connection-header uri)
               "If KEEP-ALIVE is in the connection-header and the user is not requesting a stream,
              we will push the connection to our connection pool if allowed, otherwise we return
              the stream back to the user who must close it."
               (unless want-stream
                 (cond
                   ((and use-connection-pool (connection-keep-alive-p connection-header) (not user-supplied-stream))
                    (return-stream-to-pool stream uri))
                   ((not (connection-keep-alive-p connection-header))
                    (when (open-stream-p stream)
                      (close stream)))))))
      (let* ((uri (uri:uri uri))
             (proxy (when (http-proxy-p proxy-uri) proxy))
             (content-type (cdr (find :content-type headers :key #'car :test #'string-equal)))
             (multipart-p (or (and content-type
                                   (>= (length content-type) 10)
				   (string= content-type "multipart/" :end1 10))
                              (and (not content-type)
                                   (consp content)
                                   (find-if #'pathnamep content :key #'cdr))))
             (form-urlencoded-p (or (string= content-type "application/x-www-form-urlencoded")
                                    (and (not content-type)
                                         (consp content)
                                         (not multipart-p))))
             (boundary (and multipart-p
                            (random-string 12)))
             (content (if (and form-urlencoded-p (not (stringp content))) ;; user can provide already encoded content, trust them.
                          (uri:url-encode-params content)
                          content))
             (stream (or user-supplied-stream
                         (and use-connection-pool
                              (steal-connection (format nil "~A://~A"
                                                        (uri:scheme uri)
                                                        (uri:authority uri))))))
             (reusing-stream-p (not (null stream))) ;; user provided or from connection-pool
             (stream (or stream
                         (make-new-connection uri)))
             (content-length
               (assoc :content-length headers :test #'string-equal))
             (transfer-encoding
               (assoc :transfer-encoding headers :test #'string-equal))
             (chunkedp (or (and transfer-encoding
                                (equalp (cdr transfer-encoding) "chunked"))
                           (and content-length
                                (null (cdr content-length)))))
             (first-line-data
               (with-fast-output (buffer)
                 (write-first-line method uri version buffer)))
             (headers-data
               (flet ((write-header* (name value)
                        (let ((header (assoc name headers :test #'string-equal)))
                          (if header
                              (when (cdr header)
                                (write-header name (cdr header)))
                              (write-header name value)))
                        (values)))
                 (with-header-output (buffer)
                   (write-header* :user-agent #.*default-user-agent*)
                   (write-header* :host (uri:authority uri))
                   (write-header* :accept "*/*")
                   (cond
                     ((and keep-alive
                           (= (the real version) 1.0))
                      (write-header* :connection "keep-alive"))
                     ((and (not keep-alive)
                           (= (the real version) 1.1))
                      (write-header* :connection "close")))
                   (when basic-auth
                     (write-header* :authorization
                                    (format nil "Basic ~A"
                                            (string-to-base64-string
                                             (format nil "~A:~A"
                                                     (car basic-auth)
                                                     (cdr basic-auth))))))
                   (when proxy
                     (let ((scheme (uri:scheme uri)))
                       (when (string= scheme "http")
                         (let* ((uri (uri:uri proxy))
                                (proxy-authorization (make-proxy-authorization uri)))
                           (when proxy-authorization
                             (write-header* :proxy-authorization proxy-authorization))))))
                   (cond
                     (multipart-p
                      (write-header* :content-type (format nil "~A; boundary=~A"
                                                           (or content-type "multipart/form-data")
                                                           boundary))
                      (unless chunkedp
                        (write-header* :content-length
                                       (multipart-content-length content boundary))))
                     (form-urlencoded-p
                      (write-header* :content-type "application/x-www-form-urlencoded")
                      (unless chunkedp
                        (write-header* :content-length (length (the string content)))))
                     (t
                      (etypecase content
                        (null
                         (unless chunkedp
                           (write-header* :content-length 0)))
                        (string
                         (write-header* :content-type (or content-type "text/plain"))
                         (unless chunkedp
                           (write-header* :content-length (content-length content))))
                        ((array u8 *)
                         (write-header* :content-type (or content-type "text/plain"))
                         (unless chunkedp
                           (write-header* :content-length (length content))))
                        (pathname
                         (write-header* :content-type (or content-type (content-type content)))
                         (unless chunkedp
                           (write-header :content-length
                                         (or (cdr (assoc :content-length headers :test #'string-equal))
                                             (content-length content))))))))
                   ;; Transfer-Encoding: chunked
                   (when (and chunkedp
                              (not transfer-encoding))
                     (write-header* :transfer-encoding "chunked"))

                   ;; Custom headers
                   (loop for (name . value) in headers
                         unless (member name '(:user-agent :host :accept
                                               :connection
                                               :content-type :content-length) :test #'string-equal)
                           do (write-header name value))))))
        (macrolet ((maybe-try-again-without-reusing-stream (&optional (force nil))
                     `(progn ;; retrying by go retry avoids generating the header, parsing, etc.
                        (when (open-stream-p stream)
                          (close stream :abort t)
                          (setf stream nil))
                        
                        (when ,(or force 'reusing-stream-p)
                          (setf reusing-stream-p nil
                                user-supplied-stream nil
                                stream (make-new-connection uri))
                          (go retry))))
                   (try-again-without-reusing-stream ()
                     `(maybe-try-again-without-reusing-stream t))
                   (with-retrying (&body body)
                     `(restart-case
                          (handler-bind (((and error
                                               ;; We should not retry errors received from the server.
                                               ;; Only technical errors such as disconnection or some
                                               ;; problems with the protocol should be retried automatically.
                                               ;; This solves https://github.com/fukamachi/epsilon.net.http/issues/137 issue.
                                               (not http-request-failed))
                                           (lambda (e)
                                             (declare (ignorable e))
                                             (maybe-try-again-without-reusing-stream))))
                            ,@body)
                        (retry-request () :report "Retry the same request."
                          (return-from request (apply #'request uri args)))
                        (ignore-and-continue () :report "Ignore the error and continue."))))
          (tagbody
           retry

             (unless (open-stream-p stream)
               (try-again-without-reusing-stream))
             
             (with-retrying
                 (write-sequence first-line-data stream)
               (write-sequence headers-data stream)
               (write-sequence +crlf+ stream)
               (force-output stream))

             ;; Sending the content
             (when content
               (let ((stream (if chunkedp
                                 (make-chunked-stream stream)
                                 stream)))
                 (when chunkedp
                   (setf (chunked-stream-output-chunking-p stream) t))
                 (with-retrying
                     (if (consp content)
                         (write-multipart-content content boundary stream)
                         (write-as-octets stream content))
                   (when chunkedp
                     (setf (chunked-stream-output-chunking-p stream) nil))
                   (finish-output stream))))

           start-reading
             (multiple-value-bind (http body response-headers-data transfer-encoding-p)
                 (with-retrying
                     (read-response stream (not (eq method :head)) verbose (not want-stream)))
               (let* ((status (http-status http))
                      (response-headers (http-headers http))
                      (content-length (gethash "content-length" response-headers))
                      (content-length (etypecase content-length
                                        (null content-length)
                                        (string (parse-integer content-length))
                                        (integer content-length))))
                 (when (= status 0)
                   (with-retrying
                       (http-request-failed status
                                            :body body
                                            :headers headers
                                            :uri uri
                                            :method method)))
                 (when verbose
                   (print-verbose-data :outgoing first-line-data headers-data +crlf+)
                   (print-verbose-data :incoming response-headers-data))
                 
                 (when (and (member status '(301 302 303 307 308) :test #'=)
                            (gethash "location" response-headers)
                            (/= max-redirects 0))
                   ;; Need to read the response body
                   (when (and want-stream
                              (not (eq method :head)))
                     (cond
                       ((integerp content-length)
                        (dotimes (i content-length)
                          (loop until (read-byte body nil nil))))
                       (transfer-encoding-p
                        (read-until-crlf*2 body))))

                   (let* ((location-uri (uri:uri (gethash "location" response-headers)))
                          (same-server-p (or (null (uri:host location-uri))
                                             (and (string= (uri:scheme location-uri)
                                                           (uri:scheme uri))
                                                  (string= (uri:host location-uri)
                                                           (uri:host uri))
                                                  (eql (uri:port location-uri)
                                                       (uri:port uri))))))
                     (if (and same-server-p
                              (or (= status 307) (= status 308)
                                  (member method '(:get :head) :test #'eq)))
                         (progn ;; redirection to the same host
                           (setq uri (uri:merge uri location-uri))
                           (setq first-line-data
                                 (with-fast-output (buffer)
                                   (write-first-line method uri version buffer)))
                           (decf max-redirects)
                           (if (equalp (gethash "connection" response-headers) "close")
                               (try-again-without-reusing-stream)
                               (progn
                                 (setq reusing-stream-p t)
                                 (go retry))))
                         (progn ;; this is a redirection to a different host
                           (setf location-uri (uri:merge uri location-uri))
                           ;; Close connection if it isn't from our connection pool or from the user and we aren't going to
                           ;; pass it to our new call.
                           (when (not same-server-p) (return-stream-to-pool-or-close stream (gethash "connection" response-headers) uri))
                           (setf (getf args :headers)
                                 (nconc `((:host . ,(uri:host location-uri))) headers))
                           (setf (getf args :max-redirects)
                                 (1- max-redirects))
                           ;; Redirect as GET if it's 301, 302, 303
                           (unless (or (= status 307) (= status 308)
                                       (member method '(:get :head) :test #'eq))
                             (setf (getf args :method) :get))
                           (return-from request
                             (apply #'request location-uri (if same-server-p
                                                               args
                                                               (progn (remf args :stream) args))))))))
                 (unwind-protect
                      (let* ((keep-connection-alive (connection-keep-alive-p
                                                     (gethash "connection" response-headers)))
                             (body (convert-body body
                                                 (gethash "content-encoding" response-headers)
                                                 (gethash "content-type" response-headers)
                                                 content-length
                                                 transfer-encoding-p
                                                 force-binary
                                                 force-string
                                                 keep-connection-alive
                                                 (if (and use-connection-pool keep-connection-alive (not user-supplied-stream) (streamp body))
                                                     (lambda (underlying-stream abort)
                                                       (declare (ignore abort))
                                                       (when (and underlying-stream (open-stream-p underlying-stream))
                                                         ;; read any left overs the user may have not read (in case of errors on user side?)
                                                         (loop while (ignore-errors (listen underlying-stream)) ;; ssl streams may close
                                                               do (read-byte underlying-stream nil nil))
                                                         (when (open-stream-p underlying-stream)
                                                           (push-connection (format nil "~A://~A"
                                                                                    (uri:scheme uri)
                                                                                    (uri:authority uri)) underlying-stream #'close))))
                                                     #'keep-alive-stream-close-underlying-stream))))
                        ;; Raise an error when the HTTP response status code is 4xx or 50x.
                        (when (<= 400 status)
                          (with-retrying
                              (http-request-failed status
                                                   :body body
                                                   :headers response-headers
                                                   :uri uri
                                                   :method method)))
                        ;; Have to be a little careful with the fifth value stream we return --
                        ;; the user may be not aware that keep-alive t without use-connection-pool can leak
                        ;; sockets, so we wrap the returned last value so when it is garbage
                        ;; collected it gets closed.  If the user is getting a stream back as BODY,
                        ;; then we instead add a finalizer to that stream to close it when garbage collected
                        (return-from request
                          (values body
                                  status
                                  response-headers
                                  uri
                                  (when (and keep-alive
                                             (not (equalp (gethash "connection" response-headers) "close"))
                                             (or (not use-connection-pool) user-supplied-stream))
                                    (or (and original-user-supplied-stream ;; user provided a stream
					     (if (socket-wrapped-stream-p original-user-supplied-stream) ;; but, it came from us
					         (eql (socket-wrapped-stream-stream original-user-supplied-stream) stream) ;; and we used it
					         (eql original-user-supplied-stream stream)) ;; user provided a bare stream
					     original-user-supplied-stream) ;; return what the user sent without wrapping it
                                        (if want-stream ;; add a finalizer to the body to close the stream
                                            (progn
                                              (epsilon.sys.gc:finalize body (lambda () (close stream)))
                                              stream)
                                            (let ((wrapped-stream (make-socket-wrapped-stream :stream stream)))
                                              (epsilon.sys.gc:finalize wrapped-stream (lambda () (close stream)))
                                              wrapped-stream)))))))
                   (finalize-connection stream (gethash "connection" response-headers) uri))))))))))

(defun get (uri &rest args
            &key version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout max-redirects
              force-binary force-string want-stream content
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  "Make a GET request to URI and return
    (values body-or-stream status response-headers uri &optional opaque-socket-stream)

  You may pass a real stream in as STREAM if you want us to communicate with the server via it --
  though if any errors occur, we will open a new connection to the server.  If you have a previous
  OPAQUE-SOCKET-STREAM you can pass that in as STREAM as well and we will re-use that connection.

  OPAQUE-SOCKET-STREAM is not returned if USE-CONNECTION-POOL is T, instead we keep track of it and
  re-use it when needed.

  If WANT-STREAM is T, then a STREAM is returned as the first value.  You may read this as needed to
  get the body of the response.  If KEEP-ALIVE and USE-CONNECTION-POOL are T, then the stream will be
  returned to the connection pool when you have read all the data or closed the stream. If KEEP-ALIVE
  is NIL then you are responsible for closing the stream when done.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is NIL, then the fifth value returned is a stream which
  you can then pass in again using the STREAM option to re-use the active connection.  If you ignore
  the stream, it will get closed during garbage collection.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is T, then there is no fifth
  value (OPAQUE-SOCKET-STREAM) returned, but the active connection to the host/port may be reused in
  subsequent calls.  This removes the need for the caller to keep track of the active socket-stream
  for subsequent calls.

  While CONTENT is allowed in a GET request the results are ill-defined and not advised."
  (declare (ignore version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout max-redirects force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path content))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout
               force-binary force-string want-stream
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers basic-auth connect-timeout read-timeout max-redirects
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth connect-timeout read-timeout max-redirects ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout
              force-binary force-string want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :put args))

(defun patch (uri &rest args
              &key version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout
                force-binary force-string want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :patch args))

(defun delete (uri &rest args
               &key version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout
                 force-binary force-string want-stream content
                 ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path content))
  (apply #'request uri :method :delete args))

(defun fetch (uri destination &rest args
                  &key (if-exists :error)
                    version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout max-redirects
                    ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth keep-alive use-connection-pool connect-timeout read-timeout max-redirects ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (unless (and (eql if-exists nil)
               (epsilon.sys.fs:file-p destination))
    (with-open-file (out destination
                         :direction :output :element-type 'u8
                         :if-exists if-exists
                         :if-does-not-exist :create)
      (let ((body (apply #'get uri :want-stream t :force-binary t
                         (remove-from-plist args :if-exists))))
        (copy-stream body out)
        ;; Nominally the body gets closed, but if keep-alive is nil we need to explicitly do it.
        (when (open-stream-p body)
          (close body))))))

(defun ignore-and-continue (e)
  (let ((restart (find-restart 'ignore-and-continue e)))
    (when restart
      (invoke-restart restart))))

(defun retry-request (times &key (interval 3))
  (declare (type (or function integer) interval))
  (etypecase times
    (condition
     (let ((restart (find-restart 'retry-request times)))
       (when restart
         (invoke-restart restart))))
    (integer
     (retry-request-ntimes times :interval interval))))

(defun retry-request-ntimes (n &key (interval 3))
  (declare (type integer n)
           (type (or function integer) interval))
  (let ((retries 0))
    (declare (type integer retries))
    (lambda (e)
      (declare (type condition e))
      (let ((restart (find-restart 'retry-request e)))
        (when restart
          (when (< retries n)
            (incf retries)
            (etypecase interval
              (function (funcall interval retries))
              (integer (sleep interval)))
            (invoke-restart restart)))))))
