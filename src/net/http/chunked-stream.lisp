(defpackage #:net.http.chunked-stream
  (:use
   #:cl
   #:sb-gray
   #:lib.binding)
  (:export
   :chunked-input-stream
   :chunked-io-stream
   :chunked-output-stream
   :chunked-stream
   :chunked-stream-input-chunking-p
   :chunked-stream-output-chunking-p
   :make-chunked-stream))

(in-package #:net.http.chunked-stream)

(defconstant +output-buffer-size+ 8192)

(define-constant +crlf+
  (make-array 2 :element-type '(unsigned-byte 8)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-keyword (string destructivep)
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Destructively modifies STRING if DESTRUCTIVEP is true."
  (intern (funcall
           (if destructivep
             (if (eq (readtable-case *readtable*) :upcase)
               #'nstring-upcase
               #'nstring-downcase)
             (if (eq (readtable-case *readtable*) :upcase)
               #'string-upcase
               #'string-downcase))
           string)
          :keyword))
)

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
          do (setf (gethash word hash) (make-keyword word nil)))
    hash)
  "A hash table which case-insensitively maps the strings from
+KNOWN-WORDS+ to keywords.")

(define-constant +keyword-to-string-hash+
  (let ((hash (make-hash-table :test 'eq :size (length +known-words+))))
    (loop for word in +known-words+
          do (setf (gethash (make-keyword word nil) hash)
                   (string-capitalize word)))
    hash)
  "A hash table which maps keywords derived from +KNOWN-WORDS+ to
capitalized strings.")

(defun as-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (or (gethash string +string-to-keyword-hash+)
      (make-keyword string destructivep)))

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
  (declare (optimize
            speed
            (space 0)
            (debug 1)
            (compilation-speed 0)
            #+:lispworks (hcl:fixnum-safety 0)))
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
   (output-buffer :initform (make-array +output-buffer-size+ :element-type '(unsigned-byte 8))
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
  '(unsigned-byte 8))                   ; TODO u8

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
                    ;;:last-char last-char
                    ;;:expected-chars expected-chars
                    ))))
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
                 (setq input-buffer (make-array chunk-size :element-type '(unsigned-byte 8)))))
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
