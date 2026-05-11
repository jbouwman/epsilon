;;;; PEM Encoding/Decoding (RFC 7468)
;;;;
;;;; Implements PEM armor/dearmor for DER-encoded cryptographic objects.
;;;; Supports CERTIFICATE, PRIVATE KEY, PUBLIC KEY, RSA PRIVATE KEY, etc.

(defpackage epsilon.crypto.pem
  (:use :cl)
  (:export
   #:pem-block
   #:make-pem-block
   #:pem-block-label
   #:pem-block-data
   #:pem-block-headers
   #:pem-encode
   #:pem-decode
   #:pem-decode-all))

(in-package :epsilon.crypto.pem)

;;; ---------------------------------------------------------------------------
;;; PEM block structure
;;; ---------------------------------------------------------------------------

(defstruct (pem-block (:constructor %make-pem-block))
  (label "" :type string)
  (data #() :type (simple-array (unsigned-byte 8) (*)))
  ;; Alist of (name . value) pairs. Present when the PEM armor carries
  ;; RFC 1421-style header lines between BEGIN and the blank separator,
  ;; e.g. "Proc-Type: 4,ENCRYPTED" / "DEK-Info: AES-256-CBC,<hex IV>".
  (headers nil :type list))

(defun make-pem-block (label data &key headers)
  (%make-pem-block :label label :data data :headers (copy-alist headers)))

;;; ---------------------------------------------------------------------------
;;; Base64 encoding/decoding
;;; ---------------------------------------------------------------------------

(defparameter +b64-chars+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun b64-encode (data)
  "Encode byte array to base64 string."
  (let ((len (length data))
        (result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for i from 0 below len by 3
          for b0 = (aref data i)
          for b1 = (if (< (1+ i) len) (aref data (1+ i)) 0)
          for b2 = (if (< (+ i 2) len) (aref data (+ i 2)) 0)
          for triple = (logior (ash b0 16) (ash b1 8) b2)
          do (vector-push-extend (char +b64-chars+ (logand (ash triple -18) #x3F)) result)
             (vector-push-extend (char +b64-chars+ (logand (ash triple -12) #x3F)) result)
             (if (< (1+ i) len)
                 (vector-push-extend (char +b64-chars+ (logand (ash triple -6) #x3F)) result)
                 (vector-push-extend #\= result))
             (if (< (+ i 2) len)
                 (vector-push-extend (char +b64-chars+ (logand triple #x3F)) result)
                 (vector-push-extend #\= result)))
    (coerce result 'string)))

(defun b64-decode-char (c)
  "Decode a single base64 character to its 6-bit value."
  (cond
    ((<= (char-code #\A) (char-code c) (char-code #\Z))
     (- (char-code c) (char-code #\A)))
    ((<= (char-code #\a) (char-code c) (char-code #\z))
     (+ 26 (- (char-code c) (char-code #\a))))
    ((<= (char-code #\0) (char-code c) (char-code #\9))
     (+ 52 (- (char-code c) (char-code #\0))))
    ((char= c #\+) 62)
    ((char= c #\/) 63)
    (t nil)))

(defun b64-decode (string)
  "Decode base64 string to byte array."
  ;; Strip whitespace
  (let* ((clean (remove-if (lambda (c) (or (char= c #\Newline)
                                            (char= c #\Return)
                                            (char= c #\Space)
                                            (char= c #\Tab)))
                           string))
         (len (length clean))
         (result (make-array (* 3 (ceiling len 4))
                             :element-type '(unsigned-byte 8)
                             :fill-pointer 0)))
    (loop for i from 0 below len by 4
          for c0 = (b64-decode-char (char clean i))
          for c1 = (and (< (1+ i) len) (b64-decode-char (char clean (1+ i))))
          for c2 = (and (< (+ i 2) len) (b64-decode-char (char clean (+ i 2))))
          for c3 = (and (< (+ i 3) len) (b64-decode-char (char clean (+ i 3))))
          when (and c0 c1)
          do (vector-push-extend (logand #xFF (ash (logior (ash c0 2) (ash c1 -4)) 0)) result)
             (when c2
               (vector-push-extend (logand #xFF (logior (ash (logand c1 #xF) 4)
                                                         (ash c2 -2))) result)
               (when c3
                 (vector-push-extend (logand #xFF (logior (ash (logand c2 #x3) 6) c3)) result))))
    (let ((final (make-array (length result) :element-type '(unsigned-byte 8))))
      (replace final result)
      final)))

;;; ---------------------------------------------------------------------------
;;; PEM encoding
;;; ---------------------------------------------------------------------------

(defun pem-encode (block)
  "Encode a PEM block to a string."
  (let ((b64 (b64-encode (pem-block-data block)))
        (label (pem-block-label block)))
    (with-output-to-string (s)
      (format s "-----BEGIN ~a-----~%" label)
      ;; Split base64 into 64-character lines
      (loop for i from 0 below (length b64) by 64
            do (write-string (subseq b64 i (min (+ i 64) (length b64))) s)
               (terpri s))
      (format s "-----END ~a-----~%" label))))

;;; ---------------------------------------------------------------------------
;;; PEM decoding
;;; ---------------------------------------------------------------------------

(defun pem-decode (string)
  "Decode the first PEM block from a string.
   Returns a pem-block or NIL if no PEM block found."
  (let ((blocks (pem-decode-all string)))
    (first blocks)))

(defun %trim-cr (line)
  (let ((end (length line)))
    (if (and (plusp end) (char= (char line (1- end)) #\Return))
        (subseq line 0 (1- end))
        line)))

(defun %split-headers (body)
  "Parse RFC 1421 header lines from the start of BODY. Returns
(values alist data-offset). Headers end at the first blank line; if no
blank line appears, BODY has no headers and data-offset is 0. Continuation
lines (indented by whitespace) are folded onto the previous header value."
  (let ((pos 0)
        (len (length body))
        (headers nil))
    (loop
      (let* ((eol (position #\Newline body :start pos))
             (line-end (or eol len))
             (line (%trim-cr (subseq body pos line-end))))
        ;; Blank line ends the header block.
        (when (zerop (length line))
          (return (values (nreverse headers) (if eol (1+ eol) len))))
        (let ((colon (position #\: line)))
          (cond
            ;; Continuation line: whitespace-prefixed, folds onto prior header.
            ((and headers
                  (plusp (length line))
                  (or (char= (char line 0) #\Space)
                      (char= (char line 0) #\Tab)))
             (let ((tail (string-left-trim '(#\Space #\Tab) line)))
               (setf (cdar headers)
                     (concatenate 'string (cdar headers) " " tail))))
            ;; Not a header: no headers present, stop and report zero offset
            ;; so the caller treats the whole body as base64.
            ((null colon)
             (return (values nil 0)))
            (t
             (let ((name (subseq line 0 colon))
                   (value (string-left-trim '(#\Space #\Tab)
                                            (subseq line (1+ colon)))))
               (push (cons name value) headers)))))
        (unless eol (return (values (nreverse headers) len)))
        (setf pos (1+ eol))))))

(defun pem-decode-all (string)
  "Decode all PEM blocks from a string."
  (let ((blocks nil)
        (pos 0))
    (loop
      ;; Find -----BEGIN
      (let ((begin-pos (search "-----BEGIN " string :start2 pos)))
        (unless begin-pos (return))
        ;; Find the label
        (let* ((label-start (+ begin-pos 11))
               (label-end (search "-----" string :start2 label-start)))
          (unless label-end (return))
          (let ((label (subseq string label-start label-end)))
            ;; Find the end of the BEGIN line
            (let ((body-start (1+ (or (position #\Newline string :start label-end)
                                      (return)))))
              ;; Find -----END
              (let ((end-marker (format nil "-----END ~a-----" label)))
                (let ((end-pos (search end-marker string :start2 body-start)))
                  (unless end-pos (return))
                  ;; Split any RFC 1421 header block from the body; base64
                  ;; starts after the blank line that terminates the headers.
                  (let ((body (subseq string body-start end-pos)))
                    (multiple-value-bind (headers data-offset)
                        (%split-headers body)
                      (let* ((b64-data (subseq body data-offset))
                             (der-data (b64-decode b64-data)))
                        (push (make-pem-block label der-data :headers headers)
                              blocks))))
                  (setf pos (+ end-pos (length end-marker))))))))))
    (nreverse blocks)))
