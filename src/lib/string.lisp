(defpackage #:epsilon.lib.string
  (:use :cl)
  (:export
   #:concat
   #:first-char
   #:last-char
   #:join
   #:split
   #:random-string
   #:ends-with-p
   #:starts-with-p
   #:string-designator
   #:strip
   #:strip-left
   #:strip-right))

(in-package #:epsilon.lib.string)

(defun join (sep components)
  (let ((s (make-string (+ (apply #'+ (mapcar #'length components))
                           (1- (length components)))
                        :initial-element sep))
        (offset 0))
    (dolist (c components)
      (dotimes (i (length c))
        (setf (aref s (+ i offset))
              (aref c i)))
      (incf offset (1+ (length c))))
    s))

(defun split (sep string)
  (loop :with p := 0
        :for i :from 0 :to (length string)
        :when (or (= i (length string))
                  (char= (aref string i) sep))
          :collect (let ((s (subseq string p i)))
                     (setf p (1+ i))
                     s)))

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defun concat (&rest strings)
  (let ((output (make-string (apply #'+ (mapcar #'length strings))))
        (offset 0))
    (dolist (string strings output)
      (dotimes (i (length string))
        (setf (aref output (+ offset i))
              (aref string i)))
      (incf offset (length string)))))

(defun random-string (&optional (length 12))
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))

(defun first-char (s)
  "Return the first character of a non-empty string S, or NIL"
  (and (stringp s) (plusp (length s)) (char s 0)))

(defun last-char (s)
  "Return the last character of a non-empty string S, or NIL"
  (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

(defun first-index (string char-fn)
  (loop :for i :from 0 :to (1- (length string))
        :when (funcall char-fn (aref string i))
          :return (when (< 0 i)
                    i)))

(defun last-index (string char-fn)
  (loop :for i :from (length string) :downto 0
        :when (funcall char-fn (aref string (1- i)))
          :return (when (< i (length string))
                    i)))

(defun strip-right (string c)
  (let ((index (last-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string 0 index)
        string)))

;; todo when-let

(defun strip-left (string c)
  (let ((index (first-index string (lambda (test-c)
                                    (not (char= test-c c))))))
    (if index
        (subseq string index)
        string)))

;; todo ->

(defun strip (string c)
  (strip-right (strip-left string c) c))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun starts-with-p (seq prefix &key (test #'char-equal))
  "Returns true if the sequence SEQ starts with the sequence
PREFIX whereby the elements are compared using TEST."
  (let ((mismatch (mismatch seq prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))
