(defpackage #:lib.string
  (:use :cl)
  (:export
   #:concat
   #:ends-with-p
   #:starts-with-p
   #:string-designator
   #:strip
   #:strip-left
   #:strip-right))

(in-package #:lib.string)

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
