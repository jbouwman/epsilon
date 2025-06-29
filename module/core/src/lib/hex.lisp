(defpackage #:epsilon.lib.hex
  (:use
   #:cl
   #:epsilon.lib.type)
  (:export
   #:u8-to-hex
   #:hex-to-u8))

;; Hexadecimal string encoding

(in-package #:epsilon.lib.hex)

(defun u8-to-hex (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector u8) vector)
           (type fixnum start)
           (type (or null fixnum) end))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
       for i from start below end
       for j from 0 below (* length 2) by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))

(defun hex-to-u8 (string &key (start 0) (end nil))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type 'u8)))
    (declare (type ->u8 key))
    (flet ((char-to-digit (char)
             (or (position char "0123456789abcdef" :test #'char-equal)
                 (error 'error
                        :format-control "~A is not a hex digit"
                        :format-arguments (list char)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))
