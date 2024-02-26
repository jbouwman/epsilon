(in-package #:encode)

(defmacro with-u8-in ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'encode:u8)
     ,@body))

(defmacro with-u8-out ((f in) &body body)
  `(with-open-file (,f ,in :element-type 'encode:u8
                           :direction :output
                           :if-exists :supersede)
     ,@body))

(defun stream-files (f in out)
  (with-u8-in (in in)
    (with-u8-out (out out)
      (funcall f in out))))

(defun stream= (a b)
  (loop :for ab := (read-byte a nil nil)
        :for bb := (read-byte b nil nil)
        :unless (eql ab bb)
          :return nil
        :unless (and ab bb)
          :return t))

(defun file= (a b)
  (with-u8-in (a a)
    (with-u8-in (b b)
      (stream= a b))))
