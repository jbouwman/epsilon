(in-package #:epsilon.lib.codec)

(defun make-stream-output-callback (stream)
  "Return a function suitable for use as a compressor callback that
writes all compressed data to STREAM."
  (lambda (buffer end)
    (write-sequence buffer stream :end end)))

(defun compress-stream (compressor-name in out)
  (let ((callback (make-stream-output-callback out))
        (buffer (->u8 8192)))
    (with-compressor (compressor compressor-name
                                 :callback callback)
      (loop :for bytes-read := (read-sequence buffer in)
            :if (zerop bytes-read)
              :return
            :else
            :do (compress-u8-vector buffer compressor :end bytes-read)))))

(defun codec (format)
  (ecase format
    (:bzip2 (make-instance 'bzip2-codec))
    (:deflate (make-instance 'deflate-codec))
    (:zlib (make-instance 'zlib-codec))
    (:gzip (make-instance 'gzip-codec))))

;; Stream-based encode/decode functions
(defmethod encode ((format symbol) in-stream out-stream)
  "Encode data from IN-STREAM to OUT-STREAM using the specified FORMAT codec."
  (let ((codec-instance (codec format)))
    (encode codec-instance in-stream out-stream)))

(defmethod decode ((format symbol) in-stream out-stream)
  "Decode data from IN-STREAM to OUT-STREAM using the specified FORMAT codec."
  (let ((codec-instance (codec format)))
    (decode codec-instance in-stream out-stream)))
