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
