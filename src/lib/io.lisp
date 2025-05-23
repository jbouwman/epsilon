(defpackage :epsilon.lib.io
  (:use
   :cl
   :epsilon.lib.syntax)
  (:export
   ;; Re-exports from stream package
   :binary-stream
   :binary-input-stream
   :binary-output-stream
   :make-input-stream
   :make-output-stream
   :copy-stream
   
   ;; Re-exports from reader package
   :reader
   :buffered-reader
   :line-reader
   :make-reader
   :make-string-reader
   :make-file-reader
   :read-char
   :unread-char
   :read-line
   :read-n-chars
   :peek-char
   
   ;; Re-exports from writer package
   :writer
   :buffered-writer
   :print-writer
   :formatted-writer
   :make-writer
   :make-string-writer
   :make-file-writer
   :write-char
   :write-string
   :write-line
   :newline
   :flush
   
   ;; Convenience functions
   :with-open-file*
   :with-input-from-file*
   :with-output-to-file*
   :with-open-reader
   :with-open-writer
   :with-input-from-string*
   :with-output-to-string*))

(in-package :epsilon.lib.io)

;;;; IO Convenience Functions
;;;
;;; This package provides a unified interface for binary streams, character readers,
;;; and character writers, along with convenience functions and macros.

;;; Stream and reader/writer re-exports

;; Re-export from epsilon.lib.stream
(dolist (sym '(:binary-stream :binary-input-stream :binary-output-stream
               :make-input-stream :make-output-stream :copy-stream))
  (import sym :epsilon.lib.stream)
  (export sym))

;; Re-export from epsilon.lib.reader
(dolist (sym '(:reader :buffered-reader :line-reader
               :make-reader :make-string-reader :make-file-reader
               :read-char :unread-char :read-line :read-n-chars :peek-char))
  (import sym :epsilon.lib.reader)
  (export sym))

;; Re-export from epsilon.lib.writer
(dolist (sym '(:writer :buffered-writer :print-writer :formatted-writer
               :make-writer :make-string-writer :make-file-writer
               :write-char :write-string :write-line :newline :flush))
  (import sym :epsilon.lib.writer)
  (export sym))

;;; Convenience macros

(defmacro with-open-file* ((var path &key (direction :input) 
                                          (element-type '(unsigned-byte 8))
                                          (if-exists :error) 
                                          (if-does-not-exist :error))
                          &body body)
  "Open a binary file and execute body with VAR bound to the stream.
   Automatically closes the stream when body exits."
  `(let ((,var (open ,path :direction ,direction 
                          :element-type ,element-type
                          :if-exists ,if-exists
                          :if-does-not-exist ,if-does-not-exist)))
     (unwind-protect
          (progn ,@body)
       (when ,var (close ,var)))))

(defmacro with-input-from-file* ((var path &key (element-type '(unsigned-byte 8))
                                             (if-does-not-exist :error))
                                &body body)
  "Read from a binary file and execute body with VAR bound to the input stream.
   Automatically closes the stream when body exits."
  `(with-open-file* (,var ,path :direction :input 
                              :element-type ,element-type
                              :if-does-not-exist ,if-does-not-exist)
     ,@body))

(defmacro with-output-to-file* ((var path &key (element-type '(unsigned-byte 8))
                                            (if-exists :supersede)
                                            (if-does-not-exist :create))
                               &body body)
  "Write to a binary file and execute body with VAR bound to the output stream.
   Automatically closes the stream when body exits."
  `(with-open-file* (,var ,path :direction :output
                              :element-type ,element-type
                              :if-exists ,if-exists
                              :if-does-not-exist ,if-does-not-exist)
     ,@body))

(defmacro with-open-reader ((var stream-or-path &key (encoding :utf-8)
                                                   (buffered t)
                                                   (track-position nil))
                          &body body)
  "Create a reader and execute body with VAR bound to it.
   Automatically closes the reader when body exits.
   
   STREAM-OR-PATH can be a stream or a pathname. If it's a pathname,
   a file will be opened with the specified encoding."
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,(if (typep stream-or-path 'pathname)
                              `(open ,stream-or-path :direction :input 
                                                   :element-type '(unsigned-byte 8))
                              stream-or-path))
            (,var (epsilon.lib.reader:make-reader ,stream-var 
                                                :encoding ,encoding
                                                :buffered ,buffered
                                                :track-position ,track-position)))
       (unwind-protect
            (progn ,@body)
         (when ,var 
           (epsilon.lib.reader:close-reader ,var))))))

(defmacro with-open-writer ((var stream-or-path &key (encoding :utf-8)
                                                   (buffered t)
                                                   (auto-flush nil)
                                                   (track-position nil)
                                                   (if-exists :supersede))
                          &body body)
  "Create a writer and execute body with VAR bound to it.
   Automatically closes the writer when body exits.
   
   STREAM-OR-PATH can be a stream or a pathname. If it's a pathname,
   a file will be opened with the specified encoding."
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,(if (typep stream-or-path 'pathname)
                              `(open ,stream-or-path :direction :output
                                                   :if-exists ,if-exists
                                                   :element-type '(unsigned-byte 8))
                              stream-or-path))
            (,var (epsilon.lib.writer:make-writer ,stream-var
                                                :encoding ,encoding
                                                :buffered ,buffered
                                                :auto-flush ,auto-flush
                                                :track-position ,track-position)))
       (unwind-protect
            (progn ,@body)
         (when ,var
           (epsilon.lib.writer:close-writer ,var))))))

(defmacro with-input-from-string* ((var string &key (start 0) end
                                              (track-position nil))
                                 &body body)
  "Execute body with VAR bound to a reader that reads from STRING."
  (let ((reader-var (gensym "READER")))
    `(let ((,reader-var (epsilon.lib.reader:make-string-reader ,string
                                                             :track-position ,track-position)))
       (unwind-protect
            (let ((,var ,reader-var))
              ;; Skip to start position
              (when (plusp ,start)
                (epsilon.lib.reader:read-n-chars ,var ,start nil nil))
              
              ;; Execute body
              ,@body)
         (epsilon.lib.reader:close-reader ,reader-var)))))

(defmacro with-output-to-string* (var &body body)
  "Execute body with VAR bound to a writer that collects output in a string.
   Returns the collected string."
  (let ((writer-var (gensym "WRITER"))
        (stream-var (gensym "STREAM")))
    `(let* ((,stream-var (epsilon.lib.stream:make-output-stream))
            (,writer-var (epsilon.lib.writer:make-writer ,stream-var)))
       (unwind-protect
            (let ((,var ,writer-var))
              ,@body)
         (epsilon.lib.writer:flush ,writer-var)
         (epsilon.lib.writer:close-writer ,writer-var))
       (epsilon.lib.char:u8-to-string (epsilon.lib.stream:buffer ,stream-var)))))