;;;; File Position and Line Tracking Utilities
;;;;
;;;; This module provides consolidated file utilities for position-to-line conversion,
;;;; file reading, and line caching shared across compilation and logging modules.

(defpackage epsilon.file-utils
  (:use cl)
  (:export
   ;; File line cache
   #:file-line-cache
   #:file-line-cache-p
   #:file-line-cache-pathname
   #:file-line-cache-line-starts
   #:file-line-cache-modification-time
   #:*file-line-cache*
   #:clear-file-line-cache
   
   ;; Position-to-line conversion
   #:file-position-to-line-column
   #:estimate-line-number
   #:cache-file-lines
   #:get-cached-file-lines
   
   ;; File reading utilities
   #:read-file-lines
   #:get-file-modification-time
   
   ;; Form position tracking
   #:build-form-position-cache
   #:track-form-positions))

(in-package epsilon.file-utils)

;;; File line cache structure

(defstruct file-line-cache
  "Cache of file line positions for fast lookup."
  (pathname nil :type (or pathname null))
  (line-starts nil :type (or simple-vector null))
  (modification-time nil :type (or integer null)))

(defvar *file-line-cache* (make-hash-table :test 'equal)
  "Global cache mapping pathnames to file-line-cache structures.")

;;; File utilities

(defun get-file-modification-time (pathname)
  "Get the modification time of a file."
  (handler-case
      (file-write-date pathname)
    (error () nil)))

(defun clear-file-line-cache ()
  "Clear the file line cache."
  (clrhash *file-line-cache*))

(defun cache-file-lines (pathname)
  "Cache line positions for a file, returning the cache structure."
  (let ((truename (handler-case (truename pathname)
                    (error () (return-from cache-file-lines nil)))))
    (handler-case
        (let ((mod-time (get-file-modification-time truename))
              (line-starts (make-array 1000 :adjustable t :fill-pointer 0)))
          
          ;; First line starts at position 0
          (vector-push-extend 0 line-starts)
          
          ;; Read file and record line starts
          (with-open-file (stream truename :direction :input)
            (let ((pos 0))
              (loop for char = (read-char stream nil nil)
                    while char
                    do (progn
                         (incf pos)
                         (when (char= char #\Newline)
                           (vector-push-extend pos line-starts))))))
          
          ;; Create and cache the result
          (let ((cache (make-file-line-cache
                        :pathname truename
                        :line-starts (coerce line-starts 'simple-vector)
                        :modification-time mod-time)))
            (setf (gethash truename *file-line-cache*) cache)
            cache))
      (error () nil))))

(defun get-cached-file-lines (pathname)
  "Get cached file line information, updating if necessary."
  (let* ((truename (handler-case (truename pathname)
                     (error () (return-from get-cached-file-lines nil))))
         (cached (gethash truename *file-line-cache*)))
    
    ;; Check if cache is still valid
    (if (and cached
             (equal (file-line-cache-modification-time cached)
                    (get-file-modification-time truename)))
        cached
        ;; Cache is invalid or missing, rebuild it
        (cache-file-lines truename))))

(defun file-position-to-line-column (pathname position)
  "Convert a file position to line and column numbers."
  (when (and pathname position)
    (let ((cache (get-cached-file-lines pathname)))
      (when cache
        (let ((line-starts (file-line-cache-line-starts cache)))
          ;; Binary search for the line containing the position
          (let ((line-num (position-if (lambda (start)
                                          (> start position))
                                        line-starts)))
            (if line-num
                (values line-num
                        (1+ (- position (aref line-starts (1- line-num)))))
                ;; Position is on the last line
                (values (length line-starts)
                        (1+ (- position (aref line-starts (1- (length line-starts)))))))))))))

(defun estimate-line-number (file position)
  "Estimate line number from file position."
  (when (and file position (probe-file file))
    (multiple-value-bind (line column)
        (file-position-to-line-column file position)
      (declare (ignore column))
      line)))

;;; Additional file reading utilities

(defun read-file-lines (pathname)
  "Read all lines from a file, returning them as a list."
  (when (probe-file pathname)
    (handler-case
        (with-open-file (stream pathname :direction :input)
          (loop for line = (read-line stream nil nil)
                while line
                collect line))
      (error () nil))))

;;; Form position tracking

(defun build-form-position-cache (pathname)
  "Build a cache of form positions within a file for compilation tracking."
  (let ((positions (make-hash-table :test 'eq))
        (line-starts (make-array 1000 :adjustable t :fill-pointer 0)))
    
    ;; Record line start positions
    (vector-push-extend 0 line-starts)
    
    (handler-case
        (with-open-file (stream pathname :direction :input)
          (let ((char-pos 0)
                (*read-suppress* nil))
            
            ;; First pass: collect line starts
            (file-position stream 0)
            (loop for char = (read-char stream nil nil)
                  while char
                  do (progn
                       (when (char= char #\Newline)
                         (vector-push-extend (1+ char-pos) line-starts))
                       (incf char-pos)))
            
            ;; Second pass: map forms to positions
            (file-position stream 0)
            (let ((form-start-pos 0))
              (handler-case
                  (loop
                    (setf form-start-pos (file-position stream))
                    (let ((form (read stream nil :eof)))
                      (when (eq form :eof)
                        (return))
                      (setf (gethash form positions) form-start-pos)))
                (error () nil))))) ; Ignore read errors
      (error () nil))
    
    (values positions (coerce line-starts 'simple-vector))))

(defun track-form-positions (forms line-starts)
  "Track forms and their line positions for efficient lookup."
  (let ((form-lines (make-hash-table :test 'eq)))
    (loop for form being the hash-keys of forms
          for pos being the hash-values of forms
          do (let ((line-num (position-if (lambda (start)
                                            (> start pos))
                                          line-starts)))
               (when line-num
                 (setf (gethash form form-lines) line-num))))
    form-lines))