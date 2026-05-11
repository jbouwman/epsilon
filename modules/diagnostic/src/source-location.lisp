;;;; source-location.lisp - Canonical source location and file cache
;;;;
;;;; One source-location type used by reader, compiler, diagnostics, and
;;;; stacktrace. Supports both byte offsets (from reader) and line/column
;;;; (from SBCL), with lazy conversion between the two via a shared
;;;; source-file cache.

(defpackage :epsilon.source-location
  (:use :cl)
  (:import (epsilon.sys.lock lock))
  (:export
   ;; Source location
   #:source-location
   #:make-source-location
   #:source-location-p
   #:copy-source-location
   #:source-location-file
   #:source-location-offset
   #:source-location-end-offset
   #:source-location-line
   #:source-location-column
   #:source-location-end-line
   #:source-location-end-column

   ;; Predicates
   #:source-location-contains-p
   #:source-location-overlaps-p

   ;; Formatting
   #:format-source-location

   ;; Source file cache
   #:source-file-cache-entry
   #:source-file-cache-entry-pathname
   #:source-file-cache-entry-lines
   #:source-file-cache-entry-line-offsets
   #:get-source-file-entry
   #:get-source-lines
   #:get-source-context-lines
   #:clear-source-file-cache

   ;; Offset / line-column conversion
   #:build-line-offsets
   #:offset-to-line-column
   #:line-column-to-offset
   #:resolve-source-location))

(in-package :epsilon.source-location)

;;; ====================================================================
;;; Source Location
;;; ====================================================================

(defstruct (source-location (:constructor %make-source-location) (:copier nil))
  "Canonical source location used by reader, compiler, diagnostics, and stacktrace.

   Carries both byte offsets (from reader/CST) and line/column (from SBCL
   or diagnostics). Either representation can be lazily derived from the
   other via the source-file cache."
  (file       nil :type (or pathname string null))
  (offset     nil :type (or fixnum null))
  (end-offset nil :type (or fixnum null))
  (line       nil :type (or fixnum null))
  (column     nil :type (or fixnum null))
  (end-line   nil :type (or fixnum null))
  (end-column nil :type (or fixnum null)))

(defun make-source-location (&key file offset end-offset line column end-line end-column)
  "Create a source location. Supply byte offsets, line/column, or both."
  (%make-source-location
   :file file
   :offset offset
   :end-offset end-offset
   :line line
   :column column
   :end-line end-line
   :end-column end-column))

(defun copy-source-location (loc &key
                                   (file nil file-p)
                                   (offset nil offset-p)
                                   (end-offset nil end-offset-p)
                                   (line nil line-p)
                                   (column nil column-p)
                                   (end-line nil end-line-p)
                                   (end-column nil end-column-p))
  "Copy a source-location, overriding specified fields."
  (%make-source-location
   :file       (if file-p file (source-location-file loc))
   :offset     (if offset-p offset (source-location-offset loc))
   :end-offset (if end-offset-p end-offset (source-location-end-offset loc))
   :line       (if line-p line (source-location-line loc))
   :column     (if column-p column (source-location-column loc))
   :end-line   (if end-line-p end-line (source-location-end-line loc))
   :end-column (if end-column-p end-column (source-location-end-column loc))))

;;; ====================================================================
;;; Predicates
;;; ====================================================================

(defun source-location-contains-p (loc line column)
  "Check if LOC contains the position at LINE:COLUMN."
  (and loc
       (source-location-line loc)
       (source-location-column loc)
       (let ((sl (source-location-line loc))
             (sc (source-location-column loc))
             (el (or (source-location-end-line loc) (source-location-line loc)))
             (ec (or (source-location-end-column loc) (source-location-column loc))))
         (and (or (> line sl)
                  (and (= line sl) (>= column sc)))
              (or (< line el)
                  (and (= line el) (<= column ec)))))))

(defun source-location-overlaps-p (loc1 loc2)
  "Check if two source locations overlap. Both must have line/column info."
  (and loc1 loc2
       (source-location-line loc1) (source-location-line loc2)
       (let ((file1 (source-location-file loc1))
             (file2 (source-location-file loc2)))
         (or (and (null file1) (null file2))
             (equal file1 file2)))
       (let ((s1 (source-location-line loc1))
             (e1 (or (source-location-end-line loc1) (source-location-line loc1)))
             (s2 (source-location-line loc2))
             (e2 (or (source-location-end-line loc2) (source-location-line loc2))))
         (and (<= s1 e2) (<= s2 e1)))))

;;; ====================================================================
;;; Formatting
;;; ====================================================================

(defun format-source-location (loc &optional (stream nil))
  "Format a source location as file:line:column for display."
  (when loc
    (format stream "~@[~A~]~@[:~D~]~@[:~D~]"
            (source-location-file loc)
            (source-location-line loc)
            (source-location-column loc))))

;;; ====================================================================
;;; Source File Cache
;;; ====================================================================

(defstruct (source-file-cache-entry
            (:constructor %make-source-file-cache-entry)
            (:copier nil))
  "Cached source file data for efficient position conversion and context display."
  (pathname     nil :type (or pathname null))
  (lines        #() :type simple-vector)
  (line-offsets #() :type simple-vector)
  (mtime        0   :type integer))

(defvar *source-file-cache* (make-hash-table :test 'equal)
  "Global source file cache mapping pathname-strings to cache entries.")

(defvar *source-file-cache-lock* (lock:make-lock "source-file-cache-lock")
  "Lock protecting *source-file-cache*.")

(defvar *source-file-cache-max-size* 100
  "Maximum number of files to cache.")

(defvar *source-file-cache-access-order* nil
  "LRU access order for cache eviction.")

(defun clear-source-file-cache ()
  "Clear the global source file cache."
  (lock:with-lock (*source-file-cache-lock*)
    (clrhash *source-file-cache*)
    (setf *source-file-cache-access-order* nil)))

(defun %evict-oldest ()
  "Evict the least recently used entry from the cache."
  (when *source-file-cache-access-order*
    (let ((oldest (car (last *source-file-cache-access-order*))))
      (remhash oldest *source-file-cache*)
      (setf *source-file-cache-access-order* (butlast *source-file-cache-access-order*)))))

(defun %update-access (key)
  "Move KEY to the front of the LRU access order."
  (setf *source-file-cache-access-order*
        (cons key (remove key *source-file-cache-access-order* :test #'equal))))

(defun %load-file-data (pathname)
  "Load a file and compute lines and line-offset vectors. Returns (lines . line-offsets) or NIL."
  (handler-case
      (let ((path (if (pathnamep pathname) pathname (pathname pathname))))
        (when (probe-file path)
          (with-open-file (stream path :direction :input :if-does-not-exist nil)
            (when stream
              (let ((lines-list nil)
                    (offsets-list (list 0))
                    (pos 0))
                ;; Read all lines and track byte offsets
                (loop for line = (read-line stream nil nil)
                      while line
                      do (push line lines-list)
                         ;; +1 for the newline character
                         (incf pos (+ (length line) 1))
                         (push pos offsets-list))
                (values (coerce (nreverse lines-list) 'simple-vector)
                        (coerce (nreverse offsets-list) 'simple-vector)))))))
    (error () nil)))

(defun get-source-file-entry (pathname)
  "Get or create a cached entry for PATHNAME. Returns source-file-cache-entry or NIL."
  (when (null pathname) (return-from get-source-file-entry nil))
  (let ((key (if (pathnamep pathname) (namestring pathname) pathname)))
    (lock:with-lock (*source-file-cache-lock*)
      ;; Check existing cache
      (let ((cached (gethash key *source-file-cache*)))
        (when cached
          (let ((current-mtime (handler-case (file-write-date (pathname key))
                                 (error () nil))))
            (when (and current-mtime
                       (= current-mtime (source-file-cache-entry-mtime cached)))
              (%update-access key)
              (return-from get-source-file-entry cached)))))
      ;; Cache miss or stale - load file data
      (multiple-value-bind (lines offsets) (%load-file-data key)
        (when lines
          (let ((mtime (handler-case (file-write-date (pathname key))
                         (error () 0)))
                (entry nil))
            ;; Evict if at capacity
            (when (>= (hash-table-count *source-file-cache*) *source-file-cache-max-size*)
              (%evict-oldest))
            (setf entry (%make-source-file-cache-entry
                         :pathname (pathname key)
                         :lines lines
                         :line-offsets offsets
                         :mtime mtime))
            (setf (gethash key *source-file-cache*) entry)
            (%update-access key)
            entry))))))

(defun get-source-lines (file)
  "Get the lines vector for FILE from cache. Returns simple-vector or NIL."
  (let ((entry (get-source-file-entry file)))
    (when entry
      (source-file-cache-entry-lines entry))))

(defun get-source-context-lines (file line &key (before 2) (after 2))
  "Get source lines around LINE from FILE.
   Returns a list of (line-number text current-p) plists."
  (when (or (null file) (null line) (<= line 0))
    (return-from get-source-context-lines nil))
  (let ((lines (get-source-lines file)))
    (when (null lines) (return-from get-source-context-lines nil))
    (let ((start (max 1 (- line before)))
          (end (min (length lines) (+ line after))))
      (loop for i from start to end
            collect (list :number i
                         :text (aref lines (1- i))
                         :current-p (= i line))))))

;;; ====================================================================
;;; Offset / Line-Column Conversion
;;; ====================================================================

(defun build-line-offsets (source)
  "Compute a vector of line-start byte offsets for SOURCE string.
   Element 0 is always 0 (start of line 1). Each subsequent element is
   the offset of the first character after a newline."
  (let ((offsets (list 0)))
    (loop for i from 0 below (length source)
          when (char= (char source i) #\Newline)
            do (push (1+ i) offsets))
    (coerce (nreverse offsets) 'simple-vector)))

(defun offset-to-line-column (line-offsets offset)
  "Convert byte OFFSET to (line . column) using LINE-OFFSETS vector.
   Lines are 1-based, columns are 0-based. Uses binary search."
  (let ((lo 0)
        (hi (1- (length line-offsets))))
    ;; Binary search for the last line-start <= offset
    (loop while (<= lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (<= (aref line-offsets mid) offset)
                   (setf lo (1+ mid))
                   (setf hi (1- mid)))))
    (let ((line-idx (1- lo)))
      (cons (1+ line-idx)                          ; 1-based line
            (- offset (aref line-offsets line-idx)))))) ; 0-based column

(defun line-column-to-offset (line-offsets line column)
  "Convert 1-based LINE and 0-based COLUMN to byte offset using LINE-OFFSETS.
   Returns the offset, or NIL if out of range."
  (let ((line-idx (1- line)))
    (when (and (>= line-idx 0) (< line-idx (length line-offsets)))
      (+ (aref line-offsets line-idx) column))))

(defun resolve-source-location (loc &key source)
  "Fill in missing line/column or offset fields on LOC using the source-file
   cache (for file-backed locations) or SOURCE string (for in-memory).

   Returns a new source-location with as many fields populated as possible.
   Does not mutate the original."
  (when (null loc) (return-from resolve-source-location nil))
  (let ((file (source-location-file loc))
        (offset (source-location-offset loc))
        (end-offset (source-location-end-offset loc))
        (line (source-location-line loc))
        (column (source-location-column loc))
        (end-line (source-location-end-line loc))
        (end-column (source-location-end-column loc)))
    ;; Get line-offsets from cache or source string
    (let ((line-offsets
            (cond
              ;; Source string provided - compute directly
              (source (build-line-offsets source))
              ;; File-backed - use cache
              (file (let ((entry (get-source-file-entry file)))
                      (when entry
                        (source-file-cache-entry-line-offsets entry))))
              (t nil))))
      (when line-offsets
        ;; Derive line/column from offset if missing
        (when (and offset (null line))
          (let ((lc (offset-to-line-column line-offsets offset)))
            (setf line (car lc)
                  column (cdr lc))))
        (when (and end-offset (null end-line))
          (let ((lc (offset-to-line-column line-offsets end-offset)))
            (setf end-line (car lc)
                  end-column (cdr lc))))
        ;; Derive offset from line/column if missing
        (when (and line column (null offset))
          (setf offset (line-column-to-offset line-offsets line column)))
        (when (and end-line end-column (null end-offset))
          (setf end-offset (line-column-to-offset line-offsets end-line end-column)))))
    ;; Return new location with all derived fields
    (%make-source-location
     :file file
     :offset offset
     :end-offset end-offset
     :line line
     :column column
     :end-line end-line
     :end-column end-column)))
