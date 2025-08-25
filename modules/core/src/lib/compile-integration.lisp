;;;; This module hooks into SBCL's compilation pipeline to provide
;;;; real-time source location information during compilation, including
;;;; accurate line numbers and form offsets.

(defpackage epsilon.compile-integration
  (:use cl)
  (:local-nicknames
   (api epsilon.compile-api)
   (location epsilon.compile-location)
   (log epsilon.log))
  (:export
   #:*real-time-source-tracking*
   #:*current-compilation-location*
   #:*form-position-map*
   
   #:install-compiler-hooks
   #:uninstall-compiler-hooks
   #:with-source-tracking
   
   #:get-real-time-source-location
   #:track-form-processing
   #:enhanced-process-toplevel-form
   
   #:sbcl-source-path-to-location
   #:extract-line-from-form-number
   #:build-form-position-cache
   
   ;; File info structure and functions
   #:compilation-file-info
   #:compilation-file-info-p
   #:compilation-file-info-pathname
   #:compilation-file-info-line-positions
   #:compilation-file-info-form-positions
   #:position-to-line-column
   
   ;; Integration functions
   #:enhance-logging-with-compilation-context
   
   ;; SBCL source path analysis
   #:extract-line-from-sbcl-source-path
   #:file-position-to-line-number))

(in-package epsilon.compile-integration)

;;; Global state for real-time tracking

(defvar *real-time-source-tracking* nil
  "When true, real-time source location tracking is active.")

(defvar *current-compilation-location* nil
  "The current source location being compiled.")

(defvar *form-position-map* nil
  "Hash table mapping forms to file positions during compilation.")

(defvar *original-process-toplevel-form* nil
  "Original SBCL process-toplevel-form function.")

(defvar *original-sub-find-source-paths* nil
  "Original SBCL sub-find-source-paths function.")

;;; File position tracking utilities

(defstruct compilation-file-info
  "Information about a file being compiled."
  (pathname nil :type (or pathname null))
  (stream nil :type (or stream null))
  (line-positions nil :type (or simple-vector null))
  (form-positions nil :type hash-table))

(defvar *current-file-info* nil
  "Current file compilation information.")

(defun build-form-position-cache (pathname)
  "Build a cache of form positions within a file."
  (let ((positions (make-hash-table :test 'eq))
        (line-starts (make-array 1000 :adjustable t :fill-pointer 0)))
    
    ;; Record line start positions
    (vector-push-extend 0 line-starts)
    
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
    
    (make-compilation-file-info
     :pathname (pathname pathname)
     :line-positions (coerce line-starts 'simple-vector)
     :form-positions positions)))

(defun position-to-line-column (file-info char-position)
  "Convert character position to line and column."
  (when (and file-info char-position)
    (let ((line-positions (compilation-file-info-line-positions file-info)))
      (when line-positions
        (let ((line-num (position-if (lambda (pos) (> pos char-position))
                                     line-positions)))
          (if line-num
              (values (1+ line-num)
                      (1+ (- char-position (aref line-positions (1- line-num)))))
              (values (length line-positions)
                      (1+ (- char-position 
                             (aref line-positions (1- (length line-positions))))))))))))

;;; SBCL integration functions

(defun enhanced-process-toplevel-form (form path compile-time-too)
  "Enhanced version of SBCL's process-toplevel-form with source tracking."
  (declare (list path))
  
  ;; Track current form processing and extract line numbers
  (when *real-time-source-tracking*
    (track-form-processing form path)
    
    ;; NEW: Extract and set current compilation location
    (let ((location (extract-location-from-sbcl-state form path)))
      (when location
        (setf *current-compilation-location* location))))
  
  ;; Call original function
  (funcall *original-process-toplevel-form* form path compile-time-too))

(defun enhanced-sub-find-source-paths (form path)
  "Enhanced version of sub-find-source-paths that tracks positions."
  (when *real-time-source-tracking*
    ;; Extract position information if we have current file info
    (when (and *current-file-info* form)
      (let ((char-pos (gethash form 
                              (compilation-file-info-form-positions *current-file-info*))))
        (when char-pos
          (multiple-value-bind (line column)
              (position-to-line-column *current-file-info* char-pos)
            (setf *current-compilation-location*
                  (api:make-source-location
                   :file (namestring (compilation-file-info-pathname *current-file-info*))
                   :line line
                   :column column
                   :form-number (length path)
                   :toplevel-form (when (< (length path) 2) form))))))))
  
  ;; Call original function
  (funcall *original-sub-find-source-paths* form path))

(defun track-form-processing (form path)
  "Track the processing of a form during compilation."
  (when *real-time-source-tracking*
    ;; Update current location based on SBCL's internal state
    (let ((location (extract-location-from-sbcl-state form path)))
      (when location
        (setf *current-compilation-location* location)
        
        ;; Location is now available for logging system to use
        nil))))

(defun extract-location-from-sbcl-state (form path)
  "Extract source location from SBCL's current compilation state."
  (let ((file nil)
        (line nil)
        (column nil)
        (form-number (length path))
        (toplevel-form nil))
    
    ;; Get file from compiler state
    (setf file (cond
                 ((and (boundp 'sb-c::*compile-file-pathname*)
                       sb-c::*compile-file-pathname*)
                  (namestring sb-c::*compile-file-pathname*))
                 ((and *current-file-info*
                       (compilation-file-info-pathname *current-file-info*))
                  (namestring (compilation-file-info-pathname *current-file-info*)))
                 (t nil)))
    
    ;; NEW: Extract line numbers from SBCL source path system
    (multiple-value-bind (extracted-line extracted-column)
        (extract-line-from-sbcl-source-path path)
      (when extracted-line
        (setf line extracted-line
              column extracted-column)))
    
    ;; Fallback: Extract line/column from our enhanced tracking
    (when (and (not line) *current-file-info* form)
      (let ((char-pos (gethash form 
                              (compilation-file-info-form-positions *current-file-info*))))
        (when char-pos
          (multiple-value-setq (line column)
            (position-to-line-column *current-file-info* char-pos)))))
    
    ;; Determine toplevel form
    (when (< form-number 2)
      (setf toplevel-form form))
    
    (when file
      (api:make-source-location
       :file file
       :line line
       :column column
       :form-number form-number
       :toplevel-form toplevel-form))))

;;; Hook installation and management

(defun install-compiler-hooks ()
  "Install hooks into SBCL's compilation system."
  (unless *original-process-toplevel-form*
    ;; Unlock SBCL packages to allow modification of internal functions
    (sb-ext:unlock-package :sb-c)
    (sb-ext:unlock-package :sb-int)
    (sb-ext:unlock-package :sb-kernel)
    
    ;; Save original functions
    (setf *original-process-toplevel-form* 
          (symbol-function 'sb-c::process-toplevel-form))
    (setf *original-sub-find-source-paths*
          (symbol-function 'sb-c::sub-find-source-paths))
    
    ;; Replace with enhanced versions
    (setf (symbol-function 'sb-c::process-toplevel-form)
          #'enhanced-process-toplevel-form)
    (setf (symbol-function 'sb-c::sub-find-source-paths)
          #'enhanced-sub-find-source-paths)
    
    ;; Enable real-time source tracking
    (setf *real-time-source-tracking* t)
    
    (log:debug "SBCL compiler hooks installed")))

(defun uninstall-compiler-hooks ()
  "Uninstall compiler hooks and restore original functions."
  (when *original-process-toplevel-form*
    ;; Unlock packages again in case they were re-locked
    (sb-ext:unlock-package :sb-c)
    (sb-ext:unlock-package :sb-int)
    (sb-ext:unlock-package :sb-kernel)
    
    ;; Restore original functions
    (setf (symbol-function 'sb-c::process-toplevel-form)
          *original-process-toplevel-form*)
    (setf (symbol-function 'sb-c::sub-find-source-paths)
          *original-sub-find-source-paths*)
    
    (setf *original-process-toplevel-form* nil)
    (setf *original-sub-find-source-paths* nil)
    
    ;; Re-lock packages for safety (optional)
    (handler-case
        (progn
          (sb-ext:lock-package :sb-c)
          (sb-ext:lock-package :sb-int)
          (sb-ext:lock-package :sb-kernel))
      (error () nil))  ; Ignore errors if already locked
    
    (log:debug "SBCL compiler hooks uninstalled")))

;;; High-level interface

(defmacro with-source-tracking ((&key (enable t) file) &body body)
  "Execute body with source location tracking enabled."
  `(let ((*real-time-source-tracking* ,enable)
         (*current-compilation-location* nil)
         (*current-file-info* ,(when file
                                 `(build-form-position-cache ,file))))
     (unwind-protect
          (progn
            (when ,enable
              (install-compiler-hooks))
            ,@body)
       (when ,enable
         (uninstall-compiler-hooks)))))

(defun get-real-time-source-location ()
  "Get the current real-time source location during compilation."
  (or *current-compilation-location*
      ;; Fallback to SBCL state extraction
      (when (and (boundp 'sb-c::*current-path*)
                 sb-c::*current-path*)
        (extract-location-from-sbcl-state 
         (car sb-c::*current-path*)
         sb-c::*current-path*))))

;;; SBCL source path utilities

(defun sbcl-source-path-to-location (source-path)
  "Convert SBCL source path to our location structure."
  (when source-path
    (let ((form-number (length source-path))
          (file nil))
      
      ;; Get file from compilation state
      (when (and (boundp 'sb-c::*compile-file-pathname*)
                 sb-c::*compile-file-pathname*)
        (setf file (namestring sb-c::*compile-file-pathname*)))
      
      (api:make-source-location
       :file file
       :line nil  ; Would need file position to calculate
       :column nil
       :form-number form-number
       :toplevel-form nil))))

(defun extract-line-from-sbcl-source-path (path)
  "Extract line number from SBCL source path using ORIGINAL-SOURCE-START."
  (when (and path (listp path))
    ;; Look for ORIGINAL-SOURCE-START in the path
    (let ((source-start-node (find-if (lambda (node)
                                        (and (listp node)
                                             (eq (first node) 'sb-c::original-source-start)))
                                      (if (listp (first path)) path (list path)))))
      (when source-start-node
        (let ((form-index (third source-start-node))) ; (ORIGINAL-SOURCE-START 0 form-index)
          (when (and form-index 
                     (numberp form-index)
                     (boundp 'sb-c::*source-info*)
                     sb-c::*source-info*)
            ;; Get FILE-INFO from current compilation
            (let ((file-info (when (fboundp 'sb-c::source-info-file-info)
                              (sb-c::source-info-file-info sb-c::*source-info*))))
              (when file-info
                ;; Get form positions array  
                (let ((positions (when (fboundp 'sb-c::file-info-positions)
                                  (sb-c::file-info-positions file-info))))
                  (when (and positions 
                             (< form-index (length positions)))
                    ;; Get character position of this form
                    (let ((char-pos (aref positions form-index)))
                      ;; Convert character position to line number
                      (values (file-position-to-line-number file-info char-pos)
                              nil))))))))))))  ; column (TODO: implement)

(defun file-position-to-line-number (file-info char-position)
  "Convert file character position to line number using SBCL file info."
  (when (and file-info char-position)
    ;; Read the file and count newlines up to position
    (let ((pathname (when (fboundp 'sb-c::file-info-truename)
                     (sb-c::file-info-truename file-info))))
      (when (and pathname (probe-file pathname))
        (with-open-file (stream pathname :direction :input)
          (let ((line 1)
                (pos 0))
            ;; Count lines up to the target position
            (loop while (< pos char-position)
                  do (let ((char (read-char stream nil nil)))
                       (unless char (return))
                       (incf pos)
                       (when (char= char #\Newline)
                         (incf line))))
            ;; Return the line number where the character position falls
            line))))))

(defun extract-line-from-form-number (file form-number)
  "Extract line number from form number using cached file info."
  (when (and *current-file-info* file form-number)
    ;; This is a simplified approximation
    ;; In practice, we'd need to correlate form numbers with actual positions
    (+ form-number 1))) ; Rough estimate
