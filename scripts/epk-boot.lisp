;;;; EPK-aware Fast Boot System
;;;; 
;;;; This provides the fastest possible boot by checking for:
;;;; 1. Local EPK files with combined FASL
;;;; 2. Boot cache with combined FASL
;;;; 3. Traditional sequential loading (fallback)

(defpackage epsilon.tool.epk-boot
  (:use cl)
  (:export epk-boot find-epk-file))

(in-package :epsilon.tool.epk-boot)

;; Required for ZIP handling
#-win32
(require :sb-posix)

(defparameter *epk-search-paths*
  (list (merge-pathnames ".epsilon/packages/" (user-homedir-pathname))
        (merge-pathnames "target/packages/" (probe-file "."))
        (merge-pathnames "packages/" (probe-file ".")))
  "Paths to search for EPK files")

(defparameter *boot-cache-dir*
  (merge-pathnames ".epsilon/boot-cache/" (user-homedir-pathname))
  "Directory for extracted EPK boot files")

(defun get-platform-string ()
  "Get current platform string for EPK naming"
  (format nil "~A-~A"
          #+darwin "darwin"
          #+linux "linux"  
          #+win32 "windows"
          #-(or darwin linux win32) "unknown"
          #+x86-64 "x86_64"
          #+arm64 "arm64"
          #-(or x86-64 arm64) "unknown"))

(defun find-epk-file (module-name &optional version)
  "Find an EPK file for the given module"
  (let ((platform (get-platform-string)))
    (dolist (search-path *epk-search-paths*)
      (when (probe-file search-path)
        (let* ((pattern (if version
                           (format nil "~A-~A-~A.epk" module-name version platform)
                           (format nil "~A-*-~A.epk" module-name platform)))
               (files (directory (merge-pathnames pattern search-path))))
          (when files
            ;; Return newest file if multiple versions
            (return-from find-epk-file 
              (first (sort files #'> :key #'file-write-date)))))))))

(defun extract-combined-fasl (epk-path target-path)
  "Extract combined.fasl from EPK file"
  ;; Simple implementation - in production would use proper ZIP library
  (let ((temp-dir (merge-pathnames 
                   (format nil "epk-extract-~D/" (random 1000000))
                   (merge-pathnames "tmp/" (user-homedir-pathname)))))
    (ensure-directories-exist temp-dir)
    
    (unwind-protect
         (progn
           ;; Extract EPK (ZIP file)
           #+sbcl
           (sb-ext:run-program "/usr/bin/unzip" 
                              (list "-q" (namestring epk-path) 
                                    "fasl/combined.fasl" 
                                    "-d" (namestring temp-dir))
                              :wait t)
           
           ;; Copy combined.fasl to target
           (let ((combined-fasl (merge-pathnames "fasl/combined.fasl" temp-dir)))
             (when (probe-file combined-fasl)
               (ensure-directories-exist target-path)
               (with-open-file (input combined-fasl :element-type '(unsigned-byte 8))
                 (with-open-file (output target-path 
                                        :direction :output
                                        :element-type '(unsigned-byte 8)
                                        :if-exists :supersede)
                   (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
                     (loop for bytes-read = (read-sequence buffer input)
                           while (> bytes-read 0)
                           do (write-sequence buffer output :end bytes-read)))))
               t)))
      
      ;; Cleanup
      (when (probe-file temp-dir)
        #+sbcl (sb-ext:delete-directory temp-dir :recursive t)))))

(defun read-epk-manifest (epk-path)
  "Read MANIFEST.edn from EPK file"
  ;; Simplified - in production would parse EDN properly
  (let ((temp-dir (merge-pathnames 
                   (format nil "epk-manifest-~D/" (random 1000000))
                   (merge-pathnames "tmp/" (user-homedir-pathname)))))
    (ensure-directories-exist temp-dir)
    
    (unwind-protect
         (progn
           #+sbcl
           (sb-ext:run-program "/usr/bin/unzip" 
                              (list "-q" (namestring epk-path) 
                                    "META-INF/MANIFEST.edn" 
                                    "-d" (namestring temp-dir))
                              :wait t)
           
           (let ((manifest-file (merge-pathnames "META-INF/MANIFEST.edn" temp-dir)))
             (when (probe-file manifest-file)
               (with-open-file (stream manifest-file)
                 (read stream)))))
      
      (when (probe-file temp-dir)
        #+sbcl (sb-ext:delete-directory temp-dir :recursive t)))))

(defun epk-cache-valid-p (epk-path cache-path)
  "Check if EPK cache is still valid"
  (and (probe-file cache-path)
       (probe-file epk-path)
       (> (file-write-date cache-path)
          (file-write-date epk-path))))

(defun load-from-epk (module-name epk-path)
  "Load module from EPK file"
  (let* ((cache-file (merge-pathnames 
                      (format nil "~A.epk.fasl" module-name)
                      *boot-cache-dir*))
         (manifest (read-epk-manifest epk-path)))
    
    (ensure-directories-exist *boot-cache-dir*)
    
    ;; Extract combined FASL if needed
    (unless (epk-cache-valid-p epk-path cache-file)
      (format t "~&;;; Extracting ~A from EPK...~%" module-name)
      (unless (extract-combined-fasl epk-path cache-file)
        (error "Failed to extract combined FASL from ~A" epk-path)))
    
    ;; Load the combined FASL
    (format t "~&;;; Loading ~A from EPK cache...~%" module-name)
    (load cache-file)))

(defun epk-boot (module-name &key version fallback-fn)
  "Boot a module using EPK if available, otherwise use fallback"
  (let ((epk-path (find-epk-file module-name version)))
    (cond
      ;; Found EPK - use it
      (epk-path
       (format t "~&;;; Found EPK: ~A~%" (file-namestring epk-path))
       (let ((start-time (get-internal-real-time)))
         (load-from-epk module-name epk-path)
         (format t "~&;;; EPK boot completed in ~,3F seconds~%"
                 (/ (- (get-internal-real-time) start-time)
                    internal-time-units-per-second))))
      
      ;; No EPK - use fallback
      (fallback-fn
       (funcall fallback-fn))
      
      ;; No EPK and no fallback
      (t
       (error "No EPK found for ~A and no fallback provided" module-name)))))