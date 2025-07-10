;;;; Optimized Boot System for Epsilon
;;;; 
;;;; Boot priority (fastest to slowest):
;;;; 1. Local EPK with combined FASL
;;;; 2. Boot cache with combined FASL  
;;;; 3. Compile and create cache
;;;; 4. Traditional sequential boot (fallback)

(defpackage epsilon.tool.optimized-boot
  (:use cl)
  (:export boot))

(in-package :epsilon.tool.optimized-boot)

;; Load fast boot and EPK boot systems
(load "scripts/fast-boot.lisp")
(load "scripts/epk-boot.lisp")

(defparameter *boot-times* nil
  "Collect boot timing information")

(defun time-operation (name fn)
  "Time an operation and record it"
  (let ((start (get-internal-real-time)))
    (prog1 (funcall fn)
      (let ((elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
        (push (cons name elapsed) *boot-times*)
        (format t "~&;;; ~A: ~,3F seconds~%" name elapsed)))))

(defun report-boot-times ()
  "Report collected boot times"
  (when *boot-times*
    (format t "~&;;; Boot Performance Summary:~%")
    (dolist (timing (reverse *boot-times*))
      (format t ";;;   ~A: ~,3F seconds~%" (car timing) (cdr timing)))
    (format t ";;;   Total: ~,3F seconds~%"
            (reduce #'+ *boot-times* :key #'cdr))))

(defun boot (&key (module "epsilon.core") force verbose)
  "Optimized boot with multiple strategies"
  (let ((*boot-times* nil))
    
    (handler-case
        (cond
          ;; Force rebuild requested
          (force
           (format t "~&;;; Force rebuild requested~%")
           (time-operation "Force rebuild"
                          (lambda ()
                            (epsilon.tool.fast-boot:fast-boot))))
          
          ;; Try EPK boot first (fastest)
          ((epsilon.tool.epk-boot:find-epk-file module)
           (format t "~&;;; Attempting EPK boot...~%")
           (time-operation "EPK boot"
                          (lambda ()
                            (epsilon.tool.epk-boot:epk-boot 
                             module
                             :fallback-fn #'epsilon.tool.fast-boot:fast-boot))))
          
          ;; Try cache boot (second fastest)
          ((epsilon.tool.fast-boot:cache-valid-p)
           (format t "~&;;; Attempting cached boot...~%")
           (time-operation "Cache boot"
                          (lambda ()
                            (epsilon.tool.fast-boot:fast-boot))))
          
          ;; Fall back to creating cache
          (t
           (format t "~&;;; No fast boot available, building...~%")
           (time-operation "Build and cache"
                          (lambda ()
                            (epsilon.tool.fast-boot:fast-boot)))))
      
      ;; Error handler
      (error (e)
        (format t "~&;;; Boot error: ~A~%" e)
        (format t "~&;;; Falling back to traditional boot...~%")
        (time-operation "Traditional boot"
                       (lambda ()
                         (load "scripts/boot.lisp")
                         (epsilon.tool.boot:boot)))))
    
    ;; Report performance
    (when verbose
      (report-boot-times))
    
    ;; Return success
    t))

;; Convenience function for REPL
(defun rebuild-cache ()
  "Force rebuild of boot cache"
  (boot :force t :verbose t))