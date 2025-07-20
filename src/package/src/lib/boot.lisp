(defpackage :epsilon.package.boot
  (:use
   :cl)
  (:local-nicknames
   (:pkg :epsilon.package)
   (:repo :epsilon.package.repository)
   (:dep :epsilon.package.dependency)
   (:map :epsilon.map)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.uri))
  (:export
   ;; Boot functions
   :boot-module
   :boot-modules
   :quick-boot
   
   ;; Cache management
   :create-boot-cache
   :boot-cache-valid-p
   :clear-boot-cache
   
   ;; Configuration
   :*boot-cache-dir*
   :*boot-verbose*))

(in-package :epsilon.package.boot)

;;;; ==========================================================================
;;;; Configuration
;;;; ==========================================================================

(defparameter *boot-cache-dir*
  (uri:merge (fs:home-dir) ".epsilon/boot-cache/")
  "Directory for boot FASL cache")

(defparameter *boot-verbose* nil
  "Whether to print verbose boot messages")

;;;; ==========================================================================
;;;; Boot Cache Management
;;;; ==========================================================================

(defun ensure-boot-cache-dir ()
  "Ensure boot cache directory exists"
  (fs:make-dirs *boot-cache-dir*))

(defun get-boot-cache-path (module-name)
  "Get path to cached boot FASL for a module"
  (ensure-boot-cache-dir)
  (uri:merge *boot-cache-dir* (format nil "~A.boot.fasl" module-name)))

(defun get-boot-manifest-path (module-name)
  "Get path to cached boot manifest for a module"
  (ensure-boot-cache-dir)
  (uri:merge *boot-cache-dir* (format nil "~A.manifest" module-name)))

(defun save-boot-manifest (module-name module-path timestamp source-hash)
  "Save boot manifest for cache validation"
  (let ((manifest-path (get-boot-manifest-path module-name)))
    (with-open-file (stream (uri:path manifest-path)
                           :direction :output
                           :if-exists :supersede)
      (write (map:make-map
              :module-name module-name
              :module-path (uri:path module-path)
              :timestamp timestamp
              :source-hash source-hash
              :epsilon-version (epsilon-version))
             :stream stream))))

(defun load-boot-manifest (module-name)
  "Load boot manifest if exists"
  (let ((manifest-path (get-boot-manifest-path module-name)))
    (when (fs:exists-p manifest-path)
      (with-open-file (stream (uri:path manifest-path))
        (read stream)))))

(defun epsilon-version ()
  "Get current Epsilon version"
  ;; TODO make consistent with registry and package builder
  "0.0.0-boot")

(defun boot-cache-valid-p (module-name module-path)
  "Check if boot cache is valid for a module"
  (let ((cache-path (get-boot-cache-path module-name))
        (manifest (load-boot-manifest module-name)))
    
    (and (fs:exists-p cache-path)
         manifest
         ;; Check version compatibility
         (string= (map:get manifest :epsilon-version) (epsilon-version))
         ;; Check source hash
         (string= (map:get manifest :source-hash)
                  (repo:calculate-source-tree-hash module-path)))))

;;;; ==========================================================================
;;;; Combined FASL Creation
;;;; ==========================================================================

(defun collect-module-files (module-path module-info)
  "Collect all source files for a module in load order"
  (let ((sources (map:get module-info "sources" '("src")))
        (files '()))
    
    ;; Collect all .lisp files from source directories
    (dolist (src-dir sources)
      (let ((src-path (uri:merge module-path (format nil "~A/" src-dir))))
        (when (fs:exists-p src-path)
          (fs:walk-uri src-path
                       (lambda (uri)
                         (when (and (str:ends-with-p (uri:path uri) ".lisp")
                                   (not (str:contains-p (uri:path uri) "-tests.")))
                           (push uri files)))))))
    
    ;; TODO: Sort files based on dependencies
    (reverse files)))

(defun create-combined-fasl (module-name module-path module-info output-path)
  "Create a combined FASL for fast loading"
  (let ((temp-dir (uri:merge (fs:temp-dir) 
                            (format nil "epsilon-boot-~A/" (random 100000))))
        (files (collect-module-files module-path module-info)))
    
    (ensure-directories-exist (uri:path temp-dir))
    
    (unwind-protect
         (progn
           (when *boot-verbose*
             (format t "~&;;; Creating boot cache for ~A (~D files)...~%" 
                     module-name (length files)))
           
           ;; Compile all files to temp directory
           (let ((fasl-files '()))
             (dolist (file files)
               (let* ((relative-path (enough-namestring (uri:path file) 
                                                       (uri:path module-path)))
                      (fasl-path (merge-pathnames
                                  (make-pathname :type "fasl")
                                  (merge-pathnames relative-path 
                                                  (uri:path temp-dir)))))
                 (ensure-directories-exist fasl-path)
                 (compile-file (uri:path file) :output-file fasl-path
                              :verbose nil :print nil)
                 (push fasl-path fasl-files)))
             
             ;; Concatenate FASLs into single file
             (with-open-file (output (uri:path output-path)
                                    :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
               (dolist (fasl (reverse fasl-files))
                 (with-open-file (input fasl :element-type '(unsigned-byte 8))
                   (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
                     (loop for bytes-read = (read-sequence buffer input)
                           while (> bytes-read 0)
                           do (write-sequence buffer output :end bytes-read))))))))
      
      ;; Cleanup
      (fs:delete-directory (uri:path temp-dir)))))

(defun create-boot-cache (module-name module-path)
  "Create boot cache for a module"
  (let* ((module-info (dep:load-module-info module-path))
         (cache-path (get-boot-cache-path module-name))
         (source-hash (repo:calculate-source-tree-hash module-path)))
    
    ;; Create combined FASL
    (create-combined-fasl module-name module-path module-info cache-path)
    
    ;; Save manifest
    (save-boot-manifest module-name module-path (get-universal-time) source-hash)
    
    cache-path))

;;;; ==========================================================================
;;;; Module Booting
;;;; ==========================================================================

(defun boot-module (module-name module-path &key force)
  "Boot a single module, using cache if available"
  (let ((cache-path (get-boot-cache-path module-name)))
    
    (cond
      ;; Force rebuild
      (force
       (when *boot-verbose*
         (format t "~&;;; Force rebuilding ~A...~%" module-name))
       (create-boot-cache module-name module-path)
       (load (uri:path cache-path)))
      
      ;; Use cache if valid
      ((boot-cache-valid-p module-name module-path)
       (when *boot-verbose*
         (format t "~&;;; Loading ~A from cache...~%" module-name))
       (load (uri:path cache-path)))
      
      ;; Build cache
      (t
       (when *boot-verbose*
         (format t "~&;;; Building ~A...~%" module-name))
       (create-boot-cache module-name module-path)
       (load (uri:path cache-path))))))

(defun boot-modules (modules &key force)
  "Boot multiple modules in dependency order.
   MODULES is a map of module-name -> module-path"
  (let ((build-order (dep:compute-build-order (map:keys modules) modules)))
    
    (when *boot-verbose*
      (format t "~&;;; Boot order: ~{~A~^, ~}~%" build-order))
    
    (dolist (module-name build-order)
      (let ((module-path (map:get modules module-name)))
        (boot-module module-name module-path :force force)))))

;;;; ==========================================================================
;;;; Quick Boot Functions
;;;; ==========================================================================

(defun quick-boot (&key force (verbose *boot-verbose*))
  "Quick boot epsilon.core module using cached FASL if available"
  (let ((*boot-verbose* verbose)
        (core-path (uri:make-uri :scheme "file" :path "src/core/")))
    
    (if (and (not force)
             (boot-cache-valid-p "epsilon.core" core-path))
        (progn
          (when verbose
            (format t "~&;;; Quick boot: epsilon.core (cached)~%"))
          (load (uri:path (get-boot-cache-path "epsilon.core"))))
        (progn
          (when verbose
            (format t "~&;;; Quick boot: epsilon.core (building)~%"))
          (boot-module "epsilon.core" core-path :force force)))))

(defun clear-boot-cache (&optional module-name)
  "Clear boot cache for a module or all modules"
  (if module-name
      (let ((cache-path (get-boot-cache-path module-name))
            (manifest-path (get-boot-manifest-path module-name)))
        (when (fs:exists-p cache-path)
          (delete-file (uri:path cache-path)))
        (when (fs:exists-p manifest-path)
          (delete-file (uri:path manifest-path))))
      (when (fs:exists-p *boot-cache-dir*)
        (fs:delete-directory (uri:path *boot-cache-dir*)))))
