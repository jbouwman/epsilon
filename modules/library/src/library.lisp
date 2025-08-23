;;;; Centralized Library Management System
;;;;
;;;; Provides a tiered library resolution system for epsilon modules
;;;; supporting bundled libraries, environment paths, package managers,
;;;; and system defaults.

(defpackage epsilon.library
  (:use cl epsilon.syntax)
  (:local-nicknames
   (map epsilon.map)
   (seq epsilon.sequence)
   (str epsilon.string)
   (path epsilon.path)
   (log epsilon.log))
  (:export
   ;; Core API
   find-library
   library-available-p
   library-version
   with-library
   
   ;; Library registration
   register-library
   define-library
   
   ;; Search path management
   add-search-path
   remove-search-path
   get-search-paths
   
   ;; Bundled library support
   register-bundled-library
   get-bundled-library-path
   
   ;; Diagnostics
   check-libraries
   library-info
   available-libraries
   missing-libraries
   
   ;; Platform utilities
   platform-string
   platform-library-name
   platform-library-extension
   
   ;; Library handle management
   lib-open
   lib-close
   lib-function
   get-open-libraries
   get-function-cache
   cache-function
   
   ;; Manifest
   load-manifest
   save-manifest
   get-library-manifest))

(in-package epsilon.library)

;;; Platform Detection

(defun platform-string ()
  "Return platform identifier string like 'linux-x86_64' or 'darwin-arm64'"
  (format nil "~A-~A"
          #+linux "linux"
          #+darwin "darwin"
          #+windows "windows"
          #-(or linux darwin windows) "unknown"
          #+x86-64 "x86_64"
          #+arm64 "arm64"
          #-(or x86-64 arm64) "unknown"))

(defun platform-library-extension ()
  "Return the platform-specific shared library extension"
  #+darwin ".dylib"
  #+windows ".dll"
  #+linux ".so"
  #-(or darwin windows linux) ".so")

(defun platform-library-name (base-name &optional version)
  "Convert base library name to platform-specific format"
  ;; Don't add lib prefix if already present
  (let ((needs-lib-prefix (not (and (>= (length base-name) 3)
                                     (string= (subseq base-name 0 3) "lib")))))
    #+darwin
    (if version
        (format nil "~:[~;lib~]~A.~A.dylib" needs-lib-prefix base-name version)
        (format nil "~:[~;lib~]~A.dylib" needs-lib-prefix base-name))
    
    #+linux
    (if version
        (format nil "~:[~;lib~]~A.so.~A" needs-lib-prefix base-name version)
        (format nil "~:[~;lib~]~A.so" needs-lib-prefix base-name))
    
    #+windows
    (if version
        (format nil "~A-~A.dll" base-name version)
        (format nil "~A.dll" base-name))
    
    #-(or darwin linux windows)
    (format nil "~:[~;lib~]~A.so" needs-lib-prefix base-name)))

;;; Library Registry

(defvar *library-registry* (map:make-map)
  "Registry of known libraries with their metadata")

(defstruct library-info
  "Library metadata and configuration"
  name                    ; Symbol naming the library
  base-name              ; Base name without lib prefix or extension
  version                ; Optional version requirement
  bundled-p              ; Whether to bundle with epsilon
  critical-p             ; Whether library is critical for operation
  search-names           ; List of names to search for
  validator              ; Optional function to validate library
  platforms              ; List of supported platforms or :all
  description)           ; Human-readable description

(defun register-library (name &key base-name version bundled-p critical-p 
                                   search-names validator platforms description)
  "Register a library in the global registry"
  (let ((info (make-library-info
               :name name
               :base-name (or base-name (string-downcase (symbol-name name)))
               :version version
               :bundled-p bundled-p
               :critical-p critical-p
               :search-names (or search-names
                               (list (platform-library-name 
                                     (or base-name (string-downcase (symbol-name name)))
                                     version)))
               :validator validator
               :platforms (or platforms :all)
               :description description)))
    (setf *library-registry* (map:assoc *library-registry* name info))
    info))

(defmacro define-library (name &body options)
  "Define a library with its requirements and search strategy"
  `(register-library ',name ,@options))

;;; Search Path Management

(defvar *search-paths* nil
  "Ordered list of paths to search for libraries")

(defvar *bundled-library-root* nil
  "Root directory for bundled libraries")

(defun initialize-search-paths ()
  "Initialize default search paths based on environment"
  (setf *search-paths*
        (append
         ;; Tier 2: Environment paths
         (when-let ((epsilon-path (sb-ext:posix-getenv "EPSILON_LIBRARY_PATH")))
           (str:split ":" epsilon-path))
         
         ;; Tier 3: Package manager paths
         #+darwin
         (list "/opt/homebrew/lib"
               "/opt/homebrew/opt/openssl@3/lib"
               "/usr/local/lib"
               "/usr/local/opt/openssl@3/lib")
         
         #+linux
         (append
          ;; Check for Nix store
          (when (probe-file "/nix/store/")
            (list "/nix/store"))
          ;; Standard Linux paths
          (list "/usr/lib/x86_64-linux-gnu"
                "/usr/lib64"
                "/usr/lib"
                "/usr/local/lib"
                "/lib/x86_64-linux-gnu"
                "/lib64"
                "/lib"))
         
         #+windows
         (list "C:\\Windows\\System32"
               "C:\\Windows\\SysWOW64"
               "C:\\Program Files\\Common Files"))))

(defun add-search-path (path &optional (priority :append))
  "Add a path to library search paths"
  (ecase priority
    (:prepend (push path *search-paths*))
    (:append (setf *search-paths* (append *search-paths* (list path))))))

(defun remove-search-path (path)
  "Remove a path from library search paths"
  (setf *search-paths* (remove path *search-paths* :test #'equal)))

(defun get-search-paths ()
  "Get current library search paths in order"
  *search-paths*)

;;; Bundled Library Support

(defun get-epsilon-module-root ()
  "Get the root directory of epsilon modules"
  (or (when *load-truename*
        (path:string-path-join 
         (namestring (make-pathname :directory (pathname-directory *load-truename*)))
         ".." ".."))
      ;; Fallback: try to find from current working directory
      (let ((cwd (sb-posix:getcwd)))
        (cond
          ((search "/modules/" cwd)
           ;; We're in a module directory
           (subseq cwd 0 (search "/modules/" cwd)))
          ((probe-file (path:string-path-join cwd "modules"))
           ;; We're in the epsilon root
           cwd)
          (t
           "/home/jbouwman/git/epsilon-8")))))

(defun get-bundled-library-root ()
  "Get root directory for bundled libraries"
  (or *bundled-library-root*
      (setf *bundled-library-root*
            (path:string-path-join (get-epsilon-module-root) 
                                  "library" "lib"))))

(defun get-bundled-library-path (base-name &optional version)
  "Get path to bundled library if it exists"
  (let* ((platform-dir (path:string-path-join (get-bundled-library-root)
                                              (platform-string)))
         (lib-name (platform-library-name base-name version))
         (lib-path (path:string-path-join platform-dir lib-name)))
    (when (probe-file lib-path)
      lib-path)))

(defun register-bundled-library (base-name source-path)
  "Register a bundled library by copying it to the appropriate location"
  (let* ((platform-dir (path:string-path-join (get-bundled-library-root)
                                              (platform-string)))
         (lib-name (pathname-name source-path))
         (dest-path (path:string-path-join platform-dir lib-name)))
    (ensure-directories-exist platform-dir)
    ;; Simple file copy using standard CL
    (with-open-file (in source-path :element-type '(unsigned-byte 8))
      (with-open-file (out dest-path :element-type '(unsigned-byte 8)
                                     :direction :output
                                     :if-exists :supersede)
        (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
              for bytes-read = (read-sequence buffer in)
              while (plusp bytes-read)
              do (write-sequence buffer out :end bytes-read))))
    dest-path))

;;; Tiered Library Resolution

(defun find-library-in-paths (names paths)
  "Search for library names in given paths"
  (loop for path in paths
        thereis (loop for name in names
                      for full-path = (path:string-path-join path name)
                      when (probe-file full-path)
                        return full-path)))

(defun find-library-tier-1 (info)
  "Tier 1: Check for bundled library"
  (when (library-info-bundled-p info)
    (get-bundled-library-path (library-info-base-name info)
                             (library-info-version info))))

(defun find-library-tier-2 (info)
  "Tier 2: Check environment-specified paths"
  (let ((env-paths (sb-ext:posix-getenv "EPSILON_LIBRARY_PATH")))
    (when env-paths
      (find-library-in-paths (library-info-search-names info)
                            (str:split ":" env-paths)))))

(defun find-library-tier-3 (info)
  "Tier 3: Check package manager paths"
  (find-library-in-paths (library-info-search-names info)
                        (get-search-paths)))

(defun find-library-tier-4 (info)
  "Tier 4: System default resolution"
  ;; First try to find an existing file
  (or (loop for name in (library-info-search-names info)
            when (probe-file name)
              return name)
      ;; If no file exists, return first non-absolute path for system loader
      (loop for name in (library-info-search-names info)
            when (not (and (> (length name) 0) 
                          (eql (char name 0) #\/)))
              return name)))

(defun find-library (name)
  "Find a library using tiered resolution"
  ;; Normalize name - convert keywords to regular symbols in this package
  ;; Use string-downcase to handle case sensitivity issues
  (let* ((normalized-name (if (keywordp name)
                              (intern (string-downcase (symbol-name name)) :epsilon.library)
                              name))
         (info (or (map:get *library-registry* normalized-name)
                   ;; Also try the original name in case it's already registered
                   (map:get *library-registry* name))))
    (unless info
      ;; Create temporary info for unregistered library
      (setf info (make-library-info 
                  :name name
                  :base-name (string-downcase (symbol-name name))
                  :search-names (list (platform-library-name 
                                      (string-downcase (symbol-name name))))
                  :platforms :all)))
    
    ;; Check platform compatibility
    (unless (or (eq (library-info-platforms info) :all)
               (member (platform-string) (library-info-platforms info)
                      :test #'string=))
      (error "Library ~A not available on platform ~A" 
             normalized-name (platform-string)))
    
    ;; Try each tier in order
    (or (find-library-tier-1 info)
        (find-library-tier-2 info)
        (find-library-tier-3 info)
        (find-library-tier-4 info)
        (error "Library ~A not found" name))))

(defun library-available-p (name)
  "Check if a library is available"
  (handler-case
      (let ((path (find-library name)))
        (cond
          ;; If it's an absolute path, check if file exists
          ((and path (or (char= (char path 0) #\/)
                        #+windows (and (>= (length path) 3) 
                                      (char= (char path 1) #\:))))
           (probe-file path))
          ;; If it's just a library name, try to load it
          (path
           (handler-case
               (let ((handle (sb-alien:load-shared-object path)))
                 (sb-alien:unload-shared-object handle)
                 t)
             (error () nil)))
          (t nil)))
    (error () nil)))

;;; Library Validation

(defun validate-library (path info)
  "Validate that a library at path meets requirements"
  (when (library-info-validator info)
    (funcall (library-info-validator info) path)))

(defun library-version (name)
  "Get version of loaded library if available"
  ;; This would need FFI calls to the library's version function
  ;; For now, return version from registry
  (when-let ((info (map:get *library-registry* name)))
    (library-info-version info)))

;;; Diagnostics

(defun check-libraries ()
  "Check all registered libraries and return status"
  (let ((results (map:make-map)))
    (loop for (name . info) in (map:seq *library-registry*)
          do (let ((status (handler-case
                             (let ((path (find-library name)))
                               (if path
                                   (list :found path)
                                   (list :missing nil)))
                           (error (e)
                             (list :error (format nil "~A" e))))))
               (setf results (map:assoc results name status))))
    results))

(defun library-info (name)
  "Get detailed information about a library"
  (let ((info (map:get *library-registry* name)))
    (when info
      (list :name (library-info-name info)
            :base-name (library-info-base-name info)
            :version (library-info-version info)
            :bundled-p (library-info-bundled-p info)
            :critical-p (library-info-critical-p info)
            :platforms (library-info-platforms info)
            :description (library-info-description info)
            :path (handler-case (find-library name)
                   (error () nil))))))

(defun available-libraries ()
  "List all available libraries"
  (let ((available nil))
    (loop for (name . info) in (map:seq *library-registry*)
          do (when (library-available-p name)
               (push name available)))
    (nreverse available)))

(defun missing-libraries ()
  "List all missing critical libraries"
  (let ((missing nil))
    (loop for (name . info) in (map:seq *library-registry*)
          do (when (and (library-info-critical-p info)
                       (not (library-available-p name)))
               (push name missing)))
    (nreverse missing)))

;;; Library Manifest

(defstruct library-manifest
  "Manifest of library requirements for a module"
  module-name
  libraries
  minimum-versions
  optional-libraries)

(defun load-manifest (path)
  "Load library manifest from file"
  (with-open-file (stream path :direction :input)
    (read stream)))

(defun save-manifest (manifest path)
  "Save library manifest to file"
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write manifest :stream stream :readably t)))

(defun get-library-manifest (module-name)
  "Get or create manifest for a module"
  (make-library-manifest :module-name module-name
                        :libraries nil
                        :minimum-versions (map:make-map)
                        :optional-libraries nil))

;;; Library Handle Management

(defvar *open-libraries* (map:make-map)
  "Map of library names to library handles")

(defvar *function-cache* (map:make-map)
  "Cache of function pointers keyed by (library-name function-name)")

(defun lib-open (library-name &key local paths)
  "Opens a shared library and returns a handle"
  (let ((existing (map:get *open-libraries* library-name)))
    (if existing
        existing
        (let* ((is-absolute-path (and (stringp library-name)
                                      (> (length library-name) 0)
                                      (or (char= (char library-name 0) #\/)
                                          #+windows (and (>= (length library-name) 3)
                                                        (char= (char library-name 1) #\:)))))
               (lib-path (cond
                          ;; If it's an absolute path, use it directly
                          (is-absolute-path library-name)
                          ;; For local libraries, search in specified paths
                          (local
                           (find-library-in-paths 
                            (list (platform-library-name library-name))
                            (or paths '("."))))
                          ;; For system libraries, use standard resolution
                          (t
                           ;; Handle both "libcrypto" and "crypto" style names
                           (let ((lookup-name (if (and (>= (length library-name) 3)
                                                      (string= (subseq library-name 0 3) "lib"))
                                                 (subseq library-name 3)
                                                 library-name)))
                             (find-library (intern (string-upcase lookup-name) :keyword))))))
               (final-path (or lib-path library-name)))
          ;; Try to load the library
          (let ((handle (sb-alien:load-shared-object final-path)))
            (setf *open-libraries* 
                  (map:assoc *open-libraries* library-name handle))
            handle)))))

(defun lib-close (library-handle)
  "Closes a previously opened library"
  (loop for (name . handle) in (map:seq *open-libraries*)
        when (eq handle library-handle)
          do (progn
               (setf *open-libraries* 
                     (map:dissoc *open-libraries* name))
               (sb-alien:unload-shared-object library-handle)
               (return t))
        finally (return nil)))

(defun lib-function (library-handle function-name)
  "Get function pointer from library"
  ;; Look up in cache first
  (let* ((cache-key (cons library-handle function-name))
         (cached (map:get *function-cache* cache-key)))
    (if cached
        cached
        ;; Not in cache, look it up
        (let ((addr (sb-sys:find-dynamic-foreign-symbol-address function-name)))
          (when (and addr (not (zerop addr)))
            (let ((ptr (sb-sys:int-sap addr)))
              (setf *function-cache* (map:assoc *function-cache* cache-key ptr))
              ptr))))))

(defun get-open-libraries ()
  "Get list of currently open libraries"  
  *open-libraries*)

(defun get-function-cache ()
  "Get the function cache for FFI use"
  *function-cache*)

(defun cache-function (key fn-ptr)
  "Add a function to the cache"
  (setf *function-cache* (map:assoc *function-cache* key fn-ptr)))

;;; Convenience Macro

(defmacro with-library ((var name) &body body)
  "Execute body with library handle bound to var"
  `(let ((,var (lib-open ',name)))
     ,@body))

;;; Initialize on load
(eval-when (:load-toplevel :execute)
  (initialize-search-paths)
  
  ;; Register commonly used libraries
  (define-library sqlite
    :base-name "sqlite3"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "libsqlite3.dylib" #+darwin "libsqlite3.0.dylib"
                        #+linux "libsqlite3.so" #+linux "libsqlite3.so.0" #+linux "libsqlite3.so.3"
                        #+windows "sqlite3.dll")
    :description "SQLite database engine")
  
  (define-library openssl
    :base-name "ssl"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "libssl.dylib" #+darwin "libssl.3.dylib"
                        #+linux "libssl.so" #+linux "libssl.so.3" #+linux "libssl.so.1.1"
                        #+windows "libssl-3.dll" #+windows "ssleay32.dll")
    :description "OpenSSL SSL/TLS library")
  
  (define-library crypto
    :base-name "crypto"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/lib/libcrypto.3.dylib"
                        #+darwin "/opt/homebrew/lib/libcrypto.dylib"
                        #+darwin "libcrypto.dylib" #+darwin "libcrypto.3.dylib"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libcrypto.so.3"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libcrypto.so"
                        #+linux "libcrypto.so.3" #+linux "libcrypto.so.1.1" #+linux "libcrypto.so"
                        #+windows "libcrypto-3.dll" #+windows "libeay32.dll")
    :description "OpenSSL cryptographic library")
  
  ;; Register both ffi and libffi names
  (define-library ffi
    :base-name "ffi"
    :version nil
    :bundled-p t
    :critical-p t
    :search-names (list #+darwin "libffi.dylib" #+darwin "libffi.8.dylib" #+darwin "libffi.7.dylib"
                        #+linux "/nix/store/mnq0hqsqivdbaqzmzc287l0z9zw8dp15-libffi-3.4.4/lib/libffi.so.8"
                        #+linux "/nix/store/mnq0hqsqivdbaqzmzc287l0z9zw8dp15-libffi-3.4.4/lib/libffi.so"
                        #+linux "libffi.so" #+linux "libffi.so.8" #+linux "libffi.so.7" #+linux "libffi.so.6"
                        #+windows "libffi.dll" #+windows "libffi-8.dll" #+windows "libffi-7.dll")
    :description "Foreign Function Interface library")
    
  (define-library libffi
    :base-name "ffi"
    :version nil
    :bundled-p t
    :critical-p t
    :search-names (list #+darwin "libffi.dylib" #+darwin "libffi.8.dylib" #+darwin "libffi.7.dylib"
                        #+linux "/nix/store/mnq0hqsqivdbaqzmzc287l0z9zw8dp15-libffi-3.4.4/lib/libffi.so.8"
                        #+linux "/nix/store/mnq0hqsqivdbaqzmzc287l0z9zw8dp15-libffi-3.4.4/lib/libffi.so"
                        #+linux "libffi.so" #+linux "libffi.so.8" #+linux "libffi.so.7" #+linux "libffi.so.6"
                        #+windows "libffi.dll" #+windows "libffi-8.dll" #+windows "libffi-7.dll")
    :description "Foreign Function Interface library")
  
  ;; System libraries - register both c and libc names  
  (define-library c
    :base-name "c"
    :version nil
    :bundled-p nil
    :critical-p t
    :search-names (list #+darwin "/usr/lib/libSystem.B.dylib"
                        #+linux "/nix/store/yaz7pyf0ah88g2v505l38n0f3wg2vzdj-glibc-2.37-8/lib/libc.so.6"
                        #+linux "/nix/store/yaz7pyf0ah88g2v505l38n0f3wg2vzdj-glibc-2.37-8/lib/libc.so"
                        #+linux "libc.so.6" #+linux "libc.so"
                        #+windows "msvcrt.dll")
    :description "C standard library")
    
  (define-library libc
    :base-name "c"
    :version nil
    :bundled-p nil
    :critical-p t
    :search-names (list #+darwin "/usr/lib/libSystem.B.dylib"
                        #+linux "/nix/store/yaz7pyf0ah88g2v505l38n0f3wg2vzdj-glibc-2.37-8/lib/libc.so.6"
                        #+linux "/nix/store/yaz7pyf0ah88g2v505l38n0f3wg2vzdj-glibc-2.37-8/lib/libc.so"
                        #+linux "libc.so.6" #+linux "libc.so"
                        #+windows "msvcrt.dll")
    :description "C standard library")
    
  (define-library libm
    :base-name "m"
    :version nil
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/usr/lib/libSystem.B.dylib"
                        #+linux "libm.so.6" #+linux "libm.so"
                        #+windows "msvcrt.dll")
    :description "Math library")
    
  (define-library sqlite
    :base-name "sqlite3"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "/opt/homebrew/Cellar/sqlite/3.50.4/lib/libsqlite3.dylib"
                        #+darwin "/usr/local/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "libsqlite3.dylib"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so.0.8.6"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so"
                        #+linux "libsqlite3.so" #+linux "libsqlite3.so.0"
                        #+windows "sqlite3.dll")
    :description "SQLite database library")
    
  (define-library sqlite3
    :base-name "sqlite3"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "/opt/homebrew/Cellar/sqlite/3.50.4/lib/libsqlite3.dylib"
                        #+darwin "/usr/local/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "libsqlite3.dylib"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so.0.8.6"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so"
                        #+linux "libsqlite3.so" #+linux "libsqlite3.so.0"
                        #+windows "sqlite3.dll")
    :description "SQLite database library")
    
  (define-library libsqlite3
    :base-name "sqlite3"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "/opt/homebrew/Cellar/sqlite/3.50.4/lib/libsqlite3.dylib"
                        #+darwin "/usr/local/opt/sqlite/lib/libsqlite3.dylib"
                        #+darwin "libsqlite3.dylib"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so.0.8.6"
                        #+linux "/nix/store/adg9f2bkvq6pja884rdpzlm6dv9xkhsk-sqlite-3.43.2/lib/libsqlite3.so"
                        #+linux "libsqlite3.so" #+linux "libsqlite3.so.0"
                        #+windows "sqlite3.dll")
    :description "SQLite database library")
    
  (define-library libcrypto
    :base-name "crypto"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/lib/libcrypto.3.dylib"
                        #+darwin "/opt/homebrew/lib/libcrypto.dylib"
                        #+darwin "libcrypto.dylib" #+darwin "libcrypto.3.dylib"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libcrypto.so.3"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libcrypto.so"
                        #+linux "libcrypto.so.3" #+linux "libcrypto.so.1.1" #+linux "libcrypto.so"
                        #+windows "libcrypto-3.dll" #+windows "libcrypto-1_1.dll")
    :description "OpenSSL cryptography library")
    
  ;; Register both ssl and libssl names
  (define-library ssl
    :base-name "ssl"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/lib/libssl.3.dylib"
                        #+darwin "/opt/homebrew/lib/libssl.dylib"
                        #+darwin "libssl.dylib" #+darwin "libssl.3.dylib"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libssl.so.3"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libssl.so"
                        #+linux "libssl.so.3" #+linux "libssl.so.1.1" #+linux "libssl.so"
                        #+windows "libssl-3.dll" #+windows "libssl-1_1.dll")
    :description "OpenSSL SSL/TLS library")
    
  (define-library libssl
    :base-name "ssl"
    :version "3"
    :bundled-p nil
    :critical-p nil
    :search-names (list #+darwin "/opt/homebrew/lib/libssl.3.dylib"
                        #+darwin "/opt/homebrew/lib/libssl.dylib"
                        #+darwin "libssl.dylib" #+darwin "libssl.3.dylib"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libssl.so.3"
                        #+linux "/nix/store/wvrg1kgiy79sln1fzhvj8w6g604ghsad-openssl-3.0.8/lib/libssl.so"
                        #+linux "libssl.so.3" #+linux "libssl.so.1.1" #+linux "libssl.so"
                        #+windows "libssl-3.dll" #+windows "libssl-1_1.dll")
    :description "OpenSSL SSL/TLS library"))