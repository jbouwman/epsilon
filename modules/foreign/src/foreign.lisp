(defpackage epsilon.foreign
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (seq epsilon.sequence)
   (path epsilon.path)
   (trampoline epsilon.foreign.trampoline)
   (marshalling epsilon.foreign.marshalling)
   (struct epsilon.foreign.struct)
   (callback epsilon.foreign.callback)
   (callback-impl epsilon.foreign.callback-impl)
   (clang-sigs epsilon.clang.signatures))
  (:export
   ;; Core FFI
   shared-call
   shared-call-unified
   lib-open
   lib-close
   lib-function
   defshared
   
   ;; libffi bridge functions
   load-libffi-extension
   libffi-call
   *libffi-available-p*
   resolve-function-address
   ffi-call
   ffi-call-auto
   auto-discover-signature
   *use-libffi-calls*
   *track-call-performance*
   
   ;; Type Management
   define-foreign-struct
   with-foreign-struct
   map-struct
   foreign-array
   with-zero-copy
   
   ;; Memory Management
   foreign-alloc
   foreign-free
   with-foreign-memory
   register-finalizer
   
   ;; Helper functions
   make-epoll-data
   epoll-data-fd
   
   ;; Structure Discovery
   grovel-struct
   grovel-lib
   parse-header
   
   ;; Type Mapping
   def-type-map
   *primitive-type-map*
   
   ;; New trampoline-based interface
   defshared-fast
   shared-call-fast
   
   ;; Re-export from trampoline module
   make-ffi-trampoline
   get-or-create-trampoline
   c-type
   c-type-p
   c-type-base
   c-type-size
   c-type-alignment
   c-type-signed-p
   get-c-type
   ffi-signature
   ffi-signature-p
   ffi-signature-return-type
   ffi-signature-arg-types
   ffi-signature-trampoline
   register-signature
   get-signature
   clear-signature-registry
   convert-to-foreign
   convert-from-foreign
   
   ;; Re-export from marshalling module
   infer-function-signature
   with-pinned-array
   with-string-array
   with-output-array
   define-enum
   enum-value
   enum-keyword
   defshared-auto
   defshared-smart
   define-c-type
   foreign-error
   foreign-error-p
   foreign-error-code
   foreign-error-function
   bool-to-foreign
   foreign-to-bool
   
   ;; Re-export from struct module
   define-c-struct
   define-c-struct-auto
   define-c-union
   parse-c-struct
   struct-layout-p
   get-struct-layout
   struct-layout-size
   struct-layout-alignment
   struct-field-offset
   struct-field-type
   struct-field-size
   struct-has-field-p
   with-c-struct
   with-c-union
   with-struct-view
   with-foreign-object
   struct-ref
   struct-ref-ptr
   union-ref
   struct-pointer
   struct-to-bytes
   bytes-to-struct
   struct-to-string
   
   ;; Re-export from callback module
   make-callback
   call-callback
   callback-pointer
   register-callback
   unregister-callback
   get-callback
   list-callbacks
   defcallback
   with-callback
   with-callback-scope
   callback-info
   callback-info-p
   callback-info-function
   callback-info-signature
   callback-info-pointer
   
   ;; New libffi-first architecture
   shared-call-unified
   ffi-call
   ffi-call-cached
   defshared-auto
   defshared-smart
   auto-discover-signature
   
   ;; Public API and configuration
   ffi-help
   ffi-system-status
   *use-libffi-calls*
   *track-call-performance*
   *libffi-available-p*
   
   ;; Performance and debugging
   benchmark-ffi-approach
   with-ffi-debugging
   audit-ffi-usage
   get-call-statistics))

(in-package epsilon.foreign)

;;; Forward declarations to force loading of libffi-bridge
(declaim (ftype function load-libffi-extension))
(declaim (ftype function libffi-call))

;;; Ensure libffi is initialized
(defun ensure-libffi-initialized ()
  "Ensure libffi extension is loaded and ready"
  (unless (boundp '*libffi-available-p*)
    (defvar *libffi-available-p* nil))
  (unless *libffi-available-p*
    (when (fboundp 'load-libffi-extension)
      (funcall (symbol-function 'load-libffi-extension))))
  *libffi-available-p*)

;; Initialize on load
(eval-when (:load-toplevel :execute)
  (ensure-libffi-initialized))

;;;; Core FFI Functions

;;; shared-call: Main entry point for FFI calls
;;; 
;;; Parameters:
;;;   function-designator - Either a symbol or (symbol library-name)
;;;   return-type - Lisp representation of C return type
;;;   arg-types - List of argument types
;;;   &rest args - Actual arguments to pass
;;;
;;; Example: (shared-call (:printf "libc") :int (:string) "Hello %s\n" "world")

;; Global state for library management

(defvar *open-libraries* (map:make-map)
  "Map of library names to library handles")

(defvar *function-cache* (map:make-map)
  "Cache of function pointers keyed by (library-name function-name)")

(defvar *library-search-paths* 
  (append
   ;; Check for Nix store paths
   (when (probe-file "/nix/store/")
     (append
       ;; Current OpenSSL in this environment
       (when (probe-file "/nix/store/3dxy700bd43x9zh8n2klpygrj37yy67q-openssl-3.0.14/lib")
         (list "/nix/store/3dxy700bd43x9zh8n2klpygrj37yy67q-openssl-3.0.14/lib"))
       ;; Fallback glibc path
       (when (probe-file "/nix/store/qdcbgcj27x2kpxj2sf9yfvva7qsgg64g-glibc-2.38-77/lib")
         (list "/nix/store/qdcbgcj27x2kpxj2sf9yfvva7qsgg64g-glibc-2.38-77/lib"))))
   ;; Homebrew paths on macOS
   #+darwin
   (remove-if-not #'probe-file
     '("/opt/homebrew/lib" "/opt/homebrew/opt/openssl@3/lib" "/opt/homebrew/opt/openssl/lib"
       "/usr/local/lib" "/usr/local/opt/openssl@3/lib" "/usr/local/opt/openssl/lib"))
   ;; Standard paths
   '("/usr/lib" "/usr/local/lib" "/lib" "/lib64" 
     "/usr/lib/x86_64-linux-gnu" "/lib/x86_64-linux-gnu" "/opt/lib"))
  "Default paths to search for shared libraries")

;; Platform-specific library handling

(defun platform-library-name (name)
  "Convert library name to platform-specific format"
  ;; macOS
  #+darwin
  (cond
    ((string-suffix-p ".dylib" name) name)
    ((string-suffix-p ".so" name) (concatenate 'string name ".dylib"))
    ((string= name "libc") "/usr/lib/libSystem.B.dylib")
    ((string= name "libm") "/usr/lib/libSystem.B.dylib")
    ((string= name "libpthread") "/usr/lib/libSystem.B.dylib")
    ;; Handle special case for system SSL/crypto libraries
    ((string= name "libssl") "libssl.dylib")
    ((string= name "libcrypto") "libcrypto.dylib")
    ;; Don't double-prefix names that already start with "lib"
    ((and (>= (length name) 3) (string= (subseq name 0 3) "lib"))
     (concatenate 'string name ".dylib"))
    (t (concatenate 'string "lib" name ".dylib")))
  ;; Linux
  #+linux
  (cond
    ((string-suffix-p ".so" name) name)
    ((find #\. name) name) ; Has version number
    ((string= name "libc") "libc.so.6")
    ((string= name "libm") "libm.so.6")
    ((string= name "libpthread") "libpthread.so.0")
    ;; Handle special case for system SSL/crypto libraries - try versioned first
    ((string= name "libssl") "libssl.so.3")
    ((string= name "libcrypto") "libcrypto.so.3")
    ;; Don't double-prefix names that already start with "lib"
    ((and (>= (length name) 3) (string= (subseq name 0 3) "lib"))
     (concatenate 'string name ".so"))
    (t (concatenate 'string "lib" name ".so")))
  ;; Default fallback for other platforms
  #-(or darwin linux)
  name)

(defun string-suffix-p (suffix string)
  "Check if string ends with suffix"
  (let ((pos (search suffix string :from-end t)))
    (and pos (= pos (- (length string) (length suffix))))))

(defun find-library-path (name &key local paths)
  "Find full path to library file"
  (let ((lib-name (platform-library-name name))
        (search-paths (if local 
                          (or paths '("."))
                          (append (or paths '()) *library-search-paths*))))
    (loop for path in search-paths
          for full-path = (path:string-path-join path lib-name)
          when (probe-file full-path)
            return full-path
          finally (return lib-name)))) ; Return name if not found

(defun lib-open (library-name &key local paths)
  "Opens a shared library and returns a handle"
  (let ((existing (map:get *open-libraries* library-name)))
    (if existing
        existing
        (let* ((lib-path (find-library-path library-name :local local :paths paths))
               (handle (sb-alien:load-shared-object lib-path)))
          (setf *open-libraries* 
                (map:assoc *open-libraries* library-name handle))
          handle))))

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
  "Gets a pointer to a function in a library"
  (let* ((lib-name (loop for (name . handle) in (map:seq *open-libraries*)
                         when (eq handle library-handle)
                           return name))
         (cache-key (list lib-name function-name))
         (cached (map:get *function-cache* cache-key)))
    (if cached
        cached
        (let ((fn-ptr (sb-sys:find-dynamic-foreign-symbol-address 
                       (string function-name))))
          (when fn-ptr
            (setf *function-cache* 
                  (map:assoc *function-cache* cache-key fn-ptr)))
          fn-ptr))))

(defun shared-call (function-designator return-type arg-types &rest args)
  "Main entry point for FFI calls using libffi"
  ;; Delegate to the libffi implementation
  (apply #'shared-call-unified function-designator return-type arg-types args))

(defun shared-call-old-sbcl (function-designator return-type arg-types &rest args)
  "Old SBCL-based implementation - DEPRECATED, will be removed"
  (let ((function-address
          (etypecase function-designator
            (symbol (lib-function 
                     (lib-open "libc") 
                     (string function-designator)))
            (list (destructuring-bind (fn-name lib-name) function-designator
                    (lib-function 
                     (lib-open lib-name) 
                     (string fn-name)))))))
    (unless function-address
      (error "Could not find function ~A" function-designator))
    
    ;; Simplified implementation for common function signatures
    (cond
      ;; Zero-argument functions returning int (like getpid, kqueue)
      ((and (eq return-type :int) (null arg-types))
       (eval `(sb-alien:alien-funcall 
               (sb-alien:sap-alien 
                (sb-sys:int-sap ,function-address)
                (sb-alien:function sb-alien:int)))))
      
      ;; kevent-style functions: int fn(int, pointer, int, pointer, int, pointer)
      ((and (eq return-type :int) 
            (equal arg-types '(:int :pointer :int :pointer :int :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int 
                                     sb-alien:system-area-pointer sb-alien:int
                                     sb-alien:system-area-pointer sb-alien:int 
                                     sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; close-style functions: int fn(int)
      ((and (eq return-type :int) (equal arg-types '(:int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int))
                 ,@converted-args))))
      
      ;; strlen-style functions: unsigned-long fn(string)
      ((and (eq return-type :unsigned-long) (equal arg-types '(:string)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:unsigned-long sb-alien:c-string))
                 ,@converted-args))))
      
      ;; malloc-style functions: pointer fn(unsigned-long)
      ((and (eq return-type :pointer) (equal arg-types '(:unsigned-long)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:system-area-pointer sb-alien:unsigned-long))
                 ,@converted-args))))
      
      ;; free-style functions: void fn(pointer)
      ((and (eq return-type :void) (equal arg-types '(:pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:void sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; clock_gettime style: int fn(int, pointer)
      ((and (eq return-type :int) (equal arg-types '(:int :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; bind/connect style: int fn(int, pointer, unsigned-int)
      ((and (eq return-type :int) (equal arg-types '(:int :pointer :unsigned-int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:system-area-pointer sb-alien:unsigned-int))
                 ,@converted-args))))
      
      ;; qsort style: void fn(pointer, unsigned-long, unsigned-long, pointer)
      ((and (eq return-type :void) 
            (equal arg-types '(:pointer :unsigned-long :unsigned-long :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:void 
                                     sb-alien:system-area-pointer
                                     sb-alien:unsigned-long
                                     sb-alien:unsigned-long
                                     sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; getenv style: pointer fn(string)
      ((and (eq return-type :pointer) (equal arg-types '(:string)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:system-area-pointer sb-alien:c-string))
                 ,@converted-args))))
      
      ;; setenv style: int fn(string, string, int)
      ((and (eq return-type :int) (equal arg-types '(:string :string :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:c-string sb-alien:c-string sb-alien:int))
                 ,@converted-args))))
      
      ;; pipe style: int fn(pointer)  
      ((and (eq return-type :int) (equal arg-types '(:pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; listen style: int fn(int, int)
      ((and (eq return-type :int) (equal arg-types '(:int :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int))
                 ,@converted-args))))
      
      ;; write/read style: long fn(int, pointer, unsigned-long)
      ((and (eq return-type :long) (equal arg-types '(:int :pointer :unsigned-long)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:long sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:unsigned-long))
                 ,@converted-args))))
      
      ;; socket style: int fn(int, int, int)
      ((and (eq return-type :int) (equal arg-types '(:int :int :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
                 ,@converted-args))))
                 
      ;; fcntl style: int fn(int, int, long)
      ((and (eq return-type :int) (equal arg-types '(:int :int :long)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int sb-alien:long))
                 ,@converted-args))))
      
      ;; bind/connect style: int fn(int, pointer, unsigned-int)
      ((and (eq return-type :int) (equal arg-types '(:int :pointer :unsigned-int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int 
                                     sb-alien:system-area-pointer sb-alien:unsigned-int))
                 ,@converted-args))))
      
      ;; sendto style: long fn(int, pointer, unsigned-long, int, pointer, unsigned-int)
      ((and (eq return-type :long) 
            (equal arg-types '(:int :pointer :unsigned-long :int :pointer :unsigned-int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:long sb-alien:int 
                                     sb-alien:system-area-pointer sb-alien:unsigned-long
                                     sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:unsigned-int))
                 ,@converted-args))))
      
      ;; recvfrom style: long fn(int, pointer, unsigned-long, int, pointer, pointer)
      ((and (eq return-type :long) 
            (equal arg-types '(:int :pointer :unsigned-long :int :pointer :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:long sb-alien:int 
                                     sb-alien:system-area-pointer sb-alien:unsigned-long
                                     sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; accept style: int fn(int, pointer, pointer)
      ((and (eq return-type :int) (equal arg-types '(:int :pointer :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int 
                                     sb-alien:system-area-pointer sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; send/recv style: long fn(int, pointer, unsigned-long, int)
      ((and (eq return-type :long) 
            (equal arg-types '(:int :pointer :unsigned-long :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:long sb-alien:int 
                                     sb-alien:system-area-pointer 
                                     sb-alien:unsigned-long sb-alien:int))
                 ,@converted-args))))
      
      ;; setsockopt style: int fn(int, int, int, pointer, unsigned-int)
      ((and (eq return-type :int) 
            (equal arg-types '(:int :int :int :pointer :unsigned-int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int sb-alien:int
                                     sb-alien:system-area-pointer sb-alien:unsigned-int))
                 ,@converted-args))))
      
      ;; getsockopt style: int fn(int, int, int, pointer, pointer)
      ((and (eq return-type :int) 
            (equal arg-types '(:int :int :int :pointer :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int sb-alien:int
                                     sb-alien:system-area-pointer sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; htons/ntohs style: unsigned-short fn(unsigned-short)
      ((and (eq return-type :unsigned-short) (equal arg-types '(:unsigned-short)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:unsigned-short sb-alien:unsigned-short))
                 ,@converted-args))))
      
      ;; htonl/ntohl style: unsigned-int fn(unsigned-int)
      ((and (eq return-type :unsigned-int) (equal arg-types '(:unsigned-int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:unsigned-int sb-alien:unsigned-int))
                 ,@converted-args))))
      
      ;; socketpair style: int fn(int, int, int, pointer)
      ((and (eq return-type :int) (equal arg-types '(:int :int :int :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:int 
                                     sb-alien:int sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; epoll_wait style: int fn(int, pointer, int, int)
      ((and (eq return-type :int) (equal arg-types '(:int :pointer :int :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:int sb-alien:int))
                 ,@converted-args))))
      
      ;; OpenSSL-specific signatures
      
      ;; pointer fn() - e.g., TLS_client_method
      ((and (eq return-type :pointer) (null arg-types))
       (eval `(sb-alien:alien-funcall 
               (sb-alien:sap-alien 
                (sb-sys:int-sap ,function-address)
                (sb-alien:function sb-alien:system-area-pointer)))))
      
      ;; pointer fn(pointer) - e.g., SSL_CTX_new, SSL_new
      ((and (eq return-type :pointer) (equal arg-types '(:pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:system-area-pointer sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; int fn(pointer, string, int) - e.g., SSL_CTX_use_certificate_file
      ((and (eq return-type :int) (equal arg-types '(:pointer :string :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:c-string sb-alien:int))
                 ,@converted-args))))
      
      ;; int fn(pointer, string) - e.g., SSL_CTX_set_cipher_list
      ((and (eq return-type :int) (equal arg-types '(:pointer :string)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:system-area-pointer sb-alien:c-string))
                 ,@converted-args))))
      
      ;; void fn(pointer, int, pointer) - e.g., SSL_CTX_set_verify
      ((and (eq return-type :void) (equal arg-types '(:pointer :int :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:void sb-alien:system-area-pointer 
                                     sb-alien:int sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; int fn(pointer, pointer, int) - e.g., SSL_read, SSL_write
      ((and (eq return-type :int) (equal arg-types '(:pointer :pointer :int)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:system-area-pointer 
                                     sb-alien:system-area-pointer sb-alien:int))
                 ,@converted-args))))
      
      ;; string fn(pointer) - e.g., SSL_CIPHER_get_name, SSL_get_version
      ((and (eq return-type :string) (equal arg-types '(:pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:c-string sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      ;; unsigned-long fn() - e.g., ERR_get_error
      ((and (eq return-type :unsigned-long) (null arg-types))
       (eval `(sb-alien:alien-funcall 
               (sb-alien:sap-alien 
                (sb-sys:int-sap ,function-address)
                (sb-alien:function sb-alien:unsigned-long)))))
      
      ;; void fn(unsigned-long, pointer, size) - e.g., ERR_error_string_n
      ((and (eq return-type :void) (equal arg-types '(:unsigned-long :pointer :size)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:void sb-alien:unsigned-long 
                                     sb-alien:system-area-pointer sb-alien:unsigned-long))
                 ,@converted-args))))
      
      ;; getaddrinfo style: int fn(c-string, c-string, pointer, pointer)
      ((and (eq return-type :int) (equal arg-types '(:c-string :c-string :pointer :pointer)))
       (let ((converted-args (mapcar (lambda (arg type) (convert-to-foreign arg type)) 
                                     args arg-types)))
         (eval `(sb-alien:alien-funcall 
                 (sb-alien:sap-alien 
                  (sb-sys:int-sap ,function-address)
                  (sb-alien:function sb-alien:int sb-alien:c-string sb-alien:c-string
                                     sb-alien:system-area-pointer sb-alien:system-area-pointer))
                 ,@converted-args))))
      
      (t
       (error "Function signature ~A ~A not yet implemented in simplified FFI" 
              return-type arg-types)))))

;; Type conversion and mapping system

(defvar *primitive-type-map* 
  (map:make-map :char 'sb-alien:char
                :unsigned-char 'sb-alien:unsigned-char
                :short 'sb-alien:short
                :unsigned-short 'sb-alien:unsigned-short
                :int 'sb-alien:int
                :unsigned-int 'sb-alien:unsigned-int
                :long 'sb-alien:long
                :unsigned-long 'sb-alien:unsigned-long
                :float 'sb-alien:single-float
                :double 'sb-alien:double-float
                :pointer 'sb-alien:system-area-pointer
                :string 'sb-alien:c-string
                :void 'sb-alien:void)
  "Map of Lisp type keywords to SBCL alien types")

(defun lisp-type-to-alien (type)
  "Convert Lisp type specifier to SBCL alien type"
  (cond
    ((keywordp type)
     (or (map:get *primitive-type-map* type)
         (error "Unknown primitive type: ~A" type)))
    ((and (listp type) (eq (first type) :pointer))
     `(sb-alien:* ,(lisp-type-to-alien (second type))))
    ((and (listp type) (eq (first type) :array))
     `(sb-alien:array ,(lisp-type-to-alien (second type)) ,(third type)))
    (t
     (error "Unsupported type specifier: ~A" type))))

;; Now delegating to trampoline:convert-to-foreign
(defun convert-to-foreign (value type)
  "Convert Lisp value to foreign representation"
  (trampoline:convert-to-foreign value type))

;;; defshared: Defines a Lisp function that calls a C function
;;; Creates optimized calling paths based on type information
;;;
;;; Parameters:
;;;   lisp-name - Symbol to define in Lisp
;;;   c-name - String naming the C function
;;;   library - Library name or handle
;;;   return-type - Return type specification
;;;   arg-specs - List of (arg-name arg-type) pairs
;;;   &key documentation - Optional docstring
;;;
;;; Example:
;;; (defshared my-printf "printf" "libc" :int (format :string) 
;;;            :documentation "Calls C printf function")

(defmacro defshared (lisp-name c-name library return-type &rest args)
  "Defines a Lisp function that calls a C function"
  (let* ((doc-pos (position :documentation args))
         (documentation (when doc-pos (nth (1+ doc-pos) args)))
         (arg-specs (if doc-pos
                        (append (subseq args 0 doc-pos)
                                (subseq args (+ doc-pos 2)))
                        args))
         ;; Filter out empty lists which represent no arguments
         (arg-specs (remove-if (lambda (spec) (and (listp spec) (null spec))) arg-specs))
         (arg-names (when arg-specs (mapcar #'first arg-specs)))
         (arg-types (when arg-specs (mapcar #'second arg-specs))))
    `(defun ,lisp-name ,arg-names
       ,@(when documentation (list documentation))
       (shared-call (list ,c-name ,library) ,return-type ',(or arg-types '()) ,@arg-names))))

;;;; Type Management

;;; define-foreign-struct: Defines a foreign structure layout
;;; Uses clang to determine actual memory layout
;;; Creates optimized accessors for structure fields
;;; Handles alignment and packing automatically
;;;
;;; Parameters:
;;;   name - Symbol naming the structure
;;;   &rest fields - List of field definitions (name type &key offset)
;;;   &key from-header - C header file to extract from
;;;        c-name - Name in C if different from Lisp name
;;;
;;; Example:
;;; (define-foreign-struct timespec
;;;   (sec :long)
;;;   (nsec :long)
;;;   :from-header "time.h")

(defmacro define-foreign-struct (name &rest fields &key from-header c-name)
  "Stub implementation - define a foreign struct"
  (declare (ignore fields from-header c-name))
  `(defstruct ,name))

;;; with-foreign-struct: Allocates a foreign structure and binds it
;;; Zero-copy where possible for efficiency
;;;
;;; Parameters:
;;;   (var type) - Binding and structure type
;;;   &body body - Code to execute with binding
;;;
;;; Example:
;;; (with-foreign-struct (ts 'timespec)
;;;   (setf (struct-slot ts 'sec) 10)
;;;   (my-function ts))

(defmacro with-foreign-struct ((var type) &body body)
  "Stub implementation - allocate foreign struct"
  (declare (ignore type))
  `(let ((,var (foreign-alloc 64))) ; Allocate 64 bytes for any struct
     (unwind-protect
          (progn ,@body)
       (foreign-free ,var))))

;;; map-struct: Maps a Lisp structure to/from a foreign structure
;;; Enables zero-copy sharing of structure data
;;;
;;; Parameters:
;;;   lisp-value - Lisp structure or object
;;;   foreign-type - Foreign structure type
;;;   direction - :to-foreign or :from-foreign or :bidirectional
;;;
;;; Returns: Foreign structure pointer or updated Lisp value

(defun map-struct (lisp-value foreign-type &optional (direction :bidirectional))
  "Stub implementation - map between Lisp and foreign structs"
  (declare (ignore foreign-type direction))
  lisp-value)

;;; foreign-array: Creates or maps an array for foreign code
;;; Zero-copy array sharing with foreign code
;;; Handles multi-dimensional arrays
;;;
;;; Parameters:
;;;   element-type - Type of array elements
;;;   dimensions - List of array dimensions
;;;   &key initial-contents - Initial data
;;;        existing-array - Lisp array to share
;;;        foreign-pointer - Foreign memory to use
;;;
;;; Returns: Array object that can be passed to foreign code

(defun foreign-array (element-type dimensions &key initial-contents existing-array foreign-pointer)
  "Stub implementation - create foreign array"
  (declare (ignore element-type initial-contents existing-array foreign-pointer))
  (apply #'make-array dimensions))

;;; with-zero-copy: Executes body with zero-copy foreign data sharing
;;; Optimizes data transfer between Lisp and C
;;;
;;; Parameters:
;;;   bindings - List of (var lisp-value foreign-type) triplets
;;;   &body body - Code to execute with bindings
;;;
;;; Example:
;;; (with-zero-copy ((farr my-array :float-array)
;;;                  (fstruct my-struct 'mystruct))
;;;   (my-c-function farr fstruct))

(defmacro with-zero-copy (bindings &body body)
  "Stub implementation - zero-copy data sharing"
  (let ((var-bindings (mapcar (lambda (binding)
                                `(,(first binding) ,(second binding)))
                              bindings)))
    `(let ,var-bindings
       ,@body)))

;;;; Memory Management

;; Global hash table to track allocated memory
(defvar *allocated-memory* (make-hash-table :test 'eql)
  "Maps SAPs to their underlying alien pointers for proper cleanup")

(defun foreign-alloc (type-or-size &key count initial-element initial-contents finalize)
  "Allocates foreign memory and returns a system area pointer"
  (let* ((element-size (if (keywordp type-or-size)
                           (alien-type-size type-or-size)
                           type-or-size))
         (total-size (* element-size (or count 1)))
         (alien-ptr (sb-alien:make-alien sb-alien:char total-size))
         (sap (sb-alien:alien-sap alien-ptr)))
    
    ;; Store the alien pointer for later cleanup
    (setf (gethash (sb-sys:sap-int sap) *allocated-memory*) alien-ptr)
    
    ;; Initialize memory if requested
    (cond
      (initial-contents
       (loop for i from 0
             for value in (coerce initial-contents 'list)
             while (< i total-size)
             do (setf (sb-alien:deref alien-ptr i) value)))
      (initial-element
       (loop for i from 0 below total-size
             do (setf (sb-alien:deref alien-ptr i) initial-element)))
      (t ;; Zero-initialize by default for safety
       (loop for i from 0 below total-size
             do (setf (sb-alien:deref alien-ptr i) 0))))
    
    ;; Register finalizer if requested
    (when finalize
      (let ((sap-int (sb-sys:sap-int sap)))
        (sb-ext:finalize sap (lambda () 
                               (let ((ptr (gethash sap-int *allocated-memory*)))
                                 (when ptr
                                   (sb-alien:free-alien ptr)
                                   (remhash sap-int *allocated-memory*)))))))
    
    ;; Return the system area pointer
    sap))

(defun foreign-free (pointer)
  "Explicitly frees foreign memory"
  (when pointer
    (let* ((sap-int (if (sb-sys:system-area-pointer-p pointer)
                        (sb-sys:sap-int pointer)
                        pointer))
           (alien-ptr (gethash sap-int *allocated-memory*)))
      (when alien-ptr
        ;; Free the alien memory
        (sb-alien:free-alien alien-ptr)
        ;; Remove from tracking table
        (remhash sap-int *allocated-memory*)
        t))))

(defun alien-type-size (type)
  "Get size in bytes of alien type"
  (case type
    (:char 1)
    (:short 2)
    (:int 4)
    (:long 8)
    (:float 4)
    (:double 8)
    (:pointer 8)
    (otherwise 1)))

(defmacro with-foreign-memory (bindings &body body)
  "Allocates memory for the duration of body"
  (let ((cleanup-forms '())
        (binding-forms '()))
    (dolist (binding bindings)
      (destructuring-bind (var type-or-size &rest keys) binding
        (push `(,var (foreign-alloc ,type-or-size ,@keys)) binding-forms)
        (push `(foreign-free ,var) cleanup-forms)))
    `(let ,(reverse binding-forms)
       (unwind-protect
            (progn ,@body)
         ,@cleanup-forms))))

(defun register-finalizer (object function)
  "Registers a function to run when object is GC'd"
  (sb-ext:finalize object function))

;;;; Structure Discovery

;;; grovel-struct: Extracts structure layout from C
;;; Uses clang to determine exact memory layout
;;;
;;; Parameters:
;;;   struct-name - Name of C structure
;;;   &key headers - List of headers to include
;;;        include-dirs - Additional include directories
;;;
;;; Returns: Structure description for use with define-foreign-struct

(defun grovel-struct (struct-name &key headers include-dirs)
  "Stub implementation - extract C struct layout"
  (declare (ignore struct-name headers include-dirs))
  nil)

;;; grovel-lib: Extracts function information from a library
;;; Uses various tools (nm, objdump, clang) to get function signatures
;;;
;;; Parameters:
;;;   library-name - Name of library to analyze
;;;   &key headers - Related headers to parse
;;;        include-dirs - Additional include directories
;;;
;;; Returns: List of function descriptions

(defun grovel-lib (library-name &key headers include-dirs)
  "Stub implementation - extract library functions"
  (declare (ignore library-name headers include-dirs))
  nil)

;;; parse-header: Extracts type information from C header files
;;; Creates Lisp-accessible descriptions of C types
;;;
;;; Parameters:
;;;   header-file - Header file to parse
;;;   &key include-dirs - Additional include directories
;;;        types - Specific types to extract (nil for all)
;;;        recursive - Whether to follow included headers
;;;
;;; Returns: Hash table mapping C types to their descriptions

(defun parse-header (header-file &key include-dirs types recursive)
  "Stub implementation - parse C headers"
  (declare (ignore header-file include-dirs types recursive))
  map:+empty+)

;;;; Type Mapping

;;; def-type-map: Defines mapping between Lisp and C types
;;; Creates optimized conversion paths for types
;;;
;;; Parameters:
;;;   lisp-type - Lisp type specifier
;;;   c-type - C type specifier
;;;   &key to-foreign - Function to convert Lisp->C
;;;        from-foreign - Function to convert C->Lisp
;;;        direct - If true, indicates no conversion needed
;;;
;;; Example:
;;; (def-type-map 'string '(:pointer :char)
;;;   :to-foreign #'string-to-c-string
;;;   :from-foreign #'c-string-to-string)

(defmacro def-type-map (lisp-type c-type &key to-foreign from-foreign direct)
  "Stub implementation - define type mapping"
  (declare (ignore lisp-type c-type to-foreign from-foreign direct))
  nil)

;;; *primitive-type-map* - Variable holding primitive type mappings
;;; Maps Lisp primitive types to their C equivalents
;;; Used by shared-call for efficient type conversion

;; Helper functions for epoll data handling (reused from epsilon.linux)

(defun make-epoll-data (&key fd ptr u32 u64)
  "Create epoll_data_t union value"
  (cond
    (fd (logand fd #xffffffff))            ; Store fd as lower 32 bits
    (ptr ptr)                              ; Store pointer value
    (u32 (logand u32 #xffffffff))         ; Store 32-bit value
    (u64 (logand u64 #xffffffffffffffff)) ; Store 64-bit value
    (t 0)))

(defun epoll-data-fd (data)
  "Extract file descriptor from epoll_data_t"
  (logand data #xffffffff))

(defvar *primitive-type-map*)

;;;; New Trampoline-based Fast FFI

(defun shared-call-fast (function-designator return-type arg-types &rest args)
  "Fast FFI call using compiled trampolines instead of eval"
  (let ((function-address
          (etypecase function-designator
            (symbol (lib-function 
                     (lib-open "libc") 
                     (string function-designator)))
            (list (destructuring-bind (fn-name lib-name) function-designator
                    (lib-function 
                     (lib-open lib-name) 
                     (string fn-name)))))))
    (unless function-address
      (error "Could not find function ~A" function-designator))
    ;; Use trampoline system
    (trampoline:call-with-trampoline function-address return-type arg-types args)))

(defmacro defshared-fast (lisp-name c-name library return-type &rest args)
  "Fast version of defshared using trampolines"
  (let* ((doc-pos (position :documentation args))
         (documentation (when doc-pos (nth (1+ doc-pos) args)))
         (arg-specs (if doc-pos
                        (append (subseq args 0 doc-pos)
                                (subseq args (+ doc-pos 2)))
                        args))
         ;; Filter out empty lists which represent no arguments
         (arg-specs (remove-if (lambda (spec) (and (listp spec) (null spec))) arg-specs))
         (arg-names (when arg-specs (mapcar #'first arg-specs)))
         (arg-types (when arg-specs (mapcar #'second arg-specs))))
    `(progn
       ;; Register the signature
       (trampoline:register-signature ',lisp-name ,return-type ',(or arg-types '()))
       ;; Define the function
       (defun ,lisp-name ,arg-names
         ,@(when documentation (list documentation))
         (shared-call-fast (list ,c-name ,library) ,return-type ',(or arg-types '()) ,@arg-names)))))

;; Helper functions for type conversion - these are actually needed
(defun convert-from-foreign (value type) 
  "Convert foreign value to Lisp representation"
  (trampoline:convert-from-foreign value type))

;; Re-export bool conversion functions from marshalling
(defun bool-to-foreign (value &optional type)
  "Convert Lisp boolean to C bool"
  (marshalling:bool-to-foreign value type))

(defun foreign-to-bool (value &optional type)
  "Convert C bool to Lisp boolean"
  (marshalling:foreign-to-bool value type))

;; Re-export error handling from marshalling
(defun foreign-error-p (condition)
  "Check if condition is a foreign error"
  (typep condition 'marshalling:foreign-error))

(defun foreign-error-code (condition)
  "Get error code from foreign error"
  (marshalling:foreign-error-code condition))

(defun foreign-error-function (condition)
  "Get function name from foreign error"
  (marshalling:foreign-error-function condition))

;; Re-export the condition type
(deftype foreign-error ()
  'marshalling:foreign-error)

;; Re-export define-c-type
(defun define-c-type (name size &rest args)
  "Define a new C type"
  (apply #'marshalling:define-c-type name size args))

;; Re-export smart defshared macro - now handled by smart-ffi module

;; Re-export array handling macros
(defmacro with-output-array ((var count type) &body body)
  "Create an array for output parameters"
  `(marshalling:with-output-array (,var ,count ,type) ,@body))

;; Re-export struct functions
(defun define-c-struct (name fields)
  "Define a C struct"
  (struct:define-c-struct name fields))

(defmacro with-c-struct ((var type) &body body)
  "Allocate a C struct"
  `(struct:with-c-struct (,var ,type) ,@body))

(defun struct-ref (struct field)
  "Get struct field value"
  (struct:struct-ref struct field))

(defun (setf struct-ref) (value struct field)
  "Set struct field value"
  (setf (struct:struct-ref struct field) value))


