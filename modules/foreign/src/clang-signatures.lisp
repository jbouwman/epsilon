;;;; clang-signatures.lisp - Function signature extraction using clang
;;;;
;;;; This module extends the epsilon.clang parser to extract function
;;;; signatures for automatic FFI call setup.

(defpackage epsilon.clang.signatures
  (:use cl epsilon.syntax)
  (:local-nicknames
   (clang epsilon.clang)
   (p epsilon.parser)
   (lexer epsilon.lexer)
   (map epsilon.map)
   (str epsilon.string)
   (seq epsilon.sequence))
  (:export
   ;; Signature extraction
   #:extract-function-signature
   #:extract-function-signatures
   #:parse-header-for-signatures
   
   ;; Header generation
   #:generate-minimal-header
   #:detect-function-headers
   #:compile-header-to-ast
   
   ;; Signature manipulation
   #:normalize-signature
   #:normalize-c-types
   #:signature-return-type
   #:signature-arg-types
   #:signature-function-name
   #:c-type-to-epsilon-type
   #:signature-to-epsilon-types
   
   ;; Database
   #:*signature-database*
   #:cache-signature
   #:get-cached-signature
   #:load-signature-database
   #:save-signature-database
   #:function-signature
   #:make-function-signature
   #:function-signature-name
   #:function-signature-return-type
   #:function-signature-arg-types
   
   ;; Integration
   #:clang-signature-pipeline))

(in-package :epsilon.clang.signatures)

;;;; Signature Data Structures

(defstruct function-signature
  name
  return-type
  arg-types
  headers
  source-location
  cached-time)

(defvar *signature-database* (make-hash-table :test 'equal)
  "Database of discovered function signatures")

;;;; Header Detection and Generation

(defparameter *function-header-map*
  '(;; String functions
    ("strlen" . ("string.h"))
    ("strcpy" . ("string.h"))
    ("strcmp" . ("string.h"))
    ("strcat" . ("string.h"))
    ("memcpy" . ("string.h"))
    ("memset" . ("string.h"))
    ("memcmp" . ("string.h"))
    
    ;; Memory management
    ("malloc" . ("stdlib.h"))
    ("calloc" . ("stdlib.h"))
    ("realloc" . ("stdlib.h"))
    ("free" . ("stdlib.h"))
    
    ;; I/O functions
    ("printf" . ("stdio.h"))
    ("fprintf" . ("stdio.h"))
    ("sprintf" . ("stdio.h"))
    ("scanf" . ("stdio.h"))
    ("fopen" . ("stdio.h"))
    ("fclose" . ("stdio.h"))
    ("fread" . ("stdio.h"))
    ("fwrite" . ("stdio.h"))
    
    ;; System calls
    ("open" . ("fcntl.h" "sys/stat.h" "sys/types.h"))
    ("close" . ("unistd.h"))
    ("read" . ("unistd.h"))
    ("write" . ("unistd.h"))
    ("lseek" . ("unistd.h"))
    ("getpid" . ("unistd.h"))
    ("getuid" . ("unistd.h"))
    ("getgid" . ("unistd.h"))
    
    ;; Math functions
    ("sin" . ("math.h"))
    ("cos" . ("math.h"))
    ("tan" . ("math.h"))
    ("sqrt" . ("math.h"))
    ("pow" . ("math.h"))
    ("exp" . ("math.h"))
    ("log" . ("math.h"))
    
    ;; Time functions
    ("time" . ("time.h"))
    ("clock" . ("time.h"))
    ("gmtime" . ("time.h"))
    ("localtime" . ("time.h"))
    
    ;; Network functions (POSIX)
    ("socket" . ("sys/socket.h"))
    ("bind" . ("sys/socket.h"))
    ("listen" . ("sys/socket.h"))
    ("accept" . ("sys/socket.h"))
    ("connect" . ("sys/socket.h"))
    ("send" . ("sys/socket.h"))
    ("recv" . ("sys/socket.h")))
  "Mapping of function names to their likely headers")

(defun detect-function-headers (function-name)
  "Detect likely headers for a function"
  (or (cdr (assoc function-name *function-header-map* :test #'string=))
      ;; Default headers for unknown functions
      '("stdlib.h" "unistd.h" "string.h")))

(defun generate-minimal-header (function-name &optional additional-headers)
  "Generate minimal C header for function analysis"
  (let ((headers (append (detect-function-headers function-name)
                        additional-headers)))
    (with-output-to-string (s)
      ;; Standard includes
      (dolist (header headers)
        (format s "#include <~A>~%" header))
      (terpri s)
      
      ;; Function declaration (just the name to ensure it's found)
      ;; We'll let clang provide the actual signature
      (format s "// Function: ~A~%" function-name))))

;;;; C Type Mapping

(defun c-type-to-epsilon-type (c-type-spec)
  "Convert C type specifier to epsilon FFI type"
  (cond
    ;; Handle list of type specifiers
    ((listp c-type-spec)
     (let ((specs (remove-if (lambda (x) (member x '("static" "extern" "inline" "const" "volatile" "restrict")
                                                :test #'string=))
                            c-type-spec)))
       (c-type-to-epsilon-type (first specs))))
    
    ;; Single type specifier
    ((stringp c-type-spec)
     (cond
       ((string= c-type-spec "void") :void)
       ((string= c-type-spec "char") :char)
       ((string= c-type-spec "int") :int)
       ((string= c-type-spec "long") :long)
       ((string= c-type-spec "short") :short)
       ((string= c-type-spec "float") :float)
       ((string= c-type-spec "double") :double)
       ((string= c-type-spec "unsigned") :unsigned-int)  ; Default to unsigned int
       ((string= c-type-spec "signed") :int)             ; Default to signed int
       ((string= c-type-spec "size_t") :unsigned-long)
       ((string= c-type-spec "ssize_t") :long)
       (t :pointer)))  ; Default unknown types to pointer
    
    ;; Symbol type specifier  
    ((symbolp c-type-spec)
     (c-type-to-epsilon-type (string c-type-spec)))
    
    ;; Default
    (t :pointer)))

(defun normalize-c-types (type-specs)
  "Normalize C type specifiers to standard forms"
  (when type-specs
    (let ((cleaned (remove-if (lambda (x) 
                               (member x '("static" "extern" "inline" "const" "volatile" "restrict")
                                      :test #'string=))
                             type-specs)))
      (cond
        ;; Handle unsigned/signed modifiers
        ((and (member "unsigned" cleaned :test #'string=)
              (member "int" cleaned :test #'string=))
         :unsigned-int)
        ((and (member "unsigned" cleaned :test #'string=)
              (member "long" cleaned :test #'string=))
         :unsigned-long)
        ((and (member "unsigned" cleaned :test #'string=)
              (member "char" cleaned :test #'string=))
         :unsigned-char)
        ((and (member "unsigned" cleaned :test #'string=)
              (member "short" cleaned :test #'string=))
         :unsigned-short)
        
        ;; Handle long long
        ((and (= (count "long" cleaned :test #'string=) 2))
         :long-long)
        
        ;; Single types
        ((member "void" cleaned :test #'string=) :void)
        ((member "char" cleaned :test #'string=) :char)
        ((member "short" cleaned :test #'string=) :short)
        ((member "int" cleaned :test #'string=) :int)
        ((member "long" cleaned :test #'string=) :long)
        ((member "float" cleaned :test #'string=) :float)
        ((member "double" cleaned :test #'string=) :double)
        
        ;; Special types
        ((member "size_t" cleaned :test #'string=) :size-t)
        ((member "ssize_t" cleaned :test #'string=) :ssize-t)
        
        ;; Default to first type if unknown combination
        (t (c-type-to-epsilon-type (first cleaned)))))))

;;;; AST Analysis

(defun extract-function-from-ast (ast function-name)
  "Extract function declaration from parsed AST"
  (labels ((find-function (nodes)
             (when (listp nodes)
               (dolist (node nodes)
                 (when (and (listp node)
                           (eq (getf node :type) :function)
                           (string= (getf node :name) function-name))
                   (return-from find-function node))
                 ;; Recursively search in nested structures
                 (let ((result (find-function node)))
                   (when result
                     (return-from find-function result)))))))
    (find-function ast)))

(defun extract-signature-from-function-node (function-node)
  "Extract signature information from function AST node"
  (when (and function-node (eq (getf function-node :type) :function))
    (let* ((return-type (normalize-c-types (getf function-node :return-type)))
           (params (getf function-node :parameters))
           (arg-types (mapcar (lambda (param)
                               (normalize-c-types (getf param :specifiers)))
                             params)))
      (make-function-signature
       :name (getf function-node :name)
       :return-type return-type
       :arg-types arg-types
       :cached-time (get-universal-time)))))

;;;; High-Level Signature Extraction

(defun parse-header-for-signatures (header-content)
  "Parse C header content and extract all function signatures"
  (handler-case
      (let* ((tokens (clang:tokenize (make-string-input-stream header-content)))
             (parse-result (p:parse (clang:translation-unit) tokens)))
        (if (p:success-p parse-result)
            (let ((ast (p:success-value parse-result)))
              (extract-functions-from-ast ast))
            (progn
              (warn "Failed to parse header content")
              nil)))
    (error (e)
      (warn "Error parsing header: ~A" e)
      nil)))

(defun extract-functions-from-ast (ast)
  "Extract all function declarations from AST"
  (let ((functions '()))
    (labels ((collect-functions (nodes)
               (when (listp nodes)
                 (dolist (node nodes)
                   (when (and (listp node)
                             (eq (getf node :type) :function))
                     (let ((sig (extract-signature-from-function-node node)))
                       (when sig
                         (push sig functions))))
                   ;; Recursively search
                   (collect-functions node)))))
      (collect-functions ast)
      (nreverse functions))))

(defun extract-function-signature (function-name &key headers include-paths)
  "Extract function signature using clang parser"
  (declare (ignore include-paths))
  (let* ((header-content (generate-minimal-header function-name headers))
         (signatures (parse-header-for-signatures header-content)))
    (find-if (lambda (sig)
               (string= (function-signature-name sig) function-name))
             signatures)))

;;;; Signature Database Management

(defun cache-signature (function-name signature &key library headers)
  "Cache discovered signature for reuse"
  (let ((key (if library
                 (list function-name library)
                 function-name)))
    (setf (gethash key *signature-database*)
          (list :signature signature 
                :library library 
                :headers headers
                :timestamp (get-universal-time)))))

(defun get-cached-signature (function-name &optional library)
  "Get cached signature for function"
  (let ((key (if library
                 (list function-name library)
                 function-name)))
    (let ((cached (gethash key *signature-database*)))
      (when cached
        (getf cached :signature)))))

(defun save-signature-database (&optional (path "~/.epsilon-ffi-signatures.lisp"))
  "Save signature database to file"
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out ";; Epsilon FFI Signature Database~%")
    (format out ";; Generated: ~A~%" (get-universal-time))
    (terpri out)
    (maphash (lambda (key value)
               (format out "~S~%" (list key value)))
             *signature-database*)))

(defun load-signature-database (&optional (path "~/.epsilon-ffi-signatures.lisp"))
  "Load signature database from file"
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (setf *signature-database* (make-hash-table :test 'equal))
      (loop for entry = (read in nil 'eof)
            until (eq entry 'eof)
            when (and (listp entry) (= (length entry) 2))
              do (setf (gethash (first entry) *signature-database*)
                       (second entry))))))

;;;; Complete Pipeline

(defun clang-signature-pipeline (function-name &key library headers)
  "Complete signature extraction pipeline"
  (handler-case
      (let* ((detected-headers (or headers (detect-function-headers function-name)))
             (signature (extract-function-signature function-name :headers detected-headers)))
        (when signature
          (cache-signature function-name signature :library library :headers detected-headers)
          signature))
    (error (e)
      (warn "Signature extraction pipeline failed for ~A: ~A" function-name e)
      nil)))

;;;; Integration Helpers

(defun signature-to-epsilon-types (signature)
  "Convert function signature to epsilon FFI types"
  (when signature
    (list :return-type (function-signature-return-type signature)
          :arg-types (function-signature-arg-types signature))))

(defun test-signature-extraction ()
  "Test signature extraction for common functions"
  (format t "Testing signature extraction...~%")
  
  (dolist (fn-name '("strlen" "malloc" "getpid" "printf"))
    (format t "Testing ~A: " fn-name)
    (let ((signature (clang-signature-pipeline fn-name)))
      (if signature
          (format t "Success - ~A~%" (signature-to-epsilon-types signature))
          (format t "Failed~%")))))