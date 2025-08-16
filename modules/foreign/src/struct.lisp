(defpackage epsilon.foreign.struct
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (trampoline epsilon.foreign.trampoline)
   (clang epsilon.clang)
   (parser epsilon.parser))
  (:export
   ;; Struct definition
   #:define-c-struct
   #:define-c-struct-auto
   #:define-c-union
   #:parse-c-struct
   
   ;; Layout management
   #:struct-layout-p
   #:get-struct-layout
   #:struct-layout-size
   #:struct-layout-alignment
   #:struct-field-offset
   #:struct-field-type
   #:struct-field-size
   #:struct-has-field-p
   
   ;; Instance creation and access
   #:with-c-struct
   #:with-c-union
   #:with-struct-view
   #:with-foreign-object
   #:struct-ref
   #:struct-ref-ptr
   #:union-ref
   #:struct-pointer
   #:foreign-alloc
   #:foreign-free
   
   ;; Serialization
   #:struct-to-bytes
   #:bytes-to-struct
   #:struct-to-string))

(in-package :epsilon.foreign.struct)

;;;; C Struct Layout Discovery and Support

;;; Typedef resolution system

(defvar *typedef-table* map:+empty+
  "Registry of typedef mappings to primitive types")

(defvar *system-typedefs*
  '(;; Standard C99 types typically found in stdint.h
    ("int8_t" . :char)
    ("uint8_t" . :unsigned-char)
    ("int16_t" . :short)
    ("uint16_t" . :unsigned-short)
    ("int32_t" . :int)
    ("uint32_t" . :unsigned-int)
    ("int64_t" . :long-long)
    ("uint64_t" . :unsigned-long-long)
    ;; Common POSIX types (platform-dependent, these are typical 64-bit mappings)
    ("size_t" . :unsigned-long)
    ("ssize_t" . :long)
    ("time_t" . :long)
    ("off_t" . :long)
    ("pid_t" . :int)
    ("uid_t" . :unsigned-int)
    ("gid_t" . :unsigned-int))
  "Common system typedef mappings - these should ideally be discovered at runtime")

(defun initialize-typedef-table ()
  "Initialize the typedef table with common system types"
  (loop for (name . type) in *system-typedefs*
        do (setf *typedef-table* (map:assoc *typedef-table* name type))))

;; Initialize on load
(initialize-typedef-table)

(defun discover-typedef (typedef-name)
  "Discover the primitive type for a typedef"
  (or
   ;; First check if we already know it
   (resolve-typedef typedef-name)
   ;; Try to discover its size
   (let ((size (discover-typedef-size typedef-name)))
     (when size
       (let ((primitive-type
              (case size
                (1 :unsigned-char)  ; Assume unsigned for now
                (2 :unsigned-short)
                (4 :unsigned-int)
                (8 :unsigned-long)
                (otherwise :pointer))))
         ;; Cache the result
         (register-typedef typedef-name primitive-type)
         primitive-type)))
   ;; Fall back to reasonable default
   :long))

(defun resolve-typedef (name)
  "Resolve a typedef name to its primitive type"
  (map:get *typedef-table* name))

(defun register-typedef (name primitive-type)
  "Register a new typedef mapping"
  (setf *typedef-table* (map:assoc *typedef-table* name primitive-type)))

(defun discover-typedef-size (typedef-name)
  "Discover the size of a typedef at runtime using C compiler"
  (handler-case
      (let* ((c-code (format nil "#include <sys/types.h>~%#include <stddef.h>~%#include <time.h>~%#include <stdio.h>~%int main() { printf(\"%%lu\", sizeof(~A)); return 0; }" typedef-name))
             (temp-file (format nil "/tmp/typedef_test_~A.c" (random 10000)))
             (exe-file (format nil "/tmp/typedef_test_~A" (random 10000))))
        (unwind-protect
             (progn
               ;; Write C code to file
               (with-open-file (out temp-file :direction :output :if-exists :supersede)
                 (write-string c-code out))
               ;; Compile it
               (let ((compile-result (sb-ext:run-program "cc" (list "-o" exe-file temp-file)
                                                         :search t :wait t)))
                 (when (zerop (sb-ext:process-exit-code compile-result))
                   ;; Run it and capture output
                   (let ((run-result (with-output-to-string (s)
                                      (sb-ext:run-program exe-file nil
                                                         :search nil :wait t :output s))))
                     (parse-integer run-result :junk-allowed t)))))
          ;; Clean up temp files
          (ignore-errors (delete-file temp-file))
          (ignore-errors (delete-file exe-file))))
    (error () nil)))  ; Return nil if we can't determine the size

(defun typedef-to-primitive-type (size-in-bytes signed-p)
  "Map a size and signedness to a primitive type"
  (cond
    ((= size-in-bytes 1)
     (if signed-p :char :unsigned-char))
    ((= size-in-bytes 2)
     (if signed-p :short :unsigned-short))
    ((= size-in-bytes 4)
     (if signed-p :int :unsigned-int))
    ((= size-in-bytes 8)
     (if signed-p :long :unsigned-long))
    (t :pointer)))  ; Unknown size, assume pointer

(defun discover-system-typedefs ()
  "Discover system typedefs by compiling small C programs"
  ;; This would ideally compile and run C code to discover actual sizes
  ;; For now, we use reasonable defaults based on pointer size
  (let* ((pointer-size (sb-alien:alien-size sb-alien:system-area-pointer))
         (is-64-bit (= pointer-size 64))  ; alien-size returns bits
         (platform-typedefs
          (cond
            ;; 64-bit Linux/Unix
            ((and is-64-bit (member :unix *features*))
             '(("size_t" . :unsigned-long)
               ("ssize_t" . :long)
               ("time_t" . :long)
               ("off_t" . :long)
               ("ptrdiff_t" . :long)))
            ;; 32-bit systems
            ((not is-64-bit)
             '(("size_t" . :unsigned-int)
               ("ssize_t" . :int)
               ("time_t" . :long)
               ("off_t" . :long)
               ("ptrdiff_t" . :int)))
            ;; Default fallback
            (t '()))))
    (loop for (name . type) in platform-typedefs
          do (register-typedef name type))))

(defun load-system-headers ()
  "Load typedef definitions from common system headers"
  ;; Try to parse common system headers if available
  (dolist (header '("/usr/include/sys/types.h"
                    "/usr/include/stddef.h"
                    "/usr/include/stdint.h"))
    (when (probe-file header)
      (handler-case
          (with-open-file (in header :direction :input)
            (let ((content (make-string (file-length in))))
              (read-sequence content in)
              ;; Parse typedefs from the header
              (dolist (typedef (parse-typedefs-from-header content))
                (when typedef
                  (register-typedef (car typedef) (cdr typedef))))))
        (error () nil)))))  ; Ignore errors reading system headers

;;; Struct layout representation

(defstruct struct-layout
  "Represents the memory layout of a C struct"
  name
  size
  alignment
  fields        ; map of field-name -> field-info
  field-order)  ; list of field names in order

(defstruct field-info
  "Information about a struct field"
  name
  type
  offset
  size
  alignment
  bit-field-p
  bit-offset
  bit-width)

(defvar *struct-layouts* map:+empty+
  "Registry of struct layouts")

;;; Basic type sizes and alignments (64-bit)

(defparameter *type-info*
  '((:char 1 1)
    (:signed-char 1 1)
    (:unsigned-char 1 1)
    (:short 2 2)
    (:unsigned-short 2 2)
    (:int 4 4)
    (:unsigned-int 4 4)
    (:long 8 8)
    (:unsigned-long 8 8)
    (:long-long 8 8)
    (:unsigned-long-long 8 8)
    (:float 4 4)
    (:double 8 8)
    (:long-double 16 16)
    (:pointer 8 8)
    (:size-t 8 8)
    (:ssize-t 8 8)
    (:time-t 8 8)
    (:clock-t 8 8)
    (:pid-t 4 4)
    (:uid-t 4 4)
    (:gid-t 4 4)
    (:mode-t 4 4)
    (:off-t 8 8)
    (:dev-t 8 8)
    (:ino-t 8 8))
  "Size and alignment for basic C types")

(defun get-type-info (type)
  "Get size and alignment for a type"
  (cond
    ;; Basic types
    ((assoc type *type-info*)
     (destructuring-bind (size align) (cdr (assoc type *type-info*))
       (values size align)))
    ;; Arrays
    ((and (consp type) (eq (first type) :array))
     (destructuring-bind (element-type count) (cdr type)
       (multiple-value-bind (elem-size elem-align) (get-type-info element-type)
         (values (* elem-size count) elem-align))))
    ;; Structs
    ((and (consp type) (eq (first type) :struct))
     (let ((layout (get-struct-layout (second type))))
       (values (struct-layout-size layout)
               (struct-layout-alignment layout))))
    ;; Pointers
    ((and (consp type) (eq (first type) :pointer))
     (values 8 8))
    ;; Bit fields
    ((and (consp type) (eq (first type) :bit))
     (values 0 1)) ; Bit fields don't consume separate space
    ;; Default
    (t (values 8 8))))

;;; Struct definition

(defun calculate-struct-layout (fields)
  "Calculate offsets and total size for struct fields"
  (let ((offset 0)
        (max-align 1)
        (field-infos '())
        (bit-offset 0)
        (bit-container-offset nil))
    (dolist (field-spec fields)
      (destructuring-bind (name type &rest options) field-spec
        (declare (ignore options))
        (cond
          ;; Bit field
          ((and (consp type) (eq (first type) :bit))
           (let ((width (second type)))
             ;; Start new container if needed
             (when (or (null bit-container-offset)
                       (> (+ bit-offset width) 32))
               (setf offset (align-offset offset 4))
               (setf bit-container-offset offset)
               (setf bit-offset 0)
               (incf offset 4))
             (push (make-field-info :name name
                                   :type :unsigned-int
                                   :offset bit-container-offset
                                   :size 4
                                   :alignment 4
                                   :bit-field-p t
                                   :bit-offset bit-offset
                                   :bit-width width)
                   field-infos)
             (incf bit-offset width)))
          ;; Regular field
          (t
           (setf bit-container-offset nil) ; End bit field sequence
           (multiple-value-bind (size align) (get-type-info type)
             (setf offset (align-offset offset align))
             (push (make-field-info :name name
                                   :type type
                                   :offset offset
                                   :size size
                                   :alignment align
                                   :bit-field-p nil)
                   field-infos)
             (incf offset size)
             (setf max-align (max max-align align)))))))
    ;; Align total size to struct alignment
    (setf offset (align-offset offset max-align))
    (values (nreverse field-infos) offset max-align)))

(defun align-offset (offset alignment)
  "Align offset to the given alignment"
  (let ((remainder (mod offset alignment)))
    (if (zerop remainder)
        offset
        (+ offset (- alignment remainder)))))

(defun define-c-struct (name fields)
  "Define a C struct with the given fields"
  (multiple-value-bind (field-infos size alignment)
      (calculate-struct-layout fields)
    (let ((field-map map:+empty+))
      (dolist (info field-infos)
        (setf field-map (map:assoc field-map (field-info-name info) info)))
      (let ((layout (make-struct-layout :name name
                                        :size size
                                        :alignment alignment
                                        :fields field-map
                                        :field-order (mapcar #'field-info-name field-infos))))
        (setf *struct-layouts* (map:assoc *struct-layouts* name layout))
        ;; Register with trampoline system
        (trampoline:register-c-type name
                                    :base :struct
                                    :size size
                                    :alignment alignment)
        layout))))

(defun define-c-union (name fields)
  "Define a C union with the given fields"
  (let ((max-size 0)
        (max-align 1)
        (field-infos '()))
    (dolist (field-spec fields)
      (destructuring-bind (name type &rest options) field-spec
        (declare (ignore options))
        (multiple-value-bind (size align) (get-type-info type)
          (push (make-field-info :name name
                                :type type
                                :offset 0  ; All union fields at offset 0
                                :size size
                                :alignment align
                                :bit-field-p nil)
                field-infos)
          (setf max-size (max max-size size))
          (setf max-align (max max-align align)))))
    ;; Align total size
    (setf max-size (align-offset max-size max-align))
    (let ((field-map map:+empty+))
      (dolist (info (nreverse field-infos))
        (setf field-map (map:assoc field-map (field-info-name info) info)))
      (let ((layout (make-struct-layout :name name
                                        :size max-size
                                        :alignment max-align
                                        :fields field-map
                                        :field-order (mapcar #'field-info-name
                                                           (nreverse field-infos)))))
        (setf *struct-layouts* (map:assoc *struct-layouts* name layout))
        layout))))

(defun get-struct-layout (name)
  "Get the layout for a struct type"
  (map:get *struct-layouts* name))

(defun struct-field-offset (layout field-name)
  "Get the offset of a field in a struct"
  (let ((info (map:get (struct-layout-fields layout) field-name)))
    (when info
      (field-info-offset info))))

(defun struct-field-type (layout field-name)
  "Get the type of a field in a struct"
  (let ((info (map:get (struct-layout-fields layout) field-name)))
    (when info
      (field-info-type info))))

(defun struct-field-size (layout field-name)
  "Get the size of a field in a struct"
  (let ((info (map:get (struct-layout-fields layout) field-name)))
    (when info
      (field-info-size info))))

(defun struct-has-field-p (layout field-name)
  "Check if a struct has a given field"
  (map:get (struct-layout-fields layout) field-name))

;;; Struct instances

(defstruct c-struct-instance
  "Runtime representation of a C struct"
  layout
  pointer
  owned-p)  ; Whether we own the memory

(defmacro with-c-struct ((var type) &body body)
  "Allocate a C struct and bind it to var"
  `(let* ((layout (get-struct-layout ',type))
          (size (struct-layout-size layout))
          (ptr (foreign-alloc size))
          (,var (make-c-struct-instance :layout layout
                                        :pointer ptr
                                        :owned-p t)))
     (unwind-protect
          (progn
            ;; Zero-initialize
            (dotimes (i size)
              (setf (sb-sys:sap-ref-8 ptr i) 0))
            ,@body)
       (when (c-struct-instance-owned-p ,var)
         (foreign-free ptr)))))

(defmacro with-c-union ((var type) &body body)
  "Allocate a C union and bind it to var"
  `(with-c-struct (,var ,type) ,@body))

(defmacro with-struct-view ((var pointer type) &body body)
  "Create a zero-copy view of a struct from foreign memory"
  `(let* ((layout (get-struct-layout ',type))
          (,var (make-c-struct-instance :layout layout
                                        :pointer ,pointer
                                        :owned-p nil)))
     ,@body))

(defmacro with-foreign-object ((var type) &body body)
  "Allocate foreign memory for a type"
  (let ((size-var (gensym "SIZE")))
    `(let* ((,size-var ,(if (eq type :time-t) 8
                            (if (and (consp type) (eq (first type) :pointer))
                                8
                                `(get-type-size ',type))))
            (,var (foreign-alloc ,size-var)))
       (unwind-protect
            (progn ,@body)
         (foreign-free ,var)))))

(defun get-type-size (type)
  "Get the size of a type"
  (multiple-value-bind (size align) (get-type-info type)
    (declare (ignore align))
    size))

;;; Field access

(defun struct-ref (struct field-spec)
  "Get a field value from a struct"
  (let ((layout (c-struct-instance-layout struct))
        (ptr (c-struct-instance-pointer struct)))
    (if (consp field-spec)
        ;; Nested field access
        (destructuring-bind (field &rest subfields) field-spec
          (let ((info (map:get (struct-layout-fields layout) field)))
            (when info
              (let ((field-type (field-info-type info)))
                (if (and (consp field-type) (eq (first field-type) :struct))
                    ;; Recurse into nested struct
                    (let ((nested-layout (get-struct-layout (second field-type)))
                          (nested-ptr (sb-sys:sap+ ptr (field-info-offset info))))
                      (struct-ref (make-c-struct-instance :layout nested-layout
                                                          :pointer nested-ptr
                                                          :owned-p nil)
                                 subfields))
                    ;; Array access
                    (when (and (consp field-type) (eq (first field-type) :array))
                      (let ((index (first subfields))
                            (elem-type (second field-type)))
                        (read-value-at ptr 
                                      (+ (field-info-offset info)
                                         (* index (first (get-type-info elem-type))))
                                      elem-type))))))))
        ;; Simple field access
        (let ((info (map:get (struct-layout-fields layout) field-spec)))
          (when info
            (if (field-info-bit-field-p info)
                ;; Extract bit field
                (let ((container (sb-sys:sap-ref-32 ptr (field-info-offset info)))
                      (mask (1- (ash 1 (field-info-bit-width info)))))
                  (logand (ash container (- (field-info-bit-offset info))) mask))
                ;; Regular field
                (read-value-at ptr (field-info-offset info) (field-info-type info))))))))

(defun (setf struct-ref) (value struct field-spec)
  "Set a field value in a struct"
  (let ((layout (c-struct-instance-layout struct))
        (ptr (c-struct-instance-pointer struct)))
    (if (consp field-spec)
        ;; Nested or array field
        (destructuring-bind (field &rest subfields) field-spec
          (let ((info (map:get (struct-layout-fields layout) field)))
            (when info
              (let ((field-type (field-info-type info)))
                (cond
                  ;; Nested struct
                  ((and (consp field-type) (eq (first field-type) :struct))
                   (let ((nested-layout (get-struct-layout (second field-type)))
                         (nested-ptr (sb-sys:sap+ ptr (field-info-offset info))))
                     (setf (struct-ref (make-c-struct-instance :layout nested-layout
                                                               :pointer nested-ptr
                                                               :owned-p nil)
                                      subfields)
                           value)))
                  ;; Array element
                  ((and (consp field-type) (eq (first field-type) :array))
                   (let ((index (first subfields))
                         (elem-type (second field-type)))
                     (write-value-at ptr
                                    (+ (field-info-offset info)
                                       (* index (first (get-type-info elem-type))))
                                    elem-type
                                    value))))))))
        ;; Simple field
        (let ((info (map:get (struct-layout-fields layout) field-spec)))
          (when info
            (if (field-info-bit-field-p info)
                ;; Set bit field
                (let* ((offset (field-info-offset info))
                       (bit-offset (field-info-bit-offset info))
                       (bit-width (field-info-bit-width info))
                       (mask (1- (ash 1 bit-width)))
                       (container (sb-sys:sap-ref-32 ptr offset)))
                  (setf container (logior (logand container
                                                  (lognot (ash mask bit-offset)))
                                         (ash (logand value mask) bit-offset)))
                  (setf (sb-sys:sap-ref-32 ptr offset) container))
                ;; Regular field
                (write-value-at ptr (field-info-offset info) 
                               (field-info-type info) value)))))
    value))

(defun union-ref (union field)
  "Get a field value from a union"
  (struct-ref union field))

(defun (setf union-ref) (value union field)
  "Set a field value in a union"
  (setf (struct-ref union field) value))

(defun struct-ref-ptr (pointer type field)
  "Access a struct field through a pointer"
  (let ((layout (get-struct-layout type)))
    (struct-ref (make-c-struct-instance :layout layout
                                        :pointer pointer
                                        :owned-p nil)
                field)))

(defun read-value-at (ptr offset type)
  "Read a value of the given type from memory"
  (case type
    (:char (sb-sys:signed-sap-ref-8 ptr offset))
    (:unsigned-char (sb-sys:sap-ref-8 ptr offset))
    (:short (sb-sys:signed-sap-ref-16 ptr offset))
    (:unsigned-short (sb-sys:sap-ref-16 ptr offset))
    (:int (sb-sys:signed-sap-ref-32 ptr offset))
    (:unsigned-int (sb-sys:sap-ref-32 ptr offset))
    ((:long :time-t :ssize-t :off-t) (sb-sys:signed-sap-ref-64 ptr offset))
    ((:unsigned-long :size-t :dev-t :ino-t) (sb-sys:sap-ref-64 ptr offset))
    (:float (sb-sys:sap-ref-single ptr offset))
    (:double (sb-sys:sap-ref-double ptr offset))
    (:pointer (sb-sys:sap-ref-sap ptr offset))
    ((:pid-t :uid-t :gid-t :mode-t) (sb-sys:signed-sap-ref-32 ptr offset))
    (t (if (and (consp type) (eq (first type) :pointer))
           (sb-sys:sap-ref-sap ptr offset)
           0))))

(defun write-value-at (ptr offset type value)
  "Write a value of the given type to memory"
  (case type
    (:char (setf (sb-sys:signed-sap-ref-8 ptr offset) value))
    (:unsigned-char (setf (sb-sys:sap-ref-8 ptr offset) value))
    (:short (setf (sb-sys:signed-sap-ref-16 ptr offset) value))
    (:unsigned-short (setf (sb-sys:sap-ref-16 ptr offset) value))
    (:int (setf (sb-sys:signed-sap-ref-32 ptr offset) value))
    (:unsigned-int (setf (sb-sys:sap-ref-32 ptr offset) value))
    ((:long :time-t :ssize-t :off-t) (setf (sb-sys:signed-sap-ref-64 ptr offset) value))
    ((:unsigned-long :size-t) (setf (sb-sys:sap-ref-64 ptr offset) value))
    (:float (setf (sb-sys:sap-ref-single ptr offset) (coerce value 'single-float)))
    (:double (setf (sb-sys:sap-ref-double ptr offset) (coerce value 'double-float)))
    (:pointer (setf (sb-sys:sap-ref-sap ptr offset) value))
    ((:pid-t :uid-t :gid-t :mode-t) (setf (sb-sys:signed-sap-ref-32 ptr offset) value))
    (t (when (and (consp type) (eq (first type) :pointer))
         (setf (sb-sys:sap-ref-sap ptr offset) value)))))

(defun struct-pointer (struct)
  "Get the pointer to a struct's memory"
  (c-struct-instance-pointer struct))

;;; Memory management

(defun foreign-alloc (size)
  "Allocate foreign memory"
  (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) size)))

(defun foreign-free (ptr)
  "Free foreign memory"
  (sb-alien:free-alien (sb-alien:sap-alien ptr (* (sb-alien:unsigned 8)))))

;;; Serialization

(defun struct-to-bytes (struct)
  "Serialize a struct to a byte vector"
  (let* ((layout (c-struct-instance-layout struct))
         (size (struct-layout-size layout))
         (ptr (c-struct-instance-pointer struct))
         (bytes (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref bytes i) (sb-sys:sap-ref-8 ptr i)))
    bytes))

(defun bytes-to-struct (bytes struct)
  "Deserialize bytes into a struct"
  (let* ((layout (c-struct-instance-layout struct))
         (size (min (length bytes) (struct-layout-size layout)))
         (ptr (c-struct-instance-pointer struct)))
    (dotimes (i size)
      (setf (sb-sys:sap-ref-8 ptr i) (aref bytes i)))
    struct))

(defun struct-to-string (struct)
  "Pretty-print a struct"
  (let* ((layout (c-struct-instance-layout struct))
         (name (struct-layout-name layout)))
    (with-output-to-string (s)
      (format s "#<~A" name)
      (dolist (field-name (struct-layout-field-order layout))
        (let ((value (struct-ref struct field-name)))
          (format s " ~A=~A" field-name value)))
      (format s ">"))))

;;; C header parsing

(defun parse-typedefs-from-header (header-text)
  "Extract typedef declarations from C header text using simple parsing"
  ;; Simple line-based parsing for typedef declarations
  (let ((lines (split-string header-text #\Newline))
        (typedefs '()))
    (dolist (line lines)
      (when (and (search "typedef" line)
                 (not (search "//" line))  ; Skip comments
                 (not (search "/*" line)))
        (let ((typedef (parse-typedef-line line)))
          (when typedef
            (push typedef typedefs)))))
    (nreverse typedefs)))

(defun split-string (string delimiter)
  "Split a string by delimiter character"
  (let ((result '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos
          do (push (subseq string start pos) result)
             (setf start (1+ pos))
          finally (push (subseq string start) result))
    (nreverse result)))

(defun parse-typedef-line (line)
  "Parse a single typedef line"
  ;; Simple parsing: typedef <type-spec> <name>;
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (when (and (> (length trimmed) 7)
               (string= "typedef" (subseq trimmed 0 7)))
      (let* ((rest (string-trim '(#\Space #\Tab) (subseq trimmed 7)))
             (semicolon-pos (position #\; rest)))
        (when semicolon-pos
          (let* ((type-and-name (subseq rest 0 semicolon-pos))
                 (words (remove-if (lambda (s) (= (length s) 0))
                                  (split-string type-and-name #\Space))))
            (when (>= (length words) 2)
              ;; Last word is the new typedef name, rest is the type
              (let ((new-name (car (last words)))
                    (type-words (butlast words)))
                (cons new-name (parse-field-type type-words))))))))))

(defun parse-c-type-spec (type-spec)
  "Parse a C type specification into a primitive type"
  (let ((words (remove-if (lambda (s) (= (length s) 0))
                          (split-string type-spec #\Space))))
    (parse-field-type words)))

(defun parse-c-struct (name header-text)
  "Parse a C struct definition from header text"
  ;; First, extract any typedefs from the header
  (dolist (typedef (parse-typedefs-from-header header-text))
    (when typedef
      (register-typedef (car typedef) (cdr typedef))))
  ;; For now, use simple parsing until clang parser is fully integrated
  ;; TODO: Fix clang parser integration with epsilon.parser combinators
  (parse-struct-from-text name header-text))

(defun convert-parsed-fields (parsed-fields)
  "Convert parsed field specs to our format"
  (mapcar (lambda (field)
            (list (first field) ; field name
                  (second field))) ; field type
          parsed-fields))

(defun extract-struct-fields (ast)
  "Extract fields from parsed struct AST"
  (let ((fields (getf ast :fields)))
    (when fields
      (loop for field in fields
            when (and (listp field) (eq (first field) :field-declaration))
            append (process-field-declaration field)))))

(defun process-field-declaration (field-decl)
  "Process a field declaration into our format"
  (let ((specifiers (getf (rest field-decl) :specifiers))
        (declarators (getf (rest field-decl) :declarators)))
    (when (and specifiers declarators)
      (let ((type (parse-field-type specifiers)))
        (mapcar (lambda (declarator)
                  (list (if (stringp declarator)
                           (intern (string-upcase declarator) :keyword)
                           :unknown)
                        type))
                declarators)))))

(defun parse-field-type (specifiers)
  "Parse field type from specifiers - resolve to primitive machine types"
  (cond
    ;; Handle unsigned types
    ((member "unsigned" specifiers :test #'string=)
     (cond
       ((and (member "long" specifiers :test #'string=)
             (> (count "long" specifiers :test #'string=) 1)) :unsigned-long-long)
       ((member "long" specifiers :test #'string=) :unsigned-long)
       ((member "int" specifiers :test #'string=) :unsigned-int)
       ((member "short" specifiers :test #'string=) :unsigned-short)
       ((member "char" specifiers :test #'string=) :unsigned-char)
       (t :unsigned-int)))  ; unsigned alone means unsigned int
    ;; Handle long long
    ((and (member "long" specifiers :test #'string=)
          (> (count "long" specifiers :test #'string=) 1)) :long-long)
    ;; Handle basic types
    ((member "long" specifiers :test #'string=) :long)
    ((member "int" specifiers :test #'string=) :int)
    ((member "char" specifiers :test #'string=) :char)
    ((member "short" specifiers :test #'string=) :short)
    ((member "float" specifiers :test #'string=) :float)
    ((member "double" specifiers :test #'string=) :double)
    ((member "void" specifiers :test #'string=) :void)
    ;; Handle pointers
    ((some (lambda (s) (and (stringp s) (search "*" s))) specifiers) :pointer)
    ;; Check if it's a single identifier that might be a typedef
    ((and (= (length specifiers) 1)
          (stringp (first specifiers)))
     ;; Try to discover the typedef's actual type
     (discover-typedef (first specifiers)))
    ;; Default case
    (t :int)))  ; Default to int

(defun parse-struct-from-text (name text)
  "Simple fallback struct parser"
  (declare (ignore text))
  ;; For now, just create a dummy struct
  (define-c-struct name '((dummy :int))))

(defun define-c-struct-auto (name header-text)
  "Define a struct by parsing C header text"
  (parse-c-struct name header-text))

;;; Integration with defshared

(defmacro defshared (name c-name library return-type &rest args)
  "Define a foreign function (temporary compatibility)"
  (declare (ignore c-name library return-type args))
  `(defun ,name (&rest args)
     (declare (ignore args))
     0))