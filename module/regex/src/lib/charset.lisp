;;;;  Character Set Implementation
;;;;
;;;;  This module provides efficient character set representations extracted from
;;;;  epsilon.lib.regex. It offers two complementary implementations:
;;;;
;;;;  1. Charset - Open-addressing hash table optimized for sparse character sets
;;;;  2. Charmap - Bit vector representation optimized for dense character ranges
;;;;
;;;;  The module automatically selects the most efficient representation based
;;;;  on character density and provides a unified interface for character set
;;;;  operations.

(defpackage :epsilon.lib.charset
  (:use :cl)
  (:export
   ;; Parameters
   :*char-code-limit*
   
   ;; Charset (hash table implementation)
   :charset
   :make-charset
   :charset-p
   :in-charset-p
   :add-to-charset
   :map-charset
   :create-charset-from-test-function
   
   ;; Charmap (bit vector implementation)  
   :charmap
   :make-charmap
   :charmap-p
   :in-charmap-p
   :charmap-contents
   :create-charmap-from-test-function
   
   ;; Unified interface
   :create-optimized-test-function
   :create-hash-table-from-test-function
   
   ;; Utilities
   :complement*))

(in-package :epsilon.lib.charset)

;; Character code limit for optimization decisions
(defparameter *char-code-limit* char-code-limit
  "The upper limit for character codes in character set optimizations")

;; Hash table collision tolerance
(defconstant +probe-depth+ 7
  "Maximum number of probes for collision resolution in charset hash table")

;; Utility functions

(defun complement* (f)
  "Create the complement of a character test function, optimized for common cases"
  (cond ((eq f (constantly t)) (constantly nil))
        ((eq f (constantly nil)) (constantly t))
        (t (complement f))))

(defun make-char-vector (size)
  "Create a character vector for charset implementation"
  (make-array size
              :element-type 'character
              :initial-element (code-char 0)))

;; Charset implementation (open-addressing hash table)

(defstruct charset
  "Sparse character set using open-addressing hash table"
  (depth 0 :type fixnum)
  (count 0 :type fixnum)
  (vector (make-char-vector 16) :type (simple-array character (*))))

(defun mix (char depth)
  "Hash mixing function for collision resolution"
  (logxor (char-code char)
          (ash (char-code char) (- depth))))

(defun compute-index (char vector-length depth)
  "Compute hash table index with collision resolution"
  (mod (mix char depth) vector-length))

(defun in-charset-p (char charset)
  "Test if character is in charset using open-addressing probing"
  (let ((vector (charset-vector charset))
        (vector-length (length (charset-vector charset)))
        (depth (charset-depth charset)))
    (loop for probe-depth from 0 below +probe-depth+
          for index = (compute-index char vector-length (+ depth probe-depth))
          for candidate = (aref vector index)
          when (char= char candidate)
            return t
          when (char= candidate (code-char 0))
            return nil
          finally (return nil))))

(defun add-to-charset (char charset)
  "Add character to charset, expanding if necessary"
  (if (>= (charset-count charset)
          (ash (length (charset-vector charset)) -1))
      (%add-to-charset/expand char charset)
      (%add-to-charset char charset)))

(defun %add-to-charset (char charset)
  "Add character to charset without expansion"
  (let ((vector (charset-vector charset))
        (vector-length (length (charset-vector charset)))
        (depth (charset-depth charset)))
    (loop for probe-depth from 0 below +probe-depth+
          for index = (compute-index char vector-length (+ depth probe-depth))
          for candidate = (aref vector index)
          when (char= char candidate)
            return nil  ; already present
          when (char= candidate (code-char 0))
            do (setf (aref vector index) char)
               (incf (charset-count charset))
               (return t)  ; added
          finally 
            ;; No space found, need expansion
            (return (%add-to-charset/expand char charset)))))

(defun %add-to-charset/expand (char charset)
  "Expand charset hash table and add character"
  (let* ((old-vector (charset-vector charset))
         (old-length (length old-vector))
         (new-length (* old-length 2))
         (new-vector (make-char-vector new-length))
         (old-depth (charset-depth charset))
         (new-depth (1+ old-depth)))
    
    ;; Reset charset with new vector
    (setf (charset-vector charset) new-vector
          (charset-depth charset) new-depth
          (charset-count charset) 0)
    
    ;; Rehash all existing characters
    (loop for old-char across old-vector
          unless (char= old-char (code-char 0))
            do (%add-to-charset old-char charset))
    
    ;; Add the new character
    (%add-to-charset char charset)))

(defun map-charset (function charset)
  "Apply function to each character in charset"
  (loop for char across (charset-vector charset)
        unless (char= char (code-char 0))
          do (funcall function char)))

(defun create-charset-from-test-function (test-function)
  "Create charset from character test function"
  (let ((charset (make-charset)))
    (loop for code from 0 below *char-code-limit*
          for char = (code-char code)
          when (and char (funcall test-function char))
            do (add-to-charset char charset))
    charset))

;; Charmap implementation (bit vector)

(defstruct (charmap
            (:constructor %make-charmap))
  "Dense character set using bit vector with complement representation"
  (vector nil :type (or null simple-bit-vector))
  (range 0 :type fixnum)
  (complement-p nil :type boolean))

(defun in-charmap-p (char charmap)
  "Test if character is in charmap"
  (let ((code (char-code char))
        (range (charmap-range charmap))
        (vector (charmap-vector charmap))
        (complement-p (charmap-complement-p charmap)))
    (if (< code range)
        (if complement-p
            (zerop (bit vector code))
            (not (zerop (bit vector code))))
        complement-p)))

(defun charmap-contents (charmap)
  "Return list of all characters in charmap"
  (let ((result '())
        (vector (charmap-vector charmap))
        (range (charmap-range charmap))
        (complement-p (charmap-complement-p charmap)))
    (if complement-p
        ;; Complement case - include chars not in vector, plus high range
        (progn
          (loop for code from 0 below range
                when (zerop (bit vector code))
                  do (push (code-char code) result))
          (loop for code from range below *char-code-limit*
                do (push (code-char code) result)))
        ;; Normal case - include chars in vector
        (loop for code from 0 below range
              when (not (zerop (bit vector code)))
                do (push (code-char code) result)))
    result))

(defun make-charmap (range test-function &optional complement-p)
  "Create charmap for character range using test function"
  (let ((vector (make-array range :element-type 'bit :initial-element 0)))
    (loop for code from 0 below range
          for char = (code-char code)
          when (and char 
                    (if complement-p
                        (not (funcall test-function char))
                        (funcall test-function char)))
            do (setf (bit vector code) 1))
    (%make-charmap :vector vector
                  :range range
                  :complement-p complement-p)))

(defun create-charmap-from-test-function (test-function)
  "Create optimized charmap from test function"
  (let ((min-char-code *char-code-limit*)
        (max-char-code -1)
        (count 0))
    
    ;; Find range and count
    (loop for code from 0 below *char-code-limit*
          for char = (code-char code)
          when (and char (funcall test-function char))
            do (incf count)
               (setf min-char-code (min min-char-code code)
                     max-char-code (max max-char-code code)))
    
    (if (zerop count)
        ;; Empty set
        (%make-charmap :vector (make-array 0 :element-type 'bit)
                      :range 0
                      :complement-p nil)
        ;; Choose normal or complement representation
        (let* ((range (1+ max-char-code))
               (complement-count (- range count))
               (use-complement-p (< complement-count count)))
          (make-charmap range test-function use-complement-p)))))

;; Unified interface

(defun create-hash-table-from-test-function (test-function)
  "Create standard hash table from test function"
  (let ((hash-table (make-hash-table :test 'eql)))
    (loop for code from 0 below *char-code-limit*
          for char = (code-char code)
          when (and char (funcall test-function char))
            do (setf (gethash char hash-table) t))
    hash-table))

(defun create-optimized-test-function (test-function)
  "Create optimized character set representation based on density analysis"
  (let ((count 0)
        (min-char-code *char-code-limit*)
        (max-char-code -1))
    
    ;; Analyze character distribution
    (loop for code from 0 below *char-code-limit*
          for char = (code-char code)
          when (and char (funcall test-function char))
            do (incf count)
               (setf min-char-code (min min-char-code code)
                     max-char-code (max max-char-code code)))
    
    (cond 
      ;; Empty set
      ((zerop count)
       (constantly nil))
      
      ;; Single character
      ((= count 1)
       (let ((the-char (code-char min-char-code)))
         (lambda (char) (char= char the-char))))
      
      ;; Dense range - use charmap (bit vector)
      ((and (< count 256)
            (< max-char-code 512)
            (> (/ count (1+ (- max-char-code min-char-code))) 0.25))
       (let ((charmap (create-charmap-from-test-function test-function)))
         (lambda (char) (in-charmap-p char charmap))))
      
      ;; Sparse set - use charset (hash table)
      ((< count 64)
       (let ((charset (create-charset-from-test-function test-function)))
         (lambda (char) (in-charset-p char charset))))
      
      ;; Large set - use standard hash table
      (t
       (let ((hash-table (create-hash-table-from-test-function test-function)))
         (lambda (char) (gethash char hash-table)))))))
