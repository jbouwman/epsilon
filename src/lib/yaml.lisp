;;;; A YAML parser for Common Lisp.
;;;; 
;;;; This package provides functionality to parse YAML documents into
;;;; Lisp data structures.  The parser supports a subset of YAML 1.2
;;;; specification including:
;;;; 
;;;; Supported Features:
;;;;   * Basic scalar values
;;;;   * Sequences (lists) with '-' marker
;;;;   * Mappings (key-value pairs)
;;;;   * Nested structures (sequences and mappings)
;;;;   * Multi-line scalar values
;;;;   * Proper indentation-based structure parsing
;;;; 
;;;; Limitations:
;;;;   * Does not support YAML directives or tags
;;;;   * No support for anchors and aliases
;;;;   * No support for complex scalar types (timestamps, binary, etc.)
;;;;   * No support for flow style (inline arrays/objects)
;;;;   * No support for document separators
;;;; 
;;;; Data Representation:
;;;;   * Sequences are converted to Lisp lists
;;;;   * Mappings are converted to alists (key . value)
;;;;   * Scalar values are represented as strings
;;;; 
;;;; Usage:
;;;;   (yaml:parse-string \"key: value\")  ; Parse YAML from string
;;;;   (yaml:parse-file #P\"config.yml\")  ; Parse YAML from file
;;;; 
;;;; Example:
;;;;   Input YAML:
;;;;     name: Irv
;;;;     details:
;;;;       - age: 30
;;;;       - city: Peoria
;;;;   
;;;;   Will produce:
;;;;   ((\"name\" . \"Irv\")
;;;;    (\"details\" . ((\"age\" . \"30\")
;;;;                    (\"city\" . \"Peoria\"))))
;;;; 
;;;; Notes:
;;;;   * Indentation must be consistent within the document
;;;;   * Keys in mappings must be scalar values
;;;;   * The parser is designed for clarity and correctness over performance
;;;;   * Input is assumed to be in UTF-8 encoding

(defpackage #:epsilon.lib.yaml
  (:use #:cl)
  (:local-nicknames
   (#:stream #:epsilon.lib.stream))
  (:export
   :node-value
   :parse
   :parse-file
   :parse-string))

(in-package #:epsilon.lib.yaml)

;; Input

(defclass line-stream ()
  ((underlying-stream
    :initarg :stream
    :reader line-stream-underlying-stream)
   (peek-buffer
    :initform nil
    :accessor line-stream-peek-buffer)
   (line-number
    :initform 0
    :accessor line-stream-line-number)))

(defun make-line-stream (stream)
  (make-instance 'line-stream :stream stream))

(defun strip-comment (line)
  "Remove comments and their preceding whitespace from a line.
   Returns nil if the line is only a comment.
   For lines with content, strips trailing whitespace before comments.
   For indent-only lines with comments, preserves the indentation."
  (when line
    (let ((comment-pos (position #\# line)))
      (if comment-pos
          ;; If there's a comment, check if there's content before it
          (let ((content (string-trim '(#\Space #\Tab) 
                                    (subseq line 0 comment-pos))))
            (if (string= content "")
                nil  ; Line is only whitespace + comment
                content))  ; Return content without trailing spaces
          line))))


(defun peek-line (line-stream)
  "Returns the next line without consuming it. Returns nil at end of stream.
   Comments are stripped."
  (with-slots (peek-buffer underlying-stream) line-stream
    (or peek-buffer
        (setf peek-buffer 
              (loop for line = (read-line underlying-stream nil nil)
                    while line
                    for stripped = (strip-comment line)
                    when stripped return stripped
                    finally (return nil))))))

(defun read-line-from (line-stream)
  "Reads and returns the next line. Returns nil at end of stream.
   Comments are stripped."
  (with-slots (peek-buffer underlying-stream) line-stream
    (if peek-buffer
        (prog1 peek-buffer
          (setf peek-buffer nil))
        (loop for line = (read-line underlying-stream nil nil)
              while line
              for stripped = (strip-comment line)
              when stripped return stripped
              finally (return nil)))))

;; Basic data structures

(defstruct node
  (kind nil :type (or (member :scalar :sequence :mapping) null))
  (value nil)
  (indent-level 0 :type integer))

(defstruct parser-state
  (stream nil :type (or null line-stream))
  (current-indent 0 :type integer))

;; Main parsing functions

(defun parse-string (string)
  (with-input-from-string (s string)
    (let ((state (make-parser-state 
                  :stream (make-line-stream s))))
      (parse-document state))))

(defun parse-file (pathname)
  (with-open-file (s pathname)
    (let ((state (make-parser-state 
                  :stream (make-line-stream s))))
      (parse-next-node state))))



(defun parse-document (state)
  (let ((nodes (loop for node = (parse-next-node state)
                     while node
                     collect node)))
    (construct-document nodes)))

(defun combine-top-level-nodes (nodes)
  "Combine multiple top-level nodes into a single mapping."
  (loop for node in nodes
        when (and (node-p node)
                 (eq :mapping (node-kind node)))
        append (node-value node)))


;; Sequence parsing

(defun parse-sequence-items (state first-line base-indent)
  "Parse a sequence of YAML items starting at the given base indentation level.
   Returns a list of parsed items."
  (let ((items nil))
    ;; Process the first item (from the line we already have)
    (push (parse-sequence-item state first-line base-indent) items)
    
    ;; Process subsequent items
    (loop
      (let ((next-line (peek-line (parser-state-stream state))))
        ;; Exit if we're at the end or found a line with less indentation
        (when (or (null next-line)
                  (< (count-indent next-line) base-indent))
          (return))
        
        ;; Read the line and check if it's a new sequence item at our level
        (setf next-line (read-line-from (parser-state-stream state)))
        (let ((indent (count-indent next-line)))
          (cond
            ;; If it's a new item at our level
            ((and (= indent base-indent)
                  (starts-with-sequence-marker? next-line))
             (push (parse-sequence-item state next-line base-indent) items))
            
            ;; If it's indented more, it's part of the previous item
            ((> indent base-indent)
             (let ((last-item (first items)))
               (setf (first items)
                     (append-to-scalar last-item next-line))))
            
            ;; If it's a different structure at the same level, we're done
            (t (return))))))
    
    (nreverse items)))

(defun parse-sequence-item (state line base-indent)
  "Parse a single sequence item, which could be a scalar, nested sequence, or mapping"
  (let* ((content-start (position-if-not #'whitespace-p line))
         (content (subseq line (+ content-start 2)))) ; +2 to skip "- "
    
    (cond
      ;; Empty sequence item
      ((string= "" (string-trim '(#\Space #\Tab) content))
       nil)
      
      ;; Nested sequence
      ((starts-with-sequence-marker? content)
       (parse-sequence-items state content (+ base-indent 2)))
      
      ;; Mapping
      ((starts-with-mapping-marker? content)
       (parse-mapping-pairs state content (+ base-indent 2)))
      
      ;; Simple scalar
      (t (string-trim '(#\Space #\Tab) content)))))


(defun parse-sequence (state line indent)
  (make-node :kind :sequence
             :value (parse-sequence-items state line indent)
             :indent-level indent))


(defun parse-next-node (state)
  (skip-empty-lines state)
  (let* ((line (read-line-from (parser-state-stream state)))
         (indent (when line (count-indent line))))
    (when line
      (setf (parser-state-current-indent state) indent)
      (cond
        ((starts-with-sequence-marker? line)
         (parse-sequence state line indent))
        ((starts-with-mapping-marker? line)
         (parse-mapping state line indent))
        (t (parse-scalar state line indent))))))

(defun skip-empty-lines (state)
  (loop for line = (peek-line (parser-state-stream state))
        while (and line (string= (string-trim '(#\Space #\Tab) line) ""))
        do (read-line-from (parser-state-stream state))))

(defun count-indent (line)
  (or (position-if-not #'whitespace-p line) 0))

(defun whitespace-p (char)
  (member char '(#\Space #\Tab)))

(defun starts-with-sequence-marker? (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (> (length trimmed) 0)
         (char= (char trimmed 0) #\-))))

(defun starts-with-mapping-marker? (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (find #\: trimmed)))

;; Parsing specific types
(defun parse-scalar (state line indent)
  (declare (ignore state))
  (make-node :kind :scalar
             :value (string-trim '(#\Space #\Tab) line)
             :indent-level indent))

(defun parse-mapping (state line indent)
  (make-node :kind :mapping
             :value (parse-mapping-pairs state line indent)
             :indent-level indent))

;; Construction functions
(defun construct-document (nodes)
  (if (= (length nodes) 1)
      (node-value (first nodes))
      nodes))

(defun append-to-scalar (existing-value new-line)
  "Append a new line to an existing scalar value, handling proper spacing"
  (let ((trimmed-line (string-trim '(#\Space #\Tab) new-line)))
    (if (stringp existing-value)
        (format nil "~A~%~A" existing-value trimmed-line)
        existing-value))) ; If not a string, return unchanged

;; TODO represent with lib.map

(defun parse-mapping-pairs (state first-line base-indent)
  "Parse a mapping, returning an alist of key-value pairs."
  (let ((pairs nil))
    ;; Process the first pair from the line we already have
    (multiple-value-bind (key value) (split-mapping-line first-line)
      (push (cons key (parse-mapping-value state value base-indent)) pairs))
    
    ;; Process subsequent pairs
    (loop
      (let ((next-line (peek-line (parser-state-stream state))))
        ;; Exit if we're at the end or found a line with less indentation
        (when (or (null next-line)
                  (< (count-indent next-line) base-indent))
          (return))
        
        (let ((indent (count-indent next-line)))
          (cond
            ;; New mapping pair at same level
            ((and (= indent base-indent)
                  (starts-with-mapping-marker? next-line))
             (setf next-line (read-line-from (parser-state-stream state)))
             (multiple-value-bind (key value) (split-mapping-line next-line)
               (push (cons key (parse-mapping-value state value base-indent))
                     pairs)))
            
            ;; Indented content belongs to previous value
            ((> indent base-indent)
             ;; Don't consume the line here - let parse-mapping-value handle it
             (let* ((prev-pair (first pairs))
                    (new-value (parse-indented-value state (1+ base-indent))))
               (setf (cdr prev-pair) new-value)))
            
            ;; Less indentation or different structure, we're done
            (t (return))))))
    
    (nreverse pairs)))


(defun split-mapping-line (line)
  "Split a mapping line into key and value parts.
   Returns (values key value)"
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (colon-pos (position #\: trimmed)))
    (when colon-pos
      (values (string-trim '(#\Space #\Tab) (subseq trimmed 0 colon-pos))
              (let ((value-part (string-trim '(#\Space #\Tab) 
                                           (subseq trimmed (1+ colon-pos)))))
                (if (string= value-part "") nil value-part))))))


(defun parse-mapping-value (state initial-value base-indent)
  "Parse a mapping value, which could be a scalar, sequence, or nested mapping.
   The initial-value is the part after the colon on the same line as the key."
  (cond
    ;; No value on the same line, check for indented block
    ((null initial-value)
     (let ((next-line (peek-line (parser-state-stream state))))
       (if (and next-line 
                (> (count-indent next-line) base-indent))
           (parse-indented-value state (1+ base-indent))
           nil)))
    
    ;; Value starts with sequence marker
    ((starts-with-sequence-marker? initial-value)
     (parse-sequence-items state initial-value (1+ base-indent)))
    
    ;; Value starts with mapping marker
    ((starts-with-mapping-marker? initial-value)
     (parse-mapping-pairs state initial-value (1+ base-indent)))
    
    ;; Simple scalar value
    (t initial-value)))

(defun parse-indented-value (state indent)
  "Parse an indented value block (sequence, mapping, or multiline scalar)"
  (let ((line (peek-line (parser-state-stream state))))
    (cond
      ((starts-with-sequence-marker? line)
       (setf line (read-line-from (parser-state-stream state)))
       (parse-sequence-items state line indent))
      ((starts-with-mapping-marker? line)
       (setf line (read-line-from (parser-state-stream state)))
       (parse-mapping-pairs state line indent))
      (t (parse-multiline-scalar state 
                                (read-line-from (parser-state-stream state))
                                indent)))))

(defun parse-multiline-scalar (state first-line base-indent)
  "Parse a multiline scalar value"
  (let ((lines (list (string-trim '(#\Space #\Tab) first-line))))
    (loop
      (let ((next-line (peek-line (parser-state-stream state))))
        (when (or (null next-line)
                  (< (count-indent next-line) base-indent))
          (return))
        
        (setf next-line (read-line-from (parser-state-stream state)))
        (push (string-trim '(#\Space #\Tab) next-line) lines)))
    
    (format nil "~{~A~^~%~}" (nreverse lines))))
