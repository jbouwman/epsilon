;;;; This package provides functionality to parse YAML documents into
;;;; Lisp data structures.  The parser supports a subset of YAML 1.2
;;;; specification including:
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

(defpackage :epsilon.yaml
  (:use :cl)
  (:local-nicknames
   (:stream :epsilon.stream)
   (:log :epsilon.log))
  (:export
   :node-value
   :parse
   :parse-file
   :parse-string))

(in-package :epsilon.yaml)

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
  (log:debug "Parsing YAML string: ~S" string)
  (with-input-from-string (s string)
    (let ((state (make-parser-state 
                  :stream (make-line-stream s))))
      (parse-document state))))

;;; TODO can a yaml file contain multiple nodes?

;;; TODO is it wise to wrap an open stream with something that
;;; provides a sequence of tokens, and can be closed?

(defun parse-file (pathname)
  (with-open-file (s pathname)
    (let ((state (make-parser-state 
                  :stream (make-line-stream s))))
      (parse-document state))))


(defun is-document-separator? (line)
  "Check if line is a document separator (--- or ...)"
  (when line
    (let ((trimmed (string-trim '(#\Space #\Tab) line)))
      (or (string= trimmed "---")
          (string= trimmed "...")))))

(defun parse-documents (state)
  "Parse multiple YAML documents separated by ---"
  (let ((documents nil))
    (loop
      (skip-empty-lines state)
      (let ((line (peek-line (parser-state-stream state))))
        ;; Stop if no more content
        (unless line (return))
        
        ;; Skip document start separator
        (when (string= (string-trim '(#\Space #\Tab) line) "---")
          (read-line-from (parser-state-stream state))
          (skip-empty-lines state))
        
        ;; Parse the document content
        (let ((doc (parse-single-document state)))
          (when doc
            (push doc documents)))
        
        ;; Check for document end separator
        (let ((next-line (peek-line (parser-state-stream state))))
          (when (and next-line (string= (string-trim '(#\Space #\Tab) next-line) "..."))
            (read-line-from (parser-state-stream state))))))
    
    ;; Return single document if only one, otherwise list
    (case (length documents)
      (0 nil)
      (1 (first (nreverse documents)))
      (t (nreverse documents)))))

(defun parse-single-document (state)
  "Parse a single YAML document (without separators)"
  (skip-empty-lines state)
  (let ((first-line (peek-line (parser-state-stream state))))
    (log:debug "Parsing document, first line: ~S" first-line)
    (cond
      ;; Stop at document separators
      ((is-document-separator? first-line) nil)
      
      ;; Check for flow style first
      ((and first-line 
            (or (parse-flow-sequence first-line)
                (parse-flow-mapping first-line)))
       (log:debug "Document starts with flow style")
       (setf first-line (read-line-from (parser-state-stream state)))
       (cond 
         ((parse-flow-sequence first-line))
         ((parse-flow-mapping first-line))
         (t nil)))
      ;; If the document starts with a mapping marker, parse as a single mapping
      ((and first-line (starts-with-mapping-marker? first-line))
       (log:debug "Document starts with mapping")
       (setf first-line (read-line-from (parser-state-stream state)))
       (parse-mapping-pairs state first-line 0))
      ;; Otherwise parse as multiple nodes
      (t
       (log:debug "Parsing as multiple nodes")
       (let ((nodes (loop for node = (parse-next-node state)
                          while node
                          collect node)))
         (construct-document nodes))))))

(defun parse-document (state)
  "Parse a complete YAML document (with potential multiple documents)"
  (parse-documents state))

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
  (log:debug "Parsing sequence item: ~S (indent: ~D)" line base-indent)
  (let* ((content-start (position-if-not #'whitespace-p line))
         (content (subseq line (+ content-start 2)))) ; +2 to skip "- "
    (log:debug "Sequence item content: ~S" content)
    
    (cond
      ;; Empty sequence item
      ((string= "" (string-trim '(#\Space #\Tab) content))
       nil)
      
      ;; Flow sequence
      ((parse-flow-sequence content)
       (log:debug "Sequence item is flow sequence: ~S" (parse-flow-sequence content))
       (parse-flow-sequence content))
      
      ;; Flow mapping
      ((parse-flow-mapping content)
       (log:debug "Sequence item is flow mapping: ~S" (parse-flow-mapping content))
       (parse-flow-mapping content))
      
      ;; Nested sequence
      ((starts-with-sequence-marker? content)
       (parse-sequence-items state content (+ base-indent 2)))
      
      ;; Mapping - return as an alist
      ((starts-with-mapping-marker? content)
       (let ((pairs (parse-mapping-pairs state content (+ base-indent 2))))
         ;; If it's a single pair, keep it as an alist for consistency
         pairs))
      
      ;; Simple scalar
      (t (parse-scalar-value (string-trim '(#\Space #\Tab) content))))))


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
         (char= (char trimmed 0) #\-)
         ;; Make sure it's followed by a space or end of line, not a digit
         (or (= (length trimmed) 1)
             (char= (char trimmed 1) #\Space)
             (char= (char trimmed 1) #\Tab)))))

(defun starts-with-mapping-marker? (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (find #\: trimmed)))

(defun parse-flow-sequence (string)
  "Parse a flow sequence [item1, item2, ...]"
  (log:debug "Attempting to parse flow sequence: ~S" string)
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (when (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\[)
               (char= (char trimmed (1- (length trimmed))) #\]))
      (let* ((content (subseq trimmed 1 (1- (length trimmed))))
           (items nil)
           (current-item nil)
           (depth 0)
           (in-quotes nil)
           (escape-next nil))
      (loop for char across content
            do (cond
                 (escape-next
                  (setf current-item (concatenate 'string current-item (string char)))
                  (setf escape-next nil))
                 ((char= char #\\)
                  (setf escape-next t))
                 ((and (char= char #\") (not escape-next))
                  (setf in-quotes (not in-quotes))
                  (setf current-item (concatenate 'string current-item (string char))))
                 (in-quotes
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\[)
                  (incf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\])
                  (decf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\{)
                  (incf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\})
                  (decf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((and (char= char #\,) (zerop depth))
                  (when current-item
                    (push (parse-flow-value (string-trim '(#\Space #\Tab) current-item)) items))
                  (setf current-item nil))
                 (t
                  (setf current-item (concatenate 'string current-item (string char))))))
      (when current-item
        (push (parse-flow-value (string-trim '(#\Space #\Tab) current-item)) items))
      (log:debug "Flow sequence parsed into: ~S" (nreverse items))
      (nreverse items)))))

(defun parse-flow-mapping (string)
  "Parse a flow mapping {key1: value1, key2: value2, ...}"
  (log:debug "Attempting to parse flow mapping: ~S" string)
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (when (and (> (length trimmed) 1)
               (char= (char trimmed 0) #\{)
               (char= (char trimmed (1- (length trimmed))) #\}))
      (let* ((content (subseq trimmed 1 (1- (length trimmed))))
           (pairs nil)
           (current-item nil)
           (depth 0)
           (in-quotes nil)
           (escape-next nil))
      (loop for char across content
            do (cond
                 (escape-next
                  (setf current-item (concatenate 'string current-item (string char)))
                  (setf escape-next nil))
                 ((char= char #\\)
                  (setf escape-next t))
                 ((and (char= char #\") (not escape-next))
                  (setf in-quotes (not in-quotes))
                  (setf current-item (concatenate 'string current-item (string char))))
                 (in-quotes
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\[)
                  (incf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\])
                  (decf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\{)
                  (incf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((char= char #\})
                  (decf depth)
                  (setf current-item (concatenate 'string current-item (string char))))
                 ((and (char= char #\,) (zerop depth))
                  (when current-item
                    (let ((pair (parse-flow-pair (string-trim '(#\Space #\Tab) current-item))))
                      (when pair
                        (push pair pairs))))
                  (setf current-item nil))
                 (t
                  (setf current-item (concatenate 'string current-item (string char))))))
      (when current-item
        (let ((pair (parse-flow-pair (string-trim '(#\Space #\Tab) current-item))))
          (when pair
            (push pair pairs))))
      (log:debug "Flow mapping parsed into: ~S" (nreverse pairs))
      (nreverse pairs)))))

(defun parse-flow-pair (string)
  "Parse a key: value pair from flow style"
  (log:debug "Parsing flow pair: ~S" string)
  (let ((colon-pos (position #\: string)))
    (when colon-pos
      (let ((key (parse-flow-value (string-trim '(#\Space #\Tab) (subseq string 0 colon-pos))))
            (value (parse-flow-value (string-trim '(#\Space #\Tab) (subseq string (1+ colon-pos))))))
        (log:debug "Flow pair parsed: ~S => ~S" key value)
        (cons key value)))))

(defun parse-flow-value (string)
  "Parse a value that might be a flow sequence, flow mapping, or scalar"
  (log:debug "Parsing flow value: ~S" string)
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (cond
      ((string= trimmed "") 
       (log:debug "Flow value is empty")
       nil)
      ((parse-flow-sequence trimmed))
      ((parse-flow-mapping trimmed))
      (t 
       (let ((result (parse-scalar-value trimmed)))
         (log:debug "Flow value parsed as scalar: ~S" result)
         result)))))

;; Parsing specific types
(defun parse-scalar (state line indent)
  (declare (ignore state))
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (cond
      ;; Flow sequence
      ((parse-flow-sequence trimmed)
       (make-node :kind :sequence
                  :value (parse-flow-sequence trimmed)
                  :indent-level indent))
      ;; Flow mapping
      ((parse-flow-mapping trimmed)
       (make-node :kind :mapping
                  :value (parse-flow-mapping trimmed)
                  :indent-level indent))
      ;; Regular scalar
      (t
       (make-node :kind :scalar
                  :value (parse-scalar-value trimmed)
                  :indent-level indent)))))

(defun parse-scalar-value (string)
  "Parse a scalar value with type inference"
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (log:debug "Parsing scalar value: ~S -> ~S" string trimmed)
    (cond
      ;; Empty string
      ((string= trimmed "") nil)
      
      ;; Null values
      ((member trimmed '("null" "NULL" "Null" "~") :test #'string=) nil)
      
      ;; Boolean values
      ((member trimmed '("true" "TRUE" "True" "yes" "YES" "Yes" "on" "ON" "On") :test #'string=) t)
      ((member trimmed '("false" "FALSE" "False" "no" "NO" "No" "off" "OFF" "Off") :test #'string=) nil)
      
      ;; Integer values
      ((and (every (lambda (c) (or (digit-char-p c) (char= c #\-) (char= c #\+))) trimmed)
            (or (digit-char-p (char trimmed 0))
                (and (> (length trimmed) 1)
                     (member (char trimmed 0) '(#\- #\+))
                     (digit-char-p (char trimmed 1)))))
       (handler-case
           (parse-integer trimmed)
         (error () trimmed))) ; Return as string if parsing fails
      
      ;; Float values (simple detection)
      ((and (position #\. trimmed)
            (every (lambda (c) (or (digit-char-p c) (char= c #\.) (char= c #\-) (char= c #\+))) trimmed))
       (handler-case
           (read-from-string trimmed)
         (error () trimmed))) ; Return as string if parsing fails
      
      ;; Scientific notation (e.g., 1.2e-3)
      ((and (or (position #\e trimmed) (position #\E trimmed))
            (every (lambda (c) (or (digit-char-p c) (member c '(#\. #\e #\E #\- #\+)))) trimmed))
       (handler-case
           (read-from-string trimmed)
         (error () trimmed))) ; Return as string if parsing fails
      
      ;; Everything else is a string
      (t trimmed))))

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
  "Append a new line to an existing scalar value, handling spacing"
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
      ;; Skip empty lines within the mapping
      (loop for line = (peek-line (parser-state-stream state))
            while (and line (string= (string-trim '(#\Space #\Tab) line) ""))
            do (read-line-from (parser-state-stream state)))
      
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
      (let ((key (string-trim '(#\Space #\Tab) (subseq trimmed 0 colon-pos)))
            (value-part (string-trim '(#\Space #\Tab) 
                                   (subseq trimmed (1+ colon-pos)))))
        (log:debug "Split mapping line ~S -> key: ~S, value: ~S" line key value-part)
        (values key (if (string= value-part "") nil value-part))))))


(defun is-block-scalar-indicator? (string)
  "Check if string starts with a block scalar indicator (| or >)"
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (and (> (length trimmed) 0)
         (member (char trimmed 0) '(#\| #\>)))))

(defun parse-block-scalar (state indicator base-indent)
  "Parse a literal (|) or folded (>) block scalar"
  (log:debug "Parsing block scalar with indicator: ~S" indicator)
  (let ((lines nil)
        (literal-mode (char= indicator #\|))
        (first-line-indent nil))
    ;; Read all indented lines
    (loop
      (let ((next-line (peek-line (parser-state-stream state))))
        (when (or (null next-line)
                  (<= (count-indent next-line) base-indent))
          (return))
        
        (setf next-line (read-line-from (parser-state-stream state)))
        (let ((line-indent (count-indent next-line)))
          ;; Set first line indent as reference
          (unless first-line-indent
            (setf first-line-indent line-indent))
          ;; Remove the common indentation
          (let ((content (if (>= (length next-line) first-line-indent)
                           (subseq next-line first-line-indent)
                           "")))
            (push content lines)))))
    
    ;; Join lines based on mode
    (if literal-mode
        ;; Literal mode: preserve line breaks
        (format nil "~{~A~^~%~}" (nreverse lines))
        ;; Folded mode: join with spaces (simplified)
        (format nil "~{~A~^ ~}" (nreverse lines)))))

(defun parse-mapping-value (state initial-value base-indent)
  "Parse a mapping value, which could be a scalar, sequence, or nested mapping.
   The initial-value is the part after the colon on the same line as the key."
  (log:debug "Parsing mapping value: ~S (indent: ~D)" initial-value base-indent)
  (cond
    ;; No value on the same line, check for indented block
    ((null initial-value)
     (let ((next-line (peek-line (parser-state-stream state))))
       (if (and next-line 
                (> (count-indent next-line) base-indent))
           (parse-indented-value state (1+ base-indent))
           nil)))
    
    ;; Block scalar (literal or folded)
    ((is-block-scalar-indicator? initial-value)
     (let ((indicator (char (string-trim '(#\Space #\Tab) initial-value) 0)))
       (parse-block-scalar state indicator base-indent)))
    
    ;; Flow sequence
    ((parse-flow-sequence initial-value)
     (log:debug "Mapping value is flow sequence")
     (parse-flow-sequence initial-value))
    
    ;; Flow mapping  
    ((parse-flow-mapping initial-value)
     (log:debug "Mapping value is flow mapping")
     (parse-flow-mapping initial-value))
    
    ;; Value starts with sequence marker
    ((starts-with-sequence-marker? initial-value)
     (parse-sequence-items state initial-value (1+ base-indent)))
    
    ;; Value starts with mapping marker
    ((starts-with-mapping-marker? initial-value)
     (parse-mapping-pairs state initial-value (1+ base-indent)))
    
    ;; Simple scalar value
    (t 
     (parse-scalar-value initial-value))))

(defun parse-indented-value (state indent)
  "Parse an indented value block (sequence, mapping, or multiline scalar)"
  (let ((line (peek-line (parser-state-stream state))))
    (cond
      ;; Check for block scalar indicators first
      ((is-block-scalar-indicator? line)
       (let* ((line-content (read-line-from (parser-state-stream state)))
              (indicator (char (string-trim '(#\Space #\Tab) line-content) 0)))
         (parse-block-scalar state indicator indent)))
      
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

