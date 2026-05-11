;;;; epsilon.doc.parse -- Structured docstring parser
;;;;
;;;; Parses docstrings containing keyword sections (:param, :returns,
;;;; :signals, :see, :example, :since, :deprecated, :note) into structured
;;;; plists.  Unstructured docstrings return the full text as :summary.

(defpackage epsilon.doc.parse
  (:use :cl)
  (:export
   #:parse-docstring
   #:merge-annotations
   #:split-lines
   #:*section-keywords*))

;;; Section keyword registry

(defvar *section-keywords*
  '(:param :returns :signals :see :example :since :deprecated :note)
  "The set of recognized section keywords in structured docstrings.")

;;; Line utilities

(defun split-lines (string)
  "Split STRING into a list of lines."
  (if (or (null string) (string= string ""))
    (list "")
    (loop with start = 0
          with len = (length string)
          for i from 0 to len
          when (or (= i len)
                   (char= (char string i) #\Newline))
            collect (subseq string start i)
            and do (setf start (1+ i)))))

(defun trim-whitespace (string)
  "Trim leading and trailing whitespace from STRING."
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun line-section-keyword (line)
  "If LINE starts with a recognized section keyword (after optional whitespace),
   return (values KEYWORD REST-OF-LINE).  Otherwise return NIL."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (len (length trimmed)))
    (when (and (> len 0) (char= (char trimmed 0) #\:))
      (let ((space-pos (position-if (lambda (c) (or (char= c #\Space) (char= c #\Tab)))
                                    trimmed)))
        (if space-pos
          (let* ((kw-str (subseq trimmed 1 space-pos)) ; skip leading colon
                 (kw (find kw-str *section-keywords*
                           :test #'string-equal
                           :key #'symbol-name)))
            (when kw
              (values kw (subseq trimmed space-pos))))
          ;; Keyword at end of line (no content after it)
          (let* ((kw-str (subseq trimmed 1)) ; skip leading colon
                 (kw (find kw-str *section-keywords*
                           :test #'string-equal
                           :key #'symbol-name)))
            (when kw
              (values kw ""))))))))

;;; Continuation detection

(defun continuation-line-p (line)
  "A line is a continuation if it's non-empty after trimming and does not
   start with a recognized section keyword."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (and (> (length trimmed) 0)
         (not (line-section-keyword line)))))

;;; Section content extraction

(defun strip-dash-prefix (text)
  "Remove leading '-- ' from TEXT if present."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) text)))
    (if (and (>= (length trimmed) 3)
             (char= (char trimmed 0) #\-)
             (char= (char trimmed 1) #\-))
      (trim-whitespace (subseq trimmed 2))
      (trim-whitespace trimmed))))

(defun extract-param (rest-text)
  "Extract parameter name and doc from REST-TEXT after ':param'.
   REST-TEXT is everything after ':param' on the line."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) rest-text))
         ;; Find the end of the parameter name (whitespace only; -- is handled by strip-dash-prefix)
         (name-end (or (position #\Space trimmed)
                       (position #\Tab trimmed)
                       (length trimmed)))
         (name (subseq trimmed 0 name-end))
         (doc-part (if (< name-end (length trimmed))
                     (strip-dash-prefix (subseq trimmed name-end))
                     "")))
    (list :name name :doc doc-part)))

(defun parse-see-refs (text)
  "Parse :see text into a list of reference strings.
   References are comma-separated.  Backtick quoting is stripped."
  (let* ((clean (strip-dash-prefix text))
         (parts (split-by-comma clean)))
    (mapcar (lambda (s)
              (trim-whitespace (remove #\` s)))
            parts)))

(defun split-by-comma (string)
  "Split STRING by commas, returning a list of trimmed non-empty strings."
  (loop with start = 0
        with len = (length string)
        for i from 0 to len
        when (or (= i len) (char= (char string i) #\,))
          when (> i start)
            collect (trim-whitespace (subseq string start i))
          end
          and do (setf start (1+ i))))

(defun parse-examples (lines)
  "Parse example lines into a list of (:form ... :result ...) plists.
   Example forms start with '(' and results are marked with ';=>'."
  (let ((examples nil)
        (current-form nil)
        (current-result nil))
    (labels ((flush ()
               (when current-form
                 (push (list :form (trim-whitespace
                                    (format nil "~{~A~^ ~}" (nreverse current-form)))
                             :result current-result)
                       examples)
                 (setf current-form nil
                       current-result nil))))
      (dolist (line lines)
        (let ((trimmed (trim-whitespace line)))
          (cond
            ;; Result line: ;=> or ;;=>
            ((and (>= (length trimmed) 3)
                  (or (and (char= (char trimmed 0) #\;)
                           (char= (char trimmed 1) #\=)
                           (char= (char trimmed 2) #\>))
                      (and (>= (length trimmed) 4)
                           (char= (char trimmed 0) #\;)
                           (char= (char trimmed 1) #\;)
                           (char= (char trimmed 2) #\=)
                           (char= (char trimmed 3) #\>))))
             (let* ((arrow-pos (position #\> trimmed))
                    (rest (trim-whitespace (subseq trimmed (1+ arrow-pos)))))
               (setf current-result rest)))
            ;; New form line (starts with open paren)
            ((and (> (length trimmed) 0)
                  (char= (char trimmed 0) #\())
             (flush)
             (push trimmed current-form))
            ;; Empty line between examples
            ((string= trimmed "")
             (flush))
            ;; Continuation of current form
            (current-form
             (push trimmed current-form)))))
      (flush))
    (nreverse examples)))

;;; Main parser

(defun parse-docstring (docstring)
  "Parse DOCSTRING into a structured plist.

   Returns a plist with:
     :summary     -- Prose text before the first section keyword.
     :params      -- List of (:name ... :doc ...) plists.
     :returns     -- Return value description string.
     :signals     -- Conditions description string.
     :see         -- List of reference strings.
     :examples    -- List of (:form ... :result ...) plists.
     :since       -- Version string.
     :deprecated  -- Deprecation notice string.
     :note        -- Note string.

   Returns NIL if DOCSTRING is NIL."
  (when (null docstring) (return-from parse-docstring nil))
  (let* ((lines (split-lines docstring))
         (summary-lines nil)
         (sections nil)        ; alist of (keyword . lines)
         (current-section nil) ; (keyword . collected-lines)
         (in-sections nil))
    ;; Classify each line
    (dolist (line lines)
      (multiple-value-bind (kw rest) (line-section-keyword line)
        (cond
          ;; New section starts
          (kw
           (when current-section
             (push (cons (car current-section) (nreverse (cdr current-section)))
                   sections))
           (setf current-section (list kw rest)
                 in-sections t))
          ;; Continuation of current section
          ((and in-sections current-section (continuation-line-p line))
           (push line (cdr current-section)))
          ;; Blank line within sections (belongs to current section)
          ((and in-sections current-section (string= (trim-whitespace line) ""))
           (push line (cdr current-section)))
          ;; Summary line (before any section)
          (t
           (unless in-sections
             (push line summary-lines))))))
    ;; Flush final section
    (when current-section
      (push (cons (car current-section) (nreverse (cdr current-section)))
            sections))
    (setf sections (nreverse sections))
    ;; Build result
    (let ((summary (build-summary (nreverse summary-lines)))
          (params nil)
          (returns nil)
          (signals nil)
          (see nil)
          (examples nil)
          (since nil)
          (deprecated nil)
          (note nil))
      ;; Process each collected section
      (dolist (sec sections)
        (let* ((kw (car sec))
               (sec-lines (cdr sec))
               (joined (join-section-lines sec-lines)))
          (case kw
            (:param
             (push (extract-param joined) params))
            (:returns
             (setf returns (strip-dash-prefix joined)))
            (:signals
             (setf signals (strip-dash-prefix joined)))
            (:see
             (setf see (parse-see-refs joined)))
            (:example
             (setf examples (parse-examples sec-lines)))
            (:since
             (setf since (trim-whitespace joined)))
            (:deprecated
             (setf deprecated (trim-whitespace joined)))
            (:note
             (setf note (trim-whitespace joined))))))
      (setf params (nreverse params))
      ;; Build result plist, omitting NIL values for non-summary fields
      (let ((result (list :summary summary)))
        (when params (setf (getf result :params) params))
        (when returns (setf (getf result :returns) returns))
        (when signals (setf (getf result :signals) signals))
        (when see (setf (getf result :see) see))
        (when examples (setf (getf result :examples) examples))
        (when since (setf (getf result :since) since))
        (when (and deprecated (not (string= deprecated "")))
          (setf (getf result :deprecated) deprecated))
        (when (and note (not (string= note "")))
          (setf (getf result :note) note))
        ;; Special case: deprecated with no content
        (when (and (assoc :deprecated sections)
                   (not (getf result :deprecated)))
          (setf (getf result :deprecated) ""))
        result))))

(defun build-summary (lines)
  "Build the summary string from collected summary lines.
   Trims trailing blank lines."
  ;; Remove trailing blank lines
  (let ((trimmed (let ((result (copy-list lines)))
                   (setf result (nreverse result))
                   (loop while (and result (string= (trim-whitespace (car result)) ""))
                         do (pop result))
                   (nreverse result))))
    (if trimmed
      (trim-whitespace (format nil "~{~A~^~%~}" trimmed))
      "")))

(defun join-section-lines (lines)
  "Join section content lines into a single string."
  (format nil "~{~A~^ ~}" (mapcar (lambda (l) (trim-whitespace l)) lines)))

;;; Annotation merging

(defun merge-annotations (parsed annotations)
  "Merge annotation alist ANNOTATIONS into PARSED docstring plist.

   For :see, values are merged (union).  For scalar fields (:since,
   :stability, :tags, etc.), annotation values override docstring values.
   Unknown annotation keys are added directly."
  (when (null annotations) (return-from merge-annotations parsed))
  (let ((result (copy-list parsed)))
    (dolist (pair annotations)
      (let ((key (car pair))
            (value (cdr pair)))
        (case key
          (:see
           ;; Merge :see lists
           (let* ((existing (getf result :see))
                  (new-refs (if (listp value) value (list value)))
                  (merged (union existing new-refs :test #'string=)))
             (setf (getf result :see) merged)))
          (otherwise
           ;; Scalar override or new key
           (setf (getf result key) value)))))
    result))
