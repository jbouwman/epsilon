;;;; Table Formatting Library
;;;;
;;;; Provides utilities for formatting tabular data with automatic column
;;;; sizing, alignment, and styling options.

(defpackage epsilon.table
  (:use cl)
  (:local-nicknames
   (str epsilon.string))
  (:export
   ;; Main API
   #:print-table
   #:format-table
   #:simple-table
   
   ;; Table creation
   #:create-table
   #:add-row
   #:add-column
   
   ;; Column configuration
   #:make-column
   #:column-header
   #:column-width
   #:column-align
   #:column-formatter
   
   ;; Table structure
   #:table
   #:table-columns
   #:table-rows
   #:table-options))

(in-package epsilon.table)

;;; Table Structure

(defstruct column
  "Represents a table column with formatting options"
  (header "" :type string)
  (width nil :type (or null integer))
  (align :left :type (member :left :right :center))
  (formatter #'identity :type function))

(defstruct table
  "Represents a table with columns and data rows"
  (columns '() :type list)
  (rows '() :type list)
  (options '() :type list))

;;; Table Creation

(defun create-table (&key columns (show-header t) (separator-char #\-) (column-spacing 2))
  "Create a new table with optional columns and formatting options"
  (make-table :columns (mapcar (lambda (col)
                                (if (stringp col)
                                    (make-column :header col)
                                    col))
                              columns)
              :options (list :show-header show-header
                           :separator-char separator-char
                           :column-spacing column-spacing)))

(defun add-column (table header &key width align formatter)
  "Add a column to the table"
  (let ((column (make-column :header header
                            :width width
                            :align (or align :left)
                            :formatter (or formatter #'identity))))
    (setf (table-columns table) (append (table-columns table) (list column)))
    table))

(defun add-row (table row-data)
  "Add a row of data to the table"
  (setf (table-rows table) (append (table-rows table) (list row-data)))
  table)

;;; Column Width Calculation

(defun calculate-column-widths (table)
  "Calculate optimal column widths based on content"
  (let ((columns (table-columns table))
        (rows (table-rows table)))
    (mapcar (lambda (column index)
              (let ((header-width (length (column-header column)))
                    (content-width (if rows
                                     (apply #'max
                                            (mapcar (lambda (row)
                                                     (let ((cell-data (nth index row)))
                                                       (length (string (funcall (column-formatter column) cell-data)))))
                                                   rows))
                                     0)))
                (or (column-width column)
                    (max header-width content-width))))
            columns
            (loop for i from 0 below (length columns) collect i))))

;;; Formatting Functions

(defun format-cell (content width align)
  "Format a single cell with the specified width and alignment"
  (let ((str (string content)))
    (cond
      ((eq align :left)
       (format nil "~VA" width str))
      ((eq align :right)
       (format nil "~V@A" width str))
      ((eq align :center)
       (let* ((padding (- width (length str)))
              (left-pad (floor padding 2))
              (right-pad (- padding left-pad)))
         (format nil "~V@A~A~V@A" left-pad "" str right-pad ""))))))

(defun format-separator-line (widths separator-char column-spacing)
  "Format a separator line using the specified character"
  (let ((separator-parts 
         (mapcar (lambda (width)
                   (make-string width :initial-element separator-char))
                 widths))
        (spacing-str (make-string column-spacing :initial-element #\Space)))
    (format nil "~{~A~^~A~}" 
            (loop for part in separator-parts
                  for i from 0
                  collect part
                  when (< i (1- (length separator-parts)))
                  collect spacing-str))))

(defun format-table-row (row columns widths column-spacing)
  "Format a single table row"
  (let ((formatted-cells
         (mapcar (lambda (cell column width)
                   (let ((formatted-content (funcall (column-formatter column) cell)))
                     (format-cell formatted-content width (column-align column))))
                 row columns widths))
        (spacing-str (make-string column-spacing :initial-element #\Space)))
    (format nil "~{~A~}" 
            (loop for cell in formatted-cells
                  for i from 0
                  collect cell
                  when (< i (1- (length formatted-cells)))
                  collect spacing-str))))

;;; Main API

(defun format-table (table &optional (stream nil))
  "Format table to string or stream"
  (let ((columns (table-columns table))
        (rows (table-rows table))
        (options (table-options table)))
    
    (when (null columns)
      (return-from format-table ""))
    
    (let* ((widths (calculate-column-widths table))
           (show-header (getf options :show-header t))
           (separator-char (getf options :separator-char #\-))
           (column-spacing (getf options :column-spacing 2))
           (spacing-str (make-string column-spacing :initial-element #\Space))
           (output '()))
      
      ;; Header
      (when show-header
        (let* ((headers (mapcar #'column-header columns))
               (formatted-headers (mapcar (lambda (header width column)
                                           (format-cell header width (column-align column)))
                                         headers widths columns))
               (header-line (format nil "~{~A~}" 
                                           (loop for cell in formatted-headers
                                                 for i from 0
                                                 collect cell
                                                 when (< i (1- (length formatted-headers)))
                                                 collect spacing-str)))
               (separator-line (format-separator-line widths separator-char column-spacing)))
          (push header-line output)
          (push separator-line output)))
      
      ;; Data rows
      (dolist (row rows)
        (let* ((formatted-cells (mapcar (lambda (cell column width)
                                         (let ((formatted (funcall (column-formatter column) cell)))
                                           (format-cell formatted width (column-align column))))
                                       row columns widths))
               (row-line (format nil "~{~A~}"
                                (loop for cell in formatted-cells
                                      for i from 0
                                      collect cell
                                      when (< i (1- (length formatted-cells)))
                                      collect spacing-str))))
          (push row-line output)))
      
      ;; Output
      (let ((result (format nil "~{~A~%~}" (nreverse output))))
        (if stream
            (write-string result stream)
            result)))))

(defun print-table (table &optional (stream *standard-output*))
  "Print table to stream (default: *standard-output*)"
  (format-table table stream)
  (values))

;;; Convenience Functions

(defun simple-table (headers rows &key (show-header t) (separator-char #\-))
  "Create and format a simple table from headers and rows"
  (let ((table (create-table :show-header show-header :separator-char separator-char)))
    (dolist (header headers)
      (add-column table header))
    (dolist (row rows)
      (add-row table row))
    table))