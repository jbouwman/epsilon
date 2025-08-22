(defpackage #:epsilon.frame.view
  (:use #:cl)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column)
   (#:frame #:epsilon.frame))
  (:export
   ;; View structure
   #:view
   #:view-p
   #:make-view
   #:view-source
   #:view-row-indices
   #:view-column-names
   
   ;; View creation
   #:frame-view
   #:column-view

   #:column-view-indices
   #:column-view-source
   
   ;; View materialization
   #:materialize-view
   #:materialize-column-view))

(in-package #:epsilon.frame.view)

(defstruct view
  "A zero-copy view of a frame or column"
  (source nil)           ; Original frame or column
  (row-indices nil)      ; Which rows to include (nil = all)
  (column-names nil))    ; Which columns to include (nil = all)

(defstruct column-view
  "A zero-copy view of a column"
  (source nil)           ; Original column
  (indices nil))         ; Which elements to include (nil = all)

(defun frame-view (frame &key rows columns)
  "Create a zero-copy view of a frame"
  (make-view :source frame
             :row-indices rows
             :column-names (when columns
                            (mapcar (lambda (name) (string-downcase (string name))) columns))))

(defun column-view (column &key indices)
  "Create a zero-copy view of a column"
  (make-column-view :source column
                    :indices indices))

(defun materialize-view (view)
  "Materialize a view into a new frame"
  (let* ((source (view-source view))
         (row-indices (or (view-row-indices view)
                         (loop for i from 0 below (frame:nrows source)
                               collect i)))
         (col-names (or (view-column-names view)
                       (frame:column-names source)))
         (new-columns (make-hash-table :test 'equal)))
    ;; Create new columns with selected rows
    (dolist (col-name col-names)
      (let* ((source-column (frame:get-column source col-name))
             (dtype (col:column-dtype source-column))
             (new-data (loop for idx in row-indices
                            collect (col:column-get source-column idx))))
        (setf (gethash col-name new-columns)
              (col:make-column dtype new-data))))
    ;; Create new frame
    (frame::%make-frame :columns new-columns
                        :column-order col-names
                        :nrows (length row-indices))))

(defun materialize-column-view (view)
  "Materialize a column view into a new column"
  (let* ((source (column-view-source view))
         (indices (or (column-view-indices view)
                      (loop for i from 0 below (col:column-length source)
                            collect i)))
         (dtype (col:column-dtype source))
         (new-data (loop for idx in indices
                        collect (col:column-get source idx))))
    (col:make-column dtype new-data)))

