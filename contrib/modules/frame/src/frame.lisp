(defpackage #:epsilon.frame
  (:use #:cl)
  (:local-nicknames
   (#:dtype #:epsilon.frame.dtype)
   (#:col #:epsilon.frame.column))
  (:export
   ;; Frame construction
   #:frame
   #:make-frame
   #:frame-p
   #:%make-frame  ; For internal use
   
   ;; Basic access
   #:get-column
   #:get-row
   #:ncols
   #:nrows
   #:column-names
   #:shape
   
   ;; Operations
   #:select
   #:slice
   #:where
   #:head
   #:tail
   
   ;; Column operations
   #:add-column
   #:drop-column
   #:rename-column
   
   ;; Conversion
   #:frame->lists
   #:lists->frame
   #:frame->plists
   #:plists->frame
   
   ;; Threading macro
   #:->))

(in-package #:epsilon.frame)

(defstruct (frame (:constructor %make-frame))
  "A columnar data frame"
  (columns (make-hash-table :test 'equal) :type hash-table)
  (column-order nil :type list)
  (nrows 0 :type fixnum))

(defun make-frame (&rest column-specs)
  "Create a frame from alternating column names and columns/data
   Example: (make-frame :a (column :int32 1 2 3) :b '(4 5 6))"
  (let ((frame (%make-frame))
        (first-length nil))
    (loop for (name data) on column-specs by #'cddr
          do (let ((column (if (col:column-p data)
                              data
                              (col:make-column (dtype:infer-dtype (cond
                                                      ((and (listp data) (not (null data))) (first data))
                                                      ((and (vectorp data) (> (length data) 0)) (aref data 0))
                                                      (t :any))) data))))
               ;; Ensure all columns have same length
               (if first-length
                   (unless (= (col:column-length column) first-length)
                     (error "All columns must have same length"))
                   (setf first-length (col:column-length column)))
               ;; Add column to frame
               (setf (gethash (string-downcase (string name)) (frame-columns frame)) column)
               (push (string-downcase (string name)) (frame-column-order frame))))
    (setf (frame-column-order frame) (nreverse (frame-column-order frame)))
    (setf (frame-nrows frame) (or first-length 0))
    frame))

(defun frame (&rest column-specs)
  "Convenience constructor for frames"
  (apply #'make-frame column-specs))

(defun get-column (frame name)
  "Get a column by name"
  (or (gethash (string-downcase (string name)) (frame-columns frame))
      (error "Column ~A not found" name)))

(defun get-row (frame index)
  "Get a row as a property list"
  (when (or (< index 0) (>= index (frame-nrows frame)))
    (error "Row index ~A out of bounds" index))
  (let ((result nil))
    (dolist (col-name (reverse (frame-column-order frame)))
      (let ((column (gethash col-name (frame-columns frame))))
        (push (col:column-get column index) result)
        (push (intern (string-upcase col-name) :keyword) result)))
    result))

(defun ncols (frame)
  "Number of columns in frame"
  (length (frame-column-order frame)))

(defun nrows (frame)
  "Number of rows in frame"
  (frame-nrows frame))

(defun column-names (frame)
  "List of column names in order"
  (copy-list (frame-column-order frame)))

(defun shape (frame)
  "Return (nrows ncols) of frame"
  (list (nrows frame) (ncols frame)))

(defun select (frame &rest column-names)
  "Select specified columns, returning a new frame"
  (let ((new-frame (%make-frame :nrows (frame-nrows frame))))
    (dolist (name column-names)
      (let* ((name-str (string-downcase (string name)))
             (column (gethash name-str (frame-columns frame))))
        (unless column
          (error "Column ~A not found" name))
        (setf (gethash name-str (frame-columns new-frame)) column)
        (push name-str (frame-column-order new-frame))))
    (setf (frame-column-order new-frame) 
          (nreverse (frame-column-order new-frame)))
    new-frame))

(defun slice (frame start &optional end)
  "Slice rows from start to end (exclusive)"
  (let* ((end (or end (frame-nrows frame)))
         (new-frame (%make-frame :nrows (- end start))))
    (dolist (col-name (frame-column-order frame))
      (let* ((column (gethash col-name (frame-columns frame)))
             (new-column (col:column-slice column start end)))
        (setf (gethash col-name (frame-columns new-frame)) new-column)
        (push col-name (frame-column-order new-frame))))
    (setf (frame-column-order new-frame) 
          (nreverse (frame-column-order new-frame)))
    new-frame))

(defun where (frame predicate)
  "Filter rows where predicate returns true
   Predicate receives a plist of row values"
  (let ((indices nil))
    ;; Find matching row indices
    (loop for i from 0 below (frame-nrows frame)
          when (funcall predicate (get-row frame i))
          do (push i indices))
    (setf indices (nreverse indices))
    ;; Create new frame with filtered data
    (let ((new-frame (%make-frame :nrows (length indices))))
      (dolist (col-name (frame-column-order frame))
        (let* ((column (gethash col-name (frame-columns frame)))
               (dtype (col:column-dtype column))
               (new-data (loop for idx in indices
                              collect (col:column-get column idx)))
               (new-column (col:make-column dtype new-data)))
          (setf (gethash col-name (frame-columns new-frame)) new-column)
          (push col-name (frame-column-order new-frame))))
      (setf (frame-column-order new-frame) 
            (nreverse (frame-column-order new-frame)))
      new-frame)))

(defun head (frame &optional (n 5))
  "Return first n rows"
  (slice frame 0 (min n (frame-nrows frame))))

(defun tail (frame &optional (n 5))
  "Return last n rows"
  (let ((start (max 0 (- (frame-nrows frame) n))))
    (slice frame start)))

(defun add-column (frame name column-or-data)
  "Add a column to the frame, returning a new frame"
  (let* ((column (if (col:column-p column-or-data)
                     column-or-data
                     (col:make-column (dtype:infer-dtype (first column-or-data))
                                     column-or-data)))
         (name-str (string-downcase (string name))))
    (unless (= (col:column-length column) (frame-nrows frame))
      (error "Column length ~A doesn't match frame rows ~A"
             (col:column-length column) (frame-nrows frame)))
    ;; Create new frame with added column
    (let ((new-frame (%make-frame :nrows (frame-nrows frame))))
      ;; Copy existing columns
      (dolist (col-name (frame-column-order frame))
        (setf (gethash col-name (frame-columns new-frame))
              (gethash col-name (frame-columns frame)))
        (push col-name (frame-column-order new-frame)))
      ;; Add new column
      (setf (gethash name-str (frame-columns new-frame)) column)
      (push name-str (frame-column-order new-frame))
      (setf (frame-column-order new-frame) 
            (nreverse (frame-column-order new-frame)))
      new-frame)))

(defun drop-column (frame name)
  "Remove a column from the frame, returning a new frame"
  (let* ((name-str (string-downcase (string name)))
         (new-frame (%make-frame :nrows (frame-nrows frame))))
    (unless (gethash name-str (frame-columns frame))
      (error "Column ~A not found" name))
    ;; Copy all columns except the one to drop
    (dolist (col-name (frame-column-order frame))
      (unless (string= col-name name-str)
        (setf (gethash col-name (frame-columns new-frame))
              (gethash col-name (frame-columns frame)))
        (push col-name (frame-column-order new-frame))))
    (setf (frame-column-order new-frame) 
          (nreverse (frame-column-order new-frame)))
    new-frame))

(defun rename-column (frame old-name new-name)
  "Rename a column, returning a new frame"
  (let* ((old-str (string-downcase (string old-name)))
         (new-str (string-downcase (string new-name)))
         (new-frame (%make-frame :nrows (frame-nrows frame))))
    (unless (gethash old-str (frame-columns frame))
      (error "Column ~A not found" old-name))
    ;; Copy columns with renamed column
    (dolist (col-name (frame-column-order frame))
      (let ((target-name (if (string= col-name old-str) new-str col-name)))
        (setf (gethash target-name (frame-columns new-frame))
              (gethash col-name (frame-columns frame)))
        (push target-name (frame-column-order new-frame))))
    (setf (frame-column-order new-frame) 
          (nreverse (frame-column-order new-frame)))
    new-frame))

(defun frame->lists (frame)
  "Convert frame to list of lists (row-oriented)"
  (loop for i from 0 below (frame-nrows frame)
        collect (loop for col-name in (frame-column-order frame)
                     collect (col:column-get 
                              (gethash col-name (frame-columns frame)) i))))

(defun lists->frame (column-names data)
  "Create frame from list of lists (row-oriented)"
  (let ((columns (make-hash-table :test 'equal))
        (ncols (length column-names))
        (nrows (length data)))
    ;; Transpose data to column-oriented
    (loop for col-idx from 0 below ncols
          for col-name in column-names
          do (let ((col-data (loop for row in data
                                   collect (nth col-idx row))))
               (setf (gethash (string-downcase (string col-name)) columns)
                     (col:make-column (dtype:infer-dtype (first col-data))
                                     col-data))))
    ;; Create frame
    (let ((frame (%make-frame :columns columns
                              :column-order (mapcar (lambda (name) (string-downcase (string name))) column-names)
                              :nrows nrows)))
      frame)))

(defun frame->plists (frame)
  "Convert frame to list of property lists"
  (loop for i from 0 below (frame-nrows frame)
        collect (get-row frame i)))

(defun plists->frame (plists)
  "Create frame from list of property lists"
  (when (null plists)
    (return-from plists->frame (%make-frame)))
  ;; Extract column names from first plist
  (let* ((first-plist (first plists))
         (col-names (loop for (key nil) on first-plist by #'cddr
                          collect (string-downcase (string key))))
         (columns (make-hash-table :test 'equal)))
    ;; Collect data for each column
    (loop for col-name in col-names
          do (let* ((key (intern (string-upcase col-name) :keyword))
                    (col-data (loop for plist in plists
                                   collect (getf plist key))))
               (setf (gethash col-name columns)
                     (col:make-column (dtype:infer-dtype (first col-data))
                                     col-data))))
    ;; Create frame
    (%make-frame :columns columns
                 :column-order col-names
                 :nrows (length plists))))

(defmethod print-object ((frame frame) stream)
  "Print representation of a frame"
  (print-unreadable-object (frame stream :type t)
    (format stream "~A x ~A" 
            (frame-nrows frame)
            (ncols frame))
    (when (> (ncols frame) 0)
      (format stream " [~{~A~^, ~}]" 
              (subseq (frame-column-order frame) 
                      0 (min 3 (length (frame-column-order frame))))))))


;;; Threading macro
(defmacro -> (x &rest forms)
  "Thread-first macro for chaining frame operations"
  (if (null forms)
      x
      (let ((form (first forms)))
        (if (listp form)
            `(-> (,(first form) ,x ,@(rest form)) ,@(rest forms))
            `(-> (,form ,x) ,@(rest forms))))))