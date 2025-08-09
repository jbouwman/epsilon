(defpackage #:epsilon.lsp.workspace
  (:use #:common-lisp)
  (:local-nicknames
   (#:map #:epsilon.map)
   (#:fs #:epsilon.sys.fs)
   (#:analysis #:epsilon.lsp.analysis))
  (:export
   #:workspace
   #:make-workspace
   #:create-workspace
   #:workspace-add-folder
   #:workspace-remove-folder
   #:workspace-add-document
   #:workspace-remove-document
   #:workspace-get-document
   #:workspace-update-document
   #:workspace-find-symbol
   #:workspace-get-all-symbols
   #:symbol-location
   #:symbol-location-uri
   #:symbol-location-symbol-info))

(in-package #:epsilon.lsp.workspace)

(defstruct workspace
  "LSP workspace containing multiple folders and documents."
  folders          ; List of workspace folder URIs
  documents        ; Map of URI -> document analysis
  symbol-index)    ; Map of symbol name -> list of locations

(defun create-workspace (&optional folders)
  "Create a new workspace with optional initial folders."
  (let ((ws (make-workspace
             :folders (or folders '())
             :documents (map:make-map)
             :symbol-index (map:make-map))))
    (when folders
      (dolist (folder folders)
        (workspace-add-folder ws folder)))
    ws))

;;; Folder Management

(defun workspace-add-folder (workspace folder-uri)
  "Add a folder to the workspace."
  (unless (member folder-uri (workspace-folders workspace) :test #'string=)
    (push folder-uri (workspace-folders workspace))
    ;; TODO: Implement folder scanning
    (format t "Added workspace folder: ~A~%" folder-uri)))

(defun workspace-remove-folder (workspace folder-uri)
  "Remove a folder from the workspace."
  (setf (workspace-folders workspace)
        (remove folder-uri (workspace-folders workspace) :test #'string=))
  ;; Remove all documents from this folder
  (let ((documents-to-remove '()))
    (map:each (lambda (uri analysis)
                (when (string-starts-with-p uri folder-uri)
                  (push uri documents-to-remove)))
              (workspace-documents workspace))
    (dolist (uri documents-to-remove)
      (workspace-remove-document workspace uri))))

;;; Document Management

(defun workspace-add-document (workspace uri content)
  "Add or update a document in the workspace."
  (let ((analysis (analysis:analyze-document uri content)))
    (setf (workspace-documents workspace)
          (map:put (workspace-documents workspace) uri analysis))
    (update-symbol-index workspace analysis)))

(defun workspace-remove-document (workspace uri)
  "Remove a document from the workspace."
  (let ((analysis (map:get (workspace-documents workspace) uri)))
    (when analysis
      (remove-from-symbol-index workspace analysis)
      (setf (workspace-documents workspace)
            (map:dissoc (workspace-documents workspace) uri)))))

(defun workspace-get-document (workspace uri)
  "Get a document analysis from the workspace."
  (map:get (workspace-documents workspace) uri))

(defun workspace-update-document (workspace uri content)
  "Update a document in the workspace."
  (let ((old-analysis (workspace-get-document workspace uri)))
    (when old-analysis
      (remove-from-symbol-index workspace old-analysis))
    (workspace-add-document workspace uri content)))

;;; Symbol Index Management

(defun update-symbol-index (workspace analysis)
  "Update the symbol index with symbols from the analysis."
  (dolist (symbol (analysis:document-analysis-symbols analysis))
    (let ((symbol-name (analysis:symbol-info-name symbol))
          (location (create-symbol-location 
                     (analysis:document-analysis-uri analysis)
                     symbol)))
      (let ((existing-locations (map:get (workspace-symbol-index workspace) 
                                         symbol-name)))
        (setf (workspace-symbol-index workspace)
              (map:put (workspace-symbol-index workspace)
                       symbol-name
                       (cons location existing-locations)))))))

(defun remove-from-symbol-index (workspace analysis)
  "Remove symbols from the index for the given analysis."
  (let ((uri (analysis:document-analysis-uri analysis)))
    (map:each (lambda (symbol-name locations)
                  (let ((filtered-locations 
                         (remove-if (lambda (loc) 
                                      (string= (symbol-location-uri loc) uri))
                                    locations)))
                    (if filtered-locations
                        (setf (workspace-symbol-index workspace)
                              (map:put (workspace-symbol-index workspace)
                                       symbol-name filtered-locations))
                        (setf (workspace-symbol-index workspace)
                              (map:dissoc (workspace-symbol-index workspace)
                                          symbol-name)))))
                (workspace-symbol-index workspace))))

(defstruct symbol-location
  "Location of a symbol in the workspace."
  uri
  range
  symbol-info)

(defun create-symbol-location (uri symbol-info)
  "Create a symbol location."
  (make-symbol-location
   :uri uri
   :range (analysis:symbol-info-range symbol-info)
   :symbol-info symbol-info))

;;; Search Functions

(defun workspace-find-symbol (workspace symbol-name)
  "Find all locations of a symbol in the workspace."
  (map:get (workspace-symbol-index workspace) symbol-name))

(defun workspace-get-all-symbols (workspace &optional filter)
  "Get all symbols in the workspace, optionally filtered."
  (let ((symbols '()))
    (map:each (lambda (name locations)
                  (declare (ignore name))
                  (dolist (location locations)
                    (when (or (not filter)
                              (funcall filter (symbol-location-symbol-info location)))
                      (push location symbols))))
                (workspace-symbol-index workspace))
    symbols))

;;; Utility Functions

(defun string-starts-with-p (string prefix)
  "Check if STRING starts with PREFIX."
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))
