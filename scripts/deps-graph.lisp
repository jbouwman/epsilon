#!/usr/bin/env sbcl --script

;;; Generate a markdown-renderable dependency graph of all epsilon modules

;; Suppress boot messages
(let ((*standard-output* (make-broadcast-stream)))
  (load "scripts/boot.lisp"))

;; Boot needs to happen after boot.lisp is loaded
(let ((*standard-output* (make-broadcast-stream)))
  (epsilon.tool.boot:boot))

(defun generate-dependency-graph ()
  "Generate a Mermaid graph of module dependencies"
  (let ((modules (epsilon.lib.map:vals epsilon.tool.build::*modules*))
        (module-info '()))
    
    ;; Collect module information
    (dolist (module-dir modules)
      (handler-case
          (let* ((edn-file (epsilon.lib.path:uri-merge module-dir "package.edn"))
                 (edn-content (epsilon.sys.fs:read-file (epsilon.lib.path:path-from-uri edn-file)))
                 (parsed (epsilon.lib.edn:read-edn-from-string edn-content))
                 (name (epsilon.lib.map:get parsed "name"))
                 (deps (or (epsilon.lib.map:get parsed "dependencies") '()))
                 ;; Convert vector to list if needed
                 (deps-list (if (vectorp deps) (coerce deps 'list) deps)))
            (push (list name deps-list) module-info))
        (error (e)
          (format *error-output* ";;; Warning: Failed to load module from ~A: ~A~%" 
                  (epsilon.lib.path:path-from-uri module-dir) e))))
    
    ;; Generate Mermaid graph
    (format t "# Epsilon Module Dependencies~%~%")
    (format t "```mermaid~%")
    (format t "graph TD~%")
    
    ;; Style definitions
    (format t "    classDef core fill:#f9f,stroke:#333,stroke-width:4px;~%")
    (format t "    classDef test fill:#9ff,stroke:#333,stroke-width:2px;~%")
    (format t "    classDef net fill:#ff9,stroke:#333,stroke-width:2px;~%")
    (format t "    classDef tool fill:#9f9,stroke:#333,stroke-width:2px;~%")
    (format t "    classDef data fill:#f99,stroke:#333,stroke-width:2px;~%")
    (format t "    classDef platform fill:#ccc,stroke:#333,stroke-width:2px;~%")
    (format t "~%")
    
    ;; Sort modules for consistent output
    (setf module-info (sort module-info #'string< :key #'first))
    
    ;; Generate nodes and edges
    (dolist (module-data module-info)
      (let* ((name (first module-data))
             (deps (second module-data))
             (clean-name (if (string= name "epsilon.core")
                            "core"
                            (let ((prefix "epsilon."))
                              (if (and (>= (length name) (length prefix))
                                       (string= (subseq name 0 (length prefix)) prefix))
                                  (subseq name (length prefix))
                                  name)))))
        
        ;; Add node with appropriate style
        (cond
          ((string= name "epsilon.core")
           (format t "    ~A[~A]:::core~%" clean-name clean-name))
          ((search "test" name)
           (format t "    ~A[~A]:::test~%" clean-name clean-name))
          ((or (search "http" name) (search "websocket" name) (search "net" name))
           (format t "    ~A[~A]:::net~%" clean-name clean-name))
          ((or (search "format" name) (search "lsp" name) (search "package" name))
           (format t "    ~A[~A]:::tool~%" clean-name clean-name))
          ((or (search "yaml" name) (search "msgpack" name) (search "xml" name))
           (format t "    ~A[~A]:::data~%" clean-name clean-name))
          ((or (search "darwin" name) (search "linux" name) (search "windows" name))
           (format t "    ~A[~A]:::platform~%" clean-name clean-name))
          (t
           (format t "    ~A[~A]~%" clean-name clean-name)))
        
        ;; Add edges for dependencies
        (dolist (dep deps)
          (let ((dep-clean (if (string= dep "epsilon.core")
                              "core"
                              (let ((prefix "epsilon."))
                                (if (and (>= (length dep) (length prefix))
                                         (string= (subseq dep 0 (length prefix)) prefix))
                                    (subseq dep (length prefix))
                                    dep)))))
            (format t "    ~A --> ~A~%" clean-name dep-clean)))))
    
    (format t "```~%~%")
    
    ;; Add legend
    (format t "## Legend~%~%")
    (format t "- **Pink**: Core module~%")
    (format t "- **Light Blue**: Test frameworks~%")
    (format t "- **Yellow**: Network modules~%")
    (format t "- **Green**: Development tools~%")
    (format t "- **Light Red**: Data format modules~%")
    (format t "- **Gray**: Platform-specific modules~%~%")
    
    ;; Add statistics
    (format t "## Statistics~%~%")
    (format t "- Total modules: ~D~%" (length module-info))
    (let ((deps-count (reduce #'+ module-info :key (lambda (m) (length (second m))))))
      (format t "- Total dependencies: ~D~%" deps-count))
    (format t "- Average dependencies per module: ~,2F~%"
            (if (zerop (length module-info))
                0
                (/ (float (reduce #'+ module-info :key (lambda (m) (length (second m)))))
                   (length module-info))))))

(generate-dependency-graph)