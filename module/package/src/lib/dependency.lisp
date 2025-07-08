(defpackage :epsilon.lib.package.dependency
  (:use
   :cl)
  (:local-nicknames
   (:map :epsilon.lib.map)
   (:seq :epsilon.lib.sequence)
   (:str :epsilon.lib.string)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.lib.uri)
   (:edn :epsilon.lib.edn))
  (:export
   ;; Dependency resolution
   :resolve-dependencies
   :topological-sort
   :build-dependency-graph
   
   ;; Module loading
   :load-module-info
   :get-module-dependencies
   :get-module-provides
   
   ;; Build ordering
   :compute-build-order
   :check-circular-dependencies))

(in-package :epsilon.lib.package.dependency)

;;;; ==========================================================================
;;;; Module Information Loading
;;;; ==========================================================================

(defun load-module-info (module-path)
  "Load module information from package.edn"
  (let ((package-file (uri:merge module-path "package.edn")))
    (unless (fs:exists-p package-file)
      (error "No package.edn found at ~A" module-path))
    (with-open-file (stream (uri:path package-file))
      (edn:read-edn stream))))

(defun get-module-dependencies (module-info)
  "Extract dependencies from module info"
  (or (map:get module-info "dependencies") '()))

(defun get-module-provides (module-info)
  "Extract provided packages from module info"
  (or (map:get module-info "provides") '()))

;;;; ==========================================================================
;;;; Dependency Graph Building
;;;; ==========================================================================

(defstruct dep-node
  "Node in dependency graph"
  name
  module-path
  module-info
  dependencies
  dependents)

(defun build-dependency-graph (modules)
  "Build dependency graph from module list.
   MODULES is a map of module-name -> module-path"
  (let ((nodes (map:make-map))
        (errors '()))
    
    ;; First pass: create nodes
    (map:each (lambda (name path)
                  (handler-case
                      (let* ((info (load-module-info path))
                             (deps (get-module-dependencies info))
                             (node (make-dep-node
                                    :name name
                                    :module-path path
                                    :module-info info
                                    :dependencies deps
                                    :dependents '())))
                        (setf nodes (map:assoc nodes name node)))
                    (error (e)
                      (push (cons name e) errors))))
                modules)
    
    ;; Report errors if any
    (when errors
      (error "Failed to load modules: ~{~A: ~A~^, ~}" 
             (mapcar (lambda (e) (list (car e) (cdr e))) errors)))
    
    ;; Second pass: link dependents
    (map:each (lambda (name node)
                  (dolist (dep (dep-node-dependencies node))
                    (let ((dep-node (map:get nodes dep)))
                      (when dep-node
                        (setf (dep-node-dependents dep-node)
                              (cons name (dep-node-dependents dep-node)))))))
                nodes)
    
    nodes))

;;;; ==========================================================================
;;;; Circular Dependency Detection
;;;; ==========================================================================

(defun check-circular-dependencies (graph)
  "Check for circular dependencies in the graph.
   Returns NIL if no cycles, or a list of cycles found."
  (let ((visited (map:make-map))
        (rec-stack (map:make-map))
        (cycles '()))
    
    (labels ((dfs (node-name path)
               (setf visited (map:assoc visited node-name t))
               (setf rec-stack (map:assoc rec-stack node-name t))
               (push node-name path)
               
               (let ((node (map:get graph node-name)))
                 (when node
                   (dolist (dep (dep-node-dependencies node))
                     (cond
                       ;; Found a cycle
                       ((map:get rec-stack dep)
                        (let ((cycle-start (position dep path :test #'string=)))
                          (when cycle-start
                            (push (subseq path cycle-start) cycles))))
                       
                       ;; Not visited yet
                       ((not (map:get visited dep))
                        (dfs dep path))))))
               
               (setf rec-stack (map:assoc rec-stack node-name nil))
               (pop path)))
      
      ;; Check all nodes
      (map:each (lambda (name node)
                    (declare (ignore node))
                    (unless (map:get visited name)
                      (dfs name '())))
                  graph))
    
    cycles))

;;;; ==========================================================================
;;;; Topological Sort
;;;; ==========================================================================

(defun topological-sort (graph)
  "Perform topological sort on dependency graph.
   Returns list of node names in build order."
  (let ((in-degree (map:make-map))
        (queue '())
        (result '()))
    
    ;; Calculate in-degrees
    (map:each (lambda (name node)
                  (setf in-degree (map:assoc in-degree name 0))
                  (dolist (dep (dep-node-dependencies node))
                    (setf in-degree 
                          (map:assoc in-degree dep 
                                     (1+ (or (map:get in-degree dep) 0))))))
                graph)
    
    ;; Find nodes with no dependencies
    (map:each (lambda (name degree)
                  (when (zerop degree)
                    (push name queue)))
                in-degree)
    
    ;; Process queue
    (loop while queue
          do (let* ((current (pop queue))
                    (node (map:get graph current)))
               (push current result)
               
               ;; Decrease in-degree of dependents
               (when node
                 (dolist (dependent (dep-node-dependents node))
                   (let ((new-degree (1- (map:get in-degree dependent))))
                     (setf in-degree (map:assoc in-degree dependent new-degree))
                     (when (zerop new-degree)
                       (push dependent queue)))))))
    
    ;; Check if all nodes were processed
    (unless (= (length result) (map:size graph))
      (error "Circular dependency detected in module graph"))
    
    (reverse result)))

;;;; ==========================================================================
;;;; Dependency Resolution
;;;; ==========================================================================

(defun resolve-dependencies (modules &key include-optional)
  "Resolve dependencies for a set of modules.
   Returns an ordered list of modules to build."
  (declare (ignore include-optional)) ; TODO: implement optional deps
  
  (let* ((graph (build-dependency-graph modules))
         (cycles (check-circular-dependencies graph)))
    
    (when cycles
      (error "Circular dependencies found: ~{~A~^, ~}"
             (mapcar (lambda (cycle) 
                       (format nil "~{~A~^ -> ~}" cycle))
                     cycles)))
    
    (topological-sort graph)))

;;;; ==========================================================================
;;;; Build Order Computation
;;;; ==========================================================================

(defun compute-build-order (target-modules all-modules)
  "Compute build order for target modules including all dependencies.
   TARGET-MODULES is a list of module names to build.
   ALL-MODULES is a map of all available modules (name -> path).
   Returns ordered list of module names to build."
  
  ;; Collect transitive dependencies
  (let ((required-modules (map:make-map))
        (to-process target-modules))
    
    ;; Add target modules
    (dolist (module target-modules)
      (unless (map:get all-modules module)
        (error "Unknown module: ~A" module))
      (setf required-modules (map:assoc required-modules module 
                                       (map:get all-modules module))))
    
    ;; Process dependencies
    (loop while to-process
          do (let* ((current (pop to-process))
                    (path (map:get all-modules current)))
               (when path
                 (let* ((info (load-module-info path))
                        (deps (get-module-dependencies info)))
                   (dolist (dep deps)
                     (unless (map:get required-modules dep)
                       (let ((dep-path (map:get all-modules dep)))
                         (unless dep-path
                           (error "Missing dependency ~A for module ~A" dep current))
                         (setf required-modules (map:assoc required-modules dep dep-path))
                         (push dep to-process))))))))
    
    ;; Resolve build order
    (resolve-dependencies required-modules)))