(defpackage :epsilon.package.dependency
  (:use
   :cl)
  (:local-nicknames
   (:map :epsilon.map)
   (:seq :epsilon.sequence)
   (:str :epsilon.string)
   (:fs :epsilon.sys.fs)
   (:uri :epsilon.uri)
   (:edn :epsilon.edn))
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
   :check-circular-dependencies
   
   ;; Version parsing and resolution
   :parse-version-spec
   :version-satisfies-p
   :compare-versions
   :resolve-version-spec))

(in-package :epsilon.package.dependency)

;;;; ==========================================================================
;;;; Module Information Loading
;;;; ==========================================================================

(defun load-module-info (module-path)
  "Load module information from package.lisp"
  (let ((package-file (uri:merge module-path "package.lisp")))
    (unless (fs:exists-p package-file)
      (error "No package.lisp found at ~A" module-path))
    (with-open-file (stream (uri:path package-file))
      (read stream))))

(defun get-module-dependencies (module-info)
  "Extract dependencies from module info"
  (or (getf module-info :dependencies) '()))

(defun get-module-provides (module-info)
  "Extract provided packages from module info"
  (or (getf module-info :provides) '()))

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

;;;; ==========================================================================
;;;; Version Parsing and Resolution
;;;; ==========================================================================

(defun parse-version-spec (spec-string)
  "Parse a version specification string into a structured format.
   Examples:
   - '1.0.0' -> exact version
   - '>= 1.0.0' -> minimum version (inclusive)
   - '> 1.0.0, < 2.0.0' -> range
   - '~> 1.0.0' -> compatible version (>= 1.0.0, < 1.1.0)
   - '@abc123' -> specific commit
   - 'main' or 'v1.0.0' -> git ref/tag"
  (let ((spec (str:trim spec-string)))
    (cond
      ;; Commit hash
      ((str:starts-with-p spec "@")
       (map:make-map :type :commit
                     :commit (subseq spec 1)))
      
      ;; Git ref (no version operators)
      ((and (not (find-if (lambda (c) (member c '(#\< #\> #\= #\~ #\^))) spec))
            (or (str:starts-with-p spec "v")
                (str:contains-p spec "/")
                (member spec '("main" "master" "develop") :test #'string=)))
       (map:make-map :type :commit
                     :commit spec))
      
      ;; Compatible version ~> 1.0.0
      ((str:starts-with-p spec "~>")
       (let ((version (str:trim (subseq spec 2))))
         (map:make-map :type :range
                       :min-version version
                       :max-version (increment-minor-version version)
                       :include-min t
                       :include-max nil)))
      
      ;; Range specifications
      ((str:contains-p spec ",")
       (parse-version-range spec))
      
      ;; Single constraint
      ((find-if (lambda (c) (member c '(#\< #\> #\=))) spec)
       (parse-single-constraint spec))
      
      ;; Exact version
      (t
       (map:make-map :type :exact
                     :version spec)))))

(defun parse-version-range (spec)
  "Parse a version range like '>= 1.0.0, < 2.0.0'"
  (let ((constraints (mapcar #'str:trim (str:split #\, spec)))
        (min-version nil)
        (max-version nil)
        (include-min t)
        (include-max t))
    
    (dolist (constraint constraints)
      (let ((parsed (parse-single-constraint constraint)))
        (case (map:get parsed :type)
          (:min-inclusive
           (setf min-version (map:get parsed :version)
                 include-min t))
          (:min-exclusive
           (setf min-version (map:get parsed :version)
                 include-min nil))
          (:max-inclusive
           (setf max-version (map:get parsed :version)
                 include-max t))
          (:max-exclusive
           (setf max-version (map:get parsed :version)
                 include-max nil)))))
    
    (map:make-map :type :range
                  :min-version min-version
                  :max-version max-version
                  :include-min include-min
                  :include-max include-max)))

(defun parse-single-constraint (constraint)
  "Parse a single version constraint like '>= 1.0.0'"
  (let ((constraint (str:trim constraint)))
    (cond
      ((str:starts-with-p constraint ">=")
       (map:make-map :type :min-inclusive
                     :version (str:trim (subseq constraint 2))))
      ((str:starts-with-p constraint ">")
       (map:make-map :type :min-exclusive
                     :version (str:trim (subseq constraint 1))))
      ((str:starts-with-p constraint "<=")
       (map:make-map :type :max-inclusive
                     :version (str:trim (subseq constraint 2))))
      ((str:starts-with-p constraint "<")
       (map:make-map :type :max-exclusive
                     :version (str:trim (subseq constraint 1))))
      ((str:starts-with-p constraint "=")
       (map:make-map :type :exact
                     :version (str:trim (subseq constraint 1))))
      (t
       (map:make-map :type :exact
                     :version constraint)))))

(defun compare-versions (v1 v2)
  "Compare two semantic versions. Returns :less, :equal, or :greater"
  (let ((parts1 (parse-version-parts v1))
        (parts2 (parse-version-parts v2)))
    (compare-version-parts parts1 parts2)))

(defun parse-version-parts (version)
  "Parse version string into numeric parts"
  (mapcar (lambda (part)
            (parse-integer part :junk-allowed t))
          (str:split #\. (remove-if-not (lambda (c)
                                          (or (digit-char-p c)
                                              (char= c #\.)))
                                        version))))

(defun compare-version-parts (parts1 parts2)
  "Compare two lists of version parts"
  (let ((max-len (max (length parts1) (length parts2))))
    ;; Pad shorter list with zeros
    (setf parts1 (append parts1 (make-list (- max-len (length parts1)) :initial-element 0)))
    (setf parts2 (append parts2 (make-list (- max-len (length parts2)) :initial-element 0)))
    
    (loop for p1 in parts1
          for p2 in parts2
          when (< p1 p2) return :less
          when (> p1 p2) return :greater
          finally (return :equal))))

(defun version-satisfies-p (version spec)
  "Check if a version satisfies a version specification"
  (let ((spec-type (map:get spec :type)))
    (case spec-type
      (:exact
       (string= version (map:get spec :version)))
      (:commit
       ;; For commit specs, we'd need additional metadata to resolve
       nil)
      (:range
       (and (or (null (map:get spec :min-version))
                (let ((cmp (compare-versions version (map:get spec :min-version))))
                  (or (eq cmp :greater)
                      (and (eq cmp :equal) (map:get spec :include-min)))))
            (or (null (map:get spec :max-version))
                (let ((cmp (compare-versions version (map:get spec :max-version))))
                  (or (eq cmp :less)
                      (and (eq cmp :equal) (map:get spec :include-max)))))))
      (t nil))))

(defun increment-minor-version (version)
  "Increment the minor version number for compatible version ranges"
  (let ((parts (parse-version-parts version)))
    (when (>= (length parts) 2)
      (setf (second parts) (1+ (second parts)))
      (when (>= (length parts) 3)
        (setf (third parts) 0))
      (format nil "~{~A~^.~}" parts))))

(defun resolve-version-spec (versions version-spec platform)
  "Resolve a version specification against available versions"
  (declare (ignore platform)) ; TODO: add platform filtering
  (let ((spec (parse-version-spec version-spec))
        (candidates '()))
    
    ;; Find all versions that satisfy the spec
    (map:each (lambda (version version-data)
                (declare (ignore version-data))
                (when (version-satisfies-p version spec)
                  (push version candidates)))
              versions)
    
    ;; Return the highest satisfying version
    (when candidates
      (first (sort candidates (lambda (a b)
                               (eq :greater (compare-versions a b))))))))
