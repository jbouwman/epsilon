(defpackage epsilon.web.routing
  (:use cl)
  (:local-nicknames
   (str epsilon.string)
   (seq epsilon.sequence)
   (map epsilon.map))
  (:export
   compile-route-pattern
   route-matches-p
   extract-route-params
   make-route
   find-handler
   route-pattern
   route-method
   route-handler
   route-constraints
   route-middleware
   route-raw-path))

(in-package epsilon.web.routing)

(defstruct route-pattern
  segments
  param-positions
  wildcard-position
  constraints
  min-segments
  max-segments)

(defstruct (route (:constructor make-route-internal))
  method
  pattern
  handler
  constraints
  middleware
  raw-path)

(defparameter *route-pattern-cache* (make-hash-table :test 'equal))

(defun compile-route-pattern (path &key (constraints map:+empty+))
  "Compile a route path pattern into segments and parameter positions"
  (let ((cache-key (list path constraints)))
    (or (gethash cache-key *route-pattern-cache*)
        (setf (gethash cache-key *route-pattern-cache*)
              (parse-route-pattern path constraints)))))

(defun parse-route-pattern (path constraints)
  "Parse route pattern string into compiled pattern"
  (let* ((segments-seq (str:split #\/ path))
         (segments (remove "" (seq:realize segments-seq) :test #'string=))
         (param-positions nil)
         (wildcard-position nil)
         (optional-count 0)
         (processed-segments nil))
    
    (loop for segment in segments
          for i from 0
          do (cond
               ((and (> (length segment) 0)
                     (char= (char segment 0) #\:))
                (let* ((optional-p (and (> (length segment) 1)
                                       (char= (char segment (1- (length segment))) #\?)))
                       (param-name (if optional-p
                                     (subseq segment 1 (1- (length segment)))
                                     (subseq segment 1))))
                  (push (list i param-name optional-p) param-positions)
                  (when optional-p
                    (incf optional-count))
                  (push (if optional-p
                          (concatenate 'string ":" param-name "?")
                          (concatenate 'string ":" param-name))
                        processed-segments)))
               
               ((and (> (length segment) 0)
                     (char= (char segment 0) #\*))
                (setf wildcard-position (cons i (subseq segment 1)))
                (push segment processed-segments)
                (return))
               
               (t (push segment processed-segments))))
    
    (let ((min-segs (- (length processed-segments) optional-count))
          (max-segs (if wildcard-position
                       most-positive-fixnum
                       (length processed-segments))))
      (make-route-pattern
       :segments (nreverse processed-segments)
       :param-positions (nreverse param-positions)
       :wildcard-position wildcard-position
       :constraints constraints
       :min-segments min-segs
       :max-segments max-segs))))

(defun route-matches-p (pattern path)
  "Check if a path matches a route pattern"
  (let* ((path-segments-seq (str:split #\/ path))
         (path-segments (remove "" (seq:realize path-segments-seq) :test #'string=))
         (pattern-segments (route-pattern-segments pattern))
         (wildcard-pos (route-pattern-wildcard-position pattern))
         (path-length (length path-segments))
         (min-segs (route-pattern-min-segments pattern))
         (max-segs (route-pattern-max-segments pattern)))
    
    (cond
      (wildcard-pos
       (and (>= path-length (car wildcard-pos))
            (segments-match-p pattern-segments path-segments 
                            (route-pattern-param-positions pattern)
                            (route-pattern-constraints pattern)
                            (car wildcard-pos))))
      
      ((and (>= path-length min-segs)
            (<= path-length max-segs))
       (segments-match-p pattern-segments path-segments
                       (route-pattern-param-positions pattern)
                       (route-pattern-constraints pattern)
                       nil))
      
      (t nil))))

(defun segments-match-p (pattern-segments path-segments param-positions constraints wildcard-pos)
  "Check if path segments match pattern segments"
  (loop for i from 0
        for pattern-seg in pattern-segments
        for path-seg = (nth i path-segments)
        while (or path-seg 
                  (and (< i (length pattern-segments))
                       (find-if (lambda (p) (and (= (first p) i) (third p))) 
                               param-positions)))
        do (cond
             ((and wildcard-pos (= i wildcard-pos))
              (return t))
             
             ((find-if (lambda (p) (= (first p) i)) param-positions)
              (when path-seg
                (let* ((param-info (find-if (lambda (p) (= (first p) i)) param-positions))
                       (param-name (second param-info))
                       (constraint (map:get constraints param-name)))
                  (when (and constraint
                            (not (constraint-matches-p constraint path-seg)))
                    (return nil)))))
             
             ((and path-seg (not (string= pattern-seg path-seg)))
              (return nil))
             
             ((and (not path-seg) 
                   (not (find-if (lambda (p) (and (= (first p) i) (third p))) 
                                param-positions)))
              (return nil)))
        finally (return t)))

(defun constraint-matches-p (constraint value)
  "Check if value matches constraint pattern"
  (cond
    ((string= constraint "\\d+")
     (every #'digit-char-p value))
    ((string= constraint "\\d{4}")
     (and (= (length value) 4)
          (every #'digit-char-p value)))
    ((string= constraint "\\d{2}")
     (and (= (length value) 2)
          (every #'digit-char-p value)))
    (t t)))

(defun extract-route-params (pattern path)
  "Extract parameters from a path using a route pattern"
  (let* ((path-segments-seq (str:split #\/ path))
         (path-segments (remove "" (seq:realize path-segments-seq) :test #'string=))
         (param-positions (route-pattern-param-positions pattern))
         (wildcard-pos (route-pattern-wildcard-position pattern))
         (params map:+empty+))
    
    (dolist (param-info param-positions)
      (let ((i (first param-info))
            (name (second param-info))
            (optional-p (third param-info)))
        (if (< i (length path-segments))
            (setf params (map:assoc params name (nth i path-segments)))
            (when (not optional-p)
              (error "Required parameter ~A missing" name)))))
    
    (when wildcard-pos
      (let ((wildcard-segments (nthcdr (car wildcard-pos) path-segments)))
        (when wildcard-segments
          (setf params (map:assoc params 
                                (cdr wildcard-pos)
                                (format nil "~{~A~^/~}" wildcard-segments))))))
    
    params))

(defun find-handler (routes method path)
  "Find the handler for a given method and path"
  (loop for route in routes
        when (and (or (string= (route-method route) method)
                     (string= (route-method route) "ANY"))
                  (route-matches-p (route-pattern route) path))
        return (values (route-handler route)
                      (extract-route-params (route-pattern route) path)
                      route)))

(defun make-route (method path handler &key (constraints map:+empty+) middleware)
  "Create a route with compiled pattern"
  (make-route-internal
   :method (string-upcase (string method))
   :pattern (compile-route-pattern path :constraints constraints)
   :handler handler
   :constraints constraints
   :middleware middleware
   :raw-path path))