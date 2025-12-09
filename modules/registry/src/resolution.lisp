;;;; epsilon.registry.resolution - Dependency resolution
;;;;
;;;; Provides dependency resolution with support for semantic versioning.

(in-package epsilon.registry)

;;; Version parsing and comparison

(defstruct semver
  "Semantic version representation."
  major minor patch prerelease build)

(defun parse-version (version-string)
  "Parse a semantic version string into a semver struct.
   Supports formats: 1.0.0, 1.0.0-alpha, 1.0.0+build"
  (let* ((plus-pos (position #\+ version-string))
         (build (when plus-pos (subseq version-string (1+ plus-pos))))
         (without-build (if plus-pos
                            (subseq version-string 0 plus-pos)
                            version-string))
         (dash-pos (position #\- without-build))
         (prerelease (when dash-pos (subseq without-build (1+ dash-pos))))
         (without-prerelease (if dash-pos
                                 (subseq without-build 0 dash-pos)
                                 without-build))
         (parts (seq:realize (str:split #\. without-prerelease))))
    (make-semver :major (parse-integer (first parts) :junk-allowed t)
                 :minor (parse-integer (second parts) :junk-allowed t)
                 :patch (parse-integer (third parts) :junk-allowed t)
                 :prerelease prerelease
                 :build build)))

(defun version< (v1 v2)
  "Compare two version strings. Returns T if V1 < V2.
   Follows semver spec: pre-release versions have lower precedence."
  (let ((sv1 (parse-version v1))
        (sv2 (parse-version v2)))
    (cond
      ;; Compare major.minor.patch first
      ((< (semver-major sv1) (semver-major sv2)) t)
      ((> (semver-major sv1) (semver-major sv2)) nil)
      ((< (semver-minor sv1) (semver-minor sv2)) t)
      ((> (semver-minor sv1) (semver-minor sv2)) nil)
      ((< (semver-patch sv1) (semver-patch sv2)) t)
      ((> (semver-patch sv1) (semver-patch sv2)) nil)
      ;; Same major.minor.patch - compare pre-release
      ;; A version with pre-release has lower precedence than one without
      ((and (semver-prerelease sv1) (not (semver-prerelease sv2))) t)
      ((and (not (semver-prerelease sv1)) (semver-prerelease sv2)) nil)
      ((and (semver-prerelease sv1) (semver-prerelease sv2))
       (prerelease< (semver-prerelease sv1) (semver-prerelease sv2)))
      (t nil))))

(defun prerelease< (pr1 pr2)
  "Compare two pre-release strings per semver spec.
   Identifiers are split by dots and compared left to right.
   Numeric identifiers are compared as integers.
   Alphanumeric identifiers are compared lexically.
   Numeric identifiers always have lower precedence than alphanumeric."
  (let ((parts1 (seq:realize (str:split #\. pr1)))
        (parts2 (seq:realize (str:split #\. pr2))))
    (loop for p1 in parts1
          for p2 in parts2
          do (let ((cmp (compare-prerelease-part p1 p2)))
               (unless (zerop cmp)
                 (return-from prerelease< (minusp cmp)))))
    ;; If all compared parts are equal, shorter one has lower precedence
    (< (length parts1) (length parts2))))

(defun compare-prerelease-part (p1 p2)
  "Compare two pre-release identifier parts. Returns -1, 0, or 1."
  (let ((n1 (parse-integer p1 :junk-allowed t))
        (n2 (parse-integer p2 :junk-allowed t)))
    (cond
      ;; Both numeric - compare as integers
      ((and n1 n2)
       (cond ((< n1 n2) -1)
             ((> n1 n2) 1)
             (t 0)))
      ;; Numeric has lower precedence than alphanumeric
      ((and n1 (not n2)) -1)
      ((and (not n1) n2) 1)
      ;; Both alphanumeric - compare lexically
      (t (cond ((string< p1 p2) -1)
               ((string> p1 p2) 1)
               (t 0))))))

(defun version= (v1 v2)
  "Check if two version strings are equal."
  (let ((sv1 (parse-version v1))
        (sv2 (parse-version v2)))
    (and (= (semver-major sv1) (semver-major sv2))
         (= (semver-minor sv1) (semver-minor sv2))
         (= (semver-patch sv1) (semver-patch sv2)))))

;;; Version constraint parsing

(defstruct version-constraint
  "A version constraint specification."
  operator version)

(defstruct compound-constraint
  "A compound constraint (all must be satisfied)."
  constraints)

(defun parse-constraint (constraint-string)
  "Parse a version constraint string.
   Supports: ^1.0.0, ~1.0.0, >=1.0.0, <2.0.0, =1.0.0, 1.0.0, *, latest
   Also supports compound constraints: >=1.0.0,<2.0.0 or >=1.0.0 <2.0.0"
  (let ((trimmed (string-trim '(#\Space #\Tab) constraint-string)))
    ;; Check for compound constraints (comma or space separated)
    (let ((parts (or (split-constraint-parts trimmed #\,)
                     (split-constraint-parts trimmed #\Space))))
      (if (> (length parts) 1)
          (make-compound-constraint
           :constraints (mapcar #'parse-single-constraint parts))
          (parse-single-constraint trimmed)))))

(defun split-constraint-parts (str separator)
  "Split constraint string by separator, returning NIL if only one part."
  (let ((parts (remove-if (lambda (s) (zerop (length s)))
                          (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                  (seq:realize (str:split separator str))))))
    (when (> (length parts) 1)
      parts)))

(defun parse-single-constraint (constraint-string)
  "Parse a single version constraint (not compound)."
  (cond
    ((or (string= constraint-string "*")
         (string-equal constraint-string "latest"))
     (make-version-constraint :operator :any :version nil))
    ((str:starts-with-p constraint-string "^")
     (make-version-constraint :operator :caret
                              :version (subseq constraint-string 1)))
    ((str:starts-with-p constraint-string "~")
     (make-version-constraint :operator :tilde
                              :version (subseq constraint-string 1)))
    ((str:starts-with-p constraint-string ">=")
     (make-version-constraint :operator :gte
                              :version (subseq constraint-string 2)))
    ((str:starts-with-p constraint-string "<=")
     (make-version-constraint :operator :lte
                              :version (subseq constraint-string 2)))
    ((str:starts-with-p constraint-string ">")
     (make-version-constraint :operator :gt
                              :version (subseq constraint-string 1)))
    ((str:starts-with-p constraint-string "<")
     (make-version-constraint :operator :lt
                              :version (subseq constraint-string 1)))
    ((str:starts-with-p constraint-string "=")
     (make-version-constraint :operator :eq
                              :version (subseq constraint-string 1)))
    (t
     (make-version-constraint :operator :eq
                              :version constraint-string))))

(defun version-satisfies-p (version constraint)
  "Check if VERSION satisfies CONSTRAINT.

   VERSION - Version string (e.g., '1.2.3')
   CONSTRAINT - Version constraint struct or compound-constraint"
  (etypecase constraint
    (compound-constraint
     ;; All constraints must be satisfied
     (every (lambda (c) (version-satisfies-p version c))
            (compound-constraint-constraints constraint)))
    (version-constraint
     (let ((cv (version-constraint-version constraint))
           (op (version-constraint-operator constraint)))
       (case op
         (:any t)  ; * or latest matches everything
         (:eq (version= version cv))
         (:gt (version< cv version))
         (:gte (or (version= version cv) (version< cv version)))
         (:lt (version< version cv))
         (:lte (or (version= version cv) (version< version cv)))
         (:caret (caret-compatible-p version cv))
         (:tilde (tilde-compatible-p version cv))
         (t nil))))))

(defun caret-compatible-p (version base)
  "Check caret (^) compatibility.
   ^1.2.3 allows >=1.2.3 <2.0.0
   ^0.2.3 allows >=0.2.3 <0.3.0"
  (let ((sv (parse-version version))
        (bv (parse-version base)))
    (cond
      ((> (semver-major bv) 0)
       (and (= (semver-major sv) (semver-major bv))
            (or (version= version base)
                (version< base version))))
      ((> (semver-minor bv) 0)
       (and (= (semver-major sv) 0)
            (= (semver-minor sv) (semver-minor bv))
            (or (version= version base)
                (version< base version))))
      (t
       (version= version base)))))

(defun tilde-compatible-p (version base)
  "Check tilde (~) compatibility.
   ~1.2.3 allows >=1.2.3 <1.3.0"
  (let ((sv (parse-version version))
        (bv (parse-version base)))
    (and (= (semver-major sv) (semver-major bv))
         (= (semver-minor sv) (semver-minor bv))
         (or (version= version base)
             (version< base version)))))

;;; Dependency resolution

(defun resolve-dependencies (package-definition)
  "Resolve all dependencies for a package definition.

   PACKAGE-DEFINITION - Map containing package info with 'dependencies' key

   Returns a map of package-name -> resolved-version."
  (let ((deps (map:get package-definition "dependencies"))
        (resolved (map:make-map)))
    (when deps
      (map:each (lambda (name constraint)
                  (let ((version (resolve-single-dependency name constraint)))
                    (setf resolved (map:assoc resolved name version))))
                deps))
    resolved))

(defun resolve-single-dependency (name constraint-string)
  "Resolve a single dependency to a specific version.

   NAME - Package name
   CONSTRAINT-STRING - Version constraint (e.g., '^1.0.0')

   Returns the best matching version string."
  (let* ((constraint (parse-constraint constraint-string))
         (available-versions (get-package-versions name))
         (matching (remove-if-not
                    (lambda (v) (version-satisfies-p v constraint))
                    available-versions)))
    (unless matching
      (error 'dependency-not-found
             :package name
             :constraint constraint-string))
    ;; Return highest matching version (sort descending, take first)
    (first (sort matching (lambda (a b) (version< b a))))))

(define-condition dependency-not-found (error)
  ((package :initarg :package :reader dependency-package)
   (constraint :initarg :constraint :reader dependency-constraint))
  (:report (lambda (condition stream)
             (format stream "No version of ~A satisfies constraint ~A"
                     (dependency-package condition)
                     (dependency-constraint condition)))))

;;; Conflict detection

(defun check-conflicts (resolved-dependencies)
  "Check for conflicts in resolved dependencies.

   RESOLVED-DEPENDENCIES - Map of package-name -> version

   Returns a list of conflict descriptions or NIL if no conflicts."
  (let ((conflicts nil))
    ;; For now, just verify all resolved versions are valid
    ;; Future: check transitive dependency compatibility
    (map:each (lambda (name version)
                (unless (get-package-info name)
                  (push (format nil "Unknown package: ~A" name) conflicts))
                (unless (member version (get-package-versions name) :test #'string=)
                  (push (format nil "Unknown version: ~A@~A" name version) conflicts)))
              resolved-dependencies)
    (nreverse conflicts)))
