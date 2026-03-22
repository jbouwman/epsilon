;;;; Subtests - Hierarchical test organization within a single test
;;;;
;;;; Provides Go-style t.Run() functionality for epsilon.test

(defpackage epsilon.test.subtest
  (:use :cl)
  (:require (epsilon.map map)
            (epsilon.sequence seq)
            (epsilon.string str))
  (:enter t))

;;; Subtest Result Structure

(defclass subtest-result ()
  ((name :initarg :name
         :reader subtest-name
         :documentation "Name of this subtest")
   (path :initarg :path
         :reader subtest-path
         :documentation "Full path from root (list of names)")
   (status :initform :pass
           :accessor subtest-status
           :documentation "Status: :pass, :fail, :error, :skip")
   (assertions :initform nil
               :accessor subtest-assertions
               :documentation "Assertions within this subtest")
   (children :initform nil
             :accessor subtest-children
             :documentation "Child subtest results")
   (error-info :initform nil
               :accessor subtest-error-info
               :documentation "Error details if status is :fail or :error")
   (start-time :initform nil
               :accessor subtest-start-time)
   (end-time :initform nil
             :accessor subtest-end-time)))

(defun make-subtest-result (name path)
  "Create a new subtest result"
  (make-instance 'subtest-result :name name :path path))

(defun subtest-elapsed-time (result)
  "Return elapsed time in seconds"
  (when (and (subtest-start-time result) (subtest-end-time result))
    (/ (- (subtest-end-time result) (subtest-start-time result))
       internal-time-units-per-second)))

;;; Subtest Context

(defvar *current-subtest* nil
  "Currently executing subtest result, or nil if at top level")

(defvar *subtest-results* nil
  "List of top-level subtest results for current test")

(defvar *subtest-path* nil
  "Current path in subtest hierarchy (list of names)")

;;; Subtest Macro

(defmacro subtest (name &body body)
  "Execute BODY as a named subtest within the current test.
   Subtests can be nested to create hierarchical test structures.

   Example:
   (deftest test-user-operations
     (subtest \"create\"
       (subtest \"with valid data\"
         (assert-not-null (create-user :name \"Alice\")))
       (subtest \"with invalid data\"
         (assert-condition (validation-error) (create-user)))))

   Each subtest:
   - Has its own pass/fail status
   - Can contain nested subtests
   - Reports timing independently
   - Can be filtered by path pattern"
  (let ((result-var (gensym "SUBTEST-RESULT"))
        (name-var (gensym "NAME"))
        (path-var (gensym "PATH")))
    `(let* ((,name-var ,name)
            (,path-var (append *subtest-path* (list ,name-var)))
            (,result-var (make-subtest-result ,name-var ,path-var)))
       (setf (subtest-start-time ,result-var) (get-internal-real-time))
       ;; Register this subtest with parent or top-level
       (if *current-subtest*
           (push ,result-var (subtest-children *current-subtest*))
           (push ,result-var *subtest-results*))
       ;; Execute body with this subtest as context
       (let ((*current-subtest* ,result-var)
             (*subtest-path* ,path-var))
         (handler-case
             (progn ,@body)
           (error (e)
             (setf (subtest-status ,result-var) :error)
             (setf (subtest-error-info ,result-var)
                   (format nil "~A" e)))))
       (setf (subtest-end-time ,result-var) (get-internal-real-time))
       ;; Determine final status based on children
       (update-subtest-status ,result-var)
       ,result-var)))

(defun update-subtest-status (result)
  "Update subtest status based on its assertions and children"
  (let ((current-status (subtest-status result)))
    ;; If already error, keep it
    (when (eq current-status :error)
      (return-from update-subtest-status))
    ;; Check children for failures/errors
    (dolist (child (subtest-children result))
      (case (subtest-status child)
        (:error (setf (subtest-status result) :error)
                (return-from update-subtest-status))
        (:fail (setf (subtest-status result) :fail))
        (:skip (when (eq (subtest-status result) :pass)
                 (setf (subtest-status result) :skip)))))
    ;; Check assertions
    (dolist (assertion (subtest-assertions result))
      (unless (first assertion)
        (setf (subtest-status result) :fail)
        (return-from update-subtest-status)))))

;;; Integration with epsilon.test.suite

(defun record-subtest-assertion (passed-p report-fn)
  "Record an assertion in the current subtest context"
  (when *current-subtest*
    (push (list passed-p report-fn) (subtest-assertions *current-subtest*)))
  passed-p)

(defun get-subtest-results ()
  "Get all subtest results for the current test"
  (reverse *subtest-results*))

(defun clear-subtest-context ()
  "Clear subtest context for a new test"
  (setf *current-subtest* nil
        *subtest-results* nil
        *subtest-path* nil))

(defun in-subtest-p ()
  "Return T if currently executing within a subtest"
  (not (null *current-subtest*)))

;;; Subtest Counting and Status

(defun count-subtests (results)
  "Count total subtests including nested ones"
  (let ((count 0))
    (labels ((count-recursive (r)
               (incf count)
               (dolist (child (subtest-children r))
                 (count-recursive child))))
      (dolist (r results)
        (count-recursive r)))
    count))

(defun count-subtest-failures (results)
  "Count failed subtests including nested ones"
  (let ((count 0))
    (labels ((count-recursive (r)
               (when (member (subtest-status r) '(:fail :error))
                 (incf count))
               (dolist (child (subtest-children r))
                 (count-recursive child))))
      (dolist (r results)
        (count-recursive r)))
    count))

;;; Subtest Path Matching

(defun subtest-path-matches-p (pattern path)
  "Check if PATH matches PATTERN.
   PATTERN can contain * wildcards and / separators.
   Example: \"create/*\" matches \"create/valid\", \"create/invalid\""
  (let ((pattern-parts (seq:realize (str:split #\/ pattern)))
        (path-parts path))
    (subtest-path-parts-match-p pattern-parts path-parts)))

(defun subtest-path-parts-match-p (pattern-parts path-parts)
  "Match pattern parts against path parts"
  (cond
    ;; Empty pattern matches anything
    ((null pattern-parts) t)
    ;; Pattern left but no path - no match
    ((null path-parts) nil)
    ;; Check current part
    (t (let ((pattern (first pattern-parts))
             (path (first path-parts)))
         (when (or (string= pattern "*")
                   (string= pattern path)
                   (and (str:contains-p pattern "*")
                        (wildcard-name-match-p pattern path)))
           (subtest-path-parts-match-p (rest pattern-parts)
                                       (rest path-parts)))))))

(defun wildcard-name-match-p (pattern name)
  "Match a single name against a pattern with wildcards"
  (let ((parts (seq:realize (str:split #\* pattern))))
    (and (str:starts-with-p name (first parts))
         (str:ends-with-p name (second parts)))))

;;; Format Subtest Results

(defun format-subtest-tree (results &optional (indent 0) (stream *standard-output*))
  "Format subtest results as a tree"
  (dolist (r (reverse results))
    (format-subtest-entry r indent stream)
    (format-subtest-tree (subtest-children r) (+ indent 2) stream)))

(defun format-subtest-entry (result indent stream)
  "Format a single subtest result"
  (let* ((name (subtest-name result))
         (status (subtest-status result))
         (elapsed (subtest-elapsed-time result))
         (status-str (case status
                       (:pass "[PASS]")
                       (:fail "[FAIL]")
                       (:error "[ERROR]")
                       (:skip "[SKIP]"))))
    (format stream "~&~V@T~A ~40T ~A~@[ ~,3Fs~]~%"
            indent name status-str elapsed)
    (when (subtest-error-info result)
      (format stream "~&~V@T  Error: ~A~%" indent (subtest-error-info result)))))
