;;;; Tests for epsilon.fs.watch -- filesystem watch facility

(defpackage epsilon.fs.watch.tests
  (:import cl
           epsilon.test
           (epsilon.fs.watch watch)
           (epsilon.file file)))

;;; Capture temp dir for test files
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *test-dir* (file:join-paths (file:get-cwd) ".epsilon/test-watch/")))

(defun ensure-test-dir ()
  (file:make-dirs *test-dir*)
  *test-dir*)

(defun cleanup-test-dir ()
  (when (file:exists-p *test-dir*)
    (file:walk-path *test-dir*
                    (lambda (p)
                      (when (file:file-p p)
                        (delete-file p)))
                    :recursive t)
    (handler-case (delete-file (pathname *test-dir*)) (error () nil))))

;;; ============================================================================
;;; Watcher lifecycle
;;; ============================================================================

(deftest test-watcher-create-close
  "Creating and closing a watcher does not error."
  (watch:with-watcher (w)
    (assert-not-null w "Watcher should be non-nil")))

(deftest test-watcher-add-file
  "Adding a file to watch returns T."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "watched.txt")))
           (file:write-file-string test-file "initial")
           (watch:with-watcher (w)
             (assert-true (watch:add w test-file)
                 "Should succeed watching a file")))
      (cleanup-test-dir))))

(deftest test-watcher-add-directory
  "Adding a directory to watch returns T."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (watch:with-watcher (w)
           (assert-true (watch:add w dir)
               "Should succeed watching a directory"))
      (cleanup-test-dir))))

(deftest test-watcher-add-nonexistent
  "Adding a nonexistent path returns NIL."
  (watch:with-watcher (w)
    (assert-not (watch:add w "/nonexistent/path/that/does/not/exist")
        "Should fail for nonexistent path")))

(deftest test-watcher-remove
  "Removing a watched path returns T."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "watched.txt")))
           (file:write-file-string test-file "initial")
           (watch:with-watcher (w)
             (watch:add w test-file)
             (assert-true (watch:remove-watch w test-file)
                 "Should succeed removing a watch")))
      (cleanup-test-dir))))

(deftest test-watcher-remove-unwatched
  "Removing an unwatched path returns NIL."
  (watch:with-watcher (w)
    (assert-not (watch:remove-watch w "/some/unwatched/path")
        "Should return NIL for unwatched path")))

;;; ============================================================================
;;; Event detection
;;; ============================================================================

(deftest test-watch-detects-modification
  "Watcher detects file modification."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "modify-me.txt")))
           (file:write-file-string test-file "original")
           (watch:with-watcher (w)
             (watch:add w test-file)
             ;; Modify the file
             (file:write-file-string test-file "modified content")
             ;; Poll with short timeout
             (let ((events (watch:poll w :timeout 0.5)))
               (assert-true (> (length events) 0)
                   "Should detect at least one event")
               (let ((ev (first events)))
                 (assert-true (member :modified (watch:event-types ev))
                     "Event should include :modified")))))
      (cleanup-test-dir))))

(deftest test-watch-detects-deletion
  "Watcher detects file deletion."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "delete-me.txt")))
           (file:write-file-string test-file "goodbye")
           (watch:with-watcher (w)
             (watch:add w test-file)
             ;; Delete the file
             (delete-file test-file)
             ;; Poll with short timeout
             (let ((events (watch:poll w :timeout 0.5)))
               (assert-true (> (length events) 0)
                   "Should detect at least one event")
               ;; Deletion of a directly watched file may produce multiple
               ;; events (IN_ATTRIB before IN_DELETE_SELF).  Check that at
               ;; least one event carries :deleted.
               (assert-true (some (lambda (ev)
                                    (member :deleted (watch:event-types ev)))
                                  events)
                   "Some event should include :deleted"))))
      (cleanup-test-dir))))

(deftest test-watch-event-path
  "Event path matches the watched file."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "path-check.txt")))
           (file:write-file-string test-file "check")
           (watch:with-watcher (w)
             (watch:add w test-file)
             (file:write-file-string test-file "updated")
             (let ((events (watch:poll w :timeout 0.5)))
               (when (> (length events) 0)
                 (assert-true (search "path-check.txt"
                                      (watch:event-path (first events)))
                     "Event path should contain the filename")))))
      (cleanup-test-dir))))

(deftest test-poll-no-events
  "Polling with no changes returns empty list."
  (let ((dir (ensure-test-dir)))
    (unwind-protect
         (let ((test-file (file:join-paths dir "quiet.txt")))
           (file:write-file-string test-file "nothing happens")
           (watch:with-watcher (w)
             (watch:add w test-file)
             ;; Don't modify anything
             (let ((events (watch:poll w :timeout 0.1)))
               (assert-= (length events) 0
                   "Should return empty list with no changes"))))
      (cleanup-test-dir))))
