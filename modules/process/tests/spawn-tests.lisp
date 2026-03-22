;;;; spawn-tests.lisp - Integration tests for epsilon.process.spawn
;;;;
;;;; Tests spawn-spec, process-handle, the spawn function, file actions
;;;; builder, O_CLOEXEC strategy, and process group management.

(defpackage epsilon.process.spawn-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process.spawn spawn)
            (epsilon.process.posix posix)
            (epsilon.file fs))
  (:local-nicknames (lib epsilon.foreign))
  (:enter t))

;;; ============================================================================
;;; Spawn Spec Construction Tests
;;; ============================================================================

(deftest test-spawn-spec-defaults ()
  "Test spawn-spec default values"
  (let ((spec (spawn:make-spawn-spec "echo")))
    (assert-equal "echo" (spawn:spawn-spec-program spec))
    (assert-equal '("echo") (spawn:spawn-spec-args spec))
    (assert-equal :inherit (spawn:spawn-spec-environment spec))
    (assert-true (null (spawn:spawn-spec-working-directory spec)))
    (assert-equal :inherit (spawn:spawn-spec-stdin-action spec))
    (assert-equal :inherit (spawn:spawn-spec-stdout-action spec))
    (assert-equal :inherit (spawn:spawn-spec-stderr-action spec))
    (assert-equal :inherit (spawn:spawn-spec-process-group spec))
    (assert-true (spawn:spawn-spec-cloexec-default spec))
    (assert-true (null (spawn:spawn-spec-keep-fds spec)))
    (assert-true (spawn:spawn-spec-search-path spec))))

(deftest test-spawn-spec-custom ()
  "Test spawn-spec with custom values"
  (let ((spec (spawn:make-spawn-spec "ls"
                :args '("ls" "-la" "/tmp")
                :working-directory "/tmp"
                :stdin :null
                :stdout :pipe
                :stderr :pipe
                :process-group :new-group)))
    (assert-equal "ls" (spawn:spawn-spec-program spec))
    (assert-equal '("ls" "-la" "/tmp") (spawn:spawn-spec-args spec))
    (assert-equal "/tmp" (spawn:spawn-spec-working-directory spec))
    (assert-equal :null (spawn:spawn-spec-stdin-action spec))
    (assert-equal :pipe (spawn:spawn-spec-stdout-action spec))
    (assert-equal :pipe (spawn:spawn-spec-stderr-action spec))
    (assert-equal :new-group (spawn:spawn-spec-process-group spec))))

;;; ============================================================================
;;; Validation Tests
;;; ============================================================================

(deftest test-spawn-spec-validates-program ()
  "Test that spawn validates program is provided"
  (handler-case
      (progn
        (spawn:spawn (spawn:make-spawn-spec nil))
        (assert-true nil "Should have raised spawn-error"))
    (spawn:spawn-error () (assert-true t))))

(deftest test-spawn-spec-validates-stdin ()
  "Test that spawn validates stdin action"
  (handler-case
      (progn
        (spawn:spawn (spawn:make-spawn-spec "true" :stdin :invalid))
        (assert-true nil "Should have raised spawn-error"))
    (spawn:spawn-error () (assert-true t))))

;;; ============================================================================
;;; Basic Spawn Tests
;;; ============================================================================

(deftest test-spawn-true ()
  "Test spawning 'true' and verifying exit code 0"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "true"))))
    (spawn:wait-for-handle handle)
    (assert-true (spawn:handle-exited-p handle))
    (assert-equal :exited (spawn:process-handle-status handle))
    (assert-equal 0 (spawn:process-handle-exit-code handle))))

(deftest test-spawn-false ()
  "Test spawning 'false' and verifying non-zero exit code"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "false"))))
    (spawn:wait-for-handle handle)
    (assert-true (spawn:handle-exited-p handle))
    (assert-equal :exited (spawn:process-handle-status handle))
    (assert-true (not (zerop (spawn:process-handle-exit-code handle))))))

(deftest test-spawn-exit-code ()
  "Test specific exit code"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "exit 42")))))
    (spawn:wait-for-handle handle)
    (assert-equal 42 (spawn:process-handle-exit-code handle))))

;;; ============================================================================
;;; Pipe I/O Tests
;;; ============================================================================

(deftest test-spawn-capture-stdout ()
  "Test spawning with stdout piped and reading output"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "echo"
                               :args '("echo" "hello from spawn")
                               :stdout :pipe))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (assert-true (search "hello from spawn" output))
             (assert-equal 0 (spawn:process-handle-exit-code handle))))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-capture-stderr ()
  "Test spawning with stderr piped"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "echo err-output >&2")
                               :stderr :pipe))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stderr)))
             (assert-true (search "err-output" output))))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-capture-both ()
  "Test capturing both stdout and stderr"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "echo out; echo err >&2")
                               :stdout :pipe
                               :stderr :pipe))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((stdout (spawn:read-handle-output handle :stream :stdout))
                 (stderr (spawn:read-handle-output handle :stream :stderr)))
             (assert-true (search "out" stdout))
             (assert-true (search "err" stderr))))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-stdin-pipe ()
  "Test writing to child's stdin"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "cat"
                               :stdin :pipe
                               :stdout :pipe))))
    (unwind-protect
         (progn
           ;; Write to child's stdin
           (let* ((input "piped input data")
                  (octets (sb-ext:string-to-octets input :external-format :utf-8)))
             (lib:with-foreign-memory ((buf (length octets)))
               (dotimes (i (length octets))
                 (setf (sb-sys:sap-ref-8 buf i) (aref octets i)))
               (posix:fd-write (spawn:process-handle-stdin-fd handle)
                               buf (length octets))))
           ;; Close stdin to signal EOF
           (posix:fd-close (spawn:process-handle-stdin-fd handle))
           (setf (spawn:process-handle-stdin-fd handle) nil)
           ;; Read output
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (assert-equal "piped input data" output)))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-null-stdin ()
  "Test stdin from /dev/null"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "cat"
                               :stdin :null
                               :stdout :pipe))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           ;; cat with /dev/null stdin should produce empty output
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (assert-equal "" output))
           (assert-equal 0 (spawn:process-handle-exit-code handle)))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-null-stdout ()
  "Test stdout to /dev/null"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "echo"
                               :args '("echo" "discarded")
                               :stdout :null))))
    (spawn:wait-for-handle handle)
    ;; No stdout fd on handle since it went to /dev/null
    (assert-true (null (spawn:process-handle-stdout-fd handle)))
    (assert-equal 0 (spawn:process-handle-exit-code handle))))

(deftest test-spawn-merge-stderr ()
  "Test merging stderr into stdout"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "echo out; echo err >&2")
                               :stdout :merge-stderr))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             ;; Both streams should appear in stdout
             (assert-true (search "out" output))
             (assert-true (search "err" output))))
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; Working Directory Tests
;;; ============================================================================

(deftest test-spawn-working-directory ()
  "Test spawning with a working directory"
  (fs:with-temp-dir (dir)
    (let ((handle (spawn:spawn (spawn:make-spawn-spec "pwd"
                                 :stdout :pipe
                                 :working-directory dir))))
      (unwind-protect
           (progn
             (spawn:wait-for-handle handle)
             (let ((output (spawn:read-handle-output handle :stream :stdout)))
               ;; pwd output should contain the temp dir name
               (let ((dir-name (fs:basename dir)))
                 (assert-true (search dir-name output)))))
        (spawn:handle-close-streams handle)))))

;;; ============================================================================
;;; Environment Tests
;;; ============================================================================

(deftest test-spawn-custom-environment ()
  "Test spawning with custom environment"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "echo $SPAWN_TEST_VAR")
                               :stdout :pipe
                               :environment (list "SPAWN_TEST_VAR=stage2_works"
                                                  (format nil "PATH=~A"
                                                          (sb-ext:posix-getenv "PATH")))))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (assert-true (search "stage2_works" output))))
      (spawn:handle-close-streams handle))))

(deftest test-spawn-inherit-environment ()
  "Test spawning with inherited environment"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sh"
                               :args '("sh" "-c" "echo $HOME")
                               :stdout :pipe
                               :environment :inherit))))
    (unwind-protect
         (progn
           (spawn:wait-for-handle handle)
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             ;; HOME should be set from inherited env
             (assert-true (> (length (string-trim '(#\Newline #\Space) output)) 0))))
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; Process Group Tests
;;; ============================================================================

(deftest test-spawn-new-process-group ()
  "Test spawning in a new process group"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sleep"
                               :args '("sleep" "10")
                               :process-group :new-group))))
    (unwind-protect
         (progn
           ;; Child's pgid should equal its pid
           (let ((pgid (posix:getpgid (spawn:process-handle-pid handle))))
             (assert-equal (spawn:process-handle-pid handle) pgid))
           ;; Handle should record the pgid
           (assert-equal (spawn:process-handle-pid handle)
                         (spawn:process-handle-pgid handle)))
      ;; Clean up
      (spawn:handle-terminate-gracefully handle)
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; Process Handle Management Tests
;;; ============================================================================

(deftest test-handle-poll ()
  "Test non-blocking status polling"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sleep"
                               :args '("sleep" "10")))))
    (unwind-protect
         (progn
           ;; Should still be running
           (assert-equal :running (spawn:handle-poll handle))
           (assert-true (not (spawn:handle-exited-p handle))))
      (spawn:handle-terminate-gracefully handle)
      (spawn:handle-close-streams handle))))

(deftest test-handle-kill ()
  "Test killing a process"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sleep"
                               :args '("sleep" "60")))))
    (unwind-protect
         (progn
           (spawn:handle-kill handle posix:+sigkill+)
           (spawn:wait-for-handle handle 5)
           (assert-true (spawn:handle-exited-p handle))
           (assert-equal :signaled (spawn:process-handle-status handle))
           (assert-equal posix:+sigkill+ (spawn:process-handle-term-signal handle)))
      (spawn:handle-close-streams handle))))

(deftest test-handle-terminate-gracefully ()
  "Test graceful termination (SIGTERM then SIGKILL)"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sleep"
                               :args '("sleep" "60")))))
    (unwind-protect
         (progn
           (spawn:handle-terminate-gracefully handle :timeout 2)
           (assert-true (spawn:handle-exited-p handle)))
      (spawn:handle-close-streams handle))))

(deftest test-handle-close-streams ()
  "Test that handle-close-streams cleans up fds"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "echo"
                               :args '("echo" "test")
                               :stdin :pipe
                               :stdout :pipe
                               :stderr :pipe))))
    (spawn:wait-for-handle handle)
    ;; Streams should be open
    (assert-true (not (null (spawn:process-handle-stdin-fd handle))))
    (assert-true (not (null (spawn:process-handle-stdout-fd handle))))
    (assert-true (not (null (spawn:process-handle-stderr-fd handle))))
    ;; Close them
    (spawn:handle-close-streams handle)
    ;; Now they should be nil
    (assert-true (null (spawn:process-handle-stdin-fd handle)))
    (assert-true (null (spawn:process-handle-stdout-fd handle)))
    (assert-true (null (spawn:process-handle-stderr-fd handle)))))

(deftest test-handle-wait-timeout ()
  "Test waiting with a timeout that expires"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "sleep"
                               :args '("sleep" "60")))))
    (unwind-protect
         (progn
           ;; Wait with a short timeout
           (spawn:wait-for-handle handle 0.5)
           ;; Should still be running (timeout expired)
           (assert-equal :running (spawn:process-handle-status handle)))
      (spawn:handle-terminate-gracefully handle)
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; O_CLOEXEC Verification Tests
;;; ============================================================================

#+linux
(deftest test-cloexec-no-leak ()
  "Test that O_CLOEXEC prevents fd leaks to children"
  ;; Create extra fds with O_CLOEXEC that should NOT be accessible in child.
  ;; We test by having the child attempt to write to the specific fd number;
  ;; if O_CLOEXEC worked, the fd is closed and the write fails.
  (posix:with-pipe (extra-r extra-w :cloexec t)
    ;; Test that the read end of extra pipe is not accessible in child
    (let* ((cmd (format nil "echo test >&~D 2>/dev/null; echo $?" extra-r))
           (handle (spawn:spawn (spawn:make-spawn-spec "sh"
                                  :args (list "sh" "-c" cmd)
                                  :stdout :pipe))))
      (unwind-protect
           (let ((output (spawn:read-handle-output handle :stream :stdout)))
             (spawn:wait-for-handle handle)
             ;; If fd was closed by O_CLOEXEC, echo >&N fails and $? is non-zero
             (let ((result (string-trim '(#\Space #\Newline) output)))
               (assert-true (string/= result "0")
                   (format nil "Read fd ~D leaked to child (write succeeded)" extra-r))))
        (spawn:handle-close-streams handle)))))

;;; ============================================================================
;;; pidfd Tests
;;; ============================================================================

#+linux
(deftest test-spawn-creates-pidfd ()
  "Test that spawn creates a pidfd when available"
  (when (posix:pidfd-available-p)
    (let ((handle (spawn:spawn (spawn:make-spawn-spec "true"))))
      (unwind-protect
           (progn
             (assert-true (not (null (spawn:process-handle-pidfd handle))))
             (assert-true (integerp (spawn:process-handle-pidfd handle)))
             (assert-true (>= (spawn:process-handle-pidfd handle) 0))
             (spawn:wait-for-handle handle))
        (spawn:handle-close-streams handle)))))

;;; ============================================================================
;;; Large Output Tests
;;; ============================================================================

(deftest test-spawn-large-output ()
  "Test capturing large output (>64KB pipe buffer)"
  (let ((handle (spawn:spawn (spawn:make-spawn-spec "seq"
                               :args '("seq" "1" "50000")
                               :stdout :pipe))))
    (unwind-protect
         (let ((output (spawn:read-handle-output handle :stream :stdout)))
           ;; Must read output BEFORE wait to avoid pipe buffer deadlock
           (spawn:wait-for-handle handle)
           ;; seq 1 50000 produces ~289KB of output, well beyond pipe buffer
           (assert-true (> (length output) 100000)
               (format nil "Expected large output (>100KB), got ~D chars" (length output))))
      (spawn:handle-close-streams handle))))

;;; ============================================================================
;;; File Redirection Tests
;;; ============================================================================

(deftest test-spawn-stdout-to-file ()
  "Test redirecting stdout to a file"
  (fs:with-temp-dir (dir)
    (let ((outfile (format nil "~A/output.txt" dir)))
      (let ((handle (spawn:spawn (spawn:make-spawn-spec "echo"
                                   :args '("echo" "file output test")
                                   :stdout outfile))))
        (spawn:wait-for-handle handle)
        (assert-equal 0 (spawn:process-handle-exit-code handle))
        ;; Read the file
        (let ((content (with-open-file (s outfile) (read-line s))))
          (assert-true (search "file output test" content)))))))

(deftest test-spawn-stdin-from-file ()
  "Test reading stdin from a file"
  (fs:with-temp-dir (dir)
    (let ((infile (format nil "~A/input.txt" dir)))
      ;; Write input file
      (with-open-file (s infile :direction :output)
        (write-string "file input data" s))
      ;; Spawn cat reading from file
      (let ((handle (spawn:spawn (spawn:make-spawn-spec "cat"
                                   :stdin infile
                                   :stdout :pipe))))
        (unwind-protect
             (progn
               (spawn:wait-for-handle handle)
               (let ((output (spawn:read-handle-output handle :stream :stdout)))
                 (assert-equal "file input data" output)))
          (spawn:handle-close-streams handle))))))
