;;;; posix-tests.lisp - Tests for epsilon.process.posix FFI bindings
;;;;
;;;; Validates raw POSIX subprocess primitives: posix_spawn, pipe2,
;;;; waitpid, process groups, and supporting infrastructure.

(defpackage epsilon.process.posix-test
  (:use :cl :epsilon.test :epsilon.syntax)
  (:require (epsilon.process.posix posix))
  (:local-nicknames (lib epsilon.foreign))
  (:enter t))

;;; ============================================================================
;;; waitpid Status Macro Tests
;;; ============================================================================

(deftest test-wifexited-normal-exit ()
  "Test WIFEXITED for normal exit status"
  ;; A normal exit with code 0 produces status 0x0000
  (assert-true (posix:wifexited #x0000))
  ;; A normal exit with code 1 produces status 0x0100
  (assert-true (posix:wifexited #x0100))
  ;; A normal exit with code 42 produces status 0x2A00
  (assert-true (posix:wifexited #x2A00)))

(deftest test-wexitstatus ()
  "Test WEXITSTATUS extracts correct exit code"
  (assert-equal 0 (posix:wexitstatus #x0000))
  (assert-equal 1 (posix:wexitstatus #x0100))
  (assert-equal 42 (posix:wexitstatus #x2A00))
  (assert-equal 255 (posix:wexitstatus #xFF00)))

(deftest test-wifsignaled ()
  "Test WIFSIGNALED for signal termination"
  ;; Killed by SIGTERM (15) produces status 0x000F
  (assert-true (posix:wifsignaled #x000F))
  ;; Killed by SIGKILL (9) produces status 0x0009
  (assert-true (posix:wifsignaled #x0009))
  ;; Normal exit is NOT signaled
  (assert-true (not (posix:wifsignaled #x0000)))
  (assert-true (not (posix:wifsignaled #x0100))))

(deftest test-wtermsig ()
  "Test WTERMSIG extracts correct signal number"
  (assert-equal 15 (posix:wtermsig #x000F))
  (assert-equal 9 (posix:wtermsig #x0009))
  (assert-equal 2 (posix:wtermsig #x0002)))

;;; ============================================================================
;;; Pipe Tests
;;; ============================================================================

(deftest test-make-pipe ()
  "Test basic pipe creation"
  (multiple-value-bind (read-fd write-fd) (posix:make-pipe)
    (unwind-protect
         (progn
           (assert-true (integerp read-fd))
           (assert-true (integerp write-fd))
           (assert-true (>= read-fd 0))
           (assert-true (>= write-fd 0))
           (assert-true (/= read-fd write-fd)))
      (posix:fd-close read-fd)
      (posix:fd-close write-fd))))

(deftest test-pipe-read-write ()
  "Test reading and writing through a pipe"
  (posix:with-pipe (read-fd write-fd)
    (let* ((message "hello pipe")
           (octets (sb-ext:string-to-octets message :external-format :utf-8))
           (len (length octets)))
      ;; Write message to pipe
      (lib:with-foreign-memory ((write-buf (1+ len)))
        (dotimes (i len)
          (setf (sb-sys:sap-ref-8 write-buf i) (aref octets i)))
        (let ((written (posix:fd-write write-fd write-buf len)))
          (assert-equal len written)))
      ;; Read message from pipe
      (lib:with-foreign-memory ((read-buf 256))
        (let ((bytes-read (posix:fd-read read-fd read-buf 256)))
          (assert-equal len bytes-read)
          (let ((result (sb-ext:octets-to-string
                         (let ((v (make-array bytes-read :element-type '(unsigned-byte 8))))
                           (dotimes (i bytes-read v)
                             (setf (aref v i) (sb-sys:sap-ref-8 read-buf i))))
                         :external-format :utf-8)))
            (assert-equal message result)))))))

#+linux
(deftest test-pipe2-cloexec ()
  "Test pipe creation with O_CLOEXEC (Linux)"
  (multiple-value-bind (read-fd write-fd) (posix:make-pipe-cloexec)
    (unwind-protect
         (progn
           (assert-true (integerp read-fd))
           (assert-true (integerp write-fd))
           (assert-true (>= read-fd 0))
           (assert-true (>= write-fd 0)))
      (posix:fd-close read-fd)
      (posix:fd-close write-fd))))

;;; ============================================================================
;;; posix_spawn Tests
;;; ============================================================================

(deftest test-spawn-true ()
  "Test spawning /bin/true (or 'true' via PATH) and verifying exit code 0"
  (let ((pid (posix:call-posix-spawnp "true" '("true"))))
    (assert-true (integerp pid))
    (assert-true (> pid 0))
    ;; Wait for child
    (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
      (assert-equal pid waited-pid)
      (assert-true (posix:wifexited status))
      (assert-equal 0 (posix:wexitstatus status)))))

(deftest test-spawn-false ()
  "Test spawning /bin/false and verifying non-zero exit code"
  (let ((pid (posix:call-posix-spawnp "false" '("false"))))
    (assert-true (integerp pid))
    (assert-true (> pid 0))
    (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
      (assert-equal pid waited-pid)
      (assert-true (posix:wifexited status))
      (assert-true (not (zerop (posix:wexitstatus status)))))))

(deftest test-spawn-with-args ()
  "Test spawning a process with arguments"
  ;; Use sh -c 'exit 42' to test specific exit code
  (let ((pid (posix:call-posix-spawnp "sh" '("sh" "-c" "exit 42"))))
    (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
      (assert-equal pid waited-pid)
      (assert-true (posix:wifexited status))
      (assert-equal 42 (posix:wexitstatus status)))))

(deftest test-spawn-capture-stdout ()
  "Test spawning a process and capturing its stdout via pipe"
  (posix:with-pipe (read-fd write-fd :cloexec t)
    ;; Set up file actions: dup2 write-fd to stdout (fd 1), close read-fd
    (posix:with-file-actions (fa)
      (posix:file-actions-adddup2 fa write-fd 1)
      (posix:file-actions-addclose fa read-fd)
      (let ((pid (posix:call-posix-spawnp
                  "echo" '("echo" "hello world")
                  :file-actions-sap fa)))
        ;; Close the write end in the parent
        (posix:fd-close write-fd)
        ;; Read from the pipe
        (lib:with-foreign-memory ((buf 256))
          (let ((n (posix:fd-read read-fd buf 256)))
            (assert-true (> n 0))
            (let ((output (sb-ext:octets-to-string
                           (let ((v (make-array n :element-type '(unsigned-byte 8))))
                             (dotimes (i n v)
                               (setf (aref v i) (sb-sys:sap-ref-8 buf i))))
                           :external-format :utf-8)))
              (assert-true (search "hello world" output)))))
        ;; Wait for child
        (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
          (declare (ignore waited-pid))
          (assert-true (posix:wifexited status))
          (assert-equal 0 (posix:wexitstatus status)))))))

(deftest test-spawn-capture-stderr ()
  "Test capturing stderr from a spawned process"
  (posix:with-pipe (read-fd write-fd :cloexec t)
    (posix:with-file-actions (fa)
      ;; Redirect stderr (fd 2) to the write end of our pipe
      (posix:file-actions-adddup2 fa write-fd 2)
      (posix:file-actions-addclose fa read-fd)
      (let ((pid (posix:call-posix-spawnp
                  "sh" '("sh" "-c" "echo error-msg >&2")
                  :file-actions-sap fa)))
        (posix:fd-close write-fd)
        (lib:with-foreign-memory ((buf 256))
          (let ((n (posix:fd-read read-fd buf 256)))
            (assert-true (> n 0))
            (let ((output (sb-ext:octets-to-string
                           (let ((v (make-array n :element-type '(unsigned-byte 8))))
                             (dotimes (i n v)
                               (setf (aref v i) (sb-sys:sap-ref-8 buf i))))
                           :external-format :utf-8)))
              (assert-true (search "error-msg" output)))))
        (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
          (declare (ignore waited-pid))
          (assert-true (posix:wifexited status)))))))

(deftest test-spawn-redirect-stdin ()
  "Test providing stdin to a spawned process"
  (posix:with-pipe (stdin-read stdin-write :cloexec t)
    (posix:with-pipe (stdout-read stdout-write :cloexec t)
      (posix:with-file-actions (fa)
        ;; Child reads from stdin-read, writes to stdout-write
        (posix:file-actions-adddup2 fa stdin-read 0)
        (posix:file-actions-adddup2 fa stdout-write 1)
        (posix:file-actions-addclose fa stdin-write)
        (posix:file-actions-addclose fa stdout-read)
        ;; cat copies stdin to stdout
        (let ((pid (posix:call-posix-spawnp
                    "cat" '("cat")
                    :file-actions-sap fa)))
          ;; Close child-side fds in parent
          (posix:fd-close stdin-read)
          (posix:fd-close stdout-write)
          ;; Write to child's stdin
          (let* ((input "test input")
                 (octets (sb-ext:string-to-octets input :external-format :utf-8)))
            (lib:with-foreign-memory ((write-buf (length octets)))
              (dotimes (i (length octets))
                (setf (sb-sys:sap-ref-8 write-buf i) (aref octets i)))
              (posix:fd-write stdin-write write-buf (length octets))))
          ;; Close stdin to signal EOF to cat
          (posix:fd-close stdin-write)
          ;; Read from child's stdout
          (lib:with-foreign-memory ((buf 256))
            (let ((n (posix:fd-read stdout-read buf 256)))
              (assert-true (> n 0))
              (let ((output (sb-ext:octets-to-string
                             (let ((v (make-array n :element-type '(unsigned-byte 8))))
                               (dotimes (i n v)
                                 (setf (aref v i) (sb-sys:sap-ref-8 buf i))))
                             :external-format :utf-8)))
                (assert-equal "test input" output))))
          (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
            (declare (ignore waited-pid))
            (assert-true (posix:wifexited status))
            (assert-equal 0 (posix:wexitstatus status))))))))

(deftest test-spawn-with-environment ()
  "Test spawning a process with custom environment"
  (posix:with-pipe (read-fd write-fd :cloexec t)
    (posix:with-file-actions (fa)
      (posix:file-actions-adddup2 fa write-fd 1)
      (posix:file-actions-addclose fa read-fd)
      (let ((pid (posix:call-posix-spawnp
                  "sh" '("sh" "-c" "echo $MY_TEST_VAR")
                  :environment '("MY_TEST_VAR=posix_spawn_works"
                                 "PATH=/usr/bin:/bin:/usr/local/bin")
                  :file-actions-sap fa)))
        (posix:fd-close write-fd)
        (lib:with-foreign-memory ((buf 256))
          (let ((n (posix:fd-read read-fd buf 256)))
            (assert-true (> n 0))
            (let ((output (sb-ext:octets-to-string
                           (let ((v (make-array n :element-type '(unsigned-byte 8))))
                             (dotimes (i n v)
                               (setf (aref v i) (sb-sys:sap-ref-8 buf i))))
                           :external-format :utf-8)))
              (assert-true (search "posix_spawn_works" output)))))
        (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
          (declare (ignore waited-pid))
          (assert-true (posix:wifexited status)))))))

;;; ============================================================================
;;; waitpid Tests
;;; ============================================================================

(deftest test-waitpid-blocking ()
  "Test blocking waitpid"
  (let ((pid (posix:call-posix-spawnp "true" '("true"))))
    (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
      (assert-equal pid waited-pid)
      (assert-true (posix:wifexited status))
      (assert-equal 0 (posix:wexitstatus status)))))

(deftest test-waitpid-wnohang ()
  "Test non-blocking waitpid with WNOHANG"
  ;; Spawn a long-running process
  (let ((pid (posix:call-posix-spawnp "sleep" '("sleep" "10"))))
    (unwind-protect
         (progn
           ;; WNOHANG should return 0 (child not yet exited)
           (multiple-value-bind (waited-pid status) (posix:waitpid pid posix:+wnohang+)
             (declare (ignore status))
             (assert-equal 0 waited-pid)))
      ;; Kill the child to clean up
      (sb-posix:kill pid posix:+sigkill+)
      (posix:waitpid pid 0))))

;;; ============================================================================
;;; Spawn Attribute Tests
;;; ============================================================================

(deftest test-spawnattr-flags ()
  "Test setting and getting spawn attribute flags"
  (posix:with-spawnattr (attr)
    ;; Initially flags should be 0
    (assert-equal 0 (posix:spawnattr-getflags attr))
    ;; Set SETPGROUP flag
    (posix:spawnattr-setflags attr posix:+posix-spawn-setpgroup+)
    (assert-equal posix:+posix-spawn-setpgroup+
                  (posix:spawnattr-getflags attr))
    ;; Set multiple flags
    (posix:spawnattr-setflags attr
                              (logior posix:+posix-spawn-setpgroup+
                                      posix:+posix-spawn-setsigmask+))
    (let ((flags (posix:spawnattr-getflags attr)))
      (assert-true (not (zerop (logand flags posix:+posix-spawn-setpgroup+))))
      (assert-true (not (zerop (logand flags posix:+posix-spawn-setsigmask+)))))))

(deftest test-spawnattr-pgroup ()
  "Test spawning a process in a new process group"
  (posix:with-spawnattr (attr)
    (posix:spawnattr-setflags attr posix:+posix-spawn-setpgroup+)
    (posix:spawnattr-setpgroup attr 0) ; 0 = child PID becomes pgid
    (posix:with-pipe (read-fd write-fd :cloexec t)
      (posix:with-file-actions (fa)
        (posix:file-actions-adddup2 fa write-fd 1)
        (posix:file-actions-addclose fa read-fd)
        ;; Spawn child that prints its own pgid
        (let ((pid (posix:call-posix-spawnp
                    "sh" '("sh" "-c" "echo $$")
                    :file-actions-sap fa
                    :spawnattr-sap attr)))
          (posix:fd-close write-fd)
          ;; The child's pgid should equal its pid (new group)
          (let ((child-pgid (posix:getpgid pid)))
            (assert-equal pid child-pgid))
          (posix:waitpid pid 0))))))

(deftest test-spawnattr-sigmask ()
  "Test setting signal mask on spawned process"
  (posix:with-spawnattr (attr)
    (posix:with-empty-sigset (mask)
      (posix:spawnattr-setflags attr posix:+posix-spawn-setsigmask+)
      (posix:spawnattr-setsigmask attr mask)
      ;; Spawn succeeds with signal mask set
      (let ((pid (posix:call-posix-spawnp
                  "true" '("true")
                  :spawnattr-sap attr)))
        (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
          (declare (ignore waited-pid))
          (assert-true (posix:wifexited status))
          (assert-equal 0 (posix:wexitstatus status)))))))

;;; ============================================================================
;;; File Actions Tests
;;; ============================================================================

(deftest test-file-actions-devnull ()
  "Test redirecting stdout to /dev/null via file actions"
  (posix:with-file-actions (fa)
    ;; Open /dev/null as fd 1 (stdout)
    (posix:file-actions-addopen fa 1 "/dev/null"
                                (logior posix:+o-wronly+) #o644)
    (let ((pid (posix:call-posix-spawnp
                "echo" '("echo" "this should vanish")
                :file-actions-sap fa)))
      (multiple-value-bind (waited-pid status) (posix:waitpid pid 0)
        (declare (ignore waited-pid))
        (assert-true (posix:wifexited status))
        (assert-equal 0 (posix:wexitstatus status))))))

#+linux
(deftest test-cloexec-verified ()
  "Test that O_CLOEXEC pipes are not visible to child processes"
  ;; Create a pipe with O_CLOEXEC
  (posix:with-pipe (extra-read extra-write :cloexec t)
    ;; Spawn a child that lists its open fds via /proc/self/fd
    (posix:with-pipe (stdout-read stdout-write :cloexec t)
      (posix:with-file-actions (fa)
        (posix:file-actions-adddup2 fa stdout-write 1)
        (posix:file-actions-addclose fa stdout-read)
        (let ((pid (posix:call-posix-spawnp
                    "ls" '("ls" "/proc/self/fd")
                    :file-actions-sap fa)))
          (posix:fd-close stdout-write)
          ;; Read the child's fd listing
          (lib:with-foreign-memory ((buf 4096))
            (let ((n (posix:fd-read stdout-read buf 4096)))
              (when (> n 0)
                (let ((output (sb-ext:octets-to-string
                               (let ((v (make-array n :element-type '(unsigned-byte 8))))
                                 (dotimes (i n v)
                                   (setf (aref v i) (sb-sys:sap-ref-8 buf i))))
                               :external-format :utf-8)))
                  ;; The extra-read and extra-write fds (marked O_CLOEXEC)
                  ;; should NOT appear in the child's fd listing.
                  (let* ((fd-strings (remove-if (lambda (s) (zerop (length s)))
                                               (loop for start = 0 then (1+ pos)
                                                     for pos = (position #\Newline output :start start)
                                                     collect (subseq output start (or pos (length output)))
                                                     while pos)))
                         (child-fds (mapcar #'parse-integer fd-strings))
                         (extra-read-str (format nil "~D" extra-read))
                         (extra-write-str (format nil "~D" extra-write)))
                    (assert-not (member extra-read child-fds)
                        (format nil "CLOEXEC fd ~A visible in child: ~A"
                                extra-read-str fd-strings))
                    (assert-not (member extra-write child-fds)
                        (format nil "CLOEXEC fd ~A visible in child: ~A"
                                extra-write-str fd-strings)))))))
          (posix:waitpid pid 0))))))

;;; ============================================================================
;;; Process Group Tests
;;; ============================================================================

(deftest test-getpgid-self ()
  "Test getting process group of current process"
  (let ((pgid (posix:getpgid 0)))
    (assert-true (integerp pgid))
    (assert-true (> pgid 0))))

;;; ============================================================================
;;; pidfd Tests (Linux only)
;;; ============================================================================

#+linux
(deftest test-pidfd-available ()
  "Test pidfd availability detection"
  (let ((result (posix:pidfd-available-p)))
    ;; Result should be t or nil, not :unknown
    (assert-true (or (eq result t) (eq result nil)))))

#+linux
(deftest test-pidfd-open-self ()
  "Test opening a pidfd for the current process"
  (when (posix:pidfd-available-p)
    (let ((fd (posix:pidfd-open (sb-posix:getpid))))
      (unwind-protect
           (progn
             (assert-true (integerp fd))
             (assert-true (>= fd 0)))
        (posix:fd-close fd)))))

#+linux
(deftest test-pidfd-open-child ()
  "Test opening a pidfd for a child process"
  (when (posix:pidfd-available-p)
    ;; Spawn a child that sleeps briefly
    (let ((pid (posix:call-posix-spawnp "sleep" '("sleep" "1"))))
      (let ((pidfd (posix:pidfd-open pid)))
        (unwind-protect
             (progn
               (assert-true (integerp pidfd))
               (assert-true (>= pidfd 0)))
          (posix:fd-close pidfd)))
      (posix:waitpid pid 0))))

;;; ============================================================================
;;; String Array Tests
;;; ============================================================================

(deftest test-string-array-size ()
  "Test string array size computation"
  ;; Empty list: just NULL pointer (8 bytes)
  (assert-equal 8 (posix:compute-string-array-size '()))
  ;; Single empty string: ptr (8) + NULL ptr (8) + null byte (1) = 17
  (assert-equal 17 (posix:compute-string-array-size '("")))
  ;; ["hello"]: 2 ptrs (16) + 6 bytes ("hello" + null) = 22
  (assert-equal 22 (posix:compute-string-array-size '("hello"))))

(deftest test-string-array-roundtrip ()
  "Test that string arrays can be built and read back correctly"
  (let ((strings '("hello" "world" "test")))
    (posix:with-string-array (arr strings)
      ;; Verify we can read back each string by following the pointers
      (dotimes (i 3)
        (let ((str-ptr (sb-sys:sap-ref-sap arr (* i 8))))
          (assert-true (not (sb-sys:sap= str-ptr (sb-sys:int-sap 0))))
          ;; Read the C string back
          (let ((result (loop for j from 0
                              for byte = (sb-sys:sap-ref-8 str-ptr j)
                              until (zerop byte)
                              collect (code-char byte) into chars
                              finally (return (coerce chars 'string)))))
            (assert-equal (nth i strings) result))))
      ;; Verify NULL terminator
      (let ((null-ptr (sb-sys:sap-ref-sap arr (* 3 8))))
        (assert-true (sb-sys:sap= null-ptr (sb-sys:int-sap 0)))))))

;;; ============================================================================
;;; Signal Set Tests
;;; ============================================================================

(deftest test-empty-sigset ()
  "Test creating an empty signal set"
  (posix:with-empty-sigset (set)
    ;; Just verify it doesn't error
    (assert-true (not (null set)))))

(deftest test-full-sigset ()
  "Test creating a full signal set"
  (posix:with-full-sigset (set)
    (assert-true (not (null set)))))

(deftest test-sigaddset ()
  "Test adding signals to a set"
  (posix:with-empty-sigset (set)
    ;; Should not error
    (posix:sigaddset set posix:+sigterm+)
    (posix:sigaddset set posix:+sigint+)
    (assert-true t)))
