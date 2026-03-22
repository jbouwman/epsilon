;;;; Epsilon Stack Trace
;;;;
;;;; Structured stack trace capture, source context, and formatted display.
;;;; Interfaces with SBCL's debug interface for frame extraction.

(defpackage epsilon.stacktrace
  (:use :cl)
  (:export
   ;; Frame structure
   #:stack-frame #:make-stack-frame #:stack-frame-p #:copy-stack-frame
   #:stack-frame-index #:stack-frame-function-name #:stack-frame-package-name
   #:stack-frame-source-file #:stack-frame-source-line #:stack-frame-source-column
   #:stack-frame-locals #:stack-frame-arguments
   #:stack-frame-internal-p #:stack-frame-lambda-p #:stack-frame-raw-frame
   ;; Trace structure
   #:stack-trace #:make-stack-trace #:stack-trace-p #:copy-stack-trace
   #:stack-trace-frames #:stack-trace-condition #:stack-trace-condition-type
   #:stack-trace-condition-message #:stack-trace-timestamp
   #:stack-trace-thread-name #:stack-trace-cause
   ;; Utility
   #:user-frames #:frame-count
   ;; Capture
   #:capture-stack-trace #:capture-current-stack #:extract-frame
   #:get-function-name #:get-package-name #:get-source-location
   #:get-frame-locals-info #:get-frame-arguments-info
   #:internal-package-p #:internal-function-name-p #:lambda-function-p
   #:truncate-value-string
   #:*internal-packages* #:*internal-function-patterns* #:*max-value-length*
   ;; Source context
   #:get-source-lines #:get-source-context
   #:source-context-line #:make-source-context-line #:source-context-line-p
   #:copy-source-context-line #:source-context-line-number
   #:source-context-line-text #:source-context-line-current-p
   #:source-available-p #:clear-source-cache #:load-source-lines
   #:evict-oldest-from-cache #:update-cache-access
   #:*source-cache* #:*source-cache-access-order*
   #:*source-cache-max-size* #:*source-cache-lock*
   ;; Formatting
   #:format-stack-trace #:serialize-stack-trace #:serialize-frame
   #:format-stack-trace-json #:format-json-object
   #:colorize #:color-enabled-p #:config-get
   #:format-function-name #:format-location #:shorten-path
   #:format-frame-compact #:format-frame-default #:format-frame-verbose
   #:*stack-trace-format* #:*stack-trace-color* #:*stack-trace-config*
   #:*ansi-colors* #:initialize-stack-trace-config))

(in-package epsilon.stacktrace)

;;;; ====================================================================
;;;; Data Structures
;;;; ====================================================================

(defstruct stack-frame
  "Structured representation of a single stack frame."
  (index 0 :type fixnum)
  (function-name nil :type (or symbol string null))
  (package-name nil :type (or string null))
  (source-file nil :type (or pathname string null))
  (source-line nil :type (or fixnum null))
  (source-column nil :type (or fixnum null))
  (locals nil :type list)
  (arguments nil :type list)
  (internal-p nil :type boolean)
  (lambda-p nil :type boolean)
  (raw-frame nil))

(defstruct stack-trace
  "Complete stack trace with metadata."
  (frames #() :type vector)
  (condition nil)
  (condition-type nil :type (or symbol null))
  (condition-message nil :type (or string null))
  (timestamp nil)
  (thread-name nil :type (or string null))
  (cause nil :type (or stack-trace null)))

(defun user-frames (trace)
  "Return only user (non-internal) frames from a stack trace."
  (remove-if #'stack-frame-internal-p (stack-trace-frames trace)))

(defun frame-count (trace &key include-internal)
  "Return the number of frames in a stack trace."
  (if include-internal
      (length (stack-trace-frames trace))
      (count-if-not #'stack-frame-internal-p (stack-trace-frames trace))))

;;;; ====================================================================
;;;; Capture
;;;; ====================================================================

(defparameter *internal-packages*
  '("SB-IMPL" "SB-INT" "SB-KERNEL" "SB-DEBUG" "SB-C" "SB-VM"
    "SB-ALIEN" "SB-SYS" "SB-EXT" "SB-PCL" "SB-MOP" "SB-THREAD"
    "SB-POSIX" "SB-GRAY" "SB-SEQUENCE"))

(defparameter *internal-function-patterns*
  '("EVAL-IN-LEXENV" "SIMPLE-EVAL" "%COERCE-CALLABLE"
    "CALL-WITH-" "INVOKE-WITH-" "%CALL-" "%INVOKE-"))

(defun internal-package-p (package-name)
  (and package-name
       (or (member package-name *internal-packages* :test #'string=)
           (and (>= (length package-name) 3)
                (string= (subseq package-name 0 3) "SB-")))
       t))

(defun internal-function-name-p (name)
  (and (stringp name)
       (some (lambda (pattern) (search pattern name))
             *internal-function-patterns*)
       t))

(defun get-function-name (debug-fun)
  "Extract a readable function name from a debug-fun."
  (when debug-fun
    (let ((name (sb-di:debug-fun-name debug-fun)))
      (cond
        ((null name) nil)
        ((symbolp name) name)
        ((and (consp name) (eq (car name) 'lambda)) "(lambda)")
        ((and (consp name) (eq (car name) 'flet))
         (format nil "(flet ~A)" (second name)))
        ((and (consp name) (eq (car name) 'labels))
         (format nil "(labels ~A)" (second name)))
        ((and (consp name) (eq (car name) 'sb-pcl::fast-method))
         (format nil "~A" (cdr name)))
        (t (prin1-to-string name))))))

(defun get-package-name (debug-fun)
  (when debug-fun
    (let ((name (sb-di:debug-fun-name debug-fun)))
      (when (symbolp name)
        (let ((pkg (symbol-package name)))
          (when pkg (package-name pkg)))))))

(defun lambda-function-p (debug-fun)
  (when debug-fun
    (let ((name (sb-di:debug-fun-name debug-fun)))
      (and (consp name) (member (car name) '(lambda flet labels)) t))))

(defun get-source-location (frame)
  "Get (file line column) from a frame, or (nil nil nil)."
  (handler-case
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (let ((debug-source (sb-di:code-location-debug-source code-location)))
            (when debug-source
              (values (sb-di:debug-source-namestring debug-source)
                      (ignore-errors
                        (sb-di:code-location-toplevel-form-offset code-location))
                      nil)))))
    (error () (values nil nil nil))))

(defparameter *max-value-length* 100)

(defun truncate-value-string (str)
  (if (> (length str) *max-value-length*)
      (concatenate 'string (subseq str 0 (- *max-value-length* 3)) "...")
      str))

(defun get-frame-locals-info (frame &key (capture-values t))
  "Get local variables as alist of (name . value-string)."
  (let ((locals nil))
    (ignore-errors
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (sb-di:do-debug-fun-vars (var (sb-di:frame-debug-fun frame))
            (when (eq (sb-di:debug-var-validity var code-location) :valid)
              (let* ((name (sb-di:debug-var-symbol var))
                     (value-str (if capture-values
                                    (ignore-errors
                                      (truncate-value-string
                                       (prin1-to-string (sb-di:debug-var-value var frame))))
                                    "<not captured>")))
                (push (cons name value-str) locals)))))))
    (nreverse locals)))

(defun get-frame-arguments-info (frame)
  "Get argument values as a list of strings."
  (let ((args nil))
    (ignore-errors
      (let ((debug-fun (sb-di:frame-debug-fun frame)))
        (when debug-fun
          (let ((code-location (sb-di:frame-code-location frame)))
            (when code-location
              (sb-di:do-debug-fun-vars (var debug-fun)
                (when (and (eq (sb-di:debug-var-validity var code-location) :valid)
                           (sb-di:debug-var-info-available var))
                  (let ((value-str (ignore-errors
                                     (truncate-value-string
                                      (prin1-to-string (sb-di:debug-var-value var frame))))))
                    (when value-str (push value-str args))))))))))
    (nreverse args)))

(defun extract-frame (raw-frame index &key capture-locals)
  "Extract a stack-frame from an SBCL raw frame."
  (let* ((debug-fun (ignore-errors (sb-di:frame-debug-fun raw-frame)))
         (function-name (get-function-name debug-fun))
         (package-name (get-package-name debug-fun))
         (internal-p (or (internal-package-p package-name)
                         (internal-function-name-p
                          (if (stringp function-name) function-name
                              (prin1-to-string function-name)))
                         (null function-name))))
    (multiple-value-bind (file line column) (get-source-location raw-frame)
      (make-stack-frame
       :index index :function-name function-name :package-name package-name
       :source-file file :source-line line :source-column column
       :locals (when capture-locals (get-frame-locals-info raw-frame :capture-values t))
       :arguments nil
       :internal-p internal-p :lambda-p (lambda-function-p debug-fun)
       :raw-frame raw-frame))))

(defun capture-stack-trace (condition &key (max-frames 50) (capture-locals t))
  "Capture a structured stack trace from the current execution context."
  (let ((frames nil) (raw-frame (sb-di:top-frame)) (count 0))
    (loop while (and raw-frame (< count max-frames))
          do (push (extract-frame raw-frame count :capture-locals capture-locals) frames)
             (incf count)
             (setf raw-frame (sb-di:frame-down raw-frame)))
    (make-stack-trace
     :frames (coerce (nreverse frames) 'vector)
     :condition condition
     :condition-type (type-of condition)
     :condition-message (princ-to-string condition)
     :timestamp (get-universal-time)
     :thread-name (ignore-errors (sb-thread:thread-name sb-thread:*current-thread*))
     :cause nil)))

(defun capture-current-stack (&key (max-frames 50) (capture-locals nil))
  "Capture the current stack trace without a condition."
  (let ((frames nil) (raw-frame (sb-di:top-frame)) (count 0))
    (loop while (and raw-frame (< count max-frames))
          do (push (extract-frame raw-frame count :capture-locals capture-locals) frames)
             (incf count)
             (setf raw-frame (sb-di:frame-down raw-frame)))
    (make-stack-trace
     :frames (coerce (nreverse frames) 'vector)
     :timestamp (get-universal-time)
     :thread-name (ignore-errors (sb-thread:thread-name sb-thread:*current-thread*)))))

;;;; ====================================================================
;;;; Source Context
;;;; ====================================================================

(defvar *source-cache* (make-hash-table :test 'equal))
(defvar *source-cache-access-order* nil)
(defvar *source-cache-max-size* 50)
(defvar *source-cache-lock* (sb-thread:make-mutex :name "source-cache-lock"))

(defun clear-source-cache ()
  (sb-thread:with-mutex (*source-cache-lock*)
    (clrhash *source-cache*)
    (setf *source-cache-access-order* nil)))

(defun evict-oldest-from-cache ()
  (when *source-cache-access-order*
    (let ((oldest (car (last *source-cache-access-order*))))
      (remhash oldest *source-cache*)
      (setf *source-cache-access-order* (butlast *source-cache-access-order*)))))

(defun update-cache-access (path-string)
  (setf *source-cache-access-order*
        (cons path-string (remove path-string *source-cache-access-order* :test #'equal))))

(defun load-source-lines (file)
  (handler-case
      (let ((path (if (pathnamep file) file (pathname file))))
        (when (probe-file path)
          (with-open-file (stream path :direction :input :if-does-not-exist nil)
            (when stream
              (coerce (loop for line = (read-line stream nil nil)
                            while line collect line)
                      'vector)))))
    (error () nil)))

(defun get-source-lines (file)
  "Get source lines for a file, using cache."
  (when (null file) (return-from get-source-lines nil))
  (let ((path-string (if (pathnamep file) (namestring file) file)))
    (sb-thread:with-mutex (*source-cache-lock*)
      (let ((cached (gethash path-string *source-cache*)))
        (when cached
          (update-cache-access path-string)
          (return-from get-source-lines cached)))
      (let ((lines (load-source-lines path-string)))
        (when lines
          (when (>= (hash-table-count *source-cache*) *source-cache-max-size*)
            (evict-oldest-from-cache))
          (setf (gethash path-string *source-cache*) lines)
          (update-cache-access path-string))
        lines))))

(defstruct source-context-line
  (number 0 :type fixnum)
  (text "" :type string)
  (current-p nil :type boolean))

(defun get-source-context (file line &key (before 2) (after 2))
  "Get source lines around a specific location."
  (when (or (null file) (null line) (<= line 0))
    (return-from get-source-context nil))
  (let ((lines (get-source-lines file)))
    (when (null lines) (return-from get-source-context nil))
    (let* ((start (max 1 (- line before)))
           (end (min (length lines) (+ line after))))
      (loop for i from start to end
            collect (make-source-context-line
                     :number i :text (aref lines (1- i)) :current-p (= i line))))))

(defun source-available-p (file)
  (and file (probe-file (if (pathnamep file) file (pathname file)))))

;;;; ====================================================================
;;;; Formatting
;;;; ====================================================================

(defvar *stack-trace-format* :default)
(defvar *stack-trace-color* t)
(defvar *stack-trace-config*
  '(:format :default :color t :max-frames 20 :show-internal nil
    :show-source t :source-context-lines 2 :show-locals t :truncate-values 100
    :hide-packages ("SB-IMPL" "SB-INT" "SB-KERNEL" "SB-DEBUG" "SB-C")))

(defun config-get (key) (getf *stack-trace-config* key))

(defparameter *ansi-colors*
  `(:error     ,(format nil "~C[1;31m" #\Escape)
    :warning   ,(format nil "~C[1;33m" #\Escape)
    :function  ,(format nil "~C[1;36m" #\Escape)
    :file      ,(format nil "~C[0;34m" #\Escape)
    :line-num  ,(format nil "~C[0;90m" #\Escape)
    :variable  ,(format nil "~C[0;35m" #\Escape)
    :value     ,(format nil "~C[0;32m" #\Escape)
    :arrow     ,(format nil "~C[1;33m" #\Escape)
    :dim       ,(format nil "~C[0;90m" #\Escape)
    :reset     ,(format nil "~C[0m" #\Escape)))

(defun colorize (text color)
  (if *stack-trace-color*
      (let ((color-start (getf *ansi-colors* color ""))
            (color-end (getf *ansi-colors* :reset)))
        #~"~{color-start}~{text}~{color-end}")
      (princ-to-string text)))

(defun color-enabled-p () (and *stack-trace-color* (or (config-get :color) t)))

(defun format-function-name (frame)
  (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((name (stack-frame-function-name frame)))
      (cond ((null name) "(unknown)")
            ((symbolp name) (symbol-name name))
            ((stringp name) name)
            (t (prin1-to-string name))))))

(defun format-location (frame)
  (let ((file (stack-frame-source-file frame))
        (line (stack-frame-source-line frame)))
    (when file
      (let ((short-file (shorten-path file)))
        (if line #~"~{short-file}:~{line}" short-file)))))

(defun shorten-path (path)
  (let ((str (if (pathnamep path) (namestring path) path)))
    (let ((pos (search "epsilon/" str)))
      (if pos (subseq str pos)
          (let ((src-pos (search "/src/" str)))
            (if src-pos (subseq str (1+ src-pos)) str))))))

(defun format-frame-compact (frame stream)
  (let ((func-name (format-function-name frame))
        (location (format-location frame)))
    (if location
        (format stream "  ~A (~A)~%" (colorize func-name :function) (colorize location :file))
        (format stream "  ~A~%" (colorize func-name :function)))))

(defun format-frame-default (frame stream &key show-source show-locals (source-context 2))
  (let ((func-name (format-function-name frame))
        (location (format-location frame))
        (file (stack-frame-source-file frame))
        (line (stack-frame-source-line frame)))
    (format stream "~&  at ~A" (colorize func-name :function))
    (when location (format stream " (~A)" (colorize location :file)))
    (format stream "~%")
    (when (and show-source file line)
      (let ((context (get-source-context file line :before source-context :after source-context)))
        (when context
          (dolist (ctx-line context)
            (format stream "     ~A ~A ~A~%"
                    (colorize (format nil "~4D" (source-context-line-number ctx-line)) :line-num)
                    (if (source-context-line-current-p ctx-line) (colorize "->" :arrow) "  ")
                    (source-context-line-text ctx-line)))
          (format stream "~%"))))
    (when (and show-locals (stack-frame-locals frame))
      (dolist (local (stack-frame-locals frame))
        (format stream "     ~A = ~A~%"
                (colorize (prin1-to-string (car local)) :variable)
                (colorize (cdr local) :value)))
      (format stream "~%"))))

(defun format-frame-verbose (frame stream index)
  (let ((func-name (format-function-name frame))
        (pkg-name (stack-frame-package-name frame))
        (file (stack-frame-source-file frame))
        (line (stack-frame-source-line frame)))
    (format stream "~&~A~%" (make-string 80 :initial-element #\-))
    (format stream "Frame ~D: ~A~%" index (colorize func-name :function))
    (when pkg-name (format stream "  Package: ~A~%" pkg-name))
    (when file (format stream "  File: ~A~%" file))
    (when line (format stream "  Line: ~A~%" line))
    (when (and file line)
      (let ((context (get-source-context file line :before 3 :after 3)))
        (when context
          (format stream "~%  Source Context:~%")
          (dolist (ctx-line context)
            (format stream "     ~A ~A ~A~%"
                    (colorize (format nil "~4D" (source-context-line-number ctx-line)) :line-num)
                    (if (source-context-line-current-p ctx-line) (colorize "->" :arrow) "  ")
                    (source-context-line-text ctx-line))))))
    (when (stack-frame-locals frame)
      (format stream "~%  Local Variables:~%")
      (dolist (local (stack-frame-locals frame))
        (format stream "    ~A = ~A~%"
                (colorize (prin1-to-string (car local)) :variable)
                (colorize (cdr local) :value))))))

(defun format-stack-trace (trace stream &key (show-internal nil) (show-source t)
                                             (show-locals t) (max-frames 20)
                                             (source-context 2) (format *stack-trace-format*))
  "Format a complete stack trace to stream."
  (when (null trace) (return-from format-stack-trace nil))
  (let* ((output-stream (or stream (make-string-output-stream)))
         (frames (if show-internal (stack-trace-frames trace)
                     (remove-if #'stack-frame-internal-p (stack-trace-frames trace))))
         (total-frames (length frames))
         (display-frames (min max-frames total-frames))
         (hidden-count (max 0 (- total-frames display-frames))))
    (when (stack-trace-condition-type trace)
      (format output-stream "~&~A: ~A~%~%"
              (colorize (prin1-to-string (stack-trace-condition-type trace)) :error)
              (stack-trace-condition-message trace)))
    (ecase format
      (:json (format-stack-trace-json trace output-stream))
      (:compact
       (loop for frame across frames for i from 0 below display-frames
             do (format-frame-compact frame output-stream))
       (when (> hidden-count 0)
         (format output-stream "  ~A~%"
                 (colorize (format nil "... ~D more frames" hidden-count) :dim))))
      (:verbose
       (format output-stream "~A~%" (make-string 80 :initial-element #\=))
       (format output-stream "~A: ~A~%"
               (colorize (prin1-to-string (stack-trace-condition-type trace)) :error)
               (stack-trace-condition-message trace))
       (format output-stream "~A~%" (make-string 80 :initial-element #\=))
       (loop for frame across frames for i from 0 below display-frames
             do (format-frame-verbose frame output-stream i)))
      (:default
       (loop for frame across frames for i from 0 below display-frames
             do (format-frame-default frame output-stream
                                      :show-source show-source :show-locals show-locals
                                      :source-context source-context))
       (when (> hidden-count 0)
         (format output-stream "  ~A~%"
                 (colorize (format nil "... ~D frames hidden (use :show-internal t for full trace)"
                                   (+ hidden-count
                                      (if show-internal 0
                                          (count-if #'stack-frame-internal-p
                                                    (stack-trace-frames trace)))))
                           :dim)))))
    (unless stream (get-output-stream-string output-stream))))

;;; JSON Serialization

(defun serialize-frame (frame)
  (list (cons "index" (stack-frame-index frame))
        (cons "function" (format-function-name frame))
        (cons "package" (stack-frame-package-name frame))
        (cons "file" (when (stack-frame-source-file frame)
                       (if (pathnamep (stack-frame-source-file frame))
                           (namestring (stack-frame-source-file frame))
                           (stack-frame-source-file frame))))
        (cons "line" (stack-frame-source-line frame))
        (cons "internal" (stack-frame-internal-p frame))
        (cons "locals" (mapcar (lambda (l)
                                 (list (cons "name" (prin1-to-string (car l)))
                                       (cons "value" (cdr l))))
                               (stack-frame-locals frame)))))

(defun serialize-stack-trace (trace)
  (when trace
    (list (cons "conditionType" (when (stack-trace-condition-type trace)
                                  (prin1-to-string (stack-trace-condition-type trace))))
          (cons "message" (stack-trace-condition-message trace))
          (cons "timestamp" (stack-trace-timestamp trace))
          (cons "threadName" (stack-trace-thread-name trace))
          (cons "frames" (map 'list #'serialize-frame (stack-trace-frames trace))))))

(defun format-stack-trace-json (trace stream)
  (format-json-object (serialize-stack-trace trace) stream))

(defun format-json-object (obj stream &optional (indent 0))
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (cond
      ((null obj) (format stream "null"))
      ((eq obj t) (format stream "true"))
      ((stringp obj) (format stream "~S" obj))
      ((numberp obj) (format stream "~A" obj))
      ((and (consp obj) (consp (car obj)) (stringp (caar obj)))
       (format stream "{~%")
       (loop for (pair . rest) on obj
             do (format stream "~A  ~S: " indent-str (car pair))
                (format-json-object (cdr pair) stream (+ indent 2))
                (when rest (format stream ",")))
       (format stream "~%~A}" indent-str))
      ((listp obj)
       (format stream "[~%")
       (loop for (item . rest) on obj
             do (format stream "~A  " indent-str)
                (format-json-object item stream (+ indent 2))
                (when rest (format stream ",")))
       (format stream "~%~A]" indent-str))
      (t (format stream "~S" (prin1-to-string obj))))))

(defun initialize-stack-trace-config ()
  (let ((color-env (sb-ext:posix-getenv "EPSILON_STACKTRACE_COLOR"))
        (verbose-env (sb-ext:posix-getenv "EPSILON_STACKTRACE_VERBOSE"))
        (max-frames-env (sb-ext:posix-getenv "EPSILON_STACKTRACE_MAX_FRAMES")))
    (when color-env
      (setf *stack-trace-color* (string-equal color-env "true")))
    (when verbose-env
      (when (string-equal verbose-env "true") (setf *stack-trace-format* :verbose)))
    (when max-frames-env
      (let ((max (ignore-errors (parse-integer max-frames-env))))
        (when max (setf (getf *stack-trace-config* :max-frames) max))))))
