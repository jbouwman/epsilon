(defpackage #:epsilon.tool.dev
  (:use
   #:cl)
  (:local-nicknames
   (#:map #:epsilon.lib.map)
   (#:seq #:epsilon.lib.sequence)
   (#:str #:epsilon.lib.string)
   (#:pkg #:epsilon.sys.pkg)
   (#:build #:epsilon.tool.build)
   (#:test #:epsilon.tool.test)
   (#:re #:epsilon.lib.regex)
   (#:uri #:epsilon.lib.uri))
  (:export
   :main))

(in-package #:epsilon.tool.dev)

(defun run-build (&rest args)
  "Handle build command with required module name"
  (unless (and args (not (str:starts-with-p (first args) "--")))
    (error "Module name required. Usage: build <module-name> [options]"))
  (let ((module (first args))
        (build-args (cdr args)))
    (apply #'build:build module (as-keys build-args))))

(defun run-test (&rest args)
  "Handle test command with required module name"
  (unless (and args (not (str:starts-with-p (first args) "--")))
    (error "Module name required. Usage: test <module-name> [options]"))
  (let ((module (first args))
        (test-args (cdr args)))
    ;; Build first
    (build:build module)
    ;; Then run tests
    (let ((result (apply #'test:run test-args)))
      (unless (test:success-p result)
        (sb-posix:exit 1)))))

(defvar *commands*
  (map:make-map "build" #'run-build
                "test" #'run-test))

(defun as-keys (list)
  (mapcar (lambda (element)
            (cond ((keywordp element)
                   element)  ; Already a keyword, pass through
                  ((stringp element)
                   (if (str:starts-with-p element "--")
                       (intern (string-upcase (subseq element 2)) :keyword)
                       element))
                  (t
                   element)))
          list))

(defun main ()
  (let ((argv (cdr sb-ext:*posix-argv*)))
    (unless argv
      (error "No argument provided, must be one of ~A" (map:keys *commands*)))
    (let ((command (map:get *commands* (car argv))))
      (unless command
        (error "Unknown command ~A, must be one of ~A" command (map:keys *commands*)))
      (apply command (as-keys (cdr argv))))))
      
