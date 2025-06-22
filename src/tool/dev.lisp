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

(defun test (&rest args)
  (build:build)
  (let ((result (apply #'test:run args)))
    (unless (test:success-p result)
      (sb-posix:exit 1))))

(defvar *commands*
  (map:make-map "build" #'build:build
                "test" #'test))

(defun as-keys (list)
  (mapcar (lambda (element)
            (cond ((str:starts-with-p element "--")
                   (intern (string-upcase (subseq element 2)) :keyword))
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
      
