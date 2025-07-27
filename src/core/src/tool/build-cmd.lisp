(defpackage epsilon.tool.build-cmd
  (:use cl)
  (:local-nicknames
   (map epsilon.map)
   (argparse epsilon.argparse)
   (build epsilon.tool.build)
   (build-env epsilon.tool.build-environment)
   (proto epsilon.build.protocol)
   (main epsilon.main)
   (cmd epsilon.tool.package-command)))

(in-package epsilon.tool.build-cmd)

(defclass build (cmd:package-command)
  ())

(defmethod main:run-command ((command build) parsed-args)
  (let* ((env (build:make-build-environment))
         (options (argparse:parsed-options parsed-args))
         (repos (map:get options "package_repo"))
         (parallel (map:get options "parallel")))
    ;; Configure environment
    (dolist (repo repos)
      (build-env:add-package-repo env repo))
    (when parallel
      (setf (build-env:option-parallel (build-env:environment-options env)) t))
    ;; Use this environment for package resolution and build
    (let* ((build:*current-environment* env)
           (package (cmd:resolve-package command parsed-args)))
      (build:build-with-environment env package))))

(defmethod main:argument-parser ((command build))
  (let ((parser (argparse:make-parser
                 :command "build"
                 :description "Build epsilon packages")))
    (cmd:add-package-arguments command parser)
    (argparse:add-argument parser "--parallel"
                          :action 'store-true
                          :help "Use parallel compilation (experimental)")
    parser))

(main:register-command 'build)
