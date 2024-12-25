(require :asdf)

(load "epsilon.asd")

(asdf:load-system "epsilon/tests")

(sb-posix:exit (if (epsilon.tool.test:run-success-p (epsilon.tool.test:run-tests))
                   0
                   1))
     
