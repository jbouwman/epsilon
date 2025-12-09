;;; Directory Local Variables -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Epsilon project settings for Emacs
((nil . ((fill-column . 80)
         (indent-tabs-mode . nil)
         (require-final-newline . t)))

 (lisp-mode . ((lisp-indent-function . common-lisp-indent-function)
               (eval . (when (fboundp 'epsilon-mode)
                        (epsilon-mode 1)))
               ;; Add epsilon to module search path
               (epsilon-default-module-paths . ("./modules" "./epsilon-modules"))
               ;; Project root detection
               (eval . (setq-local epsilon-project-root
                                   (locate-dominating-file default-directory "module.lisp")))
               ;; Custom indentation for epsilon macros
               (eval . (progn
                        (put 'deftest 'common-lisp-indent-function 1)
                        (put 'with-label 'common-lisp-indent-function 1)
                        (put 'with-fixture 'common-lisp-indent-function 1)
                        (put 'is-thrown 'common-lisp-indent-function 1)
                        (put 'handler-case 'common-lisp-indent-function 1)
                        (put 'multiple-value-bind 'common-lisp-indent-function 2)))))

 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (checkdoc-minor-mode . t))))
