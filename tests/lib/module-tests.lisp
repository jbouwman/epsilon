(update-package
 (build-module
  (parse-module
   '(module lib.list
     (import lib.function
      (lib.symbol poot))
     (export alist-plist
      assoc-value)))))

(make-package 'foo)
