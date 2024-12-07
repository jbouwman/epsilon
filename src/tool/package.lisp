(defpackage #:tool.package
  (:use #:cl)
  (:shadow #:find-package))

(in-package #:tool.package)

(defvar *providers*
  (list "file:///Users/jbouwman/git/epsilon"
        "https://recipe.cat/e/pkg"))


(defun find-package (name)
  )


(find-package "coalton")
