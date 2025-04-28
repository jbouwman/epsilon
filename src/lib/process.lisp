(defpackage #:epsilon.lib.process
  (:use #:cl))

(in-package #:epsilon.lib.process)

(defgeneric start (process &key &allow-other-keys)
  (:documentation "Start a process"))

(defgeneric stop (process)
  (:documentation "Stop a process"))

(defgeneric running-p (process)
  (:documentation "Check if process is running"))
