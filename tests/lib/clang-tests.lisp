(defpackage :epsilon.lib.clang.test
  (:use :cl
        :epsilon.tool.test)
  (:local-nicknames 
   (:fs :epsilon.sys.fs)
   (:parser :epsilon.lib.clang)
   (:map :epsilon.lib.map)))

(in-package :epsilon.lib.clang.test)

;; Test with socket.c content
(deftest test-socket-parser ()
  (let* ((socket-content (fs:read-file
                          (project-file "epsilon" "tests/lib/socket.c")))
         (database (parser:parse-clang-output socket-content)))
    
    (format t "~%=== TYPEDEFS ===~%")
    (dolist (typedef (parser:get-typedefs database))
      (format t "~A -> ~A~%" (car typedef) (getf (cdr typedef) :underlying-type)))
    
    (format t "~%=== STRUCTS ===~%") 
    (dolist (struct-pair (map:seq (parser::type-database-structs database)))
      (let ((struct (cdr struct-pair)))
        (format t "struct ~A {~%" (getf struct :name))
        (dolist (field (getf struct :fields))
          (format t "  ~A ~A;~%" (getf field :type) (getf field :name)))
        (format t "}~%~%")))
    
    (format t "~%=== FUNCTIONS ===~%")
    (dolist (func (parser:get-functions database))
      (let ((func-data (cdr func)))
        (format t "~A ~A(" (getf func-data :return-type) (getf func-data :name))
        (let ((params (getf func-data :parameters)))
          (when params
            (format t "~A ~A" (getf (first params) :type) (getf (first params) :name))
            (dolist (param (rest params))
              (format t ", ~A ~A" (getf param :type) (getf param :name)))))
        (format t ");~%")))
    
    database))
