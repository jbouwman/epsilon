;;;; struct-debug.lisp - Debug struct layout

(in-package :epsilon.foreign.jit.struct)

(define-jit-struct aligned-struct
  (a :char)
  (b :int))

(let* ((struct (get-jit-struct 'aligned-struct))
       (fields (jit-struct-def-fields struct)))
  (format t "Size: ~A~%" (jit-struct-def-size struct))
  (format t "Alignment: ~A~%" (jit-struct-def-alignment struct))
  (format t "Fields: ~A~%" fields)
  (force-output))
