(in-package #:lib.regex)

(defvar *syntax-error-string* nil
  "The string which caused the syntax error.")

(define-condition regex-syntax-error (simple-error)
  ((string :initarg :string
           :reader regex-syntax-error-string)
   (pos :initarg :pos
        :reader regex-syntax-error-pos))
  (:default-initargs
      :pos nil
      :string *syntax-error-string*)
  (:report (lambda (condition stream)
             (format stream "~?~@[ at position ~A~]~@[ in string ~S~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (regex-syntax-error-pos condition)
                     (regex-syntax-error-string condition))))
  (:documentation "Signaled if the parser encounters an error
when trying to parse a regex string or to convert a parse tree into
its internal representation."))

(setf (documentation 'regex-syntax-error-string 'function)
      "Returns the string the parser was parsing when the error was
encountered \(or NIL if the error happened while trying to convert a
parse tree).")

(setf (documentation 'regex-syntax-error-pos 'function)
      "Returns the position within the string where the error occurred
\(or NIL if the error happened while trying to convert a parse tree")

(define-condition regex-invocation-error (simple-error)
  ()
  (:documentation "Signaled when functions are
invoked with wrong arguments."))

(defmacro signal-syntax-error* (pos format-control &rest format-arguments)
  `(error 'regex-syntax-error
          :pos ,pos
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-syntax-error (format-control &rest format-arguments)
  `(signal-syntax-error* nil ,format-control ,@format-arguments))

(defmacro signal-invocation-error (format-control &rest format-arguments)
  `(error 'regex-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))
