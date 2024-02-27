(in-package #:lib.char)

(define-character-encoding :ascii
    "A 7-bit, fixed-width character encoding in which all
character codes map to their Unicode equivalents."
  :aliases '(:us-ascii)
  :literal-char-code-limit 128)

(define-unibyte-encoder :ascii (code)
  (if (>= code 128)
      (handle-error)
      code))

(define-unibyte-decoder :ascii (octet)
  (if (>= octet 128)
      (handle-error)
      octet))
