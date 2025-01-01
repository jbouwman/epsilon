(defpackage #:epsilon.lib.regex
  (:use #:cl
        #:epsilon.lib.binding
        #:epsilon.lib.symbol
        #:epsilon.lib.type)
  (:shadow #:digit-char-p)
  (:export #:*allow-quoting*
           #:*look-ahead-for-suffix*
           #:*optimize-char-classes*
           #:*property-resolver*
           #:*regex-char-code-limit*
           #:*use-bmh-matchers*
           #:all-matches
           #:all-matches-as-strings
           #:count-matches
           #:create-optimized-test-function
           #:create-scanner
           #:define-parse-tree-synonym
           #:do-matches
           #:do-matches-as-strings
           #:do-scans
           #:parse-string
           #:parse-tree-synonym
           #:regex-error
           #:regex-invocation-error
           #:regex-syntax-error
           #:regex-syntax-error-pos
           #:regex-syntax-error-string
           #:quote-meta-chars
           #:regex-replace
           #:regex-replace-all
           #:scan
           #:scan-to-strings
           #:split))
