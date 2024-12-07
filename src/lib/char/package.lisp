(defpackage #:epsilon.lib.char
  (:use
   #:cl
   #:epsilon.lib.binding
   #:epsilon.lib.function
   #:epsilon.lib.hash
   #:epsilon.lib.symbol
   #:epsilon.lib.type
   #:epsilon.lib.vector)
  (:export
   #:*string-vector-mappings*
   ;; character encoding objects
   #:list-character-encodings
   #:character-encoding
   #:*default-character-encoding*
   #:get-character-encoding
   #:enc-name
   #:enc-aliases
   #:enc-code-unit-size
   #:enc-max-units-per-char
   #:enc-native-endianness
   #:enc-decode-literal-code-unit-limit
   #:enc-encode-literal-code-unit-limit
   #:enc-use-bom
   #:enc-bom-encoding
   #:enc-nul-encoding
   #:enc-default-replacement
   #:ambiguous-encoding-p
   ;; concrete mappings
   #:instantiate-concrete-mappings
   #:encoder
   #:decoder
   #:octet-counter
   #:code-point-counter
   #:lookup-mapping
   #:*suppress-character-coding-errors*
   ;; errors
   #:character-coding-error
   #:character-coding-error-encoding    ; accessor
   #:character-coding-error-buffer      ; accessor
   #:character-coding-error-position    ; accessor
   #:character-decoding-error
   #:character-decoding-error-octets    ; accessor
   #:character-encoding-error
   #:character-encoding-error-code      ; accessor
   #:end-of-input-in-character
   #:character-out-of-range
   #:invalid-utf8-starter-byte
   #:invalid-utf8-continuation-byte
   #:overlong-utf8-sequence

   #:*default-character-encoding*
   #:*default-eol-style*
   #:concatenate-strings-to-octets
   #:encoding
   #:encoding-encoding
   #:encoding-eol-style
   #:encoding-equal
   #:encoding-error
   #:ensure-encoding
   #:list-character-encodings
   #:make-encoding
   #:simple-unicode-string
   #:string-size-in-octets
   #:string-to-u8
   #:u8-to-string
   #:unicode-char
   #:unicode-char-code-limit
   #:unicode-string
   #:vector-size-in-chars

   #:standard-alpha-byte-p
   #:standard-alpha-char-p
   #:standard-alphanumeric-p
   #:standard-alphanumeric-byte-p

   ))
