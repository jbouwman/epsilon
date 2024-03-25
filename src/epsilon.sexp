(("lib" "binding"
        "array"
        "symbol"
        "condition"
        "string"
        "function"
        "collect"
        "type"
        "uuid"
        ("type" "macro-utils"
                "vectors"
                "streams")
        "list"
        "sequence"
        "hash"
        "vector"
        "control"
        "xsubseq"
        ("char" "package"
                ("encoding" "encoding"
                            "ascii"
                            "cp437"
                            "iso-8859"
                            "unicode")
                "external-format"
                "string")
        "stream"
        ("seq" "package"
               "vector"
               "list"
               "extended-sequence"
               "public")
        "uri")
 ("sys" "env"
        ("ffi" "package"
               "sys-utils"
               "sbcl"
               "utils"
               "early-types"
               "foreign-vars"
               "libraries"
               "types"
               "enum"
               "strings"
               "structures"
               "functions")
        "fs"
        "gc"
        ("sync" "error"
                "atomic"
                "lock"
                "semaphore"
                "thread"
                "variable"
                "timeout"))
 ("lib" ("regex" "package"
                 "specials"
                 "util"
                 "errors"
                 "charset"
                 "charmap"
                 "chartest"
                 "lexer"
                 "parser"
                 "regex-class"
                 "regex-class-util"
                 "convert"
                 "optimize"
                 "closures"
                 "repetition-closures"
                 "scanner"
                 "public")
        "buffer"
        ("checksum" "generic"
                    "adler-32"
                    "crc-32")
        ("codec" "package"
                 "generic"
                 "constant"
                 "type"
                 "condition"
                 "hex"
                 "base64"
                 "gzip"
                 "zlib"
                 "decompress"
                 "inflate"
                 "bzip"
                 "stream"
                 "bitstream"
                 "huffman"
                 "compress"
                 "public")
        ("digest" "reader"
                  "common"
                  "generic"
                  "stream"
                  "sha-2"
                  "public")
        ("archive" "package"
                   "toolkit"
                   "parser"
                   "io"
                   "tables"
                   "compression"
                   "encryption"
                   "pkware-encryption"
                   "structures"
                   "zippy"
                   "decode"
                   "encode"))
 ("net" "socket"
        ("tls" "config"
               "package"
               "reload"
               "ffi"
               "bio"
               "conditions"
               "ssl-funcall"
               "init"
               "ffi-buffer"
               "verify-hostname"
               "streams"
               "x509"
               "random"
               "context")
        ("http" "chunked-stream"
                "parser"
                "encoding"
                "connection-cache"
                "keep-alive-stream"
                "util"
                "body"
                "error"
                "backend/socket"
               "public"))
 ("tool" "test"))
