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
                "streams"
                ("sbcl-opt" "fndb"
                            "nib-tran"))
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
               "public"))
 ("sys" "env"
        ("ffi" "package"
               "sys-utils"
               "sbcl"
               "utils"
               "libraries"
               "early-types"
               "types"
               "enum"
               "strings"
               "structures"
               "functions"
               "foreign-vars")
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
        ("digest" "macro"
                  "reader"
                  "common"
                  "generic"
                  "public"
                  "stream"
                  "sha-2")
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
        "url"
        ("tls" "config"
               "package"
               "reload"
               "ffi"
               "bio"
               "conditions"
               "ssl-funcall"
               "init"
               "ffi-buffer"
               "streams"
               "x509"
               "random"
               "context"
               "verify-hostname")
        ("http" "chunked-stream"
                "parser"
                "encoding"
               "connection-cache"
                "decoding-stream"
                "keep-alive-stream"
                "util"
                "body"
                "error"
                "backend/socket"
               "public"))
 ("tool" "test"))
