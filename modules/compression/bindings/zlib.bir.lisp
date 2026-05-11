;;; Epsilon Binding IR File
;;; Generated: 2026-02-02T20:17:20
;;; Platform: LINUX-X86-64
;;; Source: /nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h

(:BINDING-IR
 :version 1
 :source-headers ("/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h")
 :parse-time 3979081040
 :platform :LINUX-X86-64
 :include-paths NIL
 :defines NIL

 :functions
 (
  (:name "zlibVersion"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflate"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateEnd"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflate"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateEnd"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateSetDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateGetDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateCopy"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateReset"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateParams"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateTune"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT) (:NAME ""
                                                                    :TYPE :INT) (:NAME
                                                                                 ""
                                                                                 :TYPE
                                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateBound"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflatePending"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflatePrime"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateSetHeader"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateSetDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateGetDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateSync"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateCopy"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateReset"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateReset2"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflatePrime"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateMark"
   :return-type :LONG
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateGetHeader"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateBack"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER) (:NAME
                                                                                       ""
                                                                                       :TYPE
                                                                                       :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateBackEnd"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "zlibCompileFlags"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "compress"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "compress2"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER) (:NAME
                                                                                       ""
                                                                                       :TYPE
                                                                                       :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "compressBound"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "uncompress"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "uncompress2"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzdopen"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :INT) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzbuffer"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :UINT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzsetparams"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzread"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :UINT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzfread"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzwrite"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :UINT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzfwrite"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzprintf"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzputs"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzgets"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzputc"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzgetc"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzungetc"
   :return-type :INT
   :params ((:NAME "" :TYPE :INT) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzflush"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzrewind"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzeof"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzdirect"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzclose"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzclose_r"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzclose_w"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzerror"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzclearerr"
   :return-type :VOID
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "adler32"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "adler32_z"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "crc32"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "crc32_z"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "crc32_combine_op"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateInit_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :POINTER) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateInit_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateInit2_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT) (:NAME ""
                                                                    :TYPE :INT) (:NAME
                                                                                 ""
                                                                                 :TYPE
                                                                                 :INT) (:NAME
                                                                                        ""
                                                                                        :TYPE
                                                                                        :INT) (:NAME
                                                                                               ""
                                                                                               :TYPE
                                                                                               :POINTER) (:NAME
                                                                                                          ""
                                                                                                          :TYPE
                                                                                                          :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateInit2_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :POINTER) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateBackInit_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :POINTER) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzgetc_"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzopen"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzseek"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gztell"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzoffset"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "adler32_combine"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "crc32_combine"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "crc32_combine_gen"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "zError"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateSyncPoint"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "get_crc_table"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateUndermine"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateValidate"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateCodesUsed"
   :return-type :ULONG
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "inflateResetKeep"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "deflateResetKeep"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
  (:name "gzvprintf"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/ybhi1g8z643mcg5n4pkwhsihj4klk9y0-zlib-1.3.1-dev/include/zlib.h"
   :line 0)
 )

 :structs
 (
  (:name "internal_state"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "z_stream_s"
   :size 112
   :alignment 8
   :opaque NIL
   :fields (
    (:name "next_in" :type :POINTER :offset 0 :size 8)
    (:name "avail_in" :type :POINTER :offset 8 :size 4)
    (:name "total_in" :type :POINTER :offset 16 :size 8)
    (:name "next_out" :type :POINTER :offset 24 :size 8)
    (:name "avail_out" :type :POINTER :offset 32 :size 4)
    (:name "total_out" :type :POINTER :offset 40 :size 8)
    (:name "msg" :type :POINTER :offset 48 :size 8)
    (:name "state" :type :POINTER :offset 56 :size 8)
    (:name "zalloc" :type :POINTER :offset 64 :size 8)
    (:name "zfree" :type :POINTER :offset 72 :size 8)
    (:name "opaque" :type :POINTER :offset 80 :size 8)
    (:name "data_type" :type :INT :offset 88 :size 4)
    (:name "adler" :type :POINTER :offset 96 :size 8)
    (:name "reserved" :type :POINTER :offset 104 :size 8)
   ))
  (:name "gz_header_s"
   :size 80
   :alignment 8
   :opaque NIL
   :fields (
    (:name "text" :type :INT :offset 0 :size 4)
    (:name "time" :type :POINTER :offset 8 :size 8)
    (:name "xflags" :type :INT :offset 16 :size 4)
    (:name "os" :type :INT :offset 20 :size 4)
    (:name "extra" :type :POINTER :offset 24 :size 8)
    (:name "extra_len" :type :POINTER :offset 32 :size 4)
    (:name "extra_max" :type :POINTER :offset 36 :size 4)
    (:name "name" :type :POINTER :offset 40 :size 8)
    (:name "name_max" :type :POINTER :offset 48 :size 4)
    (:name "comment" :type :POINTER :offset 56 :size 8)
    (:name "comm_max" :type :POINTER :offset 64 :size 4)
    (:name "hcrc" :type :INT :offset 68 :size 4)
    (:name "done" :type :INT :offset 72 :size 4)
   ))
  (:name "gzFile_s"
   :size 1
   :alignment 1
   :opaque T
   :fields (
   ))
  (:name "gzFile_s"
   :size 1
   :alignment 1
   :opaque T
   :fields (
   ))
 )

 :enums
 (
 )

 :typedefs
 (
  (:name "z_size_t" :underlying "int")
  (:name "Byte" :underlying "unsigned char")
  (:name "uInt" :underlying "unsigned int")
  (:name "uLong" :underlying "unsigned long")
  (:name "Bytef" :underlying "unsigned char")
  (:name "charf" :underlying "char")
  (:name "intf" :underlying "int")
  (:name "uIntf" :underlying "unsigned int")
  (:name "uLongf" :underlying "unsigned long")
  (:name "voidpc" :underlying "const void *")
  (:name "voidpf" :underlying "void *")
  (:name "voidp" :underlying "void *")
  (:name "z_crc_t" :underlying "unsigned long")
  (:name "alloc_func" :underlying "void *(*)(void *, unsigned int, unsigned int)")
  (:name "free_func" :underlying "void (*)(void *, void *)")
  (:name "z_stream" :underlying "struct z_stream_s")
  (:name "z_streamp" :underlying "struct z_stream_s *")
  (:name "gz_header" :underlying "struct gz_header_s")
  (:name "gz_headerp" :underlying "struct gz_header_s *")
  (:name "in_func" :underlying "unsigned int (*)(void *, unsigned char **)")
  (:name "out_func" :underlying "int (*)(void *, unsigned char *, unsigned int)")
  (:name "gzFile" :underlying "struct gzFile_s *")
 )

 :macros
 (
  (:name "Z_NO_FLUSH" :kind :CONSTANT :value 0)
  (:name "Z_PARTIAL_FLUSH" :kind :CONSTANT :value 1)
  (:name "Z_SYNC_FLUSH" :kind :CONSTANT :value 2)
  (:name "Z_FULL_FLUSH" :kind :CONSTANT :value 3)
  (:name "Z_FINISH" :kind :CONSTANT :value 4)
  (:name "Z_BLOCK" :kind :CONSTANT :value 5)
  (:name "Z_TREES" :kind :CONSTANT :value 6)
  (:name "Z_OK" :kind :CONSTANT :value 0)
  (:name "Z_STREAM_END" :kind :CONSTANT :value 1)
  (:name "Z_NEED_DICT" :kind :CONSTANT :value 2)
  (:name "Z_ERRNO" :kind :CONSTANT :value -1)
  (:name "Z_STREAM_ERROR" :kind :CONSTANT :value -2)
  (:name "Z_DATA_ERROR" :kind :CONSTANT :value -3)
  (:name "Z_MEM_ERROR" :kind :CONSTANT :value -4)
  (:name "Z_BUF_ERROR" :kind :CONSTANT :value -5)
  (:name "Z_VERSION_ERROR" :kind :CONSTANT :value -6)
  (:name "Z_NO_COMPRESSION" :kind :CONSTANT :value 0)
  (:name "Z_BEST_SPEED" :kind :CONSTANT :value 1)
  (:name "Z_BEST_COMPRESSION" :kind :CONSTANT :value 9)
  (:name "Z_DEFAULT_COMPRESSION" :kind :CONSTANT :value -1)
  (:name "Z_FILTERED" :kind :CONSTANT :value 1)
  (:name "Z_HUFFMAN_ONLY" :kind :CONSTANT :value 2)
  (:name "Z_RLE" :kind :CONSTANT :value 3)
  (:name "Z_FIXED" :kind :CONSTANT :value 4)
  (:name "Z_DEFAULT_STRATEGY" :kind :CONSTANT :value 0)
  (:name "Z_BINARY" :kind :CONSTANT :value 0)
  (:name "Z_TEXT" :kind :CONSTANT :value 1)
  (:name "Z_ASCII" :kind :CONSTANT :value 1)
  (:name "Z_UNKNOWN" :kind :CONSTANT :value 2)
  (:name "Z_DEFLATED" :kind :CONSTANT :value 8)
  (:name "Z_NULL" :kind :CONSTANT :value 0)
 ))
