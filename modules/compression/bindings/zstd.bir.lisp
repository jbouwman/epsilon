;;; Epsilon Binding IR File
;;; Generated: 2026-02-02T21:11:34
;;; Platform: LINUX-X86-64
;;; Source: /nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h

(:BINDING-IR
 :version 1
 :source-headers ("/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h")
 :parse-time 3979084294
 :platform :LINUX-X86-64
 :include-paths NIL
 :defines NIL

 :functions
 (
  (:name "ZSTD_getErrorString"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_versionNumber"
   :return-type :UINT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_versionString"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compress"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :POINTER) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :INT) (:NAME
                                                                               ""
                                                                               :TYPE
                                                                               :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_decompress"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :POINTER) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getFrameContentSize"
   :return-type :ULLONG
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getDecompressedSize"
   :return-type :ULLONG
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_findFrameCompressedSize"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compressBound"
   :return-type :INT
   :params ((:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_isError"
   :return-type :UINT
   :params ((:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getErrorCode"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getErrorName"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_minCLevel"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_maxCLevel"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_defaultCLevel"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createCCtx"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeCCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compressCCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT) (:NAME
                                                                                          ""
                                                                                          :TYPE
                                                                                          :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createDCtx"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeDCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_decompressDCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_cParam_getBounds"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_setParameter"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_setPledgedSrcSize"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :ULLONG))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_reset"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compress2"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_dParam_getBounds"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DCtx_setParameter"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DCtx_reset"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createCStream"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeCStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compressStream2"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CStreamInSize"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CStreamOutSize"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_initCStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compressStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_flushStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_endStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createDStream"
   :return-type :POINTER
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeDStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_initDStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_decompressStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DStreamInSize"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DStreamOutSize"
   :return-type :INT
   :params ()
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compress_usingDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT) (:NAME
                                                                                          ""
                                                                                          :TYPE
                                                                                          :POINTER) (:NAME
                                                                                                     ""
                                                                                                     :TYPE
                                                                                                     :INT) (:NAME
                                                                                                            ""
                                                                                                            :TYPE
                                                                                                            :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_decompress_usingDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
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
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createCDict"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT) (:NAME "" :TYPE
                                                             :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeCDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_compress_usingCDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT) (:NAME
                                                                                          ""
                                                                                          :TYPE
                                                                                          :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_createDDict"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_freeDDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_decompress_usingDDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT) (:NAME
                                                                        ""
                                                                        :TYPE
                                                                        :POINTER) (:NAME
                                                                                   ""
                                                                                   :TYPE
                                                                                   :INT) (:NAME
                                                                                          ""
                                                                                          :TYPE
                                                                                          :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getDictID_fromDict"
   :return-type :UINT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getDictID_fromCDict"
   :return-type :UINT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getDictID_fromDDict"
   :return-type :UINT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_getDictID_fromFrame"
   :return-type :UINT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_loadDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_refCDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_CCtx_refPrefix"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DCtx_loadDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DCtx_refDDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_DCtx_refPrefix"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :INT))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_CCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_DCtx"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_CStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_DStream"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_CDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
  (:name "ZSTD_sizeof_DDict"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h"
   :line 0)
 )

 :structs
 (
  (:name "ZSTD_CCtx_s"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "ZSTD_DCtx_s"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "ZSTD_inBuffer_s"
   :size 1
   :alignment 1
   :opaque NIL
   :fields (
    (:name "src" :type :POINTER :offset -1 :size 8)
    (:name "size" :type :INT :offset -1 :size 4)
    (:name "pos" :type :INT :offset -1 :size 4)
   ))
  (:name "ZSTD_outBuffer_s"
   :size 1
   :alignment 1
   :opaque NIL
   :fields (
    (:name "dst" :type :POINTER :offset -1 :size 8)
    (:name "size" :type :INT :offset -1 :size 4)
    (:name "pos" :type :INT :offset -1 :size 4)
   ))
  (:name "ZSTD_CDict_s"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "ZSTD_DDict_s"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
 )

 :enums
 (
  (:name "ZSTD_ErrorCode"
   :values (
    (:name "ZSTD_error_no_error" :value 0)
    (:name "ZSTD_error_GENERIC" :value 1)
    (:name "ZSTD_error_prefix_unknown" :value 10)
    (:name "ZSTD_error_version_unsupported" :value 12)
    (:name "ZSTD_error_frameParameter_unsupported" :value 14)
    (:name "ZSTD_error_frameParameter_windowTooLarge" :value 16)
    (:name "ZSTD_error_corruption_detected" :value 20)
    (:name "ZSTD_error_checksum_wrong" :value 22)
    (:name "ZSTD_error_literals_headerWrong" :value 24)
    (:name "ZSTD_error_dictionary_corrupted" :value 30)
    (:name "ZSTD_error_dictionary_wrong" :value 32)
    (:name "ZSTD_error_dictionaryCreation_failed" :value 34)
    (:name "ZSTD_error_parameter_unsupported" :value 40)
    (:name "ZSTD_error_parameter_combination_unsupported" :value 41)
    (:name "ZSTD_error_parameter_outOfBound" :value 42)
    (:name "ZSTD_error_tableLog_tooLarge" :value 44)
    (:name "ZSTD_error_maxSymbolValue_tooLarge" :value 46)
    (:name "ZSTD_error_maxSymbolValue_tooSmall" :value 48)
    (:name "ZSTD_error_cannotProduce_uncompressedBlock" :value 49)
    (:name "ZSTD_error_stabilityCondition_notRespected" :value 50)
    (:name "ZSTD_error_stage_wrong" :value 60)
    (:name "ZSTD_error_init_missing" :value 62)
    (:name "ZSTD_error_memory_allocation" :value 64)
    (:name "ZSTD_error_workSpace_tooSmall" :value 66)
    (:name "ZSTD_error_dstSize_tooSmall" :value 70)
    (:name "ZSTD_error_srcSize_wrong" :value 72)
    (:name "ZSTD_error_dstBuffer_null" :value 74)
    (:name "ZSTD_error_noForwardProgress_destFull" :value 80)
    (:name "ZSTD_error_noForwardProgress_inputEmpty" :value 82)
    (:name "ZSTD_error_frameIndex_tooLarge" :value 100)
    (:name "ZSTD_error_seekableIO" :value 102)
    (:name "ZSTD_error_dstBuffer_wrong" :value 104)
    (:name "ZSTD_error_srcBuffer_wrong" :value 105)
    (:name "ZSTD_error_sequenceProducer_failed" :value 106)
    (:name "ZSTD_error_externalSequences_invalid" :value 107)
    (:name "ZSTD_error_maxCode" :value 120)
   ))
  (:name "ZSTD_strategy"
   :values (
    (:name "ZSTD_fast" :value 1)
    (:name "ZSTD_dfast" :value 2)
    (:name "ZSTD_greedy" :value 3)
    (:name "ZSTD_lazy" :value 4)
    (:name "ZSTD_lazy2" :value 5)
    (:name "ZSTD_btlazy2" :value 6)
    (:name "ZSTD_btopt" :value 7)
    (:name "ZSTD_btultra" :value 8)
    (:name "ZSTD_btultra2" :value 9)
   ))
  (:name "ZSTD_cParameter"
   :values (
    (:name "ZSTD_c_compressionLevel" :value 100)
    (:name "ZSTD_c_windowLog" :value 101)
    (:name "ZSTD_c_hashLog" :value 102)
    (:name "ZSTD_c_chainLog" :value 103)
    (:name "ZSTD_c_searchLog" :value 104)
    (:name "ZSTD_c_minMatch" :value 105)
    (:name "ZSTD_c_targetLength" :value 106)
    (:name "ZSTD_c_strategy" :value 107)
    (:name "ZSTD_c_targetCBlockSize" :value 130)
    (:name "ZSTD_c_enableLongDistanceMatching" :value 160)
    (:name "ZSTD_c_ldmHashLog" :value 161)
    (:name "ZSTD_c_ldmMinMatch" :value 162)
    (:name "ZSTD_c_ldmBucketSizeLog" :value 163)
    (:name "ZSTD_c_ldmHashRateLog" :value 164)
    (:name "ZSTD_c_contentSizeFlag" :value 200)
    (:name "ZSTD_c_checksumFlag" :value 201)
    (:name "ZSTD_c_dictIDFlag" :value 202)
    (:name "ZSTD_c_nbWorkers" :value 400)
    (:name "ZSTD_c_jobSize" :value 401)
    (:name "ZSTD_c_overlapLog" :value 402)
    (:name "ZSTD_c_experimentalParam1" :value 500)
    (:name "ZSTD_c_experimentalParam2" :value 10)
    (:name "ZSTD_c_experimentalParam3" :value 1000)
    (:name "ZSTD_c_experimentalParam4" :value 1001)
    (:name "ZSTD_c_experimentalParam5" :value 1002)
    (:name "ZSTD_c_experimentalParam7" :value 1004)
    (:name "ZSTD_c_experimentalParam8" :value 1005)
    (:name "ZSTD_c_experimentalParam9" :value 1006)
    (:name "ZSTD_c_experimentalParam10" :value 1007)
    (:name "ZSTD_c_experimentalParam11" :value 1008)
    (:name "ZSTD_c_experimentalParam12" :value 1009)
    (:name "ZSTD_c_experimentalParam13" :value 1010)
    (:name "ZSTD_c_experimentalParam14" :value 1011)
    (:name "ZSTD_c_experimentalParam15" :value 1012)
    (:name "ZSTD_c_experimentalParam16" :value 1013)
    (:name "ZSTD_c_experimentalParam17" :value 1014)
    (:name "ZSTD_c_experimentalParam18" :value 1015)
    (:name "ZSTD_c_experimentalParam19" :value 1016)
    (:name "ZSTD_c_experimentalParam20" :value 1017)
   ))
  (:name "ZSTD_ResetDirective"
   :values (
    (:name "ZSTD_reset_session_only" :value 1)
    (:name "ZSTD_reset_parameters" :value 2)
    (:name "ZSTD_reset_session_and_parameters" :value 3)
   ))
  (:name "ZSTD_dParameter"
   :values (
    (:name "ZSTD_d_windowLogMax" :value 100)
    (:name "ZSTD_d_experimentalParam1" :value 1000)
    (:name "ZSTD_d_experimentalParam2" :value 1001)
    (:name "ZSTD_d_experimentalParam3" :value 1002)
    (:name "ZSTD_d_experimentalParam4" :value 1003)
    (:name "ZSTD_d_experimentalParam5" :value 1004)
    (:name "ZSTD_d_experimentalParam6" :value 1005)
   ))
  (:name "ZSTD_EndDirective"
   :values (
    (:name "ZSTD_e_continue" :value 0)
    (:name "ZSTD_e_flush" :value 1)
    (:name "ZSTD_e_end" :value 2)
   ))
 )

 :typedefs
 (
  (:name "ZSTD_ErrorCode" :underlying "ZSTD_ErrorCode")
  (:name "ZSTD_CCtx" :underlying "struct ZSTD_CCtx_s")
  (:name "ZSTD_DCtx" :underlying "struct ZSTD_DCtx_s")
  (:name "ZSTD_strategy" :underlying "ZSTD_strategy")
  (:name "ZSTD_cParameter" :underlying "ZSTD_cParameter")
  (:name "ZSTD_bounds" :underlying "struct (unnamed at /nix/store/aqk9bbbi0kdrfcn8n1h85mj5di7l2gx8-zstd-1.5.7-dev/include/zstd.h:544:9)")
  (:name "ZSTD_ResetDirective" :underlying "ZSTD_ResetDirective")
  (:name "ZSTD_dParameter" :underlying "ZSTD_dParameter")
  (:name "ZSTD_inBuffer" :underlying "struct ZSTD_inBuffer_s")
  (:name "ZSTD_outBuffer" :underlying "struct ZSTD_outBuffer_s")
  (:name "ZSTD_CStream" :underlying "struct ZSTD_CCtx_s")
  (:name "ZSTD_EndDirective" :underlying "ZSTD_EndDirective")
  (:name "ZSTD_DStream" :underlying "struct ZSTD_DCtx_s")
  (:name "ZSTD_CDict" :underlying "struct ZSTD_CDict_s")
  (:name "ZSTD_DDict" :underlying "struct ZSTD_DDict_s")
 )

 :macros
 (
  (:name "ZSTD_VERSION_MAJOR" :kind :CONSTANT :value 1)
  (:name "ZSTD_VERSION_MINOR" :kind :CONSTANT :value 5)
  (:name "ZSTD_VERSION_RELEASE" :kind :CONSTANT :value 7)
  (:name "ZSTD_VERSION_NUMBER" :kind :CONSTANT :value 10507)
  (:name "ZSTD_CLEVEL_DEFAULT" :kind :CONSTANT :value 3)
  (:name "ZSTD_MAGICNUMBER" :kind :CONSTANT :value 4247762216)
  (:name "ZSTD_MAGIC_DICTIONARY" :kind :CONSTANT :value 3962610743)
  (:name "ZSTD_MAGIC_SKIPPABLE_START" :kind :CONSTANT :value 407710288)
  (:name "ZSTD_MAGIC_SKIPPABLE_MASK" :kind :CONSTANT :value 4294967280)
  (:name "ZSTD_BLOCKSIZELOG_MAX" :kind :CONSTANT :value 17)
  (:name "ZSTD_BLOCKSIZE_MAX" :kind :CONSTANT :value 131072)
  (:name "ZSTD_CONTENTSIZE_UNKNOWN" :kind :CONSTANT :value -1)
  (:name "ZSTD_CONTENTSIZE_ERROR" :kind :CONSTANT :value -2)
 ))
