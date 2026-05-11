;;; Epsilon Binding IR File
;;; Generated: 2026-02-02T21:12:03
;;; Platform: LINUX-X86-64
;;; Source: /nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/encode.h, /nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/decode.h

(:BINDING-IR
 :version 1
 :source-headers ("/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/encode.h"
                  "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/decode.h")
 :parse-time 3979084323
 :platform :LINUX-X86-64
 :include-paths NIL
 :defines NIL

 :functions
 (
  (:name "BrotliEncoderDestroyInstance"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/encode.h"
   :line 0)
  (:name "BrotliEncoderDestroyPreparedDictionary"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/encode.h"
   :line 0)
  (:name "BrotliDecoderDestroyInstance"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/decode.h"
   :line 0)
  (:name "BrotliDecoderErrorString"
   :return-type :POINTER
   :params ((:NAME "" :TYPE :POINTER))
   :variadic NIL
   :header "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/decode.h"
   :line 0)
  (:name "BrotliDecoderSetMetadataCallbacks"
   :return-type :INT
   :params ((:NAME "" :TYPE :POINTER) (:NAME "" :TYPE :POINTER) (:NAME "" :TYPE
                                                                 :POINTER) (:NAME
                                                                            ""
                                                                            :TYPE
                                                                            :POINTER))
   :variadic NIL
   :header "/nix/store/mkn94yad6n3m2lnnfxg7qmqzshvs5a1i-brotli-1.1.0-dev/include/brotli/decode.h"
   :line 0)
 )

 :structs
 (
  (:name "BrotliEncoderStateStruct"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "BrotliEncoderPreparedDictionaryStruct"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
  (:name "BrotliDecoderStateStruct"
   :size 0
   :alignment 0
   :opaque T
   :fields (
   ))
 )

 :enums
 (
  (:name "BrotliEncoderMode"
   :values (
    (:name "BROTLI_MODE_GENERIC" :value 0)
    (:name "BROTLI_MODE_TEXT" :value 1)
    (:name "BROTLI_MODE_FONT" :value 2)
   ))
  (:name "BrotliEncoderOperation"
   :values (
    (:name "BROTLI_OPERATION_PROCESS" :value 0)
    (:name "BROTLI_OPERATION_FLUSH" :value 1)
    (:name "BROTLI_OPERATION_FINISH" :value 2)
    (:name "BROTLI_OPERATION_EMIT_METADATA" :value 3)
   ))
  (:name "BrotliEncoderParameter"
   :values (
    (:name "BROTLI_PARAM_MODE" :value 0)
    (:name "BROTLI_PARAM_QUALITY" :value 1)
    (:name "BROTLI_PARAM_LGWIN" :value 2)
    (:name "BROTLI_PARAM_LGBLOCK" :value 3)
    (:name "BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING" :value 4)
    (:name "BROTLI_PARAM_SIZE_HINT" :value 5)
    (:name "BROTLI_PARAM_LARGE_WINDOW" :value 6)
    (:name "BROTLI_PARAM_NPOSTFIX" :value 7)
    (:name "BROTLI_PARAM_NDIRECT" :value 8)
    (:name "BROTLI_PARAM_STREAM_OFFSET" :value 9)
   ))
  (:name "BrotliDecoderResult"
   :values (
    (:name "BROTLI_DECODER_RESULT_ERROR" :value 0)
    (:name "BROTLI_DECODER_RESULT_SUCCESS" :value 1)
    (:name "BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT" :value 2)
    (:name "BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT" :value 3)
   ))
  (:name "BrotliDecoderErrorCode"
   :values (
    (:name "BROTLI_DECODER_NO_ERROR" :value 0)
    (:name "BROTLI_DECODER_SUCCESS" :value 1)
    (:name "BROTLI_DECODER_NEEDS_MORE_INPUT" :value 2)
    (:name "BROTLI_DECODER_NEEDS_MORE_OUTPUT" :value 3)
    (:name "BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_NIBBLE" :value -1)
    (:name "BROTLI_DECODER_ERROR_FORMAT_RESERVED" :value -2)
    (:name "BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_META_NIBBLE" :value -3)
    (:name "BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_ALPHABET" :value -4)
    (:name "BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_SAME" :value -5)
    (:name "BROTLI_DECODER_ERROR_FORMAT_CL_SPACE" :value -6)
    (:name "BROTLI_DECODER_ERROR_FORMAT_HUFFMAN_SPACE" :value -7)
    (:name "BROTLI_DECODER_ERROR_FORMAT_CONTEXT_MAP_REPEAT" :value -8)
    (:name "BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_1" :value -9)
    (:name "BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_2" :value -10)
    (:name "BROTLI_DECODER_ERROR_FORMAT_TRANSFORM" :value -11)
    (:name "BROTLI_DECODER_ERROR_FORMAT_DICTIONARY" :value -12)
    (:name "BROTLI_DECODER_ERROR_FORMAT_WINDOW_BITS" :value -13)
    (:name "BROTLI_DECODER_ERROR_FORMAT_PADDING_1" :value -14)
    (:name "BROTLI_DECODER_ERROR_FORMAT_PADDING_2" :value -15)
    (:name "BROTLI_DECODER_ERROR_FORMAT_DISTANCE" :value -16)
    (:name "BROTLI_DECODER_ERROR_COMPOUND_DICTIONARY" :value -18)
    (:name "BROTLI_DECODER_ERROR_DICTIONARY_NOT_SET" :value -19)
    (:name "BROTLI_DECODER_ERROR_INVALID_ARGUMENTS" :value -20)
    (:name "BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MODES" :value -21)
    (:name "BROTLI_DECODER_ERROR_ALLOC_TREE_GROUPS" :value -22)
    (:name "BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MAP" :value -25)
    (:name "BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_1" :value -26)
    (:name "BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_2" :value -27)
    (:name "BROTLI_DECODER_ERROR_ALLOC_BLOCK_TYPE_TREES" :value -30)
    (:name "BROTLI_DECODER_ERROR_UNREACHABLE" :value -31)
   ))
  (:name "BrotliDecoderParameter"
   :values (
    (:name "BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION" :value 0)
    (:name "BROTLI_DECODER_PARAM_LARGE_WINDOW" :value 1)
   ))
 )

 :typedefs
 (
  (:name "BrotliEncoderMode" :underlying "enum BrotliEncoderMode")
  (:name "BrotliEncoderOperation" :underlying "enum BrotliEncoderOperation")
  (:name "BrotliEncoderParameter" :underlying "enum BrotliEncoderParameter")
  (:name "BrotliEncoderState" :underlying "struct BrotliEncoderStateStruct")
  (:name "BrotliEncoderPreparedDictionary" :underlying "struct BrotliEncoderPreparedDictionaryStruct")
  (:name "BrotliDecoderState" :underlying "struct BrotliDecoderStateStruct")
  (:name "BrotliDecoderResult" :underlying "BrotliDecoderResult")
  (:name "BrotliDecoderErrorCode" :underlying "BrotliDecoderErrorCode")
  (:name "BrotliDecoderParameter" :underlying "enum BrotliDecoderParameter")
 )

 :macros
 (
  (:name "BROTLI_MIN_WINDOW_BITS" :kind :CONSTANT :value 10)
  (:name "BROTLI_MAX_WINDOW_BITS" :kind :CONSTANT :value 24)
  (:name "BROTLI_LARGE_MAX_WINDOW_BITS" :kind :CONSTANT :value 30)
  (:name "BROTLI_MIN_INPUT_BLOCK_BITS" :kind :CONSTANT :value 16)
  (:name "BROTLI_MAX_INPUT_BLOCK_BITS" :kind :CONSTANT :value 24)
  (:name "BROTLI_MIN_QUALITY" :kind :CONSTANT :value 0)
  (:name "BROTLI_MAX_QUALITY" :kind :CONSTANT :value 11)
  (:name "BROTLI_DEFAULT_QUALITY" :kind :CONSTANT :value 11)
  (:name "BROTLI_DEFAULT_WINDOW" :kind :CONSTANT :value 22)
  (:name "BROTLI_DEFAULT_MODE" :kind :CONSTANT :value 0)
  (:name "BROTLI_LAST_ERROR_CODE" :kind :CONSTANT :value -31)
 ))
