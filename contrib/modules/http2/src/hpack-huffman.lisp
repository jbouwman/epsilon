;;;; HPACK Huffman Encoding Implementation
;;;;
;;;; Implements Huffman encoding for HPACK per RFC 7541 Appendix B

(in-package :epsilon.http2.hpack)

;;;; Huffman Code Table (RFC 7541 Appendix B)
;;;; Each entry is (code . bits) where code is the bit pattern and bits is the bit length

(defparameter *huffman-codes*
  #((#x1ff8 . 13)     ; 0 - '0'
    (#x7fffd8 . 23)   ; 1
    (#xfffffe2 . 28)  ; 2
    (#xfffffe3 . 28)  ; 3
    (#xfffffe4 . 28)  ; 4
    (#xfffffe5 . 28)  ; 5
    (#xfffffe6 . 28)  ; 6
    (#xfffffe7 . 28)  ; 7
    (#xfffffe8 . 28)  ; 8
    (#xffffea . 24)   ; 9
    (#x3ffffffc . 30) ; 10
    (#xfffffe9 . 28)  ; 11
    (#xfffffea . 28)  ; 12
    (#x3ffffffd . 30) ; 13
    (#xfffffeb . 28)  ; 14
    (#xfffffec . 28)  ; 15
    (#xfffffed . 28)  ; 16
    (#xfffffee . 28)  ; 17
    (#xfffffef . 28)  ; 18
    (#xffffff0 . 28)  ; 19
    (#xffffff1 . 28)  ; 20
    (#xffffff2 . 28)  ; 21
    (#x3ffffffe . 30) ; 22
    (#xffffff3 . 28)  ; 23
    (#xffffff4 . 28)  ; 24
    (#xffffff5 . 28)  ; 25
    (#xffffff6 . 28)  ; 26
    (#xffffff7 . 28)  ; 27
    (#xffffff8 . 28)  ; 28
    (#xffffff9 . 28)  ; 29
    (#xffffffa . 28)  ; 30
    (#xffffffb . 28)  ; 31
    (#x14 . 6)        ; 32 - ' ' (space)
    (#x3f8 . 10)      ; 33 - '!'
    (#x3f9 . 10)      ; 34 - '"'
    (#xffa . 12)      ; 35 - '#'
    (#x1ff9 . 13)     ; 36 - '$'
    (#x15 . 6)        ; 37 - '%'
    (#xf8 . 8)        ; 38 - '&'
    (#x7fa . 11)      ; 39 - '''
    (#x3fa . 10)      ; 40 - '('
    (#x3fb . 10)      ; 41 - ')'
    (#xf9 . 8)        ; 42 - '*'
    (#x7fb . 11)      ; 43 - '+'
    (#xfa . 8)        ; 44 - ','
    (#x16 . 6)        ; 45 - '-'
    (#x17 . 6)        ; 46 - '.'
    (#x18 . 6)        ; 47 - '/'
    (#x0 . 5)         ; 48 - '0'
    (#x1 . 5)         ; 49 - '1'
    (#x2 . 5)         ; 50 - '2'
    (#x19 . 6)        ; 51 - '3'
    (#x1a . 6)        ; 52 - '4'
    (#x1b . 6)        ; 53 - '5'
    (#x1c . 6)        ; 54 - '6'
    (#x1d . 6)        ; 55 - '7'
    (#x1e . 6)        ; 56 - '8'
    (#x1f . 6)        ; 57 - '9'
    (#x5c . 7)        ; 58 - ':'
    (#xfb . 8)        ; 59 - ';'
    (#x7ffc . 15)     ; 60 - '<'
    (#x20 . 6)        ; 61 - '='
    (#xffb . 12)      ; 62 - '>'
    (#x3fc . 10)      ; 63 - '?'
    (#x1ffa . 13)     ; 64 - '@'
    (#x21 . 6)        ; 65 - 'A'
    (#x5d . 7)        ; 66 - 'B'
    (#x5e . 7)        ; 67 - 'C'
    (#x5f . 7)        ; 68 - 'D'
    (#x60 . 7)        ; 69 - 'E'
    (#x61 . 7)        ; 70 - 'F'
    (#x62 . 7)        ; 71 - 'G'
    (#x63 . 7)        ; 72 - 'H'
    (#x64 . 7)        ; 73 - 'I'
    (#x65 . 7)        ; 74 - 'J'
    (#x66 . 7)        ; 75 - 'K'
    (#x67 . 7)        ; 76 - 'L'
    (#x68 . 7)        ; 77 - 'M'
    (#x69 . 7)        ; 78 - 'N'
    (#x6a . 7)        ; 79 - 'O'
    (#x6b . 7)        ; 80 - 'P'
    (#x6c . 7)        ; 81 - 'Q'
    (#x6d . 7)        ; 82 - 'R'
    (#x6e . 7)        ; 83 - 'S'
    (#x6f . 7)        ; 84 - 'T'
    (#x70 . 7)        ; 85 - 'U'
    (#x71 . 7)        ; 86 - 'V'
    (#x72 . 7)        ; 87 - 'W'
    (#xfc . 8)        ; 88 - 'X'
    (#x73 . 7)        ; 89 - 'Y'
    (#xfd . 8)        ; 90 - 'Z'
    (#x1ffb . 13)     ; 91 - '['
    (#x7fff0 . 19)    ; 92 - '\'
    (#x1ffc . 13)     ; 93 - ']'
    (#x3ffc . 14)     ; 94 - '^'
    (#x22 . 6)        ; 95 - '_'
    (#x7ffd . 15)     ; 96 - '`'
    (#x3 . 5)         ; 97 - 'a'
    (#x23 . 6)        ; 98 - 'b'
    (#x4 . 5)         ; 99 - 'c'
    (#x24 . 6)        ; 100 - 'd'
    (#x5 . 5)         ; 101 - 'e'
    (#x25 . 6)        ; 102 - 'f'
    (#x26 . 6)        ; 103 - 'g'
    (#x27 . 6)        ; 104 - 'h'
    (#x6 . 5)         ; 105 - 'i'
    (#x74 . 7)        ; 106 - 'j'
    (#x75 . 7)        ; 107 - 'k'
    (#x28 . 6)        ; 108 - 'l'
    (#x29 . 6)        ; 109 - 'm'
    (#x2a . 6)        ; 110 - 'n'
    (#x7 . 5)         ; 111 - 'o'
    (#x2b . 6)        ; 112 - 'p'
    (#x76 . 7)        ; 113 - 'q'
    (#x2c . 6)        ; 114 - 'r'
    (#x8 . 5)         ; 115 - 's'
    (#x9 . 5)         ; 116 - 't'
    (#x2d . 6)        ; 117 - 'u'
    (#x77 . 7)        ; 118 - 'v'
    (#x78 . 7)        ; 119 - 'w'
    (#x79 . 7)        ; 120 - 'x'
    (#x7a . 7)        ; 121 - 'y'
    (#x7b . 7)        ; 122 - 'z'
    (#x7ffe . 15)     ; 123 - '{'
    (#x7fc . 11)      ; 124 - '|'
    (#x3ffd . 14)     ; 125 - '}'
    (#x1ffd . 13)     ; 126 - '~'
    (#xffffffc . 28)  ; 127
    (#xfffe6 . 20)    ; 128
    (#x3fffd2 . 22)   ; 129
    (#xfffe7 . 20)    ; 130
    (#xfffe8 . 20)    ; 131
    (#x3fffd3 . 22)   ; 132
    (#x3fffd4 . 22)   ; 133
    (#x3fffd5 . 22)   ; 134
    (#x7fffd9 . 23)   ; 135
    (#x3fffd6 . 22)   ; 136
    (#x7fffda . 23)   ; 137
    (#x7fffdb . 23)   ; 138
    (#x7fffdc . 23)   ; 139
    (#x7fffdd . 23)   ; 140
    (#x7fffde . 23)   ; 141
    (#xffffeb . 24)   ; 142
    (#x7fffdf . 23)   ; 143
    (#xffffec . 24)   ; 144
    (#xffffed . 24)   ; 145
    (#x3fffd7 . 22)   ; 146
    (#x7fffe0 . 23)   ; 147
    (#xffffee . 24)   ; 148
    (#x7fffe1 . 23)   ; 149
    (#x7fffe2 . 23)   ; 150
    (#x7fffe3 . 23)   ; 151
    (#x7fffe4 . 23)   ; 152
    (#x1fffdc . 21)   ; 153
    (#x3fffd8 . 22)   ; 154
    (#x7fffe5 . 23)   ; 155
    (#x3fffd9 . 22)   ; 156
    (#x7fffe6 . 23)   ; 157
    (#x7fffe7 . 23)   ; 158
    (#xffffef . 24)   ; 159
    (#x3fffda . 22)   ; 160
    (#x1fffdd . 21)   ; 161
    (#xfffe9 . 20)    ; 162
    (#x3fffdb . 22)   ; 163
    (#x3fffdc . 22)   ; 164
    (#x7fffe8 . 23)   ; 165
    (#x7fffe9 . 23)   ; 166
    (#x1fffde . 21)   ; 167
    (#x7fffea . 23)   ; 168
    (#x3fffdd . 22)   ; 169
    (#x3fffde . 22)   ; 170
    (#xfffff0 . 24)   ; 171
    (#x1fffdf . 21)   ; 172
    (#x3fffdf . 22)   ; 173
    (#x7fffeb . 23)   ; 174
    (#x7fffec . 23)   ; 175
    (#x1fffe0 . 21)   ; 176
    (#x1fffe1 . 21)   ; 177
    (#x3fffe0 . 22)   ; 178
    (#x1fffe2 . 21)   ; 179
    (#x7fffed . 23)   ; 180
    (#x3fffe1 . 22)   ; 181
    (#x7fffee . 23)   ; 182
    (#x7fffef . 23)   ; 183
    (#xfffea . 20)    ; 184
    (#x3fffe2 . 22)   ; 185
    (#x3fffe3 . 22)   ; 186
    (#x3fffe4 . 22)   ; 187
    (#x7ffff0 . 23)   ; 188
    (#x3fffe5 . 22)   ; 189
    (#x3fffe6 . 22)   ; 190
    (#x7ffff1 . 23)   ; 191
    (#x3ffffe0 . 26)  ; 192
    (#x3ffffe1 . 26)  ; 193
    (#xfffeb . 20)    ; 194
    (#x7fff1 . 19)    ; 195
    (#x3fffe7 . 22)   ; 196
    (#x7ffff2 . 23)   ; 197
    (#x3fffe8 . 22)   ; 198
    (#x1ffffec . 25)  ; 199
    (#x3ffffe2 . 26)  ; 200
    (#x3ffffe3 . 26)  ; 201
    (#x3ffffe4 . 26)  ; 202
    (#x7ffffde . 27)  ; 203
    (#x7ffffdf . 27)  ; 204
    (#x3ffffe5 . 26)  ; 205
    (#xfffff1 . 24)   ; 206
    (#x1ffffed . 25)  ; 207
    (#x7fff2 . 19)    ; 208
    (#x1fffe3 . 21)   ; 209
    (#x3ffffe6 . 26)  ; 210
    (#x7ffffe0 . 27)  ; 211
    (#x7ffffe1 . 27)  ; 212
    (#x3ffffe7 . 26)  ; 213
    (#x7ffffe2 . 27)  ; 214
    (#xfffff2 . 24)   ; 215
    (#x1fffe4 . 21)   ; 216
    (#x1fffe5 . 21)   ; 217
    (#x3ffffe8 . 26)  ; 218
    (#x3ffffe9 . 26)  ; 219
    (#xffffffd . 28)  ; 220
    (#x7ffffe3 . 27)  ; 221
    (#x7ffffe4 . 27)  ; 222
    (#x7ffffe5 . 27)  ; 223
    (#xfffec . 20)    ; 224
    (#xfffff3 . 24)   ; 225
    (#xfffed . 20)    ; 226
    (#x1fffe6 . 21)   ; 227
    (#x3fffe9 . 22)   ; 228
    (#x1fffe7 . 21)   ; 229
    (#x1fffe8 . 21)   ; 230
    (#x7ffff3 . 23)   ; 231
    (#x3fffea . 22)   ; 232
    (#x3fffeb . 22)   ; 233
    (#x1ffffee . 25)  ; 234
    (#x1ffffef . 25)  ; 235
    (#xfffff4 . 24)   ; 236
    (#xfffff5 . 24)   ; 237
    (#x3ffffea . 26)  ; 238
    (#x7ffff4 . 23)   ; 239
    (#x3ffffeb . 26)  ; 240
    (#x7ffffe6 . 27)  ; 241
    (#x3ffffec . 26)  ; 242
    (#x3ffffed . 26)  ; 243
    (#x7ffffe7 . 27)  ; 244
    (#x7ffffe8 . 27)  ; 245
    (#x7ffffe9 . 27)  ; 246
    (#x7ffffea . 27)  ; 247
    (#x7ffffeb . 27)  ; 248
    (#xffffffe . 28)  ; 249
    (#x7ffffec . 27)  ; 250
    (#x7ffffed . 27)  ; 251
    (#x7ffffee . 27)  ; 252
    (#x7ffffef . 27)  ; 253
    (#x7fffff0 . 27)  ; 254
    (#x3ffffee . 26)  ; 255
    (#x3fffffff . 30))) ; 256 - EOS

;; Create reverse lookup table for decoding
(defparameter *huffman-decode-tree* nil)

(defun build-decode-tree ()
  "Build a binary tree for efficient Huffman decoding"
  (let ((root (cons nil nil)))
    (loop for i from 0 below 257
          for (code . bits) = (aref *huffman-codes* i)
          do (let ((node root))
               (loop for bit-pos from (1- bits) downto 1
                     for bit = (if (logbitp bit-pos code) 1 0)
                     do (progn
                          (if (zerop bit)
                              (unless (car node)
                                (setf (car node) (cons nil nil)))
                              (unless (cdr node)
                                (setf (cdr node) (cons nil nil))))
                          (setf node (if (zerop bit) (car node) (cdr node)))))
               ;; Set the leaf value for the last bit
               (let ((last-bit (if (logbitp 0 code) 1 0)))
                 (if (zerop last-bit)
                     (setf (car node) i)
                     (setf (cdr node) i)))))
    root))

(defun huffman-encode (string)
  "Encode a string using HPACK Huffman encoding"
  (let ((bits 0)
        (bit-count 0)
        (output (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    (loop for char across string
          for char-code = (char-code char)
          for (code . code-bits) = (if (< char-code 257)
                                      (aref *huffman-codes* char-code)
                                      (aref *huffman-codes* 256))
          do (setf bits (logior (ash bits code-bits) code))
             (incf bit-count code-bits)
             ;; Output complete bytes
             (loop while (>= bit-count 8)
                   do (vector-push-extend 
                       (ldb (byte 8 (- bit-count 8)) bits)
                       output)
                      (decf bit-count 8)
                      (setf bits (ldb (byte bit-count 0) bits))))
    ;; Handle remaining bits with padding
    (when (> bit-count 0)
      (let ((padding-bits (- 8 bit-count)))
        (setf bits (logior (ash bits padding-bits)
                          (1- (ash 1 padding-bits))))
        (vector-push-extend bits output)))
    (coerce output '(vector (unsigned-byte 8)))))

(defun huffman-decode (bytes)
  "Decode bytes using HPACK Huffman decoding"
  (unless *huffman-decode-tree*
    (setf *huffman-decode-tree* (build-decode-tree)))
  
  (let ((result (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0))
        (node *huffman-decode-tree*))
    
    (loop for byte across bytes
          do (loop for bit-pos from 7 downto 0
                   do (let ((bit (if (logbitp bit-pos byte) 1 0)))
                        (setf node (if (zerop bit) 
                                       (car node) 
                                       (cdr node)))
                        
                        (cond
                          ((null node)
                           ;; Invalid encoding
                           (error "Invalid Huffman encoding"))
                          ((atom node)
                           ;; Found a character
                           (when (= node 256) ; EOS symbol
                             (return-from huffman-decode 
                               (coerce result 'string)))
                           (when (< node 256)
                             (vector-push-extend (code-char node) result))
                           (setf node *huffman-decode-tree*))))))
    
    ;; Check if we ended on padding (all 1s)
    ;; If we're not at root, we need to verify padding
    (unless (eq node *huffman-decode-tree*)
      ;; Traverse remaining path with all 1s
      (loop while (and node (not (atom node)))
            do (setf node (cdr node))) ; Follow 1 bits
      ;; Must end at EOS (256) or be valid padding
      (unless (or (null node) (= node 256))
        (error "Invalid padding in Huffman encoding")))
    
    (coerce result 'string)))