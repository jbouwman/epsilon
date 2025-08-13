# epsilon.string

String manipulation and processing functions with Unicode support.

## Overview

The `epsilon.string` package provides string manipulation functions including splitting, joining, trimming, case conversion, and pattern matching. Functions operate on Common Lisp strings.

## Quick Start

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames (:str :epsilon.string)))

(in-package :my-app)

;; Basic operations
(str:concat "Hello" " " "World")  ; => "Hello World"
(str:split #\, "a,b,c")           ; => lazy seq of ("a" "b" "c")
(str:join ", " '("a" "b" "c"))   ; => "a, b, c"

;; Case conversion
(str:upcase "hello")              ; => "HELLO"
(str:capitalize "hello world")    ; => "Hello World"

;; Pattern matching
(str:starts-with-p "hello" "he") ; => T
(str:contains-p "hello" "ell")   ; => T
```

## Function Reference

### String Construction

#### concat
`(concat &rest strings) => string`

Concatenate all strings into a single string.

```lisp
(concat "hello" " " "world") ; => "hello world"
(concat "a" "b" "c")         ; => "abc"
```

#### join
`(join delimiter strings) => string`

Join strings with delimiter between each element.

```lisp
(join ", " '("a" "b" "c"))  ; => "a, b, c"
(join "-" '("2024" "01" "15")) ; => "2024-01-15"
```

#### random-string
`(random-string &optional (length 12)) => string`

Generate a random alphanumeric string of given length.

```lisp
(random-string 5)  ; => "aB3xK"
(random-string 10) ; => "x7Yz9mN2pQ"
```

#### repeat
`(repeat string n) => string`

Repeat string n times.

```lisp
(repeat "ab" 3)  ; => "ababab"
(repeat "-" 10)  ; => "----------"
```

### String Splitting

#### split
`(split delimiter string) => lazy-sequence`

Returns a lazy sequence of substrings split by delimiter.

```lisp
(split #\, "a,b,c")        ; => lazy seq ("a" "b" "c")
(split #\space "hello world") ; => lazy seq ("hello" "world")
```

### String Testing

#### empty-p
`(empty-p s) => boolean`

Return T if s is nil or an empty string.

```lisp
(empty-p "")      ; => T
(empty-p nil)     ; => T
(empty-p "hello") ; => NIL
```

#### blank-p
`(blank-p string) => boolean`

Return T if string is nil, empty, or contains only whitespace.

```lisp
(blank-p "")       ; => T
(blank-p "   ")    ; => T
(blank-p " hello ") ; => NIL
```

#### starts-with-p
`(starts-with-p seq prefix &key (test #'char-equal)) => boolean`

Returns T if sequence starts with prefix.

```lisp
(starts-with-p "hello" "he")     ; => T
(starts-with-p "Hello" "he")     ; => T (case-insensitive by default)
(starts-with-p "Hello" "he" :test #'char=) ; => NIL (case-sensitive)
```

#### ends-with-p
`(ends-with-p seq suffix &key (test #'char-equal)) => boolean`

Returns T if sequence ends with suffix.

```lisp
(ends-with-p "hello.txt" ".txt") ; => T
(ends-with-p "README" "me")      ; => T
```

#### contains-p
`(contains-p haystack needle &key (test #'char=)) => boolean`

Returns T if haystack contains needle.

```lisp
(contains-p "hello world" "world") ; => T
(contains-p "hello" "ell")         ; => T
```

### Character Access

#### first-char
`(first-char s) => character or nil`

Return the first character of a non-empty string, or nil.

```lisp
(first-char "hello") ; => #\h
(first-char "")      ; => NIL
```

#### last-char
`(last-char s) => character or nil`

Return the last character of a non-empty string, or nil.

```lisp
(last-char "hello") ; => #\o
(last-char "x")     ; => #\x
```

### String Trimming

#### trim
`(trim string &optional (char-bag +whitespace-char-string+)) => string`

Remove characters from both ends of string.

```lisp
(trim "  hello  ")     ; => "hello"
(trim "...text..." ".") ; => "text"
```

#### strip
`(strip string c) => string`

Remove leading and trailing occurrences of character c.

```lisp
(strip "***text***" #\*) ; => "text"
(strip "___test___" #\_) ; => "test"
```

#### strip-left
`(strip-left string c) => string`

Remove leading occurrences of character c.

```lisp
(strip-left "...hello" #\.) ; => "hello"
(strip-left "###comment" #\#) ; => "comment"
```

#### strip-right
`(strip-right string c) => string`

Remove trailing occurrences of character c.

```lisp
(strip-right "hello..." #\.) ; => "hello"
(strip-right "data;;;" #\;)  ; => "data"
```

### Case Conversion

#### upcase
`(upcase string) => string`

Convert string to uppercase.

```lisp
(upcase "hello")     ; => "HELLO"
(upcase "CamelCase") ; => "CAMELCASE"
```

#### downcase
`(downcase string) => string`

Convert string to lowercase.

```lisp
(downcase "HELLO")     ; => "hello"
(downcase "CamelCase") ; => "camelcase"
```

#### capitalize
`(capitalize string) => string`

Capitalize the first letter of each word.

```lisp
(capitalize "hello world")     ; => "Hello World"
(capitalize "the quick brown") ; => "The Quick Brown"
```

#### titlecase
`(titlecase string) => string`

Convert string to title case (currently same as capitalize).

```lisp
(titlecase "hello world") ; => "Hello World"
```

### String Replacement

#### replace
`(replace string old new &key (all nil) (count 1)) => string`

Replace occurrences of old with new in string.

```lisp
(replace "hello world" "o" "0")           ; => "hell0 world"
(replace "hello world" "o" "0" :all t)    ; => "hell0 w0rld"
(replace "aaaa" "a" "b" :count 2)         ; => "bbaa"
```

#### replace-all
`(replace-all string old new) => string`

Replace all occurrences of old with new.

```lisp
(replace-all "hello world" "o" "0")  ; => "hell0 w0rld"
(replace-all "foo bar foo" "foo" "baz") ; => "baz bar baz"
```

#### replace-first
`(replace-first string old new) => string`

Replace the first occurrence of old with new.

```lisp
(replace-first "hello world" "o" "0") ; => "hell0 world"
(replace-first "aaaa" "a" "b")        ; => "baaa"
```

### Substring Operations

#### substring
`(substring string start &optional end) => string`

Extract substring from start to end position.

```lisp
(substring "hello" 1 4)  ; => "ell"
(substring "hello" 2)    ; => "llo"
```

#### substring-before
`(substring-before string delimiter) => string or nil`

Return substring before first occurrence of delimiter.

```lisp
(substring-before "hello@world" "@")  ; => "hello"
(substring-before "test.txt" ".")     ; => "test"
```

#### substring-after
`(substring-after string delimiter) => string or nil`

Return substring after first occurrence of delimiter.

```lisp
(substring-after "hello@world" "@")   ; => "world"
(substring-after "prefix:value" ":")  ; => "value"
```

#### substring-between
`(substring-between string start-delim end-delim) => string or nil`

Return substring between start and end delimiters.

```lisp
(substring-between "[hello]" "[" "]")     ; => "hello"
(substring-between "<tag>text</tag>" ">" "<") ; => "text"
```

### Search Operations

#### index-of
`(index-of string substring &key (start 0)) => integer or nil`

Find index of first occurrence of substring.

```lisp
(index-of "hello world" "world")    ; => 6
(index-of "hello" "ll")             ; => 2
(index-of "hello" "x")              ; => NIL
```

#### last-index-of
`(last-index-of string substring) => integer or nil`

Find index of last occurrence of substring.

```lisp
(last-index-of "hello world" "o")   ; => 7
(last-index-of "aaaa" "aa")         ; => 2
```

#### count-substring
`(count-substring string substring) => integer`

Count non-overlapping occurrences of substring.

```lisp
(count-substring "hello world" "o")  ; => 2
(count-substring "aaaa" "aa")        ; => 2
```

### String Comparison

#### equals-ignore-case
`(equals-ignore-case string1 string2) => boolean`

Compare strings case-insensitively.

```lisp
(equals-ignore-case "Hello" "hello") ; => T
(equals-ignore-case "ABC" "abc")     ; => T
```

#### compare
`(compare string1 string2) => integer`

Compare strings lexicographically. Returns -1, 0, or 1.

```lisp
(compare "aaa" "bbb")  ; => -1
(compare "hello" "hello") ; => 0
(compare "zzz" "aaa")  ; => 1
```

#### compare-ignore-case
`(compare-ignore-case string1 string2) => integer`

Compare strings lexicographically, ignoring case.

```lisp
(compare-ignore-case "AAA" "aaa") ; => 0
(compare-ignore-case "ABC" "def") ; => -1
```

### String Transformation

#### reverse
`(reverse string) => string`

Reverse the characters in string.

```lisp
(reverse "hello") ; => "olleh"
(reverse "12345") ; => "54321"
```

#### pad-left
`(pad-left string width &optional (padding-char #\Space)) => string`

Pad string on the left to reach width.

```lisp
(pad-left "hello" 10)      ; => "     hello"
(pad-left "42" 5 #\0)      ; => "00042"
```

#### pad-right
`(pad-right string width &optional (padding-char #\Space)) => string`

Pad string on the right to reach width.

```lisp
(pad-right "hello" 10)     ; => "hello     "
(pad-right "test" 8 #\.)   ; => "test...."
```

#### center
`(center string width &optional (padding-char #\Space)) => string`

Center string within width using padding.

```lisp
(center "hello" 11)        ; => "   hello   "
(center "x" 5 #\-)         ; => "--x--"
```

### Character Classification

#### digit-char-p
`(digit-char-p chr) => boolean`

Test if character is a decimal digit (0-9).

```lisp
(digit-char-p #\5)  ; => T
(digit-char-p #\a)  ; => NIL
```

#### word-char-p
`(word-char-p chr) => boolean`

Test if character is alphanumeric or underscore.

```lisp
(word-char-p #\a)  ; => T
(word-char-p #\5)  ; => T
(word-char-p #\_)  ; => T
(word-char-p #\!)  ; => NIL
```

#### whitespacep
`(whitespacep chr) => boolean`

Test if character is whitespace.

```lisp
(whitespacep #\Space)   ; => T
(whitespacep #\Tab)     ; => T
(whitespacep #\a)       ; => NIL
```

### Byte Conversion

#### string-to-octets
`(string-to-octets string &key (encoding :utf-8)) => vector`

Convert string to byte array.

```lisp
(string-to-octets "hello") ; => #(104 101 108 108 111)
```

#### octets-to-string
`(octets-to-string octets &key (encoding :utf-8)) => string`

Convert byte array to string.

```lisp
(octets-to-string #(104 101 108 108 111)) ; => "hello"
```

### Utility Functions

#### nsubseq
`(nsubseq sequence start &optional end) => sequence`

Return subsequence by creating displaced array (no copying).

```lisp
(nsubseq "hello world" 6 11) ; => "world"
```

## Performance Notes

- `split` returns a lazy sequence for memory efficiency with large strings
- `nsubseq` creates displaced arrays without copying data
- Most functions handle Unicode correctly but operate at the character level
- Case conversion functions use Common Lisp's built-in Unicode support

## Usage Notes

1. Use local nicknames for brevity
2. `split` returns lazy sequences
3. `nsubseq` creates displaced arrays without copying
4. Default string comparison is case-insensitive; use `:test #'char=` for case-sensitive

## See Also

- [epsilon.char](epsilon.char.md) - Character operations and Unicode support
- [epsilon.regex](epsilon.regex.md) - Regular expression matching
- [epsilon.sequence](epsilon.sequence.md) - Lazy sequence operations