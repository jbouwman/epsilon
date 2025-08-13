# epsilon.list

Extended list operations including circular list support and property list utilities.

## Overview

The `epsilon.list` package provides enhanced list manipulation functions beyond Common Lisp's standard offerings. It includes utilities for working with association lists, property lists, circular lists, and various list modification macros.

## Quick Start

```lisp
(defpackage :my-app
  (:use :cl)
  (:local-nicknames (:lst :epsilon.list)))

(in-package :my-app)

;; Property list operations
(lst:plist-alist '(:a 1 :b 2))     ; => ((:A . 1) (:B . 2))
(lst:alist-plist '((:a . 1)))      ; => (:A 1)

;; Circular lists
(defparameter *circle* (lst:circular-list 1 2 3))
(lst:circular-list-p *circle*)      ; => T

;; List utilities
(lst:ensure-list 42)                ; => (42)
(lst:ensure-list '(1 2))            ; => (1 2)
```

## Function Reference

### Association List Operations

#### alist-plist
`(alist-plist alist) => plist`

Convert association list to property list.

```lisp
(alist-plist '((:name . "Alice") (:age . 30)))
; => (:NAME "Alice" :AGE 30)
```

#### plist-alist
`(plist-alist plist) => alist`

Convert property list to association list.

```lisp
(plist-alist '(:name "Alice" :age 30))
; => ((:NAME . "Alice") (:AGE . 30))
```

#### assoc-value
`(assoc-value alist key &key (test 'eql)) => value, entry`

SETF-able accessor for association list values.

```lisp
(defparameter *alist* '((:a . 1) (:b . 2)))
(assoc-value *alist* :a)        ; => 1, (:A . 1)
(setf (assoc-value *alist* :c) 3) ; Adds (:c . 3)
```

#### rassoc-value
`(rassoc-value alist value &key (test 'eql)) => key, entry`

SETF-able reverse association list accessor.

```lisp
(rassoc-value '((1 . :one) (2 . :two)) :one)
; => 1, (1 . :ONE)
```

#### racons
`(racons key value ralist) => alist`

Reverse acons - add (VALUE . KEY) to alist.

```lisp
(racons :name "John" '()) ; => (("John" . :NAME))
```

### Property List Operations

#### doplist
`(doplist (key val plist &optional values) &body body)`

Iterate over property list key-value pairs.

```lisp
(doplist (k v '(:a 1 :b 2 :c 3))
  (format t "~A = ~A~%" k v))
; Prints:
; A = 1
; B = 2  
; C = 3
```

#### remove-from-plist
`(remove-from-plist plist &rest keys) => plist`

Return plist without specified keys (non-destructive).

```lisp
(remove-from-plist '(:a 1 :b 2 :c 3) :b)
; => (:A 1 :C 3)
```

#### delete-from-plist
`(delete-from-plist plist &rest keys) => plist`

Remove keys from plist (potentially destructive).

```lisp
(delete-from-plist '(:a 1 :b 2 :c 3) :a :c)
; => (:B 2)
```

#### remove-from-plistf
`(remove-from-plistf place &rest keys)`

Modify macro that removes keys from plist at place.

```lisp
(defparameter *plist* '(:a 1 :b 2 :c 3))
(remove-from-plistf *plist* :b)
*plist* ; => (:A 1 :C 3)
```

#### delete-from-plistf
`(delete-from-plistf place &rest keys)`

Modify macro that destructively removes keys from plist.

### Circular List Operations

#### circular-list
`(circular-list &rest elements) => circular-list`

Create a circular list from elements.

```lisp
(circular-list 1 2 3) ; => circular list #1=(1 2 3 . #1#)
```

#### make-circular-list
`(make-circular-list &rest elements) => circular-list`

Create a circular list (alternative constructor).

```lisp
(make-circular-list 'a 'b) ; => circular list
```

#### circular-list-p
`(circular-list-p object) => boolean`

Return T if object is a circular list.

```lisp
(circular-list-p (circular-list 1 2)) ; => T
(circular-list-p '(1 2 3))            ; => NIL
```

#### circular-tree-p
`(circular-tree-p object) => boolean`

Return T if object contains circular references in its tree structure.

```lisp
(circular-tree-p '(1 2 . #1=(3 . #1#))) ; => T
```

#### circular-list-subseq
`(circular-list-subseq list start end) => list`

Extract a subsequence from a circular list.

```lisp
(circular-list-subseq (circular-list 1 2 3) 0 5)
; => (1 2 3 1 2)
```

### List Type Checking

#### proper-list-p
`(proper-list-p object) => boolean`

Return T if object is a proper list (NIL-terminated, non-circular).

```lisp
(proper-list-p '(1 2 3))    ; => T
(proper-list-p '(1 . 2))    ; => NIL
(proper-list-p (circular-list 1)) ; => NIL
```

#### proper-list-length
`(proper-list-length list) => integer`

Return length of list, signaling error if not proper.

```lisp
(proper-list-length '(a b c))  ; => 3
(proper-list-length '(1 . 2))  ; Signals error
```

#### safe-endp
`(safe-endp x) => boolean`

Safe version of ENDP that checks proper list termination.

```lisp
(safe-endp '())     ; => T
(safe-endp '(1))    ; => NIL
```

#### setp
`(setp object &key (test 'eql)) => boolean`

Test if object is a list with no duplicate elements.

```lisp
(setp '(1 2 3))     ; => T
(setp '(1 2 1))     ; => NIL
```

#### set-equal
`(set-equal list1 list2 &key (test 'eql)) => boolean`

Test if two lists contain the same elements (as sets).

```lisp
(set-equal '(1 2 3) '(3 2 1))  ; => T
(set-equal '(a b) '(a b c))    ; => NIL
```

### List Utilities

#### ensure-list
`(ensure-list list) => list`

If list is already a list, return it. Otherwise wrap in a list.

```lisp
(ensure-list 42)        ; => (42)
(ensure-list '(1 2))    ; => (1 2)
(ensure-list nil)       ; => NIL
```

#### ensure-cons
`(ensure-cons cons) => cons`

If cons is a cons, return it. Otherwise create (cons . nil).

```lisp
(ensure-cons '(1 . 2))  ; => (1 . 2)
(ensure-cons 42)        ; => (42 . NIL)
```

#### ensure-car
`(ensure-car thing) => value`

If thing is a cons, return its car. Otherwise return thing.

```lisp
(ensure-car '(1 2 3))   ; => 1
(ensure-car 42)         ; => 42
```

#### lastcar
`(lastcar list) => value`

Return the last element of a proper list.

```lisp
(lastcar '(1 2 3))      ; => 3
(lastcar '(a))          ; => A
```

#### flatten
`(flatten list) => list`

Flatten nested list structure into a single list.

```lisp
(flatten '((1 2) (3 (4 5))))  ; => (1 2 3 4 5)
(flatten '(a (b) ((c))))      ; => (A B C)
```

#### mappend
`(mappend function list) => list`

Map function over list and append results.

```lisp
(mappend (lambda (x) (list x (* x 2)))
         '(1 2 3))
; => (1 2 2 4 3 6)
```

#### sans
`(sans plist &rest keys) => plist`

Remove keys from property list (non-destructive).

```lisp
(sans '(:a 1 :b 2 :c 3) :b :c)  ; => (:A 1)
```

### Modify Macros

#### appendf
`(appendf place &rest lists)`

Modify macro for APPEND.

```lisp
(defparameter *list* '(1 2))
(appendf *list* '(3 4) '(5))
*list* ; => (1 2 3 4 5)
```

#### nconcf
`(nconcf place &rest lists)`

Modify macro for NCONC (destructive concatenation).

```lisp
(defparameter *list* (list 1 2))
(nconcf *list* (list 3 4))
*list* ; => (1 2 3 4)
```

#### unionf
`(unionf place list &rest args)`

Modify macro for UNION.

```lisp
(defparameter *set* '(1 2 3))
(unionf *set* '(3 4 5))
*set* ; => (1 2 3 4 5)
```

#### nunionf
`(nunionf place list &rest args)`

Modify macro for NUNION (destructive union).

```lisp
(defparameter *set* (list 1 2))
(nunionf *set* (list 2 3))
*set* ; => (1 2 3)
```

#### reversef
`(reversef place)`

Modify macro for REVERSE.

```lisp
(defparameter *list* '(1 2 3))
(reversef *list*)
*list* ; => (3 2 1)
```

#### nreversef
`(nreversef place)`

Modify macro for NREVERSE (destructive reverse).

```lisp
(defparameter *list* (list 1 2 3))
(nreversef *list*)
*list* ; => (3 2 1)
```

## Error Handling

#### malformed-plist
`(malformed-plist plist) => error`

Signal an error for a malformed property list.

```lisp
(malformed-plist '(:a 1 :b))  ; Signals error
```

#### circular-list-error
`(circular-list-error list) => error`

Signal a TYPE-ERROR for a circular list.

## Type Definitions

### proper-list
Type designator for proper lists.

```lisp
(typep '(1 2 3) 'proper-list)     ; => T
(typep '(1 . 2) 'proper-list)     ; => NIL
```

### circular-list
Type designator for circular lists.

```lisp
(typep (circular-list 1) 'circular-list) ; => T
```

## Examples

### Working with Configuration

```lisp
;; Convert between alist and plist formats
(defparameter *config-alist* 
  '((:host . "localhost") 
    (:port . 8080)
    (:debug . t)))

(defparameter *config-plist*
  (alist-plist *config-alist*))
; => (:HOST "localhost" :PORT 8080 :DEBUG T)

;; Remove sensitive keys
(remove-from-plist *config-plist* :debug)
; => (:HOST "localhost" :PORT 8080)
```

### Circular List for Round-Robin

```lisp
(defparameter *servers* 
  (circular-list "server1" "server2" "server3"))

(defun get-next-server ()
  (prog1 (car *servers*)
    (setf *servers* (cdr *servers*))))

(get-next-server) ; => "server1"
(get-next-server) ; => "server2"
(get-next-server) ; => "server3"
(get-next-server) ; => "server1" (cycles back)
```

### Property List Iteration

```lisp
(defparameter *props* '(:width 100 :height 200 :color "blue"))

(doplist (key value *props*)
  (format t "~A: ~A~%" key value))
; WIDTH: 100
; HEIGHT: 200
; COLOR: blue
```

## Performance Notes

- Circular list detection uses the tortoise-and-hare algorithm
- Property list operations are O(n) in list length
- Modify macros may cons new structure or modify in place
- `flatten` is not tail-recursive for deeply nested structures

## Best Practices

1. Use `proper-list-p` before operations that require proper lists
2. Prefer non-destructive operations unless performance critical
3. Use `doplist` for clean property list iteration
4. Be careful with circular lists - many operations will loop forever
5. Consider using `epsilon.map` for large key-value collections

## See Also

- [Common Lisp list operations](http://clhs.lisp.se/Body/c_conses.htm)
- [epsilon.sequence](epsilon.sequence.md) - Lazy sequences
- [epsilon.map](epsilon.map.md) - Hash maps for key-value data