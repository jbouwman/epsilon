# Epsilon XML Library

A focused XML generation library for creating well-formed XML documents with proper escaping and structured element composition.

## Overview

The Epsilon XML library provides:

- **Structured XML generation** with elements, attributes, and text nodes
- **Automatic character escaping** for XML special characters
- **Composable element trees** for building complex documents  
- **Streaming output** to any Common Lisp stream
- **Clean object-oriented design** with generic functions
- **Memory-efficient emission** without DOM overhead

**Note**: This library currently focuses on XML emission/generation. XML parsing capabilities are not yet implemented.

## Quick Start

### Basic XML Generation

```lisp
(use-package :epsilon.xml)

;; Create simple elements
(let ((title (element :title :children (list (text "Hello World"))))
      (author (element :author :children (list (text "Alice")))))
  (emit title *standard-output*)
  (emit author *standard-output*))

;; Output:
;; <title>Hello World</title>
;; <author>Alice</author>
```

### Elements with Attributes

```lisp
;; Create elements with attributes
(let ((link (element :a 
                     :attributes (list :href "https://example.com"
                                      :target "_blank")
                     :children (list (text "Visit Example")))))
  (emit link *standard-output*))

;; Output:
;; <a href="https://example.com" target="_blank">Visit Example</a>
```

### Nested Document Structure

```lisp
;; Build complex nested structures
(let ((document
        (element :html
                 :children
                 (list (element :head
                               :children
                               (list (element :title
                                             :children
                                             (list (text "My Page")))))
                       (element :body
                               :children
                               (list (element :h1
                                             :children
                                             (list (text "Welcome")))
                                     (element :p
                                             :children
                                             (list (text "Hello world!")))))))))
  (emit document *standard-output*))

;; Output:
;; <html><head><title>My Page</title>
;; </head>
;; <body><h1>Welcome</h1>
;; <p>Hello world!</p>
;; </body>
;; </html>
```

## Core API

### Element Construction

**`element`** `(tag &key attributes children)`

Create an XML element with tag name, optional attributes, and child nodes:

```lisp
;; Simple element
(element :div)  ; <div/>

;; Element with attributes
(element :div :attributes (list :class "container" :id "main"))
; <div class="container" id="main"/>

;; Element with children
(element :div :children (list (text "Content")))
; <div>Content</div>

;; Element with both
(element :div 
         :attributes (list :class "box")
         :children (list (element :span :children (list (text "Inner")))))
; <div class="box"><span>Inner</span></div>
```

**`text`** `(content)`

Create XML text content with automatic escaping:

```lisp
(text "Hello World")           ; Hello World
(text "Tom & Jerry")           ; Tom &amp; Jerry  
(text "<script>alert()</script>")  ; &lt;script&gt;alert()&lt;/script&gt;
```

**`attribute`** `(name value)`

Create attribute name-value pairs (helper function):

```lisp
(attribute :href "http://example.com")  ; => (:href "http://example.com")

;; Use in element construction
(element :a 
         :attributes (list (attribute :href "http://example.com")
                          (attribute :target "_blank"))
         :children (list (text "Link")))
```

### Document Emission

**`emit`** `(node stream)`

Emit XML node to a stream with proper formatting:

```lisp
;; Emit to standard output
(emit xml-element *standard-output*)

;; Emit to string
(with-output-to-string (stream)
  (emit xml-element stream))

;; Emit to file
(with-open-file (stream "output.xml" :direction :output
                       :if-exists :supersede)
  (emit xml-element stream))
```

## Data Structures

### XML Node Classes

**`xml-node`** - Abstract base class for all XML nodes

**`xml-element`** - Represents XML elements with tag, attributes, and children
- `tag` - Element tag name (symbol or string)
- `attributes` - Property list of attribute name-value pairs  
- `children` - List of child nodes (elements or text)

**`xml-text`** - Represents text content
- `content` - String content (automatically escaped on emission)

### Class Hierarchy

```
xml-node
├── xml-element    ; <tag attr="value">children</tag>
└── xml-text       ; escaped text content
```

## Advanced Usage

### Building Document Trees

```lisp
;; Helper function for creating structured documents
(defun make-html-document (title body-content)
  (element :html
           :children
           (list (element :head
                         :children
                         (list (element :title
                                       :children (list (text title)))))
                 (element :body
                         :children body-content))))

;; Create a blog post
(defun make-blog-post (title content author)
  (make-html-document 
    title
    (list (element :article
                   :children
                   (list (element :h1 :children (list (text title)))
                         (element :div 
                                 :attributes (list :class "content")
                                 :children (list (text content)))
                         (element :footer
                                 :children
                                 (list (text (format nil "By ~A" author)))))))))

(emit (make-blog-post "Hello World" "This is my first post." "Alice")
      *standard-output*)
```

### Conditional Elements

```lisp
;; Generate elements conditionally
(defun make-user-profile (name &key email phone)
  (element :div
           :attributes (list :class "profile")
           :children
           (append (list (element :h2 :children (list (text name))))
                   (when email 
                     (list (element :p 
                                   :children 
                                   (list (text (format nil "Email: ~A" email))))))
                   (when phone
                     (list (element :p
                                   :children
                                   (list (text (format nil "Phone: ~A" phone)))))))))

(emit (make-user-profile "Alice" :email "alice@example.com") *standard-output*)
```

### Dynamic Content Generation

```lisp
;; Generate XML from data structures
(defun make-table (headers rows)
  (element :table
           :children
           (cons (element :thead
                         :children
                         (list (element :tr
                                       :children
                                       (mapcar (lambda (header)
                                                (element :th 
                                                        :children 
                                                        (list (text header))))
                                              headers))))
                 (list (element :tbody
                               :children
                               (mapcar (lambda (row)
                                        (element :tr
                                                :children
                                                (mapcar (lambda (cell)
                                                         (element :td
                                                                 :children
                                                                 (list (text (princ-to-string cell)))))
                                                       row)))
                                      rows))))))

(let ((data '(("Alice" 25 "Engineer")
              ("Bob" 30 "Designer") 
              ("Carol" 28 "Manager"))))
  (emit (make-table '("Name" "Age" "Role") data) *standard-output*))
```

## Character Escaping

The library automatically escapes XML special characters in text content and attribute values:

```lisp
;; Special characters are automatically escaped
(emit (text "Tom & Jerry say \"Hello\" <world>!") *standard-output*)
;; Output: Tom &amp; Jerry say &quot;Hello&quot; &lt;world&gt;!

;; Attribute values are also escaped
(emit (element :div :attributes (list :data "value with <>&\"'"))
      *standard-output*)
;; Output: <div data="value with &lt;&gt;&amp;&quot;&apos;"/>
```

### Escape Mappings

- `&` → `&amp;`
- `<` → `&lt;`  
- `>` → `&gt;`
- `"` → `&quot;`
- `'` → `&apos;`

## Output Formatting

### Self-Closing Elements

Elements without children automatically use self-closing syntax:

```lisp
(emit (element :br) *standard-output*)        ; <br/>
(emit (element :img :attributes (list :src "photo.jpg")) *standard-output*)
; <img src="photo.jpg"/>
```

### Elements with Content

Elements with children use full open/close tag syntax:

```lisp
(emit (element :div :children (list (text "content"))) *standard-output*)
; <div>content</div>

(emit (element :div :children nil) *standard-output*)  
; <div></div>   ; Empty but not self-closing
```

## Integration Examples

### Web Response Generation

```lisp
;; Generate HTML responses for web applications
(defun generate-error-page (status-code message)
  (element :html
           :children
           (list (element :head
                         :children
                         (list (element :title 
                                       :children 
                                       (list (text (format nil "Error ~D" status-code))))))
                 (element :body
                         :children
                         (list (element :h1 :children (list (text (format nil "Error ~D" status-code))))
                               (element :p :children (list (text message))))))))

(with-output-to-string (stream)
  (emit (generate-error-page 404 "Page not found") stream))
```

### Configuration File Generation

```lisp
;; Generate XML configuration files
(defun make-database-config (host port database username)
  (element :database-config
           :children
           (list (element :connection
                         :children
                         (list (element :host :children (list (text host)))
                               (element :port :children (list (text (princ-to-string port))))
                               (element :database :children (list (text database)))
                               (element :username :children (list (text username))))))))

(with-open-file (stream "database.xml" :direction :output :if-exists :supersede)
  (emit (make-database-config "localhost" 5432 "myapp" "dbuser") stream))
```

### RSS/Atom Feed Generation

```lisp
;; Generate RSS feeds
(defun make-rss-feed (title description items)
  (element :rss
           :attributes (list :version "2.0")
           :children
           (list (element :channel
                         :children
                         (append (list (element :title :children (list (text title)))
                                      (element :description :children (list (text description))))
                                 (mapcar #'make-rss-item items))))))

(defun make-rss-item (title link description)
  (element :item
           :children
           (list (element :title :children (list (text title)))
                 (element :link :children (list (text link)))
                 (element :description :children (list (text description))))))
```

## Performance Characteristics

### Memory Usage

- **Streaming emission** - No DOM tree kept in memory during output
- **Efficient string handling** - Minimal string copying during escaping
- **Object allocation** - One object per XML node

### Time Complexity

- **Element creation** - O(1) for basic elements
- **Emission** - O(n) where n is total nodes in document
- **Escaping** - O(m) where m is length of text content

### Optimization Tips

```lisp
;; Pre-escape frequently used text if performance critical
(defparameter *cached-title* (text "Frequently Used Title"))

;; Use with-output-to-string for string building
(defun xml-to-string (element)
  (with-output-to-string (stream)
    (emit element stream)))

;; Avoid deeply nested function calls for large documents
(defun build-large-document ()
  (let ((children '()))
    (dotimes (i 1000)
      (push (element :item :children (list (text (format nil "Item ~D" i))))
            children))
    (element :container :children (nreverse children))))
```

## Best Practices

### Document Structure

```lisp
;; Good: Use semantic element names
(element :article
         :children
         (list (element :header :children (list (text "Title")))
               (element :main :children content)
               (element :footer :children (list (text "Footer")))))

;; Good: Use appropriate attributes
(element :img 
         :attributes (list :src "photo.jpg" 
                          :alt "A beautiful sunset"
                          :width "800" 
                          :height "600"))
```

### Content Generation

```lisp
;; Good: Separate data from presentation
(defun user-to-xml (user)
  (element :user
           :attributes (list :id (user-id user))
           :children
           (list (element :name :children (list (text (user-name user))))
                 (element :email :children (list (text (user-email user)))))))

;; Good: Use helper functions for complex structures
(defun make-form-field (type name label &optional value)
  (element :div
           :attributes (list :class "field")
           :children
           (list (element :label 
                         :attributes (list :for name)
                         :children (list (text label)))
                 (element :input
                         :attributes (append (list :type type :name name)
                                           (when value (list :value value)))))))
```

### Error Handling

```lisp
;; Validate input when building elements
(defun safe-element (tag &key attributes children)
  (unless (or (symbolp tag) (stringp tag))
    (error "Tag must be symbol or string: ~A" tag))
  (element tag :attributes attributes :children children))
```

The Epsilon XML library provides reliable XML generation with automatic escaping and clean composition suitable for web applications, configuration files, and data serialization tasks.