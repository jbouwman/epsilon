;;;; Comprehensive tests for epsilon.xml

(defpackage epsilon.xml.tests
  (:use cl epsilon.test)
  (:local-nicknames
   (xml epsilon.xml)))

(in-package epsilon.xml.tests)

;;; Helper functions for testing

(defun xml-to-string (node)
  "Convert XML node to string for testing"
  (with-output-to-string (stream)
    (xml:emit node stream)))

(defun normalize-whitespace (string)
  "Normalize whitespace for comparison"
  (string-trim '(#\Space #\Tab #\Newline) string))

;;; Character Escaping Tests

(deftest escape-xml-basic-test
  "Test basic XML character escaping"
  (is-equal (xml::escape-xml "normal text") "normal text")
  (is-equal (xml::escape-xml "Tom & Jerry") "Tom &amp; Jerry")
  (is-equal (xml::escape-xml "<script>") "&lt;script&gt;")
  (is-equal (xml::escape-xml "Quote: \"Hello\"") "Quote: &quot;Hello&quot;")
  (is-equal (xml::escape-xml "Apostrophe: 'Hi'") "Apostrophe: &apos;Hi&apos;"))

(deftest escape-xml-multiple-characters-test
  "Test escaping multiple special characters"
  (is-equal (xml::escape-xml "<tag attr=\"value\" & 'other'>")
            "&lt;tag attr=&quot;value&quot; &amp; &apos;other&apos;&gt;")
  (is-equal (xml::escape-xml "A&B<C>D\"E'F")
            "A&amp;B&lt;C&gt;D&quot;E&apos;F"))

(deftest escape-xml-empty-and-nil-test
  "Test escaping edge cases"
  (is-equal (xml::escape-xml "") "")
  (is-equal (xml::escape-xml "no special chars") "no special chars"))

;;; Data Structure Tests

(deftest xml-element-creation-test
  "Test XML element creation"
  (let ((elem (xml:element :div)))
    (is (typep elem 'xml:xml-element))
    (is-eq (xml::tag elem) :div)
    (is (null (xml::attributes elem)))
    (is (null (xml::children elem)))))

(deftest xml-element-with-attributes-test
  "Test XML element with attributes"
  (let ((elem (xml:element :div :attributes (list :class "container" :id "main"))))
    (is (typep elem 'xml:xml-element))
    (is-eq (xml::tag elem) :div)
    (is-equal (xml::attributes elem) '(:class "container" :id "main"))))

(deftest xml-element-with-children-test
  "Test XML element with children"
  (let ((child (xml:text "Hello"))
        (elem (xml:element :div :children (list (xml:text "Hello")))))
    (is (typep elem 'xml:xml-element))
    (is (= (length (xml::children elem)) 1))
    (is (typep (first (xml::children elem)) 'xml:xml-text))))

(deftest xml-text-creation-test
  "Test XML text node creation"
  (let ((text-node (xml:text "Hello World")))
    (is (typep text-node 'xml:xml-text))
    (is-equal (xml::content text-node) "Hello World")))

(deftest attribute-helper-test
  "Test attribute helper function"
  (let ((attr (xml:attribute :href "http://example.com")))
    (is-equal attr '(:href "http://example.com"))))

;;; Basic Emission Tests

(deftest emit-simple-element-test
  "Test emitting simple elements"
  ;; Self-closing element
  (let ((elem (xml:element :br)))
    (is-equal (xml-to-string elem) "<br/>"))
  
  ;; Element with text content
  (let ((elem (xml:element :p :children (list (xml:text "Hello")))))
    (is-equal (normalize-whitespace (xml-to-string elem)) "<p>Hello</p>")))

(deftest emit-element-with-attributes-test
  "Test emitting elements with attributes"
  (let ((elem (xml:element :div :attributes (list :class "container" :id "main"))))
    (let ((output (xml-to-string elem)))
      (is (search "<div" output))
      (is (search "class=\"container\"" output))
      (is (search "id=\"main\"" output))
      (is (search "/>" output)))))

(deftest emit-element-with-mixed-content-test
  "Test emitting elements with mixed content"
  (let ((elem (xml:element :div 
                           :attributes (list :class "box")
                           :children (list (xml:text "Hello ")
                                           (xml:element :strong 
                                                        :children (list (xml:text "World")))))))
    (let ((output (normalize-whitespace (xml-to-string elem))))
      (is (search "<div class=\"box\">" output))
      (is (search "Hello <strong>World</strong>" output))
      (is (search "</div>" output)))))

(deftest emit-text-node-test
  "Test emitting text nodes"
  (let ((text-node (xml:text "Plain text")))
    (is-equal (xml-to-string text-node) "Plain text"))
  
  ;; Test text with special characters
  (let ((text-node (xml:text "Tom & Jerry <script>")))
    (is-equal (xml-to-string text-node) "Tom &amp; Jerry &lt;script&gt;")))

(deftest emit-string-directly-test
  "Test emitting strings directly"
  (is-equal (xml-to-string "Direct string") "Direct string")
  (is-equal (xml-to-string "String with <tags>") "String with &lt;tags&gt;"))

(deftest emit-null-test
  "Test emitting null values"
  (is-equal (xml-to-string nil) ""))

;;; Nested Structure Tests

(deftest emit-nested-elements-test
  "Test emitting nested element structures"
  (let ((doc (xml:element :html
                          :children
                          (list (xml:element :head
                                             :children
                                             (list (xml:element :title
                                                                :children
                                                                (list (xml:text "Test Page")))))
                                (xml:element :body
                                             :children
                                             (list (xml:element :h1
                                                                :children
                                                                (list (xml:text "Welcome")))))))))
    (let ((output (xml-to-string doc)))
      (is (search "<html>" output))
      (is (search "<head>" output))
      (is (search "<title>Test Page</title>" output))
      (is (search "</head>" output))
      (is (search "<body>" output))
      (is (search "<h1>Welcome</h1>" output))
      (is (search "</body>" output))
      (is (search "</html>" output)))))

(deftest emit-deeply-nested-test
  "Test deeply nested structures"
  (let ((deep-elem
          (xml:element :level1
                       :children
                       (list (xml:element :level2
                                          :children
                                          (list (xml:element :level3
                                                             :children
                                                             (list (xml:element :level4
                                                                                :children
                                                                                (list (xml:text "Deep content")))))))))))
    (let ((output (xml-to-string deep-elem)))
      (is (search "<level1>" output))
      (is (search "<level2>" output))
      (is (search "<level3>" output))
      (is (search "<level4>Deep content</level4>" output))
      (is (search "</level3>" output))
      (is (search "</level2>" output))
      (is (search "</level1>" output)))))

;;; Complex Document Tests

(deftest emit-html-document-test
  "Test emitting a complete HTML document"
  (let ((doc
          (xml:element :html
                       :attributes (list :lang "en")
                       :children
                       (list (xml:element :head
                                          :children
                                          (list (xml:element :meta
                                                             :attributes (list :charset "UTF-8"))
                                                (xml:element :title
                                                             :children (list (xml:text "My Page")))))
                             (xml:element :body
                                          :children
                                          (list (xml:element :h1
                                                             :children (list (xml:text "Hello World")))
                                                (xml:element :p
                                                             :children (list (xml:text "This is a paragraph.")))
                                                (xml:element :ul
                                                             :children
                                                             (list (xml:element :li :children (list (xml:text "Item 1")))
                                                                   (xml:element :li :children (list (xml:text "Item 2")))))))))))
    (let ((output (xml-to-string doc)))
      ;; Check document structure
      (is (search "html lang=\"en\"" output))
      (is (search "<meta charset=\"UTF-8\"/>" output))
      (is (search "<title>My Page</title>" output))
      (is (search "<h1>Hello World</h1>" output))
      (is (search "<p>This is a paragraph.</p>" output))
      (is (search "<ul>" output))
      (is (search "<li>Item 1</li>" output))
      (is (search "<li>Item 2</li>" output))
      (is (search "</ul>" output)))))

(deftest emit-xml-with-namespaces-test
  "Test emitting XML with namespace-like attributes"
  (let ((elem (xml:element :root
                           :attributes (list :xmlns "http://example.com/ns"
                                             "xmlns:prefix" "http://example.com/prefix")
                           :children (list (xml:text "Namespaced content")))))
    (let ((output (xml-to-string elem)))
      (is (search "xmlns=\"http://example.com/ns\"" output))
      (is (search "xmlns:prefix=\"http://example.com/prefix\"" output))
      (is (search "Namespaced content" output)))))

;;; Attribute Handling Tests

(deftest emit-various-attribute-types-test
  "Test emitting elements with various attribute value types"
  (let ((elem (xml:element :div
                           :attributes (list :data-count 42
                                             :enabled t
                                             :disabled nil
                                             :class "test"
                                             :style "color: red;"))))
    (let ((output (xml-to-string elem)))
      (is (search "data-count=\"42\"" output))
      (is (search "enabled=\"T\"" output))
      ;; nil attributes should be skipped
      (is (not (search "disabled" output)))
      (is (search "class=\"test\"" output))
      (is (search "style=\"color: red;\"" output)))))

(deftest emit-attribute-escaping-test
  "Test that attribute values are properly escaped"
  (let ((elem (xml:element :div
                           :attributes (list :title "Tom & Jerry \"quotes\" <tags>"
                                             :data "value with 'apostrophes'"))))
    (let ((output (xml-to-string elem)))
      (is (search "title=\"Tom &amp; Jerry &quot;quotes&quot; &lt;tags&gt;\"" output))
      (is (search "data=\"value with &apos;apostrophes&apos;\"" output)))))

(deftest emit-empty-attributes-test
  "Test handling of empty and edge-case attributes"
  (let ((elem (xml:element :div
                           :attributes (list :empty ""
                                             :space " "
                                             :number 0))))
    (let ((output (xml-to-string elem)))
      (is (search "empty=\"\"" output))
      (is (search "space=\" \"" output))
      (is (search "number=\"0\"" output)))))

;;; Self-Closing vs. Full Tag Tests

(deftest self-closing-elements-test
  "Test self-closing element behavior"
  ;; Elements without children should self-close
  (is-equal (xml-to-string (xml:element :br)) "<br/>")
  (is-equal (xml-to-string (xml:element :img :attributes (list :src "photo.jpg")))
            "<img src=\"photo.jpg\"/>")
  
  ;; Elements with empty children list should use full tags
  (let ((elem (xml:element :div :children '())))
    (let ((output (normalize-whitespace (xml-to-string elem))))
      (is (search "<div></div>" output)))))

(deftest full-tag-elements-test
  "Test full tag element behavior"
  ;; Elements with children should use full open/close tags
  (let ((elem (xml:element :div :children (list (xml:text "content")))))
    (let ((output (normalize-whitespace (xml-to-string elem))))
      (is (search "<div>content</div>" output))))
  
  ;; Even with whitespace-only content
  (let ((elem (xml:element :div :children (list (xml:text " ")))))
    (let ((output (xml-to-string elem))))
    (is (search "<div> </div>" output))))

;;; Stream Output Tests

(deftest emit-to-different-streams-test
  "Test emitting to different stream types"
  (let ((elem (xml:element :test :children (list (xml:text "content")))))
    
    ;; Test string stream
    (let ((output (with-output-to-string (stream)
                    (xml:emit elem stream))))
      (is (search "<test>content</test>" output)))
    
    ;; Test file stream
    (let ((temp-file "/tmp/xml-test.xml"))
      (unwind-protect
           (progn
             (with-open-file (stream temp-file :direction :output :if-exists :supersede)
               (xml:emit elem stream))
             (with-open-file (stream temp-file :direction :input)
               (let ((content (read-line stream nil)))
                 (is (search "<test>content</test>" content)))))
        ;; Clean up
        (when (probe-file temp-file)
          (delete-file temp-file))))))

;;; Error Handling and Edge Cases

(deftest emit-with-special-tag-names-test
  "Test emitting elements with various tag name types"
  ;; Symbol tags
  (is-equal (xml-to-string (xml:element :symbol-tag)) "<symbol-tag/>")
  
  ;; String tags
  (is-equal (xml-to-string (xml:element "string-tag")) "<string-tag/>")
  
  ;; Keywords work as expected
  (is-equal (xml-to-string (xml:element :keyword-tag)) "<keyword-tag/>"))

(deftest emit-large-content-test
  "Test emitting elements with large content"
  (let ((large-text (make-string 10000 :initial-element #\X))
        (elem (xml:element :large :children (list (xml:text (make-string 10000 :initial-element #\X))))))
    (let ((output (xml-to-string elem)))
      (is (> (length output) 10000))
      (is (search "<large>" output))
      (is (search "</large>" output))
      (is (search (subseq large-text 0 100) output)))))

;;; Complex Construction Tests

(deftest build-document-programmatically-test
  "Test building documents programmatically"
  (let ((items '("Apple" "Banana" "Cherry"))
        (list-elem (xml:element :ul)))
    
    ;; Build list items dynamically
    (let ((list-items (mapcar (lambda (item)
                                (xml:element :li :children (list (xml:text item))))
                              items)))
      (let ((full-list (xml:element :ul :children list-items)))
        (let ((output (xml-to-string full-list)))
          (is (search "<ul>" output))
          (is (search "<li>Apple</li>" output))
          (is (search "<li>Banana</li>" output))
          (is (search "<li>Cherry</li>" output))
          (is (search "</ul>" output)))))))

(deftest conditional-element-construction-test
  "Test conditional element construction"
  (labels ((make-user-profile (name &key email phone)
             (xml:element :div
                          :attributes (list :class "profile")
                          :children
                          (append (list (xml:element :h2 :children (list (xml:text name))))
                                  (when email 
                                    (list (xml:element :p :children (list (xml:text (format nil "Email: ~A" email))))))
                                  (when phone
                                    (list (xml:element :p :children (list (xml:text (format nil "Phone: ~A" phone))))))))))
    
    ;; Test with all fields
    (let ((full-profile (make-user-profile "Alice" :email "alice@example.com" :phone "555-1234")))
      (let ((output (xml-to-string full-profile)))
        (is (search "<h2>Alice</h2>" output))
        (is (search "Email: alice@example.com" output))
        (is (search "Phone: 555-1234" output))))
    
    ;; Test with partial fields
    (let ((partial-profile (make-user-profile "Bob" :email "bob@example.com")))
      (let ((output (xml-to-string partial-profile)))
        (is (search "<h2>Bob</h2>" output))
        (is (search "Email: bob@example.com" output))
        (is (not (search "Phone:" output)))))))

;;; Data-Driven Generation Tests

(deftest table-generation-test
  "Test generating tables from data"
  (labels ((make-table (headers rows)
             (xml:element :table
                          :children
                          (cons (xml:element :thead
                                             :children
                                             (list (xml:element :tr
                                                                :children
                                                                (mapcar (lambda (header)
                                                                          (xml:element :th :children (list (xml:text header))))
                                                                        headers))))
                                (list (xml:element :tbody
                                                   :children
                                                   (mapcar (lambda (row)
                                                             (xml:element :tr
                                                                          :children
                                                                          (mapcar (lambda (cell)
                                                                                    (xml:element :td
                                                                                                 :children
                                                                                                 (list (xml:text (princ-to-string cell)))))
                                                                                  row)))
                                                           rows)))))))
    
    (let ((data '(("Alice" 25 "Engineer")
                  ("Bob" 30 "Designer")))
          (headers '("Name" "Age" "Role")))
      (let ((table (make-table headers data)))
        (let ((output (xml-to-string table)))
          (is (search "<table>" output))
          (is (search "<thead>" output))
          (is (search "<th>Name</th>" output))
          (is (search "<th>Age</th>" output))
          (is (search "<th>Role</th>" output))
          (is (search "<tbody>" output))
          (is (search "<td>Alice</td>" output))
          (is (search "<td>25</td>" output))
          (is (search "<td>Engineer</td>" output))
          (is (search "<td>Bob</td>" output)))))))

;;; Performance Tests

(deftest large-document-performance-test
  "Test performance with large documents"
  (let ((start-time (get-internal-real-time)))
    
    ;; Create a large document
    (let ((large-doc
            (xml:element :root
                         :children
                         (loop for i from 1 to 1000
                               collect (xml:element :item
                                                    :attributes (list :id i)
                                                    :children (list (xml:text (format nil "Item ~D" i))))))))
      
      ;; Emit to string
      (let ((output (xml-to-string large-doc)))
        (is (> (length output) 50000))  ; Should be substantial output
        (is (search "<root>" output))
        (is (search "</root>" output))
        (is (search "Item 1" output))
        (is (search "Item 1000" output)))
      
      ;; Check that it didn't take too long (more than 1 second would be concerning)
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (is (< elapsed 1.0))))))

;;; Integration Tests

(deftest xml-generation-pipeline-test
  "Test complete XML generation pipeline"
  ;; Simulate generating a configuration file
  (labels ((make-config (settings)
             (xml:element :configuration
                          :children
                          (mapcar (lambda (setting)
                                    (destructuring-bind (section-name section-data) setting
                                      (xml:element :section
                                                   :attributes (list :name section-name)
                                                   :children
                                                   (mapcar (lambda (item)
                                                             (destructuring-bind (key value) item
                                                               (xml:element :setting
                                                                            :attributes (list :key key)
                                                                            :children (list (xml:text (princ-to-string value))))))
                                                           section-data))))
                                  settings))))
    
    (let ((config-data '(("database" (("host" "localhost")
                                      ("port" 5432)
                                      ("name" "myapp")))
                         ("logging" (("level" "INFO")
                                     ("file" "/var/log/app.log"))))))
      (let ((config-xml (make-config config-data)))
        (let ((output (xml-to-string config-xml)))
          (is (search "<configuration>" output))
          (is (search "section name=\"database\"" output))
          (is (search "setting key=\"host\"" output))
          (is (search ">localhost<" output))
          (is (search "setting key=\"port\"" output))
          (is (search ">5432<" output))
          (is (search "section name=\"logging\"" output))
          (is (search ">INFO<" output)))))))

;;; Compatibility and Standards Tests

(deftest xml-standards-compliance-test
  "Test basic XML standards compliance"
  ;; Test that output looks like valid XML structure
  (let ((doc (xml:element :root
                          :attributes (list :version "1.0")
                          :children
                          (list (xml:element :child1 :children (list (xml:text "content1")))
                                (xml:element :child2)
                                (xml:element :child3 :children (list (xml:text "content3")))))))
    (let ((output (xml-to-string doc)))
      ;; Basic structure checks
      (is (char= (char output 0) #\<))  ; Starts with <
      (is (search "<root" output))      ; Has root element
      (is (search "</root>" output))    ; Properly closed
      (is (search "version=\"1.0\"" output))  ; Attributes quoted
      (is (search "<child2/>" output))  ; Self-closing elements
      
      ;; Check proper nesting
      (let ((root-start (search "<root" output))
            (root-end (search "</root>" output)))
        (is (< root-start root-end)))  ; Proper order
      
      ;; All opening tags have corresponding closing tags or are self-closed
      (is (or (search "<child1>" output) (search "<child1/>" output)))
      (is (or (search "</child1>" output) (search "<child1/>" output))))))

(deftest xml-character-encoding-test
  "Test proper character encoding in XML output"
  ;; Test various Unicode and special characters
  (let ((elem (xml:element :test
                           :children (list (xml:text "Unicode: αβγ Emoji: 🌟 Special: ©®™")))))
    (let ((output (xml-to-string elem)))
      ;; Should preserve Unicode characters
      (is (search "αβγ" output))
      ;; Note: Emoji handling depends on Lisp implementation
      ;; Focus on basic character preservation
      (is (search "©®™" output)))))

(deftest xml-empty-elements-consistency-test
  "Test consistency in empty element handling"
  ;; Various ways to create empty elements should behave consistently
  (let ((empty1 (xml:element :empty))
        (empty2 (xml:element :empty :children nil))
        (empty3 (xml:element :empty :children '())))
    
    ;; All should self-close
    (is-equal (xml-to-string empty1) "<empty/>")
    
    ;; empty2 and empty3 should use full tags (they have children list, even if empty)
    (let ((output2 (normalize-whitespace (xml-to-string empty2)))
          (output3 (normalize-whitespace (xml-to-string empty3))))
      (is (search "<empty></empty>" output2))
      (is (search "<empty></empty>" output3)))))
