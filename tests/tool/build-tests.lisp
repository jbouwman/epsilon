

#++
(sort-sources (source-info (uri:uri "file:///Users/jbouwman/git/epsilon/src")))

#++
(sort-sources (list (make-source-info :uri "a" :hash "a"
                                      :defines "a" :requires nil)
                    (make-source-info :uri "abc" :hash "abc"
                                      :defines "a.b.c" :requires (list "a" "a.b"))
                    (make-source-info :uri "ab" :hash "ab"
                                      :defines "a.b" :requires (list "a"))))


#++
(parse-project-info "/Users/jbouwman/git/epsilon/epsilon.yaml")

#++
(load-source-set (uri:uri "file:///Users/jbouwman/git/epsilon/src"))

(build-order (uri:uri "file:///Users/jbouwman/git/epsilon"))

#++
(status (uri:uri "file:///Users/jbouwman/git/epsilon"))
