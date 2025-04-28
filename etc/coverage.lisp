(require :asdf)
(require :sb-cover)

(declaim (optimize sb-cover:store-coverage-data))

(load "epsilon.asd")

(asdf:load-system "epsilon" :force t)
(asdf:test-system "epsilon")

(sb-cover:report "target/coverage/")
