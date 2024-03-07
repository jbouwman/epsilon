#!/usr/bin/env nu

def build [] {
    (sbcl --noinform
          --non-interactive
          --eval "(load \"epsilon.lisp\")"
          --eval "(load-epsilon)")
}

def test [] {
    (sbcl --noinform
          --non-interactive
          --eval "(load \"epsilon.lisp\")"
          --eval "(load-epsilon)"
          # ...
          )
}

def coverage [] {
    mkdir target/coverage
    (sbcl --noinform
          --non-interactive
          --eval "(require :sb-cover)"
          --eval "(setf *compile-verbose* nil)"
          --eval "(load \"epsilon.asd\")"
          --eval "(declaim (optimize sb-cover:store-coverage-data))"
          --eval "(asdf:test-system :epsilon :force '(:epsilon))"
          --eval "(sb-cover:report \"target/coverage/\")")
    open target/coverage/cover-index.html
}

def main [args] {
    match $args {
        "build"    => build,
        "test"     => test,
        "coverage" => coverage
    }
}
