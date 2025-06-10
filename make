#!/usr/bin/env nu

def build [] {
    (sbcl --noinform
          --non-interactive
          --eval "(require :asdf)"
          --eval "(load \"epsilon.asd\")"
          --eval "(asdf:load-system \"epsilon\" :force t)")
}

def test [] {
    (sbcl --noinform
          --non-interactive
          --eval "(require :asdf)"
          --eval "(require :sb-posix)"
          --eval "(load \"epsilon.asd\")"
          --eval "(asdf:load-system \"epsilon/tests\" :force t)"
          --eval "(sb-posix:exit
                    (if (epsilon.tool.test:run-success-p 
                          (epsilon.tool.test:run-tests))
                        0
                        1))")
}

def coverage [] {
    mkdir target/coverage
    (sbcl --noinform
          --non-interactive
          --eval "(load \"etc/coverage.lisp\")")
    open target/coverage/cover-index.html
}

def main [args] {
    match $args {
        "build"    => build,
        "test"     => test,
        "coverage" => coverage
    }
}
