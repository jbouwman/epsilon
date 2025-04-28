#!/usr/bin/env nu

def build [] {
    (sbcl --noinform
          --non-interactive
          --eval "(load \"epsilon.asd\")"
          --eval "(asdf:load-system \"epsilon\" :force t)")
}

def test [] {
    (sbcl --noinform
          --non-interactive
          --eval "(load \"etc/test.lisp\")")
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
