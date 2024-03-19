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
          --eval "(load-epsilon-tests)"
          --eval "(tool.test:run-all-tests)")
}

def coverage [] {
    mkdir target/coverage
    (sbcl --noinform
          --non-interactive
          --eval "(require :sb-cover)"
          --eval "(load \"epsilon.lisp\")"
          --eval "(load-epsilon)"
          --eval "(load-epsilon-tests)"
          --eval "(tool.test:run-all-tests)"
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
