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
          --eval "(require :asdf)"
          --eval "(load \"epsilon.asd\")"
          --eval "(asdf:test-system \"epsilon\")")
}

def coverage [] {
    mkdir target/coverage
    (sbcl --noinform
          --non-interactive
          --eval "(require :sb-cover)"
          --eval "(load \"epsilon.asd\" :force t)"
          --eval "(asdf:test-system \"epsilon\")"
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