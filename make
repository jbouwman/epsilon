#!/usr/bin/env nu

def build [] {
    (sbcl --noinform
          --non-interactive
          --eval "(load \"boot.lisp\")"
          --eval "(boot *boot-order*)"
          --eval "(epsilon.tool.build:build)")
}

def test [] {
    try {
       (sbcl --noinform
          --non-interactive
          --eval "(load \"boot.lisp\")"
          --eval "(boot *boot-order*)"
          --eval "(epsilon.tool.build:build)"
          --eval "(sb-posix:exit
                    (if (epsilon.tool.test:run-success-p 
                          (epsilon.tool.test:run-tests))
                        0
                        1))")
    } catch {
      exit 1
    }
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
