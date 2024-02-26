#!/usr/bin/env nu

def run_unit_tests [] {
    (sbcl --noinform
          --non-interactive
          --eval "(setf *compile-verbose* nil)"
          --eval "(load \"epsilon.asd\")"
          --eval "(asdf:test-system :epsilon)")
}

def generate_coverage_report [] {
    mkdir target/covearage
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

def make_etags [] {
    rm -f TAGS
    ls src | where type == "file" and name =~ ".lisp" | each {|x| etags -a $x.name}
}

def main [args] {
    match $args {
        "test"     => run_unit_tests,
        "coverage" => generate_coverage_report,
        "tags"     => make_etags
    }
}
