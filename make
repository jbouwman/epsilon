#!/bin/zsh

# Default values
ERROR_BEHAVIOR="halt"
WARN_BEHAVIOR="ignore"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --error)
            ERROR_BEHAVIOR="$2"
            shift 2
            ;;
        --warn)
            WARN_BEHAVIOR="$2"
            shift 2
            ;;
        build|test|coverage)
            COMMAND="$1"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Validate error behavior
case "$ERROR_BEHAVIOR" in
    halt|ignore|print) ;;
    *)
        echo "Invalid error behavior: $ERROR_BEHAVIOR (must be halt, ignore, or print)"
        exit 1
        ;;
esac

# Validate warning behavior  
case "$WARN_BEHAVIOR" in
    halt|ignore|print) ;;
    *)
        echo "Invalid warning behavior: $WARN_BEHAVIOR (must be halt, ignore, or print)"
        exit 1
        ;;
esac

build() {
    local build_eval="(epsilon.tool.build:build :error-behavior :${ERROR_BEHAVIOR} :warning-behavior :${WARN_BEHAVIOR})"
    
    sbcl --noinform \
         --non-interactive \
         --eval "(load \"boot.lisp\")" \
         --eval "(load-epsilon)" \
         --eval "$build_eval"
}

test() {
    local build_eval="(epsilon.tool.build:build :error-behavior :${ERROR_BEHAVIOR} :warning-behavior :${WARN_BEHAVIOR})"
    
    if sbcl --noinform \
           --non-interactive \
           --eval "(load \"boot.lisp\")" \
           --eval "(load-epsilon)" \
           --eval "$build_eval" \
           --eval "(sb-posix:exit
                     (if (epsilon.tool.test:run-success-p 
                           (epsilon.tool.test:run-tests))
                         0
                         1))"; then
        exit 0
    else
        exit 1
    fi
}

coverage() {
    mkdir -p target/coverage
    sbcl --noinform \
         --non-interactive \
         --eval "(load \"etc/coverage.lisp\")"
    open target/coverage/cover-index.html
}

# Show usage if no command provided
usage() {
    cat << EOF
Usage: $0 [OPTIONS] COMMAND

Commands:
    build      Build the project
    test       Build and run tests
    coverage   Generate test coverage report

Options:
    --error BEHAVIOR    Error handling: halt (default), ignore, print
    --warn BEHAVIOR     Warning handling: ignore (default), halt, print

Examples:
    $0 build
    $0 build --error print --warn print
    $0 test --error halt --warn halt
EOF
}

# Execute the command
case "$COMMAND" in
    build)
        build
        ;;
    test)
        test
        ;;
    coverage)
        coverage
        ;;
    *)
        usage
        exit 1
        ;;
esac