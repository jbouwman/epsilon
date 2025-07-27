#!/bin/bash
# Epsilon Documentation Development Server
# Serves documentation with live reload

set -e

mkdocs serve --dev-addr 0.0.0.0:8000 --livereload
