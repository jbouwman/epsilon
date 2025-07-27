#!/bin/sh

set -e

PACKAGES="epsilon.catalog \
         epsilon.core \
           epsilon.crypto \
           epsilon.foreign \
           epsilon.format \
           epsilon.http \
           epsilon.json \
           epsilon.lsp \
           epsilon.monitor \
           epsilon.msgpack \
           epsilon.parsing \
           epsilon.regex \
           epsilon.test \
           epsilon.tls \
           epsilon.web \
           epsilon.websocket \
           epsilon.xml \
           epsilon.yaml"

for x in $PACKAGES; do
   ./epsilon test $x
done
