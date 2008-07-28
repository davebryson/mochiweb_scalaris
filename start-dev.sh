#!/bin/sh
cd `dirname $0`

## Set to the Scalaris Boot Node
export BOOT_NODE="boot@localhost"

## Fire it up...
exec erl -setcookie "chocolate chip cookie" \
    -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl \
    -s reloader -s simple_messaging_scalaris -sname scalaris_web
