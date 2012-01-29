#!/bin/sh
erl -sname sponge -pa ebin -pa deps/*/ebin \
    -boot start_sasl -s sponge
