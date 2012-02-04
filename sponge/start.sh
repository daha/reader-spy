#!/bin/sh
script_dirname=`dirname $0`
script_dir=`(cd $script_dirname; pwd)`

cd $script_dir

erl -sname sponge -pa ebin -pa deps/*/ebin \
    -boot start_sasl -s sponge
