#!/bin/sh
script_dirname=`dirname $0`
script_dir=`(cd $script_dirname; pwd)`
host=`hostname | cut -d'.' -f1`

cd $script_dir

find data -name $host-\* -size 0 -delete
gzip -q data/$host-*

erl -sname sponge -pa ebin -pa deps/*/ebin \
    -boot start_sasl -s sponge
