#!/bin/sh

# -mnesia dir "'"$PWD"/Mnesia'"

cd `dirname $0`
exec erl -sname earwig -config $PWD/sys.config -pa _build/default/lib/*/ebin