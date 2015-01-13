#!/bin/sh
cd `dirname $0`
export HOME=.
exec erl -detached -smp auto +K true -sname flukso -setcookie mycookie -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -boot start_sasl -s reloader -s flukso
