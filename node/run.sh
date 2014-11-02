#!/bin/sh
set -e

[ -d node ] && cd node

if [ ! -d ../deps/sync ]; then
  git clone git://github.com/rustyio/sync.git ../deps/sync && make -C ../deps/sync
fi

echo "Starting development console.\n"

REPO=$(dirname $PWD)

exec erl \
  -pa $REPO/ebin \
  -pa $REPO/deps/*/ebin \
  -pa $REPO/apps/*/ebin \
  -eval '[code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || F <- filelib:wildcard("../ebin/*.beam")].' \
  -args_file $REPO/node/system.args
