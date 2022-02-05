#!/usr/bin/bash

set -o xtrace

(cd web; elm make src/Main.elm --output app.js)

rebar3 compile
#rebar3 eunit
