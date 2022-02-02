evervote
=====

An OTP application

Build
-----

    $ rebar3 compile
    $ (cd web; elm make src/Main.elm --output elm.js)

Running
-------

The server can easily be started in an interactive shell like this:

    $ rebar3 shell

A handy shortcut to exit this shell is:

    > q().