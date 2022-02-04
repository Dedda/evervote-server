#!/usr/bin/bash

docker run -i -p 8080:8080 --rm evervote:latest rebar3 shell
