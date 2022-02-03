FROM erlang:24

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz

RUN gunzip elm.gz
RUN chmod +x elm

RUN wget https://s3.amazonaws.com/rebar3/rebar3
RUN chmod +x rebar3

COPY src src
COPY test test
COPY rebar.config .
COPY build.sh .

RUN mkdir web
COPY web/src web/src
COPY web/*.css web/
COPY web/bootstrap.min.js web/
COPY web/index.html web/
COPY web/elm.json web/

ENV PATH="${PWD}:${PATH}"

EXPOSE 8080

RUN (cd web; ../elm make src/Main.elm --output elm.js)
RUN ./rebar3 compile
RUN ./rebar3 eunit

