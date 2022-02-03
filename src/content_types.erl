%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 15:03
%%%-------------------------------------------------------------------
-module(content_types).
-author("dedda").

-export([text/0, html/0, json/0]).

-spec text() -> #{binary() => binary()}.
text() -> #{<<"content-type">> => <<"text/plain">>}.

-spec html() -> #{binary() => binary()}.
html() -> #{<<"content-type">> => <<"text/html">>}.

-spec json() -> #{binary() => binary()}.
json() -> #{<<"content-type">> => <<"application/json">>}.