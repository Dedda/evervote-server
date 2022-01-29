%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 12:55
%%%-------------------------------------------------------------------
-module(hello_handler).
-author("dedda").

%% API
-behaviour(cowboy_handler).
-export([init/2]).

init(Req, State) ->
  Req1 = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello, world!">>,
    Req
  ),
  {ok, Req1, State}.