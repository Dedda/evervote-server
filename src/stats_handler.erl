%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 14:59
%%%-------------------------------------------------------------------
-module(stats_handler).
-author("dedda").

%% API
-behaviour(cowboy_handler).
-export([init/2]).

-include("stats.hrl").

-type handler_fun() :: fun((term(), term()) -> {ok, term(), term()}).

init(Req, State) ->
  Path = cowboy_req:path_info(Req),
  StringPath = lists:map(fun binary_to_list/1, Path),
  HandlerFunc = handler_for_path(StringPath),
  HandlerFunc(Req, State).

-spec handler_for_path([string()]) -> handler_fun().
handler_for_path(Path) ->
  Mapping = #{
    [] => fun index/2
  },
  maps:get(Path, Mapping, fun unknown_path/2).

-spec unknown_path(term(), term()) -> {ok, term(), term()}.
unknown_path(Req, State) ->
  Req1 = cowboy_req:reply(
    200,
    content_types:text(),
    <<"Unknown path requested!">>,
    Req
  ),
  {ok, Req1, State}.

-spec index(term(), term()) -> {ok, term(), term()}.
index(Req, State) ->
  Items = length(maps:keys(item_cache:get_all())),
  Stats = #stats{item_count = Items},
  Json = mochijson:binary_encode(stats:to_map(Stats)),
  Response = cowboy_req:reply(
    200,
    content_types:json(),
    Json,
    Req
  ),
  {ok, Response, State}.
