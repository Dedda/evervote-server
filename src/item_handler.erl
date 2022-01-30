%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 15:17
%%%-------------------------------------------------------------------
-module(item_handler).
-author("dedda").

%% API
-behaviour(cowboy_handler).
-export([init/2]).

-include("items.hrl").

-type handler_fun() :: fun((term(), term()) -> {ok, term(), term()}).

-define(CONTENT_TYPE_JSON, #{<<"content-type">> => <<"application/json">>}).
-define(CONTENT_TYPE_TEXT, #{<<"content-type">> => <<"text/plain">>}).

-spec init(term(), term()) -> {ok, term(), term()}.
init(Req, State) ->
  Path = cowboy_req:path_info(Req),
  StringPath = lists:map(fun binary_to_list/1, Path),
  HandlerFunc = handler_for_path(StringPath),
  HandlerFunc(Req, State).

-spec handler_for_path([string()]) -> handler_fun().
handler_for_path(Path) ->
  Mapping = #{
    [] => fun index/2,
    ["add"] => fun add_item/2
  },
  maps:get(Path, Mapping, fun unknown_path/2).

-spec unknown_path(term(), term()) -> {ok, term(), term()}.
unknown_path(Req, State) ->
  Req1 = cowboy_req:reply(
    200,
    ?CONTENT_TYPE_TEXT,
    <<"Unknown path requested!">>,
    Req
  ),
  {ok, Req1, State}.

-spec index(term(), term()) -> {ok, term(), term()}.
index(Req, State) ->
  Items = maps:values(item_cache:get_all()),
  Json = mochijson:binary_encode(lists:map(fun items:to_binary_map/1, Items)),
  Response = cowboy_req:reply(
    200,
    ?CONTENT_TYPE_JSON,
    Json,
    Req
  ),
  {ok, Response, State}.

-spec add_item(term(), term()) -> {ok, term(), term()}.
add_item(Req, State) ->
  Params = maps:from_list(cowboy_req:parse_qs(Req)),
  StringParams = maps:map(fun (_Key, Value) -> binary_to_list(Value) end, Params),
  Id = id(StringParams),
  Title = title(StringParams),
  Description = description(StringParams),
  item_cache:add(#item{id = list_to_integer(Id), title = Title, description = Description}),
  {ok, Req, State}.

-spec id(#{ binary() => string() }) -> string().
id(Params) ->
  maps:get(<<"id">>, Params).

-spec title(#{ binary() => string() }) -> string().
title(Params) ->
  maps:get(<<"title">>, Params).

-spec description(#{ binary() => string() }) -> string().
description(Params) ->
  maps:get(<<"description">>, Params).