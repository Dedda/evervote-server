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
-export([allowed_methods/2]).
-export([init/2]).

-include("items.hrl").

-type handler_fun() :: fun((term(), term()) -> {ok, term(), term()}).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

-spec init(term(), term()) -> {ok, term(), term()}.
init(Req, State) ->
  Path = cowboy_req:path_info(Req),
  Method = cowboy_req:method(Req),
  io:format("Method: ~w~nPath: ~w~n", [Method, Path]),
  StringPath = lists:map(fun binary_to_list/1, Path),
  HandlerFunc = handler_for_method(Method, StringPath),
  HandlerFunc(Req, State).

-spec handler_for_method(binary(), [string()]) -> handler_fun().
handler_for_method(<<"GET">>, Path) ->
  get_handler_for_path(Path);
handler_for_method(<<"POST">>, Path) ->
  post_handler_for_path(Path).

-spec get_handler_for_path([string()]) -> handler_fun().
get_handler_for_path(Path) ->
  Mapping = #{
    [] => fun index/2,
    ["add_fixtures"] => fun add_fixtures/2
  },
  maps:get(Path, Mapping, fun unknown_path/2).

-spec post_handler_for_path([string()]) -> handler_fun().
post_handler_for_path(Path) ->
  Mapping = #{
    [] => fun add_item/2
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
  Items = maps:values(item_cache:get_all()),
  Json = mochijson:binary_encode(lists:map(fun items:to_binary_map/1, Items)),
  Response = cowboy_req:reply(
    200,
    content_types:json(),
    Json,
    Req
  ),
  {ok, Response, State}.

-spec add_item(term(), term()) -> {ok, term(), term()}.
add_item(Req, State) ->
  {ok, Body, _} = cowboy_req:read_body(Req),
  Json = mochijson2:decode(Body, [{format, map}]),
  Item = items:item_from_binary_map(Json),
  item_cache:add(Item),
  {ok, Req, State}.

-spec add_fixtures(term(), term()) -> {ok, term(), term()}.
add_fixtures(Req, State) ->
  item_cache:add_fixtures(),
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