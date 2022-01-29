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

init(Req, State) ->
  Path = cowboy_req:path_info(Req),
  StringPath = lists:map(fun binary_to_list/1, Path),
  io:format("PATH: <<~s>>~n", [Path]),
  handle_path(StringPath, Req, State).

-spec handle_path([string()], term(), term()) -> {ok, term(), term()}.
handle_path([], Req, State) ->
  Items = maps:values(item_cache:get_all()),
  Json = mochijson:binary_encode(lists:map(fun items:to_binary_map/1, Items)),
  io:format("json: ~w~n", [Json]),
  Response = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"application/json">>},
    Json,
    Req
  ),
  {ok, Response, State};

handle_path(["add"], Req, State) ->
  Query = maps:from_list(cowboy_req:parse_qs(Req)),
  add_item(Query, Req, State);

handle_path(_, Req, State) ->
  {ok, Req, State}.

-spec add_item(#{ binary() => binary() }, term(), term()) -> {ok, term(), term()}.
add_item(Params, Req, State) ->
  StringParams = maps:map(fun (_Key, Value) -> binary_to_list(Value) end, Params),
  Id = maps:get(<<"id">>, StringParams),
  Title = maps:get(<<"title">>, StringParams),
  Description = maps:get(<<"description">>, StringParams),
  item_cache:add(#item{id = list_to_integer(Id), title = Title, description = Description}),
  io:format("All items: ~n~w~n", [item_cache:get_all()]),
  {ok, Req, State}.