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

init(Req, State) ->
  Path = cowboy_req:path_info(Req),
  StringPath = lists:map(fun (Item) -> binary_to_list(Item) end, Path),
  io:format("PATH: ~s~n", [Path]),
  handle_path(StringPath, Req, State).

-spec handle_path(string(), term(), term()) -> {ok, term(), term()}.
handle_path([], Req, State) ->
  {ok, Req, State};

handle_path(["add"], Req, State) ->
  {ok, Req, State};

handle_path(_, Req, State) ->
  {ok, Req, State}.