%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 15:17
%%%-------------------------------------------------------------------
-module(vote_handler).
-author("dedda").

%% API
-behaviour(cowboy_handler).
-export([init/2]).

-include("items.hrl").

-spec init(term(), term()) -> {ok, term(), term()}.
init(Req, State) ->
  Query = cowboy_req:parse_qs(Req),
  QMap = maps:from_list(Query),
  Winner = binary_to_integer(maps:get(<<"winner">>, QMap)),
  Loser = binary_to_integer(maps:get(<<"loser">>, QMap)),
  io:format("Winner: ~w | Loser: ~w~n", [Winner, Loser]),
  {ok, Req, State}.
