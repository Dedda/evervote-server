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
-include("voting.hrl").

-spec init(term(), term()) -> {ok, term(), term()}.
init(Req, State) ->
  Query = cowboy_req:parse_qs(Req),
  QMap = maps:from_list(Query),
  voting:add_votes(parse_vote_map(QMap)),
  {ok, Req, State}.

-spec parse_vote_map(#{binary() => binary()}) -> vote_list().
parse_vote_map(BinMap) ->
  Winner = binary_to_integer(maps:get(<<"winner">>, BinMap)),
  Loser = binary_to_integer(maps:get(<<"loser">>, BinMap)),
  Now = calendar:universal_time(),
  WinnerVote = #vote{item_id = Winner, score = 1, cast = Now},
  LoserVote = #vote{item_id = Loser, score = -1, cast = Now},
  [WinnerVote, LoserVote].