%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 20:18
%%%-------------------------------------------------------------------
-module(vote_lists).
-author("dedda").

%% API
-export([add_vote/2, find_votes/2, total_votes/1]).
-include("items.hrl").

-type votes_map() :: #{ item_id() => integer() }.

-spec add_vote({item_id(), integer()}, votes_map()) -> votes_map().
add_vote({ItemId, ItemVotes}, Votes) ->
  NewVotes = find_votes(ItemId, Votes) + ItemVotes,
  maps:put(ItemId, NewVotes, Votes).

-spec find_votes(item_id(), votes_map()) -> integer().
find_votes(ItemId, Votes) ->
  maps:get(ItemId, Votes, 0).

-spec total_votes(votes_map()) -> {pos_integer(), pos_integer()}.
total_votes(Votes) ->
  Keys = length(maps:keys(Votes)),
  VotesTotal = lists:sum(maps:values(Votes)),
  {Keys, VotesTotal}.
