%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aggregate).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([aggregate_incremental/2]).

-include("aggregate.hrl").
-include("voting.hrl").

-record(state, { aggregated :: aggregated_votes() }).

-type state() :: #state{}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, #state{}}.

handle_call({aggregate, incremental, Votes}, _From, State = #state{}) ->
  {ok, Aggregated} = aggregate_incremental(Votes, State#state.aggregated),
  {reply, ok, State#state{aggregated = Aggregated}}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

-spec aggregate_incremental(vote_list(), aggregated_votes()) -> {vote_list(), aggregated_votes()}.
aggregate_incremental(Votes, Aggregated) ->
  NewAggregated = lists:foldl(fun(Vote, Acc) -> aggregate_single(Vote, Acc) end, Aggregated, Votes),
  {ok, NewAggregated}.

-spec aggregate_single(vote(), aggregated_votes()) -> aggregated_votes().
aggregate_single(Vote, Aggregated) ->
  ItemId = Vote#vote.item_id,
  ItemVotes = maps:get(ItemId, Aggregated, #aggregated_vote{item = ItemId}),
  UpdatedItemVotes = ItemVotes#aggregated_vote{votes = ItemVotes#aggregated_vote.votes + 1, score = ItemVotes#aggregated_vote.score + Vote#vote.score},
  maps:put(ItemId, UpdatedItemVotes, Aggregated).

-spec aggregate(vote_list(), aggregate_strategy()) -> ok.
aggregate(Votes, Strategy) ->
  gen_server:cast(?MODULE, {aggregate, Strategy, Votes}).
