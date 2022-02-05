%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aggregate).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, stop/0]).
-export([aggregate/2, aggregated_votes/0, votes_count/0]).
-export([aggregate_incremental/2]).

-include("aggregate.hrl").
-include("voting.hrl").

-record(state, {aggregated = #{} :: aggregated_votes()}).

-type state() :: #state{}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, Votes} = db:load_aggregated_votes(),
  io:format("Loaded ~w aggregated vote records from database~n", [length(Votes)]),
  {ok, #state{aggregated = maps:from_list(lists:map(fun(Vote) -> {Vote#aggregated_vote.item, Vote} end, Votes))}}.

handle_call({aggregate, incremental, Votes}, _From, State) ->
  {ok, Aggregated} = aggregate_incremental(Votes, State#state.aggregated),
  {reply, ok, State#state{aggregated = Aggregated}};
handle_call(aggregated_votes, _From, State) ->
  {reply, State#state.aggregated, State};
handle_call(votes_count, _From, State) ->
  Votes = maps:values(State#state.aggregated),
  Count = lists:sum(lists:map(fun(Vote) -> Vote#aggregated_vote.votes end, Votes)),
  {reply, Count, State}.

handle_cast(_Request, State = #state{}) ->
  {noreply, State}.

-spec aggregate_incremental(vote_list(), aggregated_votes()) -> {vote_list(), aggregated_votes()}.
aggregate_incremental(Votes, Aggregated) ->
  NewAggregated = lists:foldl(fun(Vote, Acc) -> aggregate_single(Vote, Acc) end, Aggregated, Votes),
  spawn(
    fun() ->
      dump_to_db(Aggregated)
    end),
  {ok, NewAggregated}.

-spec aggregate_single(vote(), aggregated_votes()) -> aggregated_votes().
aggregate_single(Vote, Aggregated) ->
  ItemId = Vote#vote.item_id,
  ItemVotes = maps:get(ItemId, Aggregated, #aggregated_vote{item = ItemId}),
  UpdatedItemVotes = ItemVotes#aggregated_vote{votes = ItemVotes#aggregated_vote.votes + 1, score = ItemVotes#aggregated_vote.score + Vote#vote.score},
  maps:put(ItemId, UpdatedItemVotes, Aggregated).

-spec aggregate(vote_list(), aggregate_strategy()) -> ok.
aggregate(Votes, Strategy) ->
  gen_server:call(?MODULE, {aggregate, Strategy, Votes}).

aggregated_votes() ->
  gen_server:call(?MODULE, aggregated_votes).

votes_count() ->
  gen_server:call(?MODULE, votes_count).

stop() ->
  gen_server:stop(?MODULE).

dump_to_db(Aggregated) ->
  db:write_aggregated_votes(maps:values(Aggregated)).