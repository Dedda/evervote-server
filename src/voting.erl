%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 14:11
%%%-------------------------------------------------------------------
-module(voting).
-author("dedda").

%% API
-behaviour(gen_server).
-export([start_link/0, init/1, stop/0, handle_call/3, handle_cast/2]).
-export([add_votes/1, votes_count/0, force_aggregation/0]).
-export([start_aggregation_interval/1]).

-include("items.hrl").
-include("voting.hrl").

-record(state, {votes = [] :: vote_list()}).

-type state() :: #state{}.

%% ------------------------------
%% INIT
%% ------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(any()) -> {ok, state()}.
init(_Args) ->
  {ok, #state{votes = []}}.

%% ------------------------------
%% HANDLE CALLS
%% ------------------------------

handle_call(total_votes, _From, Votes) ->
  {reply, length(Votes#state.votes), Votes};

handle_call({add_votes, ToAdd}, _From, Votes) ->
  NewVotes = Votes#state{votes = ToAdd ++ Votes#state.votes},
  {reply, ok, NewVotes};

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast(force_aggregation, State) ->
  aggregate:aggregate(State#state.votes, incremental),
  {noreply, State#state{votes = []}};

handle_cast(_, State) ->
  {noreply, State}.

%% ------------------------------
%% PUBLIC INTERFACE
%% ------------------------------

-spec add_votes([vote()]) -> ok.
add_votes(Votes) ->
  gen_server:call(?MODULE, {add_votes, Votes}).

-spec votes_count() -> pos_integer().
votes_count() ->
  gen_server:call(?MODULE, total_votes).

force_aggregation() ->
  gen_server:cast(?MODULE, force_aggregation).

stop() ->
  gen_server:stop(?MODULE).

%% ------------------------------
%% AGGREGATION TIMER
%% ------------------------------

start_aggregation_interval(Interval) ->
  Pid = spawn(fun() -> aggregate_interval_loop(Interval) end),
  timer:send_after(Interval, Pid, recur),
  {ok, Pid}.

%% ------------------------------
%% INTERNAL
%% ------------------------------

aggregate_interval_loop(Interval) ->
  receive
    recur ->
      voting:force_aggregation(),
      timer:send_after(Interval, self(), recur),
      aggregate_interval_loop(Interval);
    stop -> ok
  end.