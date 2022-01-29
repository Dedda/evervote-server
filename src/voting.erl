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
-export([get_votes/1, add_votes/2]).
-export([start_link/0, init/1, stop/0, handle_call/3, handle_cast/2]).

-include("items.hrl").

-record(state, {votes}).

-type item_vote() :: {item_id(), integer()}.

-type state() :: #state{}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(any()) -> {ok, state()}.
init(_Args) ->
  {ok, #{}}.

-spec handle_call
    ({get_votes, item_id()}, pid(), state()) -> {reply, integer(), state()};
    (total_votes, pid(), state()) -> {reply, {pos_integer(), pos_integer()}, state()}.
handle_call({get_votes, ItemId}, _From, Votes) ->
  {reply, vote_lists:find_votes(ItemId, Votes), Votes};

handle_call(total_votes, _From, Votes) ->
  {reply, vote_lists:total_votes(Votes), Votes};

handle_call(_, _From, State) ->
  {reply, ok, State}.

-spec handle_cast({add_votes, item_vote(), item_vote()}, state()) -> {noreply, state()}.
handle_cast({add_votes, Votes1, Votes2}, Votes) ->
  New1 = vote_lists:add_vote(Votes1, Votes),
  New2 = vote_lists:add_vote(Votes2, New1),
  {noreply, New2};

handle_cast(_, State) ->
  {noreply, State}.

-spec get_votes(item_id()) -> integer().
get_votes(ItemId) ->
  gen_server:call(?MODULE, {get_votes, ItemId}).

-spec add_votes(item_vote(), item_vote()) -> ok.
add_votes(Votes1, Votes2) ->
  gen_server:cast(?MODULE, {add_votes, Votes1, Votes2}).

stop() ->
  gen_server:stop(?MODULE).