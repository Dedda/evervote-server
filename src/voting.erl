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
-behavior(gen_server).
-export([get_votes/1, add_votes/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #{}}.

handle_call({get_votes, ItemId}, _From, Votes) ->
  {reply, vote_lists:find_votes(ItemId, Votes), Votes};

handle_call(total_votes, _From, Votes) ->
  {reply, vote_lists:total_votes(Votes), Votes};

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast({add_votes, Votes1, Votes2}, Votes) ->
  New1 = vote_lists:add_vote(Votes1, Votes),
  New2 = vote_lists:add_vote(Votes2, New1),
  {noreply, New2};

handle_cast(_, State) ->
  {noreply, State}.

get_votes(ItemId) ->
  gen_server:call(?MODULE, {get_votes, ItemId}).

add_votes(Votes1, Votes2) ->
  gen_server:cast(?MODULE, {add_votes, Votes1, Votes2}).