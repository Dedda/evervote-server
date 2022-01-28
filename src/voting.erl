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
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).


start_link() ->
  gen_server:start_link({local, vote}, ?MODULE, [], []).

init(_Args) ->
  {ok, []}.

handle_call(_, _From, State) ->
  {reply, ok, State};

handle_call({get_votes, ItemId}, _From, Votes) ->
  Find = fun ({{Id, _, _}, _}) -> Id =:= ItemId end,
  Found = lists:last(lists:filter(Find, Votes)).


handle_cast({vote, {{ItemId_1, Votes_1}, {ItemId_2, Votes_2}}}, State) ->

  {noreply, State}.

