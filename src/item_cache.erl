%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 14:21
%%%-------------------------------------------------------------------
-module(item_cache).
-author("dedda").

%% API
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, items}, ?MODULE, []).

init(_Args) ->
  {ok, []}.

handle_call({find, _Id}, _From, []) ->
  {reply, {error, no_items}, []};

handle_call({find, Id}, _From, Items) ->
  Filtered = lists:filter(fun ({CurrentId, _, _}) -> CurrentId =:= Id end, Items),
  Item = lists:last(Filtered),
  {reply, {ok, Item}, Items};

handle_call(get_random, _From, []) ->
  {reply, {error, no_items}, []};

handle_call(get_random, _From, Items) ->
  Item = ktn_random:pick(Items),
  {reply, {ok, Item}, Items}.

handle_cast({add, Item}, Items) ->
  {noreply, [Item | Items]}.
