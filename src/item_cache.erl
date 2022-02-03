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
-behaviour(gen_server).
-export([start_link/0, init/1, stop/0, handle_call/3, handle_cast/2]).
-export([get_all/0, get_random/0, add/1, set_votes/1, clear/0]).
-include("items.hrl").

-record(state, {items = #{} :: items_map()}).

-type state() :: #state{}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(any()) -> {ok, state()}.
init(_Args) ->
  FixtureItems = #{
    1 => #item{id = 1, title = "First item", description = "The first item ever!"},
    2 => #item{id = 2, title = "Second item", description = "This item is second."}
  },
  {ok, #state{items = FixtureItems}}.

-spec handle_call
    ({find, item_id()}, pid(), state()) -> {reply, {ok, item()} | {error, term()}, state()};
    (get_random, pid(), state()) -> {reply, {ok, item()} | {error, term()}, state()};
    (get_all, pid(), state()) -> {reply, items_map(), state()}.
handle_call({find, Id}, _From, Items) ->
  Found = item_lists:find_by_id(Id, Items#state.items),
  {reply, Found, Items};

handle_call(get_random, _From, Items) when map_size(Items#state.items) == 0 ->
  {reply, {error, no_items}, Items};

handle_call(get_random, _From, Items) ->
  Key = ktn_random:pick(maps:keys(Items#state.items)),
  Item = maps:get(Key, Items#state.items),
  {reply, {ok, Item}, Items};

handle_call(get_all, _From, Items) ->
  {reply, Items#state.items, Items}.

-spec handle_cast
    ({add, item()}, state()) -> {noreply, state()};
    ({set_votes, items_map()}, state()) -> {noreply, state()}.
handle_cast({add, Item}, Items) ->
  Id = Item#item.id,
  {noreply, Items#state{items = maps:put(Id, Item, Items#state.items)}};

handle_cast({set_votes, Votes}, State) ->
  {noreply, State#state{items = Votes}}.

stop() ->
  gen_server:stop(?MODULE).

-spec get_all() -> items_map().
get_all() ->
  gen_server:call(?MODULE, get_all).

-spec get_random() -> {ok, item()} | {error, no_items}.
get_random() ->
  gen_server:call(?MODULE, get_random).

-spec add(item()) -> ok.
add(Item) ->
  gen_server:cast(?MODULE, {add, Item}).

-spec set_votes(items_map()) -> ok.
set_votes(Votes) ->
  gen_server:cast(?MODULE, {set_votes, Votes}).

-spec clear() -> ok.
clear() ->
  set_votes(#{}).