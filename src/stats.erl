%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 17:14
%%%-------------------------------------------------------------------
-module(stats).
-author("dedda").

%% API
-export([current/0, to_map/1]).

-include("stats.hrl").

-spec current() -> stats().
current() ->
  ItemCount = length(maps:keys(item_cache:get_all())),
  #stats{item_count = ItemCount}.

-spec to_map(stats()) -> #{ atom() => any() }.
to_map(#stats{ item_count = ItemCount }) ->
  #{ item_count => ItemCount }.
