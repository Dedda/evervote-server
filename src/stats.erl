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
-export([to_map/1]).

-include("stats.hrl").

-spec to_map(stats()) -> #{ atom() => any() }.
to_map(#stats{ item_count = ItemCount }) ->
  #{ item_count => ItemCount }.
