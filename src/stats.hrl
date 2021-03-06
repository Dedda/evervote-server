%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 17:04
%%%-------------------------------------------------------------------
-author("dedda").

-ifndef(STATS_HRL).
-define(STATS_HRL, 1).

-record(stats, {
  item_count :: pos_integer(),
  aggregated_votes_count :: pos_integer(),
  unaggregated_votes_count :: pos_integer()
}).
-type stats() :: #stats{}.

-endif.