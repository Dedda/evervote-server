%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2022 13:18
%%%-------------------------------------------------------------------
-author("dedda").

-ifndef(ITEMS_HRL).
-define(ITEMS_HRL, 1).

-type item_id() :: pos_integer().

-record(item, {id :: item_id(), title :: string(), description :: string()}).
-type item() :: #item{}.

-type items_map() :: #{ item_id() => item() }.

-endif.