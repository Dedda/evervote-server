%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2022 15:59
%%%-------------------------------------------------------------------
-author("dedda").

-ifndef(MIMETYPES_HRL).
-define(MIMETYPES_HRL, 1).

-type mimetype() :: binary().

-define(TEXT, <<"text/plain">>).
-define(HTML, <<"text/html">>).

-define(JSON, <<"application/json">>).

-endif.