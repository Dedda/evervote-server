%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2022 15:03
%%%-------------------------------------------------------------------
-module(content_types).
-author("dedda").

-export([content_type/1]).

-include("mimetypes.hrl").

-spec content_type(mimetype()) -> #{binary() => mimetype()}.
content_type(Mimetype) ->
  #{<<"content-type">> => Mimetype}.
