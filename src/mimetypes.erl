%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2022 16:10
%%%-------------------------------------------------------------------
-module(mimetypes).
-author("dedda").

%% API
-export([for_filetype/1]).

-include("mimetypes.hrl").
-include("resources.hrl").

-spec for_filetype(file_type()) -> {ok, mimetype()} | {error, unknown}.
for_filetype(Filetype) ->
  Mapping = #{
    "html" => ?HTML,
    "txt" => ?TEXT,
    "json" => ?JSON
  },
  case maps:get(Filetype, Mapping, unknown) of
    unknown -> {error, unknown};
    Mimetype -> {ok, Mimetype}
  end.