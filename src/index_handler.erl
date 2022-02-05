%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jan 2022 12:55
%%%-------------------------------------------------------------------
-module(index_handler).
-author("dedda").

%% API
-behaviour(cowboy_handler).
-export([init/2]).

-include("mimetypes.hrl").
-import(content_types, [content_type/1]).

init(Req, State) ->
  Req1 = cowboy_req:reply(
    200,
    content_type(?HTML),
    index_html(),
    Req
  ),
  {ok, Req1, State}.

-spec index_html() -> binary().
index_html() ->
  {ok, Index} = file:read_file("web/index.html"),
  Index.
