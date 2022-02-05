%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2022 12:36
%%%-------------------------------------------------------------------
-author("dedda").

-type file_type() :: string().

-record(image, { url :: string(), format :: file_type() }).
-type image() :: #image{}.

-record(audio, { url :: string(), format :: file_type() }).
-type audio() :: #audio{}.

-type resource_type() :: image | text | audio.
-type resource_data() :: image() | string() | audio().

-record(item_resource, { type :: resource_type(), data :: resource_data() }).
-type item_resource() :: #item_resource{}.
