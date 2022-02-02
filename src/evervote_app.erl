%%%-------------------------------------------------------------------
%% @doc evervote public API
%% @end
%%%-------------------------------------------------------------------

-module(evervote_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/">>, index_handler, []},
            {<<"/items/[...]">>, item_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        hello_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    evervote_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
