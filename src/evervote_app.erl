%%%-------------------------------------------------------------------
%% @doc evervote public API
%% @end
%%%-------------------------------------------------------------------

-module(evervote_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, _} = cowboy:start_clear(
    hello_listener,
    [{port, 8080}],
    #{env => #{dispatch => routing()}}
  ),
  SupStart = evervote_sup:start_link(),
  {ok, Pid} = voting:start_aggregation_interval(5000),
  register(vote_aggregation_interval, Pid),
  SupStart.

stop(_State) ->
  ok.

%% internal functions
routing() ->
  cowboy_router:compile([
    {'_', [
      {<<"/">>, cowboy_static, {file, "web/index.html"}},
      {<<"/res/[...]">>, cowboy_static, {dir, "web/"}},
      {<<"/stats/[...]">>, stats_handler, []},
      {<<"/items/[...]">>, item_handler, []},
      {<<"/vote">>, vote_handler, []}
    ]}
  ]).