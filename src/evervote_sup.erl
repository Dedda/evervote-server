%%%-------------------------------------------------------------------
%% @doc evervote top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(evervote_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 0,
    period => 1},
  ItemCache = #{
    id => item_cache,
    start => {item_cache, start_link, []}
  },
  Voting = #{
    id => voting,
    start => {voting, start_link, []}
  },
  Aggregate = #{
    id => aggregate,
    start => {aggregate, start_link, []}
  },
  ChildSpecs = [ItemCache, Voting, Aggregate],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
