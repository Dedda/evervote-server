%%%-------------------------------------------------------------------
%%% @author dedda
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2022 12:50 AM
%%%-------------------------------------------------------------------
-module(db).
-author("dedda").

%% API
-export([init_mnesia/0, db_version/0]).
-export([load_items/0]).

-include("db.hrl").
-include("aggregate.hrl").
-include("items.hrl").

-define(CURRENT_VERSION, 1).
-define(DATA_TABLES, [item, aggregated_vote]).
-define(SEC_10, 10000).

init_mnesia() ->
  ok = create_schema(),
  mnesia:start(),
  ok = create_meta(),
  ok = mnesia:wait_for_tables([meta], ?SEC_10),
  ok = migrate_to_current(),
  ok = mnesia:wait_for_tables(?DATA_TABLES, ?SEC_10),
  ok.

%% ------------------------------
%% INTERNAL
%% ------------------------------

create_schema() ->
  case mnesia:create_schema([node()]) of
    ok -> ok;
    {error, {_, {already_exists, _}}} -> ok;
    Error -> Error
  end.

create_meta() ->
  case mnesia:create_table(meta, [{attributes, record_info(fields, meta)}, {disc_copies, [node()]}]) of
    {aborted, {already_exists, meta}} -> ok;
    {atomic, ok} ->
      mnesia:transaction(write_version(0)),
      ok;
    Error -> Error
  end.

write_version(Version) ->
  fun() ->
    mnesia:write(meta, #meta{key = version, value = Version}, write)
  end.

db_version() ->
  ReadVersion = fun() -> mnesia:read(meta, version, read) end,
  {atomic, [{meta, version, Version}]} = mnesia:transaction(ReadVersion),
  Version.

%% ------------------------------
%% MIGRATIONS
%% ------------------------------

migrate_to_current() ->
  Version = db_version(),
  if
    Version < ?CURRENT_VERSION ->
      io:format("Migrating mnesia from v~w to v~w~n", [Version, Version + 1]),
      migrate_to(Version + 1),
      migrate_to_current();
    Version > ?CURRENT_VERSION ->
      {error, db_too_new};
    true -> ok
  end.

-spec migrate_to(pos_integer()) -> ok.
migrate_to(1) ->
  mnesia:create_table(item, [
    {attributes, record_info(fields, item)},
    {disc_copies, [node()]}
  ]),
  mnesia:create_table(aggregated_vote, [
    {attributes, record_info(fields, aggregated_vote)},
    {disc_copies, [node()]}
  ]),
  ok = mnesia:wait_for_tables([item, aggregated_vote], ?SEC_10),
  {atomic, _} = mnesia:transaction(
    fun() ->
      Version = write_version(1),
      Version()
    end
  ),
  io:format("Migration to v~w OK~n", [1]),
  ok.

%% ------------------------------
%% PUBLIC INTERFACE
%% ------------------------------

load_items() ->
  Select = fun() -> mnesia:select(item, [{'_', [], ['$_']}]) end,
  {atomic, Items} = mnesia:transaction(Select),
  {ok, Items}.