%%%-------------------------------------------------------------------
%%% @author Martin Hald <mhald@mac.com>
%%% @doc RPS Server Supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(rps_sup).
-author('mhald@mac.com').

-behaviour(supervisor).

-export([start_link/1, init/1]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc  Starts a new supervisor
-spec start_link(list()) -> {ok, pid()}.
start_link(Options) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
%%-------------------------------------------------------------------
%% SUPERVISOR API
%%-------------------------------------------------------------------
%% @hidden
-spec init(list()) -> {ok, {{one_for_one, 5, 60}, [supervisor:child_spec()]}}.
init(Lists) ->
   Children = [{proplists:get_value(name, List),
                  {rps, start_link, [List]}, permanent, 2000, worker, []}
            || List <- Lists],
  {ok,
    {_SupFlags = {one_for_one, 5, 60}, Children}}.
