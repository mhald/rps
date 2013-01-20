%% Copyright (c) 2013 Martin Hald

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(gen_info).

-author('mhald@mac.com').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Behavior callbacks

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%% @doc Start server
-spec start_link() -> ok | ignore | {error, any()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the callback module's state
-spec init(list()) -> {ok, #state{}} | {error, bad_init_state} | {error, any()}.
init([]) ->
    {ok, #state{}}.

%% @hidden
handle_call(_, _From, State) ->
    {ok, State}.

%% @hidden
handle_info(_, State) ->
    {noreply, State}.

%% @hidden
handle_cast({raw_stats, Stats}, State) ->
    io:format("Raw stats ~p~n", [Stats]),
    {noreply, State};

%% @hidden
handle_cast({stats, Stats}, State) ->
    io:format("Stats ~p~n", [Stats]),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    io:format("Terminating DB~n"),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
