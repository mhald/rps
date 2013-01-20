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

-module(rps_reporter).

-author('mhald@mac.com').

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Behavior callbacks

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(guage, {timestamp :: string(),
                value = 0 :: integer()}).

-record(state, {timer              :: any(), 
                interval           :: integer(),
                report_type = raw  :: undefined | atom(),
                name               :: undefined | string(),
                mod_ref            :: undefined | string(),
                hit_list = []      :: list()}).

%% @doc Start server
-spec start_link(list()) -> ok | ignore | {error, any()}.
start_link(List) -> gen_server:start_link({local, ?MODULE}, ?MODULE, List, []).

%% @doc Returns the callback module's state
-spec init(list()) -> {ok, #state{}} | {error, bad_init_state} | {error, any()}.
init(List) ->
    Interval = proplists:get_value(time, List),
    Mod = proplists:get_value(module, List),
    {ok, Timer} = timer:send_interval(Interval, send_upstream),
    {ok, #state{timer=Timer,
                interval=Interval,
                mod_ref=Mod,
                report_type=proplists:get_value(send, List),
                name=proplists:get_value(name, List)}}.

%% @hidden
handle_call(_, _From, State) ->
    {ok, State}.

%%% @hidden
handle_cast({per_second, Count, Timestamp}, #state{hit_list=List} = State) ->
    %io:format("Gathering ~p~n", [List]),
    {noreply, State#state{hit_list = List ++ [#guage{value=Count, timestamp=Timestamp}]}};

handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
handle_info(send_upstream, #state{name=Name, hit_list=List, report_type=Format, mod_ref=Mod} = State) ->
    %io:format("Flush called for ~p with ~p hits~n", [Name, List]),
    gen_server:cast(Mod, report(Format, List)),
    {noreply, State#state{hit_list=[]}}.

report(raw, List) ->
    {raw_stats,
      [{TS, Value} || #guage{timestamp=TS, value=Value} <- List]
    };

report(stats, List) ->
    {stats,
       stats_util:high_low(List)
    }.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
