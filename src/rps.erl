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

-module(rps).

-author('mhald@mac.com').

-behaviour(gen_server).

%% API
-export([start_link/1, info/0, incr/0, incr/1, test/0]).

%% Behavior callbacks

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {timer     :: any(), 
                name      :: undefined | string(),
                mod_ref   :: any(),
                timestamp :: string(),
                hits = 0  :: integer()}).

incr() -> gen_server:cast(?MODULE, {incr, 1}).
incr(Count) -> gen_server:cast(?MODULE, {incr, Count}).

info() -> gen_server:call(?MODULE, {info}).

%% @doc Start server listening on IpAddr:Port
-spec start_link(list()) -> ok | ignore | {error, any()}.
start_link(List) -> gen_server:start_link({local, ?MODULE}, ?MODULE, List, []).

%% @doc Returns the callback module's state
-spec init(list()) -> {ok, #state{}} | {error, bad_init_state} | {error, any()}.
init([List]) ->
    {ok, Timer} = timer:send_interval(1000, flush),
    {ok, Pid} = rps_reporter:start_link(List),
    {ok, #state{timer=Timer, 
                name=proplists:get_value(name, List),
                timestamp=timestamp(),
                mod_ref=Pid}}.

handle_call({info}, _From, State) ->
    {reply, erlang:process_info(self()), State};

%% @hidden
handle_call(_, _From, State) ->
    {ok, State}.

%%% @hidden
handle_cast({incr, Count}, #state{hits=Hits} = State) ->
    {noreply, State#state{hits=Hits+Count}};

handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
handle_info(flush, #state{name=Name, hits=Hits, mod_ref=Mod, timestamp=TS} = State) ->
    gen_server:cast(Mod, {per_second, TS, Hits}),
    {noreply, State#state{hits=0, timestamp=timestamp()}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

timestamp() ->
    {{Y,M,D}, {H,Min,S}} = erlang:universaltime(),
    iolist_to_binary([integer_to_list(Y),
                      integer_to_list(M),
                      integer_to_list(D),
                      integer_to_list(H),
                      integer_to_list(Min),
                      integer_to_list(S)
                     ]).

test() ->
    spawn(fun() -> [begin timer:sleep(250), incr() end || _Count <- lists:seq(1,1000) ] end).
