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
-export([start_link/1, info/0, incr/1, incr/2, test/0]).

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

incr(Name) -> gen_server:cast({global, Name}, {incr, 1}).
incr(Name, Count) -> gen_server:cast({global, Name}, {incr, Count}).

info() -> gen_server:call(?MODULE, {info}).

%% @doc Start server listening on IpAddr:Port
-spec start_link(list()) -> ok | ignore | {error, any()}.
start_link([List]) -> 
    Name = proplists:get_value(name, List),
    gen_server:start_link({global, Name}, ?MODULE, [List], []).

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
handle_info(flush, #state{hits=Hits, mod_ref=Mod, timestamp=TS} = State) ->
    gen_server:cast(Mod, {per_second, TS, Hits}),
    {noreply, State#state{hits=0, timestamp=timestamp()}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

timestamp() ->
    %% return the timestamp as a binary in rfc1123 format
    Date = erlang:universaltime(),
    {{Y,Mo,D}, {H,M,S}} = Date,
    Wday = calendar:day_of_the_week({Y, Mo, D}),
	  << (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		   (month(Mo))/binary, " ", (list_to_binary(integer_to_list(Y)))/binary,
		   " ", (pad_int(H))/binary, $:, (pad_int(M))/binary,
		   $:, (pad_int(S))/binary, " GMT" >>.

-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
	<< $0, ($0 + X) >>;
pad_int(X) ->
	list_to_binary(integer_to_list(X)).

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.


test() ->
    spawn(fun() -> [begin timer:sleep(250), incr(api_call) end || _Count <- lists:seq(1,1000) ] end).
