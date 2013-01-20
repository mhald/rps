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

-module(stats_util).

-author('mhald@mac.com').

%% API
-export([high_low/1]).

-record(stats, {high = 0 :: integer(),
                low  = 0 :: integer()}).

high_low([]) -> [{max, 0}, {min, 0}];
high_low(List) ->
    Acc = high_low_scan(List, []),
    #stats{high=High, low=Low} = lists:nth(length(Acc), Acc),
    [{max, High}, {min, Low}].

high_low_scan([], Acc) -> Acc;
high_low_scan([H|T], []) ->
    {_, Value, _} = H,
    high_low_scan(T, [#stats{high=Value, low=Value}]);
high_low_scan([H|T], Acc) ->
    {_, Value, _} = H,
    #stats{high=High, low=Low} = lists:nth(length(Acc), Acc),
    high_low_scan(T, Acc ++ [#stats{high=erlang:max(Value, High), low=erlang:min(Value,Low)}]).
