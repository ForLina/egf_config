%% MIT License
%%
%% Copyright (c) 2020 Jack Liu
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(egf_event_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0]).
-export([
    event_test/1
]).

-export([init_per_suite/1, end_per_suite/1]).

init_per_suite(Config) ->
    ok = application:start(egf_config),
    Config.

end_per_suite(Config) ->
    application:stop(egf_config),
    Config.

all() ->
    [event_test
    ].

event_test(_Config) ->
    {ok, _Pid} = egf_timer_test_counter:start_link(),
    0 = gen_server:call(egf_timer_test_counter, get),
    
    EventCfg = filename:absname("event.cfg"),
    
    %% whenever receives a event, call
    %% apply(erlang, send, [egf_timer_test_counter ++ InputArgs])
    ok = file:write_file(EventCfg, <<"{egf_event, event_test, [{erlang, send, [egf_timer_test_counter]}]}.">>),
    ok = egf_config:load_file(EventCfg),
    
    TimerCfg = filename:absname("timer.cfg"),
    %% Send to egf_timer_test_counter every 2 seconds
    %% Please note how the parameters are passed here
    ok = file:write_file(TimerCfg, <<"{egf_timer, timer_test, {egf_event, notify, [event_test, [set]]},  2}.">>),
    
    ok = egf_config:load_file(TimerCfg),
    
    timer:sleep(2000),
    1 = gen_server:call(egf_timer_test_counter, get),
    timer:sleep(2000),
    2 = gen_server:call(egf_timer_test_counter, get).