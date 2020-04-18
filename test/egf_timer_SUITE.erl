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

-module(egf_timer_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0]).
-export([
    timer_test/1
]).

-export([init_per_suite/1, end_per_suite/1]).

init_per_suite(Config) ->
    ok = application:start(egf_config),
    Config.

end_per_suite(Config) ->
    application:stop(egf_config),
    Config.

all() ->
    [timer_test
    ].

timer_test(_Config) ->
    {ok, _Pid} = egf_timer_test_counter:start_link(),
    0 = gen_server:call(egf_timer_test_counter, get),
    Filename = filename:absname("test.cfg"),
    
    %% Send to egf_timer_test_counter every 2 seconds
    ok = file:write_file(Filename, <<"{egf_timer, test, {erlang, send, [egf_timer_test_counter, set]},  2}.">>),
    ok = egf_config:load_file(Filename),
    timer:sleep(2000),
    1 = gen_server:call(egf_timer_test_counter, get),
    timer:sleep(2000),
    2 = gen_server:call(egf_timer_test_counter, get).
    