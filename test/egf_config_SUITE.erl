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

-module(egf_config_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0]).
-export([
    load_file_test/1,
    load_dir1_test/1, load_dir2_test/1,
    add_dir_test/1
]).

-export([init_per_suite/1, end_per_suite/1]).

init_per_suite(Config) ->
    ok = application:start(egf_config),
    Config.

end_per_suite(Config) ->
    application:stop(egf_config),
    Config.

all() ->
    [load_file_test,
     load_dir1_test, load_dir2_test,
     add_dir_test
    ].

load_file_test(_Config) ->
    Filename = filename:absname("test.cfg"),
    
    %% right config
    ok = file:write_file(Filename, <<"{egf_config, test,  [named_table], [
    {jack, 13},
    {lin, 12}]}.">>),
    
    ok = egf_config:load_file(Filename),
    [{jack, 13}] = ets:lookup(test, jack),
    
    %% reload same file
    ok = file:write_file(Filename, <<"{egf_config, test,  [named_table], [
    {jack, 12},
    {lin, 14}]}.">>),
    
    ok = egf_config:load_file(Filename),
    [{lin, 14}] = ets:lookup(test, lin),
    
    %% bad term
    ok = file:write_file(Filename, <<"{egf_config, test,  [named_table], [
    {jack, 12},
    {lin, 14]}.">>),
    
    {error, _} = egf_config:load_file(Filename),
    
    %% when config is not a tuple
    ok = file:write_file(Filename, <<"[egf_config, test,  [named_table], [
    {jack, 12},
    {lin, 14}]].">>),
    
    {error, bad_config} = egf_config:load_file(Filename),
    
    %% wrong handler
    ok = file:write_file(Filename, <<"{egf_cfg, test,  [named_table], [
    {jack, 13},
    {lin, 12}]}.">>),
    
    {error, no_such_handler} = egf_config:load_file(Filename),
    
    %% wrong arguments
    ok = file:write_file(Filename, <<"{egf_config, test, [name_table ], [
    {jack, 13},
    {lin, 12}]}.">>),
    
    {error, badarg} = egf_config:load_file(Filename),
    
    %% wrong tuple size
    ok = file:write_file(Filename, <<"{egf_config, [name_table ], [
    {jack, 13},
    {lin, 12}]}.">>),
    
    {error, function_clause} = egf_config:load_file(Filename).

load_dir1_test(_Config) ->
    AbsName = filename:absname("test.cfg"),
    DirName = filename:dirname(AbsName),
    {ok, FD} =  file:open(AbsName, [write]),
    ok = file:write(FD,  <<"{egf_config, test,  [named_table], [
    {jack, 11},
    {lin, 12}]}.">>),
    file:close(FD),
    
    egf_config:load_dir(DirName),
    [{jack, 11}] = ets:lookup(test, jack).

load_dir2_test(_Config) ->
    AbsName = filename:absname("test.config"),
    DirName = filename:dirname(AbsName),
    {ok, FD} =  file:open(AbsName, [write]),
    ok = file:write(FD,  <<"{egf_config, test,  [named_table], [
    {jack, 12},
    {lin, 12}]}.">>),
    file:close(FD),
    
    egf_config:load_dir(DirName, "*.config"),
    [{jack, 12}] = ets:lookup(test, jack).
    
add_dir_test(_Config) ->
    AbsName = filename:absname("test.cfg"),
    DirName = filename:dirname(AbsName),
    
    {ok, FD} =  file:open(AbsName, [write]),
    
    ok = file:write(FD,  <<"{egf_config, test,  [named_table], [
    {jack, 13},
    {lin, 12}]}.">>),
    
    file:close(FD),
    ok = egf_config:add_dir(DirName),
    
    %% Default check interval is 5000
    timer:sleep(6000),
    
    [{jack, 13}] = ets:lookup(test, jack),
    
    %% Test auto reload file
    ok = file:write_file(AbsName, <<"{egf_config, test,  [named_table], [
    {jack, 14},
    {lin, 12}]}.">>),
    
    %% Default check interval is 5000
    timer:sleep(6000),
    
    [{jack, 14}] = ets:lookup(test, jack),
    
    %% if set interval works
    egf_config:set_interval(1000),
    
    ok = file:write_file(AbsName, <<"{egf_config, test,  [named_table], [
    {jack, 15},
    {lin, 12}]}.">>),
    
    %% Default check interval is 5000
    timer:sleep(1100),
    
    [{jack, 15}] = ets:lookup(test, jack),
    
    %% Test set suffix works
    
    egf_config:set_suffix("*.config"),
    
    ConfigName = filename:join(DirName, "test.config"),
    
    ok = file:write_file(ConfigName, <<"{egf_config, test,  [named_table], [
    {jack, 16},
    {lin, 12}]}.">>),
    
    timer:sleep(1100),
    
    [{jack, 16}] = ets:lookup(test, jack).
    