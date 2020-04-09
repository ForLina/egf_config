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

-module(egf_config).

-callback handle_config(Config :: tuple()) -> ok.

%% API
-export([
      load_file/1,
      load_dir/1, load_dir/2,
      add_dir/1,
      set_interval/1,
      set_suffix/1
]).

%% @doc Sometimes we just need load a configuration file once when server
%%      starts.
-spec load_file(AbsPath :: string()) -> ok | {error, Reason :: term()}.
load_file(AbsPath) ->
    egf_config_loader:load_file(AbsPath).

-spec load_dir(AbsPath :: string()) -> ok | {error, Reason :: term()}.
load_dir(AbsPath) ->
    load_dir(AbsPath, "*.cfg").

-spec load_dir(AbsPath :: string(), Suffix :: string()) ->
    ok | {error, Reason :: term()}.
load_dir(AbsPath, Suffix) ->
    ConfigFiles  = filelib:wildcard(Suffix, AbsPath),
    do_load_dir(ConfigFiles, AbsPath).

-spec add_dir(AbsPath :: string()) -> ok | {error, Error :: term()}.
add_dir(AbsPath) ->
    egf_config_loader:add_dir(AbsPath).

-spec set_interval(Interval :: integer()) -> ok.
set_interval(Interval) ->
    egf_config_loader:set_interval(Interval).

-spec set_suffix(Suffix :: string()) -> ok.
set_suffix(Suffix) ->
    egf_config_loader:set_suffix(Suffix).

%%%==================================================================
%%% Internal functions
%%%==================================================================

do_load_dir([], _AbsPath) ->
    ok;
do_load_dir([Filename|T], AbsPath) ->
    case load_file(filename:join(AbsPath, Filename)) of
        ok ->
            do_load_dir(T, AbsPath);
        {error, Reason} ->
            {error, Reason}
    end.
