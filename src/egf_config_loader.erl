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

-module(egf_config_loader).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([
    load_file/1,
    add_dir/1,
    set_interval/1,
    set_suffix/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(INTERVAL, 5000).
-define(SUFFIX, "*.cfg").

-record(state, {
    interval = ?INTERVAL,
    ref :: reference(),
    path_list = [],
    suffix = ?SUFFIX
}).

-define(CFG_INFO, cfg_info).
-record(cfg_info, {
    file_abs_path :: string(),
    last_modify_time :: integer()
}).

-include("logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    Options = [set, private, named_table, {keypos, #cfg_info.file_abs_path}],
    ?CFG_INFO = ets:new(?CFG_INFO, Options),
    State = #state{},
    NewState = tick(State),
    {ok, NewState}.

-spec load_file(AbsPath :: string()) -> ok | {error, Reason :: term()}.
load_file(AbsPath) ->
    case file:consult(AbsPath) of
        {ok, Configs} ->
            handle_configs(Configs);
        {error, Reason} ->
            {error, Reason}
    end.

-spec add_dir(AbsPath :: string()) -> ok | {error, Error :: term()}.
add_dir(AbsPath) ->
    gen_server:call(?MODULE, {add_dir, AbsPath}).

-spec set_interval(Interval :: integer()) -> ok.
set_interval(Interval) when is_integer(Interval), Interval >= 1000 ->
    gen_server:cast(?MODULE, {set_interval, Interval}).

-spec set_suffix(Suffix :: string()) -> ok.
set_suffix(Suffix) ->
    gen_server:cast(?MODULE, {set_suffix, Suffix}).

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call({add_dir, AbsPath}, _From, State = #state{path_list = OldPath}) ->
    
    case filelib:is_dir(AbsPath) of
        true ->
            NewPath = lists:usort([AbsPath | OldPath]),
            NewState = State#state{path_list = NewPath},
            {reply, ok, NewState};
        false ->
            {reply, {error, not_dir}, State}
    end;

handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({set_interval, Interval}, State = #state{ref = Ref})
    when is_integer(Interval), Interval >= 1000 ->
    catch erlang:cancel_timer(Ref),
    NewState =  State#state{interval = Interval},
    {noreply, tick(NewState)};
handle_cast({set_suffix, Suffix}, State = #state{})
    when is_list(Suffix) ->
    {noreply, State#state{suffix = Suffix}};
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(tick, State = #state{}) ->
    check(State),
    NewState = tick(State),
    {noreply, NewState};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_configs([H|T]) ->
    case handle_config(H) of
        ok ->
            handle_configs(T);
        Error ->
            Error
    end;
handle_configs([]) ->
    ok.

handle_config(Config) when is_tuple(Config) ->
    CallbackMod = element(1, Config),
    c:l(CallbackMod),
    case erlang:function_exported(CallbackMod, handle_config, 1) of
        true ->
            try
                ok = apply(CallbackMod, handle_config, [Config])
            catch
                Class:Reason:Stacktrace ->
                    ?LOG_ERROR("Config = ~p, Class = ~p, Reason = ~p, Stacktrace = ~p",
                               [Config, Class, Reason, Stacktrace]),
                    {error, Reason}
            end;
        false ->
            ?LOG_ERROR("No such config handler, Config = ~p, ", [Config]),
            {error, no_such_handler}
    end;
handle_config(_Any) ->
    {error, bad_config}.

tick(#state{interval = Interval, ref = Ref} = State) ->
    catch timer:cancel(Ref),
    NewRef = erlang:send_after(Interval, self(), tick),
    State#state{ref = NewRef}.

check(#state{path_list = Paths, suffix = Suffix}) ->
    check(Paths, Suffix).

check([DirPath | T], Suffix) ->
    Files = filelib:wildcard(Suffix, DirPath),
    check_load(Files, DirPath),
    check(T, Suffix);
check([], _Suffix) ->
    ok.

check_load([Filename | T], DirPath) ->
    AbsPath = filename:join(DirPath, Filename),
    
    case filelib:last_modified(AbsPath) of
        0 ->
            ok;
        LastModifyTime ->
            case ets:lookup(?CFG_INFO, AbsPath) of
                [#cfg_info{last_modify_time = LastModifyTime}] ->
                    ok;
                _Other ->
                    case load_file(AbsPath) of
                        ok ->
                            R = #cfg_info{file_abs_path = AbsPath,
                                          last_modify_time = LastModifyTime},
                            ets:insert(?CFG_INFO, R);
                        {error, Reason} ->
                            ?LOG_ERROR("file = ~p, error = ~p", [AbsPath, Reason])
                    end
            end
    end,
    
    check_load(T, DirPath);
check_load([], _DirPath) ->
    ok.
