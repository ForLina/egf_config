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

-module(egf_timer).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([handle_config/1]).
-export([remove_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-define(SERVER, ?MODULE).

-define(DEFAULT_SELECT_NUM, 200).

-define(DEFAULT_INTERVAL, 1000).
-record(state, {
    interval = ?DEFAULT_INTERVAL,
    time_fun = fun unixtime/0
}).

-record(scheduler, {
    key :: atom(),
    mfa :: {M :: atom(), F :: atom(), A :: list()},
    next_exec :: integer(), % timestamp
    interval :: integer() %% in seconds
}).

-include_lib("stdlib/include/ms_transform.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Callback of egf_config
-spec handle_config(tuple()) -> ok.
handle_config({?MODULE, Key, {M, F, A} = MFA, Interval}) when
    is_atom(Key), is_atom(M), is_atom(F), is_list(A), is_integer(Interval) ->
    gen_server:cast(?MODULE, {add, Key, MFA, Interval}).

-spec remove_timer(Key) -> ok.
remove_timer(Key) when is_atom(Key) ->
    gen_server:cast(?MODULE, {remove, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    TableOpts = [named_table, public, set, {keypos, #scheduler.key}],
    ?MODULE = ets:new(?MODULE, TableOpts),
    State = #state{},
    tick(State),
    {ok, State}.

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
handle_call(_Request, _From, State = #state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({add, Key, MFA, Interval}, State = #state{time_fun = TimeFun}) ->
    Now = TimeFun(),
    case ets:lookup(?MODULE, Key) of
        [#scheduler{} = R] ->
            ets:insert(?MODULE, R#scheduler{mfa = MFA,
                                            interval = Interval});
        [] ->
            ets:insert(?MODULE, #scheduler{key = Key,
                                           mfa = MFA,
                                           next_exec = Now + Interval,
                                           interval = Interval})
    end,
    {noreply, State};
handle_cast({remove, Key}, State) ->
    ets:delete(?MODULE, Key),
    {noreply, State};
handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(tick, State = #state{time_fun = TimeFun}) ->
    tick(State),
    proc_lib:spawn(fun() -> check(TimeFun) end),
    {noreply, State};
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

tick(#state{interval = Interval}) ->
    erlang:send_after(Interval, ?MODULE, tick).


unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.


check(TimeFun) ->
    Now = TimeFun(),
    MS = ets:fun2ms(fun(#scheduler{next_exec = ExecTime} = R)
        when ExecTime =:= Now -> R end),
    Ret = ets:select(?MODULE, MS, ?DEFAULT_SELECT_NUM),
    do_check(Ret, Now).

do_check({Match, Continuation}, Now) ->
    [exec_fun(R, Now) || R <- Match],
    NewRet = ets:select(Continuation),
    do_check(NewRet, Now);
do_check('$end_of_table', _Now) ->
    ok.

exec_fun(#scheduler{mfa = {M, F, A}, interval = Interval} = R, Now) ->
    ets:insert(?MODULE, R#scheduler{next_exec = Now + Interval}),
    proc_lib:spawn(fun() -> apply(M, F, A) end).
