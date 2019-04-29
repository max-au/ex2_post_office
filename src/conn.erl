-module(conn).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-include_lib("kernel/include/logger.hrl").

start_link(Socket) ->
    Ret = {ok, Pid} = gen_server:start_link(?MODULE, Socket, []),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = gen_server:cast(Pid, start),
    Ret.

-record(state, {socket, name}).

init(Socket) ->
    {ok, #state{socket = Socket}}.

handle_call(Request, From, _State) ->
    error({bad_call, Request, From}).

handle_cast(start, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, true}]),
    ok = gen_tcp:send(Socket, io_lib:format("Welcome to post office!~n", [])),
    {noreply, State};
handle_cast(Request, _State) ->
    error({bad_cast, Request}).

handle_info({tcp_closed, Sock}, #state{socket = Sock}) ->
    gen_tcp:close(Sock),
    {stop, normal, undefined};
handle_info({tcp, Sock, [$N, $a, $m, $e, $  | NewName]},
    #state{socket = Sock, name = Name} = State) ->
    NewName1 = string:trim(NewName),
    ok = gen_tcp:send(Sock, io_lib:format("~s, known as ~s before,  welcome!~n", [NewName1, Name])),
    {noreply, State#state{name = NewName1}};
handle_info({tcp, Sock, [$D, $o, $  | Line]}, #state{socket = Sock, name = Name} = State) ->
    Count = list_to_integer(string:trim(Line)),
    ok = gen_tcp:send(Sock, io_lib:format("~s, taking ~b parcels from you... ", [Name, Count])),
    deliver(Count),
    ok = gen_tcp:send(Sock, io_lib:format("done!~n", [])),
    {noreply, State};
handle_info(Info, _State) ->
    error({bad_info, Info}).

terminate(_Reason, #state{socket = Sock}) ->
    gen_tcp:controlling_process(Sock, whereis(conn_sup)).

deliver(0) ->
    ok;
deliver(Count) ->
    dispatch:dispatch({letter, Count}),
    deliver(Count - 1).
