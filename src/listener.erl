-module(listener).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {listener}).

init([]) ->
    % wait until dispatcher is ready
    wait_for_couriers(dispatch:count_couriers(), config:get(min_couriers)),
    % start listener
    {ok, Socket} = gen_tcp:listen(9999, [{reuseaddr, true}, {packet, line}]),
    {ok, _} = prim_inet:async_accept(Socket, -1),
    {ok, #state{listener = Socket}}.

handle_call(Request, From, _State) ->
    error({bad_call, Request, From}).

handle_cast(Request, _State) ->
    error({bad_cast, Request}).

handle_info({inet_async, LSock, _, {ok, Accepted}}, #state{listener = LSock} = State) ->
    inet_db:register_socket(Accepted, inet_tcp),
    conn_sup:accept(Accepted),
    {ok, _} = prim_inet:async_accept(LSock, -1),
    {noreply, State};
handle_info(Info, _State) ->
    error({bad_info, Info}).

wait_for_couriers(Have, HowMany) when Have >= HowMany ->
    ok;
wait_for_couriers(Have, HowMany) ->
    ?LOG_NOTICE("Waiting for couriers, have ~b and need ~b", [Have, HowMany]),
    timer:sleep(1000),
    wait_for_couriers(dispatch:count_couriers(), HowMany).
