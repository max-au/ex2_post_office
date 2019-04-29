-module(stats).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    count/2,
    print/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

count(Key, Value) ->
    gen_server:cast(?MODULE, {count, Key, Value}).

print() ->
    gen_server:cast(?MODULE, print).

init([]) ->
    {ok, #{tasks => 0}}.

handle_call(get, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast({count, Key, Value}, State) ->
    NewMap = maps:update_with(Key, fun (V) -> V + Value end, Value, State),
    {noreply, NewMap};
handle_cast(print, State) ->
    io:format("Counters: ~200p~n", [State]),
    {noreply, State};
handle_cast(Request, _State) ->
    error({badarg, Request}).

handle_info(Info, _State) ->
    error({badarg, Info}).
