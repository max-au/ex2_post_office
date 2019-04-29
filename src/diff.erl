-module(diff).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

-include_lib("kernel/include/logger.hrl").

start_link(Name, Source) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, Source}, []).

-record(state, {name, source, prev, diff = #{}}).

init({Name, Source}) ->
    next_tick(),
    {ok, #state{name = Name, source = Source, prev = fetch(Source)}}.

handle_call(get, _From, #state{diff = Diff} = State) ->
    {reply, {ok, Diff}, State}.

handle_cast(Request, _State) ->
    error({badarg, Request}).

handle_info(tick, #state{name = Name, prev = Prev, source = Source} = State) ->
    next_tick(),
    New = fetch(Source),
    {Zero, Diff} = maps:fold(
        fun (K, V, {Z, Acc}) ->
            NewV = V - maps:get(K, Prev, 0),
            {min(Z, NewV =:= 0), maps:put(K, NewV, Acc)}
        end, {true, #{}}, New),
    Zero =:= false andalso ?LOG_NOTICE("~p: ~200p~n", [Name, Diff]),
    {noreply, State#state{prev = New, diff = Diff}};
handle_info(Info, _State) ->
    error({badarg, Info}).

fetch(Source) ->
    {ok, New} = gen_server:call(Source, get),
    New.

next_tick() ->
    erlang:send_after(1000, self(), tick).