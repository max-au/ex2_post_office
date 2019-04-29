-module(courier).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    deliver/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

-include_lib("kernel/include/logger.hrl").

start_link(ID) ->
    gen_server:start_link(?MODULE, ID, []).

deliver(Me, Parcel) ->
    gen_server:cast(Me, {deliver, Parcel}).

-record(state, {id, patience}).

init(ID) ->
    Timer = 3 + rand:uniform(20),
    ?LOG_NOTICE("Hiring courier ~b, expecting one in ~b seconds", [ID, Timer]),
    erlang:send_after(Timer * 1000, self(), start),
    {ok, #state{id = ID}}.

handle_call(Request, From, _State) ->
    error({bad_call, Request, From}).

handle_cast({deliver, Parcel}, #state{id = ID, patience = P} = State) when P > 0 ->
    ?LOG_INFO("Courier ~p delivering ~p", [ID, Parcel]),
    timer:sleep(rand:uniform(config:get(delivery_rand)) + config:get(delivery_min)),
    dispatch:report(self(), ID, free),
    stats:count(delivered, 1),
    {noreply, State#state{patience = P - 1}};
handle_cast({deliver, Parcel}, #state{id = ID, patience = P}) when P =:= 0 ->
    exit({ugh, ID, "I'm dropping ", Parcel, " and quitting this job!"});
handle_cast(Request, _State) ->
    error({bad_cast, Request}).

handle_info(start, #state{id = ID} = State) ->
    dispatch:report(self(), ID, hired),
    P = rand:uniform(config:get(patience)),
    S = rand:uniform(config:get(resign) + rand:uniform(config:get(resign))),
    erlang:send_after(S * 1000, self(), resign),
    ?LOG_WARNING("Courier ~b hired, it has ~b patience and will resign in ~b seconds", [ID, P, S]),
    stats:count(patience, P),
    {noreply, State#state{patience = P}};

handle_info(resign, #state{id = ID} = State) ->
    ?LOG_WARNING("Courier ~b resigned", [ID]),
    stats:count(resign, 1),
    {stop, normal, State};

handle_info(Info, _State) ->
    error({bad_info, Info}).
