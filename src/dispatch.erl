-module(dispatch).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    count_couriers/0,
    report/3,
    dispatch/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-record(state, {next_id, couriers = [], queue}).

count_couriers() ->
    gen_server:call(?MODULE, count_couriers).

report(Pid, ID, Reason) ->
    gen_server:cast(?MODULE, {report, Pid, ID, Reason}).

dispatch(Parcel) ->
    gen_server:cast(?MODULE, {dispatch, Parcel}).

init([]) ->
    % hire some couriers for a start
    MaxId = config:get(couriers),
    courier_sup:hire(lists:seq(1, MaxId)),
    {ok, #state{next_id = MaxId + 1, queue = queue:new()}}.

handle_call(count_couriers, _From, #state{couriers = Couriers} = State) ->
    {reply, length(Couriers), State};
handle_call(Request, From, _State) ->
    error({bad_call, Request, From}).

handle_cast({dispatch, Parcel}, #state{couriers = [], queue = Queue} = State) ->
    % no more couriers, put it in the queue
    {noreply, State#state{queue = queue:in(Parcel, Queue)}};
handle_cast({dispatch, Parcel}, #state{couriers = [Courier | Couriers]} = State) ->
    courier:deliver(Courier, Parcel),
    {noreply, State#state{couriers = Couriers}};

handle_cast({report, Pid, ID, Reason}, #state{couriers = Couriers, queue = Queue} = State) ->
    Reason =:= hired andalso monitor(process, Pid),
    case queue:out(Queue) of
        {{value, Parcel}, NewQ} ->
            ?LOG_INFO("Courier ~p reported, and for immediate delivery of ~p", [ID, Parcel]),
            courier:deliver(Pid, Parcel),
            {noreply, State#state{couriers = [Pid | Couriers], queue = NewQ}};
        {empty, _} ->
            ?LOG_INFO("Courier ~p reported to dispatcher!", [ID]),
            {noreply, State#state{couriers = [Pid | Couriers]}}
    end;

handle_cast(Request, _State) ->
    error({bad_cast, Request}).


handle_info({'DOWN', _, process, Pid, _}, #state{next_id = NextId, couriers = Couriers} = State) ->
    % some worker resigned, could be one in the pool
    NewCouriers = case lists:member(Pid, Couriers) of
                      true ->
                          lists:delete(Pid, Couriers);
                      false ->
                          Couriers
                  end,
    % in any case, we need to hire another courier
    courier_sup:hire([NextId]),
    {noreply, State#state{next_id = NextId + 1, couriers = NewCouriers}};

handle_info(Info, _State) ->
    error({bad_info, Info}).
