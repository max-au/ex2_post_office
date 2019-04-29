-module(config).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    [{Key, Value}] = ets:lookup(?MODULE, Key),
    Value.

init([]) ->
    Terms = consult(),
    % avoid race conditions: create a temporary table for configuration
    %   and rename it to ?TABLE when bootstrap is done
    Loader = ets:new('$temporary', [named_table, {read_concurrency, true}]),
    _ = [true = ets:insert(Loader, Term) || Term <- Terms],
    ?MODULE = ets:rename('$temporary', ?MODULE),
    {ok, {}}.

handle_call(Request, From, _State) ->
    error({bad_call, Request, From}).

handle_cast(Request, _State) ->
    error({bad_cast, Request}).

handle_info(Info, _State) ->
    error({bad_info, Info}).

consult() ->
    {ok, App} = application:get_application(),
    File = filename:join([code:priv_dir(App), "config"]),
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, Reason} ->
            ?LOG_WARNING("Still waiting for ~200p... (last reason ~s)", [File, Reason]),
            timer:sleep(1000),
            consult()
    end.
