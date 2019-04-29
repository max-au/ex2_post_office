-module(courier_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0, hire/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

hire(Ids) ->
    [{ok, _} = supervisor:start_child(?MODULE, [Seq]) || Seq <- Ids].

init([]) ->
    {ok, {
        #{strategy => simple_one_for_one, intensity => 100, period => 10},
        [
            #{id => courier, start => {courier, start_link, []},
                modules => [courier], restart => temporary}
        ]}}.
