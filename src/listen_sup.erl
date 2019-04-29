-module(listen_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
        #{strategy => one_for_all, intensity => 1, period => 60},
        [
            #{id => listener, start => {listener, start_link, []},
                modules => [listener]},
            #{id => conn_sup, start => {conn_sup, start_link, []},
                modules => [conn_sup], type => supervisor}
        ]}}.
