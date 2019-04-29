-module(conn_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1, accept/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

accept(Sock) ->
    % give the socket to supervisor
    ok = gen_tcp:controlling_process(Sock, whereis(?MODULE)),
    supervisor:start_child(?MODULE, [Sock]).

init([]) ->
    {ok, {
        #{strategy => simple_one_for_one, intensity => 2, period => 10},
        [
            #{id => conn, start => {conn, start_link, []},
                modules => [conn], restart => transient}
        ]}}.
