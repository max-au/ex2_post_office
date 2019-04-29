-module(stats_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
        #{strategy => rest_for_one, intensity => 2, period => 60},
        [
            #{id => stats, start => {stats, start_link, []},
                modules => [stats]},
            #{id => diff, start => {diff, start_link, [diff, stats]},
                modules => [diff]},
            #{id => diff_diff, start => {diff, start_link, [diff_diff, diff]},
                modules => [diff]}
        ]}}.
