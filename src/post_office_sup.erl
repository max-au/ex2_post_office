-module(post_office_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
        #{strategy => one_for_all, intensity => 2, period => 60},
        [
            #{id => courier_sup, start => {courier_sup, start_link, []},
                modules => [courier_sup], type => supervisor},
            #{id => dispatch, start => {dispatch, start_link, []},
                modules => [dispatch]}
        ]}}.
