%%%-------------------------------------------------------------------
%% @doc ex2_eightball top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ex2_post_office_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
        #{strategy => one_for_one, intensity => 2, period => 60},
        [
            #{id => config, start => {config, start_link, []},
                modules => [config]},
            #{id => stats_sup, start => {stats_sup, start_link, []},
                modules => [stats_sup], type => supervisor, restart => transient},
            #{id => post_office_sup, start => {post_office_sup, start_link, []},
                modules => [post_office_sup], type => supervisor},
            #{id => listen_sup, start => {listen_sup, start_link, []},
                modules => [listen_sup], type => supervisor}
        ]}}.

