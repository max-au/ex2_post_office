-module(ex2_post_office_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ex2_post_office_sup:start_link().

stop(_State) ->
    ok.
