-module(iam_auth_app).

-behaviour(application).

-export([
    start/2,
    stop/1]).


start(_StartType, _StartArgs) ->
    iam_auth_sup:start_link().


stop(_State) ->
    ok.
