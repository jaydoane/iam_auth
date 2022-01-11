-module(iam_auth_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 100, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Children = [
        ?CHILD(iam_auth_simple_sup, supervisor)
    ],
    {ok, {{one_for_one, 2, 10}, Children} }.
