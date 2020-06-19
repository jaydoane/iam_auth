-module(session_cookie_refresher).

-behavior(refresher).

% refresher behaviour callbacks
-export([
    refresh/1
]).

-export([
    start_link/1,
    value/0
]).

-include("refresher.hrl").


start_link(Config) ->
    refresher:start_link(?MODULE, Config).


value() ->
    refresher:value(?MODULE).


-spec refresh(Config :: map()) ->
    {Value :: string(), ExpiresAfter :: second()} | {error, Error :: term()}.
refresh(#{} = Config) ->
    #{
        session_url := Url,
        username := Username,
        password := Password
    } = Config,
    ContentType = "application/x-www-form-urlencoded",
    Body = "name=" ++ Username ++ "&password=" ++ Password,
    refresher_session:refresh_cookie(Url, ContentType, Body).
