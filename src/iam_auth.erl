-module(iam_auth).

-export([
    start/0,
    stop/0,
    req/2,
    req/3,
    req/4,
    req_session_cookie/1,
    session_req/3,
    session_req/4
]).


start() ->
    application:start(iam_auth).


stop() ->
    application:stop(iam_auth).


req(Url, Method) ->
    req(Url, Method, []).


req(Url, Method, Body) ->
    Headers = json_headers() ++ iam_auth_server:auth_headers(),
    req(Url, Method, Body, Headers).


req(Url, Method, Body, Headers) ->
    {ok, Code, _, RspBody} = send_req(Url, Method, Body, Headers),
    {ok, Code, RspBody}.


session_req(Cookie, Url, Method) ->
    session_req(Cookie, Url, Method, []).


session_req(Cookie, Url, Method, Body) ->
    Headers = [{"Cookie", Cookie} | json_headers()],
    req(Url, Method, Body, Headers).


req_session_cookie(Url) ->
    {ok, 200, Headers, {[{<<"ok">>,true}]}} = post_token(Url),
    Cookie = proplists:get_value("Set-Cookie", Headers),
    hd(string:tokens(Cookie, ";")).


%% private


post_token(Url) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = "access_token=" ++ iam_auth_server:token(),
    send_req(Url, post, Body, Headers).


send_req(Url, Method, Body, Headers) ->
    Opts = [{response_format, list}],
    case ibrowse:send_req(Url, Headers, Method, Body, Opts) of
        {ok, Code, RspHeaders, RspBody} ->
            {ok, list_to_integer(Code), RspHeaders, jiffy:decode(RspBody)};
        Else ->
            Else
    end.


json_headers() ->
    [
        {"Accept", "application/json"},
        {"Content-Type", "application/json"}
    ].
