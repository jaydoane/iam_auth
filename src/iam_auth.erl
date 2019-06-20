-module(iam_auth).

-export([
    start/0,
    stop/0,
    req/2,
    req/3,
    req/4,
    req_token/3,
    auth_session_cookie/3,
    iam_session_cookie/1,
    default_headers/0,
    session_req/3,
    session_req/4,
    session_req/5
]).


start() ->
    application:start(iam_auth).


stop() ->
    application:stop(iam_auth).


req(Url, Method) ->
    req(Url, Method, []).


req(Url, Method, Body) ->
    req(Url, Method, Body, default_headers()).


req(Url, Method, Body, Headers) ->
    {ok, Code, _, RspBody} = send_req(Url, Method, Body, Headers),
    {ok, Code, RspBody}.


session_req(Cookie, Url, Method) ->
    session_req(Cookie, Url, Method, []).


session_req(Cookie, Url, Method, Body) ->
    session_req(Cookie, Url, Method, Body, []).


session_req(Cookie, Url, Method, Body, Headers0) ->
    Headers = [{"Cookie", Cookie} | json_headers()] ++ Headers0,
    req(Url, Method, Body, Headers).


iam_session_cookie(Url) ->
    Body = "access_token=" ++ iam_auth_server:token(),
    session_cookie(Url ++ "/_iam_session", Body).


auth_session_cookie(Url, User, Pass) ->
    Body = "name=" ++ User ++ "&password=" ++ Pass,
    session_cookie(Url ++ "/_session", Body).


default_headers() ->
    json_headers() ++ iam_auth_server:auth_headers().


req_token(Url, Creds, ApiKey) ->
    {ReqHeaders, ReqBody} = epep_util:token_req_headers_body(ApiKey, Creds),
    Opts = [{response_format, binary}],
    case ibrowse:send_req(Url, ReqHeaders, post, ReqBody, Opts) of
        {ok, "200", _, RspBody} ->
            jiffy:decode(RspBody, [return_maps]);
        Error ->
            Error
    end.


%% private


json_headers() ->
    [
        {"Accept", "application/json"},
        {"Content-Type", "application/json"}
    ].


session_cookie(Url, Body) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    {ok, 200, RspHeaders, _} = send_req(Url, post, Body, Headers),
    Cookie = proplists:get_value("Set-Cookie", RspHeaders),
    hd(string:tokens(Cookie, ";")).


send_req(Url, Method, Body, Headers) ->
    Opts = [{response_format, list}],
    case ibrowse:send_req(Url, Headers, Method, Body, Opts) of
        {ok, Code, RspHeaders, RspBody} ->
            {ok, list_to_integer(Code), RspHeaders, jiffy:decode(RspBody)};
        Else ->
            Else
    end.


