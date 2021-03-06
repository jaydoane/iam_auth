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
    req(Url, Method, Body, []).


req(Url, Method, Body, Headers) ->
    req(Url, Method, Body, Headers, []).


req(Url, Method, Body, Headers0, Opts) ->
    Headers = default_headers() ++ Headers0,
    send_req(Url, Method, Body, Headers, Opts).


default_headers() ->
    json_headers() ++ iam_auth_server:auth_headers().


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
    {ok, 200, RspHeaders, _} = send_req(Url, post, Body, Headers, []),
    Cookie = proplists:get_value("Set-Cookie", RspHeaders),
    hd(string:tokens(Cookie, ";")).


send_req(Url, Method, Body, Headers, Opts0) ->
    Opts = default_opts() ++ Opts0,
    case ibrowse:send_req(Url, Headers, Method, Body, Opts) of
        {ok, Code, RspHeaders, RspBody} ->
            {ok, list_to_integer(Code), RspHeaders, maybe_decode(RspBody)};
        Else ->
            Else
    end.


default_opts() ->
    [{response_format, list}].


maybe_decode([]) ->
    [];
maybe_decode(Body) ->
    jiffy:decode(Body, [return_maps]).
