-module(iam_auth).

-export([
    start/0,
    session_req/5,
    token_req/5
]).

start() ->
    application:start(iam_auth).


session_req(IamId, Url, Method, Body, Headers) when is_atom(IamId) ->
    send_auth_req(session, IamId, Url, Method, Body, Headers).


token_req(IamId, Url, Method, Body, Headers) when is_atom(IamId) ->
    send_auth_req(token, IamId, Url, Method, Body, Headers).


%% private


send_auth_req(AuthType, IamId, Url, Method, Body, Headers0) ->
    AuthVal = wait_val(IamId, AuthType),
    Headers = [
        auth_header(AuthVal),
        {"X-Cloudant-User", username(IamId)}
    ] ++ Headers0,
    send_req(Url, Method, Body, Headers, []).


wait_val(IamId, AuthType)
    when AuthType =:= session orelse AuthType =:= token
->
    whereis(IamId) =/= undefined orelse iam_auth_simple_sup:add(IamId),
    test_util:wait(fun() ->
        case fresh_iam_auth:value(IamId, AuthType) of
            {ok, Val} -> Val;
            _ -> wait
        end
    end).


username(IamId) when is_atom(IamId) ->
    [Username, _] = string:tokens(atom_to_list(IamId), ":"),
    Username.


auth_header(<<"IAMSession=", _/binary>> = Cookie) ->
    {"Cookie", binary_to_list(Cookie)};
auth_header(<<"Bearer ", _/binary>> = Token) ->
    {"Authorization", binary_to_list(Token)}.


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
