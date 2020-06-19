-module(refresher_session).

-export([
    refresh_cookie/3
]).

-include("refresher.hrl").

-define(DEFAULT_MAX_AGE, "600"). % seconds, from couch_httpd_auth.erl


-spec refresh_cookie(Url :: string(), ContentType :: string(), Body :: iodata()) ->
    {Values :: map(), ExpiresAfter :: second()} | term().
refresh_cookie(Url, ContentType, Body) ->
    case request_cookie(Url, ContentType, Body) of
        Cookie when is_list(Cookie) ->
            Tokens = [string:strip(S) || S <- string:tokens(Cookie, ";")],
            Session = hd(Tokens),
            {#{session => Session}, max_age(props(Tokens))};
        Error ->
            Error
    end.


request_cookie(Url, ContentType, Body) ->
    Request = {Url, [], ContentType, Body},
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, Request, [], []),
    proplists:get_value("set-cookie", Headers).


props(Tokens) ->
    lists:foldl(fun(Token, Acc) ->
        case string:tokens(Token, "=") of
            [Key, Val] ->
                [{Key, Val} | Acc];
            Else ->
                [Else | Acc]
        end
    end, [], Tokens).


max_age(Props) ->
    list_to_integer(
        proplists:get_value("Max-Age", Props, ?DEFAULT_MAX_AGE)).
