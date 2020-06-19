-module(iam_auth_refresher).

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
    {Value :: term(), ExpiresAfter :: second()} | {error, Error :: term()}.
refresh(#{} = Config) ->
    #{
        token_url := TokenUrl,
        token_creds := TokenCreds,
        api_key := ApiKey,
        session_url := SessionUrl
    } = Config,
    case request_token(TokenUrl, TokenCreds, ApiKey) of
        {Token, ExpiresAfter} ->
            ContentType = "application/x-www-form-urlencoded",
            Body = <<"access_token=", Token/binary>>,
            case refresher_session:refresh_cookie(SessionUrl, ContentType, Body) of
                {Session, _SessionExpiresAfter} ->
                    {#{
                        token => Token,
                        session => Session
                    }, ExpiresAfter};
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.


request_token(Url, Creds, ApiKey) ->
    {ReqHeaders, ReqBody} = headers_body(ApiKey, Creds),
    Request = {Url, ReqHeaders, "", ReqBody},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RspBody}} ->
            #{
                <<"access_token">> := Token,
                <<"expiration">> := Expiration
            } = jiffy:decode(RspBody, [return_maps]),
            {Token, Expiration - refresher:now(sec)};
        Error ->
            Error
    end.


-spec headers_body(string(), string()) ->
    {headers(), string()}.
headers_body(ApiKey, Creds) ->
    Headers = [
        {"Accept", "application/json"},
        {"Content-Type", "application/x-www-form-urlencoded"}
    ] ++ auth_headers(Creds),
    %% Body = mochiweb_util:urlencode([
    Body = urlencode([
        {"grant_type", "urn:ibm:params:oauth:grant-type:apikey"},
        {"response_type", "cloud_iam"},
        {"apikey", ApiKey}
    ]),
    {Headers, Body}.


-spec auth_headers(string()) ->
    headers().
auth_headers("") ->
    [];
auth_headers(ColonSeparatedCreds) ->
    B64Auth = base64:encode_to_string(ColonSeparatedCreds),
    [{"Authorization", "Basic " ++ B64Auth}].


-spec urlencode([{string(), string()}]) ->
    string().
urlencode(Props) ->
    string:join(
        [http_uri:encode(K) ++ "=" ++ http_uri:encode(V) || {K, V} <- Props],
        "&"
    ).
