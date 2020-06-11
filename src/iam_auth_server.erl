-module(iam_auth_server).

-behavior(gen_server).

-export([
    start_link/0,
    refresh/0,
    token/0,
    auth_headers/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).


-record(state, {
    api_key :: string(),
    expiration :: undefined | non_neg_integer(),
    token :: undefined | string(),
    user :: string()
}).


-define(REFRESH_ERROR_DELAY, 2000).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


refresh() ->
    gen_server:call(?MODULE, refresh).


token() ->
    gen_server:call(?MODULE, token).


auth_headers() ->
    gen_server:call(?MODULE, auth_headers).


init(_Opts) ->
    State = refresh(#state{
        api_key = config:get("iam_auth", "api_key"),
        user = config:get("iam_auth", "user")
    }),
    erlang:send_after(0, self(), refresh),
    {ok, State}.


handle_call(auth_headers, _From, State) ->
    Headers = [
        {"X-Cloudant-User", State#state.user},
        {"Authorization", "Bearer " ++ State#state.token}],
    {reply, Headers, State};

handle_call(refresh, _From, State) ->
    {reply, ok, refresh(State)};

handle_call(token, _From, State) ->
    {reply, State#state.token, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(refresh, State0) ->
    State = refresh(State0),
    erlang:send_after(next_refresh(State#state.expiration), self(), refresh),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%% private

next_refresh(Expiration) ->
    Val = max(?REFRESH_ERROR_DELAY,
        (Expiration - epep_util:now(sec) - 60) * 1000),
    case Val of ?REFRESH_ERROR_DELAY ->
        couch_log:debug("~p next_refresh ~p", [?MODULE, Val]);
        _ -> ok
    end,
    Val.


refresh(State) ->
    Creds = config:get("epep", "token_creds", ""),
    Url = config:get("epep", "token_url"),
    case iam_auth:req_token(Url, Creds, State#state.api_key) of
        #{<<"access_token">> := Token, <<"expiration">> := Expiration} ->
            %% {ok, {Claims}} = epep:jwt_decode(Token),
            %% couch_log:info("~p claims ~p", [?MODULE, Claims]),
            couch_log:debug("~p new access token expires in ~p sec",
                [?MODULE, Expiration - epep_util:now(sec)]),
            State#state{expiration = Expiration, token = binary_to_list(Token)};
        Else ->
            couch_log:warning("~p refresh error ~p", [?MODULE, Else]),
            State
    end.
