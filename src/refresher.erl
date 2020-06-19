-module(refresher).

-callback refresh(Config :: map()) -> ok.

-behaviour(gen_server).

-export([
    start_link/2,
    value/1,
    now/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(REFRESH_EXPIRATION_PERCENT, 90).

-define(MINIMUM_REFRESH_SEC, 2).

-define(MILLION, 1000000).

-record(entry, {
    key :: term(),
    value :: term()
}).

-include("refresher.hrl").


start_link(Name, Config) when is_atom(Name), is_map(Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Config], []).


value(Name) ->
    case ets:lookup(Name, value) of
        [#entry{value = Value}] ->
            Value;
        Else ->
            Else
    end.


now(sec) ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * ?MILLION + Sec + round(Micro / ?MILLION).


%% gen_server functions


init([Name, Config]) ->
    Opts = [
        named_table,
        {keypos, #entry.key},
        {read_concurrency, true}
    ],
    Name = ets:new(Name, Opts),
    State = #{
        name => Name,
        config => Config
    },
    erlang:send_after(0, self(), refresh),
    {ok, State}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(refresh, State0) ->
    #{expires_after := ExpiresAfter} = State = refresh(State0),
    NextRefresh = next_refresh(ExpiresAfter),
    erlang:send_after(NextRefresh * 1000, self(), refresh),
    {noreply, State#{
        next_refresh => NextRefresh,
        expiration => now(sec) + ExpiresAfter
    }};

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%% private


refresh(#{} = State) ->
    #{
        name := Name,
        config := Config
    } = State,
    {Value, ExpiresAfter} = Name:refresh(Config),
    true = ets:insert(Name, #entry{key = value, value = Value}),
    State#{expires_after => ExpiresAfter}.


-spec next_refresh(ExpiresAfter :: second()) ->
    second().
next_refresh(ExpiresAfter) ->
    Refresh = trunc(ExpiresAfter * ?REFRESH_EXPIRATION_PERCENT / 100),
    max(?MINIMUM_REFRESH_SEC, Refresh).
