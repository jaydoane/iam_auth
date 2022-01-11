-module(iam_auth_simple_sup).

-behaviour(supervisor).

-export([
    add/1,
    delete/1,
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpec = #{
        id => ?MODULE,
        start => {fresh_iam_auth, start_link, []}
    },
    {ok, {SupFlags, [ChildSpec]}}.


add(Name) ->
    case whereis(Name) of
        undefined ->
            Config = iam_config(Name),
            supervisor:start_child(?MODULE, [Name, Config]);
        Pid ->
            {already_exists, Pid}
    end.


delete(Name) ->
    ok = supervisor:terminate_child(?MODULE, Name).


iam_config() ->
    {ok, [Config]} = file:consult(os:getenv("HOME") ++ "/.config/iam_auth"),
    Config.


iam_config(Id) when is_atom(Id) ->
    [Username, Role] = string:tokens(atom_to_list(Id), ":"),
    case iam_config(Username, list_to_atom(Role), iam_config()) of
        [#{} = Config] ->
            Config;
        _ ->
            undefined
    end.


iam_config(Username, Role, IamEnvs) ->
    maps:fold(fun(IamEnv, EnvConfig, Acc) ->
        Accounts = maps:get(accounts, EnvConfig, undefined),
        Accounts =/= undefined orelse throw({IamEnv, accounts_missing}),
        case maps:find(Username, Accounts) of
            {ok, Roles} ->
                case maps:find(Role, Roles) of
                    {ok, ApiKey} ->
                        Config = EnvConfig#{
                            api_key => ApiKey,
                            iam_env => IamEnv,
                            role => Role,
                            username => Username
                        },
                        [maps:remove(accounts, Config) | Acc];
                    _ ->
                        Acc
                end;
            _ ->
                Acc
        end
    end, [], IamEnvs).

