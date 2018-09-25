%%%
%%%
%%%
-module(yaws_cave_type_ldap).
-behaviour(yaws_cave_type).
-compile([{parse_transform, lager_transform}]).
-export([login/3]).


%%
%% Ex. cn=admin, password=root
%%
login(UserName, Password, _Opts) when is_list(UserName); is_list(Password) ->
    lager:debug("xxxxxxxxxxxx Start"),
    {ok, LdapConfig} = yaws_cave_app:get_env(ldap),
    Ip   = proplists:get_value(ip,   LdapConfig),
    Dn   = proplists:get_value(dn,   LdapConfig),
    Opts = proplists:get_value(opts, LdapConfig),
    DefaultOpts = [
        {port, proplists:get_value(port, Opts)},
        {ssl, proplists:get_value(ssl, Opts, false)}
    ],
    LdapOpts = case proplists:get_value(log, Opts, false) of
        true ->
            [{log, fun (_L, S, A) -> io:format(S, A) end} | DefaultOpts];
        false ->
            DefaultOpts
    end,
    {ok, H} = eldap:open([Ip], LdapOpts),
    lager:debug("xxxxxxxxxxxx H=~p", [H]),
    case eldap:simple_bind(H, Dn ++ "\\" ++ UserName, Password) of
        ok ->
            {ok, #{
                user_id     => UserName,
                user_name   => UserName
            }};
        {error, invalidCredentials} ->
            {error, bad_credentials};
        Other ->
            {error, Other}
    end.


