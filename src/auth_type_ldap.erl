%%%
%%%
%%%
-module(auth_type_ldap).
-behaviour(auth_type).
-compile([{parse_transform, lager_transform}]).
-export([login/3]).


%%
%% Ex. cn=admin, password=root
%%
login(UserName, Password, _Opts) when is_list(UserName); is_list(Password) ->
    {ok, LdapConfig} = auth_app:get_env(ldap),
    Ip   = proplists:get_value(ip,   LdapConfig),
    Dc   = proplists:get_value(dc,   LdapConfig),
    Opts = proplists:get_value(opts, LdapConfig),
    DefaultOpts = [{port, proplists:get_value(port, Opts)}, {ssl, proplists:get_value(ssl, Opts, false)}],
    LdapOpts = case proplists:get_value(log, Opts, false) of
        true ->
            [{log, fun (_L, S, A) -> io:format(S, A) end} | DefaultOpts];
        false ->
            DefaultOpts
    end,
    {ok, H} = eldap:open([Ip], LdapOpts),
    case eldap:simple_bind(H, "cn=" ++ UserName ++ "," ++ Dc, Password) of
        ok ->
            {ok, #{
                user_id     => UserName,
                user_name   => UserName
            }};
        _Other ->
            {error, bad_credentials}
    end.


