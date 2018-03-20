%%%
%%%
%%%
-module(auth_type_file).
-behaviour(auth_type).
-compile([{parse_transform, lager_transform}]).
-export([login/3]).


%%
%%
%%
login(UserName, Password, Opts) when is_list(UserName); is_list(Password) ->
    login(encode(string, UserName), encode(string, Password), Opts);

%%
%%
login(UserName, Password, _Opts) ->
    {ok, UsersFile} = auth_app:get_env(users),
    {ok, Data} = file:read_file(UsersFile),
    Users = binary:split(Data, [<<"\n">>], [global]),
    ParsedUsers = [ list_to_tuple(binary:split(UserData, [<<":">>], [global])) || UserData <- Users ],
    case proplists:get_value(UserName, ParsedUsers, false) of
        false    ->
            {error, user_not_found};
        Password ->
            {ok, #{
                user_id     => UserName,
                user_name   => UserName
            }};
        _Other   ->
            {error, bad_credentials}
    end.


%%
%%
%%
encode(string, String) when is_binary(String) ->
    String;

encode(string, String) when is_list(String) ->
    list_to_binary(String).
