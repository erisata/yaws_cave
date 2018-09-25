%%%
%%%
%%%
-module(yaws_cave_type).
-compile([{parse_transform, lager_transform}]).
-export([login/2]).

%%
%%
%%
-callback login(
    Username :: string(),
    Password :: string(),
    Opts     :: term()
) ->
    {ok, #{user_id => string(), user_name => string()}} |
    {error, Reason :: term()}.

%%
%%
%%
login(Username, Password) ->
    {ok, AuthModule} = yaws_cave_app:get_env(auth_module),
    AuthModule:login(Username, Password, yaws_cave_app:get_env(auth_opts, [])).


