-module(yaws_cave_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([name/0, get_env/1, get_env/2, version/0]).

-define(APP, yaws_cave).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Returns name of this application.
%%
name() ->
    ?APP.


%%
%%
%%
version() ->
    case lists:keyfind(?APP, 1, application:which_applications()) of
        {_App, _Type, Version}  -> Version;
        false                   -> undefined
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    yaws_cave_sup:start_link().

stop(_State) ->
    ok.


%%
%%  Returns environment variables for this application.
%%
get_env(Name) ->
    application:get_env(?APP, Name).


%%
%%  Returns environment variables for this application.
%%
get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).