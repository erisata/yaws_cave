-module(auth_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([name/0, get_env/1, get_env/2]).

-define(APP, auth).

%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Returns name of this application.
%%
name() ->
    ?APP.



%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    auth_sup:start_link().

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