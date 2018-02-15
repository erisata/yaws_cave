%%%
%%%  Testcases
%%%
-module(auth_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_basic/1
]).
-include_lib("common_test/include/ct.hrl").


%%
%%  CT API.
%%
all() -> [
    test_basic
].


%%
%%  CT API, initialisation.
%%
init_per_suite(Config) ->
    {ok, Started} = application:ensure_all_started(auth),
    [{auth_apps, Started} | Config].


%%
%%  CT API, cleanup.
%%
end_per_suite(Config) ->
    [ ok = application:stop(App) || App <- proplists:get_value(auth_apps, Config)],
    ok.



%% =============================================================================
%%  Testcases.
%% =============================================================================

%%
%%  Idle application tests.
%%
test_basic(_Config) ->
    ok.


