%%%
%%%  Testcases
%%%
-module(auth_SUITE).
-compile([{parse_transform, lager_transform}]).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    test_login_credentials_true/1,
    test_login_credentials_false/1,
    test_login_jwt_credentials_true/1,
    test_login_jwt_credentials_false/1
]).
-include_lib("common_test/include/ct.hrl").


%%
%%  CT API.
%%
all() -> [
    test_login_credentials_true,
    test_login_credentials_false,
    test_login_jwt_credentials_true,
    test_login_jwt_credentials_false
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

test_login_credentials_true(_Config) ->
    {ok, 302, _Headers, <<>>} = hackney:request(post, <<"http://localhost:8027/admin/login">>, [], {form, [{"username", "sarunas"}, {"password", "pa55word"}]}, [with_body]),
    ok.


test_login_credentials_false(_Config) ->
    {ok, 200, _Headers, _Body} = hackney:request(post, <<"http://localhost:8027/admin/login">>, [], {form, [{"username", "bad"}, {"password", "bad"}]}, [with_body]),
    ok.


test_login_jwt_credentials_true(_Config) ->
    {ok, AuthToken} = auth_jwt_login:make_jwt(<<"sarunas">>, <<"sarunas">>),
    {ok, 200, _Headers, <<"API\n">>} = hackney:request(get, <<"http://localhost:8027/admin/api">>, [{<<"Authorization">>, <<"Bearer ", AuthToken/binary>>}], <<>>, [with_body]),
    ok.

test_login_jwt_credentials_false(_Config) ->
    AuthToken = <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJiYWQiLCJuwiaXNzIjoiYXV0sjnUomZpxqezaE8lYKNsMEiQ7kENabNQNo0P5tHGVz7Hf_X43WpPjwaCIsImlhdCI6MTUyMTQ3NTczNiwiZXhwIjoxNTIxODM1NzM2fQ.keq9fqMRjRQX6Mn4b7LA7uhpl9-api5Xo38MRi_eig-y_BGC4XKs11BHYJP_YzHLUhBH26vIEeLaH-eDMbSHoQBKcmqRBgpwa-q5bcoMCapRgcDgbxHQzvWnij22KuWEz2ExXemZ9tfZXEPxSvuiMuovz0YscI7XWbqye0C-siA5rXxKDdKBmD4LqUE5D3Eri5WSbKZ4GFInwe3hVy42vpSHVSFUKIstJyujJltliem_jSQ">>,
    {ok, 401, _Headers, _Body} = hackney:request(get, <<"http://localhost:8027/admin/api">>, [{<<"Authorization">>, <<"Bearer ", AuthToken/binary>>}], <<>>, [with_body]),
    ok.