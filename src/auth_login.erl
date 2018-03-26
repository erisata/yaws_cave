%%%
%%%
%%%
-module(auth_login).
-compile([{parse_transform, lager_transform}]).
-export([auth/2, out401/3, out/1, get_auth_token/1]).
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% Yaws callbacks.
%%% ============================================================================

%%
%%
%%
auth(Arg, _Auth) ->
    case get_auth_token(Arg) of
        {ok, _UserId}    -> true;
        {error, _Reason} -> {appmod, ?MODULE}
    end.


%%
%%
%%
out401(#arg{opaque = Opaque}, _Auth, _Realm) ->
    {_, LoginUri} = proplists:lookup("auth_webui_login_uri", Opaque),
    {redirect, LoginUri}.


%%
%%  Entry point for Yaws appmod.
%%
out(Arg = #arg{req = Req, client_ip_port = {_Ip, _Port}, opaque = Opaque}) ->
    Path = auth_static:path_tokens(Arg),
    Method = yaws_api:http_request_method(Req),
    case lists:member({"auth_webui_debug", "true"}, Opaque) of
        true ->
            ok;
        false ->
            ok
    end,
    handle_request(Path, Method, Arg).


%%
%%
%%
handle_request([], 'POST', Arg = #arg{opaque = Opaque}) ->
    Fields = yaws_api:parse_post(Arg),
    Username = proplists:get_value("username", Fields),
    Password = proplists:get_value("password", Fields),
    FormOrCodeFun = fun(Message) ->
        case auth_app:get_env(return_form_on_error, true) of
            true ->
                auth_static:serve_translated(["login.html"], Arg, [
                    {<<"@NOTIF_MESSAGE@">>, Message},
                    {<<"@NOTIF_CLASS@">>,   <<"alert-danger">>}
                ]);
            false ->
                auth_static:respond_error_json(200, jiffy:encode({[{<<"error">>, true}, {<<"message">>, Message}]}))
        end
    end,
    case auth_type:login(Username, Password) of
        {ok, #{user_id := AuthUserId, user_name := AuthUserName}} ->
            {ok, JWT} = auth_jwt_login:make_jwt(AuthUserId, AuthUserName),
            {_, StartUri} = proplists:lookup("auth_webui_start_uri", Opaque),
            [
                yaws_api:set_cookie("auth_token", erlang:binary_to_list(JWT), [{path, "/"}]),
                {redirect, StartUri}
            ];
        {error, Reason} when Reason =:= user_not_found; Reason =:= bad_credentials ->
            FormOrCodeFun(<<"Login failed: invalid username or password.">>);
        {error, Reason} ->
            FormOrCodeFun(erlang:iolist_to_binary(io_lib:format("Login failed: ~p", [Reason])))
    end;

handle_request([], 'GET', Arg = #arg{opaque = Opaque}) ->
    case get_auth_token(Arg) of
        {ok, UserId} ->
            lager:debug("User ~p provided a valid token, redirecting to the app.", [UserId]),
            {_, StartUri} = proplists:lookup("auth_webui_start_uri", Opaque),
            {redirect, StartUri};
        {error, no_token} ->
            lager:debug("User provided no token, login screen will be shown."),
            auth_static:serve_translated(["login.html"], Arg, [
                {<<"@NOTIF_MESSAGE@">>, <<"Login please.">>},
                {<<"@NOTIF_CLASS@">>,   <<"alert-info">>}
            ]);
        {error, Reason} ->
            lager:debug("User provided an invalid token, login screen will be shown, error=~p", [Reason]),
            auth_static:serve_translated(["login.html"], Arg, [
                {<<"@NOTIF_MESSAGE@">>, <<"Login please.">>},
                {<<"@NOTIF_CLASS@">>,   <<"alert-info">>}
            ])
    end;

handle_request(["img", "favicon.ico"] = Path, 'GET', Arg) ->
    auth_static:serve_plain(Path, Arg);

handle_request(["css." ++ _] = Path, 'GET', Arg) ->
    auth_static:serve_plain(Path, Arg);

handle_request(Path, Method, Arg) ->
    auth_static:respond_unknown(?MODULE, Path, Method, Arg).



%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%
%%
get_auth_token(#arg{headers = #headers{cookie = Cookie}}) ->
%%    @todo AuthToken
    AuthToken = yaws_api:find_cookie_val("auth_token", Cookie),
%%    AuthToken = <<"eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJzYXJ1bmFzIiwibmFtZSI6InNhcnVuYXMiLCJpc3MiOiJhdXRoIiwiaWF0IjoxNTE5NzQxNDQ2LCJleHAiOjE1MjAxMDE0NDZ9.iLYtlme7lutCJC-2XTsRiE3BZW3pkzNRExCkhJdy0uLMvHmT54sYOZQY2AeXw33WFKSHbEGhvuZj18p4x2g5HTRPBMoG3FQa0VuGvKMbk5i3AZJ_sJfkbW92ZCiZiGnJAQwERcA5SpibcBmThISd9lBXVkBnVeOG9yhMjtHDGY68XQ6q8WrBkaT-UPwouFk5DgNJ47vjqsafml94dKvUvC7YMdCN9UlPswt3BwIzhFxZ59Me5SiINNZ_XlY9TYjKV77qXsnJcDKrTOjK-XprAFK2d9T0MFXvNbKuqIAu-XQ23Uo4-b4CYbLJDZYh2nlXfdjv0hO1rGaYmdPPzrEABQ">>,
    case AuthToken of
        "" ->
            {error, no_token};
        _ ->
            lager:debug("xxxxxx AuthToken=~p", [AuthToken]),
            case catch auth_jwt_login:check_jwt(AuthToken) of
                {ok, UserId}     -> {ok, UserId};
                {error, Reason}  -> {error, Reason};
                {'EXIT', Reason} -> {error, Reason};
                Error            -> {error, Error}
            end
    end.

