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
    Path = auth_yaws_default:path_tokens(Arg),
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
    {ok, StaticFilesAppmod} = auth_app:get_env(static_files_appmod),
    Fields = yaws_api:parse_post(Arg),
    Username = proplists:get_value("username", Fields),
    Password = proplists:get_value("password", Fields),
    FormOrCodeFun = fun(Message) ->
        case auth_app:get_env(return_form_on_error, true) of
            true ->
                StaticFilesAppmod:serve_translated(["login.html"], Arg, [
                    {<<"@NOTIF_MESSAGE@">>, Message},
                    {<<"@NOTIF_CLASS@">>,   <<"alert-danger">>}
                ]);
            false ->
                respond_error_json(200, jiffy:encode({[{<<"error">>, true}, {<<"message">>, Message}]}))
        end
    end,
    case auth_type:login(Username, Password) of
        {ok, #{user_id := AuthUserId, user_name := AuthUserName}} ->
            {ok, JWT} = auth_http_login:make_jwt(AuthUserId, AuthUserName),
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
    {ok, StaticFilesAppmod} = auth_app:get_env(static_files_appmod),
    case get_auth_token(Arg) of
        {ok, UserId} ->
            lager:debug("User ~p provided a valid token, redirecting to the app.", [UserId]),
            {_, StartUri} = proplists:lookup("auth_webui_start_uri", Opaque),
            {redirect, StartUri};
        {error, no_token} ->
            lager:debug("User provided no token, login screen will be shown."),
            StaticFilesAppmod:serve_translated(["login.html"], Arg, [
                {<<"@NOTIF_MESSAGE@">>, <<"Login please.">>},
                {<<"@NOTIF_CLASS@">>,   <<"alert-info">>}
            ]);
        {error, Reason} ->
            lager:debug("User provided an invalid token, login screen will be shown, error=~p", [Reason]),
            StaticFilesAppmod:serve_translated(["login.html"], Arg, [
                {<<"@NOTIF_MESSAGE@">>, <<"Login please.">>},
                {<<"@NOTIF_CLASS@">>,   <<"alert-info">>}
            ])
    end;

handle_request(["img", "favicon.ico"] = Path, 'GET', Arg) ->
    {ok, StaticFilesAppmod} = auth_app:get_env(static_files_appmod),
    StaticFilesAppmod:serve_plain(Path, Arg);

handle_request(["css." ++ _] = Path, 'GET', Arg) ->
    {ok, StaticFilesAppmod} = auth_app:get_env(static_files_appmod),
    StaticFilesAppmod:serve_plain(Path, Arg);

handle_request(Path, Method, Arg) ->
    respond_unknown(?MODULE, Path, Method, Arg).



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
            case catch auth_http_login:check_jwt(AuthToken) of
                {ok, UserId}     -> {ok, UserId};
                {error, Reason}  -> {error, Reason};
                {'EXIT', Reason} -> {error, Reason};
                Error            -> {error, Error}
            end
    end.

%%% ============================================================================
%%% API Functions
%%% ============================================================================



%%
%%  Generic method for responding to unknown paths.
%%
respond_unknown(Module, Path, Method, _Args) ->
    respond_error(404, "Unknown resource", Module, Path, Method, '#').


%%
%%  Respond to request with error message.
%%
respond_error(Number, Message, Module, Path, Method, Args) ->
    lager:warning(string:concat(Message, " in ~p, path=~p, method=~p, args=~p"), [Module, Path, Method, Args]),
    [
        {status, Number},
        {ehtml, get_error_html(Message)}
    ].


%%
%%
%%
respond_error_json(Number, Body) ->
    [
        {status,  Number},
        {content, <<"application/json">>, Body}
    ].


%%
%%
%%
get_error_html(Error) ->
    [{h1, [], "ERROR!"},
        {p, [], Error}].


