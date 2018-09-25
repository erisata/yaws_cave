%%%
%%%
%%%
-module(auth_static).
-compile([{parse_transform, lager_transform}]).
-export([
    out/1,
    serve_plain/2,
    serve_translated/2,
    serve_translated/3,
    respond_unknown/4,
    respond_error/6,
    path_tokens/1,
    respond_error_json/2,
    handle_request/3
]).
-include_lib("yaws/include/yaws_api.hrl").



%%% ============================================================================
%%% Yaws entry point
%%% ============================================================================

%%
%%  Entry point for Yaws appmod.
%%
out(Arg = #arg{req = Req, client_ip_port = {Ip, _Port}, opaque = Opaque}) ->
    Path = path_tokens(Arg),
    Method = yaws_api:http_request_method(Req),
    handle_request(Path, Method, Arg).



%%% ============================================================================
%%% Handlers for REST URIs
%%% ============================================================================

%%
%%  Handle all the requests.
%%
handle_request([], 'GET', Arg) ->
    serve_translated(["index.html"], Arg);

handle_request(["css." ++ _] = Path, 'GET', Arg) ->
    serve_translated(Path, Arg);

handle_request(["app." ++ _] = Path, 'GET', Arg) ->
    serve_translated(Path, Arg);

handle_request(["vendor." ++ _] = Path, 'GET', Arg) ->
    serve_translated(Path, Arg);

handle_request(["polyfills." ++ _] = Path, 'GET', Arg) ->
    serve_translated(Path, Arg);

handle_request(["assets" | _] = Path, 'GET', Arg) ->
    serve_translated(Path, Arg);

handle_request(["img", "logo.png"], 'GET', Arg) ->
    case auth_app:get_env(inst_logo) of
        {ok, {static_files, LogoFile}} ->
            ContentType = yaws_api:mime_type(LogoFile),
            serve_file(priv_file(LogoFile), ContentType, Arg);
        {ok, LogoFile} ->
            ContentType = yaws_api:mime_type(LogoFile),
            serve_file(LogoFile, ContentType, Arg);
        undefined ->
            serve_plain(["img", "erisata-logo.png"], Arg)
    end;

handle_request(["img" | _] = Path, 'GET', Arg) ->
    serve_plain(Path, Arg);

handle_request(_Path, 'GET', Arg) ->
    % Consider all other paths as dynamic urls,
    % handled by the client side.
    serve_translated(["index.html"], Arg);

handle_request(Path, Method, Arg) ->
    respond_unknown(?MODULE, Path, Method, Arg).



%%% ============================================================================
%%% API Functions.
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
    [{h1,[],"ERROR!"},
        {p,[],Error}].


%%
%%
%%
serve_plain(Path, Arg) ->
    FileName = string:join(Path, "/"),
    serve_priv_file(FileName, yaws_api:mime_type(FileName), Arg).


%%
%%
%%
serve_translated(Path, Arg) ->
    serve_translated(Path, Arg, []).

serve_translated(Path, Arg, Bindings) ->
    FileName = string:join(Path, "/"),
    Response = serve_priv_file(FileName, yaws_api:mime_type(FileName), Arg),
    case lists:member({status, 200}, Response) of
        true ->
            AllBindings = [
                {<<"@VERSION@">>,     erlang:iolist_to_binary(auth_app:version())},
                {<<"@PREFIX@">>,      erlang:iolist_to_binary(appmod_path(Arg))},
                {<<"@API_PREFIX@">>,  erlang:iolist_to_binary(api_prefix(Arg))},
                {<<"@INST_ENV@">>,    erlang:iolist_to_binary(auth_app:get_env(inst_env,  "Default"))},
                {<<"@INST_NAME@">>,   erlang:iolist_to_binary(auth_app:get_env(inst_name, "Auth module"))}
                | Bindings
            ],
            ReplaceBinding = fun ({BndName, BndValue}, Binary) ->
                binary:replace(Binary, BndName, BndValue, [global])
            end,
            Preprocess = fun
                ({content, ContentType, Content}) ->
                    BinContent = erlang:iolist_to_binary(Content),
                    NewContent = lists:foldl(ReplaceBinding, BinContent, AllBindings),
                    {content, ContentType, NewContent};
                (Other) ->
                    Other
            end,
            lists:map(Preprocess, Response);
        false ->
            Response
    end.


%%
%%
%%
serve_priv_file(FileName, ContentType, Arg) ->
    serve_file(priv_file(FileName), ContentType, Arg).

serve_file(FileName, ContentType, Arg) ->
    case file:read_file(FileName) of
        {ok, Content} ->
            ContentType = yaws_api:mime_type(FileName),
            [
                {status, 200},
                {content, ContentType, Content}
            ];
        {error, enoent} ->
            respond_unknown(?MODULE, FileName, 'GET', Arg);
        {error, eacces} ->
            respond_error(401, "Insuficient access rights", ?MODULE, FileName, 'GET', Arg);
        {error, Reason} ->
            respond_error(400, lists:concat(["Error: ", Reason]), ?MODULE, FileName, 'GET', Arg)
    end.


%%
%%
%%
priv_file(FileName) ->
    PrivDir = case auth_app:get_env(static_files) of
        undefined ->
            case code:priv_dir(auth_app:name()) of
                {error, bad_name} -> "priv/www/"; % To allow testing without creating whole app.
                Dir -> Dir ++ "/www/"
            end;
        {ok, {path, StaticFilesDir}} ->
            StaticFilesDir ++ "/";
        {ok, {app, App, StaticFilesDir}} ->
            code:priv_dir(App) ++ "/../" ++ StaticFilesDir ++ "/"
    end,
    lists:flatten(PrivDir ++ FileName).


%%
%%  Returns path tokens, that are under the appmod path.
%%
path_tokens(#arg{appmoddata = undefined}) ->
    [];

path_tokens(#arg{appmoddata = AppmodUri}) ->
    path_tokens_normalize(string:tokens(AppmodUri, "/")).


%%
%%  Checks path for the path_traversal attack.
%%
path_tokens_normalize(Tokens) ->
    path_tokens_normalize([], Tokens).

path_tokens_normalize(Normalized, []) ->
    lists:reverse(Normalized);

path_tokens_normalize([], [".." | _Tokens]) ->
    erlang:exit(path_traversal);

path_tokens_normalize([_ | Normalized], [".." | Tokens]) ->
    path_tokens_normalize(Normalized, Tokens);

path_tokens_normalize(Normalized, [Token | Tokens]) when Token =/= ".." ->
    path_tokens_normalize([Token | Normalized], Tokens).


%%
%%  Returns a path, at which the appmod is mounted.
%%
%%  <AppmodPath>/<AppmodData> = <PrePath><Appmod>/AppmodData = <ServerPath>
%%
appmod_path(#arg{server_path = ServerPath, appmoddata = undefined}) ->
    ServerPath;

appmod_path(#arg{server_path = ServerPath, appmoddata = "/"}) ->
    string:substr(ServerPath, 1, length(ServerPath) - 1);

appmod_path(#arg{server_path = ServerPath, appmoddata = AppmodData}) ->
    string:substr(ServerPath, 1, length(ServerPath) - length(AppmodData) - 1).


%%
%%
%%
api_prefix(#arg{opaque = Opaque}) ->
    {"auth_webui_api_prefix", ApiPrefix} = proplists:lookup("auth_webui_api_prefix", Opaque),
    ApiPrefix.


