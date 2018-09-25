%%%
%%% YAWS Appmod redirecting all the requests to the start page of this application.
%%%
-module(yaws_cave_yaws_default).
-compile([{parse_transform, lager_transform}]).
-export([out/1, path_tokens/1]).
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%  Entry point for Yaws appmod.
%%
out(Arg = #arg{req = Req}) ->
    Path = path_tokens(Arg),
    Method = yaws_api:http_request_method(Req),
    handle_request(Path, Method, Arg).

%%% ============================================================================
%%% Private functions
%%% ============================================================================



%%
%%
%%
handle_request(_Path, _Method, #arg{opaque = Opaque}) ->
    {_, StartUri} = proplists:lookup("auth_webui_start_uri", Opaque),
    {redirect, StartUri}.


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


