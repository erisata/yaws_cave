%%%
%%%
%%%
-module(auth_login).

-compile([{parse_transform, lager_transform}]).
-export([out/3]).
-include_lib("yaws/include/yaws_api.hrl").



%%% ============================================================================
%%% API Functions
%%% ============================================================================

out(Module, Arg, Opts) ->
    #arg{req = #http_request{method = Method}} = Arg,
    Path = path_tokens(Arg),
    Module:handle_request(Path, Method, Arg, Opts).



%%% ============================================================================
%%% Internal functions
%%% ============================================================================

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


