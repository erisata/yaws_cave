%%%
%%% YAWS Appmod redirecting all the requests to the start page of this application.
%%%
-module(auth_webui_yaws_default).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% API Functions
%%% ============================================================================

%%
%%  Entry point for Yaws appmod.
%%
out(Arg = #arg{req = Req}) ->
    Path = axb_webui_yaws_appmod:path_tokens(Arg),
    Method = yaws_api:http_request_method(Req),
    handle_request(Path, Method, Arg).


%%
%%
%%
handle_request(_Path, _Method, #arg{opaque = Opaque}) ->
    {_, StartUri} = proplists:lookup("auth_webui_start_uri", Opaque),
    {redirect, StartUri}.


